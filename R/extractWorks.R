#' Extract works associated with a concept in openAlex and store data as a compressed R list object
#'
#' @param data_style options for how much/how little data to return, see @details
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @param api_key optional OpenAlex API key; if NULL the \code{OPENALEX_API_KEY} environment variable is used (see \code{\link{performOA}})
#' @param debug boolean, if TRUE returns query url, if FALSE actually does query
#' @param topic_id an openAlex topic id string, e.g. "T13459" (from \code{\link{queryTopics}}); topics replace the deprecated concepts classification
#' @param topic_page string for an openAlex topic page url (id is extracted from it)
#' @param concept_id (deprecated) an openAlex concept id string; concepts are deprecated in favor of topics, but \code{concept.id} still works as an alias filter
#' @param concept_page (deprecated) string for openAlex concept page url
#' @param cursor boolean if TRUE will perform cursor pagination needed for iterating
#' @param from_date earliest publication date, in YYYY-MM-DD format a YEAR (assumes YEAR/01/01)
#' @param to_date latest publication date, in YYYY-MM-DD format or a YEAR (assumes YEAR/12/31)
#' @param per_page how many works returned per page
#' @param keep_paratext boolean to retain or exclude paratext from returns
#' @param sleep_time time to Sys.sleep() in between cursor iterations
#' @param dest_file location to save output as a json.gz
#' @param return_to_workspace boolean for whether final result should be returned as workspace object
#' @param source_id (optional) openAlex ID# for the source associated with the work(s)
#' @param source_page (optional) openAlex webpage for the source
#' @param override override 1M query result limit?
#' @param batch_size how large to chunk up into subfiles?, defaults to 50e3
#' @param credit_floor stop the crawl cleanly (flushing partial results) once the OpenAlex \code{X-RateLimit-Remaining} header drops below this value, before hitting a hard rate-limit wall. Defaults to 0 (disabled; rely on 429 handling instead). See @details on the credit model.
#' @param parallel defaults to 1, sets cluster value in pblapply for processing works in parallel. may provide speed-ups for large lists.
#' @description Primary use is to extract works associated with a given journal (source) or topic. Because the OpenAlex API limits returns to 200, this function iterates to grab all works returned by the query. Each return is a list or 200 works, to which the processWork() function is applied to iteratively develop a flat file. Every page request is routed through \code{\link{performOA}}, so the crawl shares the polite-pool throttle and retries transient 429/5xx errors with backoff. Classification can be supplied either as a \code{topic_id} (openAlex's current Topics scheme, filtered on \code{topics.id}) or, for backward compatibility, a \code{concept_id} (the deprecated Concepts scheme, filtered on the \code{concept.id} alias).
#' @details Note that because extracted records can be pretty large--and are complicated, nested json file--there is an optional "data_style" command that lets the user specify what to return. Currently there are three options: (1) bare_bones returns OpenAlex ID + DOI, basically, results that can be used to look up the work again; (2) citation returns typical citation information, like journal name, author, etc., with a couple bonus items like source.id to link back to openAlex (3) comprehensive returns author institutional affiliations, open access info, funding data, etc.; and (4) all returns the entire result in original json format.
#'
#' OpenAlex now meters usage with a credit/cost budget rather than a raw call
#' count: list-endpoint requests (which this function makes) cost roughly 10
#' credits each regardless of \code{per_page}, so \code{per_page} defaults to the
#' 200-work maximum to minimize the number of paged requests. On page 1 the
#' function prints an estimate of the paged requests and credits a full crawl
#' will cost. The live \code{X-RateLimit-*} response headers are tracked as the
#' crawl runs; if the budget is exhausted (HTTP 429 after retries) or
#' \code{credit_floor} is crossed, whatever has been collected so far is flushed
#' to \code{dest_file} and the function stops with a message so the crawl can be
#' resumed with a finer query or date range.
#' @export
#' @import jsonlite
#' @import httr
#' @import httr2
#' @import stringr
#' @import data.table
#' @importFrom pbapply pblapply
#' @example man/examples/extract.R

extractWorks <- function(data_style = c('bare_bones','citation','comprehensive','all'),
                         dest_file = NULL,override = 1e6,batch_size = 50e3,
                         credit_floor = 0,
                         mailto = NULL,api_key = NULL,
                         topic_id = NULL,topic_page = NULL,
                         concept_id = NULL,concept_page = NULL,
                         source_id = NULL,
                         source_page = NULL,cursor = TRUE,per_page = NULL,
                         to_date = NULL,from_date = NULL,keep_paratext = FALSE,
                         debug = FALSE,sleep_time = 0.1,parallel = 1,
                         return_to_workspace = TRUE){
  if(missing(topic_id)&!missing(topic_page)){topic_id <- stringr::str_extract(topic_page,'[A-Za-z0-9]+$')}
  if(missing(concept_id)&!missing(concept_page)){concept_id <- stringr::str_extract(concept_page,'[A-Za-z0-9]+$')}
  if(missing(source_id)&!missing(source_page)){source_id <- stringr::str_extract(source_page,'[A-Za-z0-9]+$')}
  if(missing(source_id)&missing(concept_id)&missing(topic_id)){stop("Must specify a topic, concept, and/or a source to query (using page or id)")}
  if(missing(dest_file)&return_to_workspace==FALSE){stop("Must specify a file destination to save the result or set return = T")}
  works_base <- 'https://api.openalex.org/works'
  url <- parse_url(works_base)
  if(!is.null(mailto)){
    url$query$mailto<-mailto
    }
  if(cursor){
    url$query$cursor<-"*"
    }
  ## Cost protocol: list-endpoint requests cost ~10 credits each regardless of
  ## page size, so default to the 200-work maximum to minimize the number of
  ## paged requests (and thus credits) needed to walk a result set.
  if(is.null(per_page)){per_page <- 200}
  if(per_page>200){per_page <- 200; message('openAlex allows up to 200 results per page, lowering to per_page = 200')}
  url$query$`per-page` <- per_page
  if(!missing(topic_id)){
    ## Topics are openAlex's current classification scheme (concepts are deprecated)
    url$query$filter$topics.id<-topic_id
    }
  if(!missing(concept_id)){
    ## deprecated concepts scheme; concept.id still works as an alias filter
    url$query$filter$concept.id<-concept_id
    }
  if(!missing(source_id)){
    url$query$filter$locations.source.id<-source_id
    }
  if(!missing(from_date)){
    from_date <- if(nchar(from_date)==4){paste(from_date,'01','01',sep = '-')}
    url$query$filter$from_publication_date<-from_date
    }
  if(!missing(to_date)){
    to_date <- if(nchar(to_date)==4){paste(to_date,'12','31',sep = '-')}
    url$query$filter$to_publication_date<-to_date
    }
  if(!keep_paratext){
    url$query$filter$is_paratext<-"false"
    }
  if(length(url$query$filter)>1){
  url$query$filter<-paste(paste0(paste0(names(url$query$filter),':'),url$query$filter),collapse = ',')
  }
  qurl <- build_url(url)
  if(debug){return(qurl)}
  if(!debug){
  p = 1
  store_results <- list()
  iterfile <- 1
  credits_used <- 0
  ## helper: pull a numeric OpenAlex rate-limit header (NA if absent)
  hdr_num <- function(resp,name){v <- httr2::resp_header(resp,name); if(is.null(v)) NA_real_ else suppressWarnings(as.numeric(v))}
  ## helper: process + save the current buffer to the right chunk file so a
  ## crawl that stops early (rate limit, error) is not lost
  flush_partial <- function(results,iterfile){
    if(length(results)==0 || is.null(dest_file)){return(invisible())}
    processed_list <- pblapply(results,processWork,data_style = data_style,cl = parallel)
    processed_dt <- rbindlist(processed_list,use.names = T,fill = T)
    f <- if(iterfile>1){stringr::str_replace(dest_file,'\\.rds$',paste0('_',iterfile,'.rds'))}else{dest_file}
    saveRDS(object = processed_dt,file = f)
  }
  while(p==1|ifelse(!exists('js'),T,!is.null(js$meta$next_cursor))){
    print(paste('querying page',p))
    ## throttled + retrying request (shared polite-pool token bucket; retries
    ## transient 429/5xx with backoff, honoring Retry-After -- see performOA)
    resp <- performOA(qurl, api_key = api_key)
    status <- httr2::resp_status(resp)
    if(status != 200){
      ## persistent non-2xx after retries: most often the daily credit budget is
      ## exhausted (HTTP 429). Flush what we have so the crawl can be resumed.
      reset_s <- hdr_num(resp,'x-ratelimit-reset')
      flush_partial(store_results,iterfile)
      stop(sprintf('OpenAlex request failed (HTTP %s) after retries%s.%s',
                   status,
                   if(!is.na(reset_s)) sprintf('; rate-limit budget resets in ~%.0f s',reset_s) else '',
                   if(!is.null(dest_file)) ' Partial results saved; re-run to resume with a finer query or date range.' else ''))
    }
    js <- httr2::resp_body_json(resp)
    ## track credit spend / remaining budget from response headers when present
    cu <- hdr_num(resp,'x-ratelimit-credits-used'); if(!is.na(cu)){credits_used <- credits_used + cu}
    remaining <- hdr_num(resp,'x-ratelimit-remaining')
    ### js$results is list with length = per_page (or less if fewer than per_page works are returned)
    if(p==1){
      if(js$meta$count>override){
        stop(paste0('more than ',override,' works returned, set a higher limit or make a finer query'))
      }
      else{
        est_pages <- ceiling(js$meta$count/per_page)
        ## list endpoints cost ~10 credits each under OpenAlex's credit model
        print(paste0(js$meta$count,' works found (~',est_pages,' paged requests, ~',est_pages*10,' credits)'))
      }
    }
    ## stop cleanly before hitting a hard rate-limit wall (opt-in via credit_floor)
    if(credit_floor>0 && !is.na(remaining) && remaining < credit_floor){
      reset_s <- hdr_num(resp,'x-ratelimit-reset')
      store_results <- append(store_results,js$results)
      flush_partial(store_results,iterfile)
      stop(sprintf('Stopping: rate-limit budget nearly exhausted (%.0f remaining, floor %s)%s. Partial results saved; re-run to resume.',
                   remaining,credit_floor,
                   if(!is.na(reset_s)) sprintf('; resets in ~%.0f s',reset_s) else ''))
    }
    store_results <- append(store_results,js$results)
    if(length(store_results)>=batch_size){
      query_label <- if(!missing(source_id)){source_id}else if(!missing(topic_id)){topic_id}else{concept_id}
      print(paste('processing',length(store_results),'works from',query_label))
      processed_list <- pblapply(store_results,processWork,data_style = data_style,cl = parallel)
      processed_dt <- rbindlist(processed_list,use.names = T,fill = T)
      saveRDS(object = processed_dt, file = stringr::str_replace(dest_file,'\\.rds$',paste0('_',iterfile,'.rds')))
      iterfile <- iterfile + 1
      store_results <- list()
    }
    url$query$cursor<-js$meta$next_cursor
    qurl <- build_url(url)
    p <- p + 1
    Sys.sleep(sleep_time)
  }
    print(paste('processing',length(store_results),'works'))
    processed_list <- pblapply(store_results,processWork,data_style = data_style,cl = parallel)
    processed_dt <- rbindlist(processed_list,use.names = T,fill = T)
 if(!missing(dest_file)){
    print(paste('saving result'))
    if(iterfile>1){saveRDS(object = processed_dt,
                           file = stringr::str_replace(dest_file,'\\.rds$',
                                                       paste0('_',iterfile,'.rds')))}else{
      saveRDS(object = processed_dt, file = dest_file)
      }
    }

  }
  if(return_to_workspace){return(processed_dt)}
  }

