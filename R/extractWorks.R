#' Extract works associated with a source or concept in openAlex and store data as a compressed R list object
#'
#' @param data_style options for how much/how little data to return, see @details
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @param concept_id a concept id string (https://docs.openalex.org/api)
#' @param debug boolean, if TRUE returns query url, if FALSE actually does query
#' @param concept_page string for openAlex concept page url
#' @param concept_id string for openAlex concept ID
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
#' @param parallel defaults to 1, sets cluster value in pblapply for processing works in parallel. may provide speed-ups for large lists.
#' @param exit_if_over integer value -- abort if you find more than this number of works
#' @description Primary use is to extract works associated with a given journal (source) or concept. Because the OpenAlex API limits returns to 200, this function iterates to grab all works returned by the query. Each return is a list or 200 works, to which the processWork() function is applied to iteratively develop a flat file.
#' @details Note that because extracted records can be pretty large--and are complicated, nested json file--there is an optional "data_style" command that lets the user specify what to return. Currently there are three options: (1) bare_bones returns OpenAlex ID + DOI, basically, results that can be used to look up the work again; (2) citation returns typical citation information, like journal name, author, etc., with a couple bonus items like source.id to link back to openAlex (3) comprehensive returns author institutional affiliations, open access info, funding data, etc.; and (4) all returns the entire result in original json format.
#' @export
#' @import jsonlite
#' @import httr
#' @import stringr
#' @import data.table
#' @importFrom pbapply pblapply
#' @example man/examples/extract.R

extractWorks <- function(data_style = c('bare_bones','citation','comprehensive','all'),
                         dest_file = NULL,override = 1e6,
                         mailto = NULL,concept_id = NULL,
                         concept_page = NULL,source_id = NULL,
                         source_page = NULL,cursor = T,per_page = NULL,
                         to_date = NULL,from_date = NULL,keep_paratext = FALSE,
                         debug = FALSE,sleep_time = 0.1,parallel = 1,
                         return_to_workspace = T){
  if(missing(concept_id)&!missing(concept_page)){concept_id <- stringr::str_extract(concept_page,'[A-Za-z0-9]+$')}
  if(missing(source_id)&!missing(source_page)){source_id <- stringr::str_extract(source_page,'[A-Za-z0-9]+$')}
  if(missing(source_id)&missing(concept_id)){stop("Must specify a concept and/or a source to query (using page or id)")}
  if(missing(dest_file)&return_to_workspace==F){stop("Must specify a file destination to save the result or set return = T")}
  works_base <- 'https://api.openalex.org/works'
  url <- parse_url(works_base)
  if(!is.null(mailto)){
    url$query$mailto<-mailto
    }
  if(cursor){
    url$query$cursor<-"*"
    }
  if(!is.null(per_page)){
    if(per_page>200){per_page = 200;print('openAlex allows up to 200 results per page, lowering to per_page = 200')}
    url$query$`per-page`<-per_page
    }
  if(!missing(concept_id)){
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
  while(p==1|ifelse(!exists('js'),T,!is.null(js$meta$next_cursor))){
    print(paste('querying page',p))
    ## json return
    js <- jsonlite::read_json(qurl)
    ### js$results is list with length = per_page (or less if fewer than per_page works are returned)
    if(p==1){
      if(js$meta$count>override){
        stop(paste0('more than ',override,' works returned, set a higher limit or make a finer query'))
      }
      else{print(paste0(js$meta$count,' works found'))}
    }
    store_results <- append(store_results,js$results)
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
    saveRDS(object = processed_dt, file = dest_file)
  }
  if(return_to_workspace){return(processed_dt)}
  }
}
