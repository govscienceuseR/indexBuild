#' Extract works associated with a concept in openAlex
#'
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
#' @param file location to save output as a json.gz
#' @param reduce boolean for whether to reduce scope of final results, see @details
#' @description Primary use is to query a concept ID and extract associated works, e.g., all article records associated with "habitat"
#' @details Note that because extracted records can be pretty large, there is an optional "reduce" command that selects out a subset of key variables before saving or returning the final json list
#' @export
#' @import jsonlite
#' @import stringr

extractConcepts <- function(mailto = NULL,concept_id = NULL,concept_page = NULL,cursor = T,per_page = NULL,to_date = NULL,from_date = NULL,keep_paratext = FALSE,debug = FALSE,sleep_time = 0.1){
  if(missing(concept_id)&missing(concept_page)){stop("Must specify a concept_id string or the https page for a concept")}
  works_base <- 'https://api.openalex.org/works'
  url <- parse_url(works_base)
  if(!is.null(mailto)){url$query$mailto<-mailto}
  if(cursor){url$query$cursor<-"*"}
  if(!is.null(per_page)){url$query$`per-page`<-per_page}
  if(missing(concept_id)){url$query$filter$concept.id<-stringr::str_extract(concept_page,'[A-Za-z0-9]+$')}
  if(!missing(concept_id)){url$query$filter$concept.id<-concept_id}
  if(!missing(from_date)){
    from_date <- if(nchar(from_date)==4){paste(from_date,'01','01',sep = '-')}
    url$query$filter$from_publication_date<-from_date}
  if(!missing(to_date)){
    to_date <- if(nchar(to_date)==4){paste(to_date,'12','31',sep = '-')}
    url$query$filter$to_publication_date<-to_date}
  if(!keep_paratext){url$query$filter$is_paratext<-"false"}
  if(length(url$query$filter)>1){
  url$query$filter<-paste(paste0(paste0(names(url$query$filter),':'),url$query$filter),collapse = ',')
  }
  qurl <- build_url(url)
  if(debug){return(qurl)}
  if(!debug){
  p = 1
  print(paste('querying page',p))
  js <- read_json(qurl)
  temp_js_list <- list()
  temp_js_list <- append(x = temp_js_list,js$result)

  while(!is.null(js$meta$next_cursor)){
  p <- p + 1
  print(paste('querying page',p))
  url$query$cursor<-js$meta$next_cursor
  qurl <- build_url(url)
  js <- read_json(qurl)
  temp_js_list <- append(x = temp_js_list,js$result)
  Sys.sleep(sleep_time)
  }
  if(reduce){
    temp_js_list <- lapply(temp_js_list, function(x) x[c('id','title','doi','host_venue','cited_by_count','is_paratext','open_access','publication_year','authorships','type')])
  }
  if(!is.null(file)){
    print(paste('saving result to',file))
    json_object <- toJSON(temp_js_list)
    write(json_object, file=gzfile(file))
  }
  return(temp_js_list)
  }
}
