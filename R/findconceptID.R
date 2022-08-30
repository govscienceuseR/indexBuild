#' Find concepts and ids for use in openAlex
#'
#' @param query_string string or regular expression for searching concept descriptions
#' @description Search spreadsheet of openAlex concept tree (see https://docs.openalex.org/about-the-data/concept). Current google sheet url is: https://docs.google.com/spreadsheets/d/1LBFHjPt4rj_9r0t0TTAlT68NwOtNH8Z21lBMsJDMoZg/edit#gid=1473310811
#' @return datatable of results
#' @importFrom stringr str_detect
#' @importFrom jsonlite read_json
#' @import httr
#' @import data.table
#' @export
#' @details NOTE: note that https://api.openalex.org/concepts doesn't seem to tolerate regex at this point, so that needs to be done with the output

find_concepts <- function(query_string = NULL,maxlevel = 10,per_page = 10,mailto = NULL,max_return = 100,vs = c('id','display_name','level','works_count','works_api_url','description')){
  purl <- parse_url('https://api.openalex.org/concepts?')
  if(!is.null(mailto)){purl$query$mailto<-mailto}
  if(!is.null(query_string)){purl$query$filter<-paste0('display_name.search:',query_string)}
  if(!is.null(per_page)){purl$query$`per-page`<-per_page}
  if(!is.null(maxlevel)){purl$query$filter <- paste0(purl$query$filter,',level:<',maxlevel+1)}
  url <- build_url(purl)
  jresult <- read_json(url)
  dt <- rbindlist(lapply(jresult$results,function(x) as.data.table(x[vs])),fill = T,use.names = T)
  return(dt)
}
