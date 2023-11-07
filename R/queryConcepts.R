#' Find concepts and concept ids for use in openAlex
#'
#' @param concept_string string or regular expression for searching concept descriptions
#' @param maxlevel most detailed level of concept tree to return (lower is more general, higher is more specific)
#' @param per_page how many results to return per query page
#' @param mailto email address to allow priority query
#' @param max_return maximum number of concepts to return
#' @param variables to return in data.table
#' @description Search spreadsheet of openAlex concept tree (see https://docs.openalex.org/about-the-data/concept). Current google sheet url is: https://docs.google.com/spreadsheets/d/1LBFHjPt4rj_9r0t0TTAlT68NwOtNH8Z21lBMsJDMoZg/edit#gid=1473310811
#' @return datatable of results
#' @export
#' @details NOTE: note that https://api.openalex.org/concepts doesn't seem to tolerate regex at this point, so that needs to be done with the output
#' @importFrom stringr str_detect
#' @importFrom jsonlite read_json
#' @import httr
#' @import data.table

queryConcepts <- function(concept_string = NULL,maxlevel = 10,per_page = 10,mailto = NULL,max_return = 100,variables = c('id','display_name','level','works_count','works_api_url','description')){
  purl <- parse_url('https://api.openalex.org/concepts?')
  if(!is.null(mailto)){purl$query$mailto<-mailto}
  if(!is.null(concept_string)){purl$query$filter<-paste0('display_name.search:',concept_string)}
  if(!is.null(per_page)){purl$query$`per-page`<-per_page}
  if(!is.null(maxlevel)){purl$query$filter <- paste0(purl$query$filter,',level:<',maxlevel+1)}
  url <- build_url(purl)
  jresult <- read_json(url)
  dt <- rbindlist(lapply(jresult$results,function(x) as.data.table(x[variables])),fill = T,use.names = T)
  return(dt)
  Sys.sleep(2)
}
