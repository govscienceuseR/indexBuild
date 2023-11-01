#' Find sources (journals) and source ids for use in openAlex
#'
#' @param source a string source name https://docs.openalex.org/api
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @param type which type of source should be included in query, defaults to all
#' @description Primary use of this function is to get source ID for use in API
#' @export
#' @example man/examples/sources.R
#' @import jsonlite
#' @import stringr
#' @import httr

querySources <- function(source = NULL,mailto = NULL,type = NULL){
  # type options are c('journal','repository','conference','ebook platform')
  source_base <- 'https://api.openalex.org/sources'
  url <- parse_url(source_base)
  if(!is.null(mailto)){url$query$mailto<-mailto}
  if(!is.null(source)){url$query$filter1<-paste0('display_name.search:',source)}
  if(length(type)>1){type <- paste(type,collapse = 'OR')}
  if(!is.null(type)){url$query$filter2<-paste0('type.',type)}
  #if(length(url$query$filter)>1){url$query$filter<-paste(url$query$filter,collapse = '&')}
  qurl <- build_url(url)
  qurl <- str_replace_all(qurl,'filter[1-2]','filter')
  return(jsonlite::read_json(qurl))
}
