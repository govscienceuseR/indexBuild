#' Find venues (journals) and venue ids for use in openAlex
#'
#' @param venue a string venue name https://docs.openalex.org/api
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @param type which type of venue should be included in query, defaults to all
#' @description Primary use of this function is to get venue ID for use in API
#' @export
#' @import jsonlite
#' @import stringr
#' @import httr

queryVenues <- function(venue = NULL,mailto = NULL,type = NULL){
  # type options are c('journal','repository','conference','ebook platform')
  venue_base <- 'https://api.openalex.org/venues'
  url <- parse_url(venue_base)
  if(!is.null(mailto)){url$query$mailto<-mailto}
  if(!is.null(venue)){url$query$filter1<-paste0('display_name.search:',venue)}
  if(length(type)>1){type <- paste(type,collapse = 'OR')}
  if(!is.null(type)){url$query$filter2<-paste0('type.',type)}
  #if(length(url$query$filter)>1){url$query$filter<-paste(url$query$filter,collapse = '&')}
  qurl <- build_url(url)
  qurl <- str_replace_all(qurl,'filter[1-2]','filter')
  return(jsonlite::read_json(qurl))
}
