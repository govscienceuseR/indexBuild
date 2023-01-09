#' Find venues (journals) and venue ids for use in openAlex
#'
#' @param venue a string venue name https://docs.openalex.org/api
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @description Primary use of this function is to get venue ID for use in API
#' @export
#' @import jsonlite
#' @import stringr
#' @import httr

queryVenues <- function(venue_string = NULL,mailto = NULL){
  venue_base <- 'https://api.openalex.org/venues'
  url <- parse_url(venue_base)
  if(!is.null(mailto)){url$query$mailto<-mailto}
  if(!is.null(venue_string)){url$query$filter<-paste0('display_name.search:',venue_string)}
  qurl <- build_url(url)
  return(jsonlite::read_json(qurl))
}
