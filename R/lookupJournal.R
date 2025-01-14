#' Find a journal in openAlex using issn code
#'
#' @param issn a string issn value (see https://docs.openalex.org/api-entities/sources/get-a-single-source)
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @description Primary use of this function is to get internal (OpenAlex) venue ID for a journal
#' @export
#' @importFrom jsonlite read_json
#' @importFrom stringr str_replace_all
#' @importFrom httr parse_url build_url

lookupJournal <- function(issn = NULL,mailto = NULL){
  ### takes the form: https://api.openalex.org/sources/issn:2041-1723
  journal_base <- 'https://api.openalex.org/sources'
  url <- parse_url(journal_base)
  if(!is.null(mailto)){url$query$mailto<-mailto}
  if(!is.null(issn)){url$query$filter1<-paste0('issn:',issn)}else{stop('please provide an ISSN value')}
  ### currently only supports one ISSN at a time
  #if(length(type)>1){type <- paste(type,collapse = 'OR')}
  #if(length(url$query$filter)>1){url$query$filter<-paste(url$query$filter,collapse = '&')}
  qurl <- build_url(url)
  qurl <- str_replace_all(qurl,'filter[1-2]','filter')
  return(read_json(qurl))
}
