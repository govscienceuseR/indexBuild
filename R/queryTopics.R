#' Find topics and topic ids for use in openAlex
#'
#' @param topic_string string for searching topic display names (openAlex stems by default)
#' @param per_page how many results to return per query page
#' @param mailto email address to allow priority (polite pool) query
#' @param api_key optional OpenAlex API key; if NULL the \code{OPENALEX_API_KEY} environment variable is used (see \code{\link{performOA}})
#' @param variables scalar fields to return in the data.table
#' @description Search the openAlex \code{/topics} endpoint. Topics are openAlex's
#'   replacement for the deprecated Concepts classification (see
#'   \code{\link{queryConcepts}}). Unlike concepts, topics are not organized by a
#'   numeric \code{level}; instead each topic sits in a
#'   domain > field > subfield > topic hierarchy, whose display names are returned
#'   as extra columns. The resulting topic \code{id} (e.g. \code{T13459}) can be
#'   passed to \code{\link{extractWorks}} via \code{topic_id}.
#' @return data.table of candidate topic matches
#' @export
#' @importFrom jsonlite read_json
#' @import httr
#' @import data.table
#' @examples
#' \dontrun{
#' topics <- queryTopics(topic_string = 'public administration')
#' extractWorks(topic_id = topics$id[1], from_date = 2020, to_date = 2021)
#' }

queryTopics <- function(topic_string = NULL, per_page = 25, mailto = NULL,
                        api_key = NULL,
                        variables = c('id','display_name','description','works_count','cited_by_count')){
  purl <- parse_url('https://api.openalex.org/topics')
  if(!is.null(mailto)){purl$query$mailto <- mailto}
  if(!is.null(topic_string)){purl$query$filter <- paste0('display_name.search:',topic_string)}
  if(!is.null(per_page)){purl$query$`per-page` <- per_page}
  url <- build_url(purl)
  jresult <- readOA(url, api_key = api_key)
  dt <- rbindlist(lapply(jresult$results,function(x){
    base <- as.data.table(x[variables])
    ## flatten the domain > field > subfield hierarchy display names into columns
    hier <- data.table(
      subfield = if(is.null(x$subfield$display_name)) NA_character_ else x$subfield$display_name,
      field    = if(is.null(x$field$display_name))    NA_character_ else x$field$display_name,
      domain   = if(is.null(x$domain$display_name))   NA_character_ else x$domain$display_name)
    cbind(base,hier)
  }),fill = T,use.names = T)
  return(dt)
}
