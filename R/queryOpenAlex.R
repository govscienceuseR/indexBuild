#' Search for and return candidate matches to a title in openAlex
#'
#' @param title a string name for title https://docs.openalex.org/api
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @param wait_time how long to wait on query results before timing out, in seconds
#' @param max_results how many results to return (uses "per_page" feature on OA to grab first page of results)
#' @param url the base url for openAlex query
#' @description Primary use of this function is to make a query object for use in openAlex that returns potential matches to a title string
#' @examples
#' data(titles)
#' #query a single titles
#' queryOpenAlex(titles[22])
#' @import httr
#' @import jsonlite
#' @import magrittr
#' @import data.table
#' @export
queryOpenAlex <- function(title = NULL,mailto = NULL,wait_time = 5,max_results = 5,url = "https://api.openalex.org/works"){
  query <- generateTitleQuery(title = title,mailto = mailto,max_results = max_results,url = url)
  response <- GET(query,timeout(wait_time))#,error = function(e) NULL)
  code <- status_code(response)
  if(code != 200){result <- NULL}
  if(code == 200){
    json_response<-content(response,as="parsed")
    if(json_response$meta$count == 0){
      result <- data.table(title = title,query = query)
    }
    if(json_response$meta$count > 0){
      json_response$results <- json_response$results[!duplicated(sapply(json_response$results,'[[','id'))]
      result <- processOA(response = json_response)
    }
  }
  return(result)
}

#' Vectorized search for and return candidate matches to a title in openAlex
#'
#' @description Same function but can take a set of titles. Not advisable for large data sets, parallelize queryOpenAlex instead.
#' @param title a vector of title strings https://docs.openalex.org/api
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @param wait_time how long to wait on query results before timing out, in seconds
#' @param max_results how many results to return (uses "per_page" feature on OA to grab first page of results)
#' @param url the base url for openAlex query
#' @export
queriesOpenAlex <- Vectorize(queryOpenAlex,vectorize.args = 'title',SIMPLIFY = F)



#' Take a title string and make a query
#'
#' @param title a string name for title https://docs.openalex.org/api
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @param max_results how many results to return (uses "per_page" feature on OA to grab first page of results)
#' @param url the base url for openAlex query
#' @description Primary use of this function is to make a query url for openAlex
#' @import httr
#' @export
generateTitleQuery = function(title = NULL,mailto = NULL,max_results = 5,url = "https://api.openalex.org/works"){
  ### note, results are automatically sorted by relevance so return is best matches ###
  url <- parse_url(url)
  url$query <- list(mailto = mailto,search = title,per_page = max_results,sort = 'relevance_score:desc')
  url$scheme <- "https"
  url <- build_url(url)
  return(url)
}
#generateTitleQuery <- Vectorize(generateTitleQuery, vectorize.args = "title")

#' Take a json query result from openAlex and parse it into a flat data.frame
#'
#' @param response a json object obtained from an openAlex query
#' @param keycol character string representing column of ID values
#' @import data.table
#' @importFrom purrr reduce
#' @description Primary use of this function is to generate a flat file of results to include in search index
#' @export
processOA <- function(response,keycol = 'work.id'){
    return(tryCatch(reduce(list(extractWork(response$results),
                                       extractBiblio(response$results),
                                       extractAuthors(response$results),
                                       extractSource(response$results),
                                       extractOpenAccess(response$results),
                                       extractConcepts(response$results)),merge, by = keycol),error = function(e) NULL))}

#' Takes set of results from query, turns works info into 1-line-per-result dt
#'
#' @param x a parsed json list obtained via openAlex query
#' @param vars variable names to pull from sublist
#' @import data.table
#' @export
extractWork <- function(x,vars = c('id','doi','title',
                                   'publication_date','type','cited_by_count',
                                   'is_paratext','updated_date')){
  ##### this is a data.table issue ###
  ### binding for global variables ###
  ### see: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
   id <- NULL
  lapply(x,function(w) as.data.table(w[vars]))
  rbindlist(lapply(x,function(w){
    temp <- as.data.table(w[vars])
    temp$work.id <- basename(temp$id)
    temp[,id:=NULL]
    temp}),use.names = T,fill = T)}

#' Takes set of results from query, turns source info into 1-line-per-result dt
#'
#' @param x a parsed json list obtained via openAlex query
#' @param vars variable names to pull from sublist
#' @import data.table
#' @export
extractSource <- function(x,vars = c('id','display_name','issn_l','host_organization','type')){
  ##### this is a data.table issue ###
  ### binding for global variables ###
  ### see: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  host_organization  <- NULL
  ### right now, this just gets first location ###
  rbindlist(lapply(x,function(l){
    temp = as.data.table(l$primary_location$source[vars])
    setnames(temp,c('display_name','issn_l','type'), paste0('source.',c('display_name','issn_l','type')),skip_absent = T)
    temp$work.id = basename(l$id)
    temp$source.id = ifelse(is.null(temp$id),NA,basename(temp$id))
    temp$host.id = ifelse(is.null(temp$host_organization),NA,basename(temp$host_organization))
    if('host_organization' %in% colnames(temp)){temp[,host_organization:=NULL]}
    temp
  }),use.names = T,fill = T)
}

#' Takes set of results from query, turns bibliometric info into 1-line-per-result dt
#'
#' @param x a parsed json list obtained via openAlex query
#' @import data.table
#' @export
extractBiblio <- function(x) {
  rbindlist(lapply(seq_along(x),function(b) {temp = as.data.table(x[[b]]$biblio); temp$work.id = basename(x[[b]]$id);temp}),use.names = T,fill = T)
}

#' Takes set of results from query, turns author info into 1-line-per-result dt
#'
#' @param x a parsed json list obtained via openAlex query
#' @import data.table
#' @export
extractAuthors <- function(x) {
  rbindlist(lapply(x,function(a){
    temp<- apply(rbindlist(lapply(a$authorships,function(y) {yt = as.data.table(y$author);yt$id = basename(yt$id);yt}),use.names = T,fill = T),2,function(p) {paste(p,collapse = ';')},simplify = F)
    temp <- as.data.table(temp)
    setnames(temp,c('id','display_name'),c('author.ids','author.display_names'),skip_absent = T)
    temp$work.id <- basename(a$id)
    temp}),use.names = T,fill = T)}

#' Takes set of results from query, turns access info into 1-line-per-result dt
#'
#' @param x a parsed json list obtained via openAlex query
#' @import data.table
#' @export
extractOpenAccess <- function(x) rbindlist(lapply(x,function(b) {
  temp = as.data.table(b$open_access);
  temp$work.id = basename(b$id)
  temp}),use.names = T,fill = T)

#' Takes set of results from query, turns concept info into 1-line-per-result dt
#'
#' @param x a parsed json list obtained via openAlex query
#' @import data.table
#' @export
extractConcepts <- function(x) {rbindlist(lapply(x,function(b) {
  data.table(work.id =basename(b$id),
             concept.ids = paste(basename(sapply(b$concepts,'[[','id')),collapse = ';'),
             concept.scores = paste(sapply(b$concepts,function(d){round(d$score,3)}),collapse = ';'))}),
  use.names = T,fill = T)}

