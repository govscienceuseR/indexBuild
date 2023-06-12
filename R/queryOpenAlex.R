#' Search for and return candidate matches to a title in openAlex
#'
#' @param url the base url for openAlex query
#' @param title a string name for title https://docs.openalex.org/api
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @param wait_time how long to wait on query results before timing out, in seconds
#' @param max_results how many results to return (uses "per_page" feature on OA to grab first page of results)
#' @description Primary use of this function is to make a query object for use in openAlex that returns potential matches to a title string
#' @export
#' @import httr
#' @import jsonlite
#' @import magrittr
#' @import data.table
#' @importFrom purrr reduce
#'

queryOpenAlex <- function(url = "https://api.openalex.org/works",title = NULL,mailto = NULL,wait_time = 5,max_results = 5){
  query = generateTitleQuery(title = title,mailto = mailto,max_results = max_results,url = url)
  response <- tryCatch(httr::GET(query,httr::timeout(wait_time)),error = function(e) NULL)
  if(is.null(response)){stop(paste("query",query,"failed"))}
  if(!is.null(response)){
    json_response<-httr::content(response,as="parsed")
    result<-processOA(response = json_response)
    return(result)
  }
}
queryOpenAlex <- Vectorize(queryOpenAlex,vectorize.args = 'title')

generateTitleQuery = function(url = "https://api.openalex.org/works",title = NULL,mailto = NULL,max_results = 5){
  ### note, results are automatically sorted by relevance so return is best matches ###
  url <- parse_url(url)
  url$query <- list(mailto = mailto,search = title,per_page = max_results,sort = 'relevance_score:desc')
  url$scheme <- "https"
  url <- build_url(url)
  return(url)
}
#generateTitleQuery <- Vectorize(generateTitleQuery, vectorize.args = "title")


processOA <- function(response,keycol = 'work.id'){
  if(response$meta$count==0|!is.null(response$error)){return(NULL)}else{
    return(tryCatch(reduce(list(extractWork(response$results),
                                       extractBiblio(response$results),
                                       extractAuthors(response$results),
                                       extractSource(response$results),
                                       extractOpenAccess(response$results),
                                       extractConcepts(response$results)),merge, by = keycol),error = function(e) NULL))}}

### takes set of results from query, turns works info into 1-line-per-result dt
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

#### takes set of results from query, turns source info into 1-line-per-result dt
extractSource <- function(x,vars = c('id','display_name','issn_l','host_organization','type')){
  ##### this is a data.table issue ###
  ### binding for global variables ###
  ### see: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  id <- host_organization  <- NULL
  ### right now, this just gets first location ###
  rbindlist(lapply(x,function(l){
    temp = as.data.table(l$primary_location$source[vars])
    setnames(temp,c('display_name','issn_l','type'), paste0('source.',c('display_name','issn_l','type')))
    temp$work.id = basename(l$id)
    temp$source.id = ifelse(is.null(temp$id),NA,basename(temp$id))
    temp$host.id = ifelse(is.null(temp$host_organization),NA,basename(temp$host_organization))
    temp[,id:=NULL]
    if('host_organziation' %in% colnames(temp)){temp[,host_organization:=NULL]}
    temp
  }),use.names = T,fill = T)
}

extractBiblio <- function(x) {
  rbindlist(lapply(seq_along(x),function(b) {temp = as.data.table(x[[b]]$biblio); temp$work.id = basename(x[[b]]$id);temp}),use.names = T,fill = T)
}

extractAuthors <- function(x) {
  rbindlist(lapply(x,function(a){
    temp<- apply(rbindlist(lapply(a$authorships,function(y) {yt = as.data.table(y$author);yt$id = basename(yt$id);yt}),use.names = T,fill = T),2,function(p) {paste(p,collapse = ';')},simplify = F)
    temp <- as.data.table(temp)
    setnames(temp,c('id','display_name'),c('author.ids','author.display_names'))
    temp$work.id <- basename(a$id)
    temp}),use.names = T,fill = T)}

extractOpenAccess <- function(x) rbindlist(lapply(x,function(b) {
  temp = as.data.table(b$open_access);
  temp$work.id = basename(b$id)
  temp}),use.names = T,fill = T)

extractConcepts <- function(x) {rbindlist(lapply(x,function(b) {
  data.table(work.id =basename(b$id),
             concept.ids = paste(basename(sapply(b$concepts,'[[','id')),collapse = ';'),
             concept.scores = paste(sapply(b$concepts,function(d){round(d$score,3)}),collapse = ';'))}),
  use.names = T,fill = T)}

