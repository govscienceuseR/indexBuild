#' takes a json formatted list of works associated with a host venue and outputs a data.table of simple citation information
#'
#' @description takes a json work returned by an openAlex query and returns a 1 by X data.table of citation information
#' @param result a result output from a query function
#' @return a data.table object with one work per row
#' @export
#' @importFrom methods is
#' @import data.table

processOA <- function(response,keycol = 'work.id'){
  if(response$meta$count==0|!is.null(response$error)){return(NULL)}else{
    return(tryCatch(purrr::reduce(list(extractWork(response$results),
                                       extractBiblio(response$results),
                                       extractAuthors(response$results),
                                       extractSource(response$results),
                                       extractOpenAccess(response$results),
                                       extractConcepts(response$results)),merge, by = keycol),error = function(e) NULL))}}

### takes set of results from query, turns works info into 1-line-per-result dt
extractWork <- function(x,vars = c('id','doi','title',
                                   'publication_date','type','cited_by_count',
                                   'is_paratext','updated_date')){
  lapply(x,function(w) as.data.table(w[vars]))
  rbindlist(lapply(x,function(w){
    temp <- as.data.table(w[vars])
    temp$work.id <- basename(temp$id)
    temp[,id:=NULL]
    temp}),use.names = T,fill = T)}

#### takes set of results from query, turns source info into 1-line-per-result dt
extractSource <- function(x,vars = c('id','display_name','issn_l','host_organization','type')){
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
