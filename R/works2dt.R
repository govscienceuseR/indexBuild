#' takes a json formatted list of works associated with a host venue and outputs a data.table of simple citation information
#' @description takes a json formatted list of works associated with a host venue and outputs a data.table of simple citation information
#' @param json_result a list output by calling extractVenues.R or extractConcepts.R
#' @return a data.table object with one work per row
#' @import data.table
#' @export

works2dt <- function(json_result){
  if(class(json_result)=='character'){json_result <- streamGZ(json_result)}
  if(!class(json_result)%in%c('data.table','data.frame')){stop('expected a data.frame or a data.table, did not get it')}
  dt <- data.table()
  single_vars <- c('id','doi','type','publication_date','cited_by_count')
  for(v in single_vars){
    dt[,(v):=list2vector(json_result,v,fill = NA)]
  }
  host_items <- c('id','display_name','issn_l','publisher')
  host_results <- json_result$host_venue[,host_items]
  names(host_results) <- paste0('host.',names(host_results))
  dt <- cbind(dt,host_results)

  author_items <- c('id','display_name')
  author_results <- lapply(author_items,function(v){
  sapply(json_result$authorships,function(x) paste(unlist(x[['author']][[v]]),collapse = ';'))
  })
  author_results_dt <- (as.data.table(author_results))
  names(author_results_dt) <- author_items
  dt <- cbind(dt,author_results_dt)
  dt$is_oa <- unlist(json_result$open_access$is_oa)
  dt$oa_status <- unlist(json_result$open_access$oa_status)
  return(dt)
}
