#' takes a json formatted list of works associated with a host venue and outputs a data.table of simple citation information
#' @description takes a json formatted list of works associated with a host venue and outputs a data.table of simple citation information
#' @param json_result a list output by calling extractVenues.R or extractConcepts.R
#' @return a data.table object with one work per row
#' @export
#' @importFrom methods is
#' @import data.table

works2dt <- function(json_result){
  if(is(json_result,'character')){json_result <- streamGZ(json_result)
  json_result <- data.table(json_result)}
  if(!is(json_result,'data.table')){stop('expected a data.frame or a data.table, did not get it')}
  dt <- data.table()
  single_vars <- c('id','title','doi','type','publication_year','cited_by_count')
  for(v in single_vars){
    dt[,(v):=list2vector(json_result,v,fill = NA)]
  }
  host_items <- c('id','display_name','issn_l','publisher')
  host_results <- data.table(json_result$host_venue[,host_items])
  for(h in host_items){
    host_results[,(h):=list2vector(host_results,h,fill = NA)]
    }
  names(host_results) <- paste0('host.',names(host_results))
  dt <- cbind(dt,host_results)

  author_items <- c('id','display_name')
  author_results <- lapply(author_items,function(v){
  sapply(json_result$authorships,function(x) paste(unlist(x[['author']][[v]]),collapse = ';'))
  })
  author_results_dt <- (as.data.table(author_results))
  names(author_results_dt) <- paste0('author.',author_items)
  dt <- cbind(dt,author_results_dt)

  oa <- json_result$open_access
  oa$oa_status[sapply(oa$oa_status,length)==0] <- NA
  oa$is_oa[sapply(oa$is_oa,length)==0] <- NA
  dt$is_oa <- unlist(oa$is_oa)
  dt$oa_status <- unlist(oa$oa_status)

   return(dt)
}
