#' takes a json formatted list of works associated with a host venue and outputs a data.table of simple citation information
#' @description takes a json formatted list of works associated with a host venue and outputs a data.table of simple citation information
#' @param venueworks_list a list output by calling extractVenues.R
#' @return a data.table object with one work per row
#' @import data.table
#' @export
venueworks2dt <- function(venueworks_list){
  dt = data.table()
  dt$id <- sapply(venueworks_list,'[[','id')
  dt$doi <- sapply(venueworks_list,'[[','doi')
  dt$host_venue <- lapply(venueworks_list,'[[','host_venue')
  dt$type <- sapply(venueworks_list,'[[','type')
  dt$publication_date <- sapply(venueworks_list,'[[','publication_date')
  dt$open_access <- lapply(venueworks_list,'[[','open_access')
  dt$cited_by_count <- sapply(venueworks_list,'[[','cited_by_count')
  dt$authorships <- lapply(venueworks_list,'[[','authorships')
  dt$authors <- flattenAuthorNames(dt$authorships)
  dt[,authorships:=NULL]
  dt$journal_title <- sapply(dt$host_venue,"[[",'display_name')
  dt$publisher <- sapply(dt$host_venue,"[[",'publisher')
  dt$venue_id <- sapply(dt$host_venue,"[[",'id')
  dt$issn_l <- sapply(dt$host_venue,"[[",'issn_l')
  dt[,host_venue:=NULL]
  dt$is_oa <- sapply(dt$open_access,"[[",'is_oa')
  dt$oa_status <- sapply(dt$open_access,"[[",'oa_status')
  dt[,open_access:=NULL]
  return(dt)
}
