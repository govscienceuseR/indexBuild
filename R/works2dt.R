#' takes a json formatted list of works associated with a host venue and outputs a data.table of simple citation information
#' @description takes a json formatted list of works associated with a host venue and outputs a data.table of simple citation information
#' @param works_list a list output by calling extractVenues.R or extractConcepts.R
#' @return a data.table object with one work per row
#' @import data.table
#' @export
works2dt <- function(works_list){
  dt = data.table()
  dt$id <- sapply(works_list,'[[','id')
  dt$doi <- sapply(works_list,'[[','doi')
  dt$host_venue <- lapply(works_list,'[[','host_venue')
  dt$type <- sapply(works_list,'[[','type')
  dt$publication_date <- sapply(works_list,'[[','publication_date')
  dt$open_access <- lapply(works_list,'[[','open_access')
  dt$cited_by_count <- sapply(works_list,'[[','cited_by_count')
  dt$authorships <- lapply(works_list,'[[','authorships')
  dt$authors <- flattenAuthorNames(dt$authorships)
  dt$author_institutions <- flattenAuthorInsitutions(dt$authorships,'display_name')
  dt$author_institutions_ror <- flattenAuthorInsitutions(dt$authorships,'ror')
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
