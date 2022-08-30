#' Read in json.gz file, select data needed for citationSearch, and generate data.table
#'
#' @param jfile a json.gz file (NOTE: easy to make this adaptable, will do...)
#' @param file_location can either take a workspace object (location = 'workspace') or a saved json.gz file (location = 'disk'))
#' @description openAlex queries produce tons of data. queryWorks retains a subset for saving as a json.gz, but these data are still way more than is needed for citationSearch. json2dt reads in an openAlex concept search result file, selects necessary metadata items, and generates a data.table object where each OA work is one row, with columns appropriately titled for use as citationSearch index.
#' @export
#'
json2dt <- function(jfile,source='openAlex',file_location = 'workspace'){
  if(file_location=='workspace'){temp <- jfile}
  if(file_location=='disk'){temp <- streamGZ(jfile)}
  dt <- data.table(Title = character(), Authors = character(), Year = character(),
                   Publisher = character(), Source = character(), Misc = character(),
                   `Journal Title` = character(), DOI= character())
  dt <- cbind(dt,temp$id)
  author_sets <- (lapply(temp$authorships,'[[','author'))
  dt$Authors <- sapply(lapply(author_sets,'[[','display_name'),paste,collapse = '; ')
  dt$DOI <- temp$doi
  dt$Year <- temp$publication_year
  dt$Title <- temp$title
  dt$`Journal Title`<-temp$host_venue$display_name
  dt$Publisher <- temp$host_venue$publisher
  dt$Source <- source
  rm(temp)
  return(dt)
}
