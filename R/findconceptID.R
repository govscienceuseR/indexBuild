#' Find concepts and ids for use in openAlex
#'
#' @param query_string string or regular expression for searching concept descriptions
#' @description Search spreadsheet of openAlex concept tree (see https://docs.openalex.org/about-the-data/concept). Current google sheet url is: https://docs.google.com/spreadsheets/d/1LBFHjPt4rj_9r0t0TTAlT68NwOtNH8Z21lBMsJDMoZg/edit#gid=1473310811
#' @return dataframe of results
#' @importFrom stringr str_detect
#' @details NOTE: this function uses the Aug 2022 concept sheet
find_concepts <- function(query_string,which_column = 'normalized_name',maxlevel = Inf){
  oa_concepts[str_detect(as.name(which_column),query_string)&level<=maxlevel,][order(level),]
}
