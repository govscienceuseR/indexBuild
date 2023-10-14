#' takes a json formatted list of works associated with a host venue and outputs a data.table of simple citation information
#'
#' @description takes a json formatted openAlex works object returns a data.table of simple citation information
#' @param json_result a list output by calling extractVenues.R or extractConcepts.R
#' @return a data.table object with one work per row
#' @export
#' @importFrom methods is
#' @import data.table
#' @param data_style options for how much/how little data to return, see @details
#' @details Note that because extracted records can be pretty large--and are complicated, nested json file--there is an optional "data_style" command that lets the user specify what to return. Currently there are three options: (1) bare_bones returns OpenAlex ID + DOI, basically, results that can be used to look up the work again; (2) citation returns typical citation information, like journal name, author, etc., with a couple bonus items like source.id to link back to openAlex (3) custom can be tailored to suit researcher aims (note this likely requires building a funciton or two to parse a nested list of interest); and (4) all returns the entire result in original json format.


work2dt <- function(work = NULL,style = c('bare_bones','citation','custom','all')){
  dt <- data.table()
  bare_bones <- c('id','doi')
  citation <- c(bare_bones,'author.display_name','publication_year','display_name','source.display_name','source.id','volume','issue','first_page','last_page')
  custom <- c(citation,'is_oa','source.is_oa','author.institutions.id','author.institutions.type','author.institutions.country_code','type','cited_by_count','grants.funder.id','grants.funder_display_name','source.issn_l')
  all <- NULL
  if(style == 'all'){stop('returning all data as flat file currently not supported, please select another style')}
  if(style!='bare_bones'){
    source_info <- parseLocationObject(work)

  mini_work <- work[sapply(work,length)<2]
  work2 <- cbind(work,source_info)



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
}}



