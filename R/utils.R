#' simple wrapper to combine stream_in and gzfile to read json.gz OA files
#'
#' This function just combines two existing functions. Stream_in is preferred over normal read functions because the files are very large.
#' @param x a json.gz file
#' @return a decompressed json file
#' @export
#' @import jsonlite

streamGZ <- function(x){stream_in(gzfile(x))}

#' this is just a shorthand to go grab single entry items from json result lists
#' @param list a list of json openAlex results
#' @param var the variable name to grab from each list entry
#' @param fill replaces empty values with NA before collapsing to vector


list2vector <- function(list,var,fill = NA){
  return(unlist(lapply(list[[var]],function(x) ifelse(length(x)==0,fill,x))))
}

#' this is just a shorthand to go into lists within lists and grab items
#' @param list a list of json openAlex results
#' @param subvar the name of the 2nd level list
#' @param var the name of the variable to pull out from the 2nd level list
#' @param fill replaces empty values with NA before collapsing to vector
subitems2vector <- function(list,var,subvar,fill = NA){
  unlist(lapply(seq_along(list),function(i) ifelse(is.null(list[[i]][[var]][[subvar]]),NA,list[[i]][[var]][[subvar]])))
}



#' openAlex provides A LOT of author info stored in json. this goes into the nested structure and pulls out just the author names
#' @param authors_json a list in json structure that contains openAlex works
#' @return a vector of strings, 1 string for each work, of author names pasted together separated by ";"
#' @export
flattenAuthorNames <- function(authors_json){
  sapply(sapply(lapply(authors_json,'[[','author'),'[[','display_name'),paste,collapse=';')
}

#' openAlex provides A LOT of author info stored in json. this goes into the nested structure and pulls out just the author institutions
#' @param authors_json a list in json structure that contains openAlex works
#' @param institutional_var the variable to return for institution, either 'ror' (ROR ID#) or 'display name'
#' @return a vector of strings, 1 string for each work, of author names pasted together separated by ";"
#' @export
flattenAuthorInstitutions <- function(authors_json,institutional_var = NULL){
  if(missing(institutional_var)){stop("must specify insitutional variable like 'ror' or 'display name'")}
  sapply(sapply(lapply(lapply(authors_json,'[[','author'),'[[','institutions'),'[[',institutional_var),paste,collapse=';')
}

#' openAlex stores information about a work's location(s) as a list nested in the larger works object. This function takes the works object and returns a flatted version of the location object which can be "cbinded" with the works object.
#' @param work an openAlex works object
#' @param primary a boolean value, default = TRUE, for whether to return _all_ locations (FALSE) or the primary location (TRUE) (see https://docs.openalex.org/api-entities/works/work-object/location-object)
#' @return a flattened data.table of location info
#' @export
parseLocationObject <- function(work = NULL, primary = TRUE){
  if(missing(work)){stop('please provide an openAlex works object')}
  if(primary){
    temp = work[['primary_location']]
    temp2 <- temp[sapply(temp,length)<2]
    if(!is.null(temp$source)){
      temp3 <- temp$source[sapply(temp,length)<2]
      keep <- c('id','display_name','issn_l','is_oa','host_organization','type')
      temp3 <- temp3[names(temp3) %in% keep]
      names(temp3)<-paste0('source.',names(temp3))
    }else{temp3 <- NULL}
    primary_location <- cbind(as.data.table(temp2),
    as.data.table(temp3))
    return(primary_location)
  }
  else{
    stop('currently, only primary location is supported. this will be addressed in a future build')
  }
}

#' openAlex stores information about a work's authors(s) as a list nested in the larger works object. This function takes the works object and returns a flatted version of the location object which can be "cbinded" with the works object.
#' @param work an openAlex works object
#' @return a flattened data.table of authorship info
#' @export
parseAuthorsObject <- function(work = NULL){
  if(missing(work)){stop('please provide an openAlex works object')}
  # which author info items to keep
  auth_keep <- c('author_id','author_display_name','institutions_id','institutions_country_code','institutions_type','institutions_display_name')
 #### flatten twice to collapse two levels
  author_dt_list <- lapply(work$authorships,function(x){
  flat_author <- purrr::list_flatten(purrr::list_flatten(x))
  author_flat <- as.data.table(flat_author[names(flat_author) %in% auth_keep])
  author_flat})
  authors_n <- length(work$authorships)
  if(authors_n==1){author_dt <- author_dt_list[[1]]}else{author_dt <- rbindlist(author_dt_list,use.names = T,fill = T)}
  return(author_dt)
  }



#' openAlex stores information about a work's funding as a list nested in the larger works object. This function takes the works object and returns a flatted version of the location object which can be "cbinded" with the works object.
#' @param work an openAlex works object
#' @return a flattened data.table of funding info
#' @export
parseGrantsObject <- function(work = NULL){
  if(missing(work)){stop('please provide an openAlex works object')}
  # which grant info items to keep
  grant_keep <- c('funder','funder_display_name','award_id')
  #### flatten twice to collapse two levels
  grant_dt_list <- lapply(work$grants,function(x){
    flat_grant <- purrr::list_flatten(purrr::list_flatten(x))
    grant_flat <- as.data.table(flat_grant[names(flat_grant) %in% grant_keep])
    grant_flat})
  if(length(work$grants)==1){grant_dt <- grant_dt_list[[1]]}else{grant_dt <- rbindlist(grant_dt_list,use.names = T,fill = T)}
  return(grant_dt)
}



