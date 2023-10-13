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
    temp3 <- temp$source[sapply(temp,length)<2]
    names(temp3)<-paste0('source.',names(temp3))
    primary_location = cbind(temp2,temp3)
    return(primary_location)
  }
  else{
    stop('currently, only primary location is supported. this will be addressed in a future build')
  }
}


#' openAlex stores information about a work's authors(s) as a list nested in the larger works object. This function takes the works object and returns a flatted version of the location object which can be "cbinded" with the works object.
#' @param work an openAlex works object
#' @return a flattened data.table of location info
#' @export
parseAuthorsObject <- function(work = NULL){
  if(missing(work)){stop('please provide an openAlex works object')}
  if(primary){
    temp = work[['primary_location']]
    temp2 <- temp[sapply(temp,length)<2]
    temp3 <- temp$source[sapply(temp,length)<2]
    names(temp3)<-paste0('source.',names(temp3))
    primary_location = cbind(temp2,temp3)
    return(primary_location)
  }
  else{
    stop('currently, only primary location is supported. this will be addressed in a future build')
  }
}


