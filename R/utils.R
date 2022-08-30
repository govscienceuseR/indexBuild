#' simple wrapper to combine stream_in and gzfile to read json.gz OA files
#'
#'This function just combines two existing functions. Stream_in is preferred over normal read functions because the files are very large.
#' @param x a json.gz file
#' @return a decompressed json file
#' @export
streamGZ <- function(x){stream_in(gzfile(x))}

#' openAlex provides A LOT of author info stored in json. this goes into the nexted structure and pulls out just the author names
#' @param authors_json a list in json structure that contains openAlex works
#' @return a vector of strings, 1 string for each work, of author names pasted together separated by ";"
#' @export
flattenAuthorNames <- function(authors_json){
  sapply(sapply(lapply(authors_json,'[[','author'),'[[','display_name'),paste,collapse=';')
}