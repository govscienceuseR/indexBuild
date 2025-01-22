#' Search for and return candidate matches to a title in openAlex
#'
#' @param title a string name for title
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @param wait_time how long to wait on query results before timing out, in seconds
#' @param max_results how many results to return (uses "per_page" feature on OA to grab first page of results)
#' @param url the base url for openAlex query
#' @param data_style options for processWork() --> how much/how little data to grab from json list, see @details
#' @param try_reduced_string option to try removing words FAILED by hunspell package if nothing found in main query
#' @description Primary use of this function is to make a query object for use in openAlex that returns potential matches to a title string
#' @examples
#' data(titles)
#' #query a single title
#' queryTitle(titles[22])
#' @import httr2
#' @import jsonlite
#' @import magrittr
#' @import stringr
#' @import data.table
#' @import hunspell
#' @details Note that because extracted records can be pretty large--and are complicated, nested json file--there is an optional "data_style" command that lets the user specify what to return. Currently there are three options: (1) bare_bones returns OpenAlex ID + DOI, basically, results that can be used to look up the work again; (2) citation returns typical citation information, like journal name, author, etc., with a couple bonus items like source.id to link back to openAlex (3) comprehensive returns author institutional affiliations, open access info, funding data, etc.; and (4) [not active] all returns the entire result in original json format.
#' @export
#'
queryTitle <- function(title = NULL,mailto = NULL,wait_time = 5,max_results = 5,url = "https://api.openalex.org/works",data_style = c('citation'),try_reduced_string = TRUE){
  query <- generateTitleQuery(title = title,mailto = mailto,max_results = max_results,url = url)
  ### if bad response, result is NULL
  req <- request(query) |> req_timeout(wait_time)
  perf <- req_perform(req)
  code <- perf$status_code
  reduced <- FALSE
  if(code != 200){result <- NULL}
  ### if code response...
  if(code == 200){
    #### parse content
    json_response<-httr2::resp_body_json(perf)
    #### are there any matches returned?
    if(json_response$meta$count == 0){
      reduced <- TRUE
      ### if no matches returned, try removing misspelled words
      ### note: this also loses a lot of special science words, so really only works if you are diligent with matching scores later on
      unspell <- unlist(hunspell(title))
      if(length(unspell)>0){
      reduced_title <- str_remove_all(title,paste(unspell,collapse = '|'))
      reduced_query <- generateTitleQuery(title = reduced_title,mailto = mailto,max_results = max_results,url = url)
      req <- request(reduced_query) |> req_timeout(wait_time)
      perf <- req_perform(req)
      json_response<-httr2::resp_body_json(perf)
      }
    }
    ### if still no matches, give up
    if(json_response$meta$count == 0){
        result <- data.table(query_title = title,query = query)
    }
    if(json_response$meta$count > 0){
      json_response$results <- json_response$results[!duplicated(sapply(json_response$results,'[[','id'))]
      if(json_response$meta$count==1)
      {result <- processWork(work = json_response$results,data_style = data_style)}else{
        result <- rbindlist(processWorks(work = json_response$results,data_style = data_style),use.names = T,fill = T)
      }
      result$query_title = title
      result$query = query
      result$reduced = reduced
    }
    }
  return(result)
}

#' Vectorized search for and return candidate matches to a title in openAlex
#'
#' @description Same function but can take a set of titles. Not advisable for large data sets, parallelize queryOpenAlex instead.
#' @param title a vector of title strings
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @param wait_time how long to wait on query results before timing out, in seconds
#' @param max_results how many results to return (uses "per_page" feature on OA to grab first page of results)
#' @param url the base url for openAlex query
#' @param data_style options for processWork() --> how much/how little data to grab from json list, see @details
#' @param try_reduced_string option to try removing words FAILED by hunspell package if nothing found in main query
#' @export
queryTitles <- Vectorize(queryTitle,vectorize.args = 'title',SIMPLIFY = F)


#' Take a title string and make a query
#'
#' @param title a string name for title
#' @param mailto email address of user, needed to get in 'polite pool' of API
#' @param max_results how many results to return (uses "per_page" feature on OA to grab first page of results)
#' @param url the base url for openAlex query
#' @description Primary use of this function is to make a query url for openAlex
#' @import httr
#' @import httr2
#' @export
generateTitleQuery = function(title = NULL,mailto = NULL,max_results = 5,url = "https://api.openalex.org/works"){
  ### note, results are automatically sorted by relevance so return is best matches ###
  url <- url_parse(url)
  url$query <- list(mailto = mailto,search = title,per_page = max_results,sort = 'relevance_score:desc')
  url$scheme <- "https"
  url <- url_build(url)
  return(url)
}
#generateTitleQuery <- Vectorize(generateTitleQuery, vectorize.args = "title")

