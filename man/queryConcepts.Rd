% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/queryConcepts.R
\name{queryConcepts}
\alias{queryConcepts}
\title{Find concepts and concept ids for use in openAlex}
\usage{
queryConcepts(
  concept_string = NULL,
  maxlevel = 10,
  per_page = 10,
  mailto = NULL,
  max_return = 100,
  variables = c("id", "display_name", "level", "works_count", "works_api_url",
    "description")
)
}
\arguments{
\item{concept_string}{string or regular expression for searching concept descriptions}

\item{maxlevel}{most detailed level of concept tree to return (lower is more general, higher is more specific)}

\item{per_page}{how many results to return per query page}

\item{mailto}{email address to allow priority query}

\item{max_return}{maximum number of concepts to return}

\item{variables}{to return in data.table}
}
\value{
datatable of results
}
\description{
Search spreadsheet of openAlex concept tree (see https://docs.openalex.org/about-the-data/concept). Current google sheet url is: https://docs.google.com/spreadsheets/d/1LBFHjPt4rj_9r0t0TTAlT68NwOtNH8Z21lBMsJDMoZg/edit#gid=1473310811
}
\details{
NOTE: note that https://api.openalex.org/concepts doesn't seem to tolerate regex at this point, so that needs to be done with the output
}
