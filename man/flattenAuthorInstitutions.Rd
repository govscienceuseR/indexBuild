% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{flattenAuthorInstitutions}
\alias{flattenAuthorInstitutions}
\title{openAlex provides A LOT of author info stored in json. this goes into the nested structure and pulls out just the author institutions}
\usage{
flattenAuthorInstitutions(authors_json, institutional_var = NULL)
}
\arguments{
\item{authors_json}{a list in json structure that contains openAlex works}

\item{institutional_var}{the variable to return for institution, either 'ror' (ROR ID#) or 'display name'}
}
\value{
a vector of strings, 1 string for each work, of author names pasted together separated by ";"
}
\description{
openAlex provides A LOT of author info stored in json. this goes into the nested structure and pulls out just the author institutions
}
