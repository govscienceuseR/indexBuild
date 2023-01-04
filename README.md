# indexBuild

The referenceSearch (https://github.com/govscienceuseR/citationSearch) tool from govscienceuseR needs two basic inputs: (1) a set of extracted references from source documents; and (2) a set of canonical citations against which the extracted references can be searched. The user can customize this source set. 

The indexBuild package provides a way to automate a tailored reference set by querying and compiling bibliographic records from the open-source academic information liberary openAlex.org. 

The full openAlex database is ~300GB and so hosting the entire database is not a good time. indexBuild provides options for downloaded a customized subset filtered by attributes such as concept area, date, and citation count. This package is inspired by the openalexR package (https://cran.r-project.org/web/packages/openalexR/index.html) but serves a slightly different purpose: whereas openalexR works well for targeted queries (sort of like using Google Scholar), indexeBuild is designed to produce a custom reference library.

# functions

indexBuild currently does three main tasks: (1) search and identify IDs for venues (e.g., journals) and concept tags in openAlex; (2) query works associated with venues or concepts in openAlex and return a json database; (3) turn json file trees for openAlex works into a row-wise data.table object with a simple subset of metadata. Right now, the first two tasks are split across venues and concepts, e.g., there are separate extractVenues() and extractConcepts() functions. At some point, these can be combined.

# example
To get information about a journal, you can feed in a journal title:
```
queryVenues(venue_string = 'Journal of Public Administration Research and Theory')
```

and get information about a concept, you can feed in a concept. 

```
queryConcepts(concept_string = 'public administration')
```
Both functions use basic string search, and the openAlex API stems by default. The results of the function are somewhat different, because we assume that the user knows what venue they are interested in ahead of time, whereas concept search is likely to be more iterative. Thus, queryConcepts returns a data.table of potential concept matches for the user to check out. 

Once you have a concept or venue of interest, you can use the extractVenues() or extractConcepts() functions to query openAlex and return a set of "works" (an entity in the openAlex database that roughly corresponds to a publication, but groups cases where the same basic product is stored in both a journal and SSRN.). The extractXXX() functions also contain numerous options for filtering (e.g., by date) and controlling outputs (e.g., to reduce file size). The following example returns all JPART publications between 2015 and 2020, excluding paratext works (e.g., journal issue introductions, etc.)

```
extractVenues(concept_page = "https://openalex.org/V169433491",from_date = 2015,to_date = 2020,keep_paratext = F)
```
