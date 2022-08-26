# referenceBuild

The citationSearch tool from govscienceuseR needs two basic inputs: (1) a set of extracted references from source documents; and (2) a set of canonical citations against which the extracted references can be searched. The user can customize this source set. 

The referenceBuild package provides a way to automate a tailored reference set by querying and compiling bibliographic records from the open-source academic information liberary openAlex.org. 

The full openAlex database is ~300GB and so hosting the entire database is not a good time. referenceBuild provides options for downloaded a customized subset filtered by attributes such as concept area, date, and citation count. This package is inspired by the openalexR package (https://cran.r-project.org/web/packages/openalexR/index.html) but serves a slightly different purpose: whereas openalexR works well for targeted queries (sort of like using Google Scholar), referenceBuild is designed to produce a custom reference library.
