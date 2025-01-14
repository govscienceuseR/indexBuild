# concept usage of extractWorks()
id <- queryConcepts('public administration')
extractWorks(concept_id = id$id,from_date = 2020,to_date = 2021,cursor = FALSE,data_style = 'citation',per_page = 10)
# journal (source) usage of extractWorks()
id <- querySources(source = 'Journal of Public Administration Research and Theory')
extractWorks(source_id =id$results[[1]]$id,cursor = FALSE,data_style = 'citation',per_page = 10)


