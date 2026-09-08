# indexBuild

The referenceSearch (https://github.com/govscienceuseR/citationSearch) tool from govscienceuseR needs two basic inputs: (1) a set of extracted references from source documents; and (2) a set of canonical citations against which the extracted references can be searched. The user can customize this source set. 

The indexBuild package provides a way to automate a tailored reference set by querying and compiling bibliographic records from the open-source academic information liberary openAlex.org. 

The full openAlex database is ~300GB and so hosting the entire database is not a good time. indexBuild provides options for downloaded a customized subset filtered by attributes such as topic area, date, and citation count. This package is inspired by the openalexR package (https://cran.r-project.org/web/packages/openalexR/index.html) but serves a slightly different purpose: whereas openalexR works well for targeted queries (sort of like using Google Scholar), indexeBuild is designed to produce a custom reference library.

# functions

indexBuild currently does three main tasks: (1) search and identify IDs for sources (e.g., journals) and topic/concept tags in openAlex — `querySources()`, `queryTopics()`, and (deprecated) `queryConcepts()`; (2) query works associated with a source or topic in openAlex and return a data.table — `extractWorks()`; (3) turn json trees for openAlex works into a row-wise data.table object with a simple subset of metadata — `processWork()`. A single `extractWorks()` function handles both source- and topic-based crawls (rather than the older separate `extractVenues()`/`extractConcepts()` idea).

# example
To get information about a journal (source), you can feed in a journal title:
```
querySources(source = 'Journal of Public Administration Research and Theory')
```

and to get information about a topic, you can feed in a topic string.

```
queryTopics(topic_string = 'public administration')
```
Both functions use basic string search, and the openAlex API stems by default. The results are somewhat different, because we assume the user knows what source they are interested in ahead of time, whereas topic search is likely to be more iterative. Thus, `queryTopics()` returns a data.table of candidate topic matches (with their domain > field > subfield hierarchy) for the user to check out.

> **Note on classification:** openAlex has deprecated Concepts in favor of Topics. `queryTopics()` is the recommended path; `queryConcepts()` still works against the deprecated `/concepts` endpoint, and `extractWorks()` accepts either a `topic_id` (filtered on `topics.id`) or a `concept_id` (the deprecated `concept.id` alias).

Once you have a source or topic of interest, you can use `extractWorks()` to query openAlex and return a set of "works" (an entity in the openAlex database that roughly corresponds to a publication, but groups cases where the same basic product is stored in both a journal and SSRN.). `extractWorks()` also contains numerous options for filtering (e.g., by date) and controlling outputs (e.g., `data_style` to reduce file size). The following example returns all JPART publications between 2015 and 2020, excluding paratext works (e.g., journal issue introductions, etc.):

```
src <- querySources(source = 'Journal of Public Administration Research and Theory')
extractWorks(source_id = src$results[[1]]$id, from_date = 2015, to_date = 2020, keep_paratext = FALSE)
```

Or by topic:

```
tp <- queryTopics(topic_string = 'public administration')
extractWorks(topic_id = tp$id[1], from_date = 2015, to_date = 2020, keep_paratext = FALSE)
```

# rate limits and API keys

openAlex now meters usage with a credit/cost budget rather than a raw daily call count: list-endpoint requests (which `extractWorks()` makes) cost roughly 10 credits each regardless of `per_page`, so `extractWorks()` defaults `per_page` to the 200-work maximum and reports an up-front credit estimate. Every request is routed through `performOA()`, which throttles to the polite pool and retries transient 429/5xx responses with backoff (honoring `Retry-After`). If the budget is exhausted mid-crawl, `extractWorks()` flushes partial results to `dest_file` so the crawl can be resumed with a finer date range.

Supplying an email via `mailto` gets you into the faster "polite pool." You can optionally supply an OpenAlex API key — either per call via the `api_key` argument, or by setting the `OPENALEX_API_KEY` environment variable — which is sent as an `Authorization: Bearer` header (never in the URL) to raise your allowance.
