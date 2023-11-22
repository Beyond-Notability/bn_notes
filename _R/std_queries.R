## Code you'll need all the time ####

# abbreviations...
# bn = beyond notability
# wb = wikibase
# wd = wikidata



## functions ####

# sparql2df adding 1 set of prefixes 

sparql2df_prefix <- function(endpoint, prefixes, data) {
  sparql2df(endpoint, paste(prefixes, data))
}


# a standard query using bn_prefixes and bn_endpoint. sparql= 'query string'
bn_std_query <- function(sparql){
  c(paste(
    bn_prefixes,
    sparql
  )) |>
    SPARQLchunks::sparql2df(endpoint=bn_endpoint) 
}



#mutate(across(c(a, b), ~str_extract(., "([^/]*$)") )) 
# previous: \\bQ\\d+$
# get an ID out of a wikibase item URL. v is often but not always person. could be eg item, place, woman, etc.
make_bn_item_id <- function(df, v) {
  df |>
    mutate(bn_id = str_extract({{v}}, "([^/]*$)")) |>
    relocate(bn_id)
}

# the same if it was a query for properties
make_bn_prop_id <- function(df, v) {
  df |>
    mutate(bn_prop_id = str_extract({{v}}, "([^/]*$)")) |>
    relocate(bn_prop_id)
}

# use across to extract IDs from URLs for 1 or more cols, no renaming or relocating
# across_cols can be any tidy-select kind of thing
make_bn_ids <- function(data, across_cols=NULL, ...) {
  data |>
    mutate(across({{across_cols}}, ~str_extract(., "([^/]*$)")))
}


## endpoint URLs ####

bn_endpoint <- "https://beyond-notability.wikibase.cloud/query/sparql"
wd_endpoint <- "https://query.wikidata.org/sparql" 

## prefixes 

### as strings for glueing 

## added some extra prefixes - probably won't want the ones to do with references, but will need the psv and pqv.
bn_prefixes <- 
  "PREFIX bnwd: <https://beyond-notability.wikibase.cloud/entity/>
PREFIX bnwds: <https://beyond-notability.wikibase.cloud/entity/statement/>
PREFIX bnwdv: <https://beyond-notability.wikibase.cloud/value/>
PREFIX bnwdt: <https://beyond-notability.wikibase.cloud/prop/direct/>
PREFIX bnp: <https://beyond-notability.wikibase.cloud/prop/>
PREFIX bnps: <https://beyond-notability.wikibase.cloud/prop/statement/>
PREFIX bnpq: <https://beyond-notability.wikibase.cloud/prop/qualifier/> 
PREFIX bnpsv: <https://beyond-notability.wikibase.cloud/prop/statement/value/>
PREFIX bnpqv: <https://beyond-notability.wikibase.cloud/prop/qualifier/value/>
  PREFIX bnwdref: <https://beyond-notability.wikibase.cloud/reference/>
  PREFIX bnpr: <https://beyond-notability.wikibase.cloud/prop/reference/>
  PREFIX bnprv: <https://beyond-notability.wikibase.cloud/prop/reference/value/>
"


wd_prefixes <- 
  "PREFIX wdt: <http://www.wikidata.org/prop/direct/>
   PREFIX wd:  <http://www.wikidata.org/entity/>
"

props_prefixes <-
  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX wikibase: <http://wikiba.se/ontology#>
PREFIX schema: <http://schema.org/>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
"

## following should be installed by default? so in fact you only need skos: and you don't need that unless you do altLabel
# PREFIX wikibase: <http://wikiba.se/ontology#>
# PREFIX schema: <http://schema.org/>
# PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
# PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
# PREFIX owl: <http://www.w3.org/2002/07/owl#>
# PREFIX prov: <http://www.w3.org/ns/prov#>






# std triples, filters etc 
# haven't really used these much as the workflow has turned out, but keep for reference
# DON'T use double braces here, only needed directly within a glue statement

bn_triple_woman <- "?person bnwdt:P3 bnwd:Q3 . " # get women

bn_filter_project <- "FILTER NOT EXISTS { ?person bnwdt:P4 bnwd:Q12 . } " # filter out project team

wb_service_label <- 'SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE], en, en-gb". }' # labels





# std reference queries

## all the properties in the wikibase with label and description.
bn_properties <-
  c("SELECT DISTINCT ?property ?propertyType ?propertyLabel  ?propertyDescription  # ?propertyAltLabel
      WHERE {
        ?property a wikibase:Property ;
              rdfs:label ?propertyLabel ;
              wikibase:propertyType ?propertyType .

      # OPTIONAL { ?property skos:altLabel ?propertyAltLabel . } # not many of these

      OPTIONAL {SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE], en-gb, en'.
              ?property schema:description ?propertyDescription . }
      } 
  
      FILTER(LANG(?propertyLabel) = 'en-gb') 
    }
    order by ?propertyLabel") |>
  sparql2df(endpoint=bn_endpoint) |>
  make_bn_prop_id(property) |>
#  mutate(bn_id = str_extract(property, "\\bP\\d+$")) |>
  mutate(property_type = str_extract(propertyType, "[A-Za-z]+$")) |>
  relocate(property_type, .after = bn_prop_id) |>
  relocate(property, propertyType, .after = last_col())  |>
  mutate(propertyDescription = na_if(propertyDescription, "")) |>
  arrange(parse_number(str_extract(bn_prop_id, "\\d+"))) 

## must have properties

bn_properties_musthave <-            
  c(paste(
    bn_prefixes,
    "SELECT DISTINCT ?property ?property_label ?propertyType
    WHERE {
    ?property ?p  bnwd:Q2947;
          rdfs:label ?property_label;
              wikibase:propertyType ?propertyType.  
    filter(lang(?property_label)='en-gb') .
  }")) |>
  sparql2df(endpoint=bn_endpoint) |>
  make_bn_prop_id(property) |>
#  mutate(bn_id = str_extract(property, "\\bP\\d+$")) |>
  mutate(property_type = str_extract(propertyType, "[A-Za-z]+$")) |>
  relocate(property_type, .after = bn_prop_id) |>
  relocate(property, propertyType, .after = last_col())  |>
  arrange(parse_number(str_extract(bn_prop_id, "\\d+"))) |>
  # ALBS needs to be removed from must haves if it hasn't already.
  filter(bn_prop_id !="P134")


## The Women
## how to use the glue statements...

bn_women_list <-
  c(
    glue(bn_prefixes,
         "select distinct ?person ?personLabel 
        where {{
         {bn_triple_woman}
         {bn_filter_project}
         {wb_service_label}
        }}") 
  ) |>
  sparql2df(endpoint=bn_endpoint) |>
  make_bn_item_id(person) |>
  #  mutate(bn_id = str_extract(person, "\\bQ\\d+$")) |>
  relocate(bn_id, personLabel) |>
  arrange( parse_number(str_remove(bn_id, "Q")))





# ## Wikibase example queries (needs WikipediR)
# ## this is not actually in use anywhere - it's easier to C&P queries as needed, but keep the code for reference.
# bn_example_queries_fetch <- 
#   page_content(
#     domain = "beyond-notability.wikibase.cloud",
#     page_name = "Project:SPARQL/examples",
#     as_wikitext = TRUE
#   )
# 
# # same but as html; easier to work with wikitext for examples queries
# # bn_example_queries_html <- page_content(
# #   domain = "beyond-notability.wikibase.cloud",
# #   page_name = "Project:SPARQL/examples"
# # )
# 
# 
# bn_example_queries <-
#   bn_example_queries_fetch$parse$wikitext$`*` |> 
#   #write("test-wikipage.txt")
#   enframe() |>
#   select(-name) |>
#   # have a feeling this may not be the most robust regex ever but we'll see
#   unnest_regex(section, value, pattern = "(?<![A-Za-z=])==(?!=)", to_lower = F) |>
#   separate(section, into=c("section", "queries"), sep="==\n+", extra = "merge", fill="right") |> # hmm this has started giving a missing pieces warning which it wasn't doing before...
#   ## there are a few ==== ! but they have their own headings so I think it'll be ok to drop their ===
#   unnest_regex(query, queries, pattern = "====?(?=[A-Za-z ])", to_lower = F) |>
#   separate(query, into=c("title", "query"), sep="====?\n+", fill="right") |> # warning started here as well
#   relocate(section, .after = last_col()) |>
#   relocate(query) |>
#   mutate(query = str_remove_all(query, "\\s*</?sparql[^>]*>\\s*")) |>
#   # separate pre-comments from query. doesn't seem to matter for sending a query so this is purely to make it easier to look at here.
#   # start-query terms: PREFIX SELECT #defaultView: not sure if there are any others?
#   # BUT start-query terms could also be in pre-comments...
#   # SO only split on the start-query terms at the beginning of the query or the beginning of a line. (a comment/comment line can't start with a term given that it has to be preceded by #
#   # is it worth separating any post-comments as well? not sure i fancy working out that regex...
#   separate(query, into=c("pre", "query_txt"), sep="(?=((\n|^)\\s*(PREFIX|#defaultView:|SELECT)))", extra = "merge", remove = F) |> # missing pieces warning started here as well
#   mutate(across(c(section, title, query, pre, query_txt), str_trim))  |>
#   # quick fix (hopefully...) for queries that only specify en-gb in SERVICE language. i think auto_language doesn't work when querying from here rather than in browser
#   mutate(query = str_replace(query, '\\[AUTO_LANGUAGE\\], *en-gb *(?=["\'])', '[AUTO_LANGUAGE], en-gb, en'))
# 
# # nb: query for lines that start # *except* #defaultView
# #filter(str_detect(query, "^\\s*#(?!defaultView)"))




### c() for glitter

# g_bn_prefixes <- c(bnwd = "https://beyond-notability.wikibase.cloud/entity/",
#                    bnwds = "https://beyond-notability.wikibase.cloud/entity/statement/",
#                    bnwdv = "https://beyond-notability.wikibase.cloud/value/",
#                    bnwdt = "https://beyond-notability.wikibase.cloud/prop/direct/",
#                    bnp = "https://beyond-notability.wikibase.cloud/prop/",
#                    bnps = "https://beyond-notability.wikibase.cloud/prop/statement/",
#                    bnpq = "https://beyond-notability.wikibase.cloud/prop/qualifier/",
#                    bnpsv = "https://beyond-notability.wikibase.cloud/prop/statement/value/",
#                    bnpqv = "https://beyond-notability.wikibase.cloud/prop/qualifier/value/",
#                    bnwdref = "https://beyond-notability.wikibase.cloud/reference/",
#                    bnpr = "https://beyond-notability.wikibase.cloud/prop/reference/",
#                    bnprv = "https://beyond-notability.wikibase.cloud/prop/reference/value/"
#                    )


# g_props_prefixes <- c(
#   rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
#   rdfs = "http://www.w3.org/2000/01/rdf-schema#",
#   xsd = "http://www.w3.org/2001/XMLSchema#",
#   wikibase = "http://wikiba.se/ontology#",
#   schema = "http://schema.org/",
#   skos = "http://www.w3.org/2004/02/skos/core#"
# )
# 
# g_wd_prefixes <- c(
#   wdt= "http://www.wikidata.org/prop/direct/",
#   wd=  "http://www.wikidata.org/entity/"
# )

