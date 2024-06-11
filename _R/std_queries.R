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


# a standard query using bn_prefixes and bn_endpoint. sparql= 'query string *without prefixes*'
bn_std_query <- function(sparql){
  c(paste(
    bn_prefixes,
    sparql
  )) |>
    SPARQLchunks::sparql2df(endpoint=bn_endpoint) 
}



#mutate(across(c(a, b), ~str_extract(., "([^/]*$)") )) 
# previous: \\bQ\\d+$
# make an ID out of a wikibase item URL. v is often but not always person. could be eg item, place, woman, etc.
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
# generally only use this on ID cols, but sometimes qualifiers can be mixed: what if there were a / somewhere in a non URI  ??? 
# could add http to the rgx? then you'd have to change to str_match.
make_bn_ids <- function(data, across_cols=NULL, ...) {
  data |>
    mutate(across({{across_cols}}, ~str_extract(., "([^/]*$)")))
}


## turn <uv> into NA.
uv_to_na_across <- function(data, across_cols=NULL, ...) {
  data |>
    mutate(across({{across_cols}}, ~if_else(str_detect(., "^(_:)?t\\d+$"), NA, . ) ))
}


## making union/values queries (usually for subquerying something already run)

# construct lists of IDs for union/values query; bn_id is default ID column but can name another.
# nb will still need to be enclosed in appropriate brackets in the sparql.
bn_make_union <- function(data, bn_id=bn_id){
  data |>
    mutate(bn_bnwd = paste0("bnwd:",{{bn_id}})) |> # for VALUES
    mutate(bn_bnwdt = paste0("bnwdt:",{{bn_id}})) |>
    mutate(bn_bnp = paste0("bnp:",{{bn_id}})) |>
    mutate(bn_bnps = paste0("bnps:", {{bn_id}})) |>
    mutate(wd = paste0("wd:", {{bn_id}})) |> # this one for a wikidata query
    # construct the contents of the UNION (add to sparql with data$thing or pull)
    # these need to be unique!
    summarise(bn_bnp_union = paste(unique(bn_bnp), collapse = " | "), 
              bn_bnps_union = paste(unique(bn_bnps), collapse = " | "), 
              bn_bnwd_values = paste(unique(bn_bnwd), collapse = " "),
              wd_values = paste(unique(wd), collapse = " "),
              bn_bnwdt_union = paste(unique(bn_bnwdt), collapse = " | ") ) 
}



## building query string for VALUES (or shorthand union) query, if it's not already saved as a thing.
## improved! RTFM and discovered .open and .close. options for glue
## now replaces the mutate(q=...) and pull stuff entirely.
## spql = the VALUES sparql query string from WQS; need to insert "glue_values" placeholder  
## default values = bn_bnwd_values for use with bn_make_union, but could be any list of Ps or Qs to go into a sparql.
## also a ptential template for more complex replacements... 
mutate_glue_sparql <- function(data, spql, values=bn_bnwd_values){
  data |>
    mutate(s = glue(
      spql,
      .open = "<<", .close = ">>"
    )) |>
    pull(s)
}

## example
# example_spql <- 
#   'select distinct ?item ?itemLabel ?location ?locationLabel ?wd
#     where {
#       values ?item { <<bn_bnwd_values>> }
#       ?item bnwdt:P2 ?location .
#       optional {?location bnwdt:P117 ?wd .}
#     SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
#     }'
# bn_example_sparql <-
# bn_work_loc_query |>
#   filter(!is.na(location)) |>
#   bn_make_union(location) |>
#   mutate_glue_sparql(example_spql)
# bn_example_query <- bn_std_query(bn_example_sparql)


## deprecate but keep code for now 
## can't just paste in a sparql string because {} have to be escaped, but need glue {} for the values.
## replaces single { or } with {{ or }} 
# glue_values_sparql <- function(spql, values="bn_bnwd_values"){
#   str_replace_all(
#     spql,
#     c(
#       "\\{"= "\\{\\{", 
#       "\\}"="\\}\\}", 
#       "glue_values"=paste0("\\{", values, "\\}") 
#     )
#   )
# }
## example
# glue_values_sparql(
#   'SELECT ?person ?marriedname ?married
# WHERE {  
#   VALUES ?person { glue_values }
#   OPTIONAL {?person bnwdt:P141 ?marriedname .} 
#   optional {?person (bnwdt:P130 | bnwdt:P132)  ?married . }
#   FILTER ( EXISTS { ?person bnwdt:P141 ?marriedname .} || EXISTS { ?person (bnwdt:P130 | bnwdt:P132 ) ?married .  } ) .
# }
# ORDER BY ?person' 
# )


## endpoint URLs ####

bn_endpoint <- "https://beyond-notability.wikibase.cloud/query/sparql"
wd_endpoint <- "https://query.wikidata.org/sparql" 

## prefixes 

### as strings for glueing 

## added some extra prefixes - references, psv, pqv.
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
# DON'T use double braces here, only needed directly within a glue statement. R U SURE?

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
  
      FILTER(LANG(?propertyLabel) = 'en') 
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
## switched to usual format to include statements. keep this for reference though.

# bn_women_list <-
#   c(
#     glue(bn_prefixes,
#          "select distinct ?person ?personLabel 
#         where {{
#          {bn_triple_woman}
#          {bn_filter_project}
#          {wb_service_label}
#         }}") 
#   ) |>
#   sparql2df(endpoint=bn_endpoint) |>
#   make_bn_item_id(person) |>
#   #  mutate(bn_id = str_extract(person, "\\bQ\\d+$")) |>
#   relocate(bn_id, personLabel) |>
#   arrange( parse_number(str_remove(bn_id, "Q")))


# update April 2024 to include statements and dob/dod
bn_women_list_sparql <-
  'SELECT distinct ?person ?personLabel ?statements ?dob ?dod
WHERE {
   ?person bnwdt:P3 bnwd:Q3 ;
         wikibase:statements ?statements .
   FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .}

      optional { ?person bnwdt:P15 ?dod .   }
      optional { ?person bnwdt:P26 ?dob .   }

    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}'

# minimal processing to use as is or for dob/dod
bn_women_list <-
  bn_std_query(bn_women_list_sparql) |>
  make_bn_item_id(person) |>
  relocate(bn_id, personLabel) |>
  mutate(across(c(dob, dod), ~na_if(., ""))) |>
  arrange(parse_number(str_remove(bn_id, "Q")))


# ## dates of birth/death. added March 2024 - increasingly using this so let's put it here.
# bn_women_dob_dod_sparql <-
#   'SELECT distinct ?person ?bn_dob ?bn_dod
# WHERE {
#    ?person bnwdt:P3 bnwd:Q3 .
#   FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .}
#   optional { ?person bnwdt:P15 ?bn_dod .   }
#   optional { ?person bnwdt:P26 ?bn_dob .   }
#   FILTER ( EXISTS { ?person bnwdt:P15 ?bn_dod .} || EXISTS { ?person bnwdt:P26 ?bn_dob .  } ) . #  date of birth OR date of death.
# }'
# 
# 
# bn_women_dob_dod_query <-
#   bn_std_query(bn_women_dob_dod_sparql) |>
#   make_bn_item_id(person) |>
#   mutate(across(c(bn_dob, bn_dod), ~na_if(., ""))) |>
#   select(-person) 

bn_women_dob_dod <-
  bn_women_list |>
  filter(!is.na(dob) | !is.na(dod)) |>
  #bn_women_dob_dod_query |>
  mutate(across(c(dob, dod), ~parse_date_time(., "ymdHMS"), .names = "bn_{.col}")) |>
  mutate(across(c(bn_dob, bn_dod), year, .names = "{.col}_yr")) |>
  select(-dob, -dod) |>
  # only one row per person please
  group_by(bn_id) |>
  top_n(1, row_number()) |>
  ungroup() 
# to add +/- 80 years for missing dob/dod
# mutate(bn_yob = case_when(
#   !is.na(y_bn_dob) ~ y_bn_dob,
#   is.na(y_bn_dob) ~ y_bn_dod - 80
# )) |>
# mutate(bn_yod = case_when(
#   !is.na(y_bn_dod) ~ y_bn_dod,
#   is.na(y_bn_dod) ~ y_bn_dob+80
# )) 








## spql ####

## a few templates that may be regularly used 
## NB use of <<values>> (with .open=<< and .close=>> in glue function), so {} in sparql don't clash with glue defaults.
## bn_bnwd_values is created in bn_make_union() function

## linked P2 locations. may occasionally get multis.
bn_linked_p2_spql <-
  'select distinct ?item ?itemLabel ?location ?locationLabel ?wd
  where {
    values ?item { <<bn_bnwd_values>> }
  ?item bnwdt:P2 ?location .
  optional {?location bnwdt:P117 ?wd .}
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
  }' 


## linked P33 admin territories. likely to get multis.
bn_linked_p33_spql <-
  'select distinct ?item ?itemLabel ?admin ?adminLabel ?wd
  where {
    values ?item { <<bn_bnwd_values>> }
  ?item bnwdt:P33 ?admin .
  optional { ?admin bnwdt:P117 ?wd .}
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
  }'  



## additional non-locality wd_geo/bn_geo if needed in analysis
## though shouldn't need this now you've separated locality and wd_geo queries
## keep it anyway, in case you decide not to fetch all wd_geo after all.

## fetch wd_geo for a VALUES bnwd items (might be created with bn_make_union); will need to make glue_values first.
## non-optional but beware of having too many items in the values list. 
## seems to be able to handle several hundred, but probably depends how much data you're fetching/complexity of query...
bn_get_wd_geo_spql <-
  'SELECT distinct ?item ?itemLabel ?wd ?wd_geo 
      WHERE {  
      VALUES ?item { <<glue_values>> }
      ?item bnwdt:P117 ?wd .
      bind(iri(concat("http://www.wikidata.org/entity/", str(?wd))) as ?wikidata) .
      SERVICE <https://query.wikidata.org/sparql> {
           ?wikidata wdt:P625 ?wd_geo .
       } # /wikidata service
      SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
      }
      ORDER BY ?itemLabel' 



## next step is to insert data in spql to make a sparql. along the lines of

## sparql <-
## data |>
## filter(!is.na(item)) |> # if there are likely to be any NAs, get rid!
## bn_make_union(item) |>
## mutate_glue_sparql(spql)

## query <- bn_std_query(sparql) |> make_bn_item_id(item)








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

