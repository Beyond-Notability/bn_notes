## AFTER std_queries.

## boundaries data ####

library(geographr)

library(sf)


## historic counties borders project (uk)
## NB need an acknowledgment line in any public posts: 
## will need to get formatting right 
hcp_credit <- "This mapping made use of data provided by the [Historic County Borders Project](https://www.county-borders.co.uk)."
## there is also a readme in the folder with the data.

hcp_uk_historic_counties_big <-
  st_read(here::here("shp/hcp_uk_counties/UKDefinitionA.shp"), quiet = TRUE)

# the HCP map needs simplification and transform to CRS coords. sf::simplify might work
hcp_uk_historic_counties_spl <-
  rmapshaper::ms_simplify(hcp_uk_historic_counties_big) |>
  st_as_sf() |>
  st_transform(crs = 4326) |>
  clean_names("snake")

# bn_name for differences in naming; work in progress
hcp_uk_historic_counties_names <-
  hcp_uk_historic_counties_spl$name |>
  enframe(value = "name", name=NULL)  |>
  # adjust some names to match BN labels
  mutate(bn_name = case_match(
    name,
    "Cardiganshire" ~ "Ceredigion",
    "Anglesey" ~ "Isle of Anglesey", 
    "Durham" ~ "County Durham",
    "Down" ~ "County Down",
    "Antrim" ~ "County Antrim",
    "Armagh" ~ "County Armagh",
    "Londonderry" ~ "County Londonderry",
    "Peeblesshire" ~ "Peebleshire",
    .default = name
  )) 

hcp_uk_historic_counties_map <-
  hcp_uk_historic_counties_spl |>
  inner_join(hcp_uk_historic_counties_names, by="name")


## ONS regions (uk)

## CREDIT. ONS Geography.
## NUTS, level 1 (January 2018) Boundaries UK BUC
## https://geoportal.statistics.gov.uk/datasets/ons::nuts-level-1-january-2018-boundaries-uk-buc-2/about
## Digital boundary products and reference maps are supplied under the Open Government Licence. You must use the following copyright statements when you reproduce or use this material:

ons_uk_regions_map_credit <- "Source: Office for National Statistics licensed under the Open Government Licence v.3.0. Contains OS data © Crown copyright and database right [[2018](https://geoportal.statistics.gov.uk/datasets/ons::nuts-level-1-january-2018-boundaries-uk-buc-2/about)]"

# buc ultra generalised. 
ons_uk_regions_map_buc <-
  st_read(here::here("shp/ons_uk_nuts1_regions/NUTS1_Jan_2018_UGCB_in_the_UK.shp"), quiet = TRUE)

ons_uk_regions_map_spl <-
  ons_uk_regions_map_buc |>
  st_transform(crs=4326) |>
  st_simplify() |>
  clean_names("snake") 

# bn_name to match differences in naming
ons_uk_regions_map <-
  ons_uk_regions_map_spl |>
  # bn region names
  arrange(nuts118nm) |>
  bind_cols(
    enframe(c("East Midlands", "East of England", "London (region)" ,                
              "North East England", "North West England",  "Northern Ireland", "Scotland",         
              "South East England",  "South West England", "Wales",     
              "West Midlands", "Yorkshire and the Humber" ), value="bn_name", name=NULL)
  ) 



## TODO counties


## wikibase ####

## NUTS regions for localities

bn_regions_localities_sparql <-
  'select distinct ?item ?itemLabel  ?region  ?regionLabel  ?ioLabel
where {

    ?item (bnwdt:P33+ | bnwdt:P2+ ) ?region . # parent or ancester P2/P33 
        ?region bnwdt:P12+ bnwd:Q4207 . # in NUTS region
    
    ?item bnwdt:P12 ?io .
        filter (?io != bnwd:Q3801 ) . #  q3801 unesco site; only 2 and both also have arch.site

  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}
order by ?itemLabel'


bn_regions_localities_query <-
  bn_std_query(bn_regions_localities_sparql) |>
  make_bn_ids(c(item, region))


# regions only
# ons_name for differences in naming
bn_regions <-
  bn_regions_localities_query |>
  distinct(regionLabel, region) |>
  arrange(regionLabel) |>
  # ons region names. make sure they're in the right order...
  bind_cols(
    enframe(c("East Midlands (England)", "East of England", "London" ,                
              "North East (England)", "North West (England)",  "Northern Ireland", "Scotland",         
              "South East (England)",  "South West (England)", "Wales",     
              "West Midlands (England)", "Yorkshire and The Humber" ), value="ons_name", name=NULL)
  ) 

bn_regions_localities <-
bn_regions_localities_query |>
  # add ons versions of region names
  inner_join(bn_regions |> select(-regionLabel), by="region")


## full paths for localities in UK


## all four countries from NUTS regions
# fiddly, but i can't see another way to get NUTS region and sub-region/county all at the same level. 
bn_localities_nuts_gas_sparql <-
  'SELECT ?depth  ?loc1Label ?loc2Label ?loc1 ?loc2 
WHERE {
      SERVICE gas:service {
           gas:program gas:gasClass "com.bigdata.rdf.graph.analytics.BFS" ; 
                       gas:in bnwd:Q1231 ;
                       gas:in bnwd:Q67;
                       gas:in bnwd:Q75 ;
                       gas:in bnwd:Q200 ;
                       gas:in bnwd:Q1202 ;
                       gas:in bnwd:Q85 ;
                       gas:in bnwd:Q204 ;
                       gas:in bnwd:Q1215 ;
                       gas:in bnwd:Q72 ;
                       gas:in bnwd:Q1224 ; 
                       gas:in bnwd:Q425 ;
                       gas:in bnwd:Q1740 ;
                       gas:linkType bnwdt:P33 ; 
                       gas:traversalDirection "Reverse"; 
                       gas:out ?loc2 ; 
                       gas:out1 ?depth ; 
                       gas:out2 ?loc1 . 
      }
  
    SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
    } 
order by ?depth ?loc1Label ?loc2Label'

bn_localities_nuts_gas_sparql <-
  'SELECT ?depth  ?loc1Label ?loc2Label ?loc1 ?loc2 
WHERE {
      SERVICE gas:service {
           gas:program gas:gasClass "com.bigdata.rdf.graph.analytics.BFS" ; 
                       gas:in bnwd:Q1231 ;
                       gas:in bnwd:Q67;
                       gas:in bnwd:Q75 ;
                       gas:in bnwd:Q200 ;
                       gas:in bnwd:Q1202 ;
                       gas:in bnwd:Q85 ;
                       gas:in bnwd:Q204 ;
                       gas:in bnwd:Q1215 ;
                       gas:in bnwd:Q72 ;
                       gas:in bnwd:Q1224 ; 
                       gas:in bnwd:Q425 ;
                       gas:in bnwd:Q1740 ;
                       gas:linkType bnwdt:P33 ; 
                       gas:traversalDirection "Reverse"; 
                       gas:out ?loc2 ; 
                       gas:out1 ?depth ; 
                       gas:out2 ?loc1 . 
      }
  
    SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
    } 
order by ?depth ?loc1Label ?loc2Label'

bn_localities_nuts_gas_query <-
  bn_std_query(bn_localities_nuts_gas_sparql) |>
  make_bn_ids(c(loc1, loc2))



bn_localities_nuts_gas <-
  bn_localities_nuts_gas_query   |>
  mutate(item_name = str_remove(loc1Label, " *\\(county\\) *$"))



## sparql for each of the UK countries

bn_localities_scot_gas_sparql <-
  'SELECT ?depth  ?loc1Label ?loc2Label ?loc1 ?loc2
WHERE {
      SERVICE gas:service {
           gas:program gas:gasClass "com.bigdata.rdf.graph.analytics.BFS" ; 
                       gas:in bnwd:Q1224 ; 
                       gas:linkType bnwdt:P33 ; 
                       gas:traversalDirection "Reverse"; 
                       gas:out ?loc2 ; 
                       gas:out1 ?depth ; 
                       gas:out2 ?loc1 . 
      }
    SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
    } 
order by ?depth ?loc1Label ?loc2Label'


# 
# bn_localities_scot_gas_query <-
#   bn_std_query(bn_localities_scot_gas_sparql) |>
#   make_bn_ids(c(loc1, loc2))
# 
# 
# bn_localities_scot_gas <-
#   bn_localities_scot_gas_query   |>
#   mutate(item_name = str_remove(loc1Label, " *\\(county\\) *$"))


bn_localities_eng_gas_sparql <-
  'SELECT ?depth  ?loc1Label ?loc2Label ?loc1 ?loc2
WHERE {
      SERVICE gas:service {
           gas:program gas:gasClass "com.bigdata.rdf.graph.analytics.BFS" ; 
                       gas:in bnwd:Q617 ; 
                       gas:linkType bnwdt:P33 ; 
                       gas:traversalDirection "Reverse"; 
                       gas:out ?loc2 ; 
                       gas:out1 ?depth ; 
                       gas:out2 ?loc1 . 
      }
    SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
    } 
order by ?depth ?loc1Label ?loc2Label'


bn_localities_wales_gas_sparql <-
  'SELECT ?depth  ?loc1Label ?loc2Label ?loc1 ?loc2
WHERE {
      SERVICE gas:service {
           gas:program gas:gasClass "com.bigdata.rdf.graph.analytics.BFS" ; 
                       gas:in bnwd:Q425 ; 
                       gas:linkType bnwdt:P33 ; 
                       gas:traversalDirection "Reverse"; 
                       gas:out ?loc2 ; 
                       gas:out1 ?depth ; 
                       gas:out2 ?loc1 . 
      }
    SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
    } 
order by ?depth ?loc1Label ?loc2Label'


bn_localities_nire_gas_sparql <-
  'SELECT ?depth  ?loc1Label ?loc2Label ?loc1 ?loc2
WHERE {
      SERVICE gas:service {
           gas:program gas:gasClass "com.bigdata.rdf.graph.analytics.BFS" ; 
                       gas:in bnwd:Q1740 ; 
                       gas:linkType bnwdt:P33 ; 
                       gas:traversalDirection "Reverse"; 
                       gas:out ?loc2 ; 
                       gas:out1 ?depth ; 
                       gas:out2 ?loc1 . 
      }
    SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
    } 
order by ?depth ?loc1Label ?loc2Label'
