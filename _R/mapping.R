## AFTER std_queries.

## boundaries data ####

##devtools::install_github("humaniverse/geographr")
library(geographr)

library(sf)


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


