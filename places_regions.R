## boundaries data

library(geographr)

## NUTS regions

# regions is england only; use countries for the rest.
nuts_boundaries <-
  bind_rows(
    boundaries_countries20 |>
      filter(country20_name != "England") |>
      rename(region_name=country20_name),
    boundaries_region21 |>
      rename(region_name=region21_name)
  ) |>
  # some names don't quite match BN names
  mutate(region_label = case_when(
    region_name %in% c("North East", "North West", "South East", "South West") ~ paste(region_name, "England"),
    region_name=="Yorkshire and The Humber" ~ "Yorkshire and the Humber",
    .default = region_name
  ))

## TODO counties




## NUTS regions for localities

bn_regions_sparql <-
  'select distinct ?item ?itemLabel  ?region  ?regionLabel  ?ioLabel
where {

    ?item (bnwdt:P33+ | bnwdt:P2+ ) ?region . # parent or ancester P2/P33 
        ?region bnwdt:P12+ bnwd:Q4207 . # in NUTS region
    
    ?item bnwdt:P12 ?io .
        filter (?io != bnwd:Q3801 ) . #  q3801 unesco site; only 2 and both also have arch.site

  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}
order by ?itemLabel'

bn_regions_query <-
  bn_std_query(bn_regions_sparql) |>
  make_bn_ids(c(item, region))




# DO *NOT* TRY TO JOIN INDIVIDUAL DATA POINTS TO THE POLYGONS AND THEN VIEW ANYTHING.
# it's better to aggregate before joining
# TODO this seems slow to use in ggplot, maybe look for alternative data sources?

bn_regions_boundaries <-
bn_regions_query |>
  mutate(region_label = str_remove(regionLabel, " *\\(region\\).*$")) |>
  count(region_label) |>
  inner_join(nuts_boundaries, by=c("region_label")) 



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

bn_localities_nuts_gas_query <-
  bn_std_query(bn_localities_nuts_gas_sparql) |>
  make_bn_ids(c(loc1, loc2))

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


## sparql for each of the UK countries

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
