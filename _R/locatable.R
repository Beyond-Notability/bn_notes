## 1. fetch all BN items with instance of locality/archaeological site/historic house
## 2. fetch all available coords for BN items with wikidata ID (whether localities or not)
## 3. fetch (main) bn_geo coords (*excl street addresses in resided at*)
## 4. put them together

bn_io_locality_sparql <-
  'SELECT distinct ?localityLabel ?locality ?ioLabel ?io ?wd_id ?bn_geo ?adminLabel ?admin
WHERE {  
  
  ?locality bnwdt:P12 ?io . # this will get all the io; prob need a filter
  
  # is a filter better than union here? i think it is. not much time difference but it gets rid of a few extras you dont want (will still have some multis)
  
  filter( ?io in (bnwd:Q2147, bnwd:Q86, bnwd:Q2961)   ) .

  optional {  ?locality bnwdt:P117 ?wd_id .   } # wikidata id
  optional { ?locality bnwdt:P153 ?bn_geo . } # nb this is not all bn_geo
  optional { ?locality bnwdt:P33 ?admin . } # will be multis
  
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}
ORDER BY ?localityLabel'


bn_io_locality_query <-
  bn_std_query(bn_io_locality_sparql) |>
  make_bn_ids(c(locality, io, admin)) |>
  mutate(across(c(wd_id, bn_geo, admin, adminLabel), ~na_if(., "")))


# no admin territory or wd_geo
# top_n is unusual in this set. needed to fix lexden. more once you add admin.
bn_io_locality <-
  bn_io_locality_query |>
  distinct(localityLabel, locality, ioLabel, io, wd_id, bn_geo) |>
  group_by(locality) |>
  arrange(ioLabel, .by_group = TRUE) |> # should result in locality.
  top_n(1, row_number()) |>
  ungroup()  |>
  arrange(localityLabel) |>
  # drop a few locality that are actually i/o Qs
  filter(!locality %in% c("Q86", "Q619", "Q2961")) 




## fetch all wd geo coords P625 that are available for BN items 
## not trying to fetch any other kind of wikidata geo coords of which there are a few.
# (about 1300. nb some multis.)

wd_geo_sparql <-
  'SELECT distinct ?item ?itemLabel ?wd ?wd_geo
WHERE {
  ?item bnwdt:P117 ?wd .
      bind(iri(concat("http://www.wikidata.org/entity/", str(?wd))) as ?wikidata) .
      SERVICE <https://query.wikidata.org/sparql> {
           ?wikidata wdt:P625 ?wd_geo .
       } # /wikidata service
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". }
}
ORDER BY ?item'

# this takes a couple of seconds, but probably worth it to have all the geos in one place
# if it doesn't work well, convert to a sub routine. [something like: gather every potentially locatable Q in data and use spql>sparql]
wd_geo_query <-
  bn_std_query(wd_geo_sparql) |>
  make_bn_ids(item)

# deduping. 
# there are occasionally multi geos for one wikidata item; very occasionally multi wikidata IDs for one BN item (London Museum/MoLondon).
# reduce to one row per item; occasionally might not get the optimal coords; to be monitored
wd_geo <-
  wd_geo_query |>
  group_by(item) |>
  arrange(itemLabel, .by_group = TRUE) |> # should work for desired london museum ID. but might not always give what you want...
  top_n(1, row_number()) |> 
  ungroup()




## get BN geos for main only. get them for street addresses (quals) as needed.
## nb that atm SAL and FS have both...

bn_geo_sparql <-
  'SELECT distinct ?item ?itemLabel  ?bn_geo ?wd
  WHERE {  
  ?item bnwdt:P153 ?bn_geo . 
  optional { ?item bnwdt:P117 ?wd .}
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
  }
  ORDER BY ?itemLabel'

bn_geo_query <-
  bn_std_query(bn_geo_sparql) |>
  make_bn_ids(item) |>
  mutate(wd = na_if(wd, ""))


## add i/o data to geo data
# remember not all geo are localities... 

bn_geo_io_locality <-
  bn_geo_query |>
  left_join(bn_io_locality |> select(locality, io, ioLabel), by=c("item"="locality"))

wd_geo_bn_locality <-
  wd_geo |>
  left_join(
    bn_io_locality |> select(-localityLabel, -bn_geo), by=c("item"="locality", "wd"="wd_id") 
  ) 


# other way round: add wd_geo to io localities . seems unlikely to be useful?
# bn_locality_wd_geo <-
# bn_io_locality |>
#   left_join(wd_geo |> select(-itemLabel), by=c("locality"="item", "wd_id"="wd")) 


# combine wd_geo and bn_geo (incl localities)
geo_bn_wd_locality <-
  bn_geo_io_locality |>
  bind_rows(wd_geo_bn_locality)



## adding the geos to data goes something like...

# # query will need a per-s unique id.
# bn_*_loc_id <-
#   bn_*_loc_query |>
#   # occasional multis so you need a unique ID; if it's based on s it's easy to turn it back into the original
#   group_by(s) |>
#   mutate(s_id = paste(s, row_number(), sep = ".")) |>
#   ungroup()  |>
# prioritise variable to use for linked p2 (location should always be first)
# mutate(p2_id =case_when(
#   !is.na(location) ~ location,
#   !is.na(employer) ~ employer,
#   !is.na(organised) ~ organised,
#   !is.na(of) ~ of
# )) |>
# so you know which one you used
#   mutate(p2_type = case_when(
#     !is.na(location) ~ "location",
#     !is.na(employer) ~ "employer",
#     !is.na(organised) ~ "organised",
#     !is.na(of) ~ "of"
#     # now you also have a way to filter the ones that don't have any!
#   ))

# # direct geo_coords to do location/organised/of/employer separately rather than using consolidated p2_id
# # the exact choice of variables here is likely to vary depending on analysis. 
# # but the principle should be generalisable. maybe even functionifiable.
# bn_*_loc_geos <-
#   bn_*_loc_id |>
#   select(bn_id, location, organised, of, employer, s_id) |>
#   pivot_longer(location:employer, names_to = "loc_type", values_to = "loc_id", values_drop_na = TRUE) |>
#   # any possibility of dups with geo_bn_wd? fine atm but there's a bind_rows that coudl in the future fck things up
#   inner_join(geo_bn_wd_locality, by=c("loc_id"="item")) |>
#   pivot_wider(id_cols = c(s_id), names_from = loc_type, values_from = c(wd_geo, bn_geo) ) 

# # rejoin the geos to the work df (by s_id rather than s)
# bn_*_loc <-
#   bn_*_loc_id |>
#   left_join(bn_*_loc_geos, by="s_id")


# 
# 
# ## spql ####
# moved to std_queries.
# 


## todo: areas ????
