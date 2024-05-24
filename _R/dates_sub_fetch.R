## after shared/std_queries
## to fetch subsets of dates in consistent format for processing.


# three steps: spql -> sparql -> query
## glue values string has to be specific; will often be *bn_bnwd_values*; and does it have to match values= in sparql?
# spql likely to be reusable for variants of the same query

## spql for *ALL* qualifier dates for a VALUES list of people
## likely to still be quite slow for any number of people so ideally should only be used for small groups.
bn_qual_dates_spql <-
  'SELECT distinct ?person  ?date_qual  ?date_qual_precision ?date_qual_prop ?s  
  WHERE {
    values ?person { <<bn_bnwd_values>> }
    ?person ?p ?s .
      ?prop wikibase:claim ?p. # timed out *without* this
  # qualifier timevalue and precision.
  # pit/start/end/earliest/latest. noticeably slower with the last 2.
      ?s (bnpqv:P1 | bnpqv:P27 | bnpqv:P28 | bnpqv:P51 | bnpqv:P53  ) ?pqv. 
      ?s ?date_qual_prop ?pqv.
          ?pqv wikibase:timeValue ?date_qual .
          ?pqv wikibase:timePrecision ?date_qual_precision .
} # /where

ORDER BY ?person ?s'


## example for selective properties
## now you can drop ?p and claim which makes a BIG difference to query times
# bn_qual_work_dates_spql <-
#   'SELECT distinct ?person  ?date_qual  ?date_qual_precision ?date_qual_prop ?s
#    WHERE {
#     values ?person { <<bn_bnwd_values>> }
# 
#    # work activities: held position / held position (free text) /  employed as
#     ?person ( bnp:P17|bnp:P48|bnp:P105 ) ?s . 
# 
#   # qualifier timevalue and precision.
#       ?s (bnpqv:P1 | bnpqv:P27 | bnpqv:P28 | bnpqv:P51 | bnpqv:P53  ) ?pqv. 
#       ?s ?date_qual_prop ?pqv.
#           ?pqv wikibase:timeValue ?date_qual .
#           ?pqv wikibase:timePrecision ?date_qual_precision .
# } # /where
# ORDER BY ?person ?s'

# bn_qual_work_dates_sparql <-
#   bn_work_dates_query |> 
#   bn_make_union(bn_id) |>
#   mutate_glue_sparql(bn_qual_work_dates_spql)

# bn_qual_work_dates_query <-
#   bn_std_query(bn_qual_work_dates_sparql) |>
#   make_bn_item_id(person) |>
#   make_bn_ids(c(s, date_qual_prop)) |>
#  # better way to add date prop and other labels!
#  left_join(bn_properties |> select(date_qual_prop= bn_prop_id, date_qual_prop_label= propertyLabel), by="date_qual_prop") |>
#  #mutate(date_qual_prop_label = date_property_labels(date_qual_prop)) |>
#   mutate(date_qual = if_else(str_detect(date_qual, "^_:t"), NA, date_qual))  |>
#   mutate(date_qual = parse_date_time(date_qual, "ymdHMS"))  |>
#   mutate(year_qual = year(date_qual)) |>
#   select(-person)



## spql for main dates point in time, birth/death/marriage
bn_women_dates_main_pit_bmd_spql <-
  'SELECT distinct ?person ?date_pit ?date_pit_precision  ?date_prop  ?s
  WHERE {
    values ?person { <<bn_bnwd_values>> }
   ?person ( bnp:P15 | bnp:P26 | bnp:P130 ) ?s.
      ?s ?date_prop ?wdv .
        ?wdv wikibase:timeValue ?date_pit ;
             wikibase:timePrecision ?date_pit_precision .
} # /where
ORDER BY ?person ?date_pit'

# example usage 
# bn_work_dates_main_pit_bmd_sparql <-
#   bn_work_dates_query |> 
#   bn_make_union(bn_id) |>
#   mutate_glue_sparql(bn_women_dates_main_pit_bmd_spql)
# 
# bn_work_dates_main_pit_bmd_query <-
#   bn_std_query(bn_work_dates_main_pit_bmd_sparql) |>
#   make_bn_item_id(person)  |>
#   make_bn_ids(c(s, date_prop)) |>
#   select(-person)
# #date labels. name date_propLabel
#  left_join(bn_properties |> select(date_prop=bn_prop_id, date_propLabel=propertyLabel), by="date_prop") 



## spql for EDTF bmd dates
## will need adjusting if any new EDTF date properties are added
bn_women_dates_main_edtf_bmd_spql <-
  'SELECT distinct ?person ?date_edtf  ?date_prop ?s
   WHERE {
    values ?person { <<bn_bnwd_values>> }

    ?person ( bnp:P131 | bnp:P132 | bnp:P133  ) ?s .
         ?s ?date_prop ?date_edtf .   

    ## filter for edtf date values
    FILTER ( datatype(?date_edtf) = xsd:edtf  ) .
} # /where

ORDER BY ?person ?date_edtf'

# #date labels. need to be named date_propLabel
#  left_join(bn_properties |> select(date_prop=bn_prop_id, date_propLabel=propertyLabel), by="date_prop") 
#     date_prop=="P131" ~ "had child in",
#     date_prop=="P132" ~ "was married in (EDTF value)",
#     date_prop=="P133" ~ "was widowed in"


## named child birth and spouse death dates

bn_women_dates_main_add_bmd_spql <-
  'SELECT distinct ?person  ?related   ?date_prop  ?date_pit ?date_pit_precision ?s
   WHERE {
     values ?person { <<bn_bnwd_values>> }

  # P45 named children (there were a few in both named child and had child in, from dates. [wherry / hodgson])
    {?person bnp:P45 ?s.
        ?s bnps:P45 ?related .
        ?related bnp:P26 ?ss . } # birth date
     union 
  # P41 spouses
    {?person bnp:P41 ?s.
        ?s bnps:P41 ?related .
        ?related bnp:P15 ?ss . }   # death date  
     
            ?ss ?date_prop ?wdv .
                 ?wdv wikibase:timeValue ?date_pit .
                 ?wdv wikibase:timePrecision ?date_pit_precision .
} # /where
ORDER BY ?person ?date_pit'


## might be followed by dates_processing.R ...