## this has to go AFTER std_query r.



## fetch all dates for women from wikibase ####

## main dates PIT 

bn_women_dates_main_pit_sparql <-
  'SELECT distinct ?person ?personLabel ?date_propLabel ?date_pit ?date_pit_precision  ?date_prop  ?s
  WHERE {
   ?person bnwdt:P3 bnwd:Q3 . #select women
   FILTER NOT EXISTS { ?person bnwdt:P4 bnwd:Q12 . } 
   ?person ?p ?s .   
      ?date_prop wikibase:claim ?p .  
      ?date_prop wikibase:propertyType wikibase:Time . # for PIT only.  
   
  # get dates detail via ?s and psv
      ?s ?psv ?wdv .
        ?wdv wikibase:timeValue ?date_pit ;
           wikibase:timePrecision ?date_pit_precision .

 SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en-gb,en". } 
  
} # /where
ORDER BY ?person ?date_pit'


bn_women_dates_main_pit_query <-
  bn_std_query(bn_women_dates_main_pit_sparql) |>
  make_bn_item_id(person)  |>
  make_bn_ids(c(s, date_prop)) |>
  #mutate(across(c(date_propLabel, date_pit), ~na_if(., ""))) |>
  #make_date_year() |> # leave this to the next stage.
  relocate(person, .after = last_col()) 

## updated with separate main EDTF query
## will need adjusting if any new EDTF date properties are added

bn_women_dates_main_edtf_sparql <-
  'SELECT distinct ?person ?personLabel ?date_edtf  ?date_prop ?s
   WHERE {
 
    ?person bnwdt:P3 bnwd:Q3 . 
    FILTER NOT EXISTS { ?person bnwdt:P4 bnwd:Q12 . } 

    ?person ( bnp:P131 | bnp:P132 | bnp:P133  ) ?s .
         ?s ( bnps:P131 | bnps:P132 | bnps:P133 ) ?date_edtf .
  
         ?s ?date_prop ?date_edtf .   

    ## filter for edtf dates
    ## docs: https://github.com/ProfessionalWiki/WikibaseEdtf
    ## cant see any way other than a filter to get the edtf value.
      FILTER ( datatype(?date_edtf) = xsd:edtf  ) .
  
 SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en-gb,en". } 

} # /where

ORDER BY ?person ?date_edtf'

bn_women_dates_main_edtf_query <-
  bn_std_query(bn_women_dates_main_edtf_sparql) |>
  make_bn_item_id(person)  |>
  make_bn_ids(c(s, date_prop)) |>
  # make date labels here
  mutate(date_propLabel = case_when(
    date_prop=="P131" ~ "had child in",
    date_prop=="P132" ~ "was married in (EDTF value)",
    date_prop=="P133" ~ "was widowed in"
  )) |>
  relocate(s, person, .after = last_col())


## need to check whether you've used bn_women_dates_main_query anywhere else
## renamed date_prop_label to *Label. 

##put pit and edtf together... should be identical to original version...
bn_women_dates_main_query <- 
  bind_rows(
    bn_women_dates_main_pit_query,
    bn_women_dates_main_edtf_query
  )


# updated with improved query. but still slow! original is in ppa-2023-12-08 qmd for reference.

bn_women_dates_qual_sparql <-
  'SELECT distinct ?person ?personLabel ?propLabel ?prop_valueLabel ?date_qual  ?date_qual_precision ?qual_date_prop ?qual_date_propLabel ?prop_value ?prop ?s

WHERE {
    ?person bnwdt:P3 bnwd:Q3 . # women
    FILTER NOT EXISTS { ?person bnwdt:P4 bnwd:Q12 . } 
  
    # get stuff about ?person   
    ?person ?p ?s .   
  
      # the claim for ?p .  do i need psv as well as ps?
      ?prop wikibase:claim ?p;      
         wikibase:statementProperty ?ps.
 
  # the direct value (usually item) for the property, things like annual meeting, girton college. .
        ?s ?ps ?prop_value.
     
  # qualifier timevalue and precision. 
  # pit/start/end/earliest/latest 
      ?s ?pqvp ?pqv.
          ?pqv wikibase:timeValue ?date_qual .  
          ?pqv wikibase:timePrecision ?date_qual_precision .
          
  # works without dups. use *Label for the prop label.
        ?s ?pq ?date_qual .   
          ?qual_date_prop wikibase:qualifier ?pq .
          ?qual_date_prop wikibase:propertyType wikibase:Time.            
      
 SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en, en-gb". } 
  
} # /where

ORDER BY ?personLabel ?s ?prop_label'


# update no longer has a date_qual_simple column. qual_date_propLabel instead of date_qual_label.
bn_women_dates_qual_query <-
  bn_std_query(bn_women_dates_qual_sparql) |>
  make_bn_item_id(person) |> 
  make_bn_ids(c(qual_date_prop, prop_value, prop, s)) |>
  relocate(person, .after = last_col())



# pretty sure these queries drop <uv> dates. but might not always be the case
# qual prop value can contain stuff other than Qs


## edtf-notes ####

## docs: The characters '?', '~' and '%' are used to mean "uncertain", "approximate", and "uncertain" as well as "approximate", respectively. These characters may occur only at the end of the date string and apply to the entire date.

#    parse_date_time('1984?', "y")  # ? is ignored and date parsed as 1984-01-01  
#    parse_date_time('2004-06~', "ym") # ~ is ignored and date parsed as 2004-06-01
#    parse_date_time('2004-06-11%', "ymd")   # **fails to parse**
# parse_date_time(str_remove('2004%', "%$"), "y") # ok

## edtf documentation https://www.loc.gov/standards/datetime/
## wikibase Time datatype https://www.wikidata.org/wiki/Help:Dates#Time_datatype




## followed by dates processing ####
## source(here::here(_R/dates_processing.R))
