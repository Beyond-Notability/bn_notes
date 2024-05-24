## getting this has to go AFTER std_query.r and dates fetching.



## fetch dates for women from wikibase ####

## main dates PIT 
# bn_women_dates_main_pit_sparql <-
#   'SELECT distinct ?person ?personLabel ?date_propLabel ?date_pit ?date_pit_precision  ?date_prop  ?s
# bn_women_dates_main_pit_query <-
#   bn_std_query(bn_women_dates_main_pit_sparql) |>

## separate main EDTF query
# bn_women_dates_main_edtf_sparql <-
#   'SELECT distinct ?person ?personLabel ?date_edtf  ?date_prop ?s
# bn_women_dates_main_edtf_query <-
#   bn_std_query(bn_women_dates_main_edtf_sparql) |>
## renamed date_prop_label to *Label. 
## put pit and edtf together
# bn_women_dates_main_query <- 
#   bind_rows(
#     bn_women_dates_main_pit_query,
#     bn_women_dates_main_edtf_query
#   )

## qualifier dates
## improved query. but still slow! original is in ppa-2023-12-08 qmd for reference.
# bn_women_dates_qual_sparql <-
#   'SELECT distinct ?person ?personLabel ?propLabel ?prop_valueLabel ?date_qual  ?date_qual_precision ?qual_date_prop ?qual_date_propLabel ?prop_value ?prop ?s
# update no longer has a date_qual_simple column. qual_date_propLabel instead of date_qual_label.
# bn_women_dates_qual_query <-
#   bn_std_query(bn_women_dates_qual_sparql) 



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




## dates processing ####


bn_women_dates_main <-
  bn_women_dates_main_query  |>
  # in case there are any <uv>, and keep the original.
  mutate(date_pit_parsed = if_else(str_detect(date_pit, "t"), NA, date_pit))  |>
  mutate(date_pit_parsed = parse_date_time(date_pit_parsed, "ymdHMS"))  |>
  # edtf certainty. currently little use but likely to become more significant
  # The characters '?', '~' and '%' are used to mean "uncertain", "approximate", and "uncertain" as well as "approximate", respectively. These characters may occur only at the end of the date string and apply to the entire date.
  # parse_date_time ignores ? and ~ but fails on % . handle with str_remove() 
  mutate(date_certainty = case_when(
    str_detect(date_edtf, "%$") ~ "approx-uncertain", 
    str_detect(date_edtf, "\\?$") ~ "uncertain",
    str_detect(date_edtf, "~$") ~ "approx"
  )) |>
  # remove any % from edtf dates before parsing. do same to ~ and ? as well just in case
  mutate(date_edtf_parsed = parse_date_time(str_remove(date_edtf, "[%?~]$"), c("ymd", "ym", "y")) ) |>
  # make single date / year column. 
  mutate(date = if_else(!is.na(date_pit_parsed), date_pit_parsed, date_edtf_parsed)) |>
  mutate(year = year(date)) |>
  # you could do something like this (after parsing and consolidating pit/edtf, dummy)
  # mutate(date_string = case_when(
  #          date_pit_precision==11 ~  as.character(date),
  #          date_pit_precision==10 ~ str_replace(as.character(date), "-01$", "-00"),
  #          date_pit_precision==9 | str_length(date_edtf)==4 ~ paste0(year, "-00-00"),
  #          str_length(date_edtf)==10 ~ date_edtf,
  #          str_length(date_edtf)==7 ~ paste0(date_edtf, "-00")
  #       ) ) |>
  mutate(date_precision = case_when(
    date_pit_precision==11 | str_length(date_edtf)==10 ~ "ymd",
    date_pit_precision==10 | str_length(date_edtf)==7 ~ "ym",
    date_pit_precision==9 | str_length(date_edtf)==4 ~ "y",
  )) |>
  # add a type label
  mutate(date_label = case_when(
    !is.na(date_pit) ~ "point in time", 
    !is.na(date_edtf) ~ "edtf"
  )) |>
  mutate(date_level = "main") |>
  relocate(date, year, date_precision, date_certainty, date_label, date_level, .after = date_propLabel) |>
  relocate(date_pit:date_edtf, .after = person)



bn_women_dates_qual <-
  bn_women_dates_qual_query |>
  mutate(date = if_else(str_detect(date_qual, "t"), NA, date_qual))  |> # shouldn't be any of these actually!
  mutate(date = parse_date_time(date, "ymdHMS"))  |>
  mutate(year = year(date)) |>
  mutate(date_string = case_when(
    date_qual_precision==11 ~  as.character(date),
    date_qual_precision==10 ~ str_replace(as.character(date), "-\\d\\d$", "-00"),
    date_qual_precision==9 ~ paste0(year, "-00-00")
  ) ) |>
  mutate(date_precision = case_when(
    date_qual_precision==11  ~ "ymd",
    date_qual_precision==10 ~ "ym",
    date_qual_precision==9 ~ "y",
  )) |>
  mutate(date_certainty = case_when(
    str_detect(qual_date_propLabel, "earliest") ~ "earliest",
    str_detect(qual_date_propLabel, "latest") ~ "latest"
  )) |>
  # rename to match date_label in mains. 
  rename(date_label = qual_date_propLabel) |>
  mutate(date_level = "qual") |>
  relocate(date, year, date_precision, date_certainty, date_label, date_level, date_string, .after = personLabel) |>
  # remove AT recorded by dates. ask about these? i suspect not current practice and they're awkward.
  # but keep na for now, check them.
  filter(year < 2020 | is.na(year))



# why is date_prop for main dates going missing here? because you used the : and moved things isn't it? NEVER USE :
bn_women_dates <-
  bind_rows(
    bn_women_dates_main |> select(-date_pit,-date_pit_parsed,-date_edtf,-date_edtf_parsed) ,
    bn_women_dates_qual |> select(-date_qual, -date_qual_precision, -date_string) |> # drop date_string because dates_main doesn't have it...
      rename(date_prop=prop, date_propLabel= propLabel)
  )  |>
  relocate(prop_valueLabel, .after = date_propLabel) |>
  relocate(prop_value, .after = date_prop) |>
  arrange(bn_id)



## add (broad) categories ####

## combine PPA buckets with sorted properties for broad categories for dates.
## if relevant new properties are added this may need amending
## for PPAs use bucket labels.
bn_dates_properties_buckets_categories <-
bn_sorted_properties |> 
  left_join(
    bn_ppa_buckets |> rename(bucket=label), by=c("bn_prop_id"="ppa_id")
  ) |>
  mutate(category = case_when(
    !is.na(bucket) ~ "PPA",
    #bn_prop_id %in% c("P94") ~ "education",   # don't understand why i included student of "P95"... now that educated at is in PPA you need a different filter to group it with academic degree anyway.
    bn_prop_id %in% c("P59", "P26", "P15", "P29") ~ "personal", # not sure why I had some IDs in here: "P142", "P143", "P134", HISCO/ALBS/90s 
    section == "Family" ~ "family",
    #section %in% c("Other Resources", "Land-Proprietor Activity") ~ "other". don't need this, just leave them out
  )) 

# based on sorted properties (bn_sorted_properties s/b in std_queries r)
# instance of really doesn't add anything to section categories, dropped.
# subcat may still be needed for non-PPAs?

bn_women_dates_categories <-
  bn_women_dates |>
  left_join(
    bn_dates_properties_buckets_categories |>
      select(bn_prop_id, ppa_bucket=bucket, section, category, label), by=c("date_prop"="bn_prop_id")
  ) |>
  # for display labels only; don't use in processing
  # remove minor variations in name labels: free text v item, EDTF v PIT
  # don't forget can't use std_ col for joins back to original...
  mutate(date_prop_label_std = str_trim(str_remove(date_propLabel, "\\((free text|item|EDTF value|PIT value)\\)"))) |>
  # shorten a few very long labels...
  mutate(date_prop_label_std = case_when(
    date_prop=="P36" ~ "excavation director",
    date_prop=="P37" ~ "excavation worker",
    date_prop=="P142" ~ "paternal occupation",
    date_prop=="P38" ~ "archaeological committee",
    date_prop=="P49" ~ "proposed CAS",
    date_prop=="P7" ~ "proposed RAI",
    date_prop=="P16" ~ "proposed SAL",
    date_prop=="P155" ~ "signed RHS personal",
    .default = date_prop_label_std
  )) 

