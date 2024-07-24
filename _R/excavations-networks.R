## general networks functions ####

source(here::here("_R/networks-functions.R"))


## temporary dates from descriptions
## for convenience make eg "1930s" first half of decade "1930-1935" (really only using start dates and broad periods so it's not a big deal)
bn_excavations_circa_dates <-
  tribble(
    ~id , ~circa_dates, ~circa_year1, ~circa_year2,
    "Q3200" , "1937-39" , 1937, 1939,
    "Q3893" , "1932", 1932, 1932,
    "Q1631" , "early 20th century", 1900, 1905,
    "Q2589" , "1930s", 1930, 1935,
    "Q3729" , "1950", 1950, 1950,
    "Q3125" , "1948-51", 1948, 1951,
    "Q3727" , "1920s/30s", 1920, 1935,
    "Q3334" , "1930s", 1930, 1935,
    "Q3343" , "1920s", 1920, 1925,
    "Q2026" , "1930s" , 1930, 1935,
    "Q3175" , "1929-34", 1929, 1934,
    "Q2516" , "1920s", 1920, 1925,
    "Q3726" , "1920s", 1920, 1925
    #"Q4386", "1911-12", 1911, 1912
  ) 


# networks functions mostly moved to networks-functions.R. any more specific functions can go here.


# make nodes and edges lists ####
# do any data filtering etc before feeding to the function
# using person for to and from. [tbl_graph will need node_key=person]
# data is assumed to be people_participants_named or a subset of it
# these are probably redundant now.

bn_excavations_node_list <- function(data){
  data |>
  group_by(person) |>
  summarise(grp_excavations = n() , .groups = "drop_last") |>
  ungroup() |>
  arrange(person)|>
  inner_join(
    bn_excavations_people_participants_named |>
      count(person, personLabel, gender, year1, year2, name="all_excavations"), by="person")  |>
  mutate(person_id = row_number())
}

bn_excavations_edge_list <- function(data){
  data |>
  #distinct(excavation, person)
  pairwise_count(person, excavation, upper=F) |>
  select(from=item1, to=item2, weight=n) |>
  arrange(from, to)
}


# sub-network for time periods, with just one tbl graph as output
# data = bn_excavations_people_participants_named or df with cols:
# person, personLabel, excavation, excavationLabel, gender, e_year1, year1, year2, years
# start = start year; period = desired period of time, 1 yr, 5 yrs, 10 yrs etc.

bn_excavations_grp_network <- function(data, start, period){

  # make end year
  end <- start + period - 1  # have to subtract 1!

  # filter data.
  filtered_data <-
  data |>
  filter(between(e_year1, start, end))

  nodes <-
  filtered_data |>
  group_by(person) |>
  summarise(grp_excavations = n() , .groups = "drop_last") |>
  ungroup() |>
  arrange(person) |>
  inner_join(
    data |>
      count(person, personLabel, gender, year1, year2, name="all_excavations"), by="person")  |>
    # should match the numerical ids in edges...
  mutate(person_id = row_number())

  edges <-
  filtered_data |>
  #distinct(excavation, person)
  pairwise_count(person, excavation, upper=F) |>
  select(from=item1, to=item2, weight=n) |>
  arrange(from, to)

  bn_tbl_graph(nodes, edges)
}



# same process but for all data or an already filtered subset. 
bn_excavations_make_network <- function(data){
  nodes <-
  data |>
  group_by(person) |>
  summarise(grp_excavations = n() , .groups = "drop_last") |>
  ungroup() |>
  arrange(person) |>
  inner_join(
    data |>
      count(person, personLabel, gender, year1, year2, name="all_excavations"), by="person")  |>
    # should match the numerical ids in edges...
  mutate(person_id = row_number())

  edges <-
  data |>
  #distinct(excavation, person)
  pairwise_count(person, excavation, upper=F) |>
  select(from=item1, to=item2, weight=n) |>
  arrange(from, to)

  bn_tbl_graph(nodes, edges)
}




# wikibase data


# separate queries for persons and excavations initially
# three lots of info
# a) excavation>excavation 
# b) excavation>participants  - "participants" = directors and members only
# c) person>excavation participation


## gender ####

# just get it for everyone and then join. it doesn't take any time.
bn_gender_sparql <-
  'SELECT ?person  ?genderLabel  
   WHERE {  
      ?person bnwdt:P3 ?gender . 
   SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
  }'
bn_gender_query <-
  bn_std_query(bn_gender_sparql) |>
  make_bn_ids(person) |>
  # fix <uv> gender
  mutate(gender = if_else(genderLabel %in% c("man", "woman"), genderLabel, NA)) 


## excavation ####

bn_excavations_main_sparql <-
  'SELECT ?excavation ?excavationLabel ?propLabel ?valueLabel ?value  ?prop  ?s 

WHERE {  
   # instance of excavation  (128)
   ?excavation bnwdt:P12 bnwd:Q38 .
  
    ?excavation ?p ?s .
  
      ?prop wikibase:claim ?p .
      ?prop wikibase:statementProperty ?ps .
  
   optional { ?s ?ps ?value . } # need optional to keep a <uv> member.

  
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}'

bn_excavations_main_query <-
  bn_std_query(bn_excavations_main_sparql) |>
  make_bn_ids(c(excavation, prop, value, s)) |>
  mutate(across(c(valueLabel, value), ~na_if(., ""))) |>
  arrange(excavationLabel, propLabel) |>
  # filter out 1960 excavation
  filter(excavation !="Q3372")



## excavation>participants  ####

## all directors and members in excavation pages; drop a few duplicate rows 

bn_excavations_participants <-
  bn_excavations_main_query |>
  filter(prop %in% c("P36", "P37"))  |>
  # a few <uv> which seem to be unnamed groups; turn into NA
  uv_to_na_across(c(value, valueLabel)) |>
  ## give unnamed people unique IDs
  mutate(person_rn = if_else(value=="Q576", paste(value, row_number(), sep="_"), value)) |>
  left_join(bn_gender_query |> select(-genderLabel), by=c("value"= "person")) |>
  select(excavation, excavationLabel, person=value, personLabel=valueLabel, gender, roleLabel= propLabel, role= prop, person_rn, s) |>
  # drop a couple of dups
  group_by(person_rn, role, excavation) |>
  top_n(1, row_number()) |>
  ungroup() 




bn_excavations_qual_sparql <-
  'SELECT ?excavation ?excavationLabel ?prop ?propLabel  ?qual_propLabel ?qual_valueLabel  ?qual_value ?qual_prop ?s

  WHERE {  
   ?excavation bnwdt:P12 bnwd:Q38 .
  
    ?excavation ?p ?s .
        ?prop wikibase:claim ?p .
  
    ?s ?qual_p ?qual_value .
        ?qual_prop wikibase:qualifier ?qual_p
  
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}'

bn_excavations_qual_query <-
  bn_std_query(bn_excavations_qual_sparql) |>
  make_bn_ids(c(excavation, prop, qual_prop, s))|>
  # filter out 1960 excavation
  filter(excavation !="Q3372")


## excavation>excavation dates in instance of ####

bn_excavations_excavation_dates <-
  bind_rows(
    # qualifier dates
    bn_excavations_qual_query |>
      # keep qual dates that *aren't* directors/members (participants) - funders, organised by etc
      filter(qual_prop %in% c("P1", "P27", "P28") & !prop %in% c("P36", "P37") ) |> 
      # only keep qual dates that are i/o dates
      #filter(qual_prop %in% c("P1", "P27", "P28") & prop=="P12") |> # i/o qual dates
      mutate(date = parse_date_time(qual_value, "ymdHMS")) |>
      mutate(year = year(date)) |>
      select(excavation, excavationLabel, propLabel, prop, year, date_type= qual_propLabel, s) ,
    
    # main dates
    bn_excavations_main_query |>
      filter(prop %in% c("P1", "P27", "P28")) |> # dont bother with earliest/latest
      mutate(date = parse_date_time(value, "ymdHMS")) |>
      mutate(year = year(date))   |>
      mutate(date_type = propLabel) |>
      select(excavation, excavationLabel, propLabel, prop, year, date_type, s) 
  ) |>
  arrange(excavation, year, date_type) 

# excavation>participants dates in qualifiers. 

bn_excavations_participants_qual_dates <-
  bn_excavations_qual_query |>
  # keep only qual dates for dir/mem.
  filter(qual_prop %in% c("P1", "P27", "P28") & prop %in% c("P36", "P37") ) |> 
  #filter(qual_prop %in% c("P1", "P27", "P28") & prop!="P12") |> # exclude i/o dates
  mutate(date = parse_date_time(qual_value, "ymdHMS")) |>
  mutate(year = year(date))  |>
  # add person IDs (and remove the dups as well)
  inner_join(bn_excavations_participants |> select(s, person), by="s")  |>
  select(excavation, excavationLabel, person, propLabel, prop, year, date_type= qual_propLabel, s) |>
  arrange(excavationLabel, year) 





## person>excavation participation ####

bn_people_excavations_sparql <-
  'SELECT ?person ?personLabel ?excavationLabel ?excavation ?qual_propLabel ?qual_valueLabel ?qual_value ?ex_ioLabel ?role ?qual_prop ?ex_io ?s 

WHERE {  
   # people only. 
   ?person bnwdt:P12 bnwd:Q2137 .
   
     ?person (bnp:P36 | bnp:P37) ?s .
       ?s (bnps:P36|bnps:P37) ?excavation .
       ?s ?role ?excavation . # only two roles so dont bother with labels

  ?excavation bnwdt:P12 ?ex_io .  filter(?ex_io != bnwd:Q618 ). # check i/o for the excavation. one multi atm, just filter.
  
   # get any qualifiers
    optional { 
      ?s ?qual_p ?qual_value .   
      ?qual_prop wikibase:qualifier ?qual_p. 
     }
   
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}'

bn_people_excavations_query <-
  bn_std_query(bn_people_excavations_sparql) |>
  #make_bn_item_id(person) |>
  make_bn_ids(c(person, excavation, qual_prop, role, ex_io, s))|>
  # filter out 1960 excavation
  filter(excavation !="Q3372") |>
  mutate(roleLabel = if_else(role=="P36", "director of archaeological fieldwork", "member of excavation during archaeological fieldwork")) |>
  mutate(across(c(qual_prop, qual_propLabel, qual_valueLabel, qual_value, ex_io, ex_ioLabel), ~na_if(., ""))) |>
  # add gender
  left_join(bn_gender_query |> select(-genderLabel), by="person") |>
  #relocate(person, .after = last_col()) |>
  arrange(person, excavation)


bn_people_excavations_dates <-
  bn_people_excavations_query |>
  filter(qual_prop %in% c("P1", "P27", "P28")) |>
  mutate(date = parse_date_time(qual_value, "ymdHMS")) |>
  mutate(year = year(date)) |>
  select(person, date, year, date_type=qual_propLabel, excavation, role, s)



## excavation covering dates summaries ####

# separate out people/participants' dates from other dates; make lists of all dates as well as earliest/latest years

# update: include temporary circa dates . 
bn_excavations_excavations_with_circa_dates <-
  bn_excavations_excavation_dates |>
  bind_rows(
    # circa dates need pivot
    bn_excavations_circa_dates |>
      rename(excavation=id) |>
      pivot_longer(circa_year1:circa_year2, names_to = "date_type", values_to = "year")
  ) 

# summarised version; probably won't need this
bn_excavations_excavations_covering_dates <-
  bn_excavations_excavations_with_circa_dates |>
  group_by(excavation) |>
  arrange(year, .by_group = T) |>
  summarise(years = paste(unique(year), collapse = " "), year1 = min(year), year2 = max(year)) |>
  ungroup()


# participants is p36/p37 only so you'll need to get the other qual dates as well
# not keeping role; would need a tweak if you change current one person per excavation approach 
# is this dates before reducing roles? think so. that's ok isn't it?
bn_excavations_participants_covering_dates <-
  bn_excavations_participants_qual_dates|>
  group_by(person, excavation)  |>
  arrange(year, .by_group = T) |>
  summarise(years = paste(unique(year), collapse = " "), year1 = min(year), year2 = max(year), .groups = "drop_last") |>
  ungroup()


# again not including role. before reduction of roles; would need a tweak if you change that
bn_people_excavations_covering_dates <-
  bn_people_excavations_dates |>
  group_by(person, excavation) |>
  summarise(years = paste(unique(year), collapse = " "), year1 = min(year), year2 = max(year) , .groups = "drop_last") |>
  ungroup()


# merged covering dates for people and participants; should represent every date associated with a member/director on an excavation. 
# focusing on excavation dates overall, probably don't need this
bn_excavations_people_participants_covering_dates <-
  bind_rows(
    bn_excavations_participants_qual_dates,
    bn_people_excavations_dates
  ) |>
  filter(!is.na(year)) |>
  group_by(person, excavation) |>
  summarise(p_years = paste(unique(year), collapse = " "), p_year1 = min(year), p_year2 = max(year) , .groups = "drop_last") |>
  ungroup()


# merge ALL dates from all sources into covering dates which represent every date associated with an excavation
# use excavations with circa dates 
bn_excavations_all_covering_dates <-
  bind_rows(bn_excavations_participants_qual_dates, 
            bn_excavations_excavations_with_circa_dates,
            bn_people_excavations_dates
  ) |>
  filter(!is.na(year)) |> # in case.
  group_by(excavation) |>
  arrange(year, .by_group = T) |>
  summarise(e_years = paste(unique(year), collapse = " "), year1 = min(year), year2 = max(year)) |>
  ungroup()



# a base for excavations with dates?
# one row per excavation
# but omits a number of "excavations" on person pages. i think they're some scrappy things. so probably need to avoid this for people-y analysis or as a starting point for anything
# bn_excavations <-
# bn_excavations_main_query |>
#   distinct(excavation, excavationLabel) |>
#   left_join(bn_excavations_all_covering_dates, by="excavation")  


## people excavations ####
# distinct *with* role; occasionally people have both. 
bn_people_excavations <-
  bn_people_excavations_query |>
  distinct(person, personLabel, roleLabel, excavationLabel, excavation, role, gender, ex_ioLabel, ex_io, s) 


# without role in case you want it.
bn_people_excavations_no_roles <-
  bn_people_excavations_query |>
  distinct(person, personLabel, gender, excavation, excavationLabel, ex_ioLabel, ex_io) 


## bring all participants together from excavation and person src  ####

bn_excavations_people_participants_all <-
  bind_rows(
    bn_excavations_participants |> 
      # exclude NA person
      filter(!is.na(person)) |> 
      mutate(ex_io="Q38", ex_ioLabel="excavation", src="ex"),
    bn_people_excavations |> 
      mutate(person_rn=person, src="ppl") 
  )  |>
  arrange(person, role, excavation, src) |> 
  mutate(role_short = word(roleLabel)) |>
  # just use e dates
  #left_join(bn_excavations_people_participants_covering_dates, by=c("person", "excavation")) |>
  left_join(bn_excavations_all_covering_dates, by="excavation")  
#left_join(bn_excavations_circa_dates, by=c("excavation"="id"))


## dedup and reduce: **if someone has both director and member role on the same excavation, keep dir only.**
# you need individual dates as well as excavation dates
bn_excavations_people_participants <-
  bn_excavations_people_participants_all |>
  # hmm, some have both roles but not in the same src. seems tricky to do in one step.....
  group_by(person_rn, excavation, src) |>
  top_n(-1, row_number()) |>
  ungroup() |>
  #  distinct(person, personLabel, excavation, excavationLabel, gender, person_rn, role, role_short, p_year1, p_year2, p_years, e_year1, e_year2, e_years) |> original with p years
  distinct(person, personLabel, excavation, excavationLabel, gender, person_rn, role, role_short, year1, year2, e_years) |>  
  # finish off in 2nd step. can probably do better... 
  group_by(person_rn, excavation) |>
  top_n(-1, row_number()) |>
  ungroup() |>
  arrange(person, excavation)

# count of named people per excavation
bn_excavations_named_person_n <-
  bn_excavations_people_participants |>
  filter(person != "Q576" & !is.na(gender))|>
  # any specific individuals to remove. Q3388 = Grace Simpson, 1960s.
  filter(!person %in% c("Q3388"))  |>
  count(excavation, name="excavation_n")


# covering dates *per person* for nodes list
bn_excavations_people_participants_dates_summary <-
  bn_excavations_people_participants |>
  #select(person, p_year1, p_year2, e_year1, e_year2) |>
  select(person, year1, year2) |>
  # drop unnamed
  filter(person !="Q576") |>
  # pivot longer for excavation years then get min/max overall
  #pivot_longer(p_year1:circa_year2) |>
  pivot_longer(-person) |>
  filter(!is.na(value)) |>
  group_by(person) |>
  arrange(value, .by_group = TRUE) |>
  #summarise(year1=min(value), year2=max(value), years=paste(unique(value), collapse = " ")) |>
  summarise(year1=min(value), year2=max(value) )|>
  ungroup()


# named and gendered individuals only, mainly for network analysis
# also drop Q3388 Grace Simpson as out of scope
bn_excavations_people_participants_named <-
  bn_excavations_people_participants |>
  select(-person_rn, -role) |>
  #  select(person, personLabel, excavation, excavationLabel, gender, role_short, year1, year2, years) |>
  # unnamed
  filter(person !="Q576") |>
  # any specific individuals to remove
  filter(!person %in% c("Q3388")) |>
  # all but one named individual have gender, so drop that person from named as well 
  filter(!is.na(gender)) |>
  # add count of named people for the excavation
  left_join(bn_excavations_named_person_n, by="excavation")  |>
  # add person's number of excavations
  add_count(person, name="p_n_excavations") |>
  # for now use e start year [including circa].
  # mutate(e_start_year = case_when(
  #   is.na(e_year1) & !is.na(circa_year1) ~ circa_year1,
  #   .default = e_year1
  # )) |>
  # year1 and year2 should be for specific excavation, need to be renamed
  rename(e_year1=year1, e_year2=year2) |>
  # add per person covering years.
  left_join(bn_excavations_people_participants_dates_summary, by="person") |>
  # period based on covering dates for excavations
  mutate(p = case_when(
    e_year1 < 1918 ~ "1900",
    e_year1 < 1930 ~ "1920",
    e_year1 <=1940 ~ "1930",
    e_year1 > 1940 ~ "1950"
  )) 
#only 15 rows without any date.




# make (sub) networks ####


# bn_excavations_people_participants  |>
#   count(excavation, sort = T) 

# number of people on excavation ####
# maybe should be using all people count, not just named?

# size_excavation <- 5
# 

# size_excavation <- 5
# # filter/split by size of excavation? this doesn't quite seem to match numbers below, but that includes unnamed
# 
# bn_excavations_people_participants_named_size_large <-
#   bn_excavations_people_participants_named |>
#   filter(excavation_n > size_excavation)
# 
# bn_excavations_people_participants_named_size_small <-
#   bn_excavations_people_participants_named |>
#   filter(excavation_n <= size_excavation)


# you really do need to think about map....

# bn_excavations_people_participants_named_size_03 <-
#   bn_excavations_people_participants_named |>
#   filter(excavation_n <= 3)

bn_excavations_people_participants_named_size_05 <-
  bn_excavations_people_participants_named |>
  filter(excavation_n <=5)

bn_excavations_people_participants_named_size_gt05 <-
  bn_excavations_people_participants_named |>
  filter(excavation_n > 5)


# bn_excavations_people_participants_named_size_10 <-
#   bn_excavations_people_participants_named |>
#   filter(excavation_n <=10)


# bn_excavations_people_participants_named_size_20 <-
#   bn_excavations_people_participants_named |>
#   filter(excavation_n <=20)
# 
# bn_excavations_people_participants_named_size_gt03 <-
#   bn_excavations_people_participants_named |>
#   filter(excavation_n > 3)
# 

# bn_excavations_people_participants_named_size_gt10 <-
#   bn_excavations_people_participants_named |>
#   filter(excavation_n > 10)



## make network/sub-network
set.seed(240710)

network_all <-
  bn_excavations_people_participants_named |>
  bn_excavations_make_network() |>
  bn_centrality() |>
  bn_clusters()

