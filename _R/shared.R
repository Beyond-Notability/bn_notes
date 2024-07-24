## shared libraries, functions etc ####

## BEFORE std_queries.R ##


# date of R session
session_date_ymd <- format(Sys.time(), "%Y%m%d") 

# today's date as a string, for filenames
today_date_ymd <- function(){
  format(Sys.time(), "%Y%m%d") 
}

# libraries ####

library(knitr)
library(kableExtra)

library(readxl) 
library(writexl)

library(janitor)
library(scales)
library(glue)

library(tidytext)

library(tidyverse)

# viz/ggplot extras

library(patchwork)

library(ggthemes)
library(ggalt)

# gg theme slightly modified default
theme_set(theme_gray())
theme_update(panel.background = element_rect(fill="#fafafa"), 
             strip.background = element_rect(fill="#f0f0f0"),
             axis.ticks = element_line(colour = 'grey'))

# other stuff

library(reactable)
#https://glin.github.io/reactable/index.html



# wikidata/sparql etc

#SPARQL package (which used to be the standard go-to) has been removed from CRAN. 
#Could install it anyway or even write a custom function using curl but SPARQLchunks seems to work fine.
#remotes::install_github("aourednik/SPARQLchunks", build_vignettes = TRUE)
library(SPARQLchunks) # can't get chunks working! but it's fine for inline queries.

## ARRRGHHH. 
## run into an unexpected issue with the sparqldf function: 
## some external IDs (eg VIAF) are very long strings of numbers 
## the function insists on converting them to numerical but then R does *stuff* to them
## it should be a rare problem and if so it can be fixed by hacking the sparql query to add a prefix string.
## in where: BIND(concat("idstr:", ?id) as ?id_str ) or in select:  (concat("idstr:", ?id) as ?id_str )
## followed by a str_remove

library(WikidataQueryServiceR) # may be useful 

# keep this here for reference, in case sparqlchunks stops working or whatever
# library(SPARQL)
# useragent <- paste("BN-SH", "(https://beyondnotability.org/)", R.version.string) # for SPARQL. see https://w.wiki/CX6

# ditto, not convinced by this one
# library(glitter)

# for stuff in mediawiki non wikibase pages (also works on wikipedia)
library(WikipediR)



## may also need at some point... 
#library(jsonlite)
#library(listviewer)
## xml2? httr?


## functions (general purpose) ####



## DATES

# since i can never remember how to do this...
make_decade <- function(data, year) {
  data |>
    mutate(decade = {{year}} - ({{year}} %% 10)) |>
    relocate(decade, .after = {{year}})
}

# for single column named date; needs to be in wikibase format. won't work on edtf.
make_date_year <-function(data){
  data  |>
    mutate(date = if_else(str_detect(date, "^_:t"), NA, date))  |>
    mutate(date = parse_date_time(date, "ymdHMS"))  |>
    mutate(year = year(date))
}


# add date property labels inside a mutate (primarily for qualifier dates)
date_property_labels <- function(v) {
  case_when(
    {{v}}=="P1" ~ "point in time",
    {{v}}=="P27" ~ "start time",
    {{v}}=="P28" ~ "end time",
    {{v}}=="P53" ~ "earliest date",
    {{v}}=="P51" ~ "latest date"
  )
}
# but it's probably easier and more generalisable to (left) join to bn_properties, renaming as appropriate.
# eg:
#  left_join(bn_properties |> select(date_qual_prop= bn_prop_id, date_qual_label= propertyLabel), by="date_qual_prop") |>



# turn standard ymdHMS or ymd dates with precision levels into EDTF format. works with posixct dates as well as strings.
# use inside a mutate.
make_edtf_date_from_ymdhms <- 
  function(p=date_precision, d=date){
    case_when(
      {{p}}==9 ~ str_sub({{d}}, 1, 4), # year only
      {{p}}==10 ~ str_sub({{d}}, 1,7), # year-month
      {{p}}==11 ~ str_sub({{d}}, 1, 10) # ymd
    )
  }

# take a posixct date + wikibase numerical date precision and turn into display date
# would be trivial to add parse date step for a ymdHMS date straight from wikibase but usually done that already
make_display_date <- 
  function(data, date=date, date_precision=date_precision){
  # requires lubridate
  data |>
    mutate(m = month({{date}}, label=TRUE, abbr=F), d=day({{date}}), y=year({{date}})) |>
    mutate(display_date = case_when(
      is.na({{date}}) ~ NA,
      {{date_precision}}==11 ~ paste(d, m, y),
      {{date_precision}}==10 ~ paste(m, y),
      {{date_precision}}==9 ~ as.character(y)
    )) 
}

## to get stuff in shared data folder... 

# this might be slightly different in _site project ?
get_folder_one_above_root <- function(ext_folder){
  file.path(dirname(here::here()), ext_folder)  ## nb creates absolute path
}




# TIL how to take a DF (or other object) name and use in a column. eg for bind_rows.
# deparse(substitute(.)) isn't pipable afaict... but it is if you put it in a function
# NB object has to already exist! and may not work as expected if you use sthg like as_tibble before it

obj_name_as_var <- function(d){
  d_name <- deparse(substitute(d))
  d |>
    mutate(src = d_name)
}



##function to read all tabs in a spreadsheet...
# based on stuff in https://readxl.tidyverse.org/articles/articles/readxl-workflows.html

read_bn_excel <- function(sheet, path) {
  pathbase <- path |>
    basename() |>
    tools::file_path_sans_ext()
  path |>
    readxl::read_excel(sheet = sheet) |>
    clean_names(case="snake")
}
# 
read_bn_sheets <- function(path){
  path |>
    readxl::excel_sheets() |>
    set_names()
}


## example usage

# #read in all the sheets
# bn_sheets <-
#   bn_xlsx_path %>%
#   read_bn_sheets() %>%
#   map(read_bn_excel, path=bn_xlsx_path)
# 
# # get sheet1
# bn_sheet1_xlsx <-
#   bn_sheets$sheet1


## PPA buckets
# v1 Feb 2024

bn_ppa_buckets <-
  read_csv(here::here("_data/bn_ppa_buckets_v1_240212.csv"), show_col_types = F)


## moved from std-queries.r
## Sorted Properties (needs WikipediR)
## https://beyond-notability.wikibase.cloud/wiki/MediaWiki:Wikibase-SortedProperties
## this is simpler structure than the queries page and query shouldn't break...
## thing to watch out for is that some properties might be in more than one page section (and not all properties are included!)

# fetch the page
bn_sorted_properties_fetch <-
  page_content(
    domain = "beyond-notability.wikibase.cloud",
    page_name = "MediaWiki:Wikibase-SortedProperties",
    as_wikitext = TRUE
  )

# parse the page content into sections, ids, labels
bn_sorted_properties <-
  bn_sorted_properties_fetch$parse$wikitext$`*` |>
  enframe() |>
  select(-name) |>
  # drop initial ==
  mutate(value=str_remove(value, "^ *==+ *")) |>
  # use start of line == to make new rows
  unnest_regex(section, value, pattern = "\n+ *==", to_lower = F) |>
  # use end of line == to separate heading sfrom text
  separate(section, into = c("section", "text"), sep=" *== *\n+ *") |>
  # include \n here in case there's ever a literal * in the text
  unnest_regex(text, text, pattern = " *\n+\\* *", to_lower = F) |>
  # get rid of the remaining * at start of text
  mutate(text = str_remove(text, "^ *\\* *")) |>
  # extract P id from text
  mutate(bn_prop_id = word(text)) |>
  # extract label in (...) nb that some of the labels contain ()
  mutate(label = str_match(text, "\\((.+)\\) *$")[,2]) |>
  mutate(across(c(section, label), str_trim)) |>
  relocate(bn_prop_id, label)



# ## Wikibase example queries (needs WikipediR) ####
# ## not in use - a big frequently changing page and turned out to be safer to C&P queries as needed; they often need adjusting anyway
# ## but keep the code for reference.

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






## labels and display ####

# for abbreviating names of the three main societies in labels (use in str_replace_all)
sal_rai_cas_abbr <-
  c("Society of Antiquaries of London"="SAL", "Royal Archaeological Institute"="RAI", "Congress of Archaeological Societies"="CAS")

##(make sure you do this after sal_rai_cas_abbr if you're using that)
organisations_abbr <-
  c("Archaeological" = "Arch", "Antiquarian" = "Antiq", "Society" = "Soc", "Association" = "Assoc")


# see dates_timelines_231114 rmd
# function to make a named vector of colours for variable values
# df needs cols: group, colour_category
# get rows in required order before using this (this sorts by frequency; could make another version for random/uncounted etc) and then add rowid
# grpname is the name of the group or bucket in the group col
# **grpcols has to be a vector of colour codes** - sthg like viridis_pal()(16)  will work, just have to have enough colours. may need to reverse direction to stop the first one being white grr.
make_named_colours <- function(df, grpname, grpcols){
  df |>
    filter(group==grpname) |>
    count(colour_category, sort = TRUE) |>
    rowid_to_column() |>
    left_join(grpcols |> enframe(), by=c("rowid"="name")) |>
    fill(value) |>
    select(colour_category, value) |>
    # turn the df into a named vector with deframe and this can be used in scale_color_manual
    deframe()
}