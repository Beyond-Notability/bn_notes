## shared libraries, functions etc ####

## BEFORE std_queries.R ##


today_date_ymd <- format(Sys.time(), "%Y%m%d") 


## BEFORE std_queries.R ##

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




## labels and display ####

# for abbreviating names of the three main societies in labels (use in str_replace_all)
sal_rai_cas_abbr <-
  c("Society of Antiquaries of London"="SAL", "Royal Archaeological Institute"="RAI", "Congress of Archaeological Societies"="CAS")