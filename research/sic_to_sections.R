## Assign SIC codes to Sections

library(readr)
library(jsonlite)
library(magrittr)


NIFirmsJSON <- getwd() %>% 
  paste0("/../src/data/northern-ireland/allNIFirms.json") %>% 
  jsonlite::read_json()

NIFirms <- NIFirmsJSON[[1]][1] %>%
  jsonlite::fromJSON()

NIFirms$`5DIGITSIC` %<>% 
  as.integer()

UKSIC2007Condensed <- "data/uk-sic-2007-condensed.csv" %>%
  readr::read_csv()

section <- sicDescription <- sectionDescription <- c()
for (i in 1:(NIFirms %>% nrow())) {
  if (NIFirms$`5DIGITSIC`[i] %>% is.na()) {
    section %<>% 
      append(NA)
    sicDescription %<>% 
      append(NA)
    sectionDescription %<>% 
      append(NA)
  } else {
    r <- NIFirms$`5DIGITSIC`[i] %>% 
      match(UKSIC2007Condensed$sic_code)
    section %<>% 
      append(UKSIC2007Condensed$section[r])
    sicDescription %<>% 
      append(UKSIC2007Condensed$sic_description[r])
    sectionDescription %<>% 
      append(UKSIC2007Condensed$section_description[r])
  }
}

NIFirms$section <- section
NIFirms$sicDescription <- sicDescription
NIFirms$sectionDescription <- sectionDescription

NIFirms %>% 
  save(file = getwd() %>% 
         paste0("/data/NIFirms.rda"))
