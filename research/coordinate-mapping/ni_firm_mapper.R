library(ggplot2)
library(ggthemes)
library(jsonlite)
library(magrittr)


NIFirmsJSON <- jsonlite::read_json(
  path = getwd() %>% 
    paste0("/../src/data/northern-ireland/allNIFirms.json"))

NIFirms <- jsonlite::fromJSON(
  txt = NIFirmsJSON[[1]][1])

NIFirms %<>% 
  subset(lat < 60) %>% 
  subset(long < -4) 

NIFirms$year <- strsplit(
  x = NIFirms$`Incorporation Date (Formatted)`, 
  split = "-", 
  fixed = TRUE) %>% 
  purrr::flatten_chr() %>%
  subset(nchar(.) == 4) %>%
  as.integer()

uniqueYears <- NIFirms$year %>% 
  unique() %>% 
  sort()

for (year in NIFirms$year %>% unique() %>% sort()) {
  NIFirmsYear <- subset(
    NIFirms, 
    year <= year)
  
  ggplot(data = NIFirmsYear) + 
    geom_point(aes(x = long, y = lat, colour = SICCode.SicText_1)) + 
    labs(title = "Map of NI firms established by " %>% paste0(year)) +
    theme_minimal() + 
    theme(
      legend.position = "none",
      axis.ticks = element_blank())
  
}


