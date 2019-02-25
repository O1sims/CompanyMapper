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


generateCompanyMapAnimation <- function(firmData) {
  uniqueYears <- firmData$year %>% 
    unique() %>% 
    sort() %>%
    as.integer()
  
  for (i in 1:length(uniqueYears)) {
    NIFirmsYear <- subset(
      firmData, 
      year <= uniqueYears[i])
    
    companiesEstablised <- ggplot(data = NIFirmsYear) + 
      geom_point(aes(x = long, y = lat, colour = SICCode.SicText_1)) + 
      labs(title = "Map of NI firms established by " %>% paste0(uniqueYears[i])) +
      coord_cartesian(
        xlim = c(-8.25, -5),
        ylim = c(54, 55.5)) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.ticks = element_blank())
    
    ggsave(
      filename = getwd() %>% paste0("/coordinate-mapping/animation/", uniqueYears[i], "-company-map.png"), 
      plot = companiesEstablised, 
      device = "png")  
  }
}

