library(ggplot2)
library(jsonlite)
library(magrittr)


TECHNOLOGY_SIC_CODES <- c(
  58210, 58290, 61100, 61200, 61300, 61900, 62011, 
  62012, 62020, 62030, 62090, 63110, 63120, 71200)


aggregate_yearly_numbers <- function(NIFirms, fromYear, techSector = FALSE) {
  if (techSector) NIFirms %<>% 
    subset(`5DIGITSIC` %in% TECHNOLOGY_SIC_CODES)
  
  firmNumbers <- c()
  
  uniqueYears <- NIFirms$year %>% 
    unique()
  
  minYear <- ifelse(
    test = missing(fromYear), 
    yes = min(uniqueYears),
    no = fromYear)
  
  allYears <- seq(
    minYear, 
    max(uniqueYears))
  
  for (y in allYears) {
    firmNumbers %<>% append(
      NIFirms %>% 
        subset(year <= y) %>%
        nrow())
  }
  
  data.frame(
    year = allYears,
    number = firmNumbers,
    stringsAsFactors = FALSE) %>%
    ggplot() +
    geom_line(
      aes(
        x = year,
        y = number,
        colour = "red")) +
    geom_point(
      aes(
        x = year,
        y = number,
        colour = "red")) +
    geom_area(
      aes(
        x = year,
        y = number,
        fill = "red"),
      alpha = 0.3) +
    ylab(label = "Number of technology firms") +
    xlab(label = "Year") +
    theme_minimal() +
    theme(legend.position = "none")
}


change_in_yearly_firms <- function(NIFirms, fromYear, techSector = FALSE) {
  if (techSector) NIFirms %<>% 
    subset(`5DIGITSIC` %in% TECHNOLOGY_SIC_CODES)
  
  changeIn <- c()
  
  uniqueYears <- NIFirms$year %>% 
    unique()
  
  minYear <- ifelse(
    test = missing(fromYear), 
    yes = min(uniqueYears),
    no = fromYear)
  
  allYears <- seq(
    minYear, 
    max(uniqueYears))
  
  for (y in allYears) {
    changeIn %<>% append(
      NIFirms %>% 
        subset(year == y) %>%
        nrow())
  }
  
  data.frame(
    year = allYears,
    change = changeIn,
    stringsAsFactors = FALSE) %>%
    ggplot() +
    geom_line(
      aes(
        x = year,
        y = change)) +
    geom_point(
      aes(
        x = year,
        y = change)) +
    ylab(label = "Number of firms") +
    xlab(label = "Year") +
    theme_minimal()
}


NIFirmsJSON <- jsonlite::read_json(
  path = getwd() %>% 
    paste0("/../src/data/northern-ireland/allNIFirms.json"))

NIFirms <- jsonlite::fromJSON(
  txt = NIFirmsJSON[[1]][1])

# Only considered the firms that are based on the island
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

aggregate_yearly_numbers(
  NIFirms = NIFirms,
  techSector = TRUE)

change_in_yearly_firms(
  NIFirms = NIFirms,
  fromYear = 2000,
  techSector = TRUE)
