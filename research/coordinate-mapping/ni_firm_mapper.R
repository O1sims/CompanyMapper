library(ggmap)
library(ggplot2)
library(ggthemes)
library(jsonlite)
library(magrittr)
library(maptools)
library(gganimate)


generate_shapefile_map <- function() {
  spdf <- getwd() %>% 
    paste0("/coordinate-mapping/data/map/parliamentaries/NI-parliamentary-boundaries.shp") %>%
    maptools::readShapePoly()
  
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- ggplot2::fortify(spdf, region = "id")
  heatmap.data <- dplyr::inner_join(
    spdf.points, 
    spdf@data, 
    by = "id")
  
  heatmap.data$COUNTYNAME %<>% 
    as.vector()
  
  shapefileMap <- ggplot(
    data = heatmap.data) + 
    geom_polygon(
      colour = "white",
      fill = "grey",
      size = 0.5, 
      aes(
        x = long, 
        y = lat, 
        group = group))
  
  return(shapefileMap)
}


generate_company_map_years <- function(firmData, fromYear, sectionOnly = FALSE, techSector = FALSE, legend = TRUE) {
  if (techSector) {
    firmData %<>% 
      subset(`5DIGITSIC` %in% TECHNOLOGY_SIC_CODES)
    sectorName <- "technology"
  } else {
    sectorName <- "all"
  }
  
  if (missing(fromYear)) {
    uniqueYears <- seq(min(firmData$year), max(firmData$year))
  } else {
    uniqueYears <- seq(fromYear, max(firmData$year))
  }
  
  shapefileMap <- generate_shapefile_map()
  
  sectorColours <- setNames(
    c("#9C27B0", "#E91E63", "#F44336", "#2196F3", "#3F51B5", "#673AB7", 
      "#009688", "#00BCD4", "#03A9F4", "#CDDC39", "#8BC34A", "#4CAF50", 
      "#FF9800", "#FFC107", "#FFEB3B", "#9E9E9E", "#795548", "#FF5722", 
      "#607D8B", "#880E4F", "#1565C0", "#388E3C"), 
    c(sort(unique(NIFirms$section)), NA))
  
  for (i in 1:length(uniqueYears)) {
    NIFirmsYear <- firmData %>% 
      subset(year <= uniqueYears[i]) %>%
      subset(!is.na(section))
    
    if (sectionOnly) {
      `SIC Sector` <- NIFirmsYear$section
    } else {
      `SIC Sector` <- NIFirmsYear$`5DIGITSIC`
    }
    
    "Generating map for " %>%
    paste0(uniqueYears[i]) %>% 
      print()
    
    companiesEstablised <- shapefileMap +
      geom_point(
        data = NIFirmsYear, 
        aes(
          x = long, 
          y = lat,
          colour = section),
        alpha = 0.6) + 
      labs(
        color = "Industry sector code",
        title = paste0("All NI ", ifelse(techSector, sectorName, "")," firms registered with Companies House: ", uniqueYears[i])) +
      ylab("") + xlab("") +
      coord_cartesian(
        xlim = c(-8.25, -5),
        ylim = c(54, 55.5)) +
      theme_minimal() +
      scale_color_manual(values = sectorColours) +
      theme(
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.position = ifelse(legend, "right", "none"))
    
    ggsave(
      filename = getwd() %>% 
        paste0("/coordinate-mapping/animation/", sectorName, "-sector/", uniqueYears[i], "-company-map.png"), 
      plot = companiesEstablised,
      device = "png", 
      width = 8, 
      height = 5, 
      units = "in")
   }
}


animate_firm_establishment <- function(firmData, techSector = FALSE) {
  if (techSector) firmData %<>% 
    subset(`5DIGITSIC` %in% TECHNOLOGY_SIC_CODES)
  
  uniqueYears <- firmData$year %>% 
    unique() %>% 
    sort() %>%
    as.integer()
  
  shapefileMap <- generate_shapefile_map()
  
  companiesAnimation <- shapefileMap + 
    geom_point(
      data = firmData, 
      aes(
        x = long, 
        y = lat,
        colour = `5DIGITSIC`)) + 
    labs(title = "NI tech firms established from 1984 to 2018") +
    ylab("") + xlab("") +
    coord_cartesian(
      xlim = c(-8.25, -5),
      ylim = c(54, 55.5)) +
    transition_reveal(year) +
    theme_minimal() +
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank())
  
  anim_save(
    filename = getwd() %>% 
      paste0("/coordinate-mapping/animation/technology-sector/company-establishment.gif"), 
    animation = companiesAnimation %>% 
      animate())
}


getwd() %>% 
  paste0("/data/NIFirms.rda") %>% 
  load()

TECHNOLOGY_SIC_CODES <- c(
  58210, 58290, 61100, 61200, 61300, 61900, 62011, 
  62012, 62020, 62030, 62090, 63110, 63120, 71200)

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

generate_company_map_years(
  firmData = NIFirms,
  sectionOnly = TRUE)

