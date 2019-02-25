library(magrittr)

companyName <- "kainos"

ni_companies$CompanyName[
  grep(
    pattern = companyName, 
    x = ni_companies$CompanyName, 
    ignore.case = TRUE)]

allNIFirms <- ni_companies
allUKFirms <- All_UK_Firms_Geocoded_Jan_19

save(allUKFirms, file = "~/Code/UKFirms/data/allUKFirms.rda")

load(file = "~/Code/UKFirms/data/northern-ireland/allNIFirms.rda")

load("~/Code/CompanyMapper/src/data/northern-ireland/allNIFirms.rda")

ni.firms.json <- jsonlite::toJSON(allNIFirms, auto_unbox = TRUE)

write_json(
  ni.firms.json, 
  path = "~/Code/CompanyMapper/src/data/northern-ireland/allNIFirms.json")

irelandMap <- ggmap::get_map(
  location = c(-7.739754, 53.521773), 
  zoom = 7,
  maptype = "roadmap") %>% 
  ggmap::ggmap()

subscr <- data.frame(
  lat = property.data$details$latitude,
  lon = property.data$details$longitude,
  agentType = property.data$details$sellerType,
  propertyType = property.data$details$propertyType,
  stringsAsFactors = FALSE)

subscr <- subscr[complete.cases(subscr), ]
