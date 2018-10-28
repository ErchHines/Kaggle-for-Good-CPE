library(tidycensus)
library(tidyverse)
library(censusapi)
library(dplyr)
library(sf)
library(data.table)
library(rgdal)
library(tmap)
library(ggmap)
library(googleway)

#Read in our police district data
police_districts <- readOGR(
  dsn = "data/Dept_11-00091/11-00091_Shapefiles", 
  layer = "boston_police_districts_f55"
)

#Read in our incidents file, skipping the first row where it appears the headers
#are mostly just slightly reworded
police_incidents <- read.csv(
  "data/Dept_11-00091/11-00091_Field-Interviews_2011-2015.csv", 
  skip = 1, header = TRUE
)

#Get a list of our metadata 
v16 <- load_variables(2016, "acs5", cache = TRUE)

#Pull in acs data using tidycensus
acs_edu <- get_acs("tract", 
                   table = "B15003", 
                   cache_table = TRUE,
                   geometry = TRUE, 
                   survey = "acs5", 
                   state = "MA", county = "Suffolk",
                   year = 2016, output = "wide")

#Remove all the margin of error columns
acs_edu <- acs_edu[, -grep("\\M$", colnames(acs_edu))]

#Group education into mappable categories
acs_edu <- acs_edu %>%
  mutate(less_than_high_school = rowSums(acs_edu[,4:18,drop = TRUE]),      #Nursery school through 12th grade unfished
         high_school_diploma = acs_edu[,19,drop=TRUE],                     #High school diploma only
         GED = acs_edu[,20,drop=TRUE],                                     #Ged only
         some_college_associates = rowSums(acs_edu[,21:23,drop = TRUE]),   #Some college +- 1 year and associate's degrees
         bachelors = acs_edu[,24,drop=TRUE],                               #bachelor's only
         post_graduate = rowSums(acs_edu[,25:27,drop = TRUE])) %>%         #Master's doctorate or professional
  select(-(3:27))

random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

num_dots <- as.data.frame(acs_edu) %>% 
  select(less_than_high_school:post_graduate) %>% 
  mutate_all(funs(. / 25)) %>% 
  mutate_all(random_round)

census_dots <- map_df(names(num_dots), 
                  ~ st_sample(acs_edu, size = num_dots[,.x], type = "random") %>% # generate the points in each polygon
                    st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                    st_coordinates() %>%                                          # pull out coordinates into a matrix
                    as_tibble() %>%                                               # convert to tibble
                    setNames(c("lon","lat")) %>%                                  # set column names
                    mutate(education = .x)                                        # add categorical variable
) %>% 
  slice(sample(1:n()))

#Convert dots to spdf and reproject to epsg:4268
CRS_new <- CRS("+init=epsg:4326") 

census_dots_tm <- SpatialPointsDataFrame(
  coords = census_dots[,c("lon","lat")], data = census_dots)
proj4string(census_dots_tm) <- "+proj=longlat +datum=NAD83 +no_defs"
raster::projection(census_dots_tm)="+init=epsg:4269"
census_dots_tm <- spTransform(census_dots_tm, CRS_new)

#Our police distrists need to be converted as well
raster::projection(police_districts)="+init=epsg:2249"
police_districts <- spTransform(police_districts, CRS_new)

#Make the map
tmap_mode("view")

# tm_shape(acs_edu) +
# tm_polygons(alpha = 0.1) +
# tm_shape(police_districts) + 
# tm_polygons( col = "blue", alpha = 0.5, lwd = 3) +
# tm_shape(census_dots_tm) + 
# tm_dots("education", alpha = 0.8) +
# tm_layout(basemaps = c('OpenStreetMap'))

#Let's turn our police incidents into point data with a lat and long

#Concenate strings from address and city
police_incidents$address_city <- ifelse(
  police_incidents$CITY == "NO DATA ENTERED" | police_incidents$CITY == "OTHER",
  paste(police_incidents$LOCATION, ", Boston, MA", sep = ""), 
  paste(police_incidents$LOCATION, ", ", police_incidents$CITY, ", MA", sep ="")
)

#Let's start by working with homicides as a more manageable subset
homicides <- police_incidents[police_incidents$FIOFS_REASONS == "HOMICIDE",]

geocoded <- data.frame(stringsAsFactors = FALSE)

for(i in 1:nrow(homicides))
{
  result <- google_geocode(address = homicides$address_city[i], key = key, simplify = TRUE)
  homicides$lon[i] <- as.numeric(result$results$geometry$location$lng[1])
  homicides$lat[i] <- as.numeric(result$results$geometry$location$lat[1])
}

homicides_tm <- SpatialPointsDataFrame(
  coords = homicides[,c("lon","lat")], data = homicides)

tm_shape(acs_edu) +
  tm_polygons(alpha = 0.1) +
tm_shape(police_districts) + 
  tm_polygons( col = "blue", alpha = 0.3, lwd = 5) +
tm_shape(census_dots_tm) + 
  tm_dots("education", alpha = 0.9,
          scale = 0.8, legend.size.is.portrait = TRUE) +
tm_shape(homicides_tm) +
  tm_bubbles(col = "red", size = 0.18)  +
tm_layout(basemaps = c('OpenStreetMap'))

key <- "AIzaSyC6zvBKePDKrGn4_5dxOdN69_-ds9YdVZk"

save.image("CPE.Rdata")

