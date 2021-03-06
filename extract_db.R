library(Hmisc)
library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)
library(rgdal)
libary(sjlabelled)

# Spatial
CA <- rgdal::readOGR(dsn=here::here("data"), layer = "outline")
tr <- rgdal::readOGR(dsn=here::here("data"), layer = "tracks")

# Database
db <- here::here("data", "CPW.accdb")
mdbget <- . %>%
  Hmisc::mdb.get(db, dateformat = '%d/%m/%y %H:%M:%S', allow=c("_"), as.is=T, table = .) %>%
  tibble::as_tibble(.) %>% 
  sjlabelled::remove_all_labels()

# Database tables of interest
camloc <- mdbget("CameraLocations")
species <- mdbget("Species")
detections <- mdbget("Detections")
photos <- mdbget("Photos")
visits <- mdbget("Visits")

# camlocs is labelled, visits is not
# Hmisc::label(visits$LocationID) <- Hmisc::label(camloc$LocationID)

number_locations <- length(unique(camloc$LocationID))

# Data munging
five1 <- visits %>%
  dplyr::left_join(camloc, by = "LocationID") %>%
  dplyr::right_join(photos, by = "VisitID") %>%
  dplyr::left_join(detections, by = "ImageID") %>%
  dplyr::left_join(species, by="SpeciesID") %>%
  dplyr::select("SpeciesID", "ImageDate", "LocationID", "LocationName",
                "UTM_E", "UTM_N", "CommonName", "Genus", "Species")

# There's something wrong with ImageDate, so appending:
# image_date = lubridate::ymd_hms(ImageDate, timezone = "Australia/Perth")
# results in Error: Column `image_date` must be length 410433 (the number of rows) or one, not 410434

# Instead, we'll parse the date the old way:
five1$ImageDate <- as.POSIXct(five1$ImageDate, format = "%d/%m/%Y %H:%M", tz = "")

five <- five1 %>% 
  dplyr::mutate(
    # image_date = lubridate::ymd_hms(ImageDate, timezone = "Australia/Perth"),
    date = floor_date(ImageDate,"day") %>% as.Date,
    year = lubridate::year(ImageDate),
    month = lubridate::month(ImageDate),
    day = lubridate::day(ImageDate),
    hour = lubridate::hour(ImageDate),
    count = ifelse(CommonName == "None", 0, 1)
  ) 

# Replaces: 
# one <- merge(camloc, visits, by = "LocationID")
# two <- merge(one, photos, by = "VisitID")
# three <- merge(two, detections, by = "ImageID")
# four <- merge(three, species, by = "SpeciesID")
# five <- subset(four, select = c("SpeciesID", "ImageDate", "LocationID", "LocationName",
#                                 "UTM_E", "UTM_N", "CommonName", "Genus", "Species"))
# 
# five$ImageDate <- as.POSIXct(five$ImageDate, format = "%d/%m/%Y %H:%M", tz = "") ##### creates a column called "time" in a particular format from column "Imagedate"
# five$date <- format(five$ImageDate, "%d/%m/%Y")
# five$date <- as.Date(five$date, "%d/%m/%Y") #### format date to actual date
# five$year <- as.numeric(format(five$ImageDate, "%Y"))
# five$month <- as.numeric(format(five$ImageDate, "%m"))
# five$day <- as.numeric(format(five$ImageDate, "%d"))
# five$hour <- as.numeric(format(five$ImageDate, "%H"))
# five$count <- 1 #### add 1 to count for subsequent analysis
# five$count[five$count == 1 & five$CommonName == "None"] <- 0 #### replace count with 0 when commonname is none

save(CA, tr, five, number_locations, 
     camloc, species,
     file=here::here("data", "defaults.RData"))
