x <- c("odbc","DBI","RODBC", "here","dbplyr",  "readr", "data.table", "reshape2","qcc", "Rmisc",
       "ggplot2","dplyr","sp","ggmap","rgeos", "tidyr","gstat","deldir","dismo","rgdal","fitdistrplus","logspline","DT","maptools")
lapply(x, library, character.only = TRUE)

###myconn<-odbcConnectAccess2007("CPW.accdb")
cs <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=data/CPW.accdb"
myconn <- dbConnect(odbc::odbc(), .connection_string = cs)
# db <- here::here("data", "CPW.accdb")
# mdbget <- . %>% Hmisc::mdb.get(db, dateformat = '%Y-%m-%d', as.is = T, table = .)
# sometable <- mdbget("sometable")

# detections <- sqlFetch(myconn, 'detections')
# species <- sqlFetch(myconn, 'species')
# photos <- sqlFetch(myconn, 'photos')
#visits <- sqlFetch(myconn, 'visits')
#CameraLocations <- sqlFetch(myconn, 'CameraLocations')	
detections <- dbReadTable(myconn, 'detections')
species <- dbReadTable(myconn, 'species')
photos <- dbReadTable(myconn, 'photos')
visits <- dbReadTable(myconn, 'visits')
CameraLocations <- dbReadTable(myconn, 'CameraLocations')
# close(myconn)
dbDisconnect(myconn)

CA <- readOGR(dsn=here::here("data"), layer = "outline")
tr <- readOGR(dsn=here::here("data"), layer = "tracks")

save(
  detections, species, photos, visits, CameraLocations, CA, tr, 
  file=here::here("data", "defaults.RData")
)
  
)