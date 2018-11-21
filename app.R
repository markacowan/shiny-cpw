# install.packages(c("odbc","DBI","RODBC", "here", "readr", "data.table", "reshape2","dplyr",
# "qcc","Rmisc","ggplot2", "dbplyr", "sp","ggmap","rgeos", "tidyr","gstat","deldir","dismo","rgdal",
# "fitdistrplus","logspline","DT","maptools"))

x <- c(
  "here", "tidyr", "reshape2", "data.table", "dplyr", "readr", "ggmap", "ggplot2", "DT", # tidyverse
  # "Rmisc", 
  # "deldir", # tools
  "sp", "rgeos", "rgdal", "maptools", # spatial
  "qcc", "fitdistrplus", "logspline", "gstat", "dismo" # analysis
)
lapply(x, library, character.only = TRUE)

ui <- fluidPage(
  title="CPW explorer",
  h1(id = "title", "CPW camera trapping data explorer"),
  h3("Author: M.A. Cowan, DBCA"),
  # tags$style(HTML("#title{color: blue;font-size: 15px;}")),

  sidebarLayout(
    sidebarPanel(
      dateRangeInput("daterange1", "Date Range:", start = "2015-03-01", end = "2018-09-07"),
      sliderInput("slider1", label = ("Camera Location #'s"), min = 1, max = 100, value = c(1, 60)),
      sliderInput("slider2", label = ("Resolution for map grid (m)"), min = 10, max = 500, value = 50),
      sliderInput("intInput", label = ("Survey interval in days (default=months)"), min = 0, max = 60, value = 0),
      ## tags$style("#goButton {background-color:green;}"),
      actionButton("goButton", strong("Run"), width = 120, height = 80), br(),
      numericInput("speciesInput", "Display species", 1, min = 1, max = 30, width = 120),
      actionButton("goButton1", strong("Save occupancy data"), width = 240),
      actionButton("goButton2", strong("Save data for all Species"), width = 240),
      actionButton("goButton3", strong("Save data for selected Species"), width = 240),

      checkboxGroupInput(
        "variable",
        "Species to analyse:",
        c(
          "Brush Wallaby",
          "Cat",
          "Chuditch",
          "Echidna",
          "Euro",
          "Fox",
          "Grey Kangaroo",
          "Numbat",
          "Possum",
          "Quenda",
          "Rabbit",
          "Red-tailed Phascogale",
          "Tammar",
          "Woylie"
        )
      )
    ),

    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel("How it works", includeMarkdown("README.md")),
      tabPanel("Detection Plots", fluidRow(plotOutput("detection"), br(), br(), plotOutput("cusumplot"))),
      tabPanel("IDW Interpolation", plotOutput("IDW_Interpolation", height = 800)),
      tabPanel("NN Interpolation", plotOutput("NN_Interpolation", height = 800)),
      tabPanel("Bubble Plot", plotOutput("Bubble_Plot", height = 600)),
      tabPanel("Activity", plotOutput("Activity", height = 600)),
      tabPanel("Statistical Distribution", fluidRow(
        plotOutput("cull_frey", height = 400),
        plotOutput("norm_dist", height = 400)
      )),
      tabPanel("Summary Data Table", DTOutput("Table"))
    ))
  )
) #### outer fluidPage

server <- function(input, output) {

  # TODO https://github.com/markacowan/shiny-cpw/issues/1
  load(here::here("data", "defaults.RData"))

  observeEvent(input$goButton, {
    ####### Min_Date <- as.Date(c("29/09/2016"),"%d/%m/%Y") ###### date selection for minimum date
    ###### Max_Date <- as.Date(c("3/07/2018"),"%d/%m/%Y") ###### date selection for maximum date
    dr <- input$daterange1
    Min_Date <- dr[1] ###### date selection for minimum date
    Max_Date <- dr[2] ###### date selection for maximum date
    ###### cam_loc <- c(23:42) ##### a vector which is used to select LocationID values eg (1:30,35,40,45:50)
    # val <- input$slider1
    cam_loc <- c(input$slider1[1]:input$slider1[2])
    spec_a <- input$variable
    interval <- input$intInput
    rast <- input$slider2 ###### resolution of raster in meters

    five <- five %>%
      dplyr::filter(LocationID %in% cam_loc) %>%
      dplyr::filter(date >= Min_Date) %>%
      dplyr::filter(date <= Max_Date)
    # five <- five[five$LocationID %in% cam_loc, ] ##### only records that have the same LocationID as the values in vector "cam_loc" are kept
    # five <- five[five$date >= Min_Date, ] ##### only records that exceed minimum date are kept
    # five <- five[five$date <= Max_Date, ] ##### only records that are less than maximum date are kept

    spec_b <- as.character(unique(five$CommonName)) #### creates unique species list from matrix "five"
    spec_c <- spec_a[spec_a %in% spec_b] #### finds matching species between spec_a and spec_b and stores them as a vector in spec_c
    spec_d <- paste(spec_c, "count", sep = " ", collapse = NULL) ###### species list with count appended
    spec_e <- paste(spec_c, "month", sep = " ", collapse = NULL) ###### species list with month appended
    spec_f <- gsub(" ", "_", gsub("-", " ", sort(c(spec_d, spec_e)))) #### combines vectors spec_b and spec_c, sorts them and then removes spaces and hyphens and replaces them with underscores

    if (interval > 0) {
      spec_c <- c("date", "idate", "yearmonth", "year", "month", spec_c) #### combines original contents of c with date, yearmonth etc......
    } else {
      spec_c <- c("date", "yearmonth", "year", "month", spec_c) #### combines original contents of c with date, yearmonth etc......
    }
    spec_f <- c("LocationID", "UTM_E", "UTM_N", spec_f) #### combines original contents of f with LocationID, UTM_E etc......

    # number_locations <- length(camloc$LocationID) ##### number of distinct locations - from preprocessing
    date_range <- seq(Min_Date, Max_Date, "days") ###### range of dates
    number_dates <- length(date_range) ###### number of dates
    date_entire <- rep(date_range, number_locations) #### dates repeated for every location
    location_entire <- sort(rep(1:number_locations, number_dates)) ##### vector of locations repeated by number of dates and sorted
    LocationID <- as.data.frame(location_entire) #### converts vector to dataframe
    date <- as.data.frame(date_entire) ##### converts vector to dataframe
    colnames(LocationID)[colnames(LocationID) == "location_entire"] <- "LocationID" ##### changes the name of the the field from "location_entire" to "LocationID"
    colnames(date)[colnames(date) == "date_entire"] <- "date" ##### changes the name of the the field from "date_entire" to "date"
    locationbydate <- cbind(date, LocationID) ###### combines date and LocationID into single dataframe called locationbydate
    missing_date_location <- anti_join(locationbydate, five, by = c("date", "LocationID"))
    date_missing <- five[0, ] ##### creates an empty dataframe called "date_missing" based on the fields from dataframe "five"
    date_missing <- plyr::rbind.fill(date_missing, missing_date_location) ##### combines date_missing and date_range
    date_missing$count[is.na(date_missing$count)] <- 0 #### changes NA value in count to 0
    date_missing$UTM_E <- as.numeric(five[match(date_missing$LocationID, five$LocationID), 5])
    date_missing$UTM_N <- as.numeric(five[match(date_missing$LocationID, five$LocationID), 6])
    date_missing$CommonName <- as.factor("None")
    date_missing$day <- lubridate::day(date_missing$date)
    date_missing$month <- lubridate::month(date_missing$date)
    date_missing$year <- lubridate::year(date_missing$date)
    total <- rbind(five, date_missing) ###### combines original datafile with all missing dates

    daily <- unique(subset(total, select = c("SpeciesID", "LocationID", "UTM_E", "UTM_N", "CommonName", "Genus", "Species", "date", "year", "month", "day", "count"))) ##### raw daily detections
    daily$yearmonth <- do.call(paste, c(daily[c("year", "month")], sep = "-"))

    daily <- total %>%
      dplyr::select("SpeciesID", "LocationID", "UTM_E", "UTM_N", "CommonName",
                    "Genus", "Species", "date", "year", "month", "day", "count") %>%
      dplyr::mutate(yearmonth = glue::glue("{year}-{month}")) %>%
      unique

    if (interval > 0) {
      total$idate <- cut(total$date, breaks = paste(interval, "day")) #### creates field called idate which is grouped date based on variable interval
      total$idate <- as.Date(as.character(total$idate), format = "%Y-%m-%d") ###### changes  idate to date format
      daily$idate <- cut(daily$date, breaks = paste(interval, "day")) #### creates field called idate which is grouped date based on variable interval
      daily$idate <- as.Date(as.character(daily$idate), format = "%Y-%m-%d") ###### changes  idate to date format
    }

    matrix2 <- dcast(daily, date ~ CommonName, fun.aggregate = sum, na.rm = TRUE, value.var = "count")
    matrix2$date <- as.Date(matrix2$date, "%d/%m/%Y")
    matrix2$month <- format(matrix2$date, "%m")
    matrix2$year <- format(matrix2$date, "%Y")
    matrix2$yearmonth <- do.call(paste, c(matrix2[c("year", "month")], sep = "-"))
    ###############################################################################################################################

    ###############################################################################################################################
    if (interval > 0) {
      matrix2$idate <- cut(matrix2$date, breaks = paste(interval, "day"))
      matrix2$idate <- as.Date(as.character(matrix2$idate), format = "%Y-%m-%d")

      ###############################################################################################################################

      #############################################################################################################################
    }

    matrix3 <- matrix2[, spec_c] ##### uses vector "spec_c" to select a species subset from matrix 2 and store it in matrix 3

    locations <- unique(subset(camloc, select = c("LocationID", "UTM_E", "UTM_N"))) ###### creates dataframe locations with unique location and UTM coordinates
    commonname <- unique(subset(species, select = c("CommonName", "Genus", "Species", "SpeciesID"))) ####### creates dataframe commonname with unique taxonomic information

    if (interval > 0) {
      int <- nrow(unique(subset(total, select = c("idate")))) + 1 ####### stores the total number of intervals in the survey period
    } else {
      months <- nrow(unique(subset(total, select = c("year", "month")))) + 1 ####### stores the total number of months in the survey period
    }

    iterations <- nrow(commonname) ######## stores the number of unique names in variable iterations
    iterations2 <- length(spec_a)


    test2 <- NULL

    ##########################################################################################################################################################################

    #########################################################################################################################################################################
    if (interval > 0) {
      for (i in 1:iterations) ####### sets up a loop based on number of unique names
      {
        nameused <- paste(as.character(commonname$CommonName[i])) ####### takes the ith commonname and stores it in name used
        nameused1 <- paste(as.character(commonname$CommonName[i]), "count", sep = "_") ###### adds count to the ith commonname and stores it in namused1
        nameused2 <- paste(as.character(commonname$CommonName[i]), "month", sep = "_") ###### adds month to the ith commonname and stores it in namused2
        eight <- daily[grep(paste(nameused), daily$CommonName), ]

        test <- nrow(eight)

        if (test < 3) { ##### this and next 2 lines check for species with less than three detections
          test2 <- append(test2, nameused1)
          test2 <- append(test2, nameused2)
        }



        if (test > 2) {
          reduce <- int - nrow(unique(subset(eight, select = c("idate")))) - 1
          months2 <- int - reduce
          if (months2 > 2) {
            matrix5 <- dcast(eight, LocationID ~ idate, fun.aggregate = sum, na.rm = TRUE, value.var = "count") ########## creates a matrix of locations by month for each species
            matrix5[, paste(nameused1)] <- rowSums(matrix5[, c(2:months2)])
            matrix5[2:months2] <- lapply(matrix5[2:months2], function(x) ifelse(x > 0, 1, x))
            matrix5[, paste(nameused2)] <- rowSums(matrix5[, c(2:months2)])

            locations[, paste(nameused1)] <- as.numeric(matrix5[match(locations$LocationID, matrix5$LocationID), c(paste(nameused1))])
            locations[, paste(nameused2)] <- as.numeric(matrix5[match(locations$LocationID, matrix5$LocationID), c(paste(nameused2))])
          }
        }
      } ##### outer loop line 133
    } else {
      for (i in 1:iterations) ####### sets up a loop based on number of unique names
      {
        nameused <- paste(as.character(commonname$CommonName[i])) ####### takes the ith commonname and stores it in name used
        nameused1 <- paste(as.character(commonname$CommonName[i]), "count", sep = "_") ###### adds count to the ith commonname and stores it in namused1
        nameused2 <- paste(as.character(commonname$CommonName[i]), "month", sep = "_") ###### adds month to the ith commonname and stores it in namused2
        eight <- daily[grep(paste(nameused), daily$CommonName), ]

        test <- nrow(eight)
        if (test < 3) { ##### this and next 2 lines check for species with less than three detections
          test2 <- append(test2, nameused1)
          test2 <- append(test2, nameused2)
        }
        if (test > 2) {
          reduce <- months - nrow(unique(subset(eight, select = c("yearmonth")))) - 1
          months2 <- months - reduce
          if (months2 > 2) {
            matrix5 <- dcast(eight, LocationID ~ yearmonth, fun.aggregate = sum, na.rm = TRUE, value.var = "count") ########## creates a matrix of locations by month for each species
            matrix5[, paste(nameused1)] <- rowSums(matrix5[, c(2:months2)])
            matrix5[2:months2] <- lapply(matrix5[2:months2], function(x) ifelse(x > 0, 1, x))
            matrix5[, paste(nameused2)] <- rowSums(matrix5[, c(2:months2)])
            locations[, paste(nameused1)] <- as.numeric(matrix5[match(locations$LocationID, matrix5$LocationID), c(paste(nameused1))])
            locations[, paste(nameused2)] <- as.numeric(matrix5[match(locations$LocationID, matrix5$LocationID), c(paste(nameused2))])
          }
        }
      } ##### outer loop line 133
    }
    ## spec_g<-colnames(locations)###****************************************test to make sure column names are the same
    ## spec_f<-spec_f[spec_f%in%spec_g]##*************************************
    spec_f <- setdiff(spec_f, test2) ###### creates subset of spec_f where there are no matching records in test2
    names(locations) <- gsub(" ", "_", names(locations)) ###### removes blanks from column names in locations  to ensure no mismatch or errors
    names(locations) <- gsub("-", "_", names(locations)) ###### removes hyphen from column names in locations  to ensure no mismatch or errors
    locations2 <- locations[, spec_f] ##### uses vector "spec_f" to select a species subset from matrix locations and stores it in matrix locations2

    locations2[is.na(locations2)] <- 0
    locations[is.na(locations)] <- 0 ####### replace "NA" with 0 in df.locations



    observeEvent(input$goButton2, {
      #### write.csv (daily, file="Raw_Daily_Detections.csv")
      #### write.csv (date_missing, file="missing_dates.csv")
      write_csv(matrix2, file = here::here("data", "All_Fauna_Detections X Day.csv"))
      #### write.csv (matrix3, file="Detections X Day.csv")
      write_csv(locations, file = here::here("data", "All_Fauna_Detections X Location.csv"))
      #### write.csv (locations2, file="Detections X Location.csv")
    })


    observeEvent(input$goButton3, {
      write.csv(daily, file = "Selected_Daily_Detections.csv")
      #### write.csv (date_missing, file="missing_dates.csv")
      #### write.csv (matrix2, file="All_Fauna_Detections_Matrix.csv")
      write.csv(matrix3, file = "Selected_Fauna_Detections X Day.csv")
      #### write.csv (locations, file="All_Fauna_Detections X Location.csv")
      write.csv(locations2, file = "Selected_Fauna_Detections X Location.csv")
    })

    ################################################################################################################################################################
    active <- unique(subset(five, select = c("CommonName", "LocationID", "date", "hour", "count")))
    active <- subset(active, active$LocationID %in% cam_loc)
    active <- subset(active, active$CommonName %in% spec_a)
    active <- active[active$date >= Min_Date, ] ##### only records that exceed minimum date are kept
    active <- active[active$date <= Max_Date, ]
    active2 <- dcast(active, hour ~ CommonName, fun.aggregate = sum, na.rm = TRUE, value.var = "count")
    ############################################################################################################################################################
    output$Activity <- renderPlot({
      i <- input$speciesInput
      if (ncol(active2) >= i + 1) {
        active_name <- (colnames(active2)[i + 1])
        active3 <- subset(active2, select = c("hour", paste(active_name)))
        active4 <- ggplot(data = active3, aes(x = hour, y = active3[, 2], group = 1)) +
          ylab("number of detections") +
          xlab("hour") + geom_bar(stat = "identity", fill = "steelblue") +
          ggtitle(paste(active_name), subtitle = NULL) +
          theme(
            axis.text.x = element_text(hjust = 1, color = "black", size = 12),
            axis.text.y = element_text(hjust = 1, color = "black", size = 12),
            axis.title.x = element_text(color = "black", size = 13),
            axis.title.y = element_text(color = "black", size = 13),
            plot.title = element_text(hjust = 0.5, color = "black", size = 14, face = "bold"),
            panel.border = element_rect(colour = "black", fill = NA, size = 1),
            panel.background = element_blank()
          )

        print(active4)
      }
    })
    ##############################################################################################################################################################
    names(matrix3) <- gsub(" ", "_", names(matrix3)) ###### removes blanks from column names in matrix 3
    names(matrix3) <- gsub("-", "_", names(matrix3)) ###### removes hyphen from column names in matrix 3

    if (interval > 0) {
      numofspecies <- NCOL(matrix3) - 5
      output$detection <- renderPlot({
        i <- input$speciesInput


        species <- colnames(matrix3)[i + 5]
        if (ncol(matrix3) >= i + 5) {
          stats <- summarySE(matrix3, c(paste(species)), groupvars = c("idate"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

          pl <- ggplot(data = stats, aes(x = idate, y = stats[, 3], group = 1)) +
            ylab("mean daily detections") +
            xlab("date") + scale_x_date(date_breaks = paste(interval * 3.65, "day")) +
            # ggtitle(paste(species,"(interval:",interval,"days)"), subtitle = NULL)+
            ggtitle(glue::glue("{species} (interval: {interval} days)")) +
            geom_ribbon(aes(ymin = stats[, 3] - stats$ci, ymax = stats[, 3] + stats$ci), fill = "grey90") +
            geom_errorbar(aes(ymin = stats[, 3] - sd, ymax = stats[, 3] + sd), width = .3, color = "grey40", size = 0.2) +
            geom_smooth(method = "lm", se = FALSE, color = "grey40", size = 0.2, linetype = 2, alpha = 0.1) +
            geom_line(color = "blue", size = .5) +
            geom_point(size = .5) +
            theme(
              axis.text.x = element_text(angle = 90, hjust = 1, color = "black", size = 12),
              axis.text.y = element_text(hjust = 1, color = "black", size = 12),
              axis.title.x = element_text(color = "black", size = 13),
              axis.title.y = element_text(color = "black", size = 13),
              plot.title = element_text(hjust = 0.5, color = "black", size = 14, face = "bold"),
              panel.border = element_rect(colour = "black", fill = NA, size = 1),
              panel.background = element_blank()
            )

          print(pl)
        }
      })
      output$cusumplot <- renderPlot({
        i <- input$speciesInput
        if (ncol(matrix3) >= i + 5) {
          species <- colnames(matrix3)[i + 5]
          stats <- summarySE(matrix3, c(paste(species)), groupvars = c("idate"), na.rm = FALSE, conf.interval = 0.99, .drop = TRUE)
          vector <- stats[, 3]
          c <- cusum(vector, title = paste(species, "(interval:", interval, "days)"), xlab = paste("sample period"), decision.interval = 5, se.shift = 1, chart.all = TRUE, add.stats = TRUE)
        }
      })

      output$cull_frey <- renderPlot({
        i <- input$speciesInput
        species <- colnames(matrix3)[i + 5]
        if (ncol(matrix3) >= i + 5) {
          stats <- summarySE(matrix3, c(paste(species)), groupvars = c("idate"), na.rm = FALSE, conf.interval = 0.99, .drop = TRUE)
          vector <- stats[, 3]
          ## png(file=paste(species, " cullen and frey graph.png", sep=" "))
          descdist(vector, discrete = FALSE)
          title(
            main = paste(species, "(interval:", interval, "days)"), line = 2.9,
            cex.main = 1.2, font.main = 2, col.main = "black"
          )
          ## dev.off()
        }
      })

      output$norm_dist <- renderPlot({
        i <- input$speciesInput
        species <- colnames(matrix3)[i + 5]
        if (ncol(matrix3) >= i + 5) {
          stats <- summarySE(matrix3, c(paste(species)), groupvars = c("idate"), na.rm = FALSE, conf.interval = 0.99, .drop = TRUE)
          vector <- stats[, 3]
          fit.norm <- fitdist(vector, "norm")
          ## png(file=paste(species, " distribution.png", sep=" "))
          plot(fit.norm)
        }
        ## dev.off()
      })
    } else {
      numofspecies <- NCOL(matrix3) - 4
      output$detection <- renderPlot({
        i <- input$speciesInput


        species <- colnames(matrix3)[i + 4]
        if (ncol(matrix3) >= i + 4) {
          stats <- summarySE(matrix3, c(paste(species)), groupvars = c("yearmonth"), na.rm = FALSE, conf.interval = 0.99, .drop = TRUE)

          pl <- ggplot(data = stats, aes(x = yearmonth, y = stats[, 3], group = 1)) +
            ylab("mean daily detections") +
            xlab("month") +
            ggtitle(paste(species), subtitle = NULL) +
            geom_errorbar(aes(ymin = stats[, 3] - sd, ymax = stats[, 3] + sd), width = .3, color = "grey40", size = 0.2) +
            geom_ribbon(aes(ymin = stats[, 3] - stats$ci, ymax = stats[, 3] + stats$ci), fill = "grey90") +
            geom_smooth(method = "lm", se = FALSE, color = "grey40", size = 0.2, linetype = 2, alpha = 0.1) +
            geom_line(color = "blue", size = .5) +
            geom_point(size = .5) +
            theme(
              axis.text.x = element_text(angle = 90, hjust = 1, color = "black", size = 12),
              axis.text.y = element_text(hjust = 1, color = "black", size = 12),
              axis.title.x = element_text(color = "black", size = 13),
              axis.title.y = element_text(color = "black", size = 13),
              plot.title = element_text(hjust = 0.5, color = "black", size = 14, face = "bold"),
              panel.border = element_rect(colour = "black", fill = NA, size = 1),
              panel.background = element_blank()
            )

          print(pl)
        }
      })
      output$cusumplot <- renderPlot({
        i <- input$speciesInput
        if (ncol(matrix3) >= i + 4) {
          species <- colnames(matrix3)[i + 4]
          stats <- summarySE(matrix3, c(paste(species)), groupvars = c("yearmonth"), na.rm = FALSE, conf.interval = 0.99, .drop = TRUE)
          vector <- stats[, 3]
          c <- cusum(vector, title = paste(species), xlab = "months", decision.interval = 5, se.shift = 1, chart.all = TRUE, add.stats = TRUE)
        }
      })

      output$cull_frey <- renderPlot({
        i <- input$speciesInput
        species <- colnames(matrix3)[i + 4]
        if (ncol(matrix3) >= i + 4) {
          stats <- summarySE(matrix3, c(paste(species)), groupvars = c("yearmonth"), na.rm = FALSE, conf.interval = 0.99, .drop = TRUE)
          vector <- stats[, 3]
          ## png(file=paste(species, " cullen and frey graph.png", sep=" "))
          descdist(vector, discrete = FALSE)
          title(
            main = paste(species), line = 2.9,
            cex.main = 1.2, font.main = 2, col.main = "black"
          )
          ## dev.off()
        }
      })

      output$norm_dist <- renderPlot({
        i <- input$speciesInput
        species <- colnames(matrix3)[i + 4]
        if (ncol(matrix3) >= i + 4) {
          stats <- summarySE(matrix3, c(paste(species)), groupvars = c("yearmonth"), na.rm = FALSE, conf.interval = 0.99, .drop = TRUE)
          vector <- stats[, 3]
          fit.norm <- fitdist(vector, "norm")
          ## png(file=paste(species, " distribution.png", sep=" "))
          plot(fit.norm)
        }
        ## dev.off()
      })
    }

    d <- locations2 ##### this loads locations2 as the basis for analysis. This is different to the standalone version as that has an extra column which means the  line below would need to specify 3:4
    dsp <- SpatialPoints(d[, 2:3], proj4string = CRS("+init=epsg:32750"))
    dsp <- SpatialPointsDataFrame(dsp, d)
    po1 <- unique(subset(five, select = c(3, 4, 5, 6)))
    po1_1 <- as.matrix(po1)

    xx <- as.numeric(po1_1[, 3])
    yy <- as.numeric(po1_1[, 4])
    zz <- as.character(po1_1[, 1])
    po2 <- SpatialPoints(po1[, 3:4], proj4string = CRS("+init=epsg:32750"))
    po2 <- SpatialPointsDataFrame(po2, po1)

    # define groups for mapping
    cuts <- c(0, 5, 10, 15, 20, 25, 30, 35)
    # set up a palette of interpolated colors
    blues <- colorRampPalette(c("yellow", "orange", "blue", "dark blue"))
    ##### pols <- list("sp.polygons", CA, fill = "lightgray")


    ##### Transforms data to equivalant projections. in this case epsg: 32750 which is zone 50 South WGS 84
    ##### library(rgdal)
    TA <- CRS("+init=epsg:32750")
    dta <- spTransform(dsp, TA)
    cata <- spTransform(CA, TA)
    tr <- spTransform(tr, TA)
    ## if (file.exists(paste(wd,"data/points.shp",sep=""))){
    ## po <- spTransform(po, TA)}

    ###### calculates and plots if neccessary a voroni plot from the camarea locations
    ###### library(dismo)
    v <- voronoi(dta)
    v <- spTransform(v, TA)
    ## Loading required namespace: deldir
    #### plot(v)

    ##############################################################################################################################################
    ## bubble(dsp, species2, main = paste(species2),identify = TRUE,col = "red",maxsize = 3, do.sqrt = TRUE)
    ## pointLabel(xx,yy,labels=zz, doPlot=TRUE)
    ## symbols(xx, yy, circles=paste(matrix3,"$",species2,sep="" ))
    if (length(cam_loc) < nrow(d)) {
      bp <- subset(d, d$LocationID %in% cam_loc)
    } else {
      bp <- d
    }
    output$Bubble_Plot <- renderPlot({
      i <- input$speciesInput
      species2 <- colnames(d)[i + 3]
      if (ncol(d) >= i + 3) {
        ggplot(bp, aes(x = UTM_E, y = UTM_N, size = bp[, i + 3])) + geom_point(alpha = 0.2) + scale_size_continuous(range = c(1, 16)) +
          geom_point(colour = "darkslategray3") +
          labs(size = paste(species2)) + geom_text(aes(label = LocationID), size = 4) + coord_fixed(ratio = 1)

        ##################################################################################################################################################
      }
    })



    output$NN_Interpolation <- renderPlot({

      ###### this code  that produces interpolations of all columns from locations2. It will sequentially go through each column and produce a nearest neighbor plot and an IDW plot. Several preliminary plots can also be turned on but are not really necessary unless you want to view the entire process rather than just the final output.


      i <- input$speciesInput
      ## numofspecies<-NCOL(d)-3 ##### extracts number of columns to define the size of the loop
      ### for (i in 1:numofspecies)
      ## {
      species2 <- colnames(d)[i + 3]
      if (ncol(d) >= i + 3) {



        #### aggregates data, clips to boundary, and fills voroni plot colours based on detections at each location
        # if (file.exists(here::here("data", "outline.shp"))) {
          ca <- aggregate(cata)
          ca <- spTransform(ca, TA)
          ## Loading required namespace: rgeos
          vca <- raster::intersect(v, CA)
          ##### spplot(vca, paste(species), col.regions=rev(get_col_regions()))

          #######   rasterizes data based on required resolution
          r <- raster(cata, res = rast) #### takes value from rast to set resolution in meters
          vr <- rasterize(vca, r, paste(species2))
        # } else {
        #   r <- raster(v, res = rast)
        #   vr <- rasterize(v, r, paste(species2))
        # }
        ###### plot(vr)

        ##### set parameters for plots to write to png files
        ###### png(file=paste(species, " NN.png", sep=" "),width=900,height=1000,res=200)
        ##### par(mgp=c(0,.4,0),mar=c(2,2,1.5,1)+0.1)  ##  mar is for white space bottom, left, top and right, respectively while mgp sets label distance from ticks on the axis
        ##### op<-par(cex=0.5)##### this was required to reduce the overall font size for the legend. adjusting the individual axis label cex parameters compensates for this.

        ###### plots nearest neighbour with overlays of tracks, boundary and camera points
        ###### library(gstat)
        txt <- noquote(paste("gstat(formula=", species2, "~1, locations=dta, nmax=5, set=list(idp = 0))")) ##### this line and the following are used with the paste function to
        gs <- eval(parse(text = txt))
        nn <- interpolate(r, gs)
        ## [inverse distance weighted interpolation]
        nnmsk <- mask(nn, vr)
        #### plot (nnmsk, lwd=.2, cex.main=0.6,cex.sub=0.2, cex.axis=0.9, asp=1, tck=-0.02)
        ##### title(main = list(paste(species," (nearest neighbour)"), cex = .6, col = "blue", font = 2))
        ##### plot(tr, add = TRUE, lwd = 0.5, border = "black")
        ##### plot(CA, add = TRUE, lwd = 0.5,  border = "black")
        ##### plot(po, add = TRUE, lwd = 0.5, cex=0.5,  border = "red")
        ##### dev.off()

        ##### png(file=paste(species, " IDW.png", sep=" "),width=900,height=1000,res=200)
        ##### par(mgp=c(0,.4,0),mar=c(2,2,1.5,1)+0.1)  ##  mar is for white space bottom, left, top and right, respectively while mgp sets label distance from ticks on the axis
        ###### op<-par(cex=0.5)##### this was required to reduce the overall font size for the legend. adjusting the individual axis label cex parameters compensates for this.
        ###### plots nearest IDW with overlays of tracks, boundary and camera points
        ###### library(gstat)
        txt2 <- noquote(paste("gstat(formula=", species2, "~1, locations=dta)"))
        gs2 <- eval(parse(text = txt2))
        idw <- interpolate(r, gs2)





        # i<-input$speciesInput
        # species2<-colnames( d )[i+3]
        ##### png(file=paste(species, " NN_hill.png", sep=" "),width=900,height=1000,res=200)
        # par(mgp=c(0,.4,0),mar=c(2,2,1.5,1)+0.1)  ##  mar is for white space bottom, left, top and right, respectively while mgp sets label distance from ticks on the axis
        ## op<-par(cex=0.5)##### this was required to reduce the overall font size for the legend. adjusting the individual axis label cex parameters compensates for this.
        #### plots a hill shaded version of the nearest neighbour interpolation above
        slope <- terrain(nnmsk, opt = "slope") #### calculates slope of nnmsk with the terrain function
        aspect <- terrain(nnmsk, opt = "aspect") #### calculates aspect of nnmsk with the terrain function
        hill <- hillShade(slope, aspect, 40, 270) #### calculates hillshade from slop and aspect of nnmsk
        plot(hill, col = grey(0:100 / 100), legend = FALSE, lwd = .2, cex.main = 0.6, cex.sub = 0.2, cex.axis = 0.9, asp = 1, tck = -0.02) #### plots hillshade
        ## op<-par(cex=0.5)
        plot(nnmsk, col = rainbow(25, alpha = 0.35), cex = 1, add = TRUE) #### overlays raster layer nmsk on hillshade
        title(main = list(paste(species2, " (NN)"), cex = 1, col = "blue", font = 2))
        ### if (file.exists(paste(wd,"data/tracks.shp",sep=""))){
        # if (file.exists(here::here("data", "tracks.shp"))) {
          plot(tr, add = TRUE, lwd = 0.5, border = "black")
        # }
        # if (file.exists(paste(wd, "data/outline.shp", sep = ""))) {
          plot(CA, add = TRUE, lwd = 0.5, border = "black")
        # }
        ## if (file.exists(paste(wd,"data/points.shp",sep=""))){
        ## plot(po, add = TRUE, lwd = 0.5, cex=0.5,  border = "red")}
        ###### dev.off()
        plot(po2, add = TRUE, lwd = 0.5, border = "black")

        pointLabel(xx, yy, labels = zz, doPlot = TRUE)
      }
    })

    output$IDW_Interpolation <- renderPlot({

      ## [inverse distance weighted interpolation]


      ###### this code  that produces interpolations of all columns from locations2. It will sequentially go through each column and produce a nearest neighbor plot and an IDW plot. Several preliminary plots can also be turned on but are not really necessary unless you want to view the entire process rather than just the final output.


      i <- input$speciesInput
      ## numofspecies<-NCOL(d)-3 ##### extracts number of columns to define the size of the loop
      ### for (i in 1:numofspecies)
      ## {
      species2 <- colnames(d)[i + 3]
      if (ncol(d) >= i + 3) {


        #### aggregates data, clips to boundary, and fills voroni plot colours based on detections at each location
        # if (file.exists(here::here("data", "outline.shp"))) {
          ca <- aggregate(cata)
          ca <- spTransform(ca, TA)
          ## Loading required namespace: rgeos
          vca <- raster::intersect(v, CA)
          ##### spplot(vca, paste(species), col.regions=rev(get_col_regions()))

          #######   rasterizes data based on required resolution
          r <- raster(cata, res = rast) #### takes value from rast to set resolution in meters
          vr <- rasterize(vca, r, paste(species2))
        # } else {
          # r <- raster(v, res = rast)
          # vr <- rasterize(v, r, paste(species2))
        # }
        ###### plot(vr)


        ##### set parameters for plots to write to png files
        ###### png(file=paste(species, " NN.png", sep=" "),width=900,height=1000,res=200)
        ##### par(mgp=c(0,.4,0),mar=c(2,2,1.5,1)+0.1)  ##  mar is for white space bottom, left, top and right, respectively while mgp sets label distance from ticks on the axis
        ##### op<-par(cex=0.5)##### this was required to reduce the overall font size for the legend. adjusting the individual axis label cex parameters compensates for this.

        ###### plots nearest neighbour with overlays of tracks, boundary and camera points
        ###### library(gstat)
        txt <- noquote(paste("gstat(formula=", species2, "~1, locations=dta, nmax=5, set=list(idp = 0))")) ##### this line and the following are used with the paste function to
        gs <- eval(parse(text = txt))
        nn <- interpolate(r, gs)
        ## [inverse distance weighted interpolation]
        nnmsk <- mask(nn, vr)
        #### plot (nnmsk, lwd=.2, cex.main=0.6,cex.sub=0.2, cex.axis=0.9, asp=1, tck=-0.02)
        ##### title(main = list(paste(species," (nearest neighbour)"), cex = .6, col = "blue", font = 2))
        ##### plot(tr, add = TRUE, lwd = 0.5, border = "black")
        ##### plot(CA, add = TRUE, lwd = 0.5,  border = "black")
        ##### plot(po, add = TRUE, lwd = 0.5, cex=0.5,  border = "red")
        ##### dev.off()

        ##### png(file=paste(species, " IDW.png", sep=" "),width=900,height=1000,res=200)
        ##### par(mgp=c(0,.4,0),mar=c(2,2,1.5,1)+0.1)  ##  mar is for white space bottom, left, top and right, respectively while mgp sets label distance from ticks on the axis
        ###### op<-par(cex=0.5)##### this was required to reduce the overall font size for the legend. adjusting the individual axis label cex parameters compensates for this.
        ###### plots nearest IDW with overlays of tracks, boundary and camera points
        ###### library(gstat)
        txt2 <- noquote(paste("gstat(formula=", species2, "~1, locations=dta)"))
        gs2 <- eval(parse(text = txt2))
        idw <- interpolate(r, gs2)
        idwr <- mask(idw, vr)
        ######## plot(idwr,lwd=.2, cex.main=0.6,cex.sub=0.2, cex.axis=0.9, asp=1, tck=-0.02)
        ##### title(main = list(paste(species," (IDW)"), cex = .6, col = "blue", font = 2))
        ##### plot(tr, add = TRUE, lwd = 0.5, border = "black")
        ##### plot(CA, add = TRUE, lwd = 0.5,  border = "black")
        ##### plot(po, add = TRUE, lwd = 0.5, cex=0.5,  border = "red")
        ##### dev.off()
        ## species2<-colnames( d )[i+3]
        ### png(file=paste(species, " IDW_hill.png", sep=" "),width=900,height=1000,res=200)
        # par(mgp=c(0,.4,0),mar=c(2,2,1.5,1)+0.1)  ##  mar is for white space bottom, left, top and right, respectively while mgp sets label distance from ticks on the axis
        ## op<-par(cex=0.5)##### this was required to reduce the overall font size for the legend. adjusting the individual axis label cex parameters compensates for this.
        #### plots a hill shaded version of the IDW interpolation above
        slope <- terrain(idwr, opt = "slope")
        aspect <- terrain(idwr, opt = "aspect")
        hill <- hillShade(slope, aspect, 40, 270)
        plot(hill, col = grey(0:100 / 100), legend = FALSE, lwd = .2, cex.main = 0.6, cex.sub = 0.2, cex.axis = 0.9, asp = 1, tck = -0.02)
        ## op<-par(cex=0.5)
        plot(idwr, col = rainbow(25, alpha = 0.35), cex = 1, add = TRUE)
        title(main = list(paste(species2, " (IDW)"), cex = 1, col = "blue", font = 2))
        ### if (file.exists(paste(wd,"data/tracks.shp",sep=""))){
        # if (file.exists(here::here("data", "tracks.shp"))) {
          plot(tr, add = TRUE, lwd = 0.5, border = "black")
        # }
        # if (file.exists(paste(wd, "data/outline.shp", sep = ""))) {
          plot(CA, add = TRUE, lwd = 0.5, border = "black")
        # }
        ## if (file.exists(paste(wd,"data/points.shp",sep=""))){
        ## plot(po, add = TRUE, lwd = 0.5, cex=0.5,  border = "red")}
        plot(po2, add = TRUE, lwd = 0.5, border = "black")
        pointLabel(xx, yy, labels = zz, doPlot = TRUE)
        #### dev.off()
      }
    })


    observeEvent(input$speciesInput, {
      if (interval > 0) {
        i <- input$speciesInput
        if (ncol(matrix3) >= i + 5) {
          species <- colnames(matrix3)[i + 5]

          stats <- summarySE(matrix3, c(paste(species)), groupvars = c("idate"), na.rm = FALSE, conf.interval = 0.99, .drop = TRUE)
          stats2 <- format(stats, digits = 4)
          output$Table <- renderDT({
            stats2
          })
        }
      } else {
        i <- input$speciesInput
        if (ncol(matrix3) >= i + 4) {
          species <- colnames(matrix3)[i + 4]

          stats <- summarySE(matrix3, c(paste(species)), groupvars = c("yearmonth"), na.rm = FALSE, conf.interval = 0.99, .drop = TRUE)
          stats2 <- format(stats, digits = 4)
          output$Table <- renderDT({
            stats2
          })
        }
      }
    })
  }) ##### outer observe event trigger

  observeEvent(input$goButton1, {
    ####### Min_Date <- as.Date(c("29/09/2016"),"%d/%m/%Y") ###### date selection for minimum date
    ###### Max_Date <- as.Date(c("3/07/2018"),"%d/%m/%Y") ###### date selection for maximum date
    dr <- input$daterange1
    Min_Date <- dr[1] ###### date selection for minimum date
    Max_Date <- dr[2] ###### date selection for maximum date
    ###### cam_loc <- c(23:42) ##### a vector which is used to select LocationID values eg (1:30,35,40,45:50)
    val <- input$slider1
    cam_loc <- c(val[1]:val[2])
    ##### spec_a<-c("Brush Wallaby","Cat","Chuditch","Echidna","Fox","Grey Kangaroo","Numbat","Possum","Quenda","Rabbit","Red-tailed Phascogale","Tammar","Woylie")
    spec_a <- input$variable
    interval <- input$intInput
    outline <- ("outline") ###### boundary shapefile name
    tracks <- ("tracks") ###### tracks shapefile name
    ## points<-("points")  	###### points shapefile name
    rast <- input$slider2 ###### resolution of raster in meters


    five <- five[five$LocationID %in% cam_loc, ] ##### only records that have the same LocationID as the values in vector "cam_loc" are kept
    five <- five[five$date >= Min_Date, ] ##### only records that exceed minimum date are kept
    five <- five[five$date <= Max_Date, ] ##### only records that are less than maximum date are kept

    spec_b <- as.character(unique(five$CommonName)) #### creates unique species list from matrix "five"
    spec_c <- spec_a[spec_a %in% spec_b] #### finds matching species between spec_a and spec_b and stores them as a vector in spec_c
    spec_d <- paste(spec_c, "count", sep = " ", collapse = NULL) ###### species list with "count appended
    spec_e <- paste(spec_c, "month", sep = " ", collapse = NULL) ###### species list with month appended
    spec_f <- gsub(" ", "_", gsub("-", " ", sort(c(spec_d, spec_e)))) #### combines vectors spec_b and spec_c, sorts them and then removes spaces and hyphens and replaces them with underscaores

    if (interval > 0) {
      spec_c <- c("date", "idate", "yearmonth", "year", "month", spec_c) #### combines original contents of c with date, yearmonth etc......
    } else {
      spec_c <- c("date", "yearmonth", "year", "month", spec_c) #### combines original contents of c with date, yearmonth etc......
    }
    spec_f <- c("LocationID", "UTM_E", "UTM_N", spec_f) #### combines original contents of f with LocationID, UTM_E etc......



    number_locations <- length(camloc$LocationID) ##### number of distinct locations
    date_range <- seq(Min_Date, Max_Date, "days") ###### range of dates
    number_dates <- length(date_range) ###### number of dates
    date_entire <- rep(date_range, number_locations) #### dates repeated for every location
    location_entire <- sort(rep(1:number_locations, number_dates)) ##### vector of locations repeated by number of dates and sorted
    LocationID <- as.data.frame(location_entire) #### converts vector to dataframe
    date <- as.data.frame(date_entire) ##### converts vector to dataframe
    colnames(LocationID)[colnames(LocationID) == "location_entire"] <- "LocationID" ##### changes the name of the the field from "location_entire" to "LocationID"
    colnames(date)[colnames(date) == "date_entire"] <- "date" ##### changes the name of the the field from "date_entire" to "date"
    locationbydate <- cbind(date, LocationID) ###### combines date and LocationID into single dataframe called locationbydate
    missing_date_location <- anti_join(locationbydate, five, by = c("date", "LocationID"))
    date_missing <- five[0, ] ##### creates an empty dataframe called "date_missing" based on the fields from dataframe "five"
    date_missing <- plyr::rbind.fill(date_missing, missing_date_location) ##### combines date_missing and date_range
    date_missing$count[is.na(date_missing$count)] <- 0 #### changes NA value in count to 0
    date_missing$UTM_E <- as.numeric(five[match(date_missing$LocationID, five$LocationID), 5])
    date_missing$UTM_N <- as.numeric(five[match(date_missing$LocationID, five$LocationID), 6])
    date_missing$CommonName <- as.factor("None")
    date_missing$day <- as.numeric(format(date_missing$date, "%d"))
    date_missing$month <- as.numeric(format(date_missing$date, "%m"))
    date_missing$year <- as.numeric(format(date_missing$date, "%Y"))
    total <- rbind(five, date_missing) ###### combines original datafile with all missing dates

    daily <- unique(subset(total, select = c("SpeciesID", "LocationID", "UTM_E", "UTM_N", "CommonName", "Genus", "Species", "date", "year", "month", "day", "count"))) ##### raw daily detections
    daily$yearmonth <- do.call(paste, c(daily[c("year", "month")], sep = "-"))

    if (interval > 0) {
      total$idate <- cut(total$date, breaks = paste(interval, "day")) #### creates field called idate which is grouped date based on variable interval
      total$idate <- as.Date(as.character(total$idate), format = "%Y-%m-%d") ###### changes  idate to date format
      daily$idate <- cut(daily$date, breaks = paste(interval, "day")) #### creates field called idate which is grouped date based on variable interval
      daily$idate <- as.Date(as.character(daily$idate), format = "%Y-%m-%d") ###### changes  idate to date format
    }

    matrix2 <- dcast(daily, date ~ CommonName, fun.aggregate = sum, na.rm = TRUE, value.var = "count")
    matrix2$date <- as.Date(matrix2$date, "%d/%m/%Y")
    matrix2$month <- format(matrix2$date, "%m")
    matrix2$year <- format(matrix2$date, "%Y")
    matrix2$yearmonth <- do.call(paste, c(matrix2[c("year", "month")], sep = "-"))
    ###############################################################################################################################
    month_missing <- daily[daily[["date"]] >= Min_Date, ]
    month_missing <- daily[daily[["date"]] <= Max_Date, ]
    month_missing <- unique(month_missing[, c("yearmonth", "LocationID")])
    month_missing <- month_missing[ month_missing[["LocationID"]] >= min(cam_loc), ]
    month_missing <- month_missing[ month_missing[["LocationID"]] <= max(cam_loc), ]
    ###############################################################################################################################
    if (interval > 0) {
      matrix2$idate <- cut(matrix2$date, breaks = paste(interval, "day"))
      matrix2$idate <- as.Date(as.character(matrix2$idate), format = "%Y-%m-%d")

      ###############################################################################################################################
      idate_missing <- daily[daily[["date"]] >= Min_Date, ]
      idate_missing <- daily[daily[["date"]] <= Max_Date, ]
      idate_missing <- unique(idate_missing[, c("idate", "LocationID")])
      idate_missing <- idate_missing[idate_missing[["LocationID"]] >= min(cam_loc), ]
      idate_missing <- idate_missing[idate_missing[["LocationID"]] <= max(cam_loc), ]
      #############################################################################################################################
    }

    matrix3 <- matrix2[, spec_c] ##### uses vector "spec_c" to select a species subset from matrix 2 and store it in matrix 3

    locations <- unique(subset(camloc, select = c("LocationID", "UTM_E", "UTM_N"))) ###### creates dataframe locations with unique location and UTM coordinates
    commonname <- unique(subset(species, select = c("CommonName", "Genus", "Species", "SpeciesID"))) ####### creates dataframe commonname with unique taxonomic information

    if (interval > 0) {
      int <- nrow(unique(subset(total, select = c("idate")))) + 1 ####### stores the total number of intervals in the survey period
    } else {
      months <- nrow(unique(subset(total, select = c("year", "month")))) + 1 ####### stores the total number of months in the survey period
    }

    iterations <- nrow(commonname) ######## stores the number of unique names in variable iterations
    iterations2 <- length(spec_a)


    test2 <- NULL

    ##########################################################################################################################################################################

    if (interval > 0) {
      for (i in 1:iterations2) { ####### sets up a loop based on number of unique names

        nameused3 <- paste(as.character(spec_a[i])) ####### takes the ith commonname and stores it in name used
        eight <- daily[grep(paste(nameused3), daily$CommonName), ]
        ##############################################################################################################################
        oc1 <- eight #### save eight to oc1
        oc1 <- unique(oc1[, c("idate", "LocationID", "count")]) ### stores only three variables in oc1
        oc2 <- oc1 [, c("idate", "LocationID")] ##### stores ony two varaibles in oc2
        oc3 <- anti_join(idate_missing, oc2) ##### compares values between matices and stores unmatched from idate_missing in oc3
        oc3$count <- 0 ##### creates variable count an dstores 0 in it
        oc4 <- rbind(oc3, oc1) #### combines oc3 and oc1 together into oc4
        oc5 <- dcast(oc4, LocationID ~ idate,
          fun.aggregate = sum,
          na.rm = TRUE, value.var = "count"
        ) ########## creates a matrix of locations by month for each species
        oc5[, paste(nameused3)] <- rowSums(oc5[, c(2:ncol(oc5))])
        write.csv(oc5, file = paste(nameused3, interval, "occupancy.csv"))
      }
    } else {
      for (i in 1:iterations2) { ####### sets up a loop based on number of unique names

        nameused3 <- paste(as.character(spec_a[i])) ####### takes the ith commonname and stores it in name used
        eight <- daily[grep(paste(nameused3), daily$CommonName), ]
        ##############################################################################################################################
        oc1 <- eight #### save eight to oc1
        oc1 <- unique(oc1[, c("yearmonth", "LocationID", "count")]) ### stores only three variables in oc1
        oc2 <- oc1 [, c("yearmonth", "LocationID")] ##### stores ony two varaibles in oc2
        oc3 <- anti_join(month_missing, oc2) ##### compares values between matices and stores unmatched from idate_missing in oc3
        oc3$count <- 0 ##### creates variable count an dstores 0 in it
        oc4 <- rbind(oc3, oc1) #### combines oc3 and oc1 together into oc4
        oc5 <- dcast(oc4, LocationID ~ yearmonth,
          fun.aggregate = sum,
          na.rm = TRUE, value.var = "count"
        ) ########## creates a matrix of locations by month for each species
        oc5[, paste(nameused3)] <- rowSums(oc5[, c(2:ncol(oc5))])
        write.csv(oc5, file = paste(nameused3, "months_occupancy.csv"))
      }
    }
  })
} #### outer server

shinyApp(ui = ui, server = server)
