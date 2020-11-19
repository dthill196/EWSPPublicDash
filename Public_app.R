#
#wastewater dashboard shiny prototype alpha version
#

#prep script

#packages
library(sf)
library(dplyr)
library(lubridate)
library(tigris)
library(rgdal)
library(leaflet)
library(ggplot2)
library(shiny)
library(htmlwidgets)
library(htmltools)
library(leaflet.extras)
library(tidyr)
library(scales)



#data prep
#data
#setwd("~/DashboardProto")
#setwd("C:/Users/dthil/Dropbox/CEMI/wastewater/Dashboard/DashboardProto_4/")
#setwd("E:/Dropbox/CEMI/wastewater/Dashboard/DashboardProto_4/")



#data
wastewater <- read.csv("Dashboard_Data/Master database.csv",
                       stringsAsFactors = FALSE, header=TRUE, sep=",")

# 1 # remove word county in county column
library(stringr)
wastewater$County <- str_remove(wastewater$County, " County")

# 2 # if space in < LOQ
wastewater$SARS.mean <- str_replace(wastewater$SARS.mean, "< LOQ", "<LOQ")


# with or without saratoga
#catchment <- st_read("Dashboard_Data/New York State sewersheds with saratoga.shp")
catchment <- st_read("Dashboard_Data/New York State sewersheds.shp")

sewer.id <- read.csv("Dashboard_Data/Sewer.IDs.csv",
                     stringsAsFactors = FALSE)

#ny shapefile
ny_counties <- st_read("Dashboard_Data/Counties_Shoreline.shp")

#school pt data
school_pts <- st_read("Dashboard_Data/School_Pts_File.shp")


#change column name
colnames(wastewater)[1] <- "Date_collected"

##
# working with the date
##
wastewater$Date_collected = mdy(wastewater$Date_collected)
Last_sample <- max(wastewater$Date_collected) #date of most recent sample, helps prevent losing too much old data

# Creating the maximum date by each sewershed ID #
wastewater <- wastewater %>% 
  group_by(SW_ID) %>%
  mutate(maxdate = max(Date_collected))

# Keeping only sewersheds with recent data
wastewater <- wastewater %>%
  group_by(SW_ID)%>%
  #filter(Date_collected  >= (today() - days(16))) #may need to change this eventually
  filter(Date_collected  >= (Last_sample - days(15))) %>% #set to last sample date
  filter(n() > 1) %>%
  filter(Type == "community")

# exclude hamilton college
wastewater$County[wastewater$County == "Village of Clinton"] <- "Oneida"

wastewater <- wastewater[!(wastewater$Access.Point == "Hamilton College"), ]

## GENERATING COPIES OF SARS 2 RNA IN WASTEWATER
# If SARS2 RNA was not detected, using a value of 1
# If SARS2 RNA was detected but not quantifiable, using a value of 3.5
# Calculating copies of SARS2 RNA #
wastewater$copies <- 3.5
wastewater$copies <- ifelse(wastewater$SARS.mean!="<LOQ", as.numeric(as.character(wastewater$SARS.mean)), wastewater$copies)
wastewater$copies <- ifelse(wastewater$SARS.pos==0, 1, wastewater$copies)

### STANDARDIZING COPIES OF SARS2 RNA IN WASTEWATER TO CRASSPHAGE ###
# crAssphage cDNA
wastewater$phageR.mean <- as.numeric(as.character(as.numeric(wastewater$phageR.mean)))
wastewater$ratiocDNA <- wastewater$copies/wastewater$phageR.mean
# crAssphage DNA
wastewater$ratioDNA <- wastewater$copies/wastewater$phageD.mean

### First creating a weight based on population contribution ###
# Summing the crAssphage DNA levels by each date #
wastewater.county <- wastewater %>%
  group_by(County, Date_collected) %>%
  dplyr::summarize(total.phage = sum(phageD.mean, na.rm = TRUE))

# Merging the summed crAssphage levels back into the dataframe #
wastewater <- left_join(wastewater, wastewater.county, by = c("County", "Date_collected"))

# Weighting each sample
wastewater$weight <- wastewater$phageD.mean / wastewater$total.phage #contribution of one sample to the total for the county

# Calculating the weighted mean #
wastewater.county2 <- wastewater %>%
  group_by(County, Date_collected) %>%
  summarise(weighted_ratio = weighted.mean(copies, weight, na.rm=TRUE),
            maxdate_1 = max(Date_collected))
wastewater <- left_join(wastewater, wastewater.county2, by = c("County", "Date_collected"))

wastewater$days <- as.numeric(wastewater$maxdate-wastewater$Date_collected)

###
#county level regressions

backup <- wastewater.county #in case for loop messes up the df

datalist <- list()

for(i in unique(wastewater$County)){
  df_reg <- wastewater %>%
    filter(County == i)
  
  #df_reg_1week <- df_reg %>%
  # group_by(County) %>%
  #arrange(desc(Date_collected)) %>%
  #slice(1:2)
  #filter(days < 8)
  
  #simple regression
  #scale values so they correlate as a percentage change with each new measure?
  #model_1week <- lm(log(weighted_ratio) ~ Date_collected, data = df_reg_1week)
  
  
  #add to df
  #df_reg$OneWeekBeta <- model_1week$coefficients[2]
  #df_reg$OneWeekR2 <- summary(model_1week)$r.squared
  
  #two week regression
  df_reg_2weeks <- df_reg %>%
    group_by(County) %>%
    #arrange(desc(Date_collected)) %>%
    #slice(1:3)
    filter(days < 15)
  model_2weeks <- lm(log(weighted_ratio) ~ Date_collected, data = df_reg_2weeks)
  summary(model_2weeks)
  
  df_reg$TwoWeekBet <- model_2weeks$coefficients[2]
  df_reg$TwoWeekR2 <- summary(model_2weeks)$r.squared
  datalist[[i]] <- df_reg
}
wastewater.county <- do.call(rbind, datalist)

# select counties participating in public work
counties_public <- c("Cortland")
wastewater.county <- wastewater.county %>%
  filter(County %in% counties_public)


#############
# sewershed plots showing average and notice (create notice in app i think, or save as .rdata?)
#############
# plot two pops up when you click on county
# shows county catchments colored according to warning system

# merge in sewershed id information
# fix wastewater sw_id
library(stringr)
wastewater$SW_ID  <- gsub("[^0-9A-Za-z///' ]","" , wastewater$SW_ID ,ignore.case = TRUE)

# need to remove sewersheds with less than two observations
wastewater <- wastewater %>% 
  group_by(SW_ID) %>% 
  filter(n()>1)
wastewater$days <- as.numeric(wastewater$days)
datalist2 <- list()

for(i in unique(wastewater$SW_ID)){
  df_reg <- wastewater %>%
    filter(SW_ID == i)
  
  #df_reg_1week <- df_reg %>%
  # group_by(SW_ID) %>%
  #arrange(desc(Date_collected)) %>%
  #filter(days < 8)
  
  #simple regression
  #scale values so they correlate as a percentage change with each new measure?
  #model_1week <- lm(log(ratiocDNA) ~ Date_collected, data = df_reg_1week)
  
  #add to df
  #df_reg$OneWeekBet <- model_1week$coefficients[2]
  #df_reg$OneWeekR2 <- summary(model_1week)$r.squared
  
  #two week regression
  df_reg_2weeks <- df_reg %>%
    group_by(SW_ID) %>%
    #arrange(desc(Date_collected)) %>%
    filter(days < 15)
  model_2weeks <- lm(log(ratiocDNA) ~ Date_collected, data = df_reg_2weeks)
  summary(model_2weeks)
  
  df_reg$TwoWeekBet <- model_2weeks$coefficients[2]
  df_reg$TwoWeekR2 <- summary(model_2weeks)$r.squared
  datalist2[[i]] <- df_reg
}
wastewater.df.sewersheds <- do.call(rbind, datalist2)

# some sewersheds have values greater than 1
wastewater.df.sewersheds$TwoWeekBet[wastewater.df.sewersheds$TwoWeekBet > 1] <- 1

wastewater.sewersheds <- left_join(wastewater.df.sewersheds, sewer.id, by = c("SW_ID"))

##########################
# one catchment counties #
##########################

# indicator variable for one catchment county
one_catchment <- wastewater.sewersheds %>%
  group_by(County.x) %>%
  summarize(Number_catchments = length(unique(SW_ID)))
wastewater.sewersheds <- left_join(wastewater.sewersheds, one_catchment, by = c("County.x"))

# select catchments
one_catchment <- wastewater.sewersheds %>% filter(Number_catchments == 1)
# select columns to merge to county level file
one_catchment <- one_catchment %>%
  select(Date_collected, County.x, SW_ID, ratiocDNA, TwoWeekR2, TwoWeekBet, Number_catchments)
# rename columns to avoid duplication
colnames(one_catchment) <- c("Date_collected", "County", "SW_ID", "ratiocDNA_catch", "TwoWeekR2_catch", 
                             "TwoWeekBet_catch", "Number_catchments")
# merge
backup <- wastewater.county
#wastewater.county <- backup
wastewater.county <- left_join(wastewater.county, one_catchment, by = c("Date_collected", "County"))

# replace once catchment counties betas
wastewater.county$Number_catchments <- as.character(wastewater.county$Number_catchments)
wastewater.county$Number_catchments[is.na(wastewater.county$Number_catchments)] <- ">1"
wastewater.county$TwoWeekBet <- ifelse(wastewater.county$Number_catchments == "1", wastewater.county$TwoWeekBet_catch, wastewater.county$TwoWeekBet)

# replace R2
wastewater.county$TwoWeekR2 <- ifelse(wastewater.county$Number_catchments == "1", wastewater.county$TwoWeekR2_catch, wastewater.county$TwoWeekR2)

# replace over time data with catchment values
wastewater.county$weighted_ratio <- ifelse(wastewater.county$Number_catchments == "1", wastewater.county$ratiocDNA_catch, wastewater.county$weighted_ratio)

# display most recent data for the base map for each county

dates_ww_recent <- wastewater.county %>%
  group_by(County) %>%
  dplyr::summarize(Date_collected = format(max(Date_collected))
  )
dates_ww_recent$Date_collected <- ymd(dates_ww_recent$Date_collected)

county_ww_recent <- inner_join(dates_ww_recent, wastewater.county, by = c("County", "Date_collected"), all.y = FALSE)

recent2 <- county_ww_recent %>%
  filter(!duplicated(County))


# merge to shapefile
ny_counties$County <- ny_counties$NAME
ny_counties <- merge(ny_counties, recent2, by = c("County"), all.x = TRUE)

ny_counties$Date <- ny_counties$Date_collected


###
# sewershed map object for wwtp, initial map
###
# select wwtp influent sewersheds
# merge this to catchment
catchment.wwtp <- merge(catchment, wastewater.df.sewersheds, by = c("SW_ID"))
catchment.wwtp$Method[catchment.wwtp$SW_ID == "36055NY0028339005"] <- "Influent"
wwtp.map <- catchment.wwtp %>%
  filter(Method == "Influent")%>%#missed some...go back and add when redo buffalo i think
  filter(Date_collected == maxdate)

wwtp.map$Date <- wwtp.map$Date_collected
wwtp.map$County <- wwtp.map$County.x

#schools <- merge(school_pts, wastewater.df.sewersheds,  by = c("SW_ID"))


# sewersheds within wwtp
sewershed.map <- catchment.wwtp %>%
  filter(is.na(Method) | Method == "Manhole" | Method == "Pump station" | Method == "") %>%
  filter(Date_collected == maxdate)

# add counties with one layer of sewersheds (ie oswego)
single.catchments <- catchment %>%
  group_by(County)%>%
  filter(n()<3)

add.catchments <- catchment.wwtp %>%
  filter(County.x %in% single.catchments$County)
sewershed.map <- rbind(sewershed.map, add.catchments)

# what about when sampling only wwtp? add manually for now
cayuga <- catchment.wwtp %>% filter(County.x == "Cayuga")
sewershed.map <- rbind(sewershed.map, cayuga)
monroe <- catchment.wwtp %>% filter(Sewershed == "FEV WWTP")
sewershed.map <- rbind(sewershed.map, monroe)
sewershed.map <- rbind(sewershed.map, cayuga)
sewershed.map$Date <- sewershed.map$Date_collected
sewershed.map$County <- sewershed.map$County.x


# metro main interceptor map

# sewersheds within wwtp
metro.map <- catchment.wwtp %>%
  filter(County.x == "Onondaga" & Method == "Manhole") %>%
  filter(Date_collected == maxdate)

# shorten names
colnames(metro.map)[22] <- "Date"
colnames(metro.map)[41] <- "WeightRat"

# zip codes
zip_code <- st_read("Dashboard_Data/NY_ZipCodes_2019_sewersheds.shp")
ny_counties_2 <- ny_counties %>%
  dplyr::select(County, TwoWeekBet, TwoWeekR2)

ny_counties <- st_transform(
  ny_counties,
  
  crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
)

#scale values
wastewater.county$Scale_weightedRatio <- scales::rescale(wastewater.county$weighted_ratio, to = c(0,1))
wastewater.sewersheds$Scale_ratiodna <- scales::rescale(wastewater.sewersheds$ratiocDNA, to = c(0,1))
# Define UI for application that draws a histogram
ui <- fluidPage( 
  titlePanel("SARS-2 Wastewater Surveillance Platform: NY"),
  titlePanel( h3(paste("Last Updated: ",today(),sep=""))),    #date of last update
  titlePanel(h3(paste("Most recent sample: ", max(wastewater$maxdate), sep = ""))), #date of last sample
  #titlePanel(h5("Click on a county in the first map to see trends of SARS-CoV-2 in wastewater. Results appear in the data summary tab.")), #instructions, currently a header
  
  splitLayout(cellWidths = c("50%", "50%"), leafletOutput('NYBetaMap'), leafletOutput("NYR2Map")),
  br(),
  fluidRow(tabsetPanel(id = "tabs",
                       tabPanel("Welcome", column(6, 
                                                  h3("How to use this dashboard"),
                                                  #br(),
                                                  p("This dashboard provides trend analysis of the wastewater surveillance
                                                    being conducted in participating counties. The statewide maps display
                                                    trends at the county level and to learn more information about trends
                                                    within a county, click on a county in the first map. Results appear in the data summary tab."),
                                                  h4("First map"),
                                                  p(
                                                    "The first map displays the two week trend in wastewater detection. Hover over a county to see the
                                                    current estimated trend. In the first map, counties colored shades of orange indicate trend values greater than 0
                                                    indicate an increasing trend in detection of SARS2 RNA in 
                                                    wastewater samples from that county. Counties colored in shades of blue reflect trend values
                                                    less than zero which indicate
                                                    declining trend in detection. Counties colored in lighter shades closer to white reflect trend values
                                                    closer to 0 indicating a stable trend which is not changing over time. The value can be interpreted as a 
                                                    percent change; for example a value of 0.1 indicates a 10 percent increase in detection."),
                                                  h4("Second map"),
                                                  p("The second map displays confidence
                                                    in the estimated trend. Darker green counties in the second map reflect higher confidence values and indicate a 
                                                    greater confidence in the trend being detected.
                                                    A county that is dark orange in the first map and dark green in the second map would be a county with increasing
                                                    detection of SARS2 RNA for which we are highly confident. A dark orange county in the first map with a lighter
                                                    green in the second map would indicate an increasing trend in detection but we are less confident in the
                                                    observation.")
                                                  
                                                  
                       ), #add description here 
                       
                       column(6, 
                              #h3("Project information"), #description title
                              h4("Detection trend graph"),
                              p("In the data summary tab, there is a graph displaying the change in wastewater detection of SARS2 RNA over a two week period.
                                                    This graph displays the ratio of SARS2 to crassphage detected. This allows for adjustment based on population
                                                    so that lower levels of detection indicate fewer shedding events. As detection levels
                                                    rise, the trend line will also rise indicating increases in detection."),
                              h4("Key terms"),
                              p("Catchment - a region within a wastewater treatment plants service area. Sample point is
                                         either a manhole or a pump station."),
                              p("Crassphage - bacteria commonly excreted from humans which is used to determine the relative
                                         level of SARS2 RNA in the wastewater. The ratio of crassphage to SARS2 helps estimate if there
                                         is small amount of SARS2 or if detection levels indicate greater infection in the population."),
                              p("WWTP - wastewater treatment plant")
                              
                              
                       )
                       
                       ),
                       tabPanel("Data summary", splitLayout(cellWidths = c("33%", "33%", "33%"), leafletOutput("countyBetaMap"),
                                                            leafletOutput("countyR2Map"), plotOutput("countyPlot"))
                       ),
                       tabPanel("Project information", 
                                column(6,
                                       h4("Project description"),
                                       p("We began the SARS2 early warning wastewater surveillance platform
                                                    in early March of 2020. shortly after 
                                                    Medema and colleagues documented the feasibility of  tracking 
                                                    coronavirus transmission through wastewater. Participating  
                                                    counties and wastewater treatment plants provide wastewater 
                                                    samples  weekly or semi-weekly (twice per week), lab analyses are
                                                    conducted  at SUNY Upstate, and trends and maps are provided back
                                                    to the  county to guide response to the coronavirus pandemic. 
                                                    This project is a collaboration between Syracuse University, SUNY ESF,
                                                    SUNY Upstate, Quadrant Biosciences, Arcadis, and the New York State
                                                    Department of Health.")
                                ),
                                #h3("Additional information"), #title
                                #a("youtube video"),   #hyperlink for youtube video
                                #br(),),
                                
                                column(6,
                                       h4("Contact information"),
                                       p("To get in touch with this project or if you have questions regarding results contact
                                          David Larsen at dalarsen@syr.edu")
                                )
                       )
                       #tabPanel("Sewershed", plotOutput("syrPlot"))
  ))
)

#Make color ramp
pal <- colorNumeric(c("darkblue", "white", "darkorange"), 1:3)
ramp <- colorRamp(pal(c(1,2,3)), interpolate="spline")

#pal1 <- colorNumeric(c("yellow", "white", "darkgreen"), 1:3)
#ramp1 <- colorRamp(pal(c(1,2,3)), interpolate="spline")


server <- function(input, output, session) {
  ##
  # first panel map ny betas
  ##
  output$NYBetaMap <- renderLeaflet({
    #ny map of the betas
    pal <- colorNumeric(
      palette = ramp,
      domain = c(-1,1)) #ny_counties$TwoWeekBet)#
    
    leaflet(data = ny_counties, options = leafletOptions(zoomControl = TRUE, maxZoom = 8, minZoom = 5,
                                                         dragging = TRUE)) %>% 
      #addTiles(group = "base") %>% 
      addPolygons(data = ny_counties,
                  smoothFactor = 0.2, fillOpacity = 0.7,
                  fillColor = ~pal(TwoWeekBet),
                  stroke = TRUE,
                  color = "black",
                  weight = 1,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "black",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  layerId = ny_counties$County,
                  label = ~paste( ny_counties$County, round(TwoWeekBet, 2)),
                  group = "One week") %>%
      addLegend(position = c("bottomright"), pal = pal, 
                values = c(-1,1),#~TwoWeekBet,
                opacity = 0.7, #na.label = "NA",
                title = "Alert Level")
    
  })  
  ##
  # second panel map NY R2 values
  ##
  output$NYR2Map <- renderLeaflet({
    
    pal <- colorNumeric(
      palette = c("Greens"), 
      domain = c(0,1))
    
    leaflet(data = ny_counties, options = leafletOptions(zoomControl = TRUE, maxZoom = 8, minZoom = 5,
                                                         dragging = TRUE)) %>% 
      #addTiles() %>% 
      addPolygons( 
        smoothFactor = 0.2, fillOpacity = 0.7,
        fillColor = ~pal(TwoWeekR2),
        stroke = TRUE,
        color = "black",
        weight = 1,
        highlight = highlightOptions(
          weight = 5,
          color = "black",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste( ny_counties$County, round(TwoWeekR2, 2)),
        layerId = ny_counties$County,
        group = "1") %>%
      addLegend(position = c("bottomright"), pal = pal, 
                values = c(0,1),#~TwoWeekR2, 
                opacity = 0.7, #na.label = "NA",
                title = "Confidence"
      ) 
  }) 
  
  ##
  # open tab on click 
  ##
  observeEvent(input$NYBetaMap_shape_click,{
    updateTabsetPanel(session, "tabs",
                      selected = "Data summary")
  })
  
  
  observeEvent(input$NYBetaMap_shape_click, {
    #capture the info of the clicked polygon
    click <- input$NYBetaMap_shape_click
    
    #obtain wwtp catchments
    county.catchment.click <- wwtp.map %>%
      filter(County == click$id)
    #stop if empty df
    req(!is.na(county.catchment.click$WWTP_ID))
    
    zip.catchment.click <- zip_code %>%
      filter(County == click$id)
    
    #transform
    zip.catchment.click <- st_transform(
      zip.catchment.click,
      crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    )
    county.catchment.click <- st_transform(
      county.catchment.click,
      crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    )
    pal <- colorNumeric(
      palette = ramp,
      domain = c(-1,1))
    Bounds <- as.list(st_bbox(county.catchment.click)) #get the bounds for the catchments
    # Find a center point for each zip code area
    zip.centers <- (sf::st_centroid(zip.catchment.click, byid = TRUE))
    zip.centers2 <- as.data.frame((st_coordinates(zip.centers)))
    zip.centers2 <- cbind(zip.centers2, zip.centers)
    labs <- as.list(zip.catchment.click$ZIP_CODE)
    
    
    #sewersheds within wwtp
    #sewersheds within wwtp
    sw.catchment.click <- sewershed.map %>%
      filter(County == click$id)
    #transform
    sw.catchment.click <- st_transform(
      sw.catchment.click, crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    )
    
    #sewershed map
    county_click_Beta <-leaflet(data = county.catchment.click,
                                options = leafletOptions(zoomControl = TRUE, maxZoom = 12, minZoom = 8,
                                                         dragging = TRUE)
                                
    ) %>%
      addMapPane("catchment", zIndex = 420) %>%
      addMapPane("MiniCatchments", zIndex = 425) %>%
      addMapPane("zip_codes", zIndex = 405)%>% 
      addMapPane("tiles", zIndex = 400)%>% 
      #addMapPane("tiles_under", zIndex = 400)%>%
      
      addPolygons(data = zip.catchment.click, stroke = TRUE, color = "purple", fillColor = "yellow", weight = "0.9",
                  group = "Zip codes", 
                  options = pathOptions(pane = "zip_codes")
      )%>%
      ## 
      # base map options
      ##
      #addTiles(group = "tiles1", options = pathOptions(pane = "tiles"))%>%      
      #addProviderTiles(providers$Stamen.Toner, group = "tiles1", 
      #                options = pathOptions(pane = "tiles")) %>%
      #addProviderTiles(providers$Esri.NatGeoWorldMap, group = "tiles1",
      #                options = pathOptions(pane = "tiles")) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Base map",
                       options = pathOptions(pane = "tiles")) %>%
      
      #addProviderTiles(providers$MtbMap, group = "tiles1",
      #                options = pathOptions(pane = "tiles")) %>%
      addProviderTiles(providers$Stamen.TonerLines, group = "Base map",
                       options = c(providerTileOptions(opacity = 0.35), pathOptions(pane = "tiles")))%>%
      addProviderTiles(providers$Stamen.TonerLabels,group = "Base map",
                       options = pathOptions(pane = "tiles")) %>%
      
      
      #addMarkers
      addLabelOnlyMarkers(data = zip.centers2,lng = zip.centers2$X, lat = zip.centers2$Y, 
                          label = lapply(labs, HTML),
                          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 0.5 , textsize='15px'),
                          group = "Zip codes",
                          options = pathOptions(pane = "zip_codes")) %>%
      addPolygons(smoothFactor = 0.2, fillOpacity = 0.7,
                  fillColor = ~pal(TwoWeekBet),
                  stroke = TRUE,
                  color = "black",
                  weight = 2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "black",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  layerId = county.catchment.click$SW_ID,
                  group = "WWTP",
                  options = pathOptions(pane = "catchment"),
                  label = ~paste( county.catchment.click$WWTP, round(TwoWeekBet, 2)),
                  labelOptions = labelOptions(noHide = FALSE)) %>%
      addLegend(position = c("bottomright"), pal = pal, 
                values = c(-1,1),#~TwoWeekBet, 
                opacity = 0.7,
                title = "Alert level",
                group = "Legend")%>%
      addPolygons(data = sw.catchment.click,
                  smoothFactor = 0.2, fillOpacity = 0.7,
                  fillColor = ~pal(TwoWeekBet),
                  stroke = TRUE,
                  color = "black",
                  weight = 2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "black",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  layerId = sw.catchment.click$SW_ID,
                  group = "Catchments",
                  options = pathOptions(pane = "MiniCatchments"),
                  label = ~paste( sw.catchment.click$Sewershed, round(TwoWeekBet, 2)),
                  labelOptions = labelOptions(noHide = FALSE)) %>%
      fitBounds(~Bounds$xmin, ~Bounds$ymin, ~Bounds$xmax, ~Bounds$ymax) %>%
      #addControl(actionButton("zoomer","Reset"),position="bottomleft") %>% # optional reset button, does not seem to work currently
      # Layers control
      addLayersControl(
        #baseGroups = c("zip"),
        overlayGroups = c("WWTP", "Catchments", "Zip codes", "Base map", "Legend"),
        options = layersControlOptions(collapsed = FALSE,autoZIndex = TRUE)
        
      )%>%
      hideGroup("Zip codes") %>%
      #hideGroup("Catchments") %>%
      hideGroup("Legend")
    
    
    
    #if click id isn't null render the table
    if(!is.na(click$id)){
      output$countyBetaMap = renderLeaflet({
        county_click_Beta
      })
    }
  })
  
  #county R2 map
  observeEvent(input$NYBetaMap_shape_click, {
    #capture the info of the clicked polygon
    click <- input$NYBetaMap_shape_click
    #obtain wwtp catchments
    county.catchment.click <- wwtp.map %>%
      filter(County == click$id)
    #stop if empty df
    req(!is.na(county.catchment.click$WWTP_ID))
    pal <- colorNumeric(
      palette = c("Greens"), 
      domain = c(0,1))
    zip.catchment.click <- zip_code %>%
      filter(County == click$id)
    
    #transform
    zip.catchment.click <- st_transform(
      zip.catchment.click,
      crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    )
    county.catchment.click <- st_transform(
      county.catchment.click,
      crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    )
    #plot the county
    
    Bounds <- as.list(st_bbox(county.catchment.click)) #get the bounds for the catchments
    # Find a center point for each zip code area
    zip.centers <- (sf::st_centroid(zip.catchment.click, byid = TRUE))
    zip.centers2 <- as.data.frame((st_coordinates(zip.centers)))
    zip.centers2 <- cbind(zip.centers2, zip.centers)
    labs <- as.list(zip.catchment.click$ZIP_CODE)
    #mini catchments
    sw.catchment.click <- sewershed.map %>%
      filter(County == click$id)
    #transform
    sw.catchment.click <- st_transform(
      sw.catchment.click, crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    )
    
    #sewershed map
    county_click_R2 <-leaflet(data = county.catchment.click,
                              options = leafletOptions(zoomControl = TRUE, maxZoom = 12, minZoom = 8,
                                                       dragging = TRUE)
                              
    ) %>%
      addMapPane("catchment", zIndex = 420) %>%
      addMapPane("MiniCatchments", zIndex = 425) %>%
      addMapPane("zip_codes", zIndex = 405)%>%
      addMapPane("tiles", zIndex = 400) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Base map",
                       options = pathOptions(pane = "tiles")) %>%
      addProviderTiles(providers$Stamen.TonerLines, group = "Base map",
                       options = c(providerTileOptions(opacity = 0.35), pathOptions(pane = "tiles")))%>%
      addProviderTiles(providers$Stamen.TonerLabels,group = "Base map",
                       options = pathOptions(pane = "tiles")) %>%
      
      
      
      addPolygons(data = zip.catchment.click, stroke = TRUE, color = "purple", fillColor = "yellow", weight = "0.9",
                  group = "Zip codes", options = pathOptions(pane = "zip_codes")
      )%>%
      #addMarkers
      addLabelOnlyMarkers(data = zip.centers2,lng = zip.centers2$X, lat = zip.centers2$Y, 
                          label = lapply(labs, HTML),
                          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 0.5 , textsize='15px'),
                          group = "Zip codes",
                          options = pathOptions(pane = "zip_codes")) %>%
      addPolygons(smoothFactor = 0.2, fillOpacity = 0.7,
                  fillColor = ~pal(TwoWeekR2),
                  stroke = TRUE,
                  color = "black",
                  weight = 2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "black",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  layerId = county.catchment.click$WWTP_ID,
                  group = "WWTP",
                  options = pathOptions(pane = "catchment"),
                  label = ~paste( county.catchment.click$WWTP, round(TwoWeekR2, 2)),
                  labelOptions = labelOptions(noHide = FALSE)) %>%
      addLegend(position = c("bottomright"), pal = pal, 
                values = c(0,1), opacity = 0.7,
                title = "Confidence",
                group = "Legend")%>%
      addPolygons(data = sw.catchment.click,
                  smoothFactor = 0.2, fillOpacity = 0.7,
                  fillColor = ~pal(TwoWeekR2),
                  stroke = TRUE,
                  color = "black",
                  weight = 2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "black",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  layerId = sw.catchment.click$SW_ID,
                  group = "Catchments",
                  options = pathOptions(pane = "MiniCatchments"),
                  label = ~paste( sw.catchment.click$Sewershed, round(TwoWeekR2, 2)),
                  labelOptions = labelOptions(noHide = FALSE)) %>%
      fitBounds(~Bounds$xmin, ~Bounds$ymin, ~Bounds$xmax, ~Bounds$ymax) %>%
      #addControl(actionButton("zoomer","Reset"),position="bottomleft") %>% # optional reset button, does not seem to work currently
      # Layers control
      addLayersControl(
        #baseGroups = c("zip"),
        overlayGroups = c("WWTP", "Catchments", "Zip codes", "Base map", "Legend"),
        options = layersControlOptions(collapsed = FALSE,autoZIndex = TRUE)
        
      )%>%
      hideGroup("Zip codes") %>%
      #hideGroup("Catchments") %>%
      hideGroup("Legend")
    
    
    
    #if click id isn't null render the table
    if(!is.na(click$id)){
      output$countyR2Map = renderLeaflet({
        county_click_R2
      })
      
      
    }
  })
  
  ##
  # county ggplot
  ##
  observeEvent(input$NYBetaMap_shape_click, {
    #capture the info of the clicked polygon
    click <- input$NYBetaMap_shape_click
    county_plot <- wastewater.county %>%
      filter(County == click$id)%>%     #replace with click action
      group_by(County, Date_collected) %>%
      filter(!duplicated(County))
    #stop if empty df
    req(!is.na(county_plot$weighted_ratio))
    
    
    #simple ggplot then turn to ggplotly
    county_plot$Date_collected <- ymd(county_plot$Date_collected) 
    #simple ggplot then turn to ggplotly
    county_click_2 <- ggplot(data = county_plot, aes(x = Date_collected, y = weighted_ratio )) + #scaled varialbe, unscaled name is weighted_ratio; scaled Scale_weightedRatio
      geom_line() +
      #ylab("Percent change in Crassphage\nadjusted levels of\nSARS-CoV-2 RNA in wastewater")+ #for scaled foars
      ylab("Crassphage adjusted levels of\nSARS-CoV-2 RNA in wastewater")+ #for unscaled
      xlab("") +
      ggtitle(paste(county_plot$County, "County detection trend", sep = " ")) +
      scale_y_continuous(limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.1)))
    #ylim(0,1) #for scaled variables
    
    
    
    if(!is.na(click$id)){
      output$countyPlot = renderPlot(
        county_click_2
      )
    }
    
  })
  
  
  
  
  
  
  ##
  # sewershed ggplot
  ##
  observeEvent(input$countyBetaMap_shape_click, {
    #capture the info of the clicked polygon
    click <- input$countyBetaMap_shape_click
    sewershed_plot <- wastewater.sewersheds %>%
      filter(SW_ID == click$id)%>%     #replace with click action
      #filter(Method == "Influent")%>%
      group_by(SW_ID, Date_collected)
    
    #simple ggplot then turn to ggplotly
    sewershed_plot$Date_collected <- ymd(sewershed_plot$Date_collected) 
    #simple ggplot then turn to ggplotly
    county_click_4 <- ggplot(data = sewershed_plot, aes(x = Date_collected, y =ratiocDNA)) + #scaled variable, unscaled value is ratiocDNA; scaled  Scale_ratiodna
      geom_line() +
      #ylab("Percent change in Crassphage\nadjusted levels of\nSARS-CoV-2 RNA in wastewater")+ #scaled variables
      ylab("Crassphage adjusted levels of\nSARS-CoV-2 RNA in wastewater")+ #for unscaled
      xlab("") +
      ggtitle(paste(sewershed_plot$Sewershed, "detection trend", sep = " ")) +
      scale_y_continuous(limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.1)))
    #ylim(0,1) #for scaled variables
    
    
    
    
    if(!is.null(click$id)){
      output$countyPlot = renderPlot(
        county_click_4
      )
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)