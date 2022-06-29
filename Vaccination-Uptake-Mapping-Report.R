#remotes::install_github("r-spatial/mapview") --as advised by Stuart (there are issues in the CRAN release for html output)
#install.packages('xml2')
#install.packages('npm i leaflet.snogylop')
#install.packages('maps')
#install.packages("rsvg")
#install.packages("leaflegend")
#install.packages('leaflet.extras')
#install.packages('SpatialKDE')
#install.packages('Rcpp')
#install.packages('spatialEco')
#tinytex::tlmgr_update() ## not sure if required
#install.packages("Rtools") ## not sure if required
#install.packages("tlmgr") ## not sure if required
#install.packages("tinytex") ## not sure if required 
#install.packages("latexpdf") ## not sure if required
#tinytex::install_tinytex() ## this is necessary to use LaTex and create an RMarkdown document in PDF

#tinytex::tlmgr_update() ## This may only be required by Michael
library(classInt)
library(colorspace)
library(cowplot)
library(DBI)
library(devtools)
library(dplyr)
library(ggmap)
library(ggplot2)
library(grid)
library(gridExtra)
library(gridGraphics)
library(gridtext)
library(gt)
library(heatmaply)
library(htmltools)
library(htmlwidgets)
library(knitr)
library(latexpdf)
library(leaflegend)
library(leaflet)
library(leaflet.extras)
library(leafsync)
library(magick)
library(magrittr)
library(maps)
library(maptools)
library(mapview)
library(odbc)
library(osmdata) #openstreetmaps
library(png)
library(raster)
library(RColorBrewer)
library(Rcpp)
library(readr)
library(rgdal)
library(rgeos)
library(rmarkdown)
library(RPostgreSQL)
library(RSQLite)
library(rsvg)
library(sf)
library(sp)
library(spatialEco)
library(SpatialKDE)
library(spdplyr)
library(tidyverse)
library(tinytex)
library(tmap)
library(tmaptools)
library(webshot)
library('xml2')



#############################################################
#Set working directory and Ghostscript file location
#Set the projection and check that the spatial layers required are available to R
#If you are given a Rcpp error then follow the instructions on the following link:
#https://stackoverflow.com/questions/68416435/rcpp-package-doesnt-include-rcpp-precious-remove
#If that doesn't work try this:
#https://stackoverflow.com/questions/30308639/r-cran-install-library-rcpp-fails-after-r3-2-upgrade
#############################################################

setwd("G:/xxx")
#tools::find_gs_cmd() #check if R can find this file
#We are looking for the following file: 'gswin64.exe'
#Use windows explorer to search for its location on the c drive and then set it with the code below
Sys.setenv(R_GSCMD="G:/ghostscript/gswin64.exe") #very important!!! Need this for formatting!!
tools::find_gs_cmd() #check that R can now find the file location

#Set the projection - Projection is British National Grid but leaflet only accepts WGS84
wgs84 = '+proj=longlat +datum=WGS84'
#ogrListLayers(myMSSQLdsn) 

#############################################################
#Get the data for the maps
#############################################################

#City boundary
NottsCityLSOA<- readOGR(dsn='MSSQL:server=xxx;database=xxx;trusted_connection=true', layer='vw_NottinghamCity_lsoa_2011')
#City uptake data
NottsCityLSOA_Vacs_Uptake <- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='NottinghamshireAndCity_lsoa_2011_VaccineUptake_vs_PharmSites')
NottsCityLSOA_Vacs_Uptake2 <-NottsCityLSOA_Vacs_Uptake %>% 
  filter(ICP=="Nottingham City ICP")
#reproject for leaflet
NottsCityLSOA_Vacs_Uptake_wgs84 <- spTransform(NottsCityLSOA_Vacs_Uptake2, CRS(wgs84))
#plot(NottsCityLSOA_Vacs_Uptake_wgs84)
#view(NottsCityLSOA_Vacs_Uptake)

NottinghamshireLSOA<- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='vw_NottinghamshireExcludingBassetlaw_lsoa_2011')
#Join together LSOA shapes and the vaccine uptake data
NottinghamshireLSOA_Vacs_Uptake <- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='NottinghamshireAndCity_lsoa_2011_VaccineUptake_vs_PharmSites')
NottinghamshireLSOA_Vacs_Uptake2 <- NottinghamshireLSOA_Vacs_Uptake%>%
filter(ICP %in% c("South Notts ICP","Mid Notts ICP","Nottingham City ICP"))
#reproject for leaflet
NottinghamshireLSOA_Vacs_Uptake_wgs84 <- spTransform(NottinghamshireLSOA_Vacs_Uptake2, CRS(wgs84))
#plot(NottinghamshireLSOA_Vacs_Uptake_wgs84)

#Get vaccination sites
VacSites<- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='VaccinationSitesICS')
#filter vac sites to current sites only
VacSites2<-VacSites %>% 
filter(CurrentSite == 1)  
#Transform so that leaflet can deal with it
VacSites_wgs84 <- spTransform(VacSites2, CRS(wgs84))
VacSites_wgs84_City<-VacSites_wgs84  %>% 
  filter(Region=="Nottingham City")
VacSites_wgs84_Nottinghamshire<-VacSites_wgs84  %>% 
  filter(Region %in% c("Nottingham City","Mid Notts","South Notts"))

#Get City boundary
NottsCity<- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='NottinghamCity')
#reproject for leaflet
NottsCity_wgs84 <- spTransform(NottsCity, CRS(wgs84))

#Get County boundary
NottsCounty<- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='NottinghamshireAndCity_ExcludingBassetlaw')
#reproject for leaflet
NottsCounty_wgs84 <- spTransform(NottsCounty, CRS(wgs84))

#Get Mid Notts boundary
MidNotts<- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='MidNotts')
#reproject for leaflet
MidNotts_wgs84 <- spTransform(MidNotts, CRS(wgs84))
#Get South Notts boundary
SouthNotts<- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='SouthNotts')
#reproject for leaflet
SouthNotts_wgs84 <- spTransform(SouthNotts, CRS(wgs84))

#Get inverted county boundary
NottsInverted<- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='NottinghamshireAndCity_ExcludingBassetlaw_Inverted')
#reproject for leaflet
NottsInverted_wgs84 <- spTransform(NottsInverted, CRS(wgs84))

#Get inverted city boundary
NottsCityInverted<- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='NottinghamCity_Inverted')
#reproject for leaflet
NottsCityInverted_wgs84 <- spTransform(NottsCityInverted, CRS(wgs84))

#Get boundaries >5 miles from a pharmacy vaccination site (this was created in QGIS then exported)

PharmacySites5Miles <- st_read("Areas5MilesPlusFromPharmacyVacSite.shp")
#st_crs(PharmacySites5Miles)

#Get CITY point file for kernel density mapping
NottsCityKD<- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='NottinghamshireAndCity_oa_2011_VaccineUptake_KD')
#reproject for leaflet
NottsCityKD_wgs84 <- spTransform(NottsCityKD, CRS(wgs84))

#Get COUNTY point file for kernel density mapping
NottsCountyKD<- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='NottinghamshireAndCity_oa_2011_VaccineUptake_KD_County')
#reproject for leaflet
NottsCountyKD_wgs84 <- spTransform(NottsCountyKD, CRS(wgs84))

#report date for report info
ReportDate<-as.Date(max(NottsCityLSOA_Vacs_Uptake2$"LoadDate"),format = "%Y/%m/%d")
Footer<-paste0("Data @ ", format(ReportDate, '%d %B, %Y'))
FileSuffix<- format(ReportDate, '_%d%b%Y')

#Get tram route
NCCTramRoute<- readOGR(dsn='MSSQL:server=xxxx;database=xxxx;trusted_connection=true', layer='ncc_tram_route')
#reproject for leaflet
NCCTramRoute_wgs84 <- spTransform(NCCTramRoute, CRS(wgs84))

#############################################################
#Add map legend and custom icon
#############################################################

#set font for the map legend
LegendFormat<-
  tags$style(
    ".leaflet .legend {
                 border: 1px solid black;
                 line-height: 12px;
                 font-size: 8px;
                 background: #fff;
                 #border-left:0.7px solid #666666;
                 #border-right:0.7px solid #666666;
                 #border-top:0.7px solid #666666;
                 #border-bottom:0.7px solid #666666;
                 }",
    ".leaflet .legend i{
                width: 10px;
                height: 10px;
                 }"
  )

#Add an SVG icon for the pharmacies - exported from QGIS into task folder
pharmacy_icon <- makeIcon("Pharmacy.svg",
# iconUrl = "https://image.flaticon.com/icons/svg/2633/2633816.svg", #example online icon
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 0, iconAnchorY = 0
)
#convert the svg to png for leaflet legend
#rsvg_png("Pharmacy.svg", "pharmacy_icon.png")

#############################################################
#Add maps - notts county
#############################################################

#Set jenks breaks
cols_classes <- classIntervals(NottinghamshireLSOA_Vacs_Uptake_wgs84$All_Vaccine1stDose_Blank_Rounded, n = 4, style = "jenks")
#cols_classes$brks

#Set colour palette - jenks
binpal_jenks <- colorBin("Reds",domain = cols_classes$brks, cols_classes$brks, pretty = FALSE,na.color = "grey")

#Custom legend for areas >5 miles from vac site
# set legend features
colors <- c("white","#33A02C")
labels <- c("Areas >5 Miles from Vaccination Sites","NCC Tram Route")
sizes <- c(8,8)
heights <- c(8,3)
shapes <- c("square","pill shape")
borders <- c("#FF0000","white")

addLegendCustom <- function(map_County, colors, labels, sizes, shapes, borders, opacity = 1){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", heights, "px; border:2px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  return(addLegend(map_County, colors = legend_colors, labels = legend_labels, opacity = opacity,position="bottomright"))
  
}

#Custom legend for pharmacy site svg icon does not work in legend as requires server upload before the icons show

factorPal <- colorFactor(palette = c("#33a02c","white"),
                         levels=c(str_replace(VacSites_wgs84_Nottinghamshire$CurrentSite,"1","Planned Vaccination Site"),"2"))


################################################################################
#Create map
map_County<-leaflet(data=NottinghamshireLSOA_Vacs_Uptake_wgs84,width = "665", height = "665",
                    options = leafletOptions(zoomControl = FALSE, #disables the zoom control
                                             minZoom = 3, maxZoom = 15,
                                             dragging = TRUE)) %>% 
  #Set View
  setView(lng = -1.005493550803352, lat = 53.025230893505338, zoom = 10) %>% #sets the zoom level and centers the map
  #base layers
  #addProviderTiles("Esri.WorldGrayCanvas", group = "Esri.WorldGrayCanvas") %>%
  addTiles()%>%
  addTiles(urlTemplate = "", attribution = 'Nottinghamshire GPRCC')%>%  
  #Add inverted boundary polygon for background masking
  addPolygons(data=NottsInverted_wgs84,weight = 0, color = "#FFFFFF",
              opacity = 1.0, fillOpacity = 0.6) %>%
  addScaleBar(position="bottomleft")%>% 
  #Vaccine Uptake Polygon
  addPolygons(weight = 0.8, color = "dimgray", smoothFactor = 0.3,
              group = "All_Vaccine1stDose_Blank_Rounded",
              label = ~paste0(~name, ": ", formatC("All_Vaccine1stDose_Blank_Rounded", big.mark = "'")),
              fillOpacity = 0.6, fillColor = ~binpal_jenks(All_Vaccine1stDose_Blank_Rounded)) %>%
  #County Boundaries
  addPolygons(data=SouthNotts_wgs84,weight = 3, color = "#008080",
              opacity = 1.0, fillOpacity = 0)%>%
  addPolygons(data=MidNotts_wgs84,weight = 3, color = "#008080",
              opacity = 1.0, fillOpacity = 0)%>%  
  #Add tram route
  addPolylines(data=NCCTramRoute_wgs84,color="#33A02C",opacity = 1.0,weight=1.8)%>%
  #Add vaccination sites
  addMarkers(data = VacSites_wgs84_Nottinghamshire, icon = pharmacy_icon,label=~SiteNumber,
             labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "right",offset = c(10, 0),
                                         style = list(
                                           "color" = "black",
                                           "font-family" = "arial black",
                                           "font-style" = "bold",
                                           "font-size" = "9px",
                                           "border-color" = "white")))  %>%
  #Add areas more than 5 miles from a vac site
  addPolygons(data=PharmacySites5Miles,weight = 2.5, color = "#FF0000",
              opacity = 1.0, fillOpacity = 0)%>% 
  #Add scalebar
  addScaleBar(position="bottomleft",options=scaleBarOptions(imperial=TRUE,metric=FALSE))%>%
  #Add control
  addControl(html=LegendFormat, position="bottomright",className = "LegendFormat")%>%
  #Add custom legend for other layers
  addLegendCustom(colors,labels, sizes, shapes,borders)%>%
  #Add unvaccinated totals legend
  addLegend(title="Total Unvaccinated by LSOA",position="bottomright",
          opacity = 1, pal=binpal_jenks,values = ~All_Vaccine1stDose_Blank_Rounded)%>%
  #Add vaccination site legend
  addLegendFactor(pal= factorPal,
  labelStyle = 'font-size: 8px; font-weight: normal;',
  values = str_replace(VacSites_wgs84_Nottinghamshire$CurrentSite,"1","Planned Vaccination Site"),
  position="bottomright",
  shape = "plus",
  orientation ="vertical",
  width = 10,
  height = 10) 

map_County


saveWidget(map_County, "map_County.html", selfcontained = TRUE)
webshot("map_County.html", file = "map_County10.png", 
        vwidth = 665, vheight = 665)


#############################################################
#Add map - City
#############################################################

#Use same colour palette as county map (to mirror the QGIS version)

#Create map
map_City<-leaflet(data=NottsCityLSOA_Vacs_Uptake_wgs84,width = "665", height = "665",
             options = leafletOptions(zoomControl = FALSE, #disables the zoom control
                                      minZoom = 3, maxZoom = 15,
                                      dragging = TRUE)) %>% 
  #Set View
  setView(lng = -1.1665341865239809, lat = 52.953860154064664, zoom = 11.5) %>% #sets the zoom level and centers the map
  #base layers
  #addProviderTiles("Esri.WorldGrayCanvas", group = "Esri.WorldGrayCanvas") %>%
  addTiles()%>%
  addTiles(urlTemplate = "", attribution = 'Nottinghamshire GPRCC')%>%  
  #Add inverted boundary polygon for background masking
  addPolygons(data=NottsCityInverted_wgs84,weight = 0, color = "#FFFFFF",
              opacity = 1.0, fillOpacity = 0.6) %>%
  addScaleBar(position="bottomleft")%>% 
  #Vaccine Uptake Polygon
  addPolygons(weight = 0.8, color = "dimgray", smoothFactor = 0.3,
              group = "All_Vaccine1stDose_Blank_Rounded",
              label = ~paste0(~name, ": ", formatC("All_Vaccine1stDose_Blank_Rounded", big.mark = "'")),
              fillOpacity = 0.6, fillColor = ~binpal_jenks(All_Vaccine1stDose_Blank_Rounded)) %>%
  #City Boundary
  addPolygons(data=NottsCity_wgs84,weight = 3, color = "#008080",
              opacity = 1.0, fillOpacity = 0)%>%
  #Add tram route
  addPolylines(data=NCCTramRoute_wgs84,color="#33A02C",opacity = 1.0,weight=1.8)%>%
  #Add vaccination sites
  addMarkers(data = VacSites_wgs84_City, icon = pharmacy_icon,label=~SiteNumber,
             labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "right",offset = c(10, 0),
                                         style = list(
                                        "color" = "black",
                                        "font-family" = "arial black",
                                        "font-style" = "bold",
                                        "font-size" = "9px",
                                        "border-color" = "white")))  %>%
  #Add scalebar
  addScaleBar(position="bottomleft",options=scaleBarOptions(imperial=TRUE,metric=FALSE))%>%
  addControl(html=LegendFormat, position="bottomright",className = "LegendFormat")%>%
  #Add custom legend for other layers
  addLegendCustom(colors,labels, sizes, shapes,borders)%>%
  #Add unvaccinated totals legend
  addLegend(title="Total Unvaccinated by LSOA",position="bottomright",
            opacity = 1, pal=binpal_jenks,values = ~All_Vaccine1stDose_Blank_Rounded)%>%
  #Add vaccination site legend
  addLegendFactor(pal= factorPal,
                labelStyle = 'font-size: 8px; font-weight: normal;',
                values = str_replace(VacSites_wgs84_Nottinghamshire$CurrentSite,"1","Planned Vaccination Site"),
                position="bottomright",
                shape = "plus",
                orientation ="vertical",
                width = 10,
                height = 10) 
map_City


saveWidget(map_City, "map_City.html", selfcontained = TRUE)  
webshot("map_City.html", file = "map_City11_5.png", 
        vwidth = 665, vheight = 665)


#############################################################
#Add City kernel density maps
#############################################################

#Need to create a raster of density, weighted to unvaccinated populations by OA
#This layer was previously created in QGIS via the Heatmap addin

#Add inverted polygon extent
extent_info_wgs84_NCI <- extent(bbox(NottsCityInverted_wgs84))
extent_wgs84_NCI <- c(xmin=-1.33, xmax=-1, ymin=52.8, ymax=53.1) #GEOGRAPHY

unvaccinated.kde <- sp.kde(x = NottsCityKD_wgs84, y = NottsCityKD_wgs84$All_Vaccine1stDose_Blank_Rounded, 
                           bw = .01, 
                           nr = 665, nc = 665, 
                           newdata = extent_wgs84_NCI, 
                           standardize = TRUE
)
plot(unvaccinated.kde)

#crop to city boundary
unvaccinated.kde_mask <- mask(unvaccinated.kde, NottsCity_wgs84)

#output kde to raster
writeRaster(unvaccinated.kde_mask, filename="NottsCityKD.tif", format="GTiff", overwrite=TRUE)

#Get new raster layer
CityRaster_wgs84 <- raster("NottsCityKD.tif")
#Create contour lines
CityRaster_wgs84_ContourLines <- rasterToContour(CityRaster_wgs84,nlevels=9)
#plot(CityRaster_wgs84_ContourLines, add=TRUE)

rasterpal <- colorNumeric(c("#FFF5F0", "#FEE0D2", "#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D","black"), values(CityRaster_wgs84),
                    na.color = "transparent")


#Build Kernel Density Estimates Map

map_CityKD<-leaflet(data=NottsCityLSOA_Vacs_Uptake_wgs84,width = "665", height = "665",
                    options = leafletOptions(zoomControl = FALSE, #disables the zoom control
                                             minZoom = 3, maxZoom = 15,
                                             dragging = TRUE)) %>% 
  #Set View
  setView(lng = -1.1665341865239809, lat = 52.953860154064664, zoom = 11.5) %>% #sets the zoom level and centers the map
  #base layers
  addTiles()%>%
  addTiles(urlTemplate = "", attribution = 'Nottinghamshire GPRCC')%>%  
  #Add inverted boundary polygon for background masking
  addPolygons(data=NottsCityInverted_wgs84,weight = 0, color = "#FFFFFF",
              opacity = 1.0, fillOpacity = 0.6) %>%
  addScaleBar(position="bottomleft")%>% 
  #City Boundary
  addPolygons(data=NottsCity_wgs84,weight = 3, color = "#008080",
              opacity = 1.0, fillOpacity = 0)%>%
  #Add tram route
  addPolylines(data=NCCTramRoute_wgs84,color="#33A02C",opacity = 1.0,weight=1.8)%>%
  #Add vaccination sites
  addMarkers(data = VacSites_wgs84_City, icon = pharmacy_icon,label=~SiteNumber,
             labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "right",offset = c(10, 0),
                                         style = list(
                                           "color" = "black",
                                           "font-family" = "arial black",
                                           "font-style" = "bold",
                                           "font-size" = "9px",
                                           "border-color" = "white")))  %>%
  #Add scalebar
  addScaleBar(position="bottomleft",options=scaleBarOptions(imperial=TRUE,metric=FALSE))%>%
  addControl(html=LegendFormat, position="bottomright",className = "LegendFormat")%>%
  #Add custom legend for other layers
  addLegendCustom(colors,labels, sizes, shapes,borders)%>%
  #Add KDE legend
  addLegend(pal = rasterpal, values = values(CityRaster_wgs84),
            title = "Unvaccinated People 16Plus Density",position= "bottomright")%>%
  #Add vaccination site legend
  addLegendFactor(pal= factorPal,
                  labelStyle = 'font-size: 8px; font-weight: normal;',
                  values = str_replace(VacSites_wgs84_Nottinghamshire$CurrentSite,"1","Planned Vaccination Site"),
                  position="bottomright",
                  shape = "plus",
                  orientation ="vertical",
                  width = 10,
                  height = 10) %>%
  #Add KDE raster image
  addRasterImage(CityRaster_wgs84,colors=rasterpal,opacity=0.7)%>%
  #Add contour lines from raster
  addPolylines(data=CityRaster_wgs84_ContourLines,color="#555555",opacity = 0.6,weight=0.7)

map_CityKD

saveWidget(map_CityKD, "map_CityKD.html", selfcontained = TRUE)  
webshot("map_CityKD.html", file = "map_CityKD11_5.png", 
        vwidth = 665, vheight = 665)


#############################################################
#Add County kernel density maps
#############################################################

#Need to create a raster of density, weighted to unvaccinated populations by OA
#This layer was previously created in QGIS via the Heatmap addin

#Add inverted polygon extent
extent_info_wgs84_NCTI <- extent(bbox(NottsInverted_wgs84))
extent_wgs84_NCTI <- c(xmin=-1.85, xmax=-0.193, ymin=52.6, ymax=53.5) #GEOGRAPHY

unvaccinated.kde_county <- sp.kde(x = NottsCountyKD_wgs84, y = NottsCountyKD_wgs84$All_Vaccine1stDose_Blank_Rounded, 
                           bw = .01, 
                           nr = 665, nc = 665, 
                           newdata = extent_wgs84_NCTI, 
                           standardize = TRUE
                           #,Scale.factor = 10000  
)
plot(unvaccinated.kde_county)


#crop to County boundary
unvaccinated.kde_mask_county <- mask(unvaccinated.kde_county, NottsCounty_wgs84)

#output kde to raster
writeRaster(unvaccinated.kde_mask_county, filename="NottsCountyKD.tif", format="GTiff", overwrite=TRUE)

#Get new raster layer
CountyRaster_wgs84 <- raster("NottsCountyKD.tif")
#Create contour lines
CountyRaster_wgs84_ContourLines <- rasterToContour(CountyRaster_wgs84,nlevels=9)
#plot(CountyRaster_wgs84_ContourLines, add=TRUE)

rasterpal_county <- colorNumeric(c("#FFF5F0", "#FEE0D2", "#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D","black"), values(CountyRaster_wgs84),
                          na.color = "transparent")


#Build Kernel Density Estimates Map

map_CountyKD<-leaflet(data=NottinghamshireLSOA_Vacs_Uptake_wgs84,width = "665", height = "665",
                    options = leafletOptions(zoomControl = FALSE, #disables the zoom control
                                             minZoom = 3, maxZoom = 15,
                                             dragging = TRUE)) %>% 
  #Set View
  setView(lng = -1.005493550803352, lat = 53.025230893505338, zoom = 10) %>% #sets the zoom level and centers the map
  #base layers
  addTiles()%>%
  addTiles(urlTemplate = "", attribution = 'Nottinghamshire GPRCC')%>%  
  #Add inverted boundary polygon for background masking
  addPolygons(data=NottsInverted_wgs84,weight = 0, color = "#FFFFFF",
              opacity = 1.0, fillOpacity = 0.6) %>%
  addScaleBar(position="bottomleft")%>% 
  #County Boundaries
  addPolygons(data=SouthNotts_wgs84,weight = 3, color = "#008080",
              opacity = 1.0, fillOpacity = 0)%>%
  addPolygons(data=MidNotts_wgs84,weight = 3, color = "#008080",
              opacity = 1.0, fillOpacity = 0)%>%  
  #Add areas more than 5 miles from a vac site
  addPolygons(data=PharmacySites5Miles,weight = 2.5, color = "#FF0000",
              opacity = 1.0, fillOpacity = 0)%>% 
  #Add tram route
  addPolylines(data=NCCTramRoute_wgs84,color="#33A02C",opacity = 1.0,weight=1.8)%>%
  #Add vaccination sites
  addMarkers(data = VacSites_wgs84_Nottinghamshire, icon = pharmacy_icon,label=~SiteNumber,
             labelOptions = labelOptions(noHide = T, textOnly = TRUE,direction = "right",offset = c(10, 0),
                                         style = list(
                                           "color" = "black",
                                           "font-family" = "arial black",
                                           "font-style" = "bold",
                                           "font-size" = "9px",
                                           "border-color" = "white")))  %>%
  #Add scalebar
  addScaleBar(position="bottomleft",options=scaleBarOptions(imperial=TRUE,metric=FALSE))%>%
  addControl(html=LegendFormat, position="bottomright",className = "LegendFormat")%>%
  #Add custom legend for other layers
  addLegendCustom(colors,labels, sizes, shapes,borders)%>%
  #Add KDE legend
  addLegend(pal = rasterpal_county, values = values(CountyRaster_wgs84),
            title = "Unvaccinated People 16Plus Density",position= "bottomright")%>%
  #Add vaccination site legend
  addLegendFactor(pal= factorPal,
                  labelStyle = 'font-size: 8px; font-weight: normal;',
                  values = str_replace(VacSites_wgs84_Nottinghamshire$CurrentSite,"1","Planned Vaccination Site"),
                  position="bottomright",
                  shape = "plus",
                  orientation ="vertical",
                  width = 10,
                  height = 10) %>%
  #Add KDE raster image
  addRasterImage(CountyRaster_wgs84,colors=rasterpal_county,opacity=0.7)%>%
  #Add contour lines from raster
  addPolylines(data=CountyRaster_wgs84_ContourLines,color="#555555",opacity = 0.6,weight=0.7)

map_CountyKD

saveWidget(map_CountyKD, "map_CountyKD.html", selfcontained = TRUE)  
webshot("map_CountyKD.html", file = "map_CountyKD11_5.png", 
        vwidth = 665, vheight = 665)

#############################################################
#Add table of vaccination sites
#############################################################

df<-as.data.frame(VacSites_wgs84_Nottinghamshire) 
df2<-subset(df, select = c(SiteNumber,Type,Region,VaccinationSite,PfizerSite) )%>%
arrange(SiteNumber)

gt_table <- gt(df2)

gt_table%>%
gtsave("SitesTable.png", expand = 10)


#############################################################
#Create final report via an rmd
#The rmd can be adjusted to add notes under each map if required
#############################################################

#process the output file into a report via the rmd
render("InequalitiesMeetingFortnightlyMapsReport.Rmd", 
       output_file = paste0("InequalitiesMeetingFortnightlyMapsReport",FileSuffix,".pdf"))


