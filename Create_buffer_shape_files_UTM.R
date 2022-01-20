## ----load-data-------------------------------------------------------
# load the sp and rgdal packages
# Install rgeos package if needed
#----------------------------------------------------------------------
library(sp)
library(rgdal)
#----------------------------------------------------------------------
# Download example file here at https://alabama.box.com/s/twjrmk4yz4z9bopdoolnpvloqrgbhvvs
#----------------------------------------------------------------------
long2utmzone <- function(longitude){
  "
  Function to get UTM zone given longitude in decimal degrees
  "
  s1 <- longitude + 180
  s2 <- s1/6
  utm_zone <- ceiling(s2)
  return(utm_zone)
}
LongLatToUTM<-function(df){
  "
  Function to convert latitude and longitude points to UTM
  Arguments:
    df: DataFrame containing at least three columns with names 'Site_ID', 'nlat', and 'elon' 
    Site_ID: Site ID (any name of your location)
    nlat: latitude ([-90 90] decimal degrees, e.g., 15 in northern hemisphere, and -15 in southern hemisphere)
    elong: longitude ([-180 180] decimal degree)
  "
  if(!('Site_ID' %in% names(df)) | !('nlat' %in% names(df)) | !('elon' %in% names(df))){
    stop("Dataframe not formatted with proper column names! Provide at least three columns with names 'Site_ID', 'nlat', and 'elon'!")
  }
  zone <- long2utmzone(df$elon)
  out <- data.frame(matrix(ncol = ncol(df), nrow = 0))
  colnames(out) <- names(df)
  for(i in 1: nrow(df)){
    temp <- df[i,]
    coordinates(temp) <- c("elon", "nlat")
    proj4string(temp) <- CRS("+proj=longlat +datum=WGS84")
    res <- spTransform(temp, CRS(paste("+proj=utm +zone=",zone[i]," ellps=WGS84",sep='')))
    out[i,] <- as.data.frame(res)
  }
  return(out)
}

#----------------------------------------------------------------------
#----------------------------------------------------------------------
# read the dataframe with locations information (e.g., Site_ID, Longitude, Latitude, etc.)
df <- read.csv('~/Downloads/example_data_fluxnet_locations.csv', stringsAsFactors=FALSE)
utm_zones <- long2utmzone(df$elon)
# Creating separate shape file for each location
for(i in 1:nrow(df)){
  # Converting degree latitude and longitude points to UTM (m) [Comment below if your coordinates already are in UTM]
  centroids <- LongLatToUTM(df[i,])  # If you get error here, please check columns names as required
  #centroids <- df[i,] # uncomment here if lat-long are in UTM already
  ## ----view-data-------------------------------------------------------
  str(centroids)
  ## ----set-radius/buffer------------------------------------------------------
  # set the desired radius/buffer around a location 
  radius <- 175 # radius in meters
  # define the plot edges based upon the plot radius. 
  yPlus <- centroids$nlat+radius
  xPlus <- centroids$elon+radius
  yMinus <- centroids$nlat-radius
  xMinus <- centroids$elon-radius
  ## ----create-perimeter------------------------------------------------
  # calculate polygon coordinates for each plot centroid. 
  square=cbind(xMinus,yPlus,  # NW corner
               xPlus, yPlus,  # NE corner
               xPlus,yMinus,  # SE corner
               xMinus,yMinus, # SW corner
               xMinus,yPlus)  # NW corner again - close ploygon
  
  ## ----get-id----------------------------------------------------------
  
  # Extract the plot ID information
  ID=centroids$Site_ID
  ## ----mapply----------------------------------------------------------
  # create spatial polygons from coordinates
  polys <- SpatialPolygons(mapply(function(poly, id) 
  {
    xy <- matrix(poly, ncol=2, byrow=TRUE)
    Polygons(list(Polygon(xy)), ID=id)
  }, 
  split(square, row(square)), ID),
  proj4string=CRS(as.character(paste0('+proj=utm +zone=',utm_zones[i], ' +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'))))
  ## ----create-spdf-----------------------------------------------------
  # Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
  polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

  ## ----write-ogr-------------------------------------------------------
  # write the shapefiles 
  writeOGR(polys.df, '~/Downloads/Buffer_Zone_Shape_files/', paste0(ID,'_buffer_300m'), 'ESRI Shapefile')
  
  my_spdf <- readOGR(paste0('~/Downloads/Buffer_Zone_Shape_files/',ID,'_buffer_300m.shp'))
  par(mar=c(0,0,0,0))
  plot(my_spdf)
}
