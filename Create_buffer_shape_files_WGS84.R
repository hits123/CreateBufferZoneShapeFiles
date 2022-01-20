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

#----------------------------------------------------------------------
#----------------------------------------------------------------------
# read the dataframe with locations information (e.g., Site_ID, Longitude, Latitude, etc.)
df <- read.csv('~/Downloads/example_data_fluxnet_locations.csv', stringsAsFactors=FALSE) # Assuming long-lat are in decimal degrees  
utm_zones <- long2utmzone(df$elon)
# Creating separate shape file for each location
for(i in 1:nrow(df)){
  centroids <- df[i,] 
  ## ----view-data-------------------------------------------------------
  str(centroids)
  ## ----set-radius/buffer------------------------------------------------------
  # set the desired radius/buffer around a location 
  radius_n <- 175 # radius or offset in meters in lat direction
  radius_e <- 175 # radius or offset in meters in long direction
  # offsetting a latitude/longitude by some amount of meters
  #https://gis.stackexchange.com/questions/2951/algorithm-for-offsetting-a-latitude-longitude-by-some-amount-of-meters
  R=6378137  # Earth’s radius, sphere
  dLat = radius_n /R # Coordinate offsets in radians in lat direction
  dLon = radius_e/(R*cos(pi*centroids$nlat/180))  # Coordinate offsets in radians in long direction

  lat0 = dLat * 180/pi  # Offset in degree
  lon0 = dLon * 180/pi
  # define the plot edges based upon the plot radius. 
  yPlus <- centroids$nlat+lat0
  xPlus <- centroids$elon+lon0
  yMinus <- centroids$nlat-lat0
  xMinus <- centroids$elon-lon0
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
  # Visualization
  my_spdf <- readOGR(paste0('~/Downloads/Buffer_Zone_Shape_files/',ID,'_buffer_300m.shp'))
  par(mar=c(0,0,0,0))
  plot(my_spdf)
}
