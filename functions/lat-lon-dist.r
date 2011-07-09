
## http://www.johndcook.com/python_longitude_latitude.html

latlon2km <- function(lat1,lon1,lat2,lon2,earth.radius=6378.1) {
  ##earth.radius=6378.1 km
  lat <- c(lat1,lat2)
  lon <- c(lon1,lon2)
  phi <- (90.0-lat)*pi/180
  theta <- lon*pi/180
  arc <- acos(prod(sin(phi))*cos(diff(theta))+prod(cos(phi)))
  arc*earth.radius
}
