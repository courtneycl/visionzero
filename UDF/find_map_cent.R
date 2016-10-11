# Find center point for google map search 
# by using median Longitude and Latitude
find_map_cent <- function (LON, LAT) {
  return (c(mean(LON, na.rm = T),
            mean(LAT, na.rm = T))
  )
}