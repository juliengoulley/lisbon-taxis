# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

#library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

setwd("D:\\github\\lisbon-taxis\\branches\\readme-edits")
# system("ls ../input")

# Any results you write to the current directory are saved as output.
library(ggplot2)

taxi_data = read.csv("data/train_sample.csv")

# Create a utility function to the X axis coordinate from coordinate "x,y"
extractXCoordinate <- function(Coordinates) {
  Coordinates <- as.character(Coordinates)
  # Get the x coordinate from the "x,y" coordinates
  x = gsub(",.*", "", Coordinates)
  return(x)
}

# Create a utility function to the y axis coordinate from coordinate "x,y"
extractYCoordinate <- function(Coordinates) {
  Coordinates <- as.character(Coordinates)
  # Get the y coordinate from the "x,y" coordinates
  y = gsub(".*,", "", Coordinates)
  return(y)
}

# Create a utility function to get the pick-up location
extractPickUpCoordinates <- function(polyline) {
  polyline <- as.character(polyline)
  # Get the pick-up coordinates from the polyline
  pickup <- gsub("\\[\\[", "", polyline)
  pickup <- gsub("\\]\\]", "", pickup)
  pickup <- unlist(strsplit(pickup, "\\],\\["))
  pickup <- pickup[1]
  return(pickup)
}

# Create a utility function to get the drop-off location
extractDropOffCoordinates <- function(polyline) {
  polyline <- as.character(polyline)
  # Get the pick-up coordinates from the polyline
  dropoff <- gsub("\\[\\[", "", polyline)
  dropoff <- gsub("\\]\\]", "", dropoff)
  dropoff <- unlist(strsplit(dropoff, "\\],\\["))
  dropoff <- dropoff[length(dropoff)]
  return(dropoff)
}

# Obtain some new features, namely:
# 2. PickUp Longitude (x)
# 1. PickUp Latitude (y)
# 4. DropOff Longitude (x)
# 3. DropOff Latitude (y)
PickUpLongitude <- NULL
PickUpLatitude <- NULL
DropOffLongitude <- NULL
DropOffLatitude <- NULL
for (i in 1:nrow(taxi_data)) {
  PickUpLongitude <- c(PickUpLongitude, extractXCoordinate(extractPickUpCoordinates(taxi_data[i,"POLYLINE"])))
  PickUpLatitude <- c(PickUpLatitude, extractYCoordinate(extractPickUpCoordinates(taxi_data[i,"POLYLINE"])))
  DropOffLongitude <- c(DropOffLongitude, extractXCoordinate(extractDropOffCoordinates(taxi_data[i,"POLYLINE"])))
  DropOffLatitude <- c(DropOffLatitude, extractYCoordinate(extractDropOffCoordinates(taxi_data[i,"POLYLINE"])))
}
taxi_data$PickUpLongitude <- as.factor(PickUpLongitude)
taxi_data$PickUpLatitude <- as.factor(PickUpLatitude)
taxi_data$DropOffLongitude <- as.factor(DropOffLongitude)
taxi_data$DropOffLatitude <- as.factor(DropOffLatitude)

# Add the PickUp/DropOff coordinates and factors in the data.
taxi_data$PickUpLongitude <- as.factor(taxi_data$PickUpLongitude)
taxi_data$PickUpLatitude <- as.factor(taxi_data$PickUpLatitude)
taxi_data$DropOffLongitude <- as.factor(taxi_data$DropOffLongitude)
taxi_data$DropOffLatitude <- as.factor(taxi_data$DropOffLatitude)

# Plot Pickup and DropOff locations.
ggplot(subset(taxi_data[1:10000,],POLYLINE != ""), aes(x = as.numeric(as.character(PickUpLongitude)), y = as.numeric(as.character(PickUpLatitude)))) +
  #stat_count(width = 0.5) +
  #scale_x_continuous(breaks=c(1:250)*10) +
  #theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0)) +
  geom_point() +
  ggtitle("PickUp and DropOff Locations") +
  xlab("Longitude") +
  ylab("Latitude")
