library(tidyverse)
library(lubridate)
library(rgdal)
library(raster)
library(sp)
library(rasterVis)
library(RStoolbox)
library(maps)
library(mapproj)
library(animation)
library(cowplot)
library(twitteR)

sessionInfo()
#setwd("projects/modis")

options(httr_oauth_cache=F)

#read in Alaska map

states <- readOGR("ne_10m_admin_1_states_provinces", "ne_10m_admin_1_states_provinces")
states_ak <- states[states$code_hasc=="US.AK",]
alaska_df <- fortify(states_ak)

places <- readOGR("ne_10m_populated_places", 'ne_10m_populated_places')
places_ak <- places[places$NAME %in% c("Anchorage","Nome", "Homer", "Dutch Harbor", "Fairbanks", "Kotzebue"), ]
places_ak <- data.frame(places_ak)

#grabs the current time

currentTime <- Sys.Date()
yr <- format(Sys.Date(), "%Y")


#sets the julien day from the current Time
currentJD <- yday(currentTime)

#a function that takes a JD and returns a three digit JD 
fixJD <-  function (currentJD) {
  
  if (nchar(currentJD)==1){
    currentJD <-  paste0("00",currentJD)
  }
  else if (nchar(currentJD)==2){
    currentJD <- paste0("0",currentJD)
  }
  
  else {
    currentJD <- currentJD
  }
  return 
  currentJD
  
}

#run the function based on the current day, returns url ready to download
currentJD <- fixJD(currentJD)


#the southwest Alaska URL to download from NASA

urlSW <- paste0("https://lance3.modaps.eosdis.nasa.gov/imagery/subsets/?project=other&subset=SouthwestAlaska.",yr,currentJD,".terra.500m.tif")
urlBS <- paste0("https://lance3.modaps.eosdis.nasa.gov/imagery/subsets/?project=other&subset=BeringSea.",yr,currentJD,".terra.500m.tif")



#download the raster, save to file named based on year
getFile <- function(url) {
  
currentFile <- paste0(yr,currentJD,".tiff")
download.file(url, currentFile )
return (currentFile)
}

#**delete this


currentFile <- getFile(urlSW)

#function to download and map the raster

createMap<- function(currentFile) { 
  
#create rasterbrick
currentRaster <- brick(currentFile)
proj4string(currentRaster) <- CRS("+init=epsg:3338")
fileExpName <- paste0(yr, currentJD,"SCAK.png")
ggRGB(currentRaster, r=1, g=2, b=3, maxpixels =9999999999999999)+
  geom_path(data=alaska_df, size=1.3, color="black", aes(x=long, y=lat, group=group), alpha = 0.4)+
  geom_path(data=alaska_df, size=.9, color="white", aes(x=long, y=lat, group=group), alpha = 0.8)+
  coord_fixed(1.85, ylim=c(57.3, 61.55), xlim=c(-159.3, -147.5))+
  theme(panel.background = element_blank())+
  geom_point(data=places_ak, fontface = "bold", aes(x=LONGITUDE, y=LATITUDE, label=NAME))+
  #geom_text(data=places_ak, fontface = "bold", size=6, nudge_y =-.06, nudge_x = .4, color="black", aes(x=LONGITUDE, y=LATITUDE, label=NAME))+
  geom_label(data=places_ak, size=5, nudge_y =-.1, color="white", fill="black", nudge_x = .5, alpha=.8, label.size=0, fontface = "bold", aes(x=LONGITUDE, y=LATITUDE, label=NAME))+
    #annotate("text", x = -156, y = 61, label =format(currentTime, format="%B %d %Y")
#, color="black", size=11, family="sans",fontface = "bold" )+
  theme_nothing()+
 ggsave(filename=fileExpName, width = 12, height = 8, units ="in", dpi=110)



api_keys <- read.csv("twitter/twitter_access.csv", header =TRUE, stringsAsFactors = FALSE)

api_keys$consumer_key
api_keys$consumer_secret
api_keys$access_token
api_keys$access_secret

setup_twitter_oauth(consumer_key = api_keys$consumer_key,
                    consumer_secret = api_keys$consumer_secret,
                    access_token = api_keys$access_token,
                    access_secret = api_keys$access_secret)

# create tweet

theTime <- Sys.time()
textTweet <- paste0("The view of southcentral Alaska from MODIS satellite, downloaded at ",theTime,". #akwx Image data: https://go.nasa.gov/2gAcoFd") 

# send tweet

tweet(textTweet, mediaPath=fileExpName)
}

createMap(currentFile)

