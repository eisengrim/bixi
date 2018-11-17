# filename : bixi-analysis_v1
# author   : kody crowell
# date     : 30 oct 2018
# credit to timo grossenbacher for inspiration on map theme function:
# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/

# for a cool analysis of bixi usage and weather, check out:
# http://web.meteo.mcgill.ca/cmccray/weather-bike-traffic-montreal/#more-16

# load libs
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(ggrepel)
library(lubridate)
library(sf)
library(sp)
library(rgdal)
library(proj4)
library(rgeos)
library(tmap)
library(ggmap)
library(scales)
library(RColorBrewer)

theme_set(theme_minimal())

# when do people rent?          
# where do people go?           X
# where to people rent?         X
# average duration?             
# where do members rent?        X
# number of renters per year    
# monthly trips by member?      X
# trips per hour, weekday/end (see 1)

# unzip bixi data -- move to data folder
# unzip("BixiMontrealRentals2018.zip")
# unzip("BixiMontrealRentals2017.zip")
# unzip("BixiMontrealRentals2016.zip")

# read all files in folder, rbind into one dataframe
fnames <- dir("data/2016") 
bixi.2016 <- do.call(rbind, lapply(paste("data/2016", fnames[1:8], sep="/"), read_csv))
stations.2016 <- read_csv("data/Stations_2016.csv")

fnames <- dir("data/2017") 
bixi.2017 <- do.call(rbind, lapply(paste("data/2017", fnames[1:8], sep="/"), read_csv))
stations.2017 <- read_csv("data/Stations_2017.csv")

fnames <- dir("data/2018") 
bixi.2018 <- do.call(rbind, lapply(paste("data/2018", fnames[1:7], sep="/"), read_csv))
stations.2018 <- read_csv("data/Stations_2018.csv")


# download montreal fsa file data
fsource <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lfsa000b16a_e.zip"
download.file(fsource, destfile="data/lfsa000b16a_e.zip")
unzip("data/lfsa000b16a_e.zip")
file.remove("data/lfsa000b16a_e.zip", "data/forward_sortation_area.html")

# the shapefile is the layer 
canada <- readOGR("~/workspace/bixi/data/lfsa000b16a_e.shp")#,encoding="latin1") 
qcfsa <- canada[canada@data$PRUID=="24", ]

# subset to the island of montreal
mtlfsa <- qcfsa[startsWith(as.character(qcfsa@data$CFSAUID), 'H') &                                     
                  qcfsa@data$CFSAUID != 'H0M' &                                                       
                  !startsWith(as.character(qcfsa@data$CFSAUID), 'H7'),]
mtlfsa@data$CFSAUID <- as.character(mtlfsa@data$CFSAUID)

# check
# qtm(mtlfsa)

# quick count of station interactions
rentals.2018 <- bixi.2018 %>%
  group_by(start_station_code) %>%
  dplyr::summarise(rentals = n()) %>%
  right_join(stations.2018, by=c("start_station_code" = "code")) %>%
  rename("code" = "start_station_code")

rentals.2017 <- bixi.2017 %>%
  group_by(start_station_code) %>%
  dplyr::summarise(rentals = n()) %>%
  right_join(stations.2017, by=c("start_station_code" = "code")) %>%
  rename("code" = "start_station_code")

rentals.2016 <- bixi.2016 %>%
  group_by(start_station_code) %>%
  dplyr::summarise(rentals = n()) %>%
  right_join(stations.2016, by=c("start_station_code" = "code")) %>%
  rename("code" = "start_station_code")

# create a spatial points data frame for bixi station
stations <- SpatialPointsDataFrame(coords=rentals.2018[,c(5,4)],
                                   data=rentals.2018[,-c(5,4)],
                                   proj4string=CRS("+proj=longlat +datum=NAD83"))

# assign mtlfsa proj4string
stations <- spTransform(stations, CRS(proj4string(mtlfsa)))

# double check that they match
identical(proj4string(mtlfsa), proj4string(stations))

# get 2016 census population data
# data from https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/index-eng.cfm
# Source: Statistics Canada, 2016 Census of Population.
pop.can <- read_csv("data/population_mtl.csv")
pop.mtl <- pop.can[startsWith(pop.can$`Geographic code`, 'H'), c(1, 5)]
colnames(pop.mtl) <- c("geouid", "popn")

# join to fsa data --? not working in plot!
mtlfsa@data <- left_join(mtlfsa@data, pop.mtl, by=c("CFSAUID"="geouid"))

# fortify stations data, cut out extreme points
st.spdf <- as.data.frame(stations)
st.spdf <- st.spdf[-which(st.spdf$long > 7634600 & st.spdf$lat > 1248000),]

# note the outliers in rental data -- truncate?
# st.spdf <- st.spdf %>%  mutate(quartile = ntile(rentals, 5))
# st.spdf[st.spdf$rentals > 20000,]

st.spdf <- st.spdf %>% 
  mutate(rentals2 = ifelse(rentals > 20000, 20000, rentals))

###############################################################################
# plotting themes
# mir colors: #4b4b4b gray / #f00d0d red / #f9f9f9 white / #eeeeee light gray
mir.red <- "#cf0808" #"#f00d0d"
mir.white <- "#f9f9f9"
mir.gray <- "#4b4b4b"
mir.lgray <- "#eeeeee"
red.ramp <- c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#99000d')

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Georgia", color = mir.gray),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = mir.lgray, size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = mir.white, color = NA), 
      panel.background = element_rect(fill = mir.white, color = NA), 
      legend.background = element_rect(fill = mir.white, color = NA),
      panel.border = element_blank(),
      ...
    )
}
theme_mir <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Georgia", color = mir.gray),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle=45, hjust=1),
      axis.text.y = element_text(size=rel(1)),
      axis.title.y = element_text(size=rel(1), vjust=2),
      axis.title.x = element_text(size=rel(1), vjust=-0.5),
      panel.grid.major = element_line(color = mir.lgray, size = 0.2),
      panel.grid.minor = element_line(color = mir.lgray, size = 0.2),
      plot.background = element_rect(fill = mir.white, color = NA), 
      panel.background = element_rect(fill = mir.white, color = NA), 
      legend.background = element_rect(fill = mir.white, color = NA),
      panel.border = element_blank(),
      ...
    )
}

# plot mtl base
mtl.base <- ggplot(data=mtl.spdf, mapping=aes(x=long, y=lat, group=group)) +
  coord_fixed(1) +
  geom_polygon(fill="gray", color="white", lwd=0.2)
 
# mtl base with station locations
map.mtl <- mtl.base +
  geom_point(mapping=aes(x=long, y=lat, color=quartile), 
             data=st.spdf, #color=mir.red, 
             inherit.aes=F) +
  coord_fixed(xlim=c(7622000, 7635000), ylim=c(1236000, 1254000), ratio=1) +
  theme_map()

# create station heat map using kernel density estimation
pretty_breaks <- c(500, 1000, 2500, 5000, 7500, 10000)                                                            

# find the extremes                                                                                     
minVal <- min(st.spdf$rentals2, na.rm = T)                 
maxVal <- max(st.spdf$rentals2, na.rm = T)               
# compute labels                                                                                        
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)                 

# round the labels (actually, only the extremes)                                                        
for(idx in 1:length(brks)){                                                                             
  labels <- c(labels,round(brks[idx + 1], 2))                                                           
}  
labels <- labels[1:length(labels)-1]                                                                    

# define a new variable on the data with the breaks                                                     
st.spdf$brks <- cut(st.spdf$rentals2,                                                                     
                     breaks = brks,                                                                    
                     include.lowest = TRUE,                                                            
                     labels = labels)                                                                  

brks_scale <- levels(st.spdf$brks)                                                                     
labels_scale <- rev(brks_scale)
labels_scale <- c("20k+", "10k", "7.5k", "5k", "2.5k", "1k", "500")

mtl.base + geom_point(mapping=aes(x=longitude, y=latitude, color=rentals2, 
                         size=rentals2, alpha=0.5), 
             data=st.spdf, # color=mir.red, 
             inherit.aes=F) + #, size=1.5) +
  coord_fixed(xlim=c(7622000, 7635000), ylim=c(1236500, 1253500), ratio=1) +
  labs(x = NULL,                                                                                        
       y = NULL,                                                                                        
       title = "Montreal's Bixi Stations",                                                 
       subtitle = "Number of rentals per station, Apr-Oct 2018",               
       caption = "Author: Kody Crowell (@hummushero) // Geometries: Stats Can, 2016; Bixi Montreal, 2018") +                 
  theme_map() +
  scale_size(range=c(0.5, 3.5)) +
  scale_color_gradientn(
    colors = rev(red.ramp),
    name = "Number of Rentals",
    na.value = "grey",
    guide = guide_legend(
      direction = "horizontal", keyheight = unit(2, units = "mm"),
      keywidth = unit(70/length(labels), units = "mm"),
      title.position = 'right', title.hjust = 0.5, label.hjust = 0.5,
      nrow = 1, byrow = T, reverse = T, label.position = "bottom"
    ),
    breaks = rev(as.numeric(brks_scale)),
    labels = labels_scale
  ) +
  guides(alpha=F, size=F) +
  theme(legend.position = "bottom") # save as 530x800 // 662x1000

################################################################################
# create flow map

# collect rentals and drop-offs
# count the number of trips in between stations
# join the xy coordinates to the origin and then the destinations
trips <- bixi.2018 %>%
  mutate(origin = start_station_code,
         dest = end_station_code) %>%
  group_by(origin, dest) %>%
  dplyr::summarise(trips = n()) %>%
  distinct(.) %>%
  inner_join(stations.2018, by=c("origin"="code")) %>%
  rename("lat0" = "latitude", "lon0" = "longitude", "oname"="name") %>%
  inner_join(stations.2018, by=c("dest"="code")) %>%
  rename("lat1" = "latitude", "lon1" = "longitude", "dname"="name") %>%
  filter(origin != dest)

# create a spatial points data frame for bixi stations as origin/destination
sl.or <- SpatialPointsDataFrame(coords=trips[,c(6,5)],
                                data=trips[,-c(6,5,9,8)],
                                proj4string=CRS("+proj=longlat +datum=NAD83"))
sl.de <- SpatialPointsDataFrame(coords=trips[,c(9,8)],
                                data=trips[,-c(6,5,9,8)],
                                proj4string=CRS("+proj=longlat +datum=NAD83"))

# transform to mtlfsa coordinates
sl.or <- spTransform(sl.or, CRS(proj4string(mtlfsa)))
sl.de <- spTransform(sl.de, CRS(proj4string(mtlfsa)))

# fortify for plotting
st.or <- as.data.frame(sl.or)
st.de <- as.data.frame(sl.de)

# replace old lon/lat data in trips with new transformed variables
trips$lon0 <- st.or$lon0
trips$lat0 <- st.or$lat0
trips$lon1 <- st.de$lon1
trips$lat1 <- st.de$lat1

# as before, remove stations not on montreal island
trips <- trips[-which(trips$lon0 > 7634600 & trips$lat0 > 1248000 |
                      trips$lon1 > 7634600 & trips$lat1 > 1248000),]

# flow map, filter number of trips
# add to existing montreal base map
mtl.base +
  geom_point(mapping=aes(x=longitude, y=latitude), data=st.spdf, color="#4b4b4b", 
             inherit.aes=F, size=0.5) +
  geom_curve(aes(x=lon0, y=lat0, xend=lon1, yend=lat1, alpha=0.8),
               data=trips[trips$trips > 400,], col=mir.red, 
             curvature=0.2, inherit.aes=F) +
  scale_alpha_continuous(range = c(0.05, 0.25)) +
  coord_fixed(xlim=c(7622000, 7635000), ylim=c(1236500, 1253500), ratio=1) +
  labs(x = NULL,                                                                                        
       y = NULL,                                                                                        
       title = "Bixi Traffic in Montreal",                                                 
       subtitle = "Number of trips between stations, Apr-Oct 2018\nArcs with <400 trips are excluded",               
       caption = "Author: Kody Crowell (@hummushero) // Geometries: Stats Can, 2016; Bixi Montreal, 2018") +
  guides(alpha = F, color=F)  +
  theme_map() # save as 530x765 // 692x1000

# default: xlim=c(7622000, 7635000), ylim=c(1236500, 1253500)
# zoom: xlim=c(7628000, 7633000), ylim=c(1243000, 1247000)

top5 <- trips %>% ungroup() %>% filter(trips>2000) %>% select(-origin, -dest, -trips) 
top5 <- rbind(top5[1:3], setNames(top5[4:6], names(top5[1:3]))) %>% 
  rename(name=oname, lat=lat0, lon=lon0) %>%
  distinct()

mtl.base +
  geom_point(mapping=aes(x=longitude, y=latitude), data=st.spdf, color="#4b4b4b", 
             inherit.aes=F, size=0.5) +
  geom_curve(aes(x=lon0, y=lat0, xend=lon1, yend=lat1, alpha=0.9),
             data=trips[trips$trips > 500,], col=mir.red, 
             curvature=0.2, inherit.aes=F) +
  scale_alpha_continuous(range = c(0.05, 0.25)) +
  coord_fixed(xlim=c(7622000, 7635000), ylim=c(1236500, 1253500), ratio=1) +
  labs(x = NULL,                                                                                        
       y = NULL,                                                                                        
       title = "Bixi Traffic in Montreal",                                                 
       subtitle = "Number of trips between stations, Apr-Oct 2018\nArcs with <500 trips are excluded, stations with >2000 trips started/stopped are labelled",               
       caption = "Author: Kody Crowell (@hummushero) // Geometries: Stats Can, 2016; Bixi Montreal, 2018") +
  guides(alpha = F, color=F)  +
  geom_label_repel(data=top5, inherit.aes=F,
                       aes(x=lon, y=lat, label=name), size=3) +
  theme_map()

################################################################################
# time series of station usage
bixi <- rbind(bixi.2016, bixi.2017, bixi.2018)

rentals <- bixi %>%
  mutate(month=as.integer(month(start_date)),
         year=as.integer(year(start_date))) %>%
  mutate(date=format(as.Date(start_date), "%Y-%m")) %>%
  group_by(date, is_member) %>%
  dplyr::summarise(n.rentals = n()) %>%
  ungroup() %>%
  mutate(date2=as.Date(paste(rentals$date,"-01",sep="")))

ggplot(data=rentals, aes(x=date2, y=n.rentals, group=as.factor(is_member), fill=as.factor(is_member))) +
  geom_bar(stat="identity") +
  labs(x="Date", y="Number of Rentals",
       title="Number of Bixi Rentals Over Time",
       subtitle="Number of rentals per month stratified by membership, April 2016 - October 2018",
       caption="Author: Kody Crowell (@hummushero); Source: Bixi Open Data (2018)") +
  scale_fill_manual(values=rev(red.ramp), name="", labels=c("Occasionals","Members"),
                    guide = guide_legend(
                      direction = "vertical", keyheight = unit(2, units = "mm"),
                      keywidth = unit(100/length(labels), units = "mm"),
                      title.position = 'right', title.hjust = 0.5, label.hjust = 0.5,
                      reverse = T, label.position = "bottom")) +
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  geom_label(data=rentals %>% filter(month(date2)==7 & is_member==T), 
            aes(x=date2, y=250000, label=year(date2)), inherit.aes=F) +
  theme_mir() +
  theme(strip.text.x = element_text(size=rel(1)),
        strip.text.y = element_text(size=rel(1)),
        strip.background = element_blank(),
        legend.background = element_blank(),
        legend.justification = c(0, 0),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color=mir.gray, face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=10), 
                                    color="grey60", hjust=0)) # 1135x800

################################################################################
trips.m <- bixi.2018 %>%
  mutate(origin = start_station_code,
         dest = end_station_code) %>%
  group_by(origin, dest, is_member) %>%
  dplyr::summarise(mtrips = n()) %>%
  distinct(.) %>%
  filter(is_member == 1) %>%
  select(-is_member) %>%
  inner_join(select(trips, origin, dest, trips), by=c("origin", "dest")) %>%
  mutate(trips.p = mtrips/trips) %>%
  inner_join(stations.2018, by=c("origin"="code")) %>%
  rename("lat0" = "latitude", "lon0" = "longitude", "oname"="name") %>%
  inner_join(stations.2018, by=c("dest"="code")) %>%
  rename("lat1" = "latitude", "lon1" = "longitude", "dname"="name") %>%
  filter(origin != dest)

# create a spatial points data frame for bixi stations as origin/destination
sl.or <- SpatialPointsDataFrame(coords=trips.m[,c(8,7)],
                                data=trips.m[,-c(7,8,10,11)],
                                proj4string=CRS("+proj=longlat +datum=NAD83"))
sl.de <- SpatialPointsDataFrame(coords=trips.m[,c(11,10)],
                                data=trips.m[,-c(7,8,10,11)],
                                proj4string=CRS("+proj=longlat +datum=NAD83"))

# transform to mtlfsa coordinates
sl.or <- spTransform(sl.or, CRS(proj4string(mtlfsa)))
sl.de <- spTransform(sl.de, CRS(proj4string(mtlfsa)))

# fortify for plotting
st.or <- as.data.frame(sl.or)
st.de <- as.data.frame(sl.de)

# replace old lon/lat data in trips.m with new transformed variables
trips.m$lon0 <- st.or$lon0
trips.m$lat0 <- st.or$lat0
trips.m$lon1 <- st.de$lon1
trips.m$lat1 <- st.de$lat1

# as before, remove stations not on montreal island
# trips.m <- trips.m[-which(trips.m$lon0 > 7634600 & trips.m$lat0 > 1248000 |
#                         trips.m$lon1 > 7634600 & trips.m$lat1 > 1248000),]

# flow map, filter number of trips
# add to existing montreal base map
# color by membership!!!
mtl.base +
  geom_point(mapping=aes(x=longitude, y=latitude), data=st.spdf, color=mir.gray, 
             inherit.aes=F, size=0.5) +
  geom_curve(aes(x=lon0, y=lat0, xend=lon1, yend=lat1, alpha=0.8, color=trips.p),
             data=trips.m[trips.m$trips > 300,], curvature=0.2, inherit.aes=F) +
  scale_alpha_continuous(range = c(0.05, 0.25)) +
  scale_color_gradient2(low="blue", mid=mir.gray, high=muted(mir.red), midpoint=0.5) +
  coord_fixed(ratio=1, xlim=c(7628000, 7633000), ylim=c(1243000, 1247000)) + 
  labs(x = NULL,                                                                                        
       y = NULL,                                                                                        
       title = "Bixi Traffic by Membership in Montreal",                                                 
       subtitle = "Number of trips colored by proportion of members, Apr-Oct 2018\nArcs with <300 trips are excluded",               
       caption = "Author: Kody Crowell (@hummushero) // Geometries: Stats Can, 2016; Bixi Montreal, 2018") +
  guides(alpha = F, color=F)  +
  theme_map() # save as 530x765 // 692x1000


## bixi free sundays? 
# 27 May 
# 24 June 
# 29 July 
# 26 August 
# 30 Septembre
# 28 October 

free.days <- c("2018-05-27", "2018-06-24", "2018-07-29", "2018-08-26",
               "2018-09-30", "2018-10-28")

rentals.sun <- bixi.2018 %>%
  mutate(month=as.integer(month(start_date)),
         year=as.integer(year(start_date)),
         weekday=weekdays(start_date)) %>%
  filter(weekday=="Sunday") %>%
  mutate(date=format(as.Date(start_date), "%Y-%m-%d")) %>%
  group_by(date, is_member) %>%
  dplyr::summarise(n.rentals = n()) %>%
  ungroup() %>%
  mutate(is.free = as.factor(ifelse(date %in% free.days, "Yes", "No"))) %>%
  group_by(is.free, is_member) %>%
  dplyr::summarise(mean.rentals=mean(n.rentals))


ggplot(data=rentals.sun, aes(x=is.free, y=mean.rentals, group=factor(is_member),
                             fill=factor(is_member))) +
  geom_bar(stat="identity") +
  labs(x="Free days", y="Mean Number of Rentals",
       title="Do Free Sundays affect Bixi Rentals?",
       subtitle="Number of Sunday rentals on regular vs. free days, April 2018 - October 2018",
       caption="Author: Kody Crowell (@hummushero); Source: Bixi Open Data (2018)") +
  scale_fill_manual(values=rev(red.ramp), name="", labels=c("Occasionals","Members"),
                    guide = guide_legend(
                      direction = "vertical", keyheight = unit(2, units = "mm"),
                      keywidth = unit(100/length(labels), units = "mm"),
                      title.position = 'right', title.hjust = 0.5, label.hjust = 0.5,
                      reverse = T, label.position = "bottom")) +
  theme_mir() +
  theme(strip.text.x = element_text(size=rel(1)),
        strip.text.y = element_text(size=rel(1)),
        strip.background = element_blank(),
        legend.background = element_blank(),
        legend.justification = c(0, 0),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color=mir.gray, face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=10), 
                                    color="grey60", hjust=0)) # 1135x800
