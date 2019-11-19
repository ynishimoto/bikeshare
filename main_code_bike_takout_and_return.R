library(ggmap)
library(gganimate)
library(googleAuthR)
library(tidyverse)
library(readxl)
library(httr)
library(data.table)
library(magick)


set_config(use_proxy(url="204.40.130.129",port=3128, username = "nishimotoyu", auth="ntlm"))
register_google(key = 'AIzaSyDRXmCmahuivyf0fxQxEJr414PczuWZ1Oo')


sheet_nm <- excel_sheets('bikeshare-ridership-2014-q4-2015-q3.xlsx')
dat <- lapply(sheet_nm, function(X) read_excel('bikeshare-ridership-2014-q4-2015-q3.xlsx', sheet = X, col_names=FALSE, skip=2))
lst <- dat[grepl('[[:digit:]]', sheet_nm)]
names(lst) <- substr(sheet_nm, 1, 7)[1:length(lst)]
key <- dat[sheet_nm=='Station Key'][[1]][,-5] %>%
  setnames(c('Terminal', 'Station','Latitude','Longitude'))

for (i in 1:length(lst)) {
  tmp <- lst[[i]] %>%
    setnames(c('Terminal','to_station_name','Casual','Registered','Count')) %>%
    filter(Terminal!='Total') %>%
    mutate(Date = as.Date(paste0(names(lst)[i],'-01'), '%Y-%m-%d')) %>%
    mutate(Terminal = as.numeric(Terminal)) %>%
    mutate(to_station_name = as.numeric(to_station_name))
  lst[[i]]<- tmp
}
tmp <- bind_rows(lst) %>%
  select('Terminal', 'to_station_name', 'Count', 'Date')
  
dat1 <- data.frame(Terminal = c(tmp$Terminal, tmp$to_station_name),
                   Count = as.numeric(c(tmp$Count, tmp$Count)),
                   Date = c(tmp$Date, tmp$Date), stringsAsFactors=FALSE) %>%
  group_by(Date, Terminal) %>%
  summarise(Count=sum(Count)) %>%
  ungroup(Date) %>%
  inner_join(key[,1:2], by='Terminal') %>%
  select(Date, Station, Count)



#######################

tmp1 <- read_excel('bikeshare-ridership-2016-q3.xlsx') %>%
  select('trip_id','trip_start_time','from_station_name','to_station_name') %>%
  rename(Date=trip_start_time, Station=from_station_name) %>%
  mutate(Date = as.Date(Date, '%m/%d/%Y', tz='EST'))

tmp2 <- read_excel('bikeshare-ridership-2016-q4.xlsx') %>%
  select('trip_id','trip_start_time','from_station_name','to_station_name') %>%
  rename(Date=trip_start_time, Station=from_station_name) %>%
  mutate(Date = as.Date(Date, '%d/%m/%Y', tz='EST'))

tmp3 <- rbind(read.csv(paste0('bikeshare-ridership-2017-q1.csv'))[c('trip_id','trip_start_time','from_station_name','to_station_name')],
              read.csv(paste0('bikeshare-ridership-2017-q2.csv'))[c('trip_id','trip_start_time','from_station_name','to_station_name')],
              stringsAsFactors=FALSE) %>%
  rename(Date=trip_start_time, Station=from_station_name) %>%
  mutate(Date = gsub('^(.*) .*$', '\\1', Date)) %>%
  mutate(Date = gsub('2017', '17', Date)) %>%
  mutate(Date = as.Date(Date, '%d/%m/%y', tz='EST'))

tmp4 <- rbind(read.csv(paste0('bikeshare-ridership-2017-q3.csv'))[c('trip_id','trip_start_time','from_station_name','to_station_name')],
              read.csv(paste0('bikeshare-ridership-2017-q4.csv'))[c('trip_id','trip_start_time','from_station_name','to_station_name')],
              stringsAsFactors=FALSE) %>%
  rename(Date=trip_start_time, Station=from_station_name) %>%
  mutate(Date = gsub('^(.*) .*$', '\\1', Date)) %>%
  mutate(Date = gsub('2017', '17', Date)) %>%
  mutate(Date = as.Date(Date, '%m/%d/%y', tz='EST'))

dat <- rbind(tmp1, tmp2, tmp3, tmp4) %>%
  group_by(Date, Station, to_station_name) %>%
  count()

dat2 <- data.frame(Station = c(dat$Station, dat$to_station_name),
                   Count = as.numeric(c(dat$n, dat$n)),
                   Date = c(dat$Date, dat$Date), stringsAsFactors=FALSE) %>%
  group_by(Date, Station) %>%
  summarise(Count=sum(Count)) %>%
  ungroup(Date, Station)

df <- bind_rows(dat1, dat2) %>%
  filter(Date!='2000-01-01') %>%
  mutate(Date=as.Date(paste0(substr(Date, 1, 4),'-12-28'))) %>%
  group_by(Date, Station) %>%
  summarise(Count=sum(Count)) %>%
  mutate(Station = gsub('^(.*) - SMART$', '\\1', Station)) %>%
  mutate(Station = gsub('\\([^)]*\\)', '', Station)) %>%
  mutate(Station = gsub(' *\\bW{1}\\b *', ' ', Station)) %>%
  mutate(Station = gsub(' *\\bE{1}\\b *', ' ', Station)) %>%
  mutate(Station = gsub('\\.', '', Station)) %>%
  mutate(Station = gsub('  ', ' ', Station)) %>%
  mutate(Station = trimws(Station)) %>%
  distinct()



#LINE PLOT
linedata <- bind_rows(dat1, dat2) %>%
  filter(Date!='2000-01-01') %>%
  mutate(Date=as.Date(paste0(substr(Date, 1, 7),'-28'))) %>%
  group_by(Date) %>%
  summarise(Count=sum(Count)) %>%
  filter(!is.na(Date))

st <- unique(df$Station)
stn <- rep(st, 5)
dt <- rep(c(as.Date('2014-01-01'), unique(df$Date)), each=length(st))
frame <- data.frame(Date=dt, Station=stn, Count=0, stringsAsFactors=FALSE)

x <- left_join(frame, df, by=c('Date','Station')) %>%
  rename(Count=Count.y) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count)) %>%
  select(-3)


# #GET NEW STATION LAT/LON
# stn <- unique(x$Station)
# coord <- geocode(paste0(stn,', Toronto'))
# key <- data.frame(Station=stn, coord, stringsAsFactors=FALSE)
# write.csv(key, 'station_coordinates.csv', row.names=FALSE)
station_coordinates <- read.csv('station_coordinates.csv', stringsAsFactors=FALSE)

y <- inner_join(x, station_coordinates, by='Station') %>%
  distinct()


#MAP
#map <- qmap('Toronto', zoom=13)
p1 <- map +
  geom_point(data=y, aes(x=lon, y=lat, group=lat, size=Count, color='green'),
             alpha = 0.7, show.legend = FALSE) +
  scale_size(range = c(0, 12)) +
  transition_time(Date) +
  labs(title = 'Date: {frame_time}')
  #labs(title = "Count: {closest_state}")
anim1 <- animate(p1, nframes = 100, fps=20)



#LINE PLOT
linedata <- bind_rows(dat1, dat2) %>%
  filter(Date!='2000-01-01') %>%
  mutate(Date=as.Date(paste0(substr(Date, 1, 7),'-28'))) %>%
  group_by(Date) %>%
  summarise(Count=sum(Count)) %>%
  filter(!is.na(Date))

p2 <- ggplot(data=linedata, aes(x=Date, y=Count, color='red')) +
  geom_line() +
  geom_point() +
  transition_reveal(Date)

anim2 <- animate(p2, nframes = 100, fps=20)






a_mgif <- image_read(anim1)
b_mgif <- image_read(anim2)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif




