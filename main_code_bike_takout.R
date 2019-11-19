library(ggmap)
library(gganimate)
library(googleAuthR)
library(tidyverse)
library(readxl)
library(httr)
library(data.table)
library(magick)
library(tweenr)
library(chron)
library(caTools)


#set_config(use_proxy(url="204.40.130.129",port=3128, username = "nishimotoyu", auth="ntlm"))
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

tmp0 <- bind_rows(lst) %>%
  select('Terminal', 'to_station_name', 'Count', 'Date') %>%
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

tmp2 <- bind_rows(read_excel('bikeshare-ridership-2016-q4.xlsx')[1:44433,] %>%
                    mutate(Date = paste0(substr(Date, 1, 5), substr(Date, 9, 10),'-', substr(Date, 6, 7))),
          read_excel('bikeshare-ridership-2016-q4.xlsx')[44434:101370,] %>%
            mutate(Date=as.character(Date)),
          read_excel('bikeshare-ridership-2016-q4.xlsx')[101371:136992,] %>%
            mutate(Date = paste0(substr(Date, 1, 5), substr(Date, 9, 10),'-', substr(Date, 6, 7))),
          read_excel('bikeshare-ridership-2016-q4.xlsx')[136993:181032,] %>%
            mutate(Date=as.character(Date)),
          read_excel('bikeshare-ridership-2016-q4.xlsx')[181033:202827,] %>%
            mutate(Date = paste0(substr(Date, 1, 5), substr(Date, 9, 10),'-', substr(Date, 6, 7))),
          read_excel('bikeshare-ridership-2016-q4.xlsx')[202828:217570,] %>%
            mutate(Date=as.character(Date))) %>%
  mutate(Date=as.Date(Date)) %>%
  mutate(trip_id=1:nrow(.)) %>%
  mutate(to_station_name='a') %>%
  filter(!is.na(Date))

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

df <- rbind(tmp1, tmp2, tmp3, tmp4) %>%
  select(Date, Station) %>%
  group_by(Date, Station) %>%
  count() %>%
  ungroup() %>%
  rename(Count=n) %>%
  bind_rows(y=tmp0) %>%
  filter(Date!='2000-01-01') %>%
  mutate(Station = gsub('^(.*) - SMART$', '\\1', Station)) %>%
  mutate(Station = gsub('\\([^)]*\\)', '', Station)) %>%
  mutate(Station = gsub(' *\\bW{1}\\b *', ' ', Station)) %>%
  mutate(Station = gsub(' *\\bE{1}\\b *', ' ', Station)) %>%
  mutate(Station = gsub('\\.', '', Station)) %>%
  mutate(Station = gsub('  ', ' ', Station)) %>%
  mutate(Station = trimws(Station)) %>%
  arrange(Date)

st <- unique(df$Station)
stn <- rep(st, 5)
dt <- rep(c(as.Date('2014-01-01'), unique(df$Date)), each=length(st))
dt <- dt[order(dt)]
frame <- data.frame(Date=dt, Station=stn, Count=0, stringsAsFactors=FALSE)

df_map <- left_join(frame, df, by=c('Date','Station')) %>%
  rename(Count=Count.y) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count)) %>%
  select(-3) %>%
  filter(Station!='NULL') %>% 
  mutate(Date=as.Date(paste0(substr(Date, 1, 4),'-10-01'))) %>%
  group_by(Date, Station) %>%
  summarise(Count=sum(Count))

df_line <- df %>%
  ungroup() %>%
  mutate(Date=as.Date(paste0(substr(Date, 1, 7),'-01'))) %>%
  group_by(Date) %>%
  summarise(Count=sum(Count))
  
  #bind_rows(data.frame(
  #  as.Date(seq.dates('10/01/2015', '05/01/2016', by='months'), '%Y-%m-%d'),
  #  Count=c(85000, 65000, 20000))) %>%
  #arrange(Date)


#######################


# #GET NEW STATION LAT/LON
# stn <- unique(df$Station)
# coord <- geocode(paste0(stn,', Toronto'))
# key <- data.frame(Station=stn, coord, stringsAsFactors=FALSE)
# write.csv(key, 'station_coordinates.csv', row.names=FALSE)
station_coordinates <- read.csv('station_coordinates.csv', stringsAsFactors=FALSE)
x <- inner_join(df_map, station_coordinates, by='Station') %>%
  distinct()


#MAP
render_gif <- function(frames, fps) {

  toronto <- qmap(c(-79.388,43.664268), zoom=13)
  p1 <- toronto +
    geom_point(data=x, aes(x=lon, y=lat, group=lat, size=Count), colour='red',
               alpha = 0.5, show.legend = FALSE) +
    scale_size(range = c(0, 12)) +
    transition_time(Date) +
    labs(title = '{frame_time}') +
    theme(plot.title = element_text(size = 18))
  #animate(p1, nframes=20)
  anim1 <- animate(p1, nframes=frames, fps=fps)
  
  
  #LINE PLOT
  p2 <- ggplot(data=df_line, aes(x=Date, y=Count, color='red', alpha=0.7)) +
    geom_area(fill='red') +
    theme_light() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(hjust=-0.6),
      text = element_text(size=19),
      legend.position = 'none'
    ) +
    transition_reveal(Date)
  anim2 <- animate(p2, nframes=frames, fps=fps, height=180, width=300)
  
  #JOIN PLOTS
  gif1 <- image_read(anim1)
  gif2 <- image_read(anim2)
  
  image_composite(gif1[1], image_scale(gif2[1], 'x130'), offset = '+252+20')
  gif_joined <- image_composite(gif1[1], image_scale(gif2[1], "x130"), offset = "+252+20")
  for(i in 2:frames){
    combined <- image_composite(image_flatten(gif1[1:i]), image_scale(image_flatten(gif2[1:i]), "x130"), offset = "+252+20")
    gif_joined <- c(gif_joined, combined)
    cat('Joined frame',i,'of',frames,'\n')
  }
  return(gif_joined)
}

gif_joined <- render_gif(200, 20)
anim_save('animation.gif', gif_joined)
write.gif(gif_joined, 'animation.gif')
