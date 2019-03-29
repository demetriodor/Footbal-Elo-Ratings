### This file produces the race barcharts of football ratings, Dimiter Toshkov March 2019

# Libraries ---------------------------------------------------------------
library(rvest)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(tweenr)
library(gganimate)
library(viridis)
library(showtext)
font_add_google('Cairo') #get the fonts 
font_families() #check that the fonts are installed and available
showtext_auto() #this is to turn on the custom fonts availability
showtext_opts(dpi = 96) #set the resolution: 96 is default


# Get the data ----------------------------------------------------------
# First get the 1910 file
system("./phantomjs-2.1.1-windows/bin/phantomjs scrape_elos_1910.js")

# then prepare the rest of the js files
for (i in 1911:2019){
  year=i
  js_script_old_name = paste0('./scrape_elos_', i-1, '.js') # get the name of the the js file from the previous year
  js_script_new_name = paste0('./scrape_elos_', i, '.js') # make the name of the js file for the new year
  js_script_old_text <- readChar(js_script_old_name, file.info(js_script_old_name)$size) # read the old js file
  js_script_new_text <- gsub(i-1,i, js_script_old_text) # make changes in the text
  fileConn<-file(js_script_new_name) # open a connection to save the new file
  writeLines(js_script_new_text, fileConn) # write the new file
  close(fileConn) # close the connection
}

# then go throught the js files to download the webpages
for (i in 1910:2019){
  year=i
  system_name <- paste0('./phantomjs-2.1.1-windows/bin/phantomjs scrape_elos_',i,'.js')
  system (system_name)
  }

# now for each file read the table and record the data
data = data.frame (matrix (ncol=3, nrow=0)) # star with an empty data
colnames(data)=c('countries','elos','year')

for (i in 1910:2019){
  year=i
  file_name = paste0('./webpages elos/elos_',i,'.html')
  table_name = paste0('[id=maintable_',i, ']')

  maintable <- read_html(file_name) %>%
    html_nodes(table_name)

  countries<- maintable %>%
    html_nodes('div.l1') %>%
    html_text()

  elos<- maintable %>%
    html_nodes('div.l2') %>%
    html_text()  %>%
    as.numeric(as.character())

  data = rbind(data, cbind(countries, elos, year=rep(year, length(elos))))
}
  
data$elos <-as.numeric(as.character(data$elos))
data$year <-as.numeric(as.character(data$year))

# make an order variable
data %<>% 
  group_by(year) %>% 
  arrange(-elos) %>% 
  mutate(order = 1:n()) %>%
  ungroup() %>%
  arrange(year, -elos)

data$order = as.numeric(as.character(data$order))

# assign colors
u<-unique(data$countries)
cols = data.frame(countries=u, cols=sample(viridis(length(u))))
data<-left_join(data, cols, by='countries')

data$cols = rgb(col2rgb(data$cols)[1,], col2rgb(data$cols)[2,], col2rgb(data$cols)[3,], maxColorValue = 255)
#custom colors
data$cols[data$countries=='England'] <- rgb(184,20,45, maxColorValue=255)
data$cols[data$countries=='Netherlands'] <- rgb(252,101,0, maxColorValue=255)
data$cols[data$countries=='Brazil'] = rgb(0,129,0, maxColorValue=255)
data$cols[data$countries=='Argentina'] = rgb(123,179,242, maxColorValue=255)
data$cols[data$countries=='France'] = rgb(66,147,237, maxColorValue=255)
data$cols[data$countries=='Italy'] = rgb(1,2,250, maxColorValue=255)
data$cols[data$countries=='Spain'] = rgb(228,2,12, maxColorValue=255)
data$cols[data$countries=='Germany'] = rgb(0,0,0, maxColorValue=255)
data$cols[data$countries=='Denmark'] = rgb(181,12,36, maxColorValue=255)
data$cols[data$countries=='Uruguay'] = rgb(89,139,198, maxColorValue=255)
data$cols[data$countries=='Hungary'] = rgb(207,28,36, maxColorValue=255)
data$cols[data$countries=='Bohemia'] = rgb(252,4,4, maxColorValue=255)
data$cols[data$countries=='Czechoslovakia'] = rgb(252,4,4, maxColorValue=255)
data$cols[data$countries=='Bohemia/Moravia'] = rgb(252,4,4, maxColorValue=255)
data$cols[data$countries=='Czechia'] = rgb(252,4,4, maxColorValue=255)
data$cols[data$countries=='East Germany'] = rgb(0,0,0, maxColorValue=255)
data$cols[data$countries=='West Germany'] = rgb(0,0,0, maxColorValue=255)
data$cols[data$countries=='Austria'] = rgb(252,4,4, maxColorValue=255)
data$cols[data$countries=='Belgium'] = rgb(252,4,4, maxColorValue=255)
data$cols[data$countries=='Croatia'] = rgb(252,4,4, maxColorValue=255)
data$cols[data$countries=='Poland'] = rgb(252,4,4, maxColorValue=255)
data$cols[data$countries=='Soviet Union'] = rgb(159,22,23, maxColorValue=255)
data$cols[data$countries=='Russia'] = rgb(159,22,23, maxColorValue=255)
data$cols[data$countries=='Turkey'] = rgb(236,0,0, maxColorValue=255)
data$cols[data$countries=='Scotland'] = rgb(16,20,57, maxColorValue=255)
data$cols[data$countries=='South Africa'] = rgb(2,129,2, maxColorValue=255)
data$cols[data$countries=='Sweden'] = rgb(0,107,183, maxColorValue=255)
data$cols[data$countries=='Mexico'] = rgb(0,131,65, maxColorValue=255)
data$cols[data$countries=='Yugoslavia'] = rgb(218,0,1, maxColorValue=255)
data$cols[data$countries=='Paraguay'] = rgb(231,51,54, maxColorValue=255)
data$cols[data$countries=='Romania'] = rgb(253,211,10, maxColorValue=255)
data$cols[data$countries=='Portugal'] = rgb(103,14,48, maxColorValue=255)
data$cols[data$countries=='Bulgaria'] = rgb(0,166,81, maxColorValue=255)
data$cols[data$countries=='North Korea'] = rgb(0,136,55, maxColorValue=255)
data$cols[data$countries=='Ireland'] = rgb(0,126,0, maxColorValue=255)
data$cols[data$countries=='Colombia'] = rgb(0,0,98, maxColorValue=255)
data$cols[data$countries=='Chile'] = rgb(250,35,53, maxColorValue=255)
data$cols[data$countries=='Costa Rica'] = rgb(0,86,161, maxColorValue=255)
data$cols[data$countries=='Wales'] = rgb(10,174,65, maxColorValue=255)

jColors <- data$cols
names(jColors) <- data$countries

dark_c<-c('Germany', 'West Germany', 'East Germany','Italy','Chile','Belgium', 'Scotland', 'Brazil', 'Soviet Union', 'Russia', 'Portugal')
data$tcolors<-ifelse(data$countries%in%dark_c, 'white', 'black')
tColors <-data$tcolors
names(tColors) <- data$countries
# Subset ------------------------------------------------------------------------
data.s <-data %>% filter (order<11)
data.s2 <-data %>% filter (order<21, year>1945)
# year subsets
data.2019 <- data[data$year==2019,][1:25,]
# Static plot for a single year ------------------------------------------
sp = ggplot(data.2019, aes(x=reorder(countries, elos), y=elos, colour=countries, fill=countries)) +
  scale_fill_manual(values = jColors)+
  geom_bar(stat='identity', colour='black') +
  labs(x='', y='', title = 'Top 25 World Football Elo Ratings, March 2019' ) +
  coord_flip(expand=FALSE, ylim = c(1760,max(data.2019$elos)+30))+
  guides(color=F,fill=F)+
  theme_minimal(20)+
  theme(plot.title=element_text(family='Cairo', colour='black', size=26),
        axis.text.x = element_text(family='Cairo'),
        axis.text.y = element_text(family='Cairo'),
        plot.margin = unit(c(20, 10, 0, 0), "points"))+
  annotate("text", 3.5, 2149, label = 'Data: http://eloratings.net\n@DToshkov', hjust = 1, vjust=1, family = 'Cairo', size = 5) 
png('./output graphs/elos_2019.png', width=1000, height=660, res=96)
sp
dev.off()

# Dynamic plot, top 10  ------------------------------------------
p = ggplot(data.s, aes(-order, group=countries, colour=countries, fill=countries)) +
  scale_fill_manual(values = jColors)+
  geom_tile(aes(y = elos/2, height = elos, width = 0.75), alpha = 0.8, color='black') +
  labs(x='', y='', title='Top Ten World Football Elo Ratings:  {closest_state}',
       caption='Data: http://eloratings.net   Author: @DToshkov', subtitle = '  ') +
  annotate("text", -9, 2240, label = 'Data: http://eloratings.net\n@DToshkov', hjust = 1, vjust=1, family = 'Cairo', size = 5)+ 
  geom_text(aes(y=elos+5,label = as.character(elos)), colour='black', fontface='italic', family='Cairo', size=6,  hjust=0) +
  geom_text(aes(y = 1695, label = countries), hjust = 0, color=data.s$tcolors, family='Cairo', size=8) +
  coord_flip(expand=FALSE, ylim = c(1690,max(data.s$elos)+40))+
  theme_minimal(20) + guides(color=F,fill=F)+
  theme(plot.title=element_text(family='Cairo', colour='black', size=34, hjust=1, vjust=5),
        plot.caption = element_text(hjust = 1, family='Cairo', colour='darkgrey', size=14),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.text.x = element_text(family='Cairo'),
        plot.margin = unit(c(30, 30, 10, 10), "points")) +    
  transition_states(year, transition_length = 2, state_length = 8) +
  ease_aes('cubic-in-out') +   
  enter_grow()+
  exit_shrink()

animate(p, nframes = dim(data.s)[1]/10*3*10+10, fps = 20, end_pause = 10, width = 1000, height = 720)
anim_save("./output graphs/top10_elos_big.gif")
animate(p, nframes = dim(data.s)[1]/10*10+10, fps = 12, end_pause = 10, width = 900, height = 600)
anim_save("./output graphs/top10_elos_medium.gif")

# Dynamic plot, top 20, after 1946  ------------------------------------------
p2 = ggplot(data.s2, aes(-order, group=countries, colour=countries, fill=countries)) +
  scale_fill_manual(values = jColors)+
  geom_tile(aes(y = elos/2, height = elos, width = 0.75), alpha = 0.8, color='black') +
  labs(x='', y='', title='Top 20 World Football Elo Ratings:  {closest_state}',
       caption='Data: http://eloratings.net   Author: @DToshkov', subtitle = '  ') +
  annotate("text", -19, 2240, label = 'Data: http://eloratings.net\n@DToshkov', hjust = 1, vjust=1, family = 'Cairo', size = 5)+ 
  geom_text(aes(y=elos+5,label = as.character(elos)), colour='black', fontface='italic', size=4,  hjust=0)+ 
  geom_text(aes(y = 1605, label = countries), hjust = 0, color=data.s2$tcolors, size=6) +
  coord_flip(expand=FALSE, ylim = c(1600,max(data.s$elos)+40))+
  theme_minimal(20) + guides(color=F,fill=F)+
  theme(plot.title=element_text(family='Cairo', colour='black', size=34, hjust=1, vjust=5),
        plot.caption = element_text(hjust = 1, family='Cairo', colour='darkgrey', size=14),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.text.x = element_text(family='Cairo'),
        plot.margin = unit(c(30, 30, 10, 10), "points")) +
  transition_states(year, transition_length = 2, state_length = 8) +
  ease_aes('cubic-in-out') +   
  enter_grow()+
  exit_shrink()

animate(p2, nframes = dim(data.s2)[1]/20*10, fps = 12, end_pause = 10, width = 900, height = 780)
anim_save("./output graphs/top20_elos_medium.gif")

# Top 10 countires --------------------------------------------------------
data.top<-data%>%filter(year>1951)%>%group_by(countries)%>%mutate(sum_elo=sum(elos))%>%filter(year==2019)
data.top<-data.top[order(-data.top$sum_elo),]
top.countries<-data.top$countries[1:10]
data.top<-data%>%filter(year>1951, countries%in%top.countries)

data.top %<>% 
  group_by(year) %>% 
  arrange(-elos) %>% 
  mutate(order = 1:n()) %>%
  ungroup() %>%
  arrange(year, -elos)
data.top$order = as.numeric(as.character(data.top$order))

temp<-data.top%>%filter(year==2019)%>%select(countries, order)%>%rename(order2019=order)
data.top<-left_join(data.top, temp, by='countries')

# variable positions of the bars
p3 = ggplot(data.top, aes(-order, group=countries, colour=countries, fill=countries)) +
  geom_tile(aes(y = elos/2, height = elos, width = 0.75), alpha = 0.8, color='black') +
  scale_fill_manual(values = jColors)+
  labs(x='', y='', title='World Football Elo Ratings of Top 10 countries:  {closest_state}',
       caption='Data: http://eloratings.net   Author: @DToshkov', subtitle = '  ') +
  annotate("text", -9, 2220, label = 'Data: http://eloratings.net\n@DToshkov', hjust = 1, vjust=1, family = 'Cairo', size = 5)+ 
  geom_text(aes(y=elos+5,label = as.character(elos)), colour='black', fontface='italic', size=6,  hjust=0) +
  geom_text(aes(y = 1545, label = countries), hjust = 0, color=data.top$tcolors, size=8) +
  coord_flip(expand=FALSE, ylim = c(1540,max(data.top$elos)+30))+
  theme_minimal(20) + guides(color=F,fill=F)+
  theme(plot.title=element_text(family='Cairo', colour='black', size=30, hjust=1, vjust=5),
        plot.caption = element_text(hjust = 1, family='Cairo', colour='darkgrey', size=14),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.text.x = element_text(family='Cairo'),
        plot.margin = unit(c(30, 30, 10, 10), "points"))+  
  transition_states(year, transition_length = 2, state_length = 8) +
  ease_aes('cubic-in-out') +   
  enter_grow()+
  exit_shrink()

animate(p3, nframes = dim(data.top)[1]/10*3*10+10, fps = 20, end_pause = 10, width = 1000, height = 720)
anim_save("./output graphs/top10_10_elos.gif")

# fixed positions of the bars
p5 = ggplot(data.top, aes(-order2019, group=countries, colour=countries, fill=countries)) +
  geom_tile(aes(y = elos/2, height = elos, width = 0.75), alpha = 0.8, color='black') +
  scale_fill_manual(values = jColors)+
  labs(x='', y='', title='World Football Elo Ratings of Top 10 countries:  {closest_state}',
       caption='Data: http://eloratings.net   Author: @DToshkov', subtitle = '  ') +
  annotate("text", -9, 2220, label = 'Data: http://eloratings.net\n@DToshkov', hjust = 1, vjust=1, family = 'Cairo', size = 5)+ 
  geom_text(aes(y=elos+5,label = as.character(elos)), colour='black', fontface='italic', size=6,  hjust=0) +
  geom_text(aes(y = 1515, label = countries), hjust = 0, color=data.top$tcolors, size=8) +
  coord_flip(expand=FALSE, ylim = c(1510,max(data.top$elos)+30))+
  theme_minimal(20) + guides(color=F,fill=F)+
  theme(plot.title=element_text(family='Cairo', colour='black', size=30, hjust=1, vjust=5),
        plot.caption = element_text(hjust = 1, family='Cairo', colour='darkgrey', size=14),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.text.x = element_text(family='Cairo'),
        plot.margin = unit(c(30, 30, 10, 10), "points")) +    
  transition_states(year, transition_length = 2, state_length = 2) +
  ease_aes('cubic-in-out') +   
  enter_grow()+
  exit_shrink()

animate(p5, nframes = dim(data.top)[1]/10*3*10+10, fps = 20, end_pause = 10, width = 1000, height = 720)
anim_save("./output graphs/top10_10_fixed_elos_big.gif")
