library(dplyr)
library(lubridate)
library(ggrepel)


calendar<-collectNBACalendar(2016)

calendar$date2<-unlist(lapply(strsplit(gsub(",","",calendar$date)," "),function(x) paste(x[2:4],collapse = "-")))
calendar$date2<-as.Date(calendar$date2,"%b-%d-%Y")
calendar<-calendar%>%
  arrange(date2)

calendar<-calendar%>%
  filter(date2<='2016-04-13')


print(sapply(unique(calendar$home),
             function(x) calendar%>%
               filter((home==x | visitor==x))%>%
               nrow()))

calendar$home_location<-unlist(lapply(strsplit(calendar$home," "),function(x) paste(x[1:(length(x)-1)],collapse=" ")))
calendar$visitor_location<-unlist(lapply(strsplit(calendar$visitor," "),function(x) paste(x[1:(length(x)-1)],collapse=" ")))

calendar$home_location[calendar$home_location=="Portland Trail"]<-"Portland"
calendar$home_location[calendar$home_location=="Utah"]<-"Salt Lake City"
calendar$home_location[calendar$home_location=="Indiana"]<-"Indianapolis"
calendar$home_location[calendar$home_location=="Minnesota"]<-"Minneapolis"
calendar$home_location[calendar$home_location=="Golden State"]<-"San Francisco"
calendar$home_location[calendar$home_location=="Washington"]<-"Washington D.C."



# Download every latitude and longitude
cities<-unique(calendar$home_location)
pos<-geocode(cities)
citiesLocation<-data.frame(cities,pos)


# Every combination between two teams
distance<-expand.grid(unique(calendar$home_location),unique(calendar$home_location))
names(distance)<-c("team1","team2")

# Join the location (latitude and longitude) for each team.
distance<-merge(x=distance,
                y=citiesLocation,
                by.x="team1",
                by.y="cities",
                all.x=TRUE)
names(distance)[3:4]<-c("lon1","lat1")

distance<-merge(x=distance,
                y=citiesLocation,
                by.x="team2",
                by.y="cities",
                all.x=TRUE)
names(distance)[5:6]<-c("lon2","lat2")



distance$distanceKM<-apply(distance[,names(distance)%in%c('lon1','lat1','lon2','lat2')],1,function(x) earth.dist(x[1],x[2],x[3],x[4],R=6378.145))



randomCalendar <- calendar[sample(nrow(calendar),replace=FALSE),]



teams<-unique(calendar$home)
result<-rep(NA,100)
for(i in 1:length(result)){
  print(i)
  c_<-calendar[sample(nrow(calendar),replace=FALSE),]
  result[i]<-sum(sapply(teams,function(x) sum(nbaFlightsByTeam(c_,x,date=FALSE)$distance)))
}
#######################################

# First Type
#sum(sapply(teams,function(x) sum(nbaFlightsByTeam(calendar,x,date=FALSE)$distance)))
#last_calendar<-calendar[sample(1:nrow(calendar),nrow(calendar),replace=FALSE),]
sum(sapply(teams,function(x) sum(nbaFlightsByTeam(last_calendar,x,date=FALSE)$distance)))

n_calendars<-100
n_changes<-2
list_calendars<-list()
list_distance<-list()
n<-10

for(k in 1:n){
  last_distance<- sum(sapply(teams,function(x) sum(nbaFlightsByTeam(last_calendar,x,date=FALSE)$distance)))
  for(i in 1:n_calendars){
    print(paste0(k,"-",i))
    new_calendar <- last_calendar
    for(j in 1:n_changes){
      change<-sample(1:nrow(calendar),2,replace = TRUE)
      aux<-new_calendar[change[1],]
      new_calendar[change[1],]<-new_calendar[change[2],]
      new_calendar[change[2],]<-aux
    }
    new_distance<-sum(sapply(teams,function(x) sum(nbaFlightsByTeam(new_calendar,x,date=FALSE)$distance)))
    if(new_distance<=last_distance){
      print(new_distance)
      list_calendars[[i]]<-new_calendar
      list_distance[[i]]<-sum(sapply(teams,function(x) sum(nbaFlightsByTeam(new_calendar,x,date=FALSE)$distance)))
    }
    else{
      # print("next")
      list_distance[[i]]<-999999999
      list_calendars[[i]]<-NULL
       next
    }
  }
  if(min(unlist(list_distance))==999999999){next}
  candidate<-which(unlist(list_distance)==min(unlist(list_distance)))
  last_calendar<-list_calendars[[candidate]]
}


# write.csv(last_calendar,"project/Data-Science-Projects/NBA calendar/best_calendar_output1.csv",row.names = FALSE)

last_calendar


# Quick check: Every team must have 82 games
print(sapply(unique(last_calendar$home),
             function(x) last_calendar%>%
               filter((home==x | visitor==x))%>%
               nrow()))

print(sapply(unique(calendar$home),
             function(x) calendar%>%
               filter((home==x | visitor==x))%>%
               nrow()))


length(which(sapply(unique(last_calendar$home),
       function(x) last_calendar%>%
         filter((home==x | visitor==x))%>%
         nrow())<12))!=0

teams
nbaRouteMap(last_calendar,"Philadelphia 76ers")
sum(nbaFlightsByTeam(last_calendar,"Miami Heat",date=FALSE)$distance)
sum(nbaFlightsByTeam(calendar,"Miami Heat",date=TRUE)$distance)


#### Second Type

# Generate 100 random calendars and rank them.

n_calendars<-100
list_calendars<-list()
n_changes<-10

# Initial 100 random calendars
for(i in 1:n_calendars){
  print(i)
  list_calendars[[i]]<-calendar[sample(1:nrow(calendar),nrow(calendar),replace = FALSE),]
  list_distance[[i]]<-sum(sapply(teams,function(x) sum(nbaFlightsByTeam(list_calendars[[i]],x,date=FALSE)$distance)))
}


new_calendar_list<-list()
new_distance_list<-list()

# Select top 40 calendars + 10 random from the last 60
candidates<-c(order(unlist(list_distance))[1:40],sample(order(unlist(list_distance))[41:100],10,replace = TRUE))

####
for(i in 1:50){
  print(i)
  d<-sum(sapply(teams,function(x) sum(nbaFlightsByTeam(list_calendars[[candidates[i]]],x,date=FALSE)$distance)))
  new_d<-d+1
  count <- 0
  n_changes<-10
  while(d<new_d && count<100 && n_changes!=0){
    aux1<-list_calendars[[candidates[i]]]
    print(paste0(i,"-",count," - changes: ",n_changes))
    for(j in 1:n_changes){
      change<-sample(1:nrow(aux1),2,replace = TRUE)
      aux2<-aux1[change[1],]
      aux1[change[1],]<-aux1[change[2],]
      aux1[change[2],]<-aux2 
    }
    new_d<-sum(sapply(teams,function(x) sum(nbaFlightsByTeam(aux1,x,date=FALSE)$distance)))
    count<-count+1
    if(count>=10 && count%%10==0){n_changes<-n_changes-1}
  }
  print(paste0(d,"-",new_d))
  new_calendar_list[[i]]<-aux1
  new_distance_list[[i]]<-new_d
}

# Select top 40 calendars + 10 random from the last 60
candidates<-c(order(unlist(new_distance_list))[1:40],sample(order(unlist(new_distance_list))[41:50],10,replace = TRUE))

min(unlist(new_distance_list))

list_calendars<-new_calendar_list
