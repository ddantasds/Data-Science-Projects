

#
# Pre-defined Functions for NBA calendar project
#

library(RCurl)
library(XML)
library(xml2)
library(rvest)


# Function to collect NBA calendar for a specific year
collectNBACalendar<-function(years){
  
  calendar<-data.frame("date"=as.character(),
                       "time"=as.character(),
                       "visitor"=as.character(),
                       "visitor_pts"=as.numeric(),
                       "home"=as.character(),
                       "home_pts"=as.numeric())

  months<-tolower(month.name)

  for(j in 1:length(years)){
    
    url<-paste0("https://www.basketball-reference.com/leagues/NBA_",years[j],"_games-",months[1],".html")
    html <- xml2::read_html(url)
    node <- rvest::html_node(html, "table")
    table <- rvest::html_table(node, header = TRUE)
    table<-table[,1:6]
    
    names(table)<-c("date","time","visitor","visitor_pts","home","home_pts")
    
    for(i in 2:length(months)){
      url<-url<-paste0("https://www.basketball-reference.com/leagues/NBA_",years[j],"_games-",months[i],".html")
      if(url.exists(url)){
        html <- xml2::read_html(url)
        node <- rvest::html_node(html, "table")
        aux <- rvest::html_table(node, header = TRUE)
        aux<-aux[,1:6]
        names(aux)<-c("date","time","visitor","visitor_pts","home","home_pts")
        table<-rbind(table,aux)
      }
      else{
        next
      }
    }
    table$season<-years[j]
    calendar<-rbind(calendar,table)
  }
  return(calendar)
}


## Function to calculate distance in kilometers between two points
# reference: http://andrew.hedges.name/experiments/haversine/
earth.dist <- function (lon1, lat1, lon2, lat2, R)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- R * c
  real.d <- min(abs((R*2) - d), d)
  return(real.d)
}


# Function to calculate distance traveled by team
nbaFlightsByTeam<-function(base,team,date=TRUE){
  # Date is TRUE if we want to calculate the distance respecting the time. The FALSE is used when trying to find a new calendar
    if(date==TRUE){
        aux<-base%>%
        filter((home==team | visitor==team))%>%
        arrange(date2)
    }
    else{
        aux<-base%>%
        filter((home==team | visitor==team))
    }
    
  
  
  initial = names(tail(sort(table(aux$home_location)),1))
  
  flight_from = c(initial,rep(NA,nrow(aux)-1))
  flight_to = c(rep(NA,(nrow(aux)-1)),initial)
  d = rep(NA,nrow(aux))
  
  for(i in 1:(nrow(aux)-1)){
    flight_to[i] = aux$home_location[i]
    flight_from[i+1] = flight_to[i]
    d[i] = distance$distance[which(distance$team1==flight_from[i] & distance$team2==flight_to[i])]
  }
  d[nrow(aux)] = distance$distance[which(distance$team1==flight_from[nrow(aux)] & distance$team2==flight_to[nrow(aux)])]
  return(data.frame(flight_from,flight_to,"distance"=d))
}