

#
# Pre-defined Functions for NBA calendar project
#

library(RCurl)
library(XML)
library(xml2)
library(rvest)

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
