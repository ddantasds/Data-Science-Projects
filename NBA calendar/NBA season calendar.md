

```R
library(RCurl)
library(XML)
library(ggmap)
library(dplyr)
library(lubridate)
```

# Overview

NBA have 30 teams playing 82 games each during a regular season

Can we optmize the NBA and provide a better with lower distance travel?


```R

```

# Data


```R
calendar<-data.frame("date"=as.character(),
                     "time"=as.character(),
                     "visitor"=as.character(),
                     "visitor_pts"=as.numeric(),
                     "home"=as.character(),
                     "home_pts"=as.numeric())
```

### Scrapping data from Internet

Collecting the calendar from https://www.basketball-reference.com


```R
months<-tolower(month.name)
years<-2016

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
```


```R
calendar%>%
  filter((home=="Los Angeles Lakers" | visitor=="Los Angeles Lakers"))%>%
arrange(date2)%>%
nrow()
```


82



```R
head(calendar)
```


<table>
<thead><tr><th scope=col>date</th><th scope=col>time</th><th scope=col>visitor</th><th scope=col>visitor_pts</th><th scope=col>home</th><th scope=col>home_pts</th><th scope=col>season</th></tr></thead>
<tbody>
	<tr><td>Fri, Jan 1, 2016  </td><td>8:00 pm           </td><td>New York Knicks   </td><td>81                </td><td>Chicago Bulls     </td><td>108               </td><td>2016              </td></tr>
	<tr><td>Fri, Jan 1, 2016  </td><td>10:30 pm          </td><td>Philadelphia 76ers</td><td>84                </td><td>Los Angeles Lakers</td><td>93                </td><td>2016              </td></tr>
	<tr><td>Fri, Jan 1, 2016  </td><td>7:30 pm           </td><td>Dallas Mavericks  </td><td>82                </td><td>Miami Heat        </td><td>106               </td><td>2016              </td></tr>
	<tr><td>Fri, Jan 1, 2016  </td><td>7:30 pm           </td><td>Charlotte Hornets </td><td>94                </td><td>Toronto Raptors   </td><td>104               </td><td>2016              </td></tr>
	<tr><td>Fri, Jan 1, 2016  </td><td>7:00 pm           </td><td>Orlando Magic     </td><td>91                </td><td>Washington Wizards</td><td>103               </td><td>2016              </td></tr>
	<tr><td>Sat, Jan 2, 2016  </td><td>3:00 pm           </td><td>Brooklyn Nets     </td><td>100               </td><td>Boston Celtics    </td><td>97                </td><td>2016              </td></tr>
</tbody>
</table>



### Fixing date format


```R
calendar$date2<-unlist(lapply(strsplit(gsub(",","",calendar$date)," "),function(x) paste(x[2:4],collapse = "-")))
calendar$date2<-as.Date(calendar$date2,"%b-%d-%Y")
calendar<-calendar%>%
  arrange(date2)
```


```R
calendar%>%
    filter(complete.cases(.))%>%
    group_by()%>%
    summarise(min(date2),max(date2))
```


<table>
<thead><tr><th scope=col>min(date2)</th><th scope=col>max(date2)</th></tr></thead>
<tbody>
	<tr><td>2015-10-27</td><td>2016-06-19</td></tr>
</tbody>
</table>



For this calendar there are playoffs games and we are not interested on playoffs once we are evaluating the distance traveled during the regular season.

The 2015-16 season ranged from 10-27-2015 to 04-13-2016 (https://en.wikipedia.org/wiki/2015%E2%80%9316_NBA_season)

### Filter Regular Season Games


```R
calendar<-calendar%>%
            filter(date2<='2016-04-13')
```


```R
# Check if every team have 82 games
sapply(unique(calendar$home),
       function(x) calendar%>%
  filter((home==x | visitor==x))%>%
  nrow())
```


<dl class=dl-horizontal>
	<dt>Atlanta Hawks</dt>
		<dd>82</dd>
	<dt>Chicago Bulls</dt>
		<dd>82</dd>
	<dt>Golden State Warriors</dt>
		<dd>82</dd>
	<dt>Boston Celtics</dt>
		<dd>82</dd>
	<dt>Brooklyn Nets</dt>
		<dd>82</dd>
	<dt>Detroit Pistons</dt>
		<dd>82</dd>
	<dt>Houston Rockets</dt>
		<dd>82</dd>
	<dt>Los Angeles Lakers</dt>
		<dd>82</dd>
	<dt>Memphis Grizzlies</dt>
		<dd>82</dd>
	<dt>Miami Heat</dt>
		<dd>82</dd>
	<dt>Milwaukee Bucks</dt>
		<dd>82</dd>
	<dt>Oklahoma City Thunder</dt>
		<dd>82</dd>
	<dt>Orlando Magic</dt>
		<dd>82</dd>
	<dt>Phoenix Suns</dt>
		<dd>82</dd>
	<dt>Portland Trail Blazers</dt>
		<dd>82</dd>
	<dt>Sacramento Kings</dt>
		<dd>82</dd>
	<dt>Toronto Raptors</dt>
		<dd>82</dd>
	<dt>Indiana Pacers</dt>
		<dd>82</dd>
	<dt>Los Angeles Clippers</dt>
		<dd>82</dd>
	<dt>New York Knicks</dt>
		<dd>82</dd>
	<dt>Cleveland Cavaliers</dt>
		<dd>82</dd>
	<dt>Denver Nuggets</dt>
		<dd>82</dd>
	<dt>Philadelphia 76ers</dt>
		<dd>82</dd>
	<dt>San Antonio Spurs</dt>
		<dd>82</dd>
	<dt>New Orleans Pelicans</dt>
		<dd>82</dd>
	<dt>Washington Wizards</dt>
		<dd>82</dd>
	<dt>Charlotte Hornets</dt>
		<dd>82</dd>
	<dt>Minnesota Timberwolves</dt>
		<dd>82</dd>
	<dt>Dallas Mavericks</dt>
		<dd>82</dd>
	<dt>Utah Jazz</dt>
		<dd>82</dd>
</dl>



### Define Location of Games


```R
calendar$home_location<-unlist(lapply(strsplit(calendar$home," "),function(x) paste(x[1:(length(x)-1)],collapse=" ")))
calendar$visitor_location<-unlist(lapply(strsplit(calendar$visitor," "),function(x) paste(x[1:(length(x)-1)],collapse=" ")))
```


```R
unique(calendar$home_location)
```


<ol class=list-inline>
	<li>'Atlanta'</li>
	<li>'Chicago'</li>
	<li>'Golden State'</li>
	<li>'Boston'</li>
	<li>'Brooklyn'</li>
	<li>'Detroit'</li>
	<li>'Houston'</li>
	<li>'Los Angeles'</li>
	<li>'Memphis'</li>
	<li>'Miami'</li>
	<li>'Milwaukee'</li>
	<li>'Oklahoma City'</li>
	<li>'Orlando'</li>
	<li>'Phoenix'</li>
	<li>'Portland Trail'</li>
	<li>'Sacramento'</li>
	<li>'Toronto'</li>
	<li>'Indiana'</li>
	<li>'New York'</li>
	<li>'Cleveland'</li>
	<li>'Denver'</li>
	<li>'Philadelphia'</li>
	<li>'San Antonio'</li>
	<li>'New Orleans'</li>
	<li>'Washington'</li>
	<li>'Charlotte'</li>
	<li>'Minnesota'</li>
	<li>'Dallas'</li>
	<li>'Utah'</li>
</ol>



Based on the name of the home team we can identify the game location. For example, when the home team is 'Chicago Bulls' we know the game was hosted in Chicago.

In a simple example, for a match between 'Chicago Bulls' and 'Memphis Grizzles' where the home team is 'Chicago Bulls' we assume that there was travel from Memphis to Chicago.

Even though the home_location was able to identify the location of games we still had to do some manual adjustments. For example, Golden State -> San Franciso


```R
calendar$home_location[calendar$home_location=="Portland Trail"]<-"Portland"
calendar$home_location[calendar$home_location=="Utah"]<-"Salt Lake City"
calendar$home_location[calendar$home_location=="Indiana"]<-"Indianapolis"
calendar$home_location[calendar$home_location=="Minnesota"]<-"Minneapolis"
calendar$home_location[calendar$home_location=="Golden State"]<-"San Francisco"
calendar$home_location[calendar$home_location=="Washington"]<-"Washington D.C."
```

### Latitude and Longitude for the Cities where the Teams are located


```R
cities<-unique(calendar$home_location)
pos<-geocode(cities)
citiesLocation<-data.frame(cities,pos)
```

    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Atlanta&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Chicago&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=San+Francisco&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Boston&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Brooklyn&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Detroit&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Houston&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Los+Angeles&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Memphis&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Miami&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Milwaukee&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Oklahoma+City&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Orlando&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Phoenix&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Portland&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Sacramento&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Toronto&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Indianapolis&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=New+York&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Cleveland&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Denver&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Philadelphia&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=San+Antonio&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=New+Orleans&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Washington&sensor=false
    .Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Charlotte&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Minneapolis&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Dallas&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Salt+Lake+City&sensor=false
    


```R
head(citiesLocation)
```


<table>
<thead><tr><th scope=col>cities</th><th scope=col>lon</th><th scope=col>lat</th></tr></thead>
<tbody>
	<tr><td>Atlanta      </td><td> -84.38798   </td><td>33.74900     </td></tr>
	<tr><td>Chicago      </td><td> -87.62980   </td><td>41.87811     </td></tr>
	<tr><td>San Francisco</td><td>-122.41942   </td><td>37.77493     </td></tr>
	<tr><td>Boston       </td><td> -71.05888   </td><td>42.36008     </td></tr>
	<tr><td>Brooklyn     </td><td> -73.94416   </td><td>40.67818     </td></tr>
	<tr><td>Detroit      </td><td> -83.04575   </td><td>42.33143     </td></tr>
</tbody>
</table>



### Calculate the Distance between Teams based on Latitude and Longitude


```R
# Function to calculate distance in kilometers between two points
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
```


```R
distance<-expand.grid(unique(calendar$home_location),unique(calendar$home_location))
names(distance)<-c("team1","team2")

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
```

All Combinations


```R
head(distance)
```


<table>
<thead><tr><th scope=col>team2</th><th scope=col>team1</th><th scope=col>lon1</th><th scope=col>lat1</th><th scope=col>lon2</th><th scope=col>lat2</th><th scope=col>distanceKM</th></tr></thead>
<tbody>
	<tr><td>Atlanta    </td><td>Atlanta    </td><td> -84.38798 </td><td>33.74900   </td><td>-84.38798  </td><td>33.749     </td><td>   0.000   </td></tr>
	<tr><td>Atlanta    </td><td>New York   </td><td> -74.00594 </td><td>40.71278   </td><td>-84.38798  </td><td>33.749     </td><td>1201.665   </td></tr>
	<tr><td>Atlanta    </td><td>Phoenix    </td><td>-112.07404 </td><td>33.44838   </td><td>-84.38798  </td><td>33.749     </td><td>2559.549   </td></tr>
	<tr><td>Atlanta    </td><td>Denver     </td><td>-104.99025 </td><td>39.73924   </td><td>-84.38798  </td><td>33.749     </td><td>1949.539   </td></tr>
	<tr><td>Atlanta    </td><td>San Antonio</td><td> -98.49363 </td><td>29.42412   </td><td>-84.38798  </td><td>33.749     </td><td>1420.095   </td></tr>
	<tr><td>Atlanta    </td><td>Milwaukee  </td><td> -87.90647 </td><td>43.03890   </td><td>-84.38798  </td><td>33.749     </td><td>1078.468   </td></tr>
</tbody>
</table>




```R
result<-rep(NA,nrow(distance))
for(i in 1:nrow(distance)){
  result[i]<-earth.dist(distance$lon1[i],distance$lat1[i],distance$lon2[i],distance$lat2[i],R=6378.145)
}
distance$distanceKM<-result
```


```R
head(distance)
```


<table>
<thead><tr><th scope=col>team2</th><th scope=col>team1</th><th scope=col>lon1</th><th scope=col>lat1</th><th scope=col>lon2</th><th scope=col>lat2</th><th scope=col>distanceKM</th></tr></thead>
<tbody>
	<tr><td>Atlanta    </td><td>Atlanta    </td><td> -84.38798 </td><td>33.74900   </td><td>-84.38798  </td><td>33.749     </td><td>   0.000   </td></tr>
	<tr><td>Atlanta    </td><td>New York   </td><td> -74.00594 </td><td>40.71278   </td><td>-84.38798  </td><td>33.749     </td><td>1201.665   </td></tr>
	<tr><td>Atlanta    </td><td>Phoenix    </td><td>-112.07404 </td><td>33.44838   </td><td>-84.38798  </td><td>33.749     </td><td>2559.549   </td></tr>
	<tr><td>Atlanta    </td><td>Denver     </td><td>-104.99025 </td><td>39.73924   </td><td>-84.38798  </td><td>33.749     </td><td>1949.539   </td></tr>
	<tr><td>Atlanta    </td><td>San Antonio</td><td> -98.49363 </td><td>29.42412   </td><td>-84.38798  </td><td>33.749     </td><td>1420.095   </td></tr>
	<tr><td>Atlanta    </td><td>Milwaukee  </td><td> -87.90647 </td><td>43.03890   </td><td>-84.38798  </td><td>33.749     </td><td>1078.468   </td></tr>
</tbody>
</table>



### Function to calculate Distance traveled by a Team during the season

This function returns a data frame with flights for a specific team during the season


```R
nbaFlightsByTeam<-function(base,team){
  aux<-base%>%
    filter((home==team | visitor==team))%>%
    arrange(date2)
  
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
```


```R
PHI_flights<-nbaFlightsByTeam(calendar,"Philadelphia 76ers")
```


```R
head(PHI_flights)
```


<table>
<thead><tr><th scope=col>flight_from</th><th scope=col>flight_to</th><th scope=col>distance</th></tr></thead>
<tbody>
	<tr><td>Philadelphia</td><td>Boston      </td><td> 436.1181   </td></tr>
	<tr><td>Boston      </td><td>Philadelphia</td><td> 436.1181   </td></tr>
	<tr><td>Philadelphia</td><td>Philadelphia</td><td>   0.0000   </td></tr>
	<tr><td>Philadelphia</td><td>Milwaukee   </td><td>1115.2003   </td></tr>
	<tr><td>Milwaukee   </td><td>Cleveland   </td><td> 539.5069   </td></tr>
	<tr><td>Cleveland   </td><td>Philadelphia</td><td> 576.9254   </td></tr>
</tbody>
</table>



To get the total kilometers traveled by the Philadelphia 76ers during the 2015-16 regular season we just have to sum the variable distance.


```R
sum(PHI_flights$distance) #75620.3841074576
```


75620.3841074576



```R
lal<-nbaFlightsByTeam(calendar,"Los Angeles Lakers")
```


```R
sum(lal$distance) #78106.9400997549
```


78106.9400997549



```R
sum(nbaFlightsByTeam(calendar,"Miami Heat")$distance)
```


90774.723034815



```R
teams <- unique(calendar$home)
```


<ol class=list-inline>
	<li>'Atlanta Hawks'</li>
	<li>'Chicago Bulls'</li>
	<li>'Golden State Warriors'</li>
	<li>'Boston Celtics'</li>
	<li>'Brooklyn Nets'</li>
	<li>'Detroit Pistons'</li>
	<li>'Houston Rockets'</li>
	<li>'Los Angeles Lakers'</li>
	<li>'Memphis Grizzlies'</li>
	<li>'Miami Heat'</li>
	<li>'Milwaukee Bucks'</li>
	<li>'Oklahoma City Thunder'</li>
	<li>'Orlando Magic'</li>
	<li>'Phoenix Suns'</li>
	<li>'Portland Trail Blazers'</li>
	<li>'Sacramento Kings'</li>
	<li>'Toronto Raptors'</li>
	<li>'Indiana Pacers'</li>
	<li>'Los Angeles Clippers'</li>
	<li>'New York Knicks'</li>
	<li>'Cleveland Cavaliers'</li>
	<li>'Denver Nuggets'</li>
	<li>'Philadelphia 76ers'</li>
	<li>'San Antonio Spurs'</li>
	<li>'New Orleans Pelicans'</li>
	<li>'Washington Wizards'</li>
	<li>'Charlotte Hornets'</li>
	<li>'Minnesota Timberwolves'</li>
	<li>'Dallas Mavericks'</li>
	<li>'Utah Jazz'</li>
</ol>




```R
total_distance_by_team<-sapply(teams,function(x) sum(nbaFlightsByTeam(calendar,x)$distance))
```


```R
total_distance_by_team[order(total_distance_by_team,decreasing=TRUE)]
```


<dl class=dl-horizontal>
	<dt>Washington Wizards</dt>
		<dd>162390.79249428</dd>
	<dt>Miami Heat</dt>
		<dd>90774.723034815</dd>
	<dt>Boston Celtics</dt>
		<dd>89905.2014179597</dd>
	<dt>Orlando Magic</dt>
		<dd>88898.8876585554</dd>
	<dt>Golden State Warriors</dt>
		<dd>87413.0180373452</dd>
	<dt>Minnesota Timberwolves</dt>
		<dd>86649.3575392695</dd>
	<dt>San Antonio Spurs</dt>
		<dd>84908.7771131942</dd>
	<dt>Portland Trail Blazers</dt>
		<dd>84162.6407696086</dd>
	<dt>Houston Rockets</dt>
		<dd>82513.9601189975</dd>
	<dt>Sacramento Kings</dt>
		<dd>81895.3524800496</dd>
	<dt>Memphis Grizzlies</dt>
		<dd>80901.2596970622</dd>
	<dt>Phoenix Suns</dt>
		<dd>79741.6880092132</dd>
	<dt>Dallas Mavericks</dt>
		<dd>78689.609713861</dd>
	<dt>Los Angeles Lakers</dt>
		<dd>78106.9400997549</dd>
	<dt>Oklahoma City Thunder</dt>
		<dd>78004.532472809</dd>
	<dt>New Orleans Pelicans</dt>
		<dd>77235.0452195018</dd>
	<dt>Utah Jazz</dt>
		<dd>76250.5617452304</dd>
	<dt>Philadelphia 76ers</dt>
		<dd>75620.3841074576</dd>
	<dt>Los Angeles Clippers</dt>
		<dd>75281.2246144345</dd>
	<dt>Atlanta Hawks</dt>
		<dd>74646.7406899963</dd>
	<dt>Denver Nuggets</dt>
		<dd>74320.815159516</dd>
	<dt>Charlotte Hornets</dt>
		<dd>74156.5834878511</dd>
	<dt>New York Knicks</dt>
		<dd>73635.4347897638</dd>
	<dt>Chicago Bulls</dt>
		<dd>71553.1539265964</dd>
	<dt>Toronto Raptors</dt>
		<dd>71370.7537846694</dd>
	<dt>Milwaukee Bucks</dt>
		<dd>71173.3079974254</dd>
	<dt>Brooklyn Nets</dt>
		<dd>69089.2339753818</dd>
	<dt>Detroit Pistons</dt>
		<dd>68647.6981426709</dd>
	<dt>Indiana Pacers</dt>
		<dd>66332.2533917296</dd>
	<dt>Cleveland Cavaliers</dt>
		<dd>65417.5979489366</dd>
</dl>




```R
barplot(total_distance_by_team)
```


![png](output_44_0.png)



```R
library(plotly)
set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
myPlot <- plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat)
embed_notebook(myPlot)
```


<iframe src="plotlyJupyterHTML/9681c97c42dd31c3a84f7b52200601c5.html" width="100%" height="400" id="igraph" scrolling="no" seamless="seamless" frameBorder="0"> </iframe>

