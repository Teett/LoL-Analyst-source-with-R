library(googlesheets);library(tidyverse);library(lubridate);library(jsonlite)
key <- readLines("API/API key.txt", warn=F)
servers <- c("https://la1.api.riotgames.com","https://na1.api.riotgames.com");names(servers) =c("LAN","NA")
by_match <- "/lol/match/v4/timelines/by-match/"
#This line may ask to authenticate using a browser
gs_ls()
#get the match history
mh <- gs_url("https://docs.google.com/spreadsheets/d/1WAXDVqF0Bm2QvADV76uPd78n03oLLl_NJVFVfItHKHE/")
# get the raw match_history sheet
training_history <- gs_read(ss=mh, ws = "match_history", range = "B4:BA9")
#fixing for the lack of last character with an NA
fix <- training_history[length(training_history)-1]
names(fix) = paste0("X",as.integer(str_replace(names(fix),"X","")) + 2) 
training_history <- cbind(training_history,fix)
#Cleaning the links and dates
historial <- training_history %>%
  gather(Date, link) %>%
  mutate(Date = dmy(Date)) %>%
  filter(!is.na(Date))
#Extracting which side we played on
lados <- training_history %>%
  select(num_range("X",seq(2,length(lados), by = 2)))
colnames(lados) <- unique(historial$Date)
lados <- lados %>% gather(Date, lado) %>% 
  mutate(Date = ymd(Date))
#Joining the tables and cleaning up the link
scrims <- as.tbl(cbind(historial, lados$lado), stringsAsFactors = FALSE) %>%
  mutate(link = str_replace(link, "https://matchhistory.lan.leagueoflegends.com/es/#match-details/LA1/", "")) %>% 
  mutate(link = strtrim(link, 9)) %>% 
  mutate(`lados$lado` = as.character(`lados$lado`))
names(scrims) = c("Date","match_id","lado")

#Filtering the participant_ids based on the side data
#get_part_id <- function(x) {
  if (!is.na(x)){
  if (x == "azul") {
    participants <- c(1:5)
  } else if (x == "rojo"){
    participants <- c(6:10)
  }
  } else {
    participants <- NA
  }
  participants
}
#participants <- as.tbl(as.data.frame(map(scrims$lado,get_part_id))) ;names(participants) = paste("juego",1:length(participants))

#Construyendo función para extraer info general de los matches
#incluye: position, currentgold, totalgold, level, xp, minionsKilled, jungleMinionsKilled
extract_match_data <- function(match_id) {
  if (!is.na(match_id)) {
  fromJSON(paste0(servers[1],by_match,match_id,"?api_key=",key))[[1]][[1]]
  } else {
    NA
  }
}

#Aplicando la función de extracción de datos a los match de los scrims,
#se retorna una lista con los n juegos extraídos
#all_games <- map(scrims$match_id, extract_match_data)

#filtrando games en lado azul y añadiendo info general
blue_side <- scrims %>% filter(lado == "azul")
blue_side_games <-  map(blue_side$match_id, extract_match_data) %>% 
  map(~ .[1:5])
#filtrando games en lado rojo y añadiendo info general
red_side <- scrims %>% filter(lado == "rojo")
red_side_games <-  map(red_side$match_id, extract_match_data) %>% 
  map(~ .[6:10])

#mixing all the games regardless of the participant id, all these are our games
our_games <- purrr::flatten(list(blue_side_games, red_side_games));names(our_games) <- paste("game",1:length(our_games))
#A function to get data from a certain game x and a certain player y = (1=top,2=jg,3=mid,4=adc,5=supp)
obtain_essentials <- function(games,x,y){
  games[[x]][y]
}

#These are the matches separated by player
top <- map(our_games,1)
jungler <- map(our_games,2)
mid <- map(our_games,3)
adc <- map(our_games,4)
support <- map(our_games,5)

#Need to learn the scoped versions of dplyr functions to get through this
map_df(hobbler, ~ {summarize_all(c(.),mean)})
