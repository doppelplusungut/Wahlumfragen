pacman::p_load(
  shiny,
  rvest,
  ggplot2,
  reshape2,
  dplyr,
  tidyverse,      # for data management and viz
  slider,         # for calculating moving averages
  tidyquant       # for calculating moving averages within ggplot
)

smoothed <- function(electiondata, d) {
  for(party in c("Union", "SPD", "LINKE", "Gruene", "AfD", "FDP")) {
    vec = sapply(electiondata$time, FUN = function(x) {
      mean(electiondata[electiondata$time <= x & electiondata$time >= (x - d),][[party]])
    })
    electiondata[[paste("smoothed_",party)]] <- vec
  }
  return(electiondata)
}

htmlToDf <- function(html, inst) {
  
#this function is a mess and depends highly on the specific formatting on the source site wahlrecht.de. Goes to shit should they ever update

df <- html %>%
  html_element("body") %>%
  html_elements("table") %>%
  html_table(header = "TRUE")

i = 0

time <- df[[2]][[1]] %>% 
  as.character() %>%
  as.Date(format = "%d.%m.%Y")

i <- which.min(is.na(time))
i <- i-1

time <- time %>%
  tail(-i)
  
Union <- df[[2]][["CDU/CSU"]] %>% 
  tail(-i) %>% 
  sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
SPD <- df[[2]][["SPD"]] %>% 
  tail(-i) %>% 
  sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
Gruene <- df[[2]][["GRÜNE"]] %>% 
  tail(-i) %>% 
  sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
FDP <- df[[2]][["FDP"]] %>% 
  tail(-i) %>% 
  sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
LINKE <- df[[2]][["LINKE"]] %>% 
  tail(-i) %>% 
  sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
AfD <- df[[2]][["AfD"]] %>% 
  tail(-i) %>% 
  sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
Sonst <- df[[2]][["Sonstige"]] %>% 
  tail(-i) %>% 
  sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
institute = rep(inst,length(time))

return(data.frame(time, Union, SPD, Gruene, FDP, LINKE, AfD, Sonst, institute))
}

exporteldata <- function() {
  institutes = data.frame(row.names = c("allensbach", "emnid", "forsa", "politbarometer", "gms", "dimap", "insa", "yougov"))
  
  for(name in row.names(institutes)) {
    institutes[name, "url"] = paste("https://www.wahlrecht.de/umfragen/", name, ".htm", sep = "")
  }
  
  electiondata <- data.frame(time = as.Date(character()), Union = numeric(), SPD = numeric(), Gruene = numeric(), FDP = numeric(), LINKE = numeric(), AfD = numeric(), Sonst = numeric(), institute = character())
  
  for(name in row.names(institutes)) {
    h = read_html(institutes[name,"url"])
    instdata = htmlToDf(h,name)
    electiondata = rbind(electiondata, instdata)
  }
  
  electiondata[is.na(electiondata)] <- 0
  return(electiondata)
}
