library(rvest)

htmlToDf <- function(html) {
  
#this function is a mess and depends highly on the specific formatting on the source site wahlrecht.de. Goes to shit should they ever update

df <- html %>%
  html_element("body") %>%
  html_elements("table") %>%
  html_table(header = "TRUE")

time <- df[[2]][[1]] %>% tail(-3) %>% as.Date(format = "%d.%m.%Y")
Union <- df[[2]][["CDU/CSU"]] %>% tail(-3) %>% sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
SPD <- df[[2]][["SPD"]] %>% tail(-3) %>% sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
Gruene <- df[[2]][["GRÜNE"]] %>% tail(-3) %>% sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
FDP <- df[[2]][["FDP"]] %>% tail(-3) %>% sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
LINKE <- df[[2]][["LINKE"]] %>% tail(-3) %>% sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
AfD <- df[[2]][["AfD"]] %>% tail(-3) %>% sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})
Sonst <- df[[2]][["Sonstige"]] %>% tail(-3) %>% sapply(X = ., FUN = function(x) {(as.numeric(gsub(" %", "", gsub(",",".", x))))/100})

return(data.frame(time, Union, SPD, Gruene, FDP, LINKE, AfD, Sonst))
}

institutes = data.frame(row.names = c("allensbach", "emnid", "forsa", "politbarometer", "gms", "dimap", "insa", "yougov"))

for(name in row.names(institutes)) {
  institutes[name, "url"] = paste("https://www.wahlrecht.de/umfragen/", name, ".htm", sep = "")
}

for(name in row.names(institutes)) {
  institutes[name, "data"] = read_html(institutes[name, "url"]) %>% 
    htmlToDf
}