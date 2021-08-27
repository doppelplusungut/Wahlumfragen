#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rvest)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyverse)
library(slider)
library(tidyquant)
library(scales)
library(DT)

ndayaverage <- function(electiondata, n, date, party) {
    return(mean(electiondata[electiondata$time <= date & electiondata$time >= (date - n),][[party]]))
}

latest_surveys <- function(electiondata, n) {
    date = max(electiondata$time)
    parties = c("Union", "SPD", "LINKE", "Gruene", "AfD", "FDP")
    surevydata = data.frame(
        parties = parties,
        value = sapply(parties, FUN = function(party) {
            ndayaverage(electiondata, n, date, party)
        })
    )
}

smoothed <- function(electiondata, n) {
    for(party in c("Union", "SPD", "LINKE", "Gruene", "AfD", "FDP")) {
        vec = sapply(electiondata$time, FUN = function(date) {
            ndayaverage(electiondata, n, date, party)
        })
        electiondata[[paste("smoothed_",party,sep="")]] <- vec
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

electiondata <- exporteldata()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sonntagsfrage: Wenn nächsten Sonntag Bundestagswahl wäre..."),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("dates", label = h3("Umfragen von ... bis ..."), start = "2021-01-01", format = "dd.mm.yyyy"),
            sliderInput("smooth_days", label = h3("Anzahl Tage für rollierenden Schnitt"), min = 1, max = 30, value = 7, step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("feverCurve"),
           plotOutput("partyPie"),
           h2("Party test"),
           dataTableOutput("partytable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$feverCurve <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        d = input$smooth_days
        
        data = smoothed(electiondata, d)
        
        data = data[data$time >= input$dates[1] & electiondata$time <= input$dates[2],]

        ggplot(data = data, aes(x = time)) + 
            geom_line(aes(y = smoothed_Union), color = "black") + 
            geom_line(aes(y = smoothed_SPD), color = "red") +
            geom_line(aes(y = smoothed_Gruene), color = "green") +
            geom_line(aes(y = smoothed_AfD), color = "steelblue") + 
            geom_line(aes(y = smoothed_FDP), color = "yellow") + 
            geom_line(aes(y = smoothed_LINKE), color = "pink") +
            ggtitle("Zeitentwicklung der Umfragewerte großer Parteien") +
            scale_x_date(name = "Zeitpunkt", date_minor_breaks = "1 month") + 
            scale_y_continuous(name = "Ergebnis in Prozent", labels = scales::percent, limits = c(0,NA)) +
            theme_bw()
    })
    
    output$partyPie <- renderPlot({
        ggplot(data = latest_surveys(electiondata, input$smooth_days), aes(x = "", y = value, fill = parties)) +
            geom_bar(stat = "identity", width = 1) + 
            coord_polar("y", start = 0)
    })
    
    output$partytable <- renderDataTable({
        latest_surveys(electiondata, input$smooth_days)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
