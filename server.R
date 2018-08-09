# server.R

##################################
# JP Mariano
# FIT5147 Data Exploration and Visualisation
# Graduate Diploma of Data Science
# Monash University
##################################

library(shiny)
library(ggplot2)
library(leaflet)
library(reshape)

# read data
growth.df <- read.csv(file="data/growth_of_the_games.csv",
               head=TRUE, fileEncoding='UTF-8-BOM')

results.df <- read.csv(file="data/results_all_per_year_with_code.csv",
                head=TRUE, fileEncoding='UTF-8-BOM', stringsAsFactors=FALSE)

sports.df <- read.csv(file="data/results_all_per_sport.csv",
                      head=TRUE, fileEncoding='UTF-8-BOM')
sports.df$total <- (sports.df$gold + sports.df$silver + sports.df$bronze)

# years
uniqueYears <- as.list(sort(unique(results.df$year), decreasing=T)) # decreasing

# sports
uniqueSports <- as.list(sort(unique(sports.df$sport)))

# regions/continents
uniqueRegions <- as.list(sort(unique(results.df$continent)))
uniqueRegions <- c('All', uniqueRegions)

# region/continent colors
region.colors <- rainbow(6, alpha=NULL)

# color for each region/continent
getRegionColor <- function(df) {
  sapply(df$continent, function(continent) {
    #cat(file=stderr(), paste0(continent, ' '))
    if (continent=='Asia') { region.colors[1] }
    else if (continent=='Africa') { region.colors[2] }
    else if (continent=='Americas') { region.colors[3] }
    else if (continent=='Caribbean') { region.colors[4] }
    else if (continent=='Europe') { region.colors[5] }
    else if (continent=='Oceania') { region.colors[6] }
    else { 'gray' }
  })
}

# base url for flag images
#flagBaseUrl <- 'http://www.thecgf.com/media/flags/'
# changed to local subdir under www
flagBaseUrl <- 'flags/'


shinyServer(function(input, output, session) {
  
  ###################################
  # Growth data
  ###################################
  
  growth.misc.data <- reactive({
    if (input$growthProp=='Countries') {
      y <- growth.df$participating.countries
      y.name <- 'participating.countries'
      subtitle <- 'Number of Participating Countries'
      color <- 'red'
      styles <- list(countries=paste0('color:', color, '; font-style:italic'))
    } else if (input$growthProp=='Sports') {
      y <- growth.df$sports
      y.name <- 'sports'
      subtitle <- 'Number of Sports Played'
      color <- 'blue'
      styles <- list(sports=paste0('color:', color, '; font-style:italic'))
    } else if (input$growthProp=='Events') {
      y <- growth.df$events
      y.name <- 'events'
      subtitle <- 'Number of Events Contested'
      color <- 'green'
      styles <- list(events=paste0('color:', color, '; font-style:italic'))
    } else {
      y <- growth.df$athletes
      y.name <- 'athletes'
      subtitle <- 'Number of Participating Athletes'
      color <- 'orange'
      styles <- list(athletes=paste0('color:', color, '; font-style:italic'))
    }
    
    # save to a list
    list(y=y, y.name=y.name, subtitle=subtitle, color=color, styles=styles)
  })
  
  output$growthPlot <- renderPlot({
    ggplot(growth.df, aes(x=year, y=growth.misc.data()[['y']], 
                          color=I(growth.misc.data()[['color']]))) + 
      geom_point(size=4) + geom_line() +
      labs(x='Year', y=input$growthProp,
           title='Growth of the Commonwealth Games', subtitle=growth.misc.data()[['subtitle']]) +
      theme(plot.title=element_text(size=20, hjust=0.5, face="bold")) +
      theme(plot.subtitle=element_text(size=18, hjust=0.5, color=growth.misc.data()[['color']])) +
      scale_x_continuous(breaks=seq(1930, 2014, by=4)) +
      scale_y_continuous(labels=function(x) { floor(x) })
  })
  
  output$yearInfo <- renderUI({
    if (!is.null(input$growth.hover)) {
      res <- nearPoints(growth.df, input$growth.hover, 
                        'year', growth.misc.data()[['y.name']])
      if (nrow(res) > 0) {
        div(id="growth-plot-info",
          div(id="growth-year-logo",
            img(src=paste0(res$year, ".gif"))
          ),
          div(id="growth-year-info",
            tags$table(
              tags$tr(tags$td(class="cell-name", "Official Name"), 
                      tags$td(class="cell-value", res$official.name)),
              tags$tr(tags$td(class="cell-name", "Year"), 
                      tags$td(class="cell-value", res$year)),
              tags$tr(tags$td(class="cell-name", "Host City"), 
                      tags$td(class="cell-value", paste0(res$city, ', ', res$country))),
              tags$tr(tags$td(class="cell-name", "Countries"), 
                      tags$td(class="cell-value", res$participating.countries), style=growth.misc.data()[['styles']][['countries']]),
              tags$tr(tags$td(class="cell-name", "Sports"), 
                      tags$td(class="cell-value", res$sports), style=growth.misc.data()[['styles']][['sports']]),
              tags$tr(tags$td(class="cell-name", "Events"), 
                      tags$td(class="cell-value", res$events), style=growth.misc.data()[['styles']][['events']]),
              tags$tr(tags$td(class="cell-name", "Athletes"), 
                      tags$td(class="cell-value", res$athletes), style=growth.misc.data()[['styles']][['athletes']])
            )
          )          
        )
      } else {
        p(class="plot-note", "Hover on a point on the line graph to get more information.")
      }
    } else {
      p(class="plot-note", "Hover on a point on the line graph to get more information.") 
    }
  })
  
  ###################################
  # Results Tally
  ###################################
  
  # update input dropdowns
  updateSelectInput(session, "resultsYearInput", choices=uniqueYears)
  updateSelectInput(session, "resultsRegionInput", choices=uniqueRegions)
  
  results.data <- reactive({
    data <- results.df
    # filters
    data <- data[data$year == input$resultsYearInput, ] 
    if (input$resultsRegionInput != 'All') {
      data <- data[data$continent == input$resultsRegionInput, ]
    }
    # sort: gold, silver, bronze
    data <- data[with(data, order(-gold, -silver, -bronze)), ]
    data
  })
  
  bubbles.data <- reactive({
    data <- results.data()
    if (nrow(data) > 0) {
      data$label <- paste0(data$country, ' has won ', data$total, ' medals')
    }
    data[data$total > 0, ] # only show those with at least one medal
  })
  
  bubbles.colors <- reactive({
    getRegionColor(bubbles.data())
  })
  
  output$resultBubblesTitle <- renderUI({
    if (input$resultsYearInput == 'All') {
      bubbleChartTitle <- "Total Medals won by country (All years)"
      bubbleChartSubtitle <- ""
    } else {
      hostData <- growth.df[growth.df$year==input$resultsYearInput, ]
      bubbleChartTitle <- paste0("Total Medals won by country in ", input$resultsYearInput) 
      bubbleChartSubtitle <- paste0("Games in ", hostData[1, "city"], ", ", hostData[1, "country"])
    }
    
    div(
      h1(id="bubble-chart-title", bubbleChartTitle),
      h3(id="bubble-chart-subtitle", bubbleChartSubtitle)
    )
  })
  
  output$resultsBubbles <- renderBubbles({
    data <- bubbles.data()
    row.count <- nrow(data)
    if (row.count > 0) {
      bubbles(bubbles.data()$total, label=bubbles.data()$country.code, 
              tooltip=bubbles.data()$label, 
              color=bubbles.colors())
    } else {
      bubbles(0, label="") # empty
    }
  })
  
  output$resultBubblesLegend <- renderUI({
    div(id="bubble-chart-legend",
      br(), br(), br(), br(),
      h3("Legend"),
      div(
        p("Asia", style=paste("color:", region.colors[1], "; font-weight: bold")),
        p("Africa", style=paste("color:", region.colors[2], "; font-weight: bold")),
        p("Americas", style=paste("color:", region.colors[3], "; font-weight: bold")),
        p("Caribbean", style=paste("color:", region.colors[4], "; font-weight: bold")),
        p("Europe", style=paste("color:", region.colors[5], "; font-weight: bold")),
        p("Oceania", style=paste("color:", region.colors[6], "; font-weight: bold"))
      )
    )
  })
  
  output$resultsTally <- renderUI({
    if (nrow(results.data()) != 0) {
      
      resultsTable <- tags$table(
        tags$tr(class="results-header",
                tags$th(class="results-rank", "Rank"), 
                tags$th(class="results-flag", "Flag"),
                tags$th(class="results-country", "Country"),
                tags$th(class="results-gold", "Gold"),
                tags$th(class="results-silver", "Silver"),
                tags$th(class="results-bronze", "Bronze"),
                tags$th(class="results-total", "Total")
        )
      )
      
      for(i in 1:nrow(results.data())) {
        row <- results.data()[i,]
        #cat(file=stderr(), paste0("row: ", row[2], ",", row[3], '\n'))
        resultsTable <- tagAppendChild(resultsTable,
                           tags$tr(class="results-row",
                                   tags$td(class="results-rank", i),
                                   tags$td(class="results-flag", 
                                           img(src=paste0(flagBaseUrl, row[2], '.png'),
                                               alt=paste0(row[3], ' flag'),
                                               width=80)),
                                   tags$td(class="results-country", row[3]),
                                   tags$td(class="results-gold", row[7]),
                                   tags$td(class="results-silver", row[8]),
                                   tags$td(class="results-bronze", row[9]),
                                   tags$td(class="results-total", row[11])
                           )
        )        
      }      
      
      if (input$resultsYearInput == 'All') {
        medalTallyTitle <- "Medal Tally (All years)"
      } else {
        medalTallyTitle <- paste0("Medal Tally in ", input$resultsYearInput) 
      }
      
      div(
        h2(id="medal-tally-title", medalTallyTitle),
        div(id="results-tally", resultsTable)
      )
    }
  })  
  
  ###################################
  # Results Map
  ###################################  
  
  # update input dropdowns
  updateSelectInput(session, "mapYearInput", choices=uniqueYears)
  updateSelectInput(session, "mapRegionInput", choices=uniqueRegions)
  
  map.data <- reactive({
    data <- results.df
    # filters
    data <- data[data$year == input$mapYearInput, ] 
    if (input$mapRegionInput != 'All') {
      data <- data[data$continent == input$mapRegionInput, ]
    }
    data
  })
  
  map.medals.data <- reactive({
    data <- map.data()
    if (nrow(data) > 0) {
      if (grepl('Gold', input$mapViewInput)) {
        data$popup <- paste0(data$gold, ' medals')
        data <- data[data$gold > 0, ]
      } else if (grepl('Silver', input$mapViewInput)) {
        data$popup <- paste0(data$silver, ' medals')
        data <- data[data$silver > 0, ]
      } else {
        data$popup <- paste0(data$bronze, ' medals')
        data <- data[data$bronze > 0, ]
      }
    }
    data
  })
  
  output$mapLeaflet <- renderLeaflet({
    leaflet.map <- leaflet(data=map.data()) %>%
      addTiles()
    
    if (nrow(map.data()) > 0) {
      
      if (input$mapViewInput=='Participant Countries') {
        customIcons <- awesomeIcons(
          icon='flag',
          iconColor='black',
          library='glyphicon'
          #markerColor=getRegionColor(map.data())
        )
        leaflet.map <- leaflet.map %>%
          addAwesomeMarkers(~long, ~lat, label=~country, icon=customIcons)
        
        leaflet.map <- leaflet.map
        
      } else {
        m.data <- map.medals.data()
        
        if (nrow(m.data) > 0) {
          leaflet.map <- leaflet(data=m.data) %>%
            addTiles()
          
          if (grepl('Gold', input$mapViewInput)) {
            icon.url <- 'gold-medal-64.png'
          } else if (grepl('Silver', input$mapViewInput)) {
            icon.url <- 'silver-medal-64.png'
          } else {
            icon.url <- 'bronze-medal-64.png'
          }
          
          # set icon size based on value
          getIconSize <- function(m.data) {
            sapply(m.data$total, function(total) {
              if(total <= 10) {
                16
              } else if(total <= 50) {
                24
              } else if (total <= 100) {
                32
              } else {
                48
              }
            })
          }        
          
          medalIcons <- icons(iconUrl=icon.url, iconWidth=getIconSize(m.data), iconHeight=getIconSize(m.data))
          
          leaflet.map <- leaflet.map %>%
            addMarkers(~long, ~lat, label=~country, 
                       icon=medalIcons, 
                       popup=~popup)
        }
      }
      
    }
    
    leaflet.map
  })
  
  output$mapViewTitle <- renderUI({
    if (input$mapYearInput == 'All') {
      leafletSubtitle <- "(All years)"
    } else {
      hostData <- growth.df[growth.df$year==input$mapYearInput, ]
      leafletSubtitle <- paste0(input$mapYearInput, " Games in ", hostData[1, "city"], ", ", hostData[1, "country"])
    }
    
    div(
      h1(id="leaflet-map-title", input$mapViewInput),
      h2(id="leaflet-map-subtitle", leafletSubtitle)
    )
  })  
  
  output$mapViewCaption <- renderUI({
    if (nrow(map.data()) == 0) {
        p("No data available for the selected filters")
    } else {
      if (grepl('Medals', input$mapViewInput)) {
        p(class="plot-note", "Click on a medal to find out how many were won")
      }
    }
  })
  

  ###################################
  # Country Details data
  ###################################  
  
  updateSelectInput(session, "countryRegionInput", choices=uniqueRegions)
  
  region.countries <- reactive({
    if (input$countryRegionInput == 'All') {
      as.list(sort(unique(results.df$country)))
    } else {
      as.list(sort(unique(results.df[results.df$continent==input$countryRegionInput, ]$country)))
      #cat(file=stderr(), paste0(input$countryRegionInput, ' countries: ', nrow(results.df[results.df$continent==input$countryRegionInput]), '\n'))
    }
  })
  
  observe({
    defaultCountry = NULL
    if (input$countryRegionInput == 'All') {
      defaultCountry='Australia' # default
    }
    updateSelectInput(session, "countryNameInput", choices=region.countries(), selected=defaultCountry)
  })
  
  country.data <- reactive({
    results.df[results.df$country==input$countryNameInput, ]
  })
  
  country.misc.data <- reactive({
    country.df <- country.data()
    country.code <- country.df[1, ]$country.code
    continent <- country.df[1, ]$continent
    years.joined <- nrow(country.df) - 1 # deduct 'All' entry
    first.join.year <- min(country.df$year)
    total.gold <- country.df[country.df$year=='All', ]$gold
    total.silver <- country.df[country.df$year=='All', ]$silver
    total.bronze <- country.df[country.df$year=='All', ]$bronze
    total.medals <- country.df[country.df$year=='All', ]$total
      
    list(country.code=country.code,
         continent=continent,
         years.joined=years.joined,
         first.join.year=first.join.year,
         total.gold=total.gold,
         total.silver=total.silver,
         total.bronze=total.bronze,
         total.medals=total.medals)
  })
  
  output$countryProfile <- renderUI({
    div(id="country-profile-details",
      img(id="country-profile-flag", 
                 src=paste0(flagBaseUrl, country.misc.data()[['country.code']], '.png'),
                 alt=paste0(input$countryNameInput, ' flag'),
                 width=120),
      h1(id="country-profile-name", input$countryNameInput),
      tags$table(id="country-profile-table", border="1",
        tags$tr(tags$td(class="country-profile-label", "Region"), tags$td(class="country-profile-value", country.misc.data()[['continent']])),
        tags$tr(tags$td(class="country-profile-label", "# Years joined"), tags$td(class="country-profile-value", country.misc.data()[['years.joined']])),
        tags$tr(tags$td(class="country-profile-label", "Year first joined"), tags$td(class="country-profile-value", country.misc.data()[['first.join.year']])),
        tags$tr(tags$td(class="country-profile-label", "Gold medals"), tags$td(class="country-profile-value", country.misc.data()[['total.gold']])),
        tags$tr(tags$td(class="country-profile-label", "Silver medals"), tags$td(class="country-profile-value", country.misc.data()[['total.silver']])),        
        tags$tr(tags$td(class="country-profile-label", "Bronze medals"), tags$td(class="country-profile-value", country.misc.data()[['total.bronze']])),        
        tags$tr(tags$td(class="country-profile-label", "Total medals"), tags$td(class="country-profile-value", country.misc.data()[['total.medals']]))        
      )  
    )
  })
  
  output$countryYearResults <- renderPlot({
    year.df <- country.data()
    year.df <- year.df[year.df$year!='All', ]
    year.df <- year.df[, c(1, 7, 8, 9)]
    #cat(file=stderr(), str(year.df))
    year.df <- melt(year.df, id='year')
    names(year.df) <- c('year', 'medal.class', 'medal.count')
    #cat(file=stderr(), str(year.df))
    
    ggplot(year.df, aes(x=year, y=medal.count, fill=medal.class)) + 
      geom_bar(stat='identity', color='black') +
      labs(x='Year', y='Medals won',
           title=paste0('Medals won by ', input$countryNameInput, ' by Year')) +
      theme(plot.title=element_text(size=22, hjust=0.5, face="bold")) +
      scale_fill_manual("legend", values=c('gold'='#C98910', 'silver'='#A8A8A8', 'bronze'='#965A38')) +
      scale_y_continuous(labels=function(x) { floor(x) })
  })
  
  country.sports.data <- reactive({
    data <- sports.df[sports.df$country==input$countryNameInput, ]
    # get total
    data$total <- data$gold + data$silver + data$bronze
    # select only needed columns
    data <- data[, c('sport', 'country', 'total')]
    # aggregate: sum
    data.agg <- aggregate(data[3], data[-3], sum)
    if (nrow(data.agg) > 0) {
      # sort descending
      data.agg <- data.agg[with(data.agg, order(-total)), ]
    }
    # return aggregate
    data.agg
  })
  
  output$countrySportsResultsTitle <- renderUI({
    row.count <- nrow(country.sports.data())
    if (row.count > 0) {
      h3(id="country-sports-bubbles-title", paste0("Medals won by ", input$countryNameInput, " by Sport"))
    }
  })
  
  output$countrySportsResults <- renderBubbles({
    data <- country.sports.data()
    row.count <- nrow(data)
    if (row.count > 0) {
      colors <- heat.colors(row.count, alpha=NULL)
      bubbles(data$total, label=data$sport, 
              tooltip=paste0(data$total, ' medals'), 
              color=colors)
    } else {
      bubbles(0, label="") # empty
    }
  })
  
  ###################################
  # Sports and Events
  ###################################    
  
  updateSelectInput(session, "sportInput", choices=uniqueSports)
  
  observe({
    if (input$sportInput != '') {
      events <- sports.df[sports.df$sport==input$sportInput, c('event')]
      updateSelectInput(session, "eventInput", choices=events)
    }
  })
  
  sport.data <- reactive({
    data <- sports.df[sports.df$sport == input$sportInput, c('country', 'total')]
    if (nrow(data) > 0) {
      data.agg <- aggregate(data[2], data[-2], sum)  
    } else {
      data.agg <- NULL 
    }
    data.agg
  })
  
  event.data <- reactive({
    data <- sports.df[(sports.df$sport == input$sportInput & sports.df$event == input$eventInput), c('country', 'total')]
    if (nrow(data) > 0) {
      data.agg <- aggregate(data[2], data[-2], sum)
    } else {
      data.agg <- NULL
    }
    data.agg
  })
  
  output$SportsPlot <- renderPlot({
    data <- sport.data()
    if (!is.null(data)) {
      ggplot(data, aes(x=reorder(country, total), y=total)) + 
        geom_bar(stat='identity', color='white', fill='dodgerblue4') +
        labs(x='Country', y='Medals won', title=paste0('Medals won in ', input$sportInput)) +
        theme(plot.title=element_text(size=20, hjust=0.5, face="bold")) +      
        scale_y_continuous(labels=function(x) { floor(x) }) +
        coord_flip()
    }
  })
  
  output$EventsPlot <- renderPlot({
    data <- event.data()
    if (!is.null(data)) {
      ggplot(data, aes(x=reorder(country, total), y=total)) + 
        geom_bar(stat='identity', color='white', fill='deepskyblue3') +
        labs(x='Country', y='Medals won', title=paste0('Medals won in ', input$sportInput, ': ', input$eventInput)) +
        theme(plot.title=element_text(size=20, hjust=0.5, face="bold")) +      
        scale_y_continuous(labels=function(x) { floor(x) }) +
        coord_flip()  
    }
  })
  
}) 