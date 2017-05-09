# ui.R

library(shiny)
library(leaflet)
library(bubbles)


shinyUI(
  navbarPage("The Commonwealth Games",
             
    tabPanel("Introduction",
      fluidPage(
        column(3),
        column(6,
            div(id="intro-head",
               h1("The Commonwealth Games"),
               h2("Data Exploration and Visualisation Project"),
               hr(),
               img(id="games-logo", src="cgf-logo.gif")
            ),
            div(id="intro-text",
              br(),
              h3("Overview"),
              p("This application provides visualisations on data on the Commonwealth Games, ",
                "the international sporting event held every four years between members of the ", 
                a("Commonwealth of Nations", href="https://en.wikipedia.org/wiki/Commonwealth_of_Nations"), "."),
              p("The project has the following panels:"),
              tags$ul(
                tags$li(strong("Growth of the Games"), "- shows how the games has grown in numbers since its inception"),
                tags$li(strong("Results: Medal Tally"), "- shows the medal results for a selected year or for all years"),
                tags$li(strong("Results: Map View"), "- shows the participant countries and medal winnings on a map"),
                tags$li(strong("Country Profiles"), "- shows details about a participating country, including medals won and in which sports"),
                tags$li(strong("Sports and Events"), "- shows the countries that have won medals in order for any selected sport and event")
              ),
              br(),
              h3("Credits"),
              p("Data and images used in this project were sourced from the ", a("official website", href="http://www.thecgf.com"),
                "of the Commonwealth Games.")
            )
        ),
        column(3)
      )
    ),
             
    tabPanel("Growth of the Games", 
      fluidPage(
        column(3, 
          wellPanel(
            radioButtons("growthProp", label="Select number to view",
                         choices=c('Countries', 'Sports', 'Events', 'Athletes'), 
                         selected='Countries'))
        ),

        column(9,
          plotOutput("growthPlot", hover=hoverOpts(id="growth.hover")),
          hr(),
          htmlOutput("yearInfo")
        )
      )
    ),
    
    tabPanel("Results: Medal Tally",
      fluidPage(
        fluidRow(
          column(8, 
            fluidRow(
              column(2),
              column(4,
                wellPanel(
                  selectInput("resultsYearInput", "Select Year", "All")
                )
              ),
              column(4,
                wellPanel(
                  selectInput("resultsRegionInput", "Select Region", "All")
                )
              ),
              column(2)
            ),
            
            hr(),
            
            fluidRow(
              column(10,
                htmlOutput("resultBubblesTitle"),
                bubblesOutput("resultsBubbles", width="1000px", height="800px")
              ),
              column(2,
                htmlOutput("resultBubblesLegend")
              )
            )
          ),
          
          column(4, 
            htmlOutput("resultsTally")
          )
        )
      )
    ),
    
    tabPanel("Results: Map View",
      fluidPage(
       fluidRow(
         column(3, 
            wellPanel(
              selectInput("mapYearInput", "Select Year", "All"),
              selectInput("mapRegionInput", "Select Region", "All")
            ),
            wellPanel(
              radioButtons("mapViewInput", "Select Map View",
                           choices=c('Participant Countries', 
                                     'Countries that have won Gold Medals', 
                                     'Countries that have won Silver Medals', 
                                     'Countries that have won Bronze Medals'),
                           selected='Participant Countries')
            )
         ),
         
         column(9,
                htmlOutput("mapViewTitle"),
                div(id="leaflet-map-container",
                    tags$img(src="spinner.gif", class="loading-spinner"),
                    leafletOutput("mapLeaflet", height=700)
                ),
                htmlOutput("mapViewCaption")
         )
       )
      )
    ),    
    
    tabPanel("Country Profiles",
      fluidPage(
        fluidRow(
          column(3,
            wellPanel(
              selectInput("countryRegionInput", "Select Region", "All"),
              selectInput("countryNameInput", "Select Country", "All")
            ),
            hr(),
            htmlOutput("countryProfile")
          ),
          column(9,
            plotOutput("countryYearResults"),
            hr(),
            htmlOutput("countrySportsResultsTitle"),
            bubblesOutput("countrySportsResults", width="1000px", height="800px")
          )
        )
      )
    ),
    
    tabPanel("Sports and Events",
      fluidPage(
        fluidRow(
          column(3,
             wellPanel(
               selectInput("sportInput", "Select Sport", "Swimming")
             )
          ),
          column(9,
             plotOutput("SportsPlot")
          )
        ),
        
        hr(),
        
        fluidRow(
          column(3,
             wellPanel(
                selectInput("eventInput", "Select Event", "All")
             )
          ),
          column(9,
             plotOutput("EventsPlot")
          )          
        )
      )             
    ),
    
    # CSS styles
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="style.css")
    )
    
))