library(shiny)
library(ggvis)
library(reshape2)

ui <- fluidPage(
  headerPanel('Life Expectancy and Fertility Rate'),
  sidebarPanel(
    sliderInput("year", "Year", 1960,2014,1,animate = animationOptions(interval = 100,
                                                                       playButton = icon('play', "fa-3x"),
                                                                       pauseButton = icon('pause', "fa-3x"))),
    sliderInput("size","population",0,1000,50),
    selectInput("continent","Regions",choices = regions)
    
  ),
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis")
  )
)
