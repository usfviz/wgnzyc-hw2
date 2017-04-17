library(shiny)
library(ggvis)
library(reshape2)

setwd('~/workspace/MSAN_viz/hw2/data')
tfrt_api <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", skip = 4, header = TRUE)
tfrt_abbr <- read.csv("Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv")
le_api <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", skip = 4, header = TRUE)
le_abbr <- read.csv("Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv")
population <- read.csv("population.csv")

melt_func <- function(file){
  new_file <- melt(file,id.vars = c("Country.Name","Country.Code","Indicator.Name","Indicator.Code"))
  return (new_file)
}

tfrt_api_long <- melt_func(tfrt_api)
le_api_long <- melt_func(le_api)

tfrt_api_long$variable <- substr(tfrt_api_long$variable, 2, 5)
le_api_long$variable <- substr(le_api_long$variable, 2, 5)

colnames(tfrt_api_long) <- c("country","Country.Code","indecator_le","indicator_code_le","year","life_expectancy")
colnames(le_api_long) <- c("country","Country.Code","indecator_fe","indicator_code_fe","year","fertility")

merge_two <- merge(tfrt_api_long,le_api_long,by = intersect(names(tfrt_api_long),names(le_api_long)))
merge_abbr <- merge(merge_two,tfrt_abbr,by = intersect(names(merge_two),names(tfrt_abbr)))


melt_popu <- melt_func(population)
melt_popu$variable <- substr(melt_popu$variable, 2, 5)
colnames(melt_popu) <- c("country","Country.Code","indecator_p","indicator_code_p","year","population")

merge_pop <- merge(merge_abbr,melt_popu,by = intersect(names(merge_abbr),names(melt_popu)))
merge_pop <- merge_pop[merge_pop$Region!="",]
merge_pop$population <- as.numeric(merge_pop$population)
regions <- levels(merge_pop$Region)
regions <- regions[regions!=""]


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

server <- function(input, output) {
  sdt <- reactive({tmp = merge_pop[merge_pop$year==input$year,]
                    tmp = tmp[tmp$Region != "",]
                   tmp})
  point_size <- reactive(input$size)
  cont_selected <- reactive(input$continent)
  continent <- reactive({cnt = merge_pop[merge_pop$Region == input$continent,]
                          cnt = cnt[cnt$year == input$year,]
                          cnt})
  not_cnt <- reactive({ct = merge_pop[merge_pop$Region != input$continent,]
              ct = ct[ct$year == input$year,]
              ct})
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- sdt()[sdt()$fertility == x$fertility,]
    row$country
  }
  sdt %>% 
    ggvis(~fertility, ~life_expectancy) %>%
    add_tooltip(all_values, "hover")%>%
    layer_points(fill = ~Region,size := ~population/100000000*point_size() + 50, stroke := "black",opacity := 0.5) %>%
    layer_points(data = continent,fill = cont_selected,size := ~population/100000000*point_size() + 50, stroke := "black")%>%
    add_legend("fill", title = "Region")%>%
    bind_shiny("ggvis", "ggvis_ui")
    
}


shinyApp(ui = ui, server = server)
