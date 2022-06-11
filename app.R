library(tidyverse)
library(shiny)
library(leaflet)
library(rvest)
library(ggiraph)
library(rgdal)

url <- "https://www.worldometers.info/coronavirus/country/us/"

covid <- url %>%
  read_html() %>%
  html_node(xpath = '//*[(@id = "usa_table_countries_today")]') %>%
  html_table()

covid$TotalCases <- covid$TotalCases %>% 
  str_replace_all(",", "") %>% 
  as.numeric()

covid$TotalDeaths <- covid$TotalDeaths %>% 
  str_replace_all(",", "") %>% 
  as.numeric()

covid$ActiveCases <- covid$ActiveCases %>% 
  str_replace_all(",", "") %>% 
  as.numeric()

covid$`Tot Cases/1M pop` <- covid$`Tot Cases/1M pop` %>% 
  str_replace_all(",", "") %>% 
  as.numeric()

covid$`Deaths/1M pop` <- covid$`Deaths/1M pop` %>% 
  str_replace_all(",", "") %>% 
  as.numeric()

covid <- covid %>% 
  filter(`#` < 52) %>% 
  select(USAState, TotalCases, TotalDeaths, ActiveCases, `Tot Cases/1M pop`, `Deaths/1M pop`) %>% 
  rename(name = USAState) %>% 
  rename(TotalCasesPerMillion = `Tot Cases/1M pop`) %>% 
  rename(DeathsPerMillion = `Deaths/1M pop`)

vac <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv") %>% 
  filter(date == Sys.Date() - 1) %>% 
  select(location, people_fully_vaccinated) %>% 
  rename(name = location) %>% 
  mutate(name = case_when(name == "New York State" ~ "New York",
                          TRUE ~ name))

covid_vac <- left_join(covid, vac, by = "name")

regions <- read_csv(here::here("states.csv")) %>% 
  select(State, Region) %>% 
  rename(name = State)

covid_vac_regions <- left_join(covid_vac, regions, by = "name")

covid_vac_regions <- covid_vac_regions[-50,]

states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

test <- sp::merge(states, covid)

url2 <- "https://www.worldometers.info/coronavirus/#countries"

covid_world <- url2 %>%
  read_html() %>%
  html_node(xpath = '//*[(@id = "main_table_countries_today")]') %>%
  html_table()

covid_world <- covid_world[-(1:8),]
covid_world <- covid_world[-(231:238),]

covid_world$TotalCases <- covid_world$TotalCases %>% 
  str_replace_all(",", "") %>% 
  as.numeric()

covid_world$TotalDeaths <- covid_world$TotalDeaths %>% 
  str_replace_all(",", "") %>% 
  as.numeric()

covid_world$ActiveCases <- covid_world$ActiveCases %>% 
  str_replace_all(",", "") %>% 
  as.numeric()

covid_world <- covid_world %>% 
  select(`Country,Other`, TotalCases, TotalDeaths, ActiveCases) %>% 
  rename(NAME = `Country,Other`) %>% 
  mutate(NAME = case_when(NAME == "USA" ~ "United States",
                          NAME == "UK" ~ "United Kingdom",
                          NAME == "S. Korea" ~ "Korea, Republic of",
                          NAME == "Vietnam" ~ "Viet Nam",
                          TRUE ~ NAME))

world <- readOGR(dsn = here::here("TM_WORLD_BORDERS_SIMPL-0.3.shp"))

worldmap <- merge(world, covid_world)

theme_covid <- function(text_color = "#A67070",
                        title_color = "#E56020",
                        panel_color = "#F7F7F7",
                        background_color = "#D6EFF6",
                        gridline_color = "#D6EFF6") {
  
  theme(panel.background = element_rect(fill = panel_color),
        plot.background = element_rect(fill = background_color),
        axis.title = element_text(color = text_color,
                                  face = "bold"),
        axis.text = element_text(color = text_color,
                                 face = "bold"),
        plot.title = element_text(color = title_color,
                                  face = "bold",
                                  size = 15),
        legend.background = element_rect(fill = background_color),
        legend.key = element_rect(fill = background_color),
        legend.title = element_text(color = text_color,
                                    face = "bold"),
        legend.text = element_text(color = text_color,
                                   face = "bold"),
        panel.grid.major = element_line(color = gridline_color,
                                        size = .7),
        panel.grid.minor = element_line(color = gridline_color,
                                        size = .7))
}

ui <- navbarPage(theme = shinythemes::shinytheme("cosmo"),
                 
                 "COVID-19 Stats",
                 
                 tabPanel("COVID Numbers in the US",
                          sidebarLayout(
                            sidebarPanel = sidebarPanel(h4("US COVID Choropleth Map"),
                              p("This map shows the current numbers for different COVID-19-related stats in the US. Hover over each state to view the selected stat for that state. The data come from ",
                                a(href = "https://www.worldometers.info/coronavirus/country/us/", "this website")),
                              selectInput(inputId = "map", label = "Select a Stat", choices = c("Total Cases", "Total Deaths", "Active Cases"))),
                            mainPanel = mainPanel(leafletOutput(outputId = "maps")
                          ))),
                 
                 tabPanel("COVID Numbers in the World",
                          sidebarLayout(
                            sidebarPanel = sidebarPanel(h4("World COVID Choropleth Map"),
                                                        p("This map shows the current numbers for different COVID-19-realted stas in the world. Hover over each country to view the selected stat for that country. The data come from ",
                                                          a(href = "https://www.worldometers.info/coronavirus/#countries", "this website")),
                                                        selectInput(inputId = "worldmap", label = "Select a Stat", choices = c("Total Cases", "Total Deaths", "Active Cases"))),
                            mainPanel = mainPanel(leafletOutput(outputId = "worldmaps"))
                          )),
                 
                 tabPanel("Vaccination Rates in the US",
                          sidebarLayout(
                            sidebarPanel = sidebarPanel(h4("Correlating Vaccination Rates"),
                                                        p("This graph plots the selected variable against vaccination rates. Hover over each point to see which state it represents. A table of statistics computed through simple linear regresssion is listed below the graph. Data for the vaccination rates came from ",
                                                          a(href = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv", "here"), ", and the data for the other COVID variables came from",
                                                          a(href = "https://www.worldometers.info/coronavirus/country/us/", "this website")),
                              selectInput(inputId = "stat", label = "Choose a Variable", choices = c("TotalCasesPerMillion", "DeathsPerMillion"))),
                            mainPanel = mainPanel(ggiraphOutput("plot"),
                                                  tableOutput("mlr"))
                          )))

server <- function(input, output) {
  
  output$maps <- renderLeaflet({
    
    if(input$map == "Total Cases") {
      bins = c(100000, 300000, 500000, 750000, 1250000, 2500000, 5000000, 7500000, 10000000)
      lab = test$TotalCases
      
      labels <- paste("<p>", test$name, "<p>",
                      "<p>", lab, " Cases", "<p>",
                      sep = "")
    } else if(input$map == "Total Deaths"){
      bins = c(600, 1000, 5000, 7500, 10000, 30000, 50000, 70000, 90000, 125000)
      lab = test$TotalDeaths
      
      labels <- paste("<p>", test$name, "<p>",
                      "<p>", lab, " Deaths", "<p>",
                      sep = "")
    } else if(input$map == "Active Cases"){
      
      bins = c(1000, 5000, 10000, 50000, 70000, 90000, 125000, 225000, 500000, Inf)
      lab = test$ActiveCases
      
      labels <- paste("<p>", test$name, "<p>",
                      "<p>", lab, " Active Cases", "<p>",
                      sep = "")
    }
    
    pal <- colorBin("YlOrRd", domain = lab, bins = bins)
    
    leaflet(test) %>% 
      setView(-96, 37.8, 4) %>%
      addTiles() %>% 
      addPolygons(label = lapply(labels, htmltools::HTML), 
                  labelOptions= labelOptions(direction = 'auto'), 
                  weight=1, 
                  color='white', 
                  opacity=1,
                  fillColor = ~pal(lab), 
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(
                    color='#000000', 
                    weight = 2,
                    bringToFront = TRUE, 
                    sendToBack = TRUE)) %>% 
      addLegend("topright", 
                pal = pal, 
                values = ~lab, 
                title = htmltools::HTML(input$map), 
                opacity = .8 )
  })
  
  output$worldmaps <- renderLeaflet({
    
    if(input$worldmap == "Total Cases") {
      bins = c(0, 75000, 250000, 500000, 1000000, 5000000, 10000000, 15000000, 30000000, Inf)
      lab = worldmap$TotalCases
      
      labels <- paste("<p>", worldmap$NAME, "<p>",
                      "<p>", lab, " Cases", "<p>",
                      sep = "")
    } else if(input$worldmap == "Total Deaths"){
      bins = c(0, 5000, 10000, 25000, 50000, 100000, 300000, 500000, Inf)
      lab = worldmap$TotalDeaths
      
      labels <- paste("<p>", worldmap$NAME, "<p>",
                      "<p>", lab, " Deaths", "<p>",
                      sep = "")
    } else if(input$worldmap == "Active Cases"){
      
      bins = c(0, 5000, 15000, 35000, 100000, 250000, 500000, 1000000, Inf)
      lab = worldmap$ActiveCases
      
      labels <- paste("<p>", worldmap$NAME, "<p>",
                      "<p>", lab, " Active Cases", "<p>",
                      sep = "")
    }
    
    pal <- colorBin("YlOrRd", domain = lab, bins = bins)
    
    leaflet(worldmap) %>% 
      setView(lat = 20, lng = 0, zoom = 1) %>%
      addTiles() %>% 
      addPolygons(label = lapply(labels, htmltools::HTML), 
                  labelOptions= labelOptions(direction = 'auto'), 
                  weight=1, 
                  color='white', 
                  opacity=1,
                  fillColor = ~pal(lab), 
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(
                    color='#000000', 
                    weight = 2,
                    bringToFront = TRUE, 
                    sendToBack = TRUE)) %>% 
      addLegend("topright", 
                pal = pal, 
                values = ~lab, 
                title = htmltools::HTML(input$worldmap), 
                opacity = .8 )
  })
  
  selected <- reactive({
    select(covid_vac_regions, people_fully_vaccinated, input$stat, name, Region)
  })
  
  output$plot <- renderggiraph({
    
    g <- selected() %>% 
      ggplot() +
      geom_point_interactive(aes_string(x = "people_fully_vaccinated", y = input$stat, color = "Region", tooltip = "name", data_id = "name")) +
      labs(x = "Number of People Fully Vaccinated") +
      theme_covid()
    
    girafe(code = print(g))
  })
  
  output$mlr <- renderTable({
    
    my_formula <- paste(input$stat, "~ people_fully_vaccinated")
    
    lm_dt <- lm(my_formula, data = selected())
    
    broom::glance(lm_dt)
    
  })
}

shinyApp(ui = ui, server = server)

