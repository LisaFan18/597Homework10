#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(tidycensus)
library(tidyr)
library(shiny)
library(choroplethr)
source("../api-keys.R")
census_api_key(api.key.census)


# load and clean data
mdincome.df <- 
  get_acs(geography = "county",variables = c(medincome = "B19013_001", grossrental = "B25064_001")) %>%   mutate(region = as.integer(GEOID)) %>% 
  mutate(region = as.integer(GEOID)) %>% 
  select(-moe) %>% 
  spread(key = variable, value = estimate) %>%
  mutate(ratio = grossrental/medincome) %>% 
  separate(NAME, into = c("county", "state"), sep = ",") %>% 
  mutate(state = str_to_lower(str_trim(state))) %>% 
  filter(state != "puerto rico") %>% 
  select(-GEOID, -county)

# Define UI 
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # setting choices from data.frame
      htmlOutput("selectUI"),
      selectInput("varInput", "Variable:", 
                  choices=c("medincome", "grossrental", "ratio"), selected = "medincome")
    ),
    mainPanel(
      plotOutput("main_plot")
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  reduced_df <- reactive({
    filter(mdincome.df, state==input$stateInput) %>% 
    select(region, medincome) %>% 
    #select(region, input$varInput) %>% 
    #dplyr::rename(value = input$varInput)
    dplyr::rename(value = medincome) 
  })
  output$selectUI <- renderUI({ 
    selectInput("stateInput", "State:",  unique(select(mdincome.df, state)) )
  })
  output$main_plot <- renderPlot({
    choroplethr::county_choropleth(reduced_df(), title = "Median Household Income", num_colors =1 , state_zoom=input$stateInput)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

