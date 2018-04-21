#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(tidycensus)
library(ggplot2)
source("../api-keys.R")
census_api_key(api.key.census)


# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("varInput", "Variable:", 
                  choices=c("medincome", "grossrental", "ratio")),
      selectInput("stateInput", "State:", 
                  choices=c("AK", "AL",  "AR",  "AS",  "AZ",  "CA",  "CO",  "CT",  "DC",  "DE",  "FL",  "GA",  "GU",  "HI",  "IA",  "ID",  "IL",  "IN",  "KS",  "KY",  "LA",  "MA",  "MD",  "ME",  "MI",  "MN",  "MO",  "MS",  "MT",  "NC",  "ND",  "NE",  "NH",  "NJ",  "NM",  "NV",  "NY",  "OH",  "OK",  "OR",  "PA",  "PR",  "RI",  "SC",  "SD",  "TN",  "TX",  "UT",  "VA",  "VI",  "VT",  "WA",  "WI",  "WV",  "WY"))
    ),
    mainPanel(
      plotOutput("main_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  reduced_df <- reactive({
    get_acs(
      geography = "tract",
      variables = c(medincome = "B19013_001", grossrental = "B25064_001"),
      state = input$stateInput,
      geometry = TRUE
    ) %>%.[, -5] %>% data.frame() %>% 
      spread(key = variable, value = estimate) %>% 
      mutate(ratio = grossrental/medincome) 
  })
  
  output$main_plot <- renderPlot({
    reduced_df() %>% 
    ggplot(aes_string(fill = input$varInput)) + 
      geom_sf() + 
      ggtitle(input$varInput)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

