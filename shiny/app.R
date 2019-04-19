# Shiny app for visualizing Story County liquor sales

# Load libraries
library(plotly)
library(maps)
library(shiny)
library(tidyverse)

# Read in necessary data
story_spatial_data <- readRDS("../data/story_spatial_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Liquor Sales in Story County"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("category", 
                     label = "Liquor Category", 
                     choices = levels(story_spatial_data$catsimp))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Temporal", plotOutput("distPlot")),
                    tabPanel("Spatial", plotlyOutput("storymap"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # TEMPORAL VISUALIZATIONS ------------------------------------------------------------
  
  # SPATIAL VISUALIZATIONS -------------------------------------------------------------
  
  # Obtain map data for Story County
  story_county <- map_data("county") %>% filter(region == "iowa", subregion == "story")
  
  # Create spatial plot
  output$storymap <- renderPlotly({
    
    # Create plot
    spatial_plot <- ggplot() + 
      geom_polygon(data = story_county, 
                    aes(x = long, y = lat, group = group), 
                    alpha = 0.1, 
                    color = "black") + 
       geom_jitter(data = story_spatial_data %>% filter(catsimp == input$category), 
                  aes(x = longitude, y = latitude, color = n, size = n)) + 
       labs(x = "Longitude", y = "Latitude", title = "Story County")
   
    # Make plot interactive
    ggplotly(spatial_plot, width = 800, height = 600)
     
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

