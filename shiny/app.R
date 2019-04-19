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
        
        selectInput("response", 
                    label = "Response Variable", 
                    choices = c("Average Cost per Liter (Dollars)",
                                "Total Number of Sales")),
        
        selectInput("category", 
                     label = "Liquor Category", 
                     choices = levels(story_spatial_data$catsimp))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Temporal"),
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
    
    if (input$response == "Total Number of Sales") {
      color = "count"
      size = "count"
    } else {
      color = "ave_dollar_per_liter"
      size = "ave_dollar_per_liter"
    }
    
    # Create plot
    spatial_plot <- ggplot() + 
      geom_polygon(data = story_county, 
                    aes(x = long, y = lat, group = group), 
                    alpha = 0.2, 
                    color = "black") + 
      geom_jitter(data = story_spatial_data %>% filter(catsimp == input$category), 
                   aes_string(text = 'storename',
                              x = 'longitude', 
                              y = 'latitude', 
                              color = color, 
                              size = size),
                  alpha = 0.8) + 
      labs(x = "Longitude", y = "Latitude", title = "Story County", color = "Response") + 
      theme_bw()
   
    # Make plot interactive
    ggplotly(spatial_plot, width = 800, height = 600, tooltip = c("text", "x", "y", "color")) %>%
      layout(legend = list(orientation = "h"))
     
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

