# Shiny app for visualizing Story County liquor sales

library(maps)
library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Liquor Sales in Story County"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Temporal", plotOutput("distPlot")),
                    tabPanel("Spatial", plotOutput("storymap"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   # Obtain map data for Story County
   story_county <- map_data("county") %>% filter(region == "iowa", subregion == "story")
   
   output$storymap <- renderPlot({
     
     ggplot(story_county , aes(x = long, y = lat, group = group)) + 
       geom_polygon(alpha = 0.1, color = "black") + 
       labs(x = "Longitude", y = "Latitude", title = "Story County")
   
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

