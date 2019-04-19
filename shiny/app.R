# Shiny app for visualizing Story County liquor sales

# Load libraries
library(plotly)
library(maps)
library(shiny)
library(lubridate) # for working with dates
library(zoo)
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(grid)   # for arrangeing plots
library(dplyr)  # for subsetting by season
library(tidyverse)

# Read in necessary data
story_spatial_data <- readRDS("../data/story_spatial_data.rds")
story_temporal_data <- readRDS("../data/story_temporal_data.rds")
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
                                "Total Number of Sales")
                    ),
        # Input: Simple integer interval ----
        sliderInput("year", "Year:",
                    min = min(unique(story_temporal_data$yr)), 
                    max = max(unique(story_temporal_data$yr)),
                    value = 2015),
        
        selectInput("category", 
                     label = "Liquor Category", 
                     choices = levels(story_spatial_data$catsimp)
                    )
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Input Value", tableOutput("values")),
                    tabPanel("Temporal",plotlyOutput("yearplot")),
                    tabPanel("Spatial", plotlyOutput("storymap"))
                    )
        )
      )
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # TEMPORAL VISUALIZATIONS ------------------------------------------------------------
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Year", "Category"),
      Value = as.character(c(input$year, input$category)),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  # Create temporal plot
  output$yearplot <- renderPlotly({
    
    # set the response variable
    if (input$response == "Total Number of Sales") {
      stop(safeError('This response variable is not available for temporal plot. 
                     Please switch to another response variable.'))
    } else {
      v = "dollar_per_liter"
    }
    
    # filter the temp data with the selected category and selected year
    sy_data <- story_temporal_data %>% 
      filter(yr==as.numeric(input$year))
    
    if(input$category %in% levels(sy_data$catsimp)){
      sy_factor_data <- sy_data %>% filter(catsimp == input$category)
      sy_factor_dateData <- lapply(unique(sy_factor_data$date), 
                                function(x){sy_factor_data[sy_factor_data$date==x, ]})
      
      sy_factorData <- lapply(sy_factor_dateData, function(x){
        meanDat <- as.data.frame(t(tapply(t(x[,v]),x$date, function(z) mean(z, na.rm = T))))
        names(meanDat) <- paste0(v, "_mean")
        #combine data into a single data object
        means <- cbind(unique(x$date), input$category, meanDat)
        names(means)[1] <- "date"
        return(means)
      })
      
      sy_factorData <- Reduce(x=sy_factorData, f=rbind)
      
      #add year,month to factorData
      sy_factorData$year <- year(sy_factorData$date)
      sy_factorData$month <- month(sy_factorData$date)
      #add season to factorData
      sy_factorData <- sy_factorData %>% 
        mutate(season = ifelse(month %in% c(12, 1, 2), "Winter",
                        ifelse(month %in% c(3, 4, 5), "Spring",
                        ifelse(month %in% c(6, 7, 8), "Summer",
                        ifelse(month %in% c(9, 10, 11), "Fall", "Error")))))
      
      dplmeanDaily <- ggplot(sy_factorData, aes(date, dollar_per_liter_mean)) +
        geom_line(na.rm=TRUE) +
        ggtitle(paste0("Daily Dollar per Liter Mean", " in Year ",input$year, " of Category: ", input$category)) +
        xlab("Date") + ylab("Dollar per Liter Mean ($/L)") +
        (scale_x_date(breaks=date_breaks("2 months"),
                      labels=date_format("%b")))+
        theme(plot.title = element_text(lineheight=.8, face="bold",
                                        size = 20)) +
        theme(text = element_text(size=18))
      
      # Make plot interactive
      ggplotly(dplmeanDaily)
      
      
      
      
    }else{
      stop(safeError('This category does not show up in the selected year. Please try with another category.'))
    }
    
  })
  
  
  
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

