library(shiny)
#load the data
cleaned_data <- as.data.frame(story_liquor_cleaned)
head(cleaned_data)
#make up a new response variable: saledollars/volumesoldliters
shiny_raw_data <- cleaned_data %>%
  mutate(dollar_per_liter =saledollars/volumesoldliters)%>%
  select(date, storename, latitude, longitude, catsimp, vendorname,dollar_per_liter)

#time series plot
#overall dollar_per_liter means on each date
dollar_per_liter_means <- aggregate(shiny_raw_data$dollar_per_liter, by=list(shiny_raw_data$date),
                                    FUN=mean, na.rm=T)
colnames(dollar_per_liter_means) = c("date", "dollar_per_liter_mean")

#aggregate dollar_per_liter by catsimp on each date

#generate a list of length 1272
#each list element represent one day
dateData <- lapply(unique(shiny_raw_data$date), 
                   function(x){shiny_raw_data[shiny_raw_data$date==x, ]})

#time series plot using ggplot2
library(ggplot2)
theme_set(theme_minimal())
ggplot(data = liquor_data, aes(x = date, y = sale_dollars))+geom_point()
library(dplyr)
test_large_data <- as.data.frame(Iowa_Liquor_Sales_Story)

test_single_date_data <- test_large_data %>% 
  filter(Date=="03/29/2019") %>%
  mutate(var1 = `Sale (Dollars)`/`Volume Sold (Liters)`, var2 = `State Bottle Retail`/`Bottle Volume (ml)`*1000)

levels(as.factor(test_single_date_data$City)) #2 cities
levels(as.factor(test_single_date_data$`Store Name`)) #9 stores
levels(as.factor(test_single_date_data$`Category Name`)) #28 categories
levels(as.factor(test_single_date_data$`Vendor Name`)) #24 vendors



shinyServer(function(input, output) {
  # MA-plot
  # output$plotma <- renderPlot({
  #   par( mar=c(5,5,3,2), cex.main=1.5, cex.lab=1.35 )
  #   plotMA( res, ylim=c(-ymax, ymax) )
  # })
})