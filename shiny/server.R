library(shiny)
#load the data
cleaned_data <- as.data.frame(story_liquor_cleaned)

#make up a new response variable: saledollars/volumesoldliters
shiny_raw_data <- cleaned_data %>%
  mutate(dollar_per_liter =saledollars/volumesoldliters)%>%
  select(date, storename, latitude, longitude, catsimp, vendorname,dollar_per_liter)
shiny_raw_data$month_yr <- format(as.Date(shiny_raw_data$date), "%Y-%m")
#time series plot
#overall dollar_per_liter means on each date
# dollar_per_liter_means <- aggregate(shiny_raw_data$dollar_per_liter, by=list(shiny_raw_data$date),
#                                     FUN=mean, na.rm=T)
# colnames(dollar_per_liter_means) = c("date", "dollar_per_liter_mean")

#aggregate dollar_per_liter by catsimp on each date
# factor could be any level of catsimp (13 levels in total)
levels(as.factor(shiny_raw_data$catsimp))
#generate a list of length 1272
#each list element represent one day


#test the factorData function
f="amaretto"
v<- "dollar_per_liter"

shiny_factor_data <- shiny_raw_data %>% filter(catsimp==f)

factor_dateData <- lapply(unique(shiny_factor_data$date), 
                           function(x){shiny_factor_data[shiny_factor_data$date==x, ]})


factor_monthData <- lapply(unique(shiny_factor_data$month_yr), 
         function(x){shiny_factor_data[shiny_factor_data$month_yr==x, ]})
#loop for each date


# for example: f="amaretto"
factorData <- lapply(factor_monthData, function(x){
  # subset x with carsimp==f
  x <- x[which(x$catsimp==f), ]
  meanDat <- as.data.frame(t(tapply(x[,v],x$month_yr, function(z) mean(z, na.rm = T))))
  names(meanDat) <- paste0(v, "_mean")
  #combine data into a single data object
  means <- cbind(unique(x$month_yr), f, meanDat)
  names(means)[1] <- "date"
  return(means)
})

factorData <- Reduce(x=factorData, f=rbind)
require(zoo)
factorResult <- factorData[order(as.yearmon(factorData$date, "%Y-%m")), ]

factorData2 <- lapply(factor_dateData, function(x){
  # subset x with carsimp==f
  x <- x[which(x$catsimp==f), ]
  meanDat <- as.data.frame(t(tapply(x[,v],x$date, function(z) mean(z, na.rm = T))))
  names(meanDat) <- paste0(v, "_mean")
  #combine data into a single data object
  means <- cbind(unique(x$date), f, meanDat)
  names(means)[1] <- "date"
  return(means)
})

factorData2 <- Reduce(x=factorData2, f=rbind)

factorResult2 <- factorData2[order(as.Date(factorData2$date)),]


#time series plot using ggplot2
library(ggplot2)
 library(scales)
ggplot(data = factorResult2, aes(x = date, y = dollar_per_liter_mean)) + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 45))


shinyServer(function(input, output) {
  # MA-plot
  # output$plotma <- renderPlot({
  #   par( mar=c(5,5,3,2), cex.main=1.5, cex.lab=1.35 )
  #   plotMA( res, ylim=c(-ymax, ymax) )
  # })
})