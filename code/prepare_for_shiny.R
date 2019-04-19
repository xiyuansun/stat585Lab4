#prepare aggregated data for shiny app
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(grid)   # for arrangeing plots
library(dplyr)  # for subsetting by season
library(tidyverse)
#setwd("~/Desktop/stat585Lab4/code")
story_liquor_raw <- read_csv("../data/Iowa_Liquor_Sales-Story.csv", col_types = cols())
story_liquor_cleaned <- story_liquor_raw %>%
  
  # Rename cols
  rename_all(funs(
    str_replace_all(., "[\\(\\)_\\/ ]", "") %>%
      tolower())) %>%
  
  # Adjust date variable
  mutate(date = lubridate::mdy(date)) %>%
  
  # Send character variables to lower
  mutate_if(.predicate = is.character,
            .funs = tolower) %>%
  
  # Create factor variables where necessary
  mutate_at(.vars = c("storenumber", 
                      "storename", 
                      "address", 
                      "city",
                      "zipcode", 
                      "storelocation", 
                      "countynumber",
                      "county",
                      "category", 
                      "categoryname", 
                      "vendornumber", 
                      "vendorname", 
                      "itemnumber", 
                      "pack"),
            .funs = factor) %>%
  
  # Clean up factor variables
  mutate(address = str_replace(address, ",", ""),
         address = str_replace(address, "issac", "isaac"),
         address = str_replace(address, "4518 mortenson rd ste 109", "4518 mortonsen street suite #109"),
         address = str_replace(address, "lincolnway", "lincoln way"),
         storename = str_replace(storename, "a j's", "aj's"),
         storename = str_replace(storename, "casey's general store # 2560/ ames", 
                                 "casey's general store # 2560"),
         storename = str_replace(storename, "casey's general store # 2560", 
                                 "casey's general store #2560 / ames"), 
         storelocation = str_split(storelocation, "\n", simplify = TRUE)[,3],
         latitude = str_split(storelocation, ", ", simplify = TRUE)[,1] %>% 
           str_replace("\\(", "") %>%
           as.numeric(),
         longitude = str_split(storelocation, ", ", simplify = TRUE)[,2] %>%
           str_replace("[\\)]", "") %>%
           as.numeric(),
         categoryname = str_replace(categoryname, "american cordials & liqueurs", 
                                    "american cordials & liqueur"),
         categoryname = str_replace(categoryname, "american distilled spirits specialty", 
                                    "american distilled spirit specialty"),
         categoryname = str_replace(categoryname, "american vodkas", "american vodka"),
         categoryname = str_replace(categoryname, "cocktails /rtd", "cocktails / rtd"),
         categoryname = str_replace(categoryname, "flavored gins", "flavored gin"),
         categoryname = str_replace(categoryname, "imported distilled spirits specialty", 
                                    "imported distilled spirit specialty"),
         categoryname = str_replace(categoryname, "imported vodkas", "imported vodka"),
         vendorname = str_replace_all(vendorname, "[,\\.\\/-]", "")) %>%
  group_by(vendornumber) %>%
  mutate(vendorname = vendorname[1]) %>%
  ungroup() %>%
  mutate(vendorname = factor(vendorname)) %>%
  
  # Get rid of weird extremes
  filter(saledollars > 0,
         volumesoldliters > 0,
         volumesoldgallons > 0,
         volumesoldliters < 250) %>%
  
  # Assign new simple categories
  mutate(
    catsimp = factor(case_when(
      grepl("amaretto", categoryname) ~ "amaretto",
      grepl("brand", categoryname) ~ "brandy",
      grepl("creme", categoryname) ~ "creme",
      grepl("cocktail|sec", categoryname) ~ "general_mixer",
      grepl("spirit", categoryname) ~ "general_spirit",
      grepl("gin", categoryname) ~ "gin",
      grepl("liqueur", categoryname) ~ "liqueur",
      grepl("schnap", categoryname) ~ "schnapps",
      grepl("rum", categoryname) ~ "rum",
      grepl("special", categoryname) ~ "specialty",
      grepl("tequ|mez", categoryname) ~ "tequila",
      grepl("vodka", categoryname) ~ "vodka",
      grepl("whisk|bourb|scotch|rye", categoryname) ~ "whisky"
    ))
  ) %>%
  
  # Select the final order of the variables
  select(invoiceitemnumber:storelocation, 
         latitude, 
         longitude, 
         countynumber:categoryname, 
         catsimp, 
         vendornumber:volumesoldgallons)




#load the cleaned county data
cleaned_data <- as.data.frame(story_liquor_cleaned)

#make up a new response variable: saledollars/volumesoldliters
shiny_raw_data <- cleaned_data %>%
  mutate(dollar_per_liter =saledollars/volumesoldliters)%>%
  select(date, storename, latitude, longitude, catsimp, vendorname,dollar_per_liter)
shiny_raw_data$month_yr <- format(as.Date(shiny_raw_data$date), "%Y-%m")
shiny_sorted_data <- shiny_raw_data[order(as.Date(shiny_raw_data$date)),]


#time series plot
#overall dollar_per_liter means on each date
# dollar_per_liter_means <- aggregate(shiny_raw_data$dollar_per_liter, by=list(shiny_raw_data$date),
#                                     FUN=mean, na.rm=T)
# colnames(dollar_per_liter_means) = c("date", "dollar_per_liter_mean")

#aggregate dollar_per_liter by catsimp on each date
# factor could be any level of catsimp (13 levels in total)
levels(as.factor(shiny_sorted_data$catsimp))
#generate a list of length 1272
#each list element represent one day


#test the factorData function
f="amaretto"
v<- "dollar_per_liter"

shiny_factor_data <- shiny_sorted_data %>% filter(catsimp==f)

factor_dateData <- lapply(unique(shiny_factor_data$date), 
                          function(x){shiny_factor_data[shiny_factor_data$date==x, ]})


factor_monthData <- lapply(unique(shiny_factor_data$month_yr), 
                           function(x){shiny_factor_data[shiny_factor_data$month_yr==x, ]})
#loop for each date / month_yr

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

#add year,month to factorData2
factorData2$year <- year(factorData2$date)
factorData2$month <- month(factorData2$date)
#add season to factorData2
factorData2 <- factorData2 %>% 
  mutate(season = 
           ifelse(month %in% c(12, 1, 2), "Winter",
           ifelse(month %in% c(3, 4, 5), "Spring",
          ifelse(month %in% c(6, 7, 8), "Summer",
          ifelse(month %in% c(9, 10, 11), "Fall", "Error")))))

one_year_factorData2 <- factorData2 %>%
  filter(year==2014)

#time series plot using ggplot2

# ggplot(data = factorData2, aes(x = date, y = dollar_per_liter_mean)) + 
#   geom_line()+
#   theme(axis.text.x = element_text(angle = 45))

dplmeanDaily <- ggplot(one_year_factorData2, aes(date, dollar_per_liter_mean)) +
  geom_line(na.rm=TRUE) +
  ggtitle("Daily Dollar per Liter Mean") +
  xlab("Date") + ylab("Dollar per Liter Mean ($/L)") +
  (scale_x_date(breaks=date_breaks("2 months"),
                labels=date_format("%b")))+
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18))


dplmeanDaily

#we would add another factor data's time series line to the same plot later

############################################################################

#try to plot the county map

devtools::install_github("lydiaPenglish/smoothMap")
library(smoothMap)
library(ggplot2)


us_file <- "../data/gadm36_USA_shp/gadm36_USA_1.shp"
us_data <- team_4(us_file, tolerance = 0.01)
iowa_data <- us_data %>% filter(NAME_1=="Iowa")
#set the bounary for story county
min(cleaned_data$latitude, na.rm = TRUE) #41.87786
max(cleaned_data$latitude, na.rm = TRUE) #42.18774

min(cleaned_data$longitude, na.rm = TRUE) #-93.71566
max(cleaned_data$longitude, na.rm = TRUE) #-93.30939

story_county_data <- iowa_data %>%filter(lat>=35 & lat<=45 & long >= -95 & long <=-90)


