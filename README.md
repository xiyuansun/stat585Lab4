
# STAT 585 Lab 4 - Group 5

**Katherine Goode, Gina Nichols, Xiyuan Sun, Ying Zheng**  
**04/21/2019**

This document contains an overview of our work for lab 4. Our GitHub
repository for this lab can be found
[here](https://github.com/xiyuansun/stat585Lab4).

``` r
# Libraries used
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
```

## Iowa Liquor Sales Data

#### Demonstration of Accessing the Data from the API

``` r
#get_liquor function
get_liquor <- function(url){
  initialjson <- jsonlite::read_json(url)
  tot_length <- length(initialjson)
  liquor_data <- list()
  for(i in 1:tot_length){
    test_json_file <- initialjson[[i]]
    
    invoice <- test_json_file$invoice_line_no
    date <- gsub("[A-z].*$","",test_json_file$date)
    store_number <- test_json_file$store
    store_name <- test_json_file$name
    address <- test_json_file$address
    city <- test_json_file$city
    zipcode <- test_json_file$zipcode
    store_location <-paste0(address, " \n ", city, zipcode," \n (",
                            as.numeric(unlist(test_json_file$store_location)[-1])[1],", ", 
                            as.numeric(unlist(test_json_file$store_location)[-1])[2], ")")
    county_number <- test_json_file$county_number
    county <- test_json_file$county
    category <- test_json_file$category
    category_name <- test_json_file$category_name
    vendor_number <- test_json_file$vendor_no
    vendor_name <- test_json_file$vendor_name
    item_number <- test_json_file$itemno
    item_descp <- test_json_file$im_desc
    pack <- test_json_file$pack
    bottle_vol <- test_json_file$bottle_volume_ml 
    state_bottle_cost <- test_json_file$state_bottle_cost
    state_bottle_retail <- test_json_file$state_bottle_retail
    sale_bottles <- test_json_file$sale_bottles
    sale_dollars <- test_json_file$sale_dollars
    sale_liters <- test_json_file$sale_liters
    sale_gallons <- test_json_file$sale_gallons
    
    result_df <- as.data.frame(cbind(invoice, date, store_number, store_name, 
                       address, city, zipcode, store_location,
                       county_number, county, category, category_name,
                       vendor_number, vendor_name, item_number, item_descp,pack,
                       bottle_vol, state_bottle_cost, state_bottle_retail, sale_bottles,sale_dollars, 
                       sale_liters, sale_gallons))
    
    liquor_data[[i]]<- result_df
    
  }
  
  
  return(liquor_data)
  
}

# access the data from the API

liquor_list <- get_liquor(url="https://data.iowa.gov/resource/m3tr-qhgy.json")

# convert lists to a data frame


#skip those rows
liquor_df <- c()
for(i in 1:1000){
  liquor_df <- rbind(liquor_df, liquor_list[[i]])
}

# Glimpse the data
glimpse(liquor_df)
```

    ## Observations: 1,000
    ## Variables: 24
    ## $ invoice             <fct> S19803100026, S18258100091, S09714000020, S1…
    ## $ date                <fct> 2014-06-27, 2014-04-07, 2012-12-27, 2013-06-…
    ## $ store_number        <fct> 2568, 3612, 4609, 3495, 4800, 4137, 2601, 25…
    ## $ store_name          <fct> Hy-Vee Food Store #4 / Cedar Rapids, B and C…
    ## $ address             <fct> "1556 FIRST AVENUE NE", "509 E PLATT", "165 …
    ## $ city                <fct> CEDAR RAPIDS, MAQUOKETA, ANKENY, MONTICELLO,…
    ## $ zipcode             <fct> 52402, 52060, 50023, 52310, 50320, 50263, 52…
    ## $ store_location      <fct> "1556 FIRST AVENUE NE \n CEDAR RAPIDS52402 \…
    ## $ county_number       <fct> 57, 49, 77, 53, 77, 25, 51, 17, 19, 77, 07, …
    ## $ county              <fct> Linn, Jackson, Polk, Jones, Polk, Dallas, Je…
    ## $ category            <fct> 1031080, 1062310, 1022100, 1081030, 1032080,…
    ## $ category_name       <fct> VODKA 80 PROOF, SPICED RUM, TEQUILA, COFFEE …
    ## $ vendor_number       <fct> 434, 260, 395, 370, 35, 380, 434, 297, 115, …
    ## $ vendor_name         <fct> "Luxco-St Louis", "Diageo Americas", "Proxim…
    ## $ item_number         <fct> 36668, 43334, 89196, 67527, 34433, 37346, 36…
    ## $ item_descp          <fct> Korski Vodka, Captain Morgan Spiced Rum, Jos…
    ## $ pack                <fct> 6, 24, 12, 12, 12, 12, 6, 12, 6, 12, 6, 6, 6…
    ## $ bottle_vol          <fct> 1750, 375, 750, 1000, 750, 750, 1750, 750, 1…
    ## $ state_bottle_cost   <fct> 7.04, 5, 10.49, 15.25, 18.49, 3.48, 7.17, 3.…
    ## $ state_bottle_retail <fct> 10.56, 7.5, 15.73, 22.87, 27.74, 5.23, 10.76…
    ## $ sale_bottles        <fct> 6, 3, 3, 1, 12, 12, 24, 12, 24, 168, 6, 6, 6…
    ## $ sale_dollars        <fct> 63.36, 22.5, 47.19, 22.87, 332.88, 62.76, 25…
    ## $ sale_liters         <fct> 10.5, 1.125, 2.25, 1, 9, 9, 42, 9, 42, 126, …
    ## $ sale_gallons        <fct> 2.77380654976, 0.2971935589, 0.5943871178, 0…

#### Data Cleaning

For the Shiny app, we will use the full Story County liquor sales
dataset provided by Dr. Hofmann. This data is read in below.

``` r
# Read in data
story_liquor_raw <- read_csv("data/Iowa_Liquor_Sales-Story.csv", col_types = cols())
```

We cleaned up the data before using it in the Shiny app. This involved
renaming the variables, adjusting the variable types, and dealing with
typos. This work is all included in the following section of code.

``` r
# Clean data
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
```

A glimpse of the cleaned data is shown below.

``` r
# Glimpse the data
glimpse(story_liquor_cleaned)
```

    ## Observations: 518,050
    ## Variables: 27
    ## $ invoiceitemnumber <chr> "s27407400005", "s07743200026", "s03538200068"…
    ## $ date              <date> 2015-08-19, 2012-09-17, 2012-01-12, 2014-04-2…
    ## $ storenumber       <fct> 2501, 3524, 4509, 4987, 2500, 4509, 4509, 4081…
    ## $ storename         <chr> "hy-vee  #2 / ames", "sam's club 6568 / ames",…
    ## $ address           <chr> "640 lincoln way", "305 airport rd", "2515 cha…
    ## $ city              <fct> ames, ames, ames, ames, ames, ames, ames, stor…
    ## $ zipcode           <fct> 50010, 50010, 50010, 50010, 50010, 50010, 5001…
    ## $ storelocation     <chr> "(42.022848, -93.619455)", "(42.001123, -93.61…
    ## $ latitude          <dbl> 42.02285, 42.00112, 42.02146, 42.02292, 42.023…
    ## $ longitude         <dbl> -93.61946, -93.61365, -93.65096, -93.61652, -9…
    ## $ countynumber      <fct> 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85…
    ## $ county            <fct> story, story, story, story, story, story, stor…
    ## $ category          <fct> 1011300, 1011500, 1081340, 1081200, 1081900, 1…
    ## $ categoryname      <chr> "tennessee whiskies", "straight rye whiskies",…
    ## $ catsimp           <fct> whisky, whisky, schnapps, liqueur, liqueur, vo…
    ## $ vendornumber      <fct> 85, 255, 380, 260, 434, 65, 434, 297, 492, 260…
    ## $ vendorname        <fct> brownforman corporation, wilson daniels ltd, p…
    ## $ itemnumber        <fct> 26821, 27102, 84617, 68036, 76526, 35770, 3165…
    ## $ itemdescription   <chr> "jack daniels old #7 black lbl mini", "templet…
    ## $ pack              <fct> 12, 6, 12, 12, 12, 12, 12, 24, 6, 12, 24, 12, …
    ## $ bottlevolumeml    <int> 500, 750, 1000, 750, 750, 750, 1000, 375, 750,…
    ## $ statebottlecost   <dbl> 9.06, 18.08, 4.89, 13.00, 5.30, 7.87, 4.30, 1.…
    ## $ statebottleretail <dbl> 13.59, 27.13, 7.33, 19.49, 7.95, 11.81, 6.44, …
    ## $ bottlessold       <int> 1, 6, 6, 3, 1, 3, 12, 24, 6, 2, 3, 12, 6, 12, …
    ## $ saledollars       <dbl> 13.59, 162.78, 43.98, 58.47, 7.95, 35.43, 77.2…
    ## $ volumesoldliters  <dbl> 0.50, 4.50, 6.00, 2.25, 0.75, 2.25, 12.00, 9.0…
    ## $ volumesoldgallons <dbl> 0.13, 1.19, 1.59, 0.59, 0.20, 0.59, 3.17, 2.38…

## Shiny App Visualizations
