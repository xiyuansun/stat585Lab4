
STAT 585 Lab 4 - Group 5
========================

**Katherine Goode, Gina Nichols, Xiyuan Sun, Ying Zheng**
**04/21/2019**

This document contains an overview of our work for lab 4. Our GitHub repository for this lab can be found [here](https://github.com/xiyuansun/stat585Lab4).

``` r
# Libraries used
library(lubridate) # for working with dates
library(zoo)
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(grid)   # for arrangeing plots
library(dplyr)  # for subsetting by season
library(tidyverse)
library(shiny)
```

Iowa Liquor Sales Data
----------------------

#### Demonstration of Accessing the Data from the API

The code below demonstrates how to access the data from the API. Since we do not have a key, this accesses a subset of the data.

``` r
story_liquor_raw_subset <- jsonlite::fromJSON("https://data.iowa.gov/resource/m3tr-qhgy.json")
```

Below is a glimpse of the obtained dataset.

``` r
glimpse(story_liquor_raw_subset)
```

    ## Observations: 1,000
    ## Variables: 32
    ## $ `:@computed_region_3r5t_5243` <chr> "194", "153", "831", "767", "483",…
    ## $ `:@computed_region_e7ym_nrbf` <chr> "287", "1790", "1878", "286", "187…
    ## $ `:@computed_region_i9mz_6gmt` <chr> "264", "80", "316", "74", "345", "…
    ## $ `:@computed_region_uhgg_e8y2` <chr> "45", "43", "64", "44", "64", "62"…
    ## $ `:@computed_region_wnea_7qqw` <chr> "18", "7", "25", "7", "29", "27", …
    ## $ address                       <chr> "1556 FIRST AVENUE NE", "509 E PLA…
    ## $ bottle_volume_ml              <chr> "1750", "375", "750", "1000", "750…
    ## $ category                      <chr> "1031080", "1062310", "1022100", "…
    ## $ category_name                 <chr> "VODKA 80 PROOF", "SPICED RUM", "T…
    ## $ city                          <chr> "CEDAR RAPIDS", "MAQUOKETA", "ANKE…
    ## $ county                        <chr> "Linn", "Jackson", "Polk", "Jones"…
    ## $ county_number                 <chr> "57", "49", "77", "53", "77", "25"…
    ## $ date                          <chr> "2014-06-27T00:00:00.000", "2014-0…
    ## $ im_desc                       <chr> "Korski Vodka", "Captain Morgan Sp…
    ## $ invoice_line_no               <chr> "S19803100026", "S18258100091", "S…
    ## $ itemno                        <chr> "36668", "43334", "89196", "67527"…
    ## $ name                          <chr> "Hy-Vee Food Store #4 / Cedar Rapi…
    ## $ pack                          <chr> "6", "24", "12", "12", "12", "12",…
    ## $ sale_bottles                  <chr> "6", "3", "3", "1", "12", "12", "2…
    ## $ sale_dollars                  <chr> "63.36", "22.5", "47.19", "22.87",…
    ## $ sale_gallons                  <chr> "2.77380654976", "0.2971935589", "…
    ## $ sale_liters                   <chr> "10.5", "1.125", "2.25", "1", "9",…
    ## $ state_bottle_cost             <chr> "7.04", "5", "10.49", "15.25", "18…
    ## $ state_bottle_retail           <chr> "10.56", "7.5", "15.73", "22.87", …
    ## $ store                         <chr> "2568", "3612", "4609", "3495", "4…
    ## $ store_location                <data.frame> <data.frame[25 x 2]>
    ## $ store_location_address        <chr> "1556 FIRST AVENUE NE", "509 E PLA…
    ## $ store_location_city           <chr> "CEDAR RAPIDS", "MAQUOKETA", "ANKE…
    ## $ store_location_zip            <chr> "52402", "52060", "50023", "52310"…
    ## $ vendor_name                   <chr> "Luxco-St Louis", "Diageo Americas…
    ## $ vendor_no                     <chr> "434", "260", "395", "370", "35", …
    ## $ zipcode                       <chr> "52402", "52060", "50023", "52310"…

#### Data Cleaning

For the Shiny app, we will use the full Story County liquor sales dataset provided by Dr. Hofmann. This data is read in below.

``` r
# Read in data
story_liquor_raw <- read_csv("data/Iowa_Liquor_Sales-Story.csv", col_types = cols())
```

We cleaned up the data before using it in the Shiny app. This involved renaming the variables, adjusting the variable types, and dealing with typos. This work is all included in the following section of code.

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
    ## $ bottlevolumeml    <dbl> 500, 750, 1000, 750, 750, 750, 1000, 375, 750,…
    ## $ statebottlecost   <dbl> 9.06, 18.08, 4.89, 13.00, 5.30, 7.87, 4.30, 1.…
    ## $ statebottleretail <dbl> 13.59, 27.13, 7.33, 19.49, 7.95, 11.81, 6.44, …
    ## $ bottlessold       <dbl> 1, 6, 6, 3, 1, 3, 12, 24, 6, 2, 3, 12, 6, 12, …
    ## $ saledollars       <dbl> 13.59, 162.78, 43.98, 58.47, 7.95, 35.43, 77.2…
    ## $ volumesoldliters  <dbl> 0.50, 4.50, 6.00, 2.25, 0.75, 2.25, 12.00, 9.0…
    ## $ volumesoldgallons <dbl> 0.13, 1.19, 1.59, 0.59, 0.20, 0.59, 3.17, 2.38…

Shiny App Visualizations
------------------------

#### Preparing Temporal Data

For the temporal visualizations, we used the cleaned data to compute the average cost per liter (in dollars per liter) by liquor category for each date in the data set.

``` r
# Prepare temporal data

story_temporal_data <- story_liquor_cleaned %>%
  mutate(dollar_per_liter =saledollars/volumesoldliters)%>%
  select(date, storename, latitude, longitude, catsimp, vendorname,dollar_per_liter)%>%
  na.omit()

story_temporal_data$month_yr <- format(as.Date(story_temporal_data$date), "%Y-%m")
story_temporal_data$yr <- year(story_temporal_data$date)
story_temporal_data <- story_temporal_data[order(as.Date(story_temporal_data$date)),]
```

#### Preparing Spatial Data

For the spatial visualizations, we used the cleaned data to compute the number of sales and average cost per liter (in dollars) by liquor category for each store in the data set.

``` r
# Prepare spatial data
story_spatial_data <- story_liquor_cleaned %>%
  
  # Select desired variables and remove any na's
  select(storename, latitude, longitude, catsimp, saledollars, volumesoldliters) %>%
  na.omit() %>%
  
  # Compute dollar per liter for each observation 
  mutate(dollar_per_liter = saledollars / volumesoldliters) %>%
  
  # Round latitude and longitude to deal with differences in locations for the
  # same store
  mutate_at(.vars = c("latitude", "longitude"),
            .funs = round,
            digits = 1) %>%
  
  # Determine the number of sales and average dollar per liter
  group_by(storename, catsimp, latitude, longitude) %>%
  summarise(count = n(), 
            ave_dollar_per_liter = round(mean(dollar_per_liter), 2)) %>%
  ungroup()
```

#### Overview of the App

We wrote a shiny app to visualize the Liquor Sale data in Story County.

The app has two tabs:

1.  Temporal tab shows daily sale dollar per liter mean of selected liquor category time series plot in the user selected year. When the category does not show up in the selected year, it will print out an error message letting the user switch to another category.

2.  The spatial tab shows a plot of the locations where liquor is sold in Story County. There are two response variables that can be viewed, and there are 13 categories of liquor types to select from. The first response variable is the average cost per liter of the liquor, and the second response variable is the total number of sales of the liquor category at a location.

The app can be run using the command below.

``` r
runApp('shiny')
```
