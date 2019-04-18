
STAT 585 Lab 4 - Group 5
========================

**Katherine Goode, Gina Nichols, Xiyuan Sun, Ying Zheng**
**04/21/2019**

This document contains an overview of our work for lab 4. Our GitHub repository for this lab can be found [here](https://github.com/xiyuansun/stat585Lab4).

``` r
# Libraries used
library(tidyverse)
```

Iowa Liquor Sales Data
----------------------

#### Demonstration of Accessing the Data from the API

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
