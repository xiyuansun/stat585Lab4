Stat 585 Lab 4 - Group 5
================
Katherine Goode, Gina Nichols, Xiyuan Sun, Ying Zheng
04/21/2019

Our GitHub repository for this lab can be found [here](https://github.com/xiyuansun/stat585Lab4).

``` r
# Load libraries
library(tidyverse)
```

Iowa Liquor Sales Data
======================

### Demonstration of Reading the Data from the API

### Data Cleaning

``` r
# Read in data
story_liquor <- read_csv("data/Iowa_Liquor_Sales-Story.csv")
```

``` r
# Data cleaning
story_liquor <- story_liquor %>%
  # Remove spaces between words and send words to lower case
  rename_all(funs(str_replace_all(., "[\\(\\)_\\/ ]", "") %>% tolower(.))) %>%
  mutate(date = lubridate::mdy(date)) %>%
  mutate_at(.vars = c("storenumber", "storename", "address", "city", 
                      "zipcode", "storelocation", "countynumber",
                      "county", "category", "categoryname", "vendornumber",
                      "vendorname", "itemnumber"), 
            funs(factor(.) %>% tolower()))
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## please use list() instead
    ## 
    ## # Before:
    ## funs(name = f(.)
    ## 
    ## # After: 
    ## list(name = ~f(.))
    ## This warning is displayed once per session.

Shiny App Visualizations
========================
