Stat 585 Lab 4 - Group 5
================
Katherine Goode, Gina Nichols, Xiyuan Sun, Ying Zheng
04/21/2019

Our GitHub repository for this lab can be found [here](https://github.com/xiyuansun/stat585Lab4).

``` r
# Load libraries
library(tidyverse)
```

    FALSE ── Attaching packages ──────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    FALSE ✔ ggplot2 3.1.0       ✔ purrr   0.3.2  
    FALSE ✔ tibble  2.1.1       ✔ dplyr   0.8.0.1
    FALSE ✔ tidyr   0.8.3       ✔ stringr 1.4.0  
    FALSE ✔ readr   1.3.1       ✔ forcats 0.4.0

    FALSE ── Conflicts ─────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    FALSE ✖ dplyr::filter() masks stats::filter()
    FALSE ✖ dplyr::lag()    masks stats::lag()

Iowa Liquor Sales Data
======================

### Demonstration of Reading the Data from the API

### Data Cleaning

``` r
# Read in data
story_liquor <- read_csv("data/Iowa_Liquor_Sales-Story.csv")
```

    FALSE Parsed with column specification:
    FALSE cols(
    FALSE   .default = col_double(),
    FALSE   `Invoice/Item Number` = col_character(),
    FALSE   Date = col_character(),
    FALSE   `Store Name` = col_character(),
    FALSE   Address = col_character(),
    FALSE   City = col_character(),
    FALSE   `Store Location` = col_character(),
    FALSE   County = col_character(),
    FALSE   `Category Name` = col_character(),
    FALSE   `Vendor Name` = col_character(),
    FALSE   `Item Description` = col_character()
    FALSE )

    FALSE See spec(...) for full column specifications.

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
            funs(factor(.) %>% tolower())) %>%
  mutate_if(.predicate = is.factor, .funs = tolower)
```

Shiny App Visualizations
========================
