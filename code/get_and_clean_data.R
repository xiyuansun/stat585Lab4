#retrieve data

library(readr)
Iowa_Liquor_Sales_Story <- read_csv("~/Desktop/stat585Lab4/data/Iowa_Liquor_Sales-Story.csv")
View(Iowa_Liquor_Sales_Story)
initialjson <- jsonlite::read_json("https://data.iowa.gov/resource/m3tr-qhgy.json")
test_json_file <- initialjson[[1]]
invoice <- test_json_file$invoice_line_no
date <- gsub("[A-z].*$","",test_json_file$date)



# install.packages("devtools")
# devtools::install_github("Chicago/RSocrata")
# 
# library("RSocrata")
# 
# df <- read.socrata(
#   "https://data.iowa.gov/resource/m3tr-qhgy.json",
#   app_token = "YOURAPPTOKENHERE",
#   email     = "user@example.com",
#   password  = "fakepassword"
# )
#clean data