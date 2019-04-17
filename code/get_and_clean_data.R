#retrieve data

library(readr)
Iowa_Liquor_Sales_Story <- read_csv("~/Desktop/stat585Lab4/data/Iowa_Liquor_Sales-Story.csv")
View(Iowa_Liquor_Sales_Story)
initialjson <- jsonlite::read_json("https://data.iowa.gov/resource/m3tr-qhgy.json")

test_json_file <- initialjson[[1]]

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
##############################

#test the get function

liquor_list <- get_liquor(url="https://data.iowa.gov/resource/m3tr-qhgy.json")

col_vec <- c(0)

for (i in 1:1000){
  col_vec[i] <- ncol(liquor_list[[i]])
}

sum(col_vec!=24)
which(col_vec!=24)
#107 161 502

liquor_df <- c()
for(i in c(1:106, 108:160, 162:501, 503:1000)){
  liquor_df <- rbind(liquor_df, liquor_list[[i]])
}



