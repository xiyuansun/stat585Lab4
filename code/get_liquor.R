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
  #liquor_data <- as.data.frame(liquor_data)
  return(liquor_data)
  
}
