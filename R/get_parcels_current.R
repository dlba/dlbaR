# Pass in required fields into function as argument
 
get_parcels_current <- function(fields) { 
library(httr)
library(dplyr)
library(tidyr)
 
fields_string <- paste0(fields, collapse=",")

# Updated Parcels (Current) filepath 
url <- "https://services2.arcgis.com/qvkbeam7Wirps6zC/arcgis/rest/services/parcel_file_current/FeatureServer/0/query"

# Returns count of unique IDs in Parcel file
ids <- GET(url,
  query=list(
    where='1=1',
    #returnIdsOnly='true',
    returnCountOnly='true',
    f='pjson'
  )) %>%
    httr::content()

# Variable containing rows of parcel data
parcels <- list()

# Counter variable to keep track of iterations
i <- 0
j <- 0

for(i in 1:round(ids$count/1000+1)) {
  #Tracks progress of for loop
  print(c(i, j))

  query <- POST(url, encode="form",  # this will set the header for you
                            body=list(
                              resultOffset=j,
                              where='1=1',
                              returnGeometry=F,
                              outFields=fields_string,
                              f='pjson'
                              ))

  # Check for errors
  stop_for_status(query)
  
  # Parse response
  content <- httr::content(query)

  # Ensure response contains features
  if (!is.null(content$features)) {
    response <- content$features %>% unlist(recursive = FALSE)
    
    # Convert to dataframe
    response_df <- do.call(rbind, response) %>% as.data.frame()

    # Unnest columns (ensure 'fields' is correctly defined)
    response_df <- tidyr::unnest(response_df, cols = all_of(fields))

    # Store results
    parcels[[i]] <- response_df
  } else {
    print(paste("No features returned for batch", i))
  }

  j <- j+1000


}

parcels_bounded <- do.call(rbind, parcels)

return(parcels_bounded)
  
}