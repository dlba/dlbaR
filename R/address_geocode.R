# Hello, world!

address_geocode <- function (df, address_column, crs = 4326) {

  address <- enquo(address_column)

  df$OBJECTID <- 1:nrow(df)

  adr_df <- dplyr::select(df, OBJECTID, !!address)
  names(adr_df) <- c("OBJECTID", "SingleLine")


  if (nrow(df)>1000) {

    df_list <- split(adr_df, ceiling(seq(nrow(adr_df))/1000))

    adr_json <- lapply(df_list, function(x){


      tmp_list <- apply(x, 1, function(i) list(attributes = as.list(i)))

      tmp_list <- unname(tmp_list)

      tmp_list <- lapply(tmp_list, function(i) {
        i$attributes$OBJECTID <- as.numeric(i$attributes$OBJECTID)
        i
      })
      adr_json <- jsonlite::toJSON(list(records = tmp_list), auto_unbox = TRUE)

      adr_json <-
        jsonlite::toJSON(list(records = tmp_list), auto_unbox = TRUE)


      return(adr_json)
    }



      )


    results <-lapply(adr_json, function(x) {
      #https://opengis.detroitmi.gov/opengis/rest/services/BaseUnits/BaseUnitGeocoder/GeocodeServer/geocodeAddresses
      #https://opengis.detroitmi.gov/opengis/rest/services/Geocoders/CompositeGeocoder/GeocodeServer/geocodeAddresses
      response <- httr::POST(url = "https://opengis.detroitmi.gov/opengis/rest/services/BaseUnits/BaseUnitGeocoder/GeocodeServer/geocodeAddresses",
                             body = list(addresses = x, f = "json", outSR = crs))

      response <- jsonlite::fromJSON(httr::content(response,
                                                   "text"), flatten = TRUE)

      results <- response[["locations"]]
      results[is.na(results)] <- NA

      return(results)

    })

    results <- do.call(rbind, results)

    results <- results[,c('attributes.ShortLabel',"attributes.parcel_id",'attributes.ResultID', 
                          'attributes.address_id', 'attributes.building_id','attributes.Score')]








  }
  else {

    tmp_list <- apply(adr_df, 1, function(i) list(attributes = as.list(i)))

    tmp_list <- unname(tmp_list)

    tmp_list <- lapply(tmp_list, function(i) {
      i$attributes$OBJECTID <- as.numeric(i$attributes$OBJECTID)
      i
    })
    adr_json <- jsonlite::toJSON(list(records = tmp_list), auto_unbox = TRUE)

    adr_json <-
      jsonlite::toJSON(list(records = tmp_list), auto_unbox = TRUE)



    response <- httr::POST(url = "https://opengis.detroitmi.gov/opengis/rest/services/Geocoders/CompositeGeocoder/GeocodeServer/geocodeAddresses",
                           body = list(addresses = adr_json, f = "json", outSR = crs))
    response <- jsonlite::fromJSON(httr::content(response,
                                                 "text"), flatten = TRUE)
    results <- response[["locations"]]
    results[is.na(results)] <- NA

    results <- results[,c('attributes.ShortLabel',"attributes.parcel_id","attributes.ResultID", 
                          'attributes.address_id', 'attributes.building_id', 'attributes.Score')]

    return(results)
  }
  return(results)
}
