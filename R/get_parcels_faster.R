

#
#not set up to return geoms yet
get_parcels_faster <- function(fields=c('object_id', 'parcel_id'),returnGeom=F ) {
  library(httr)
  library(dplyr)

    fields_string <- paste0(fields, collapse=",")



  returnGeom <- tolower(as.character(returnGeom))

  #url <- "https://services2.arcgis.com/qvkbeam7Wirps6zC/arcgis/rest/services/Parcels_Current/FeatureServer/0/query" #deprecated
  url <- "https://services2.arcgis.com/qvkbeam7Wirps6zC/arcgis/rest/services/parcel_file_current/FeatureServer/0/query"

  ids <- GET(url,
             query=list(
               where='1=1',
               #returnIdsOnly='true',
               returnCountOnly='true',
               f='pjson'
             )) %>%
    httr::content()


  return <- list()
  j <- 0
  for(i in 1:round(ids$count/1000+1)) { # new parcel file allows 1000 records in one query
    print(c(i, j))
    query <- POST(url, encode="form",                      # this will set the header for you
                            #body=list(file=upload_file("example.txt")),   # this is how to upload files
                            body=list(
                              #objectIds=paste0(unlist(x),collapse=','),
                              resultOffset=j,
                              where='1=1',
                              returnGeometry=returnGeom,
                              outFields=fields_string,
                              f='pjson'
                              ))

    #get geoms if requested
    if(returnGeom=='true') {
      geoms <- return_geoms_helper(query=query, out_fields = fields)
      return[[i]] <- geoms

    } else {
      response <- query %>%
        httr::content()%>%
        .$features %>%
        unlist(recursive=F)

        names(response) <- NULL

        response <- tidyr::unnest(as.data.frame(do.call(rbind, response)),cols=all_of(fields))

      return[[i]] <- response }

      j <- j+1000

  }

  return <- do.call(rbind, return)

  return(return)

  #splits <- split(ids, ceiling(seq_along(ids)/2000))


  # a <- lapply(splits,function(x){
  #   query <- POST(url, encode="form",                      # this will set the header for you
  #           #body=list(file=upload_file("example.txt")),   # this is how to upload files
  #           body=list(
  #             objectIds=paste0(unlist(x),collapse=','),
  #             returnGeometry=returnGeom,
  #             outFields=fields_string,
  #             f='pjson',
  #             returnExceededLimitFeatures="true"
  #             ))
  #   response <- query %>%
  #     httr::content()%>%
  #   .$features %>%
  #     unlist(recursive=F)
  #
  #   names(response) <- NULL
  #   response <- tidyr::unnest(as.data.frame(do.call(rbind, response)),cols=all_of(fields))
  #   }
  #
  # )






  #a <- do.call(rbind, a)

}

