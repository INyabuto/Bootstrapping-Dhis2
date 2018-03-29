library(httr)
library(jsonlite)

# a function that accepts types of metadata, url and property.url and return a dataframes of 
# of the different metadata types (datatype)

getMetadata <- function(base.url, datatype, property.url, usr, pwd, ssl = FALSE){
    url2 <- datatype
    url <- paste0(base.url, url2, property.url)
    response <- GET(url,
                    authenticate(usr, pwd),
                    config = config(ssl_verifypeer = ssl))
    
    # verify if the API returned a json file
    if (httr::http_type(response) != "application/json"){
      stop("API did not return JSON", call. = FALSE)
    }
    
    # parse the response into an R object
    res <- httr::content(response, "text")
    parsed <- jsonlite::fromJSON(res, simplifyVector = FALSE)
    
    # turn the API errrs into R errors
    if (httr::http_error(response)){
      stop(
        sprintf(
          "getMetadata API request failed [%s]\n%s\n%s",
          status_code(response),
          parsed$message,
          parsed$documentation_url
        ),
        call. = FASLE
      )
    }
    
    # Return a helpful S3 object, i.e return the response, parsed object and 
    # provide a nice print method.
    # this makes debaugging much easier
    
    structure(
      list(
        content = parsed,
        path = url,
        response = response
      ),
      class = "getMetadata"
    )
    
    # Extract meta data names and ids
    #name <- sapply(parsed[[datatype]], "[[", "displayName")
    #id <- sapply(parsed[[datatype]], "[[", "id")
    
    # bind
    #temp <- cbind(name, id)
    
    # Recast as a data frame
    #df <- as.data.frame(temp, stringsAsFactors = FALSE,
    #                    row.names = 1:nrow(temp))

    #assertthat::assert_that(response$status_code == 200)
    #return(df)
}
# print method
print.getMetadata <- function(x, ...){
  cat("<Metadata ", x$url, ">\n", sep = "")
  str(x$content)
  invisible(x)
}



#datatype <- c("dataSets",
            # "eventReports",
            # "programIndicators")

#for (i in seq_along(datatype)) {
#  df <- getMetadata(datatype = datatype[i], base.url = base.url, property.url = property.url, "INyabuto", "Nyabuto12")
#  assign(datatype[i], df)
#  remove(df)
#}


# A function that matches and filters dataElements and Indicators based on the data dictionary
# The fuctions returns the matching data items (id, name)
# The dictionary df has dhis2 matching name in column: dhis.name

getDataElementsMeta <- function(dataElements.source, indicators.source, data.dict.dest){
  dataElements.match<- dplyr::filter(dataElements.source, name %in% data.dict.dest$dhis.name)
  indicators.match<- dplyr::filter(indicators.source, name %in% data.dict.dest$dhis.name)
  data.item <- rbind(dataElements.match, indicators.match)
  return(data.item)
}

getFocalOrgUnitsMeta <- function(organisationUnits.source, FocalCounties){
  org <- dplyr::filter(organisationUnits.source, name %in% FocalCounties)
  return(org)
}

#'getDataValues is a function that interacts with dhis2 analycal API and provides data 
#'for the last 12 monts by deafult.
#'ou is a vector of organisation units id
#'auth is a vector containing username and password
getDataValues <- function(base.url, end.point = "analytics.json?", dx, pe = "LAST_12_MONTHS", ou, property.url="displayProperty=NAME&skipMeta=TRUE", usr, pwd, ssl = TRUE){
  urlA <- paste0(base.url, end.point)
  urlB <- paste0("dimension=dx:", dx)
  urlC <- paste0("dimension=pe:", pe)
  urlD <- paste0("dimension=ou:", paste(ou, "LEVEL-3", "LEVEL-4", "LEVEL-5", "LEVEL-6", "LEVEL-7", "LEVEL-8", sep = ";"))
  urlE <- property.url
  
  # constract the urlb
  url <- paste(paste0(urlA, urlB),
               urlC, urlD, urlE,
               sep = "&")
  
  # send a get request
  response <- GET(url,
                 authenticate(usr, pwd),
                 timeout(60),
                 config = config(ssl_verifypeer = ssl))
  
  # Verify that the response returned a json file
  if (httr::http_type(response)!= "application/json"){
    stop("API did not return JSON", call. = FALSE)
  }
  
  # parse the response into an R object
  res <- httr::content(response, "text")
  parsed <- fromJSON(res, flatten = TRUE)
  
  # turn API errors into R errors
  if (httr::http_error(response)){
    stop(
      sprintf(
        "getDataValues API request failed [%s]\n%s\n%s",
        status_code(response),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FASLE
    )
  }
  
  # Return a helpful S3 object, i.e return the response, parsed object and 
  # provide a nice print method.
  
  # this makes debaugging much easier
  
  structure(
    list(
      content = parsed,
      path = url,
      response = response
    ),
    class = "getDataValues"
  )
}

# print method
print.getDataValues <- function(x, ...){
  cat("<DataValues", x$url, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

#'
#'ou is a vector of organisation units id
#'auth is a vector containing username and password
getDataValuesLevelwise <- function(base.url, end.point = "analytics.json?", dx, pe = "LAST_12_MONTHS", ou, property.url="displayProperty=NAME&skipMeta=TRUE", usr, pwd){
  urlA <- paste0(base.url, end.point)
  urlB <- paste0("dimension=dx:", dx)
  urlC <- paste0("dimension=pe:", pe)
  urlD <- paste0("dimension=ou:", ou)
  urlE <- property.url
  
  # constract the url
  url <- paste(paste0(urlA, urlB),
               urlC, urlD, urlE,
               sep = "&")
  
  # send a get request
  response <- GET(url,
                   authenticate(usr, pwd),
                   timeout(60))
  
  # verify that the response returned a json file
  if(httr::http_type(response)!= "application/json"){
    stop("API did not return json", call. = FALSE)
  }
  
  # parse the response into an R object
  res <- httr::content(response, "text")
  parsed <- fromJSON(res, flatten = TRUE)
  
  # Convert http errors into R errors
  if (httr::http_error(response)){
    stop(
      sprintf(
        "getDataValuesLevelwise API request failed [%s]\n%s\n%s",
        status_code(response),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FASLE
    )
  }
  
  # Return a helpful S3 object, i.e return the response, parsed object and 
  # provide a nice print method.
  # this makes debaugging much easier
  
  structure(
    list(
      content = parsed,
      path = url,
      response = response
    ),
    class = "getDataValuesLevelwise"
  )
}
# print method
print.getDataValuesLevelwise <- function(x, ...){
  cat("<DataValueslevelwise", x$url, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

mapDataValues <- function(data.value, data.ref, org.ref){
  # Remap the UIDs into something readable
  data.value$Data <- plyr::mapvalues(data.value$Data, data.ref$id,
                               as.character(data.ref$name), warn_missing = F)
  data.value$`Organisation unit` <- plyr::mapvalues(data.value$`Organisation unit`,
                                              org.ref$id,
                                              as.character(org.ref$name), warn_missing = F)
  return(data.value)
}


# A function to remap data.value into uids and value for import
# data.value = the data to be transformed,
# data.dict = the data to be mapped from. should contain a column dhis.name
remapDataValue <- function(data.value, data.dict, org.ref){
  data.value$Data <- plyr::mapvalues(data.value$Data, data.dict$dhis.name,
                               as.character(data.dict$id), warn_missing = F)
  data.value$`Organisation unit`  <- plyr::mapvalues(data.value$`Organisation unit`,
                                               org.ref$name,
                                               as.character(org.ref$id), warn_missing = F)
  names(data) <- c("dataElement","period","orgUnit","value")
  return(data)
}


# 
postDataValues <- function(base.url, data, datatype, endpoint = "dataValueSets?", property.url = "preheatCache=true&skipExistingCheck=true", ssl = FALSE, usr, pwd){
  url <- paste0(base.url, endpoint, property.url)
  r <- POST(url,
            body = list(datatype = data),
            encode = "json",
            config = config(ssl_verifypeer = ssl),
            authenticate(usr, pwd))
  assertthat::assert_that(r$status_code == 200)
  return(r$status_code)
}








