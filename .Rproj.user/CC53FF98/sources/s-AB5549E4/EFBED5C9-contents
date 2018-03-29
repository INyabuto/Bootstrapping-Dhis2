library(httr)
library(dplyr)
library(jsonlite)
library(RCurl)


#' Extraction transformation and loading (ETL)
#' Data exchange between national dhis2 and giz dhis2
#' 

# Set up ==================================================================+++

dest.user <- "admin"
dest.pass <- "district"
source.user <- "INyabuto"
source.pass <- "Nyabuto12"
dest.url <- "https://192.168.0.16/dhis/api/"
source.url <- "https://hiskenya.org/api/"
property.url <- ".json?paging=false&links=false"


# =============================================================================

# Specify target to extract metadata ==========================================

targets <- c("dataElements", 
            "organisationUnits",
            "indicators")

# Get metadata from our local dhis2 instance i.e GIZ-HSP

pb <- winProgressBar(title = "GIZ dhis2 Metadata Pull", label = "0% done", min = 0, max = length(targets), width = 300)
for (i in seq_along(targets)){
  obj <- getMetadata(base.url = dest.url,
                     datatype=targets[i],
                     property.url = property.url, usr = dest.user, pwd = dest.pass)
  name <- sapply(obj$content[[targets[i]]], "[[", "displayName")
  id <- sapply(obj$content[[targets[i]]], "[[", "id")
  
  # bind
  temp <- cbind(name, id)
  
  # Recast as a data frame
  df <- as.data.frame(temp, stringsAsFactors = FALSE,
                      row.names = 1:nrow(temp))
  
  info <- sprintf("%d%% done", round(i/length(targets)*100))
  setWinProgressBar(pb, i, label = info)
  assign(targets[i], df)
  remove(df)
}
close(pb)

# Get metadata from dhis2 national instance
pb <- winProgressBar(title = "Kenya dhis2 Metadata Pull", label = "0% done", min = 0, max = length(targets), width = 300)
for (i in seq_along(targets)){
  url2 <- targets[i]
  obj <- getMetadata(base.url = source.url,
                     datatype = targets[i],
                     property.url = property.url,
                     usr = source.user,
                     pwd = source.pass,
                     ssl = TRUE)

  name <- sapply(obj$content[[targets[i]]], "[[", "displayName")
  id <- sapply(obj$content[[targets[i]]], "[[", "id")
  
  # bind
  temp <- cbind(name, id)
  
  # Recast as a data frame
  df <- as.data.frame(temp, stringsAsFactors = FALSE,
                      row.names = 1:nrow(temp))
  
  info <- sprintf("%d%% done", round(i/length(targets)*100))
  setWinProgressBar(pb, i, label = info)
  assign(paste(targets[i], "ref", sep = "."), df)
  remove(df)
}
close(pb)


# ==========================================================================

# ============= TRANSFORMATION

# ===========================================================================

# Match giz instance metadata with national instance metadata and extaract 
# the matching metadata from the national instance 
# elements to be matched include dataElements and OrgUnisation units

# Load dataelements dictionary

data.dict <- read.csv("dataElements_meta.csv", stringsAsFactors = FALSE)

#Get the matching data elements metadata
data.elements <- getDataElementsMeta(dataElements.ref, indicators.ref,data.dict)

# Match organisation units
# Focal counties
focal.counties <- c("Kisumu County", "Vihiga County", "Siaya County", "Kisumu County", "Kwale County")

org.units <- getFocalOrgUnitsMeta(organisationUnits.ref, focal.counties)


# Get the data =========================================================
# montly periods
period <- paste(paste0("2015",
                       sprintf("%02d", seq(from = 1, to = 12, by = 1)),
                       collapse = ";"),
                paste0("2016",
                       sprintf("%02d", seq(from = 1, to = 12, by = 1)),
                       collapse = ";"),
                paste0("2017",
                       sprintf("%02d", seq(from = 1, to = 12, by =1)),
                       collapse = ";"),
                sep = ";")

# get data from Kenya-dhis2 and push to giz dhis2
pb <- winProgressBar(title = "Kenya - GIZ data exchange (pull & push)",
                     label = "0% done",
                     min = 0,
                     max = nrow(data.elements))
for(i in seq_along(1:nrow(data.elements))){
  for(j in seq_along(1:nrow(org.units))){
    obj <- getDataValuesLevelwise(base.url = source.url,
                         dx = data.elements$id[i],
                         pe = period,
                         ou = org.units$id[j],
                         usr = source.user,
                         pwd = source.pass)
    data <- as.data.frame(obj$content$rows, stringsAsFactors = FALSE)
    names(data) <- obj$content$headers$column
    
    #map the data into something readable
    #data <- mapDataValues(data, data.elements, org.units)
    data$Data <- plyr::mapvalues(data$Data,
                                 data.elements$id,
                                 as.character(data.elements$name), warn_missing = F)
    data$`Organisation unit` <- plyr::mapvalues(data$`Organisation unit`,
                                                organisationUnits.ref$id,
                                                as.character(organisationUnits.ref$name), warn_missing = F)
    
    assign(paste(data.elements$name[i], "dhis", sep = "."), data)
    
    # remap the data according to giz metadata.
    
    data$Data <- plyr::mapvalues(data$Data,
                                 data.dict$dhis.name,
                                 as.character(data.dict$id),warn_missing = F)
    data$`Organisation unit` <- plyr::mapvalues(data$`Organisation unit`,
                                                organisationUnits$name,
                                                as.character(organisationUnits$id), warn_missing = F)
    
    names(data) <- c("dataElement","period","orgUnit","value")
    
    
    #data <- remapDataValue(data.value = data,
    #                       data.dict = data.dict,
    #                       org.ref = organisationUnits)
    assign(paste(data.elements$name[i], "giz", sep = "."), data)
    
    # Load the data into giz dhis2
    r <- postDataValues(base.url = dest.url,
                   data = data,
                   datatype = "dataValues",
                   usr = dest.user,
                   pwd = dest.pass)
    
    # assign a post response to each data element
    assign(paste(data.elements$name[i], "status_code", sep = "."), r)
    info <- sprintf("%d%% done", round(i/nrow(data.elements)*100))
    setWinProgressBar(pb, i, label = info)
  }
}
close(pb)



