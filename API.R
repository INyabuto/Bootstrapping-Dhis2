library(httr)
library(jsonlite)

#Login into the system 
dest.url <- "http://192.168.0.128/dhis/"
dest.user <- "admin"
dest.pass <- "district"
source.url <- "https://localhost.org/"
source.user <- "INyabuto"
source.pass <- "Nyabuto12"

# Generation of UIDs
generateUID <- function(codeSize=11){
  #Generate a random seed
  runif(1)
  allowedLetters <- c(LETTERS, letters)
  allowedChars <- c(LETTERS, letters, 1:9)
  #first character must be a letter according to dhis2 specs
  firstChar <- sample(allowedLetters,1)
  otherChars <- sample(allowedChars, codeSize - 1)
  uid <- paste(c(firstChar,paste(
    otherChars, sep = "", collapse = "" )), sep = "", collapse = "")
  return(uid)
}

loginDhis2 <- function(base.url, usr, pwd){
  url <- paste0(base.url, "api/me")
  r <- GET(url, authenticate(usr,pwd))
  assertthat::assert_that(r$status_code == 200)
}

loginDhis2(dest.url, dest.user, dest.pass)
#================================================================
# Get organisation unitss

r <- GET(paste0(source.url,"api/metadata.json?assumeTrue=FALSE&organisationUnitLevels=TRUE"),
         authenticate(source.user, source.pass))

ou.levels <- fromJSON(content(r, "text"))

# Post the organisaition unit levels to training instance

r2 <- POST(paste0(dest.url,"api/metadata?importStrategy=CREATE&atomicMode=NONE"),
           body = toJSON(ou.levels, auto_unbox = TRUE),content_type_json(),
           authenticate(dest.user, dest.pass))
assertthat::assert_that(r2$status_code == 200)

#===============================================================================
# Set users organisation units to the root organisations units
# Get the global uid
url <- paste0(dest.url, "api/organisationUnits?filter=name:eq:Kenya&fields=id")
root_id <- fromJSON(content(GET(url),"text"))
# GET my user id
url <- paste0(dest.url, "api/me")
me <- fromJSON(content(GET(url), "text"))
# GET my use profile
url <- paste0(dest.url,"api/users/", me$id)
me <- fromJSON(content(GET(url), "text"))
# creat a field called organisations units and assign it the root id
me$organisationUnits <- list(list(id = root_id$organisationUnits$id))
# post changes to the user
url <- paste0(dest.url, "api/metadata?importSrategy=UPDATE")
r <- POST(url, body = toJSON(list(users=list(me)), auto_unbox = TRUE), content_type_json())
assertthat::assert_that(r$status_code==200)
#======================================================================

# GET data data elements 
r <- GET(paste0(source.url,"api/metadata.json?assumeTrue=FALSE&dataElements=TRUE"),
         authenticate(source.user, source.pass))

dataelements <- fromJSON(content(r,"text"))

# select only the required fields
indexNums <- which(names(dataelements$dataElements) %in% c("userGroupAccesses", "user", "attributeValues","lastUpdatedBy","publicAccess","translations","userAccesses","lastUpdated","optionSet","aggregationLevels"))
indexNums <- which(names(dataelements$dataElements) %in% c("name","id", "code","shortName","aggregationType","valueType","domainType","formName"))
dataelements2 <- dataelements$dataElements[-indexNums]
write.table(dataelements2, file="dataElements.csv",row.names = F,na = "")


# post the data elements
r2 <- POST(paste0(dest.url,"api/metadata?importStrategy=CREATE_AND_UPDATE&atomicMode=NONE"),
          body = toJSON(list(dataElements = dataelements2), auto_unbox = TRUE),content_type_json())
assertthat::assert_that(r2$status_code == 200)

#==================================================================================

# Extract data elements groups
r <- GET(paste0(source.url,"api/metadata.json?assumeTRUE=TRUE&dataElementGroups=TRUE"),
         authenticate(source.user, source.pass))
de_groups <- fromJSON(content(r,"text"))

indexNums <- which(names(de_groups$dataElementGroups) %in% c("name","id","shortName","dataElements"))
de_groups2 <- de_groups$dataElementGroups[indexNums]

# post the data elements groups
r2 <- POST(paste0(dest.url,"api/metadata?importStrategy=CREATE&atomicMode=NONE"),
           body = toJSON(list(dataElementGroups = de_groups2), auto_unbox = TRUE),
           content_type_json())
assertthat::assert_that(r2$status_code==200)
#============================================================================

# ==== Indicators

#Extract indicatorType 
r <- GET(paste0(source.url,"api/metadata.json?assumeTRUE=TRUE&indicatorTypes=TRUE"),
                  authenticate(source.user, source.pass))
indType <- fromJSON(content(r,"text"))
indexNums <- which(names(indType$indicatorType) %in% c("userGroupAccesses", "user", "attributeValues","lastUpdatedBy","publicAccess","translations","userAccesses","lastUpdated"))
indexNums <- which(names(indType$indicatorTypes) %in% c("translations"))
indType2 <- indType$indicatorType[-indexNums]

#post the indicatorTypes
r2 <- POST(paste0(dest.url,"api/metadata?importStrategy=CREATE&atomicMode=NONE"),
           body = toJSON(list(indicatorTypes = indType2), auto_unbox = TRUE),
           content_type_json())
assertthat::assert_that(r2$status_code==200)

# Extract indicator
r <- GET(paste0(source.url,"api/metadata.json?assumeTRUE=TRUE&indicators=TRUE"),
         authenticate(source.user, source.pass))
ind <- fromJSON(content(r,"text"))
indexNums <- which(names(ind$indicators) %in% c("userGroupAccesses", "user", "attributeValues","lastUpdatedBy","publicAccess","translations","userAccesses","lastUpdated"))
ind2 <- ind$indicators[-indexNums]

# post the indicators
r2 <- POST(paste0(dest.url,"api/metadata?importStrategy=CREATE&atomicMode=NONE"),
           body = toJSON(list(indicators = ind2), auto_unbox = TRUE),
           content_type_json())
assertthat::assert_that(r2$status_code==200)

#===========
# create indicator groups
r <- GET(paste0(source.url,"api/metadata.json?assumeTRUE=TRUE&indicatorGroups=TRUE"),
         authenticate(source.user, source.pass))
indGroups <- fromJSON(content(r,"text"))
indexNums <- which(names(indGroups$indicatorGroups) %in% c("userGroupAccesses", "user", "attributeValues","lastUpdatedBy","publicAccess","translations","userAccesses","lastUpdated"))
indGroups2 <- indGroups$indicatorGroups[-indexNums]

# post the indicators
r2 <- POST(paste0(dest.url,"api/metadata?importStrategy=CREATE_AND_UPDATE&atomicMode=NONE"),
           body = toJSON(list(indicatorGroups = indGroups2), auto_unbox = TRUE),
           content_type_json())
assertthat::assert_that(r2$status_code==200)
#=============
# create indicator group sets
r <- GET(paste0(source.url,"api/metadata.json?assumeTRUE=TRUE&indicatorGroupSets=TRUE"),
         authenticate(source.user, source.pass))
indGroupSets <- fromJSON(content(r,"text"))
indexNums <- which(names(indGroupSets$indicatorGroupSets) %in% c("userGroupAccesses", "user", "attributeValues","lastUpdatedBy","publicAccess","translations","userAccesses","lastUpdated","indicatorGroupSet"))
indGroupSets2 <- indGroupSets$indicatorGroupSets[-indexNums]

# post the indicators
r2 <- POST(paste0(dest.url,"api/metadata?importStrategy=CREATE&atomicMode=NONE"),
           body = toJSON(list(indicatorGroupSets = indGroupSets2), auto_unbox = TRUE),
           content_type_json())
assertthat::assert_that(r2$status_code==200)

#=====================================================================

# Data sets

#========
# Extract categorycombo
r <- GET(paste0(source.url,"api/metadata.json?assumeTRUE=TRUE&categoryCombos=TRUE"),
         authenticate(source.user, source.pass))
cc <- fromJSON(content(r,"text"))
indexNums <- which(names(cc$categoryCombos) %in% c("userGroupAccesses", "user", "attributeValues","lastUpdatedBy","publicAccess","translations","userAccesses","lastUpdated"))
cc2 <- cc$categoryCombos[-indexNums]

# post the indicators
r2 <- POST(paste0(dest.url,"api/metadata?importStrategy=CREATE_AND_UPDATE&atomicMode=NONE"),
           body = toJSON(list(categoryCombos = cc2), auto_unbox = TRUE),
           content_type_json())
assertthat::assert_that(r2$status_code==200)

#=============
# Extract dataEntryForm
r <- GET(paste0(source.url,"api/metadata.json?assumeTRUE=TRUE&dataEntryForms=TRUE"),
         authenticate(source.user, source.pass))
def <- fromJSON(content(r,"text"))
indexNums <- which(names(def$dataEntryForms) %in% c("userGroupAccesses", "user", "attributeValues","lastUpdatedBy","publicAccess","translations","userAccesses","lastUpdated"))
def2 <- def$dataEntryForms[-indexNums]

# post the dataEntryForms
r2 <- POST(paste0(dest.url,"api/metadata?importStrategy=CREATE_AND_UPDATE&atomicMode=NONE"),
           body = toJSON(list(dataEntryForms = def2), auto_unbox = TRUE),
           content_type_json())
assertthat::assert_that(r2$status_code==200)

#============
# Extract datasets
r <- GET(paste0(source.url,"api/metadata.json?assumeTRUE=TRUE&dataSets=TRUE"),
         authenticate(source.user, source.pass))
ds <- fromJSON(content(r,"text"))
indexNums <- which(names(ds$dataSets) %in% c("userGroupAccesses", "user", "attributeValues","lastUpdatedBy","publicAccess","translations","userAccesses","lastUpdated","categoryCombo"))
ds2 <- ds$dataSets[-indexNums]

write(toJSON(list(dataSets=ds2), auto_unbox = TRUE), "datasets.json")
# post the dataEntryForms
r2 <- POST(paste0(dest.url,"api/metadata?importStrategy=CREATE_AND_UPDATE&atomicMode=NONE"),
           body = toJSON(list(dataSets = ds2), auto_unbox = TRUE),
           content_type_json())
assertthat::assert_that(r2$status_code==200)
















