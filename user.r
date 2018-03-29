## Load multiple packages at one
packages <- c('jsonlite','plyr','dplyr')
lapply(packages, require, character.only = T)