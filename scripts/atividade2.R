################################################################
### Atividade 2 ###
################################################################

### Darwin Core

## carregar pacotes
library(tidyverse)
library(taxize)

## carregar dados

# dados prontos
iris <- read.csv("data/iris_mod.csv", header = T)

lapply(iris, unique)

# dados dos participantes
setwd("output/atividade1/")
files21 <- list.files(pattern = '\\.csv', recursive = T) # includes all subfolders
dados21 <- plyr::adply(files21, 1, read.csv, header = T)

# tags21 <- cbind(headers1[c(11),], 
#                 headers1[c(10), 3])
# names(tags21) <- c("X1", "x", "sensor", "serial")


# checar taxa
species <- iris %>% 
  distinct(Species) %>% 
  pull() %>% 
  get_tsn() %>% 
  data.frame() %>% 
  bind_cols(iris %>% 
              distinct(Species))


### Manipulando os dados

iris_1 <- iris %>% 
  dplyr::mutate(eventID = paste(site, date, sep = "_"), # create indexing fields 
                occurrenceID = paste(site, date, amostra, sep = "_")) %>% 
  left_join(species %>% 
              select(Species, uri)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = lon, # rename fields according to DwC 
                decimalLatitude = lat,
                eventDate = date,
                scientificName = Species,
                scientificNameID = uri) %>% 
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")


#
head(iris_1)


### criando o eventCore ###

eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct() 

head(eventCore)


### criando o occurrence ###
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificName, scientificNameID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 


head(occurrences)


## criando o measurementsOrFacts
eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, Sepal.Length:Petal.Width) %>%  
  pivot_longer(cols = Sepal.Length:Petal.Width,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))

head(eMOF)

### controle de qualidade

# checar eventID
setdiff(eventCore$eventID, occurrences$eventID)
setdiff(eventCore$eventID, eMOF$eventID)
setdiff(occurrences$eventID, eMOF$eventID)


# checar NAs
eMOF %>%
  filter(is.na(eventID))


occurrences %>%
  filter(is.na(eventID))


### salvar os dados

# limpar memória
rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

# ler arquivos
files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")

# loop para criar arquivos *.csv
for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}

