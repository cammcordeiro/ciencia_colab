### Atividade 3 ###

### Acesso a bancos de dados abertos

## Exemplo: *Finding Dori*

# carregar pacotes
library(rgbif)
library(tidyverse)

# checar funcoes
?occ_data

# baixar ocorrencias
dori_gbif <- occ_data(scientificName = "Paracanthurus hepatus", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# checar dimensoes
dim(dori_gbif)
dim(dori_gbif$data)

# checar campos
dori_gbif$data %>% names

# ver problemas reportados (issues)
gbif_issues() 

dori_gbif$data$issues

#
issues_gbif <- dori_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

#
gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)

#
dori_gbif1 <- dori_gbif$data %>%
  select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude, 
         issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
         datasetName, recordedBy, depth, locality, habitat) 


# ocorrencias distintas
dori_gbif1 <- dori_gbif1 %>% 
  distinct() 

# checar niveis dos fatores
lapply(dori_gbif1, unique)



## Problemas não reportados

# investigar niveis suspeitos
dori_gbif1 %>% 
  distinct(waterBody) %>% 
  pull()

# waterBody
dori_gbif1 %>%
  group_by(waterBody) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=waterBody)) +
  geom_bar(stat = 'identity') 


# fonte das regioes erradas
dori_gbif1 %>% 
  filter(waterBody %in% c("Atlantic Ocean", "Carribean", "Royal Caribbean", "Carribean Sea", "Bonaire")) %>% 
  distinct(datasetName)



# 27 ocorrencias
dori_gbif1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science"))


# filtrar todas do dataset suspeito
dori_gbif_ok <- dori_gbif1 %>% 
  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))


# checar pontos
dori_gbif_ok %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(~decimalLongitude, ~decimalLatitude, 
             popup = ~as.character(datasetName), 
             label = ~as.character(scientificName),
             clusterOptions = markerClusterOptions())


# checar profundidade
dori_gbif_ok %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 


#######################################################
#######################################################

### conferir ocorrencias no OBIS

# carregar pacotes
library(robis)


# baixar ocorrencias
dori_obis <- robis::occurrence("Paracanthurus hepatus")


# checar dados
names(dori_obis)

dori_obis1 <- dori_obis %>% 
  select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
         flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
         datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()


# check problemas reportados (flags)
dori_obis1 %>% 
  distinct(flags)


# check NA em datasetName
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         is.na(datasetName)) %>% 
  distinct(waterBody)


# depth
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 



# checar niveis
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  lapply(., unique)


# ok
dori_obis_ok <- dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique")) 



# checar ocorrencias
dori_obis_ok %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(~decimalLongitude, ~decimalLatitude, 
             popup = ~as.character(datasetName), 
             label = ~as.character(recordedBy),
             clusterOptions = markerClusterOptions())



#################################
#################################

## EXTRA ##

# unir GBIF e OBIS

rm(list = setdiff(ls(), c("dori_obis_ok", "dori_gbif_ok")))

# ver diferencas
setdiff(names(dori_gbif_ok), names(dori_obis_ok))
setdiff(names(dori_obis_ok), names(dori_gbif_ok))

all_data <- bind_rows(dori_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      dori_obis_ok%>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() 

# map the occurrence data:
maps::map("world", main = "Paracanthurus hepatus")#, xlim = range(all_data$decimalLongitude), ylim = range(all_data$decimalLatitude))
points(all_data[ , c("decimalLongitude", "decimalLatitude")], pch = ".", cex = 4, col = "red")
