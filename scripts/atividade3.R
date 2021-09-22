### Atividade 3 ###

### Acesso a bancos de dados abertos

## Exemplo: *Finding Dori*

# carregar pacotes

library(tidyverse)


#####################################################
### extrair dados do GBIF
library(rgbif)

# checar funcoes do pacote
?occ_data

# baixar ocorrencias
dori_gbif <- occ_data(scientificName = "Paracanthurus hepatus", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue = FALSE)

# dimensoes
dim(dori_gbif)

dim(dori_gbif$data)

# checar campos
dori_gbif$data %>% names


## Problemas reportados

# checar issues
gbif_issues() 


# checar problemas reportados
issues_gbif <- dori_gbif$data$issues %>% 
  # unique() %>% 
  strsplit(., "[,]") %>% 
  unlist() %>% 
  unique()


# ver quais os problemas encontrados no dataset baixado
gbif_issues() %>%
  data.frame() %>% 
  filter(code %in% issues_gbif)


# selecionar campos de interesse
dori_gbif1 <- dori_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) 

# ocorrencias únicas
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


# 25 ocorrencias
dori_gbif1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science"))

# filtrar todas do dataset suspeito
dori_gbif_ok <- dori_gbif1 %>% 
  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))

  
## PLotar ocorrencias

library(ggmap)
library(maps)
library(mapdata)

world <- map_data('world')

# checar pontos
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_gbif1, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))


# checar profundidade
dori_gbif_ok %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 



#####################################################
### extrair dados do OBIS
library(robis)

# baixar ocorrências
dori_obis <- robis::occurrence("Paracanthurus hepatus")


# checar dados
names(dori_obis)

dori_obis1 <- dori_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
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


# depth ok
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


# 
dori_obis_ok <- dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique", NA)) 


# plotar mapa
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))



#####################################################
##### EXTRA: Classificação automática de pontos #####

## função 'caseira'

source('functions/aux_functions.R')

# classificar ocorrências
marcados <- dori_gbif$data %>% 
  data.frame() %>% 
  dplyr::select(scientificName, decimalLongitude, decimalLatitude, datasetName) %>% 
  distinct() %>% 
  flag_outlier(., "Paracanthurus hepatus (Linnaeus, 1766)")

# mapa
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados, 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = flag)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Paracanthurus hepatus")))


##### Pacotes #####

## exemplo 1 (scrubr)

library(scrubr)

# usando os dados com flag
data_scrubr <- marcados %>% 
  dframe() %>% 
  coord_impossible() %>% 
  coord_incomplete() %>% 
  coord_unlikely() %>% 
  dedup()


# mapa
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = data_scrubr, 
             aes(x = decimalLongitude, y = decimalLatitude), 
             color = "red") +
  geom_point(data = marcados %>% 
               filter(flag != "OK"), 
             aes(x = decimalLongitude, y = decimalLatitude), 
             color = "blue", shape = 3) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Paracanthurus hepatus")))


## exemplo 2 (obistools)

library(obistools)

dori_obis %>% 
  dplyr::select(decimalLongitude, decimalLatitude, scientificNameID) %>% 
  distinct() %>% 
  check_outliers_species(., report=TRUE)


# usando essa configuração chegamos a valores próximos aos da limpeza manual
dori_obis %>% 
  dplyr::select(decimalLongitude, decimalLatitude, scientificNameID) %>% 
  distinct() %>% 
  check_outliers_dataset(., report = FALSE, iqr_coef = 1, mad_coef = 5) %>% 
  dim()


# mapa
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados %>% 
               filter(flag != "OK"), 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = datasetName)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Paracanthurus hepatus")))


## exemplo 3 (CoordinateCleaner)

library(CoordinateCleaner)

flags <-
  clean_coordinates(
    x = marcados,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    tests = c("equal", "gbif",
              "zeros", "seas")
  )



#####################################################
##### unir GBIF e OBIS #####

rm(list = setdiff(ls(), c("dori_obis_ok", "dori_gbif_ok")))

# ver diferencas
setdiff(names(dori_gbif_ok), names(dori_obis_ok))
setdiff(names(dori_obis_ok), names(dori_gbif_ok))

all_data <- bind_rows(dori_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      dori_obis_ok %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Paracanthurus hepatus") %>% 
  dplyr::select(-rn)

#
world <- map_data('world')

# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))


write.csv(all_data, "data/occ_GBIF-OBIS_par_hepa.csv", row.names = FALSE)



  