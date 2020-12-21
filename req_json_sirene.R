library(tidyverse)
library(sf)
library(jsonlite)
library(tidyjson)
library(lubridate)

#-----------------------------IMPORT DES DONNEES DE BASE--------------------------------------------------------------------
dessinstockunitelegale <- read_csv("dessinstockunitelegale.csv", 
                                   col_types = cols(Longueur = col_skip(), 
                                                    Type = col_skip(), Ordre = col_skip()))
naf_groupe_insee <- read_delim("naf_groupe_insee.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)
groupes <- naf_groupe_insee %>% filter(!is.na(groupe_insee_commerces))
tefet <- read_delim("tefet.csv", ";", escape_double = FALSE, 
                    locale = locale(decimal_mark = ","), 
                    trim_ws = TRUE)

# construction de l'url--------------------------------------------------------------------------------------------------
url1 <- "https://entreprise.data.gouv.fr/api/sirene/v1/full_text/*?"
req_activite <- "activite_principale="
code_activite <- "1013B"
departement <- "&departement="
code_departement <- 59
page <- "&page="
n_page <- 1
fin_url <- "&per_page=100"
################################### INUTILE MAIS AU CAS OU POUR FAIRE AVEC APPROX###################################
#url pour version par famille de naf
req_approx_activite <- "approximate_activity="
code_approx_activite <- 47
url_approx <- paste0(url1,req_approx_activite,code_approx_activite,departement,code_departement,page,n_page,fin_url)


#initialisation-------------------------------------------------------------------------------------------------------
n_page <- 1 #premiere page
code_activite <- "1013B" #premiere activité
url_init <- paste0(url1,req_activite,code_activite,departement,code_departement,page,n_page,fin_url) #url premier appel
#import json 1013B, page 1 pour initialiser et créer le fichier
json_df <- fromJSON(url_init,simplifyDataFrame = TRUE) #import et transformation en data.frame
etablissements <- json_df[["etablissement"]] # transformer le json en data.frame
etablissements <- etablissements %>% mutate(page_i=1,activite_j=1) #ajout colonnes pour test les pages et activités
#import des autres pages dans le data.frame---------------------------------------------------------------------------
for (j in groupes$naf) { #applique la requete à l'ensemble des codes naf du fichier groupe_insee
  url2 <- paste0(url1,req_activite,j,departement,code_departement,page,n_page,fin_url) #url de l'activite, page 1
  json_df2 <- fromJSON(url2,simplifyDataFrame = TRUE) #import du json de l'activité de la boucle
  total_page <- json_df2$total_pages # recup total pages de l'activite de la boucle
  for (i in 1:total_page) { #applique la requete sur l'ensemble des pages de l'activité
    url3 <- paste0(url1,req_activite,j,departement,code_departement,page,i,fin_url) #url de l'activité à la bonne page
    json_df3 <- fromJSON(url3,simplifyDataFrame = TRUE) #import  des données de la page i de l'activité j
    etablissements3 <- json_df3[["etablissement"]] # transformation du json en data.frame
    etablissements3 <- etablissements3 %>% mutate(page_i=i,activite_j=i) #ajout colonnes pour test les pages et activités
    etablissements <- bind_rows(etablissements,etablissements3) #ajout des valeurs de la boucle dans le data.frame initiale
  }
}


#FINITION DE LA PREPARATION DE DONNEES -----------------------------------------------------------------------------

#supression des doublons liés à l'intialisation
etablissements <- etablissements %>% filter(!duplicated(etablissements))
#selection des colonnes intéressantes et création de insee_com
etablissements_light <- etablissements %>% select(siret,
                                                  date_creation_entreprise,
                                                  commune,
                                                  libelle_commune,
                                                  enseigne,
                                                  l1_normalisee,
                                                  libelle_activite_principale,
                                                  activite_principale,
                                                  tranche_effectif_salarie,
                                                  tranche_effectif_salarie_centaine_pret,
                                                  numero_voie,
                                                  indice_repetition,
                                                  type_voie,
                                                  libelle_voie,
                                                  longitude,
                                                  latitude
                                                  ) %>% mutate(insee_comm = paste0(59,commune)) #création de insee_comm

#ajout des familles insee et estimation du nombre de salariés
data <- left_join(etablissements_light,groupes, by=c("activite_principale"="naf")) %>% left_join(tefet,by=c("tranche_effectif_salarie"="tefet"))



# GEOMATIQUE --------------------------------------------------------------------------------------------------
#geolocalisation avec les longitude et latitude
data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, na.fail=FALSE)
#extraction du shp
st_write(data_sf,paste0("L_commerces_api_sirene_",today(),".shp"), layer = "commerces dans le Nord",layer_options = "OVERWRITE=true")