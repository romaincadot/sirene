#analyse des données des commerces ---------------------------------------------

# Pour tester avec quelques cartos directement dans R ---------------------------------------------------------
library(tmap)
tmap_mode("view")
qtm(data_sf,symbols.col = "groupe_insee_commerces") + tm_minimap()

# analyse des données ----------------------------------------------------------------------------------------
library(ggplot2)
# effectifs par type de commerce et par grands groupes
ggplot(data) +
  aes(x = libelle_activite_principale, y = med_tefet, fill = groupe_insee_commerces) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(groupe_insee_commerces), scales = "free")
#nb d'enseigne de super et hyper
sup_hyp <- data %>% 
  filter(libelle_activite_principale=="Hypermarchés"|libelle_activite_principale=="Supermarchés") %>%
  group_by(enseigne) %>% 
  summarise(nb=n(),sum(med_tefet,na.rm = TRUE)) #creation d'un dataset contenant la somme des enseigne et des emplois

sup_hyp %>% 
  arrange(desc(nb))  %>% 
  ggplot() +
  aes(y = enseigne, fill = enseigne, weight = nb) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal() +
  theme(legend.position = "none")
