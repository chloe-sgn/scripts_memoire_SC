Packages
library(dplyr)
library(tidyr)
library(writexl)

# DB
database <- read.csv(file = "/Users/chloespagnoli/Desktop/stage_osu/R/bd.csv", header = T, sep = ";", dec = ".")

# Tri des données : séléction des variables pour les calculs d'indicateurs
data <- database %>% 
  dplyr::select(id, commune, epci, degat, observation, discussion, recherche_information, nb_sources, classement_cotier, 
                cotier_interet, cotier_implication, cotier_impact, ppri, papi, plu, pcs, sgtc, gemapi, action, 
                confiance_risque_moi,confiance_risque_media, confiance_risque_scientifique, confiance_risque_elu, 
                confiance_risque_etat, confiance_risque_habitant, confiance_risque_association, 
                confiance_risque_religion, confiance_risque_municipalite, consolidation_ouvrage, demolition,
                construction_ouvrage, entretien_dune, depodlerisation, rechargement_sable, interdiction_construction,
                alerte, apprentissage_securite, com_partie_moi, com_implication, com_souvenir, logement_privilege, 
                logement_valeurs, logement_securite, logement_lien, mer_attachement, mer_veine, mer_liberte, mer_dangereuse,
                mer_pollution, mer_beaute, somme_activites, frequence_activites)

# Mise en forme : inversion des variables pour i301
var_a_inverser <- database %>% 
  dplyr::select(id, com_etranger, com_autre, com_malheureux, logement_attachement, 
                logement_quitter, mer_vivre, mer_importance) %>% 
  mutate(com_etranger = ifelse(com_etranger == 1, 5, 
                                  ifelse(com_etranger == 2, 4,
                                         ifelse(com_etranger == 4, 2,
                                                ifelse(com_etranger == 5, 1, 3)))),
         com_autre = ifelse(com_autre == 1, 5, 
                               ifelse(com_autre == 2, 4,
                                      ifelse(com_autre == 4, 2,
                                             ifelse(com_autre == 5, 1, 3)))),
         com_malheureux = ifelse(com_malheureux == 1, 5, 
                               ifelse(com_malheureux == 2, 4,
                                      ifelse(com_malheureux == 4, 2,
                                             ifelse(com_malheureux == 5, 1, 3)))),
         logement_attachement = ifelse(logement_attachement == 1, 5, 
                               ifelse(logement_attachement == 2, 4,
                                      ifelse(logement_attachement == 4, 2,
                                             ifelse(logement_attachement == 5, 1, 3)))),
         logement_quitter = ifelse(logement_quitter == 1, 5, 
                               ifelse(logement_quitter == 2, 4,
                                      ifelse(logement_quitter == 4, 2,
                                             ifelse(logement_quitter == 5, 1, 3)))),
         mer_vivre = ifelse(mer_vivre == 1, 5, 
                               ifelse(mer_vivre == 2, 4,
                                      ifelse(mer_vivre == 4, 2,
                                             ifelse(mer_vivre == 5, 1, 3)))),
         mer_importance = ifelse(mer_importance == 1, 5, 
                               ifelse(mer_importance == 2, 4,
                                      ifelse(mer_importance == 4, 2,
                                             ifelse(mer_importance == 5, 1, 3)))),
         )

# Jeu final prêt pour les calculs d'indicateurs
data <- left_join(data, var_a_inverser, by = "id")

# Calcul des valeurs des indicateurs à l'échelle communale 
calcul_index <- data %>% 
  group_by(commune) %>% 
  mutate(i101 = (sum(degat)*100)/n(),
         i102 = (sum(ifelse(observation == 1 | discussion == 1, 1, 0))*100)/n(),
         i103 = sum(nb_sources)/sum(recherche_information),
         i104 = sum(ifelse(classement_cotier == 1, 1, 0), na.rm = T)*100/n(),
         i105 = (mean(cotier_interet) + mean(cotier_implication) + mean(cotier_impact)) / 3,
         i106 = mean(
             ifelse(ppri == "Je ne sais pas", 0, 1) + 
               ifelse(papi == "Je ne sais pas", 0, 1) + 
               ifelse(plu == "Je ne sais pas", 0, 1) + 
               ifelse(pcs == "Je ne sais pas", 0, 1) + 
               ifelse(sgtc == "Je ne sais pas", 0, 1) + 
               ifelse(gemapi == "Je ne sais pas", 0, 1)),
         i107 = mean(action), 
         i201 = mean(confiance_risque_etat),
         i202 = (mean(confiance_risque_elu) + mean(confiance_risque_municipalite)) / 2,
         i203 = (mean(confiance_risque_media) + mean(confiance_risque_scientifique) + mean(confiance_risque_habitant) +  
                     mean(confiance_risque_association)) /4,
         i204 = (mean(confiance_risque_moi) + mean(confiance_risque_religion)) / 2,
         i205 = (mean(construction_ouvrage) + mean(consolidation_ouvrage) + mean(depodlerisation) + mean(entretien_dune) + 
                   mean(rechargement_sable)) /5, 
         i206 = (mean(demolition) + mean(interdiction_construction)) / 2,
         i207 = (mean(alerte) + mean(apprentissage_securite)) / 2,
         i301 = (mean(com_partie_moi) + mean(com_etranger) + mean(com_malheureux) + mean(com_autre) + mean(com_implication)
                 + mean(com_souvenir) + mean(logement_privilege) + mean(logement_valeurs) + mean(logement_securite) + 
                   mean(logement_attachement) + mean(logement_lien) + mean(logement_quitter) + mean(mer_attachement) + 
                   mean(mer_vivre) + mean(mer_veine) + mean(mer_importance) + mean(mer_liberte) + mean(mer_dangereuse) +
                   mean(mer_pollution) + mean(mer_beaute)) / 20, 
         i302 = mean(case_when(
           frequence_activites == "Semaine" ~ somme_activites * 52,
           frequence_activites == "Mois" ~ somme_activites * 12,
           frequence_activites == "Annee" ~ somme_activites * 1,
           TRUE ~ 0), na.rm = TRUE)
              ) %>% 
  dplyr::select(id, commune, i101, i102, i103, i104, i105, i106, i107, i201, i202, i203, i204,
                i205, i206, i207, i301, i302)


### Sortie fichier excel ### 
# Liste des noms d’indicateurs
index <- grep("^i\\d+", names(calcul_index), value = TRUE)

# Calcul des cotations
# Function to assign a label based on thresholds
apply_thresholds <- function(x, thresholds, labels, default = NA_real_) {
  case_when(
    x < thresholds[1] ~ labels[1],
    x >= thresholds[1] & x < thresholds[2] ~ labels[2],
    x >= thresholds[2] & x < thresholds[3] ~ labels[3],
    x >= thresholds[3] & x < thresholds[4] ~ labels[4],
    x >= thresholds[4] ~ labels[5],
    TRUE ~ default
  )
}


calcul_index <- calcul_index %>%
  mutate(
    r101 = apply_thresholds(i101, c(10, 16, 22, 30), 1:5),
    r102 = apply_thresholds(i102, c(0, 50, 70, 90), 1:5),
    r103 = apply_thresholds(i103, c(0, 1.5, 2.5, 3.5), 1:5),
    r104 = apply_thresholds(i104, c(10, 20, 30, 40), 1:5),
    r105 = apply_thresholds(i105, c(1, 2, 3, 4), 1:5),
    r106 = apply_thresholds(i106, c(0, 1.5, 2.5, 3.5), 1:5),
    r107 = apply_thresholds(i107, c(0, 0.05, 0.1, 0.2), 1:5),
    r201 = apply_thresholds(i201, c(1, 2, 3, 4), 1:5),
    r202 = apply_thresholds(i202, c(1, 2, 3, 4), 1:5),
    r203 = apply_thresholds(i203, c(1, 2, 3, 4), 1:5),
    r204 = case_when(
      is.na(i204) ~ NA_real_, 
      i204 >= 4.5 ~ 5,
      i204 >= 3.5 ~ 4,
      i204 >= 2.5 ~ 3,
      i204 >= 1.5 ~ 2,
      i204 >= 0.5 ~ 1,
      TRUE ~ 5
    ),
    r205 = case_when(
      is.na(i205) ~ NA_real_, 
      i205 >= 4.5 ~ 5,
      i205 >= 3.5 ~ 4,
      i205 >= 2.5 ~ 3,
      i205 >= 1.5 ~ 2,
      i205 >= 0.5 ~ 1,
      TRUE ~ 5
    ),
    r206 = case_when(
      is.na(i206) ~ NA_real_, 
      i206 >= 4.5 ~ 5,
      i206 >= 3.5 ~ 4,
      i206 >= 2.5 ~ 3,
      i206 >= 1.5 ~ 2,
      i206 >= 0.5 ~ 1,
      TRUE ~ 5
    ),
    r207 = case_when(
      is.na(i207) ~ NA_real_, 
      i207 >= 4.5 ~ 5,
      i207 >= 3.5 ~ 4,
      i207 >= 2.5 ~ 3,
      i207 >= 1.5 ~ 2,
      i207 >= 0.5 ~ 1,
      TRUE ~ 5
    ),
    r301 = apply_thresholds(i301, c(3, 3.2, 3.3, 3.4), 1:5),
    r302 = apply_thresholds(i302, c(0, 5, 10, 15), 1:5)
  )

# Créer une liste nommée avec un onglet par indicateur
liste_onglets <- lapply(index, function(indic) {
  r_col <- sub("^i", "r", indic)  # r101 from i101, etc.
  calcul_index[, c("commune", indic, r_col)]
})
names(liste_onglets) <- index

# Écriture dans un fichier Excel
write_xlsx(liste_onglets, path = "/Users/chloespagnoli/Desktop/stage_osu/R/indicateurs.xlsx")
