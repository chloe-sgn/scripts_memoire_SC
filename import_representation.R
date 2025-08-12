# installer et activer les packages si ce n'est pas fait. 
library(readxl)
library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(sf)
library(readr)

# Paramètres
# Définir le dossier principal ici || VARIABLE A MODIFIER
base_dir <- "/Users/chloespagnoli/Desktop/stage_osu/R/INDICATEUR_REPR"

# Chemins vers les fichiers
input_representation <- file.path(base_dir, "DATA", "indicateurs_.xlsx")
temp_csv <- file.path(base_dir, "TEMP", "df_intermediaire.csv")

import_csv <- file.path(base_dir, "SORTIE", "df_import.csv")
output_csv <- file.path(base_dir, "SORTIE", "indicateurs_representation_commune.csv")
output_shp <- file.path(base_dir, "SORTIE", "indicateur_representation_commune.shp")

shp_file <- file.path(base_dir, "DATA", "communes", "COMMUNE.shp")
input_car <- file.path(base_dir, "DATA", "car_50.csv") 

# Feuilles à traiter
sheets_to_process <- c("i101", "i102", "i103", "i104", "i105", "i106", "i107", "i201", "i202", "i203", "i204","i205", "i206", "i207", "i301", "i302")

auteur <- "OSUR"
date_data <- "2025-07-23"
unite_data <- "integer"
source_data <- "OSUR_2025"

# Liste pour stocker les DataFrames intermédiaires
df_list <- list()

# Lecture du CSV car_50
car_data <- read.csv(input_car)

# Boucle pour traiter chaque feuille spécifiée
for (sheet_name in sheets_to_process) {
  print(paste("Traitement de la feuille :", sheet_name))  # Indiquer quelle feuille est en cours
  
  # Étape 1 : Lire la feuille Excel
  df <- read_xlsx(input_representation, sheet = sheet_name)
  
  # Étape 2 : Renommer les colonnes pour s'assurer qu'il n'y a pas de doublons
  colnames(df) <- make.unique(colnames(df))
  
  # Étape 3 : Sélectionner les colonnes par index en fonction de la feuille
  df_selected <- df %>% dplyr::select(1, 3)  

  # Étape 4 : Filtrer les lignes pour ne garder que celles où les valeurs ne sont pas NA
  df_filtered <- df_selected %>%
    distinct()  
  
  # Étape 5 : Renommer les colonnes pour faciliter le traitement
  df_filtered <- df_filtered %>%
    rename(COMMUNE = 1, data_data = 2)
  
  # Étape 6 : Ajout de la colonne id_meta selon le nom de la feuille
  df_filtered <- df_filtered %>%
    mutate(id_meta = gsub("^i", "R", sheet_name))  # Ajouter la colonne id_meta avec le suffixe correspondant
  
  # Stocker le DataFrame dans la liste
  df_list[[sheet_name]] <- df_filtered
  
  # Créer une variable distincte pour chaque DataFrame pour un contrôle plus facile
  assign(paste0("df_", sheet_name), df_filtered)
}

# Combiner tous les DataFrames en un seul en les "mettant bout à bout"
df_intermediaire_import_quentin <- bind_rows(df_list)

# Exporter les résultats intermédiaires
write.csv(df_intermediaire_import_quentin, temp_csv, row.names = FALSE)

# Jointure avec le CSV car_50
df_resultats_jointure <- df_intermediaire_import_quentin %>%
  left_join(car_data, by = c("COMMUNE" = "nom_com"))

#Existe il des valeurs n/a dans cette jointure ? (ajout Makan 300125)
df_na_jointure <- df_resultats_jointure %>% filter(is.na(id_car))


# Trouver et afficher les doublons
all_duplicated_indices <- duplicated(df_resultats_jointure[, c("id_meta", "id_car")]) | 
  duplicated(df_resultats_jointure[, c("id_meta", "id_car")], fromLast = TRUE)

all_duplicated_rows <- df_resultats_jointure[all_duplicated_indices, ]
print(all_duplicated_rows)

# Ajout des autres colonnes et sélection des données finales
df_resultats_makan <- df_resultats_jointure %>%
  mutate(unite_data = unite_data,       # Ajouter la colonne 'unite_data'
         date_data = date_data,         # Ajouter la colonne 'date_data'
         source_data = source_data,     # Ajouter la colonne 'source_data'
         auteur_data = auteur) %>%      # Ajouter la colonne 'auteur_data'
  dplyr::select(data_data, unite_data, date_data, source_data, auteur_data, id_meta, id_car)  # Sélectionner et réorganiser les colonnes

# Exporter les résultats finaux
write.csv(df_resultats_makan, import_csv, row.names = FALSE)


#  ======  PARTIE SHAPEFILE =======
# ========== TRANSFORMATION POUR VALIDATION ==========
transform_csv <- function(input_file, output_file) {
  df <- read_csv(input_file, col_types = cols(.default = "c"), col_names = TRUE)
  
  df_wide <- df %>%
    pivot_wider(names_from = id_meta, values_from = data_data) %>%
    arrange(COMMUNE)
  
  df_wide[-1] <- lapply(df_wide[-1], function(col) {
    suppressWarnings(num_col <- as.numeric(as.character(col)))
    ifelse(is.na(num_col) & !is.na(col), NA, num_col)
  })
  
  write_csv(df_wide, output_file, na = "")
  message("Fichier transformé généré : ", output_file)
}

transform_csv(temp_csv, output_csv)

# ========== JOINTURE AVEC SHAPEFILE DES COMMUNES IGN ==========
commune_shp <- st_read(shp_file)
df_transformed <- read_csv(output_csv, col_types = cols(.default = "c"))

commune_joined <- commune_shp %>%
  left_join(df_transformed, by = c("NOM" = "COMMUNE"))

# Sauvegarde shapefile
st_write(commune_joined, output_shp, driver = "ESRI Shapefile", delete_layer = TRUE)

message("Fichier shapefile généré : ", output_shp)
