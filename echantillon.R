# télécharger les packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(gridExtra)
library(dplyr)
library(colourpicker)
library(ggpattern)
library(colorspace)

df <- read.csv("echantillon_enquete.csv", , sep=";")

# -----------------------
# Colonnes par bloc
# -----------------------
cols_age <- c("pop.18_24", "X18_24",
              "pop.25_39", "X25_39",
              "pop.40_54", "X40_54",
              "pop.55_64", "X55_64",
              "pop.65_79", "X65_79",
              "pop.80", "X80")

cols_sexe <- c("hommes", "h", "femmes", "f")

cols_csp <- c("agriculteur", "agr", "commercants", "co", 
              "cadres", "ca", "prof_intermediaire", "pi", 
              "employes", "emp", "ouvriers", "ouv", 
              "retraites", "retr", "autres", "autre_vrai")

# -----------------------
# Mise au format long
# -----------------------

## ÂGE
ages_long <- df %>%
  select(commune, all_of(cols_age)) %>%
  pivot_longer(
    cols = -commune,
    names_to = "categorie",
    values_to = "valeur"
  ) %>%
  mutate(
    tranche_age = str_extract(categorie, "\\d+_?\\d*|80"),  
    type = ifelse(str_detect(categorie, "pop"), "Population", "Échantillon")
  )

## SEXE
sexe_long <- df %>%
  select(commune, all_of(cols_sexe)) %>%
  pivot_longer(
    cols = -commune,
    names_to = "categorie",
    values_to = "valeur"
  ) %>%
  mutate(
    sexe = case_when(
      str_detect(categorie, "hommes|h") ~ "Hommes",
      str_detect(categorie, "femmes|f") ~ "Femmes"
    ),
    type = ifelse(categorie %in% c("hommes", "femmes"), "Population", "Échantillon")
  )

## CSP
csp_long <- df %>%
  select(commune, all_of(cols_csp)) %>%
  pivot_longer(
    cols = -commune,
    names_to = "categorie",
    values_to = "valeur"
  ) %>%
  mutate(
    csp = case_when(
      str_detect(categorie, "agr") ~ "Agriculteurs",
      str_detect(categorie, "co") ~ "Commerçants",
      str_detect(categorie, "ca") ~ "Cadres",
      str_detect(categorie, "prof") ~ "Prof. intermédiaires",
      str_detect(categorie, "pi") ~ "Prof. intermédiaires",
      str_detect(categorie, "emp") ~ "Employés",
      str_detect(categorie, "ouv") ~ "Ouvriers",
      str_detect(categorie, "retr") ~ "Retraités",
      str_detect(categorie, "autre") ~ "Autres"
    ),
    type = ifelse(
      categorie %in% c("agriculteur","commercants","cadres",
                       "prof_intermediaire","employes","ouvriers",
                       "retraites","autres"),
      "Population", "Échantillon"
    )
  )

# -----------------------
# GRAPHIQUES
# -----------------------

# 1) Âge 
base_colors_ages <- c(
  "18_24" = "#7FFFD4",
  "25_39" = "#96CDCD",
  "40_54" = "#CDB5CD",
  "55_64" = "#CD96CD",
  "65_79" = "#B23AEE",
  "80" = "#68228B"
)
# Version foncée pour les hachures
dark_colors_ages <- darken(base_colors_ages, amount = 0.4)
ages_plot <- ggplot(
  ages_long,   # ton dataframe long pour age
  aes(
    x = tranche_age,            # Tranches d'age
    y = valeur,          # pourcentage
    fill = tranche_age,         # couleur de fond
    pattern = type,      # plein ou hachuré selon Population/Échantillon
    pattern_color = tranche_age # hachures couleur plus foncée
  )
) +
  geom_col_pattern(
    position = position_dodge(width = 0.9),
    color = NA,                 # pas de contour
    pattern_density = 0.4,      # densité des hachures
    pattern_spacing = 0.02,     # espacement
    pattern_angle = 45,         # angle des hachures
    pattern_fill = NA           # pas de remplissage dans les hachures
  ) +
  facet_wrap(~ commune, ncol = 3) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = base_colors_ages) +
  scale_pattern_color_manual(values = dark_colors_ages) +
  scale_pattern_manual(values = c(
    "Population"  = "none",    # plein
    "Échantillon" = "stripe"   # hachuré
  )) +
  labs(
    x = "",
    y = "Pourcentage (%)",
    title = "Répartition par tranches d'âges : Population vs Échantillon"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_blank(),  # enlève les intitulés
    axis.ticks.x = element_blank(), # enlève les ticks
    legend.position = "bottom"
  )
print(ages_plot)


# 2) Sexe
# Palette sexe
base_colors_sexe <- c(
  "Hommes" = "#A4D3EE",
  "Femmes" = "#FFB6C1"
)
# Version foncée pour les hachures
dark_colors_sexe <- darken(base_colors_sexe, amount = 0.4)
sexe_plot <- ggplot(
  sexe_long,   # ton dataframe long pour sexe
  aes(
    x = sexe,            # Homme/Femme
    y = valeur,          # pourcentage
    fill = sexe,         # couleur de fond
    pattern = type,      # plein ou hachuré selon Population/Échantillon
    pattern_color = sexe # hachures couleur plus foncée
  )
) +
  geom_col_pattern(
    position = position_dodge(width = 0.9),
    color = NA,                 # pas de contour
    pattern_density = 0.4,      # densité des hachures
    pattern_spacing = 0.02,     # espacement
    pattern_angle = 45,         # angle des hachures
    pattern_fill = NA           # pas de remplissage dans les hachures
  ) +
  facet_wrap(~ commune, ncol = 3) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = base_colors_sexe) +
  scale_pattern_color_manual(values = dark_colors_sexe) +
  scale_pattern_manual(values = c(
    "Population"  = "none",    # plein
    "Échantillon" = "stripe"   # hachuré
  )) +
  labs(
    x = "",
    y = "Pourcentage (%)",
    title = "Répartition par genre : Population vs Échantillon"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_blank(),  # enlève les intitulés
    axis.ticks.x = element_blank(), # enlève les ticks
    legend.position = "bottom"
  )
print(sexe_plot)


# 3) CSP
# On créé une palette de couleurs foncées pour les hachures
base_colors <- c(
  "Agriculteurs"         = "#7CCD7C",
  "Prof. intermédiaires" = "#48D1CC",
  "Employés"             = "#6CA6CD",
  "Ouvriers"             = "#EEDC82",
  "Retraités"            = "#EE5C42",
  "Autres"               = "#FFA54F",
  "Cadres"               = "#DDA0DD",
  "Commerçants"          = "#FFC0CB"
)
# On créé une version plus foncée pour les hachures
dark_colors <- darken(base_colors, amount = 0.4)
csp_plot <- ggplot(
  csp_long,
  aes(
    x = csp,
    y = valeur,
    fill = csp,            # couleur de fond par CSP
    pattern = type,        # motif selon population/échantillon
    pattern_color = csp    # hachures dans une autre couleur
  )
) +
  geom_col_pattern(
    position = position_dodge(width = 0.9),
    color = NA,                 # pas de contour
    pattern_density = 0.4,      # densité des hachures
    pattern_spacing = 0.02,     # espacement
    pattern_angle = 45,         # angle des hachures
    pattern_fill = NA           # pas de remplissage dans les hachures
  ) +
  facet_wrap(~ commune, ncol = 3) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = base_colors) +
  scale_pattern_color_manual(values = dark_colors) +  # les hachures sont plus foncées
  scale_pattern_manual(values = c(
    "Population"  = "none",    # plein
    "Échantillon" = "stripe"   # hachuré
  )) +
  labs(
    x = "",
    y = "Pourcentage (%)",
    title = "CSP : Population vs Échantillon"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_blank(),  # enlève les intitulés
    axis.ticks.x = element_blank(), # enlève les ticks
    legend.position = "bottom"      # si tu veux centrer la légende en bas
  )
print(csp_plot)
