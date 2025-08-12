#Les packages
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(gridExtra)
library(dplyr)
library(tidyverse)

data<-read.csv("boxplot.csv",header = TRUE,sep=";", dec=",", stringsAsFactors = TRUE)
head(data)

#Choix manuel des couleurs par communes 
color_commune <- c("Saint-Denis" = "#00BBDB", "Sainte-Marie" = "#00A5FF", "Sainte-Suzanne" ="#9590FF", "Saint-Andre" = "#E7861B",
                   "Saint-Benoit" ="#F8766D", "Saint-Pierre" = "#00BC59" ,"Saint-Paul" = "#FF689F" )

data$r105 <- as.numeric(data$r105)
data$r201 <- as.numeric(data$r201)
data$r202 <- as.numeric(data$r202)
data$r203 <- as.numeric(data$r203)
data$r204 <- as.numeric(data$r204)
data$r205 <- as.numeric(data$r205)
data$r206 <- as.numeric(data$r206)
data$r207 <- as.numeric(data$r207)
data$r301 <- as.numeric(data$r301)

#r101
plot_r101 <- ggplot(data, aes(x = factor(commune), y = r101, fill = commune, alpha = .8)) +
  stat_summary(
    fun = ~ mean(., na.rm = TRUE) * 100,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.7
  ) +
  labs(x = "Commune", y = "Pourcentage de personnes ayant subi des dégâts (r101)") +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_r101)

#r102
plot_r102 <- ggplot(data, aes(x = factor(commune), y = r102, fill = commune, alpha = .8)) +
  stat_summary(
    fun = ~ mean(., na.rm = TRUE) * 100,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.7
  ) +
  labs(x = "Commune", y = "Pourcentage de personnes exposées indirectement (r102)") +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_r102)

#r103
# Calcul du nombre moyen de sources pour ceux qui ont fait des recherches
data_r103 <- data %>%
  group_by(commune) %>%
  summarise(
    nb_sources = sum(r103, na.rm = TRUE),
    nb_chercheurs = sum(r103 > 0, na.rm = TRUE),
    moy_sources_par_chercheur = ifelse(nb_chercheurs > 0, nb_sources / nb_chercheurs, NA)
  )
# Vérification résultats 
print(data_r103)
# plot
plot_r103 <- ggplot(data_r103, aes(x = factor(commune), y = moy_sources_par_chercheur, fill = commune, alpha = .8)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Commune", y = "Nombre moyen de sources utilisées par chercheur (r103)") +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_r103)

#r104
plot_r104 <- ggplot(data, aes(x = factor(commune), y = r104, fill = commune, alpha = .8)) +
  stat_summary(
    fun = ~ mean(., na.rm = TRUE) * 100,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.7
  ) +
  labs(x = "Commune", y = "Pourcentage de personnes qui perçoivent le risque côtier 
       comme étant la problématique la plus dangereuse (r104)") +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_r104)

#r105
plot_r105 <- ggplot(data, aes(x = factor(commune), y = r105, fill = commune, alpha = .8)) +
  labs(x = "commune", y = " Perception de l'exposition au risque (r105)") +
  geom_boxplot(width = .4) +
  geom_text(
    aes(label = ifelse(r105 > 15, as.character(id), "")),
    hjust = 1.2, vjust = 0.5, size = 3
  ) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_r105)

#r106
plot_r106 <- ggplot(data, aes(x = factor(commune), y = r106, fill = commune, alpha = .8)) +
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.7
  ) +
  labs(x = "Commune", y = "Nombre moyen de programmes connus (r106)") +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_r106)

#r201
plot_r201 <- ggplot(data, aes(x = factor(commune), y = r201, fill = commune, alpha = .8)) +
  labs(x = "commune", y = " Confiance institutions nationales (r201)") +
  geom_boxplot(width = .4) +
  geom_text(
    aes(label = ifelse(r201 > 15, as.character(id), "")),
    hjust = 1.2, vjust = 0.5, size = 3
  ) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#On affiche le graphique seul
print(plot_r201)

#r202
plot_r202 <- ggplot(data, aes(x = factor(commune), y = r202, fill = commune, alpha = .8)) +
  labs(x = "commune", y = " Confiance institutions locales (r202)") +
  geom_boxplot(width = .4) +
  geom_text(
    aes(label = ifelse(r202 > 15, as.character(id), "")),
    hjust = 1.2, vjust = 0.5, size = 3
  ) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_r202)

#r203
plot_r203 <- ggplot(data, aes(x = factor(commune), y = r203, fill = commune, alpha = .8)) +
  labs(x = "commune", y = " Confiance médiateurs (r203)") +
  geom_boxplot(width = .4) +
  #geom_text(
    #aes(label = ifelse(r203 > 15, as.character(id), "")),
    #hjust = 1.2, vjust = 0.5, size = 3
  #) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_r203)

#r204
plot_r204 <- ggplot(data, aes(x = factor(commune), y = r204, fill = commune, alpha = .8)) +
  labs(x = "commune", y = " Confiance personnelle (r204)") +
  geom_boxplot(width = .4) +
  geom_text(
    aes(label = ifelse(r204 > 15, as.character(id), "")),
    hjust = 1.2, vjust = 0.5, size = 3
  ) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#On affiche le graphique seul
print(plot_r204)

#r205
plot_r205 <- ggplot(data, aes(x = factor(commune), y = r205, fill = commune, alpha = .8)) +
  labs(x = "commune", y = " Perception de l'efficacité du renforcement de l'existant (r205)") +
  geom_boxplot(width = .4) +
  #geom_text(
    #aes(label = ifelse(r205 > 15, as.character(id), "")),
    #hjust = 1.2, vjust = 0.5, size = 3
  #) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#On affiche le graphique seul
print(plot_r205)

#r206
plot_r206 <- ggplot(data, aes(x = factor(commune), y = r206, fill = commune, alpha = .8)) +
  labs(x = "commune", y = " Perception de l'efficacité de la réglementation du bâti (r206)") +
  geom_boxplot(width = .4) +
  geom_text(
    aes(label = ifelse(r206 > 15, as.character(id), "")),
    hjust = 1.2, vjust = 0.5, size = 3
  ) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_r206)

#r207
plot_r207 <- ggplot(data, aes(x = factor(commune), y = r207, fill = commune, alpha = .8)) +
  labs(x = "commune", y = " Perception de l'efficacité des mesures d'évacuation (r207)") +
  geom_boxplot(width = .4) +
  geom_text(
    aes(label = ifelse(r207 > 15, as.character(id), "")),
    hjust = 1.2, vjust = 0.5, size = 3
  ) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_r207)

#r301
plot_r301 <- ggplot(data, aes(x = factor(commune), y = r301, fill = commune, alpha = .8)) +
  labs(x = "commune", y = " Attachement au lieu de résidence (r301)") +
  geom_boxplot(width = .4) +
  #geom_text(
    #aes(label = ifelse(r301 > 15, as.character(id), "")),
    #hjust = 1.2, vjust = 0.5, size = 3
  #) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_r301)

#r302
plot_r302 <- ggplot(data, aes(x = factor(commune), y = r302, fill = commune, alpha = .8)) +
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.7
  ) +
  labs(x = "Commune", y = "Nombre d'activités pratiquées (r302)") +
  scale_fill_manual(values = color_commune) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#On affiche le graphique seul
print(plot_r302)

#Création d'un méga graphique en récupérant chaque graph enregistré 
#Ecrire la liste des graph, puis graph sur 2 colonnes 
ggarrange(plot_r101, plot_r102, plot_r103, plot_r104, plot_r105, plot_r106, plot_r201, plot_r202, plot_r203, plot_r204, 
          plot_r205, plot_r206, plot_r207, plot_r301, plot_r302, ncol = 2)

##on refait tout pareil pour l'échelle de l'epci

#Choix manuel des couleurs par epci
color_epci <- c("CINOR" = "#00ABFD", "CIREST" = "#FFFF66","CIVIS" = "#00BC59" ,"TCO" = "#FF689F" )

#r101
plot_e101 <- ggplot(data, aes(x = factor(epci), y = r101, fill = epci, alpha = .8)) +
  stat_summary(
    fun = ~ mean(., na.rm = TRUE) * 100,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.7
  ) +
  labs(x = "EPCI", y = "Pourcentage de personnes ayant subi des dégats (r101)") +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e101)

#r102
plot_e102 <- ggplot(data, aes(x = factor(epci), y = r102, fill = epci, alpha = .8)) +
  stat_summary(
    fun = ~ mean(., na.rm = TRUE) * 100,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.7
  ) +
  labs(x = "EPCI", y = "Pourcentage de personnes exposées indirectement (r102)") +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e102)

#r103
data_r103 <- data %>%
  group_by(epci) %>%
  summarise(
    nb_sources = sum(r103, na.rm = TRUE),
    nb_chercheurs = sum(r103 > 0, na.rm = TRUE),
    moy_sources_par_chercheur = ifelse(nb_chercheurs > 0, nb_sources / nb_chercheurs, NA)
  )
plot_e103 <- ggplot(data_r103, aes(x = factor(epci), y = moy_sources_par_chercheur, fill = epci, alpha = .8)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "EPCI", y = "Nombre moyen de sources utilisées par chercheur (r103)") +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e103)

#r104
plot_e104 <- ggplot(data, aes(x = factor(epci), y = r104, fill = epci, alpha = .8)) +
  stat_summary(
    fun = ~ mean(., na.rm = TRUE) * 100,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.7
  ) +
  labs(x = "EPCI", y = "Pourcentage de personnes qui perçoivent le risque côtier 
       comme étant la problématique la plus dangereuse (r104)") +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e104)

#r105
plot_e105 <- ggplot(data, aes(x = factor(epci), y = r105, fill = epci, alpha = .8)) +
  labs(x = "EPCI", y = " Perception de l'exposition au risque (r105)") +
  geom_boxplot(width = .4) +
  geom_text(
    aes(label = ifelse(r105 > 15, as.character(id), "")),
    hjust = 1.2, vjust = 0.5, size = 3
  ) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e105)

#r106
plot_e106 <- ggplot(data, aes(x = factor(epci), y = r106, fill = epci, alpha = .8)) +
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.7
  ) +
  labs(x = "EPCI", y = "Nombre moyen de programmes connus (r106)") +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e106)

#r201
plot_e201 <- ggplot(data, aes(x = factor(epci), y = r201, fill = epci, alpha = .8)) +
  labs(x = "EPCI", y = " Confiance institutions nationales (r201)") +
  geom_boxplot(width = .4) +
  geom_text(
    aes(label = ifelse(r201 > 15, as.character(id), "")),
    hjust = 1.2, vjust = 0.5, size = 3
  ) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e201)

#r202
plot_e202 <- ggplot(data, aes(x = factor(epci), y = r202, fill = epci, alpha = .8)) +
  labs(x = "EPCI", y = " Confiance institutions locales (r202)") +
  geom_boxplot(width = .4) +
  geom_text(
    aes(label = ifelse(r202 > 15, as.character(id), "")),
    hjust = 1.2, vjust = 0.5, size = 3
  ) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e202)

#r203
plot_e203 <- ggplot(data, aes(x = factor(epci), y = r203, fill = epci, alpha = .8)) +
  labs(x = "EPCI", y = " Confiance médiateurs (r203)") +
  geom_boxplot(width = .4) +
  #geom_text(
  #aes(label = ifelse(r203 > 15, as.character(id), "")),
  #hjust = 1.2, vjust = 0.5, size = 3
  #) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e203)

#r204
plot_e204 <- ggplot(data, aes(x = factor(epci), y = r204, fill = epci, alpha = .8)) +
  labs(x = "EPCI", y = " Confiance personnelle (r204)") +
  geom_boxplot(width = .4) +
  geom_text(
    aes(label = ifelse(r204 > 15, as.character(id), "")),
    hjust = 1.2, vjust = 0.5, size = 3
  ) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e204)

#r205
plot_e205 <- ggplot(data, aes(x = factor(epci), y = r205, fill = epci, alpha = .8)) +
  labs(x = "EPCI", y = " Perception de l'efficacité du renforcement de l'existant (r205)") +
  geom_boxplot(width = .4) +
  #geom_text(
  #aes(label = ifelse(r205 > 15, as.character(id), "")),
  #hjust = 1.2, vjust = 0.5, size = 3
  #) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e205)

#r206
plot_e206 <- ggplot(data, aes(x = factor(epci), y = r206, fill = epci, alpha = .8)) +
  labs(x = "EPCI", y = " Perception de l'efficacité de la réglementation du bâti (r206)") +
  geom_boxplot(width = .4) +
  geom_text(
    aes(label = ifelse(r206 > 15, as.character(id), "")),
    hjust = 1.2, vjust = 0.5, size = 3
  ) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e206)

#r207
plot_e207 <- ggplot(data, aes(x = factor(epci), y = r207, fill = epci, alpha = .8)) +
  labs(x = "EPCI", y = " Perception de l'efficacité des mesures d'évacuation (r207)") +
  geom_boxplot(width = .4) +
  geom_text(
    aes(label = ifelse(r207 > 15, as.character(id), "")),
    hjust = 1.2, vjust = 0.5, size = 3
  ) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e207)

#r301
plot_e301 <- ggplot(data, aes(x = factor(epci), y = r301, fill = epci, alpha = .8)) +
  labs(x = "EPCI", y = " Attachement au lieu de résidence (r301)") +
  geom_boxplot(width = .4) +
  #geom_text(
  #aes(label = ifelse(r301 > 15, as.character(id), "")),
  #hjust = 1.2, vjust = 0.5, size = 3
  #) +
  stat_summary(fun = mean, geom = "crossbar", linetype = "dashed", color = "black", width = .4, fatten = 1) +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e301)

#r302
plot_e302 <- ggplot(data, aes(x = factor(epci), y = r302, fill = epci, alpha = .8)) +
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.7
  ) +
  labs(x = "EPCI", y = "Nombre d'activités pratiquées en moyenne (r302)") +
  scale_fill_manual(values = color_epci) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#On affiche le graphique seul
print(plot_e302)

#Création d'un méga graphique en récupérant chaque graph enregistré 
#Ecrire la liste des graph, puis graph sur 2 colonnes 
ggarrange(plot_e101, plot_e102, plot_e103, plot_e104, plot_e105, plot_e106, plot_e201, plot_e202, plot_e203, plot_e204, 
          plot_e205, plot_e206, plot_e207, plot_e301, plot_e302, ncol = 2)
