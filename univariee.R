#Les packages
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(gridExtra)
library(dplyr)
library(tidyverse)
library(colourpicker)

data<-read.csv("univar.csv",header = TRUE,sep=";", dec=",", stringsAsFactors = TRUE)

data$degat <- as.factor(data$degat)
data$EI <- as.factor(data$EI)
data$proximite <- as.factor(data$proximite)
data$r201 <- as.factor(data$r201)
data$IM <- as.factor(data$IM)
data$IG <- as.factor(data$IG)
data$programme <- as.factor(data$programme)
# Boxplot pour la variable quantitative
ggplot(data, aes(y = degat)) +
  geom_boxplot() +
  labs(title = "Dégâts", y = "Valeurs")

#____________________________________BARPLOTS___________________________________

# DEGAT
plotdegat <- ggplot(data, aes(x = degat, fill = degat)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), width = .5) +
  labs(x = "Dégâts subis", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.9))+
  scale_fill_manual(values = c("#7EC0EE", "#FF9999"))+
  theme(legend.position = "none", axis.title.x = element_text(size = 9))

# EI
plotEI <-ggplot(data, aes(x = EI, fill = EI)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), width = .5) +
  labs(x = "Exposition indirecte", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.9))+
  scale_fill_manual(values = c("#7EC0EE", "#FF9999"))+
  theme(legend.position = "none", axis.title.x = element_text(size = 9))

# PROXIMITE
plotproxi <- ggplot(data, aes(x = proximite, fill = proximite)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), width = .5) +
  labs(x = "Résidence en bord de mer", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.9))+
  scale_fill_manual(values = c("#7EC0EE", "#FF9999"))+
  theme(legend.position = "none", axis.title.x = element_text(size = 9))

# PROGRAMME
plotprog <- ggplot(data, aes(x = programme, fill = programme)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), width = .5) +
  labs(x = "Connaissance d'au moins un \nprogramme de gestion", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7))+
  scale_fill_manual(values = c("#EE6363", "#FFB90F", "#B4EEB4", "#76EEC6", "#A4D3EE", "#DDA0DD", "#FFC0CB"))+
  theme(legend.position = "none", axis.title.x = element_text(size = 9))

# IM
plotIM <- ggplot(data, aes(x = IM, fill = IM)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), width = .5) +
  labs(x = "Inquiétude pour \nl'habitation", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7))+
  scale_fill_manual(values = c("#EE6363", "#FFB90F", "#B4EEB4", "#A4D3EE", "#DDA0DD"))+
  theme(legend.position = "none", axis.title.x = element_text(size = 9))

# IG
plotIG <- ggplot(data, aes(x = IG, fill = IG)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), width = .5) +
  labs(x = "Inquiétude face \naux risques côtiers", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7))+
  scale_fill_manual(values = c("#EE6363", "#FFB90F", "#B4EEB4", "#A4D3EE", "#DDA0DD"))+
  theme(legend.position = "none", axis.title.x = element_text(size = 9))

# R201
plotr201 <- ggplot(data, aes(x = r201, fill = r201)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), width = .5) +
  labs(x = "Confiance en les \ninstitutions nationales", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7))+
  scale_fill_manual(values = c("#EE6363", "#FFB90F", "#B4EEB4", "#A4D3EE", "#DDA0DD"))+
  theme(legend.position = "none", axis.title.x = element_text(size = 9))

# R202
# Créer une nouvelle colonne pour les catégories
data$r202_category <- cut(data$r202,
                        breaks = c(0.9, 1.9, 2.9, 3.9, 4.9, 5.9),
                        labels = c("[1;2[", "[2;3[", "[3;4[", "[4;5[", "5"),
                        include.lowest = TRUE)

plotr202 <- ggplot(data, aes(x = r202_category, fill = r202_category)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), width = .5) +
  labs(x = "Confiance en les \ninstitutions locales", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7))+
  scale_fill_manual(values = c("#EE6363", "#FFB90F", "#B4EEB4", "#A4D3EE", "#DDA0DD"))+
  theme(legend.position = "none", axis.title.x = element_text(size = 9))

#R203
# Créer une nouvelle colonne pour les catégories
data$r203_category <- cut(data$r203,
                          breaks = c(0.9, 1.9, 2.9, 3.9, 4.9, 5.9),
                          labels = c("[1;2[", "[2;3[", "[3;4[", "[4;5[", "5"),
                          include.lowest = TRUE)

plotr203 <- ggplot(data, aes(x = r203_category, fill = r203_category)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), width = .5) +
  labs(x = "Confiance en les \nmédiateurs", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7))+
  scale_fill_manual(values = c("#EE6363", "#FFB90F", "#B4EEB4", "#A4D3EE", "#DDA0DD"))+
  theme(legend.position = "none", axis.title.x = element_text(size = 9))

#R204
# Créer une nouvelle colonne pour les catégories
data$r204_category <- cut(data$r204,
                          breaks = c(0.9, 1.9, 2.9, 3.9, 4.9, 5.9),
                          labels = c("[1;2[", "[2;3[", "[3;4[", "[4;5[", "5"),
                          include.lowest = TRUE)

plotr204 <- ggplot(data, aes(x = r204_category, fill = r204_category)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), width = .5) +
  labs(x = "Confiance en en soi et en sa \nreligion pour gérer les risques", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7))+
  scale_fill_manual(values = c("#EE6363", "#FFB90F", "#B4EEB4", "#A4D3EE", "#DDA0DD"))+
  theme(legend.position = "none", axis.title.x = element_text(size = 9))

# ATTACHE
# Créer une nouvelle colonne pour les catégories
data$attache_category <- cut(data$attache,
                          breaks = c(0.9, 1.9, 2.9, 3.9, 4.9, 5.9),
                          labels = c("[1;2[", "[2;3[", "[3;4[", "[4;5[", "5"),
                          include.lowest = TRUE)

plotattache <- ggplot(data, aes(x = attache_category, fill = attache_category)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), width = .5) +
  labs(x = "Attachement \nau lieu de résidence", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7))+
  scale_fill_manual(values = c("#FFB90F", "#B4EEB4", "#A4D3EE"))+
  scale_x_discrete(drop = FALSE) + # Assure que toutes les catégories sont affichées
  theme(legend.position = "none", axis.title.x = element_text(size = 9))

#Graph combinés
ggarrange(plotproxi, plotdegat, plotEI, plotIM, plotIG, plotprog, plotr201, plotr202, plotr203, plotr204, plotattache, ncol = 3, nrow = 4,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"))
