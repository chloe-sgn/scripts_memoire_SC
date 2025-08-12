#Installation des packages
library(dplyr)
library(MASS)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(ade4)
library(rgl)
library(kableExtra)
library(broom) 
library(knitr)
library(kableExtra)


############## Tests pour hypothèses ############### 
## Khi2 sur inquietude par materiau 
data<-read.csv("H1.csv",header = TRUE,sep=";", dec=",", stringsAsFactors = TRUE)
head(data)
table_H1 <- table(data$inquietude_generale, data$mat)
chisq.test(table_H1)

## Test Spearman sur inquiétude par attachement 
#Importation des données
H1_com<-read.csv("H1_com.csv",header = TRUE,sep=";", dec=".", stringsAsFactors = TRUE)
head(H1_com)
H1_com$moy_attachement <- as.numeric(H1_com$moy_attachement)
H1_com$moy_inquietude <- as.numeric(H1_com$moy_inquietude)
#Test de Spearman
cor.test(H1_com$moy_attachement, H1_com$moy_inquietude, method = "spearman")

## Test Spearman sur inquiétude par confiance nationale 
#Importation des données
H4_2<-read.csv("H4_2.csv",header = TRUE,sep=";", dec=",", stringsAsFactors = TRUE)
head(H4_2)
H4_2$moy_201 <- as.numeric(H4_2$moy_201)
H4_2$inquietude_generale <- as.numeric(H4_2$inquietude_generale)
#Test de Spearman 201
cor.test(H4_2$inquietude_generale, H4_2$moy_201, method = "spearman")

## Test Spearman sur inquiétude par confiance locale
#Importation des données
H4_2<-read.csv("H4_2.csv",header = TRUE,sep=";", dec=",", stringsAsFactors = TRUE)
head(H4_2)
H4_2$moy_202 <- as.numeric(H4_2$moy_202)
H4_2$inquietude_generale <- as.numeric(H4_2$inquietude_generale)
#Test de Spearman 202
cor.test(H4_2$inquietude_generale, H4_2$moy_202, method = "spearman")

## Test Spearman sur inquiétude par confiance médiateurs 
#Importation des données
H4_2<-read.csv("H4_2.csv",header = TRUE,sep=";", dec=",", stringsAsFactors = TRUE)
head(H4_2)
H4_2$moy_203 <- as.numeric(H4_2$moy_203)
H4_2$inquietude_generale <- as.numeric(H4_2$inquietude_generale)
#Test de Spearman 203
cor.test(H4_2$inquietude_generale, H4_2$moy_203, method = "spearman")

## Test Spearman sur inquiétude par confiance personnelle
#Importation des données
H4_2<-read.csv("H4_2.csv",header = TRUE,sep=";", dec=",", stringsAsFactors = TRUE)
head(H4_2)
H4_2$moy_204 <- as.numeric(H4_2$moy_204)
H4_2$inquietude_generale <- as.numeric(H4_2$inquietude_generale)
#Test de Spearman 204
cor.test(H4_2$inquietude_generale, H4_2$moy_204, method = "spearman")

## Test Spearman sur inquiétude par connaissance 
#Importation des données
H3<-read.csv("H3.csv",header = TRUE,sep=";", dec=".", stringsAsFactors = TRUE)
head(H3)
H3$indice_indiv_connaissance <- as.numeric(H3$indice_indiv_connaissance)
H3$inquietude_generale <- as.numeric(H3$inquietude_generale)
#Test de Spearman
cor.test(H3$inquietude_generale, H3$indice_indiv_connaissance, method = "spearman")

## Test Mann-Whitney sur inquiétude par proximite
#Importation des données
H2<-read.csv("H2.csv",header = TRUE,sep=";", dec=".", stringsAsFactors = TRUE)
head(H2)
H2$proxi <- as.numeric(H2$proxi)
H2$inquietude_habitation <- as.numeric(H2$inquietude_habitation)
#Test de Mann-Whitney
wilcox.test(inquietude_habitation ~ proxi, data = H2)
#médiane par groupe 
aggregate(inquietude_habitation ~ proxi, data = H2, median)
#boxplot 
boxplot(inquietude_habitation ~ proxi, data = H2,
        col = c("#99CCFF", "#FF9966"), 
        names = c("Près de la mer", "Loin de la mer"),
        ylab = "Inquiétude face au risque",
        xlab = "Proximite",
        main = "Niveau d'inquiétude selon la proximité à la mer")

## Test Mann-Whitney sur inquiétude par exposition directe
#Importation des données
H5_1<-read.csv("H5_1.csv",header = TRUE,sep=";", dec=".", stringsAsFactors = TRUE)
head(H5_1)
H5_1$degat <- as.numeric(H5_1$degat)
H5_1$inquietude_habitation <- as.numeric(H5_1$inquietude_habitation)
#Test de Mann-Whitney
wilcox.test(inquietude_habitation ~ degat, data = H5_1)
#médiane par groupe 
aggregate(inquietude_habitation ~ degat, data = H5_1, median)
#boxplot 
boxplot(inquietude_habitation ~ degat, data = H5_1,
        col = c("#CCFFCC", "#CCCCFF"), 
        names = c("Pas de dégâts", "A subi des dégâts"),
        ylab = "Inquiétude face au risque",
        main = "Niveau d'inquiétude selon l'exposition directe")

##Test Mann-Whitney sur inquiétude par exposition indirecte
#Importation des données
H5_2<-read.csv("H5_2.csv",header = TRUE,sep=";", dec=".", stringsAsFactors = TRUE)
head(H5_2)
H5_2$exp_indirecte <- as.numeric(H5_2$exp_indirecte)
H5_2$inquietude_habitation <- as.numeric(H5_2$inquietude_habitation)
#Test de Mann-Whitney
wilcox.test(inquietude_habitation ~ exp_indirecte, data = H5_2)
#médiane par groupe 
aggregate(inquietude_habitation ~ exp_indirecte, data = H5_2, median)
#boxplot 
boxplot(inquietude_habitation ~ exp_indirecte, data = H5_2,
        col = c("#CCFFCC", "#CCCCFF"), 
        names = c("Pas exposé", "Exposé"),
        ylab = "Inquiétude face au risque",
        xlab = "Exposition indirecte",
        main = "Niveau d'inquiétude selon l'exposition indirecte")

##Test regression logistique sur inquiétude et variables individuelles 
#Importation des données
H6<-read.csv("H6.csv",header = TRUE,sep=";", dec=".", stringsAsFactors = TRUE)
head(H6)
H6$inquietude_generale<-ordered(H6$inquietude_generale, levels = c(1, 2, 3, 4, 5))
H6$age<-as.factor(H6$age)
H6$religion<-as.factor(H6$religion)
H6$genre<-as.factor(H6$genre)
H6$csp<-as.factor(H6$csp)


mod<-glm(formula = inquietude_generale ~ age * genre, data = H6, family = )
summary(mod)

#Lancer modèle regression logistique 
regression <- polr(inquietude_generale ~ age * genre, data = H6, Hess = TRUE)
summary_table <- summary(regression)
print(summary_table)
#p-values
ctable <- coef(summary(regression))
pval <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = pval))
# Extraction des résultats
coef_table <- coef(summary(regression)) %>%
  as.data.frame() %>%
  mutate(
    `p-value` = round(2 * (1 - pnorm(abs(`t value`))), 3),
    Signif = case_when(
      `p-value` < 0.001 ~ "***",
      `p-value` < 0.01 ~ "**",
      `p-value` < 0.05 ~ "*",
      `p-value` < 0.1 ~ ".",
      TRUE ~ ""
    )
  )
