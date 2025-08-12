#packages
library(dunn.test)
library(FSA)
library(colourpicker)

#importation données
data<-read.csv("bd_khi2.csv",header = TRUE,sep=";", dec=",", stringsAsFactors = TRUE)
head(data)

#test chi2 sur variables catégorielles (r101, r102, r103, r104, r106)
#r101
table_khi2 <- table(data$r101, data$commune)
chisq.test(table_khi2)

table_khi2_epci <- table(data$r101, data$epci)
chisq.test(table_khi2_epci)

#102
table_khi2 <- table(data$r102, data$commune)
chisq.test(table_khi2)

table_khi2_epci <- table(data$r102, data$epci)
chisq.test(table_khi2_epci)

#103
table_khi2 <- table(data$r103, data$commune)
chisq.test(table_khi2)

table_khi2_epci <- table(data$r103, data$epci)
chisq.test(table_khi2_epci)

#104
table_khi2 <- table(data$r104, data$commune)
chisq.test(table_khi2)

table_khi2_epci <- table(data$r104, data$epci)
chisq.test(table_khi2_epci)

#106
table_khi2 <- table(data$r106, data$commune)
chisq.test(table_khi2)

table_khi2_epci <- table(data$r106, data$epci)
chisq.test(table_khi2_epci)


#test Kruskal-Wallis sur les variables continues (r105, r201, r202, r203, r204, r205, r206, r207, r301, r302)
#echelle commune 
# Liste des variables continues sur lesquelles faire le test
vars_kw <- c("r105", "r201", "r202", "r203", "r204", "r205", "r206", "r207", "r301", "r302")

for (var in vars_kw) {
  cat("\n### Test de Kruskal-Wallis pour :", var, "\n")
  
  # Enlever les lignes avec NA dans la variable d’intérêt ou dans 'commune'
  temp <- data[!is.na(data[[var]]) & !is.na(data$commune), ]
  
  # Vérifier qu'il y a au moins deux groupes
  if (length(unique(temp$commune)) > 1) {
    
    # Faire le test
    kw_test <- kruskal.test(temp[[var]] ~ temp$commune)
    
    print(kw_test)
    
  } else {
    cat("⚠️ Moins de 2 groupes valides dans 'commune' pour cette variable.\n")
  }
}


#echelle epci 
# Liste des variables continues sur lesquelles faire le test
vars_kw <- c("r105", "r202", "r203", "r204", "r205", "r206", "r207", "r301", "r302")

for (var in vars_kw) {
  cat("\n### Test de Kruskal-Wallis pour :", var, "\n")
  
  # Enlever les lignes avec NA dans la variable d’intérêt ou dans 'epci'
  temp <- data[!is.na(data[[var]]) & !is.na(data$epci), ]
  
  # Vérifier qu'il y a au moins deux groupes
  if (length(unique(temp$epci)) > 1) {
    
    # Faire le test
    kw_test <- kruskal.test(temp[[var]] ~ temp$epci)
    
    print(kw_test)
    
  } else {
    cat("⚠️ Moins de 2 groupes valides dans 'epci' pour cette variable.\n")
  }
}

#Test de dunn
vars_dunn <- c("r105", "r201", "r202", "r203", "r204", "r205", "r206", "r207", "r301", "r302")

for (var in vars_dunn) {
  cat("\n### Test de Dunn pour :", var, "\n")
  
  # Enlever les lignes avec NA dans la variable d’intérêt ou dans 'commune'
  temp <- data[!is.na(data[[var]]) & !is.na(data$commune), ]
  
  # Vérifier qu'il y a au moins deux groupes
  if (length(unique(temp$commune)) > 1) {
    
    # Faire le test
    dunn_test <- dunnTest(temp[[var]] ~ temp$commune, method="bonferroni")
    
    print(dunn_test)
    
  } else {
    cat("⚠️ Moins de 2 groupes valides dans 'commune' pour cette variable.\n")
  }
}
