# scripts_memoire_SC
Nous présentons les scripts développés dans le cadre du mémoire de Spagnoli Chloé de Master 1 Risques et Environnement (Université Paris 1 Panthéon-Sorbonne).

Scripts R – Mémoire M1 Risques et Environnement
Auteur : Spagnoli Chloé
Stage réalisé à : Observatoire des Sciences de l’Univers de La Réunion – UMR Espace-Dev
Période : 02/25-08/25

Contexte
Ces scripts ont été développés dans le cadre de mon mémoire de Master 1 Risques et Environnement (Université Paris 1 Panthéon-Sorbonne).
Ils ont servi au traitement et à l’analyse des données issues d’une enquête sur les représentations sociales de la vulnérabilité face aux risques côtiers à La Réunion, dans le cadre du projet Nout’Bord Mer, en lien avec l’Observatoire intégré des risques de Brest.

Contenu du dépôt
Le dépôt contient des scripts :
-	De représentativité de l’échantillon : echantillon.R
-	De distribution des indicateurs à l’échelle communale et de l’epci, en barplots pour les variables binaires et en boxplots pour les variables continues : boxplot.R
-	De regroupements de variables pour la création d’indicateurs : ACM.R
-	De calcul de significativité de l'échelle à utiliser avec Khi2 et Kruskal-Wallis suivant les conditions : echelle.R
-	D’analyse univariée des variables d’intérêt : univariee.R
-	D’analyses bivariées et multivariées pour tester nos hypothèses avec Spearman, Mann-Whitney, régression logistique : stats.R
-	D’automatisation des calculs des indicateurs : calcul-index.R
-	De mise en format pivot et de création de fichier shapefile : import_representation.R

Utilisation
Télécharger les scripts .R depuis ce dépôt.
Adapter les chemins de fichiers setwd()à votre environnement de travail.
Placer vos fichiers de données (format .csv ou .xlsx) dans le dossier approprié.
Exécuter les scripts.

Licence
Ce dépôt est mis à disposition sous licence MIT. Vous pouvez réutiliser et adapter le code en citant l’autrice.

Référence
Ces scripts sont associés au mémoire :
Les risques côtiers de l’île de La Réunion : le rôle des représentations sociales dans l’étude de la vulnérabilité systémique – Université Paris 1 Panthéon-Sorbonne – 2025.
<img width="454" height="689" alt="image" src="https://github.com/user-attachments/assets/842f68c4-6ff8-43b5-93f0-76aff12a2193" />
