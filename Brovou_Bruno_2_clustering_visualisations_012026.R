# ============================
# 02_acp_clustering.R
# ACP + classification de pays
# ============================

# Chargement des packages nécessaires à l’analyse

library(tidyverse)                     # Ensemble de packages pour manipulation de données, visualisation et programmation fonctionnelle
library(FactoMineR)                    # Package spécialisé pour les analyses multivariées (ACP, AFC, etc.)
library(factoextra)                    # Outils graphiques pour visualiser les résultats d’analyses multivariées (ACP, clustering…)
library(cluster)                       # Fonctions pour l’analyse de clusters (k-means, silhouette, etc.)
library(ggplot2)                       # Visualisation avancée avec la grammaire des graphiques
library(dplyr)                         # Manipulation de données (filtrage, regroupement, transformation…)
library(tidyr)                         # Permet de restructurer les tableaux (long ↔ large), séparer ou fusionner des colonnes, gérer les valeurs manquantes, et préparer les données pour l’analyse statistique ou visuelle
library(stringr)                       # Fonctions pour manipuler les chaînes de caractères (ex. : suppression d’accents, mise en majuscule)
library(ggrepel)                       # Améliore la lisibilité des graphiques en évitant le chevauchement des labels
library(tidyverse)                     # Ensemble de packages pour la manipulation, visualisation et transformation de données (inclut dplyr, ggplot2, etc.)
library(writexl)

# Variables retenues pour l'ACP 
vars_pca <- c(
  # Volaille / marché
  "poulet_kg_par_hab",
  "nourriture_qte",
  "import_qte",
  "export_qte",
  "taux_autosuff_volaille",
  "taux_indep_volaille",
  
  # Démographie / économie
  "pop_totale",
  "pib_hab",
  
  # Socio-économique / infrastructure
  "taux_urbain",
  "acces_elec",
  
  # Gouvernance / logistique
  "stab_politique",
  "controle_corr",
  "participation_resp"
)

# Création du jeu de données df_pca à partir de base_2017_eff, en sélectionnant les variables pertinentes pour l’ACP
df_pca <- base_2017_eff %>%
  dplyr::select(zone, dplyr::all_of(vars_pca)) %>%                          # Sélectionne la colonne 'zone' + toutes les variables listées dans 'vars_pca'
  tidyr::drop_na()                                                          # Supprime les lignes contenant des valeurs manquantes (NA) sur les colonnes sélectionnées
 
df_pca <- df_pca %>%
  mutate(
    label_court = toupper(substr(zone, 1, 3))                               # Crée une nouvelle colonne 'label_court' avec les 3 premières lettres de 'zone' en majuscules
  )

# Copie le tableau df_pca dans df_acp pour travailler sur une version dédiée à l’ACP
df_acp <- df_pca

lab <- df_acp$label_court                                                   # Extrait la colonne 'label_court' (identifiants courts des pays) dans un vecteur lab

lab_unique <- make.unique(lab)                                              # Rend chaque libellé unique en ajoutant un suffixe si nécessaire (ex. : "KEN", "KEN.1")


# Réalise une analyse en composantes principales (ACP) sur les variables sélectionnées
res_pca <- FactoMineR::PCA(df_acp[ , vars_pca], 
                scale.unit = TRUE,                           # variables sont centrées et réduites (standardisation)
                ncp = 10,                                    # conserver les 10 premières composantes principales
                graph = FALSE)                               # Désactive l’affichage automatique des graphiques.

# Vérifie que les données ont bien été centrées (moyenne ≈ 0 pour chaque variable)
apply(res_pca$call$X, 2, mean)      # ≈ 0

# Vérifie que les données ont bien été réduites (écart-type ≈ 1 pour chaque variable)
apply(res_pca$call$X, 2, sd)        # ≈ 1

# Remplace les noms des lignes (pays) dans les coordonnées factorielles par des identifiants courts et uniques
rownames(res_pca$ind$coord)   <- lab_unique

# Applique les mêmes noms aux lignes de la qualité de représentation (cos²) des individus sur les axes
rownames(res_pca$ind$cos2)    <- lab_unique

# Applique les mêmes noms aux lignes des contributions des individus à la construction des axes
rownames(res_pca$ind$contrib) <- lab_unique

# Affiche la matrice des valeurs propres (eigenvalues), avec la variance expliquée par chaque axe
print(res_pca$eig)

# Identifie les composantes principales dont la valeur propre est supérieure à 1 (critère de Kaiser)
which(res_pca$eig[, "eigenvalue"] > 1)

# Affiche le pourcentage cumulé de variance expliqué par les composantes principales
res_pca$eig[, "cumulative percentage of variance"]

# Affiche le diagramme des éboulis (scree plot) des valeurs propres de l'ACP
fviz_eig(res_pca, addlabels = TRUE,                              # Ajoute les étiquettes de pourcentage sur chaque barre
         main = "Diagramme des eboulis")                         # Titre du graphique

# Interpretations
# Nous avons donc retenu 5 composantes (valeurs propres > 1), avec une interprétation centrée sur les 4 premiers axes, qui expliquent environ 65 % de la variance totale. 

# Affiche le cercle des corrélations des variables sur les axes 1 et 2
fviz_pca_var(
  res_pca,                                                       # Objet contenant les résultats de l'ACP
  col.var = "cos2",                                              # Colore les flèches selon la qualité de représentation (cos²) sur le plan F1–F2
  gradient.cols = c("#DEEBF7", "#3182BD", "#08519C"),            # Dégradé de bleu pour indiquer la qualité de projection
  repel = TRUE,                                                  # Évite le chevauchement des noms de variables (labels lisibles)
  title = "Cercle des correlations (Dim 1 et 2)"                 # Titre du graphique
)

# Interprétation des axes F1 et F2 avec le cercle des corrélations

# Sur F1, partent quasiment dans la même direction: controle_corr, participation_resp, stab_politique, taux_urbain, access_elec et poulet_kg_par_hab.  Les flèches sont longues et très proches les unes des autres ce qui veut dire que ces variables sont fortement corrélées positivement entre elles et  fortement corrélées à F1.Donc   F1 est un axe de niveau de gouvernance et de développement socio-urbain, couplé au niveau de consommation de volaille. 
# Les variables de commerce (import_qte,export_qte) sont corrélées positivement mais plus faiblement alors elles renforcent la dimension "économie organisée / intégrée au commerce", mais secondairement
# Sur F2, les flèches les plus longues sont :  nourriture_qte,  pop_total,  export_qte, stab_politique sont plutôt négative sur F2 (pointée vers le bas sur F2 dans le cercle). Donc  F2 est un axe de volume alimentaire et de taille de marché.
# On constate que nourriture_qte  est proche de pop_totale et export_qte alors les pays très peuplés sont aussi ceux qui pèsent le plus dans la disponibilité et le commerce de nourriture.

# Affiche le cercle des corrélations des variables sur les axes 1 et 2
fviz_pca_var(res_pca,
             axes = c(1, 2),                                          # Sélectionne les deux premiers axes (Dim 1 et Dim 2)
             col.var = "cos2",                                        # Colore les flèches selon la qualité de représentation (cos²)
             select.var = list(cos2 = 0.4),                           # Affiche uniquement les variables avec cos² ≥ 0.4 (bien projetées)
             gradient.cols = c("#DEEBF7", "#3182BD", "#08519C"),      # Dégradé de bleu pour indiquer la qualité de projection
             repel = TRUE,                                            # Évite le chevauchement des labels pour une meilleure lisibilité
             title     = "Cercle des correlations (axes 1 et 2)")     # Titre du graphique

# Affiche le cercle des corrélations des variables sur les axes 3 et 4
fviz_pca_var(res_pca,
             axes = c(3, 4),                                          # Sélectionne les deux premiers axes (Dim 1 et Dim 2)
             col.var = "cos2",                                        # Colore les flèches selon la qualité de représentation (cos²)
             select.var = list(cos2 = 0.4),                           # Affiche uniquement les variables avec cos² ≥ 0.4 (bien projetées)
             gradient.cols = c("#DEEBF7", "#3182BD", "#08519C"),      # Dégradé de bleu pour indiquer la qualité de projection
             repel = TRUE,                                            # Évite le chevauchement des labels pour une meilleure lisibilité
             title     = "Cercle des correlations (axes 3 et 4)")     # Titre du graphique

# Interprétation des axes F3 et F4 avec le cercle des corrélations
 # import_qte et taux_indep_volaille bien projetés positivement sur F3. taux_autosuff_volaille et stab_politique tirés vers le négatif.Donc  F3 oppose les pays dépendants des importations aux pays plus autosuffisants et stables. 
  # C’est un axe « structure des échanges : dépendance vs autosuffisance ». 
 # « L’axe 4 explique environ 9 % de la variance et révèle des tensions spécifiques entre dépendance commerciale et autonomie filière. Il oppose :
  # D’un côté, les pays fortement dépendants des importations (import_qte) et avec un faible taux d’indépendance volaille (taux_indep_volaille`)
  # De l’autre, des pays avec un PIB/habitant élevé (pib_hab), souvent plus autonomes et capables de structurer leur propre filière. 
  # Cet axe met en lumière une fracture entre dépendance et capacité économique 
  
# Affiche les pays dans le plan factoriel formé par les axes 1 et 2
fviz_pca_ind(
  res_pca,                                                           # Objet contenant les résultats de l'ACP
  axes        = c(1, 2),                                             # Sélectionne les axes 1 et 2 pour la projection
  geom = c("point", "text"),                                         # Affiche à la fois les points et les noms des pays
  label       = "all",                                               # Affiche les libellés pour tous les pays
  repel = TRUE,                                                      # Évite le chevauchement des noms (labels lisibles)
  col.ind = "cos2",                                                  # Colore les points selon leur qualité de représentation (cos²)
  gradient.cols = c("#DEEBF7", "#3182BD", "#08519C"),                # Dégradé bleu pour indiquer la qualité de projection
  title = "Pays sur le plan factoriel (Dim 1–2)"                     # Titre du graphique
)

# Interprétation des axes F1 et F2 avec le plan factoriel
#  L’axe horizontal F1 place les pays selon leur niveau de modernité/gouvernance et leur consommation par habitant. L’axe vertical F2 les positionne selon leur taille de marché alimentaire (population + volumes de volaille).
# Quadrant droite-haut (F1 élevé, F2 élevé) :  grands marchés, relativement bien gouvernés, urbanisés, solvables (ex. États-Unis, Chine continentale, Brésil),  ce sont les « pays poids lourds » à fort potentiel, mais déjà très concurrentiels.

# Affiche les pays dans le plan factoriel formé par les axes 1 et 2
fviz_pca_ind(
  res_pca,                                                           # Objet contenant les résultats de l'ACP
  axes        = c(3, 4),                                             # Sélectionne les axes 1 et 2 pour la projection
  geom = c("point", "text"),                                         # Affiche à la fois les points et les noms des pays
  label       = "all",                                               # Affiche les libellés pour tous les pays
  repel = TRUE,                                                      # Évite le chevauchement des noms (labels lisibles)
  col.ind = "cos2",                                                  # Colore les points selon leur qualité de représentation (cos²)
  gradient.cols = c("#DEEBF7", "#3182BD", "#08519C"),                # Dégradé bleu pour indiquer la qualité de projection
  title = "Pays sur le plan factoriel (Dim 3–4)"                     # Titre du graphique
)

# Interprétation des axes F3 et F4 avec le plan factoriel
 # En haut de F3 : pays très dépendants des importations de volaille. En bas de F3 : pays plus autosuffisants et relativement stables ( Chine - RAS de Hong Kong) . 
 # •	Axe 4 (~9 %) : niveau de vie & résilience économique : En haut : pays avec un PIB/habitant élevé, capables d’absorber une offre premium. En bas : pays à plus faible pouvoir d’achat ou à structuration filière limitée

# Extrait uniquement les colonnes numériques spécifiées dans 'vars_pca' depuis le tableau 'df_acp'
X_num <- df_acp[ , vars_pca]   

# Scores des pays sur les k premiers axes
k <- 5                                                        # Définit le nombre de dimensions à retenir 
scores <- res_pca$ind$coord[ , 1:k]                           # Extrait les coordonnées des pays sur les k premiers axes (F1 à F5)

# Calcule la matrice de corrélation entre les variables initiales (X_num) et les composantes principales (scores)
mat_corr_dim_var <- cor(                                      
  X_num,                                                      # Matrice des variables quantitatives initiales
  scores,                                                     # Coordonnées des individus sur les k premiers axes ACP
  use = "pairwise.complete.obs"                               # Utilise uniquement les paires d'observations complètes (ignore les NA)
)

# Attribue les noms des variables initiales comme noms de lignes de la matrice
rownames(mat_corr_dim_var) <- vars_pca            

# Génère les noms des colonnes : Dim1, Dim2, ..., Dim5
colnames(mat_corr_dim_var) <- paste0("Dim", 1:k)                     

# Arrondit les valeurs de la matrice de corrélation à 3 décimales pour une lecture plus claire
round(mat_corr_dim_var, 3)                                          

# Exporte la matrice dans un fichier CSV avec les noms de lignes
write.csv(
  mat_corr_dim_var,
  "data_clean/corr_dim_variable.csv",
  row.names = TRUE
)

#  Interpretation des axes avec la matrice de corrélation entre les variables initiales (X_num) et les composantes principales (scores)

# F1 – Gouvernance / Modernité / Consommation - Variables fortement corrélées à F1 (|corr| ≥ 0,6) :  controle_corr : 0,86;  participation_resp  : 0,77;  stab_politique : 0,70;  taux_urbain  : 0,68;  access_elec  : 0,61;  poulet_kg_par_hab  : 0,6. Donc  On résume que F1 comme un score de modernité et de qualité de gouvernance, très associé à la consommation de volaille par habitant.
# F2 – Taille de marché / Poids alimentaire - Corrélations fortes avec F2 : nourriture_qte :  0,90; export_qte :0,69; pop_totale:0,77; taux_autosuff_volaille : 0,40 (modéré). Donc  F2 mesure l’importance du pays dans le système alimentaire mondial de la volaille (volume de nourriture dispo + exportations + population).
# F3 – Dépendance vs Autosuffisance -  Corrélations fortes avec F3 : import_qte : 0,6; taux_indep_volaille : 0,68; taux_autosff_volaille : –0,47;  stab_politique  : –0,48 (lien inverse). Donc  F3 est un indice de dépendancece.
# F4 est corrélé surtout avec pib_hab (0,67) et access_elec (0,50) -> gradient supplémentaire de richesse par habitant, plutôt entre pays riches/small vs pays de taille intermédiaire.
# F5 est surtout négatif pour  taux_urbain (–0,60) et poulet_kg_par_hab (–0,47) c'est un axe plus technique.

# De interpretation des axes avec le cercle et la matrice de corrélation, nous pouvons conclure :
# Axe F1 – Modernité & Gouvernance : Fortement corrélé à : gouvernance (contrôle de la corruption, participation, stabilité), urbanisation, accès à l’électricité, consommation de volaille par habitant.
# Axe F2 – Poids alimentaire & taille de marché : Corrélé aux volumes de nourriture disponible, aux exportations et à la population totale.
# Axe F3 – Dépendance aux importations : Oppose pays dépendants des importations (import_qte, taux_indep_volaille) aux pays autosuffisants (taux_autosuff_volaille) et plus stables.
# Axe F4 - pays fortement dépendants des importations (import_qte) et avec un Ffaible taux d’indépendance volaille (taux_indep_volaille`)

# indices des points extrêmes repérés sur les graphiques
idx_out3 <- c(96, 62, 76, 126)

# Affiche les noms des zones (pays) correspondant aux indices d’outliers stockés dans 'idx_out3'
df_acp$zone[idx_out3]

# Crée un nouveau tableau en excluant les outliers identifiés (suppression des lignes idx_out3)
df_pca_no3 <- df_acp[-idx_out3, ]

# Extrait la distance de chaque pays à l’origine du plan factoriel
dist_ind <- res_pca$ind$dist   

# Crée un tableau avec les pays identifiés comme outliers
outliers_tab <- tibble(
  numero   = idx_out3,                                                  # Numéro d’indice des pays outliers
  pays     = df_pca$zone[idx_out3],                                     # Nom des pays correspondant aux indices
  distance = dist_ind[idx_out3]                                         # Distance à l’origine du plan ACP (mesure d’atypicité)
) %>%
  bind_cols(                                                            # Ajoute les coordonnées des pays sur les 5 premières dimensions ACP
    as.data.frame(res_pca$ind$coord[idx_out3, 1:5]) %>%                 # F1 à F5Extrait les scores ACP sur Dim1 à Dim5
      setNames(paste0("Dim", 1:5))                                      # Renomme les colonnes en Dim1, Dim2, ..., Dim5
  )

outliers_tab                                                            # Affiche le tableau des pays atypiques avec leurs coordonnées ACP

# Calcule la moyenne de chaque variable, en ignorant les valeurs manquantes
moy_globale <- sapply(df_pca[ , vars_pca], mean, na.rm = TRUE)          

# Calcule l’écart-type de chaque variable, en ignorant les valeurs manquantes
sd_globale  <- sapply(df_pca[ , vars_pca], sd,   na.rm = TRUE)          

# Sélectionne les variables des pays identifiés comme outliers
profil_outliers <- df_pca[idx_out3, vars_pca] %>%
  mutate(pays = df_pca$zone[idx_out3]) %>%                                       # Ajoute une colonne 'pays' avec les noms des pays outliers
  relocate(pays) %>%                                                             # Place la colonne 'pays' en première position
  mutate(                                                                        # Applique une transformation à toutes les variables sélectionnées
    across(                                                                      
      all_of(vars_pca),                                                          # Cible uniquement les variables quantitatives
      ~ (.x - moy_globale[cur_column()]) / sd_globale[cur_column()],             # Standardise chaque variable : (valeur - moyenne) / écart-type
      .names = "z_{.col}"                                                        # Nomme les nouvelles colonnes avec le préfixe 'z_' (ex. : z_import_qte)
    )
  )

profil_outliers                                                                  # Affiche le tableau des outliers avec leurs valeurs brutes et standardisées

# Fixe la graine aléatoire pour garantir la reproductibilité des résultats
set.seed(123)

# Applique un clustering k-means sur les pays outliers, selon leurs coordonnées sur les axes ACP 1 à 3
km_out <- kmeans(res_pca$ind$coord[idx_out3, 1:3], centers = 3, nstart = 50)

# Ajoute une colonne 'groupe' indiquant le cluster k-means auquel appartient chaque pays outlier
#outliers_tab <- outliers_tab %>%
#  mutate(groupe = km_out$cluster)

# Affiche le tableau mis à jour avec les groupes de segmentation
#outliers_tab                                  

# Ajoute la colonne 'groupe' au tableau des profils standardisés des pays outliers
profil_outliers_grp <- profil_outliers %>%
  mutate(groupe = km_out$cluster)

resume_groupes <- profil_outliers_grp %>% 
  group_by(groupe) %>%                                 # Regroupe les pays par cluster k-means
  summarise(
    across(
      starts_with("z_"),                               # Cible toutes les variables standardisées (z_scores)
      ~ mean(.x, na.rm = TRUE)                         # Calcule la moyenne de chaque variable pour chaque groupe
    )
  )

resume_groupes

# Définit le seuil de score z au-delà duquel une variable est considérée comme marquante
seuil <- 1.5

profil_long <- profil_outliers %>%
  select(pays, starts_with("z_")) %>%                         # Sélectionne les variables standardisées et le nom du pays
  tidyr::pivot_longer(                                        # Transforme le tableau en format long : une ligne par variable et par pays
    cols      = starts_with("z_"),
    names_to  = "variable",
    values_to = "z"
  )

profil_marque <- profil_long %>%
  filter(abs(z) >= seuil) %>%                                 # Garde uniquement les variables avec un score z ≥ 1.5 (positif ou négatif)
  arrange(pays, desc(abs(z)))                                 # Trie les résultats par pays et intensité du score z

# Affiche le tableau final des variables les plus discriminantes pour chaque pays outlier
profil_marque

# Définit un seuil de score z à ±2 pour identifier les variables très atypiques (écart fort à la moyenne)
seuil_z <- 2

# Crée un tableau avec les variables brutes et standardisées pour les pays outliers
profil_marque2 <- profil_outliers |>
  select(pays,
         poulet_kg_par_hab, nourriture_qte, import_qte, export_qte,
         taux_autosuff_volaille, taux_indep_volaille,
         pop_totale, pib_hab, taux_urbain, acces_elec,
         stab_politique, controle_corr, participation_resp,
         starts_with("z_")) |>                                      # Sélectionne aussi les variables standardisées
  
  pivot_longer(starts_with("z_"),                                   # Transforme les colonnes z_ en format long
               names_to  = "variable",                              # Nom de la variable standardisée
               values_to = "z") |>                                  # Valeur du score z
  
  filter(abs(z) >= seuil_z) |>                                      # Garde les variables avec score z ≥ 2 (très atypiques)
  arrange(pays, desc(abs(z)))                                       # Trie par pays et intensité du score z

profil_marque2

# Filtre pour ne garder que certains pays ciblés
profil_marque2 |> filter(pays == "Br\u00e9sil")
profil_marque2 |> filter(pays == "Chine, continentale")
profil_marque2 |> filter(grepl("\u00c9tats-Unis d'Am\u00e9rique", pays))
profil_marque2 |> filter(grepl("Chine - RAS de Hong-Kong", pays))

# Caractériser chaque outliers
# Interprétation détaillée des 4 outliers ( z ≈ 0 : pays “dans la moyenne” mondiale. z > 0 : au dessus de la moyenne (positif). z > 0 : au-dessus de la moyenne (positif). z < 0 : en-dessous de la moyenne. |z| ≥ 2 : très atypique.)
# Chine continentale (indice 96) : z_pop_totale ≈ +7.8;  z_nourriture_qte ≈ +6.8; z_import_qte ≈ +1.7 (au-dessus de la moyenne, mais moins extrême que les deux précédents)  et z_participation_resp ≈ -1.9 (gouvernance participative nettement plus faible que la moyenne. C’est de loin le plus grand marché en volume absolu (population et volume total de disponibilités alimentaires “nourriture_qte”).
# Brésil (indice 62) : z_export_qte ≈ +7.8;  z_nourriture_qte ≈ +3.78; z_poulet_kg_par_hab ≈ +1.7 (consommation par habitant élevée). Le Brésil est un exportateur massif de volaille (l’un des plus extrêmes sur  export_qte).
# États-Unis d’Amérique (indice 76) : z_export_qte ≈ +7.1;  z_nourriture_qte ≈ +6.1;  z_poulet_kg_par_hab ≈ +2.3;  z_pop_totale ≈ +1.6 . Comme le Brésil, les États-Unis sont un très grand exportateur de volaille, avec des volumes disponibles extrêmement élevés.
# Chine – RAS de Hong-Kong (indice 126) : z_taux_indep_volaille ≈ +6.1;  z_import_qte ≈ +3.9;  z_poulet_kg_par_hab ≈ +2.9;  z_taux_urbain ≈ +1.8;  z_controle_corr ≈ +1.6  . taux_indep_volaille  =  import_qte /  offre_total  très élevé -> Hong-Kong est quasi totalement dépendant de ses importations de volaille.

# On peut  regrouper ces outliers en “profils” 
# Géant démographique & volume alimentaire : Chine continentale (Très gros volume, très grande population, consommation massive, gouvernance peu participative.)
# Super-exportateurs de volaille : Brésil et États-Unis d’Amérique (Production et exportation très élevées, disponibilité importante, forte consommation interne.)
# Hub urbain riche totalement importateur : Chine – RAS de Hong-Kong (Dépendance extrême aux importations, consommation par habitant élevée, forte urbanisation, bonnes institutions.)


# Relancer l’ACP sur le jeu filtré

# Variables retenues pour le second ACP 
vars_pca <- c(
  "poulet_kg_par_hab",
  "nourriture_qte",
  "import_qte",
  "export_qte",
  "taux_autosuff_volaille",
  "taux_indep_volaille",
  "pop_totale",
  "pib_hab",
  "taux_urbain",
  "acces_elec",
  "stab_politique",
  "controle_corr",
  "participation_resp"
)

df_pca_no3 <- df_pca_no3 %>%
  mutate(
    label_court = toupper(substr(zone, 1, 3))                                   # Crée une abréviation en majuscules à partir des 3 premières lettres du nom du pays
  )

# Copie le tableau df_pca_no3 dans df_acp_no3 pour travailler sur une version dédiée à l’ACP
df_acp_no3 <- df_pca_no3

# Récupère les labels courts des pays (ex. : BRA, CHI, ÉTA)
lab <- df_acp_no3$label_court

# Rend les labels uniques en cas de doublons (ex. : CHI, CHI.1, CHI.2)
lab_unique <- make.unique(lab)

# Lance une nouvelle ACP sur les données nettoyées (sans outliers)
res_pca_no3 <- FactoMineR::PCA(
  df_acp_no3[ , vars_pca],                                                       # Utilise uniquement les variables quantitatives sélectionnées
  scale.unit = TRUE,                                                             # Standardise les variables avant l’ACP
  ncp       = 10,                                                                # Retient les 10 premières dimensions
  graph     = FALSE                                                              # Désactive les graphiques automatiques
)

# Vérifie que les moyennes des variables sont ≈ 0
apply(res_pca_no3$call$X, 2, mean)      

# Vérifie que les écarts-types sont ≈ 1
apply(res_pca_no3$call$X, 2, sd)        

# Attribue les labels uniques aux coordonnées des pays
rownames(res_pca_no3$ind$coord)   <- lab_unique

# Attribue les labels aux cos² (qualité de représentation)
rownames(res_pca_no3$ind$cos2)    <- lab_unique

# Attribue les labels aux contributions des pays
rownames(res_pca_no3$ind$contrib) <- lab_unique


# Affiche la matrice des valeurs propres (eigenvalues), avec la variance expliquée par chaque axe
print(res_pca_no3$eig)

# Identifie les composantes principales dont la valeur propre est supérieure à 1 (critère de Kaiser)
which(res_pca_no3$eig[, "eigenvalue"] > 1)

# Affiche le pourcentage cumulé de variance expliqué par les composantes principales
res_pca_no3$eig[, "cumulative percentage of variance"]

# Affiche le diagramme des éboulis (scree plot) des valeurs propres de l'ACP
fviz_eig(
  res_pca_no3,                                                                    # Objet contenant les résultats de la seconde ACP
  addlabels = TRUE,                                                               # Ajoute les étiquettes de pourcentage sur chaque barre
  main = "Diagramme des eboulis – ACP finale (sans outiliers)"
)

# Interpretations
# Nous avons donc retenu 5 composantes (valeurs propres > 1), avec une interprétation centrée sur les 5 premiers axes, qui expliquent environ 74 % de la variance totale. 

# Affiche le cercle des corrélations des variables sur les axes 1 et 2
fviz_pca_var(
  res_pca_no3,
  axes = c(1, 2),                                                       # Projection sur les deux premiers axes principaux. Ceux qui captent le plus d’inertie (variance expliquée)
  col.var = "cos2",               
  gradient.cols = c("#DEEBF7", "#3182BD", "#08519C"),
  repel = TRUE,
  title = "Cercle des correlations (Dim 1 et 2) - (sans outiliers)"
)


# Interprétation des axes F1 et F2 avec le cercle des corrélations
# Les flèches taux_urbain, acces_elec, stab_politique, controle_corr, participation_resp, poulet_kg_par_hab, import_qte, export_qte sont toutes pointées vers la droite, bien alignées sur Dim1, avec une longueur importante. Les variables gouvernance / institutions (stab_politique, controle_corr, participation_resp) sont très proches du cercle unité et fortement corrélées entre elles (flèches proches). Les variables d’infrastructures / niveau de vie (taux_urbain, acces_elec) sont aussi très corrélées à Dim1 et dans la même direction. 
  # Donc  Dim1 est donc clairement un axe « développement / gouvernance & infrastructures / consommation de volaille ».
# nourriture_qte est quasiment sur l’axe vertical avec une corrélation environ 0,85 sur Dim2, c’est la variable dominante. import_qte (0,52) et export_qte (0,47) ont aussi une composante positive nette sur Dim2. pop_totale et pib_hab ont une contribution modérée sur Dim2 ( environ 0,32).
  # Donc  Dim2 oppose donc des pays avec de gros volumes totaux de volaille / nourriture à des pays de volumes plus faibles, indépendamment du niveau de gouvernance.

# Affiche le cercle des corrélations des variables sur les axes 3 et 4
fviz_pca_var(
  res_pca_no3,
  axes = c(3, 4),                                                       # Projection sur les deux derniers axes principaux. Ceux qui captent le plus d’inertie (variance expliquée)
  col.var = "cos2",               
  gradient.cols = c("#DEEBF7", "#3182BD", "#08519C"),
  repel = TRUE,
  title = "Cercle des correlations (Dim 3 et 4) - (sans outiliers)"
)

# Interprétation de l'axe F3 et F4 avec le cercle des corrélations
 # Sur Dim3, poulet_kg_par_hab : +0.518;  export_qte : –0.625;  taux_autosuff_volaille : –0.530;  contributions plus modestes des autres variables. Donc un axe qui oppose d’un côté, des pays fortement autosuffisants et/ou exportateurs de volaille, de l’autre, des pays où la consommation par habitant est élevée mais moins tirée par l’autosuffisance et les exportations.
 # import_qte : 0.611;  taux_indep_volaille : 0.634;  taux_autosuff_volaille : –0.464; Donc  Dim4 = dépendance aux importations de volaille

# Affiche le cercle des corrélations des variables sur les axes 1 et 5
fviz_pca_var(res_pca_no3,
             axes = c(1, 5),                                                             # Projection sur les deux premiers axes principaux. Ceux qui captent le plus d’inertie (variance expliquée)
             col.var = "cos2",                                                           # Coloration des variables selon leur qualité de représentation (cos²). Plus le cos² est élevé, plus la variable est bien projetée
             gradient.cols = c("#DEEBF7", "#3182BD", "#08519C"),
             repel = TRUE,                                                               # Évite le chevauchement des labels pour une meilleure lisibilité
             title     = "Cercle des correlations (axes 1 et 5) - (sans outiliers)")     # Titre du graphique

# Interprétation de l'axe F5 avec le cercle des corrélations
# pop_totale : 0.647;  pib_hab : 0.379;  contributions modérées des variables de gouvernance.Donc  Dim5 = gabarit démographique / économique résiduel 


# Affiche les pays dans le plan factoriel formé par les axes 1 et 2
factoextra::fviz_pca_ind(
  res_pca_no3,
  axes        = c(1, 2),     
  geom = c("point", "text"),
  label       = "all", 
  col.ind    = "cos2",
  repel      = TRUE,
  gradient.cols = c("#DEEBF7", "#3182BD", "#08519C"),
  title      = "Pays sur le plan factoriel (Dim 1–2) – (sans outiliers)"
)

# Interprétation des axes F1 et F2 avec le plan factoriel
# Dim1 -> Gradient de développement / bonne gouvernance / modernité car forte corrélation avec stab_politique, controle_corr, participation_resp, taux_urbain, acces_elec, et aussi poulet_kg_par_hab
# Dim2 -> Gradient de volume de volaille dans l’offre alimentaire et d’autosuffisance car corrélée à nourriture_qte, taux_autosuff_volaille, import_qte, export_qte, pop_totale.

# Affiche les pays dans le plan factoriel formé par les axes 3 et 4
factoextra::fviz_pca_ind(
  res_pca_no3,
  axes        = c(3, 4),     
  geom = c("point", "text"),
  label       = "all", 
  col.ind    = "cos2",
  repel      = TRUE,
  gradient.cols = c("#DEEBF7", "#3182BD", "#08519C"),
  title      = "Pays sur le plan factoriel (Dim 3–4) – (sans outiliers)"
)

# Interprétation de l'axe F3 et F4 avec le plan factoriel
 # Dim3 : Oppose les pays importateurs dépendants aux pays plutôt exportateurs / excédentaires, avec une composante “consommation par habitant” (poulet_kg_par_hab)
 # Dim4 contraste fortement taux_indep_volaille (positif) et taux_autosuff_volaille / import_qte (négatif / positif). Donc F4 est un axe de dépendance structurelle aux importations.

# Affiche les pays dans le plan factoriel formé par les axes 1 et 5
factoextra::fviz_pca_ind(
  res_pca_no3,
  axes       = c(1, 5),
  geom = c("point", "text"),
  label       = "all", 
  col.ind    = "cos2",
  repel      = TRUE,
  gradient.cols = c("#DEEBF7", "#3182BD", "#08519C"),
  title      = "Pays sur le plan factoriel (Dim 1–5) – (sans outiliers)"
)

# Interprétation de l'axe F5 avec le plan factoriel
# Dim5 est fortement corrélé à pop_totale et, dans une moindre mesure, à pib_hab et aux indicateurs de gouvernance. Donc F5 est un axe de taille démographique / potentiel de volume, modulé par le niveau de richesse.


# Extrait uniquement les colonnes numériques spécifiées dans 'vars_pca' depuis le tableau 'df_acp_no3'
X_num_no3 <- df_acp_no3[ , vars_pca]   

# Scores des individus sur les k premiers axes
k <- 5                                                                           # Définit le nombre de dimensions à retenir 
scores_no3 <- res_pca_no3$ind$coord[ , 1:k]                                      # Extrait les coordonnées des pays sur les k premiers axes (F1 à F5)

# Calcule la matrice de corrélation entre les variables initiales (X_num) et les composantes principales (scores)
mat_corr_dim_var_no3 <- cor(
  X_num_no3,
  scores_no3,
  use = "pairwise.complete.obs"
)

# Attribue les noms des variables initiales comme noms de lignes de la matrice
rownames(mat_corr_dim_var_no3) <- vars_pca

# Génère les noms des colonnes : Dim1, Dim2, ..., Dim5
colnames(mat_corr_dim_var_no3) <- paste0("Dim", 1:k)

# Arrondit les valeurs de la matrice de corrélation à 3 décimales pour une lecture plus claire
round(mat_corr_dim_var_no3, 3)

# Exporte la matrice dans un fichier CSV avec les noms de lignes
write.csv(
  mat_corr_dim_var_no3,
  "data_clean/corr_dim_variable_no3.csv",
  row.names = TRUE
)

# Interpretation des axes avec la matrice de corrélation entre les variables initiales (X_num_no3) et les composantes principales (scores)
  # les corrélations les plus fortes sont : controle_corr : controle_corr : 0.862;  participation_resp : 0.781;  stab_politique : 0.753;  acces_elec : 0.683;  taux_urbain : 0.605;  poulet_kg_par_hab : 0.526;  export_qte : 0.450, import_qte : 0.357;  corrélations faibles ou négatives pour pop_totale (-0.17), pib_hab (-0.14).
     # Donc F1 est comme axe de “modernité politico-économique et d’intégration au marché”
  # nourriture_qte : 0.853;  import_qte : 0.523;  export_qte : 0.472;  pop_totale environ 0.323, pib_hab environ 0.323. Donc  F2 = taille de marché et niveau de disponibilité globale
  # export_qte : –0.625;  taux_autosuff_volaille : –0.530;  poulet_kg_par_hab : +0.518; Donc  F3 = mode d’approvisionnement / profil exportateur vs consommateur.
  # import_qte : 0.611;  taux_indep_volaille : 0.634;  taux_autosuff_volaille : –0.464; Donc  Dim4 = dépendance aux importations de volaille
  # pop_totale : 0.647;  pib_hab : 0.379;  contributions modérées des variables de gouvernance.Donc  Dim5 = gabarit démographique / économique résiduel 

# Coordonnées des pays dans le nouvel espace
coords_5d <- as.data.frame(res_pca_no3$ind$coord[, 1:5])

# On ajoute le nom du pays (et éventuellement le code court)
coords_5d$pays <- df_pca_no3$zone          

# Réorganiser : mettre le pays en première colonne
coords_5d <- coords_5d |>
  relocate(pays, .before = 1)

# Vérification rapide
head(coords_5d)

readr::write_csv(coords_5d,
                 "data_clean/coordonnees_pays_ACP5D_sans_outliers.csv")

# Interpretation 
# Dim1 (27,8 % de variance)  Axe « environnement institutionnel & niveau d’équipement » 
  # Fortement + : controle_corr, stab_politique, participation_resp, taux_urbain, acces_elec, poulet_kg_par_hab, export_qte.  Pays stables, bonne gouvernance, urbanisés, bien électrifiés, avec une consommation de volaille déjà élevée.
# Dim2 (17,6 %)  Axe « niveau de disponibilité interne de volaille & taille du marché » 
  # Fortement + : nourriture_qte, taux_autosuff_volaille, pop_total.  légèrement + : import_qte.  Pays avec forte disponibilité alimentaire de volaille (production interne) et/ou gros marché en volume.
# Dim3 (11,6 %)  Axe « dépendance / structure des échanges de volaille » 
  # Fortement + :  import_qte ,  taux_autosuff_volaille,  poulet_kg_par_hab  et  – :  export_qte , taux_autosuff_volaille . Pays plutôt importateurs nets, dépendants de l’extérieur, vs pays exportateurs/autosuffisants. 
# Dim4 (≈9–10 %)  Axe plus discret, mélange de : 
  # Fortement + :  pop_total, pib_hab (selon ta matrice),  signes plus faibles sur quelques variables de gouvernance.  Nuance « taille du marché × niveau de richesse » (petits pays riches / grands pays plus pauvres). 
# Dim5 (8,3 %)  Axe « démographie & gouvernance avancée » 
  # Fortement  + :  pop_total,  participation_resp ,  controle_corr  et  – :  poulet_kg_par_hab,  taux_autosuff_volaille.  Pays très peuplés avec institutions assez fortes mais encore un potentiel de montée en consommation/autosuffisance.

# Matrice numérique des coordonnées
X_clust <- coords_5d %>%                               
  select(starts_with("Dim.")) %>%                      # Sélectionne les colonnes correspondant aux dimensions ACP (Dim.1, Dim.2, …)
  as.matrix()                                          # Convertit le tableau en matrice numérique, nécessaire pour les algorithmes de clustering

# Affiche les dimensions de la matrice utilisée pour le clustering (n lignes × p dimensions ACP)
dim(X_clust)                                  

#  Méthodes hiérarchiques (Classification Ascendante Hiérarchique – CAH)

# Calcule la matrice des distances euclidiennes entre les pays selon leurs coordonnées ACP
d_clust <- dist(X_clust, method = "euclidean")

# Applique un clustering hiérarchique avec la méthode de Ward (minimisation de la variance intra-groupe)
hc_ward <- hclust(d_clust, method = "ward.D2")

# Affiche le dendrogramme avec les codes pays en abscisse
fviz_dend(
  hc_ward,
  k       = 5,                      # nombre de clusters choisi
  cex     = 0.5,                    # taille des labels
  horiz   = FALSE,                   # dendrogramme horizontal
  rect    = TRUE,                   # dessine des rectangles autour des clusters
  rect_fill   = TRUE,
  rect_border = "grey40",
  k_colors    = c("#E41A1C", "#4DAF4A", "#377EB8", "#984EA3", "#654321"),
  main   = "Dendrogramme (Ward) sur les 5 axes de l'ACP",
  xlab   = "Hauteur",
  ylab   = "Pays",
  label_cols = "black"
) +
  theme_minimal()

# Interpretation
# Cluster 1 (rouge) : grands émergents / marchés de volume.
# Cluster 2 (vert) : pays en développement, gouvernance plus faible, filière encore peu structurée.
# Cluster 3 (cyan) : pays très développés, bien gouvernés, filière volaille mature.
# Cluster 4 (violet) : pays « intermédiaires » (émergents avancés + pays développés de taille moyenne).
# Cluster 5 (marron) : Pays très développés, excellente gouvernance, institutions stables.

# 5 groupes par CAH
grp_cah <- cutree(hc_ward, k = 5)                   # Découpe le dendrogramme issu de la CAH (hc_ward) en 5 clusters et stocke l’appartenance de chaque pays

# data.frame avec clusters CAH + 5 dimensions
data_cah <- coords_5d %>%
  mutate(cluster_CAH = grp_cah)                     # Ajoute la variable d’appartenance aux clusters CAH dans la base factorielle à 5 dimensions


# barycentres (moyennes par cluster, dans l'espace 5D)
centres_cah <- data_cah %>%
  group_by(cluster_CAH) %>%                           # Regroupe les pays par cluster issu de la CAH
  summarise(
    Dim.1 = mean(Dim.1),                              # Calcule la moyenne des coordonnées sur l’axe 1
    Dim.2 = mean(Dim.2),                              # Calcule la moyenne sur l’axe 2
    Dim.3 = mean(Dim.3),                              # Calcule la moyenne sur l’axe 3
    Dim.4 = mean(Dim.4),                              # Calcule la moyenne sur l’axe 4
    Dim.5 = mean(Dim.5),                              # Calcule la moyenne sur l’axe 5
    .groups = "drop"                                  # Supprime le regroupement après le résumé
  )

centres_cah <- as.data.frame(centres_cah)             # Convertit l’objet en data frame pour manipulation
round(centres_cah, 3)                                 # Arrondit les coordonnées des centres à 3 décimales

tab_cah_5d <- as.data.frame(centres_cah) %>%
  rename(
    cluster = cluster_CAH,                            # Renomme la colonne des clusters
    Axe1 = Dim.1,                                     # Renomme les axes pour une lecture business-friendly
    Axe2 = Dim.2,
    Axe3 = Dim.3,
    Axe4 = Dim.4,
    Axe5 = Dim.5
  )

tab_cah_5d                                            # Affiche le tableau final des centres typologiques

# Interpretations
# Cluster 1 – pays en retard, plutôt autosuffisants : Dim1 très négatif, Dim2 négatif, Dim3 négatif et Pop et PIB par hab. moyennes (Dim4–Dim5 près de 0).
  # Pays « éloignés de la cible », mais sans grosse dépendance aux importations. Marchés à structurer avant d’investir
# Cluster 2 – pays intermédiaires, relativement développés mais volaille secondaire : Dim1 positif, Dim2 négatif, Dim3 légèrement positif
  # Pays déjà un peu développés, mais où la volaille est encore un segment opportunité/croissance plutôt qu’un pilier historique.
# Cluster 3 – grands émergents bien gouvernés, marchés cibles prioritaires : Dim1 positif, Dim2 très positif (3.65), Dim3 légèrement positif, Dim4 et Dim5 positifs : grandes populations, PIB/hab plus élevé.
  # Grands émergents où la volaille est déjà majeure, populations importantes, gouvernance correcte. Ce sont typiquement les marchés à fort potentiel volume + montée en gamme.
# Cluster 4 – pays en développement, filière volaille en croissance : Dim1 légèrement négatif, Dim2 positif, Dim3 proche de 0, Dim5 négatif
  # Pays en développement où la filière volaille progresse. Marchés de construction / partenariat local.
# Cluster 5 – petits pays très développés, exportateurs premium : Dim1 extrêmement positif (5.3), Dim2 fortement positif, Dim3 très négatif (-6.2), Dim5 négatif
  # Petits pays très développés, très bien gouvernés, avec filière volaille mature et exportatrice.

# Pays prioritaires selon CAH

# Priorité volume : cluster 3
pays_prioritaires_CAH_vol <- data_cah %>%
  filter(cluster_CAH == 3) %>%                                    # Sélectionne les pays du cluster 3 
  arrange(desc(Dim.2), desc(Dim.5)) %>%                           # Trie par importance sur les axes 2 et 5 
  select(pays, cluster_CAH, Dim.1:Dim.5)                          # Garde les infos utiles : pays, cluster, coordonnées ACP

pays_prioritaires_CAH_vol                                         

# Marchés premium : cluster 5
pays_prioritaires_CAH_premium <- data_cah %>%
  filter(cluster_CAH == 5) %>%                                    # Sélectionne les pays du cluster 5
  arrange(desc(Dim.1)) %>%                                        # Trie par importance sur l’axe 1
  select(pays, cluster_CAH, Dim.1:Dim.5)                          # Garde les infos utiles

pays_prioritaires_CAH_premium

# exporter 
write_xlsx(
  list(
    CAH_volume  = pays_prioritaires_CAH_vol,
    CAH_premium = pays_prioritaires_CAH_premium
  ),
  path = "data_clean/pays_prioritaires_CAH.xlsx"
)

# Premium / consolidation selon CAH + outliers
pays_premium_CAH <- data_cah %>% 
  filter(cluster_CAH %in% c(3, 5) |
           pays %in% c("Brésil", "États-Unis d'Amérique"))

#  Méthodes non hiérarchiques (Partitionnement K-means)

# Méthode silhouette : mesure la cohésion et la séparation des clusters
fviz_nbclust(X_clust, FUN = hcut, method = "silhouette") +
  ggtitle("Nb de clusters - silhouette")

# Interpretation
# On peut considérer que le coude de la courbe se situe autour de k = 4 (éventuellement 5). Au-delà, on complexifie la segmentation sans gagner énormément en compacité des groupes.
# La méthode de la silhouette montre un optimum net pour 4 clusters : c’est à k = 4 que les pays sont, en moyenne, le mieux séparés et le plus homogènes dans leur cluster.

# Définit le nombre de clusters à 4 
k_opt <- 4   

# Lance un clustering k-means avec 4 centres et 50 initialisations pour robustesse
set.seed(123)
km_res <- kmeans(X_clust, centers = k_opt, nstart = 50)

# Récupère l’appartenance de chaque pays aux clusters k-means
clusters_km <- km_res$cluster

# Affiche le nombre de clusters distincts
length(unique(clusters_km))  

# Affiche la répartition des individus par cluster
table(clusters_km)

# Convertit les centres des clusters en data frame
centres_km <- as.data.frame(km_res$centers)

# Ajoute un identifiant de cluster (1 à k_opt)
centres_km$clusters_km <- 1:k_opt

# Réorganise les colonnes pour mettre l’identifiant en premier
centres_km <- centres_km[, c("clusters_km","Dim.1","Dim.2","Dim.3","Dim.4","Dim.5")]

# Arrondit les coordonnées des centres à 3 décimales
round(centres_km, 3)

tab_km_5d <- as.data.frame(centres_km) %>%
  rename(                                               # Renomme les colonnes pour une lecture business-friendly
    cluster = clusters_km,
    Axe1 = Dim.1,
    Axe2 = Dim.2,
    Axe3 = Dim.3,
    Axe4 = Dim.4,
    Axe5 = Dim.5
  )

tab_km_5d                                               # Affiche le tableau final des centres typologiques k-means

# Interpretations
# Cluster 1 (K-means) – pays en développement, forte demande mais importateurs : Dim1 négatif, Dim2 très positif (2.8), Dim3 positif (0.74
  # marchés de volume, très demandeurs en volaille mais dépendants des importations → opportunité pour l’export / partenariat de production locale.
# Cluster 2 (K-means) – pays en retard, volaille secondaire : Dim1 très négatif, Dim2 légèrement négatif, Dim3 légèrement négatif
  # marchés peu mûrs, faible priorité à court terme.
# Cluster 3 (K-means) – pays développés exportateurs : Dim1 très positif (3.64), Dim2 très positif (2.6), Dim3 négatif (-2.34), Dim4–Dim5 positifs
  # pays clés à la fois pour la consommation intérieure premium et comme plateformes d’export.
# Cluster 4 (K-means) – pays développés mais volaille encore à développer : Dim1 positif, Dim2 négatif, Dim3 positif
  # pays déjà développés mais où la volaille n’a pas encore atteint son plein potentiel → marchés intéressants pour une stratégie de développement de consommation (éducation / marketing / montée en gamme).

# Pays prioritaires selon K-means

# Ajoute la variable d’appartenance aux clusters k-means dans la base factorielle à 5 dimensions
data_kmean <- coords_5d %>%
  mutate(
    clusters_km  = km_res$cluster   
  )

# Marchés importateurs à fort potentiel (cluster 1)
pays_prioritaires_KM_import <- data_kmean %>%
  filter(clusters_km == 1) %>%
  arrange(desc(Dim.2), desc(Dim.3)) %>%   # volaille importante + importations élevées
  select(pays, clusters_km, Dim.1:Dim.5)

pays_prioritaires_KM_import

# Marchés développés / premium (cluster 3)
pays_prioritaires_KM_premium <- data_kmean %>%
  filter(clusters_km == 3) %>%
  arrange(desc(Dim.1), desc(Dim.2)) %>%
  select(pays, clusters_km, Dim.1:Dim.5)

pays_prioritaires_KM_premium

# Exporter
write_xlsx(
    list(
      KM_premium = pays_prioritaires_KM_premium,
      KM_import  = pays_prioritaires_KM_import  # si tu en crées un pour les importateurs
    ),
    path = "data_clean/pays_prioritaires_Kmeans.xlsx"
  )

# Premium / consolidation selon K-means + outliers
pays_premium_km <- data_kmean %>% 
  filter(clusters_km == 3 |
           pays %in% c("Brésil", "États-Unis d'Amérique"))

# Marchés de volume (ex : Chine continentale)
pays_volume <- data_kmean %>% 
  filter(clusters_km == 1 |
           pays %in% c("Chine, continentale"))

# Hub / niche (ex : Hong-Kong)
pays_hub <- data_kmean %>% 
  filter(pays %in% c("Chine - RAS de Hong-Kong"))

# Exporter 
write_xlsx(list(
  Premium_CAH = pays_premium_CAH,
  Premium_Kmeans = pays_premium_km,
  Volume = pays_volume,
  Hub = pays_hub
), "data_clean/pays_prioritaires.xlsx")
