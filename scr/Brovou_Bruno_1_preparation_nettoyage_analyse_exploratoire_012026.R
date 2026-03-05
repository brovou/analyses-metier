# ============================
# 01_exploration.R
# Projet : La poule qui chante - Expansion internationale
# Objectif : Charger, nettoyer et explorer les données FAO (population + volaille)
# ============================

# Charger les librairies 
library(tidyverse)                     # dplyr, ggplot2, readr, etc.
library(janitor)                       # pour nettoyer les noms de colonnes
library(ggplot2)                       # Visualisation avancée avec la grammaire des graphiques
library(dplyr)                         # Manipulation de données (filtrage, regroupement, transformation…)
library(tidyr)                        # Permet de passer d’un format large à long (pivot_longer), de séparer ou fusionner des colonnes, et de gérer les valeurs manquantes
library(tibble)

# Définir le chemin de base 
# Les fichiers bruts sont stockés dans data_raw/

path_pop  <- "data_raw/Population_2000_2018.csv"
path_disp <- "data_raw/DisponibiliteAlimentaire_2017.csv"
path_gdp  <- "data_raw/PIB_par_habitant_basse_mondiale.csv" 
path_urb  <- "data_raw/Taux_urbanisation_basse_mondiale.csv" 
path_stab  <- "data_raw/Stabilite_politique_basse_mondiale.csv"
path_reg  <- "data_raw/Qualite_regulation_base_mondiale.csv"
path_corr <- "data_raw/Controle_corruption_base_mondiale.csv" 
path_elec <- "data_raw/Acces_electricite_base_mondiale.csv" 
path_part  <- "data_raw/participation_responsabilite_base_mondiale.csv" 
path_eff   <- "data_raw/Efficacite_gouvernementale_base_mondiale.csv" 

# Importer les données FAO et Banque mondiale 

pop_raw <- read_csv(path_pop)
disp_raw <- read_csv(path_disp)
gdp_raw <- read_csv(path_gdp)
urb_raw <- read_csv(path_urb)
stab_raw <- read_csv(path_stab)
reg_raw <- read_csv(path_reg)
corr_raw <- read_csv(path_corr)
elec_raw <- read_csv(path_elec)
part_raw <- read_csv(path_part)
eff_raw <- read_csv(path_eff)

# Nettoyer les noms de colonnes 
# On enlève les accents, espaces, majuscules pour avoir des noms simples

pop <- pop_raw %>% clean_names()

disp <- disp_raw %>% clean_names()


gdp <- read_delim(
  path_gdp,                              # Chemin du fichier PIB
  delim = ";",                           # Délimiteur utilisé dans le fichier
  locale = locale(decimal_mark = "."),   # Format des décimales (point)
  show_col_types = FALSE                 # Ne pas afficher les types de colonnes
) %>%  clean_names()                     # Standardisation des noms de colonnes


urb <- read_delim(
  path_urb,                              # Chemin du fichier taux Urbanisation
  delim = ";",
  locale = locale(decimal_mark = "."),
  show_col_types = FALSE
) %>%
  clean_names()

stab <- read_delim(
  path_stab,                            # Chemin du fichier stabilité politique
  delim = ";",
  locale = locale(decimal_mark = "."),
  show_col_types = FALSE
) %>%
  clean_names()

reg <- read_delim(
  path_reg,                             # Chemin du fichier qualité de regulation
  delim = ";",
  locale = locale(decimal_mark = "."),
  show_col_types = FALSE
) %>%
  clean_names()

corr <- read_delim(
  path_corr,                            # Chemin du fichier controle corruption
  delim = ";",
  locale = locale(decimal_mark = "."),
  show_col_types = FALSE
) %>%
  clean_names()

elec <- read_delim(
  path_elec,                            # Chemin du fichier accès électrique
  delim = ";",
  locale = locale(decimal_mark = "."),
  show_col_types = FALSE
) %>%
  clean_names()                                                                                                                                                                                                                                                                                                                                                                                                                                      

part <- read_delim(
  path_part,                            # Chemin du fichier participation responsabilité
  delim = ";",
  locale = locale(decimal_mark = "."),
  show_col_types = FALSE
) %>%
  clean_names()

eff <- read_delim(
  path_eff,                             # Chemin du fichier efficacité gouvernementale
  delim = ";",
  locale = locale(decimal_mark = "."),
  show_col_types = FALSE
) %>%
  clean_names()


# Affichage des noms de colonnes et Aperçu de la structure du jeu de données

names(pop)
glimpse(pop)

names(disp)
glimpse(disp)

names(gdp)
glimpse(gdp)

names(stab)
glimpse(stab)

names(reg)
glimpse(reg)

names(corr)
glimpse(corr)

names(elec)
glimpse(elec)

names(part)
glimpse(part)

names(eff)
glimpse(eff)


# Filtrage du jeu de données population  et disponibilité alimentaire pour la viande de volailles

pop_2017 <- pop %>%
  filter(
    annee == 2017,                                        # Sélection de l’année 2017
    element == "Population totale"                        # Sélection de la population totale
  ) %>%
  select(                                                 # Sélection des variables pertinentes
    code_zone,
    zone,
    annee,
    unite,
    valeur
  ) %>%
  rename(pop_1000 = valeur) %>%                           # Renommage de la variable valeur en pop_1000 (population en milliers)
  mutate(
    code_zone = as.character(code_zone),                  # Conversion explicite de la variable code_zone en type caractère
    pop_1000  = as.numeric(pop_1000)
  )
                              

disp_poulet <- disp %>%                                             # Création du jeu de données disp_poulet à partir de la base disp
  mutate(code_zone = as.character(code_zone)) %>%                   # Conversion du code_zone en caractère pour faciliter les jointures ou regroupements
  dplyr::filter(                                                    # Filtrage des données
    domaine == "Nouveaux Bilans Alimentaire",                       # Domaine ciblé
    produit == "Viande de Volailles",                               # Produit ciblé
    annee   == 2017,                                                # Année d’analyse
    element %in% c(                                                 # Éléments sélectionnés
      "Disponibilité alimentaire en quantité (kg/personne/an)",     # Indicateur de consommation
      "Importations - Quantité",                                    # Volume importé
      "Exportations - Quantité",                                    # Volume exporté
      "Production",                                                 # Volume produit
      "Nourriture"                                                  # Volume destiné à l’alimentation
    )
  ) %>%
  select(                                                           # Sélection des colonnes utiles pour l’analyse
    code_zone,
    zone,
    code_produit,
    produit,
    annee,
    element,
    unite,
    valeur
  ) %>%
  pivot_wider(                                                      # Restructuration du tableau
    names_from  = element,                                          # Les éléments deviennent des noms de colonnes
    values_from = valeur                                            # Les valeurs associées deviennent le contenu des colonnes
  ) %>%
  rename(                                                           # Renommage des colonnes pour plus de clarté et de lisibilité
    poulet_kg_par_hab = `Disponibilité alimentaire en quantité (kg/personne/an)`,  # Consommation par habitant
    import_qte        = `Importations - Quantité`,                                 # Volume importé
    export_qte        = `Exportations - Quantité`,                                 # Volume exporté
    prod_qte          = Production,                                                # Volume produit
    nourriture_qte    = Nourriture                                                 # Volume destiné à l’alimentation
  )


disp_poulet <- disp_poulet %>%
  mutate(
    dispo_volaille = prod_qte + import_qte - export_qte,              # Disponibilite de marche (meme unite que prod_qte/import_qte/export_qte)
    
    taux_autosuff_volaille = if_else(                                 # Taux d'autosuffisance en volaille
      dispo_volaille > 0,
      prod_qte / dispo_volaille,
      NA_real_
    ),
    
    taux_indep_volaille = if_else(                                    # Taux d'independance (part des importations dans la dispo)
      dispo_volaille > 0,
      import_qte / dispo_volaille,
      NA_real_
    )
  )


# Normalisation des tables 2017 

# PIB / habitant
gdp_2017 <- gdp %>%                                   # Transformation du jeu de données PIB 
  mutate(
    code_zone        = as.character(code_zone),       # Conversion de la variable code_zone en caractère (utile pour les jointures)
    pib_par_habitant = as.numeric(pib_par_habitant)   # Conversion de pib_par_habitant en numérique (utile pour calculs et visualisations)
  ) %>%
  transmute(                                          # Création d’un nouveau jeu de données avec seulement les variables utiles
    code_zone,                                        # Zone géographique (pays, région, etc.)
    pib_hab = pib_par_habitant                        # Renommage en pib_hab pour simplifier les appels
  )

# Taux d'urbanisation
urb_2017 <- urb %>%                                    # Transformation du jeu de données urbanisation 
  mutate(
    code_zone        = as.character(code_zone),
    taux_urbanisation = as.numeric(taux_urbanisation)
  ) %>%
  transmute(
    code_zone,
    taux_urbain = taux_urbanisation
  )

# Stabilité politique
stab_2017 <- stab %>%                                        # Transformation du jeu de données stabilité politique 
  mutate(
    code_zone            = as.character(code_zone),
    stabilite_politique  = as.numeric(stabilite_politique)
  ) %>%
  transmute(
    code_zone,
    stab_politique = stabilite_politique
  )

# Qualité de la régulation
reg_2017 <- reg %>%                                         # Transformation du jeu de données qualité regulation 
  mutate(
    code_zone          = as.character(code_zone),
    qualite_regulation = as.numeric(qualite_regulation)
  ) %>%
  transmute(
    code_zone,
    qualite_reg = qualite_regulation
  )

# Contrôle de la corruption
corr_2017 <- corr %>%                                      # Transformation du jeu de données controle corruption  
  mutate(
    code_zone               = as.character(code_zone),
    controle_de_la_corruption = as.numeric(controle_de_la_corruption)
  ) %>%
  transmute(
    code_zone,
    controle_corr = controle_de_la_corruption
  )

# Accès à l'électricité
elec_2017 <- elec %>%                                     # Transformation du jeu de données accès électricité 
  mutate(
    code_zone        = as.character(code_zone),
    acces_a_electricite = as.numeric(acces_a_electricite)
  ) %>%
  transmute(
    code_zone,
    acces_elec = acces_a_electricite
  )

# Participation et responsabilité
part_2017 <- part %>%                                     # Transformation du jeu de données participation responsabilité 
  mutate(
    code_zone                     = as.character(code_zone),
    participation_et_responsabilite = as.numeric(participation_et_responsabilite)
  ) %>%
  transmute(
    code_zone,
    participation_resp = participation_et_responsabilite
  )

# Efficacité gouvernementale
eff_2017 <- eff %>%                                      # Transformation du jeu de données efficacité gouvernementale 
  mutate(
    code_zone = as.character(code_zone),
    efficacite_gouvernementale = as.numeric(efficacite_gouvernementale)
  ) %>%
  select(code_zone, efficacite_gouvernementale) %>%
  rename(efficacite_gouv = efficacite_gouvernementale)


# # Création du jeu de données base_2017 en croisant consommation de poulet et population

base_2017 <- disp_poulet %>%
  inner_join(pop_2017 %>% select(code_zone, pop_1000),  # Sélection de code_zone et population en milliers
             by = "code_zone") %>%                      # Clé de jointure : code_zon
  mutate(pop_totale = pop_1000 * 1000)                  # Conversion de milliers en unités

write_csv(base_2017, "data_clean/pays_base_2017.csv")   # Sauvegarder le jeu de données nettoyé 

# Fusionner PIB/hab avec base_2017 sur code_zone

base_2017_pib <- disp_poulet %>%
  inner_join(pop_2017 %>% select(code_zone, pop_1000),        # Ajoute la population
             by = "code_zone") %>%
  left_join(gdp_2017, by = "code_zone") %>%                   # Ajoute le PIB / hab
  mutate(pop_totale = pop_1000 * 1000)

write_csv(base_2017_pib, "data_clean/pays_base_2017_pib.csv") # Sauvegarder le jeu de données nettoyé 

# Fusionner urbanisation avec base_2017 sur code_zone
base_2017_urbain <- disp_poulet %>%
  inner_join(pop_2017 %>%  mutate(code_zone = as.character(code_zone)) %>%
               select(code_zone, pop_1000),
             by = "code_zone") %>%
  left_join(gdp_2017, by = "code_zone") %>%
  left_join(urb_2017, by = "code_zone") %>%                            # Ajout du taux d'urbanisation
  mutate(pop_totale = pop_1000 * 1000)

write_csv(base_2017_urbain, "data_clean/pays_base_2017_pib_urb.csv")   # Sauvegarder le jeu de données nettoyé 

# Fusionner stabilité politique avec base_2017 sur code_zone
base_2017_stab <- disp_poulet %>%
  inner_join(pop_2017 %>%  mutate(code_zone = as.character(code_zone)) %>%
               select(code_zone, pop_1000),
             by = "code_zone") %>%
  left_join(gdp_2017, by = "code_zone") %>%
  left_join(urb_2017, by = "code_zone") %>%
  left_join(stab_2017, by = "code_zone") %>%                              # Ajout de la stabilité politique
  mutate(pop_totale = pop_1000 * 1000)

write_csv(base_2017_stab, "data_clean/pays_base_2017_pib_urb_stab.csv")   # Sauvegarder le jeu de données nettoyé 

# Fusionner qualité de la régulation avec base_2017 sur code_zone
base_2017_reg <- disp_poulet %>%
  inner_join(pop_2017 %>%  mutate(code_zone = as.character(code_zone)) %>%
               select(code_zone, pop_1000),
             by = "code_zone") %>%
  left_join(gdp_2017, by = "code_zone") %>%
  left_join(urb_2017, by = "code_zone") %>%
  left_join(stab_2017, by = "code_zone") %>%
  left_join(reg_2017, by = "code_zone") %>%                                 # Ajout de la qualité de la régulation
  mutate(pop_totale = pop_1000 * 1000)

write_csv(base_2017_reg, "data_clean/pays_base_2017_pib_urb_stab_reg.csv")  # Sauvegarder le jeu de données nettoyé

# Fusionner contrôle de la corruption avec base_2017 sur code_zone
base_2017_corr <- disp_poulet %>%
  # Population
  inner_join(pop_2017 %>%  mutate(code_zone = as.character(code_zone)) %>%
               select(code_zone, pop_1000),
             by = "code_zone") %>%
  left_join(gdp_2017, by = "code_zone") %>%
  left_join(urb_2017, by = "code_zone") %>%
  left_join(stab_2017, by = "code_zone") %>%
  left_join(reg_2017, by = "code_zone") %>%
  left_join(corr_2017, by = "code_zone") %>%                                      # Contrôle de la corruption
  mutate(pop_totale = pop_1000 * 1000)

write_csv(base_2017_corr, "data_clean/pays_base_2017_pib_urb_stab_reg_corr.csv")  # Sauvegarder le jeu de données nettoyé

# Fusionner accès électricité avec base_2017 sur code_zone
base_2017_elec <- disp_poulet %>%
  inner_join(pop_2017 %>%  mutate(code_zone = as.character(code_zone)) %>%
               select(code_zone, pop_1000),
             by = "code_zone") %>%
  left_join(gdp_2017, by = "code_zone") %>%
  left_join(urb_2017, by = "code_zone") %>%
  left_join(stab_2017, by = "code_zone") %>%
  left_join(reg_2017, by = "code_zone") %>%
  left_join(corr_2017, by = "code_zone") %>%
  left_join(elec_2017, by = "code_zone") %>%                                      # Accès à l'électricité
  mutate(pop_totale = pop_1000 * 1000)

write_csv(base_2017_elec, "data_clean/pays_base_2017_pestel_plus_elec.csv")       # Sauvegarder le jeu de données nettoyé

# Fusionner Participation et responsabilité avec base_2017 sur code_zone
base_2017_part <- disp_poulet %>%
  inner_join(pop_2017 %>%  mutate(code_zone = as.character(code_zone)) %>%
               select(code_zone, pop_1000),
             by = "code_zone") %>%
  left_join(gdp_2017, by = "code_zone") %>%
  left_join(urb_2017, by = "code_zone") %>%
  left_join(stab_2017, by = "code_zone") %>%
  left_join(reg_2017, by = "code_zone") %>%
  left_join(corr_2017, by = "code_zone") %>%
  left_join(elec_2017, by = "code_zone") %>%
  left_join(part_2017, by = "code_zone") %>%                                      # Participation et responsabilité
  mutate(pop_totale = pop_1000 * 1000)

write_csv(base_2017_part, "data_clean/pays_base_2017_pestel_plus_part.csv")       # Sauvegarder le jeu de données nettoyé

# Fusionner Efficacité gouvernementale avec base_2017 sur code_zone

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

df_pays <- disp_poulet %>%   # adapte le nom si besoin
  group_by(code_zone, zone) %>%         # 1 groupe = 1 pays
  summarise(
    across(where(is.numeric), first_non_na),
    .groups = "drop"
  )

df_pays <- df_pays %>%
  left_join(pop_2017 %>% select(code_zone, pop_1000), by = "code_zone") %>%
  mutate(
    pop_totale = pop_1000 * 1000      # transformation "1000 personnes" -> personnes
  )

base_2017_eff <- df_pays %>%
  left_join(gdp_2017, by = "code_zone") %>%
  left_join(urb_2017, by = "code_zone") %>%
  left_join(stab_2017, by = "code_zone") %>%
  left_join(reg_2017, by = "code_zone") %>%
  left_join(corr_2017, by = "code_zone") %>%
  left_join(elec_2017, by = "code_zone") %>%
  left_join(part_2017, by = "code_zone") %>%
  left_join(eff_2017, by = "code_zone")                                          # Efficacité gouvernementale

write_csv(
  base_2017_eff,
  "data_clean/pays_base_2017_pestel_plus_eff.csv"
)        # Sauvegarder le jeu de données nettoyé

# Sélectionner uniquement les variables numériques utiles
df_correl <- base_2017_eff %>%
  select(
    poulet_kg_par_hab,
    pop_totale,
    pib_hab,
    import_qte,
    export_qte,     
    prod_qte,  
    nourriture_qte, 
    taux_urbain,
    acces_elec,
    stab_politique,
    qualite_reg,
    controle_corr,
    participation_resp,
    efficacite_gouv,
    taux_autosuff_volaille,
    taux_indep_volaille
  )

# Calcul de la matrice de corrélation (Pearson)
mat_correl <- cor(
  df_correl,
  use    = "pairwise.complete.obs",  # gère les NA
  method = "pearson"
)

mat_correl    

mat_correl_df <- mat_correl %>%
  as.data.frame() %>%                                # Convertit la matrice de corrélation en data frame
  rownames_to_column(var = "variable")               # Ajoute les noms de lignes comme colonne "variable"

readr::write_csv(
  mat_correl_df,
  "data_clean/matrice-correlation-pays-volailles.csv"
)

# Exploration rapide -----------------------------
# Quelques statistiques de base pour comprendre les données

# Nombre de pays
n_distinct(base_2017$zone)
n_distinct(base_2017_pib$zone)
n_distinct(base_2017_urbain$zone)
n_distinct(base_2017_stab$zone)
n_distinct(base_2017_reg$zone)
n_distinct(base_2017_corr$zone)
n_distinct(base_2017_elec$zone)
n_distinct(base_2017_part$zone)
n_distinct(base_2017_eff$zone)

# Aperçu des premières lignes
head(base_2017)
head(base_2017_pib)
head(base_2017_urbain)
head(base_2017_stab)
head(base_2017_reg)
head(base_2017_corr)
head(base_2017_elec)
head(base_2017_part)
head(base_2017_eff)

# Résumé statistique des variables numériques
summary(select(base_2017, poulet_kg_par_hab, pop_totale))

summary(base_2017_pib$pib_hab)

names(base_2017_urbain)
summary(base_2017_urbain$taux_urbain)

names(base_2017_stab)
summary(base_2017_stab$stab_politique)

names(base_2017_reg)
summary(base_2017_reg$qualite_reg)

names(base_2017_corr)
summary(base_2017_corr$controle_corr)

names(base_2017_elec)
summary(base_2017_elec$acces_elec)

names(base_2017_part)
summary(base_2017_part$participation_resp)

names(base_2017_eff)
glimpse(eff_2017)
summary(base_2017_eff$efficacite_gouv)
