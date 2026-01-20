

# 1. IMPORTATION DIRECTE DES DONNÉES
cat(" ÉTAPE 1: IMPORTATION DES DONNÉES...\n")

if (!file.exists("worldometer_coronavirus_daily_data.csv")) {
  stop(" ERREUR: Le fichier 'worldometer_coronavirus_daily_data.csv' est introuvable!\n",
       "   Placez-le dans le même dossier que ce script.")
}

donnees <- read.csv("worldometer_coronavirus_daily_data.csv", stringsAsFactors = FALSE)

cat(" Données importées avec succès!\n")
cat("    Dimensions:", nrow(donnees), "lignes x", ncol(donnees), "colonnes\n")
cat("    Pays:", paste(unique(donnees$country), collapse = ", "), "\n\n")

# 2. ANALYSE EXPLORATOIRE COMPLÈTE
cat(" ÉTAPE 2: ANALYSE EXPLORATOIRE DES DONNÉES...\n")

# 2.1 Valeurs manquantes
cat("   2.1. Valeurs manquantes par colonne:\n")
valeurs_manquantes <- colSums(is.na(donnees))
for(col in names(valeurs_manquantes)) {
  if(valeurs_manquantes[col] > 0) {
    cat("      ", col, ":", valeurs_manquantes[col], "NA (", 
        round(valeurs_manquantes[col]/nrow(donnees)*100, 1), "%)\n")
  } else {
    cat("      ", col, ": Aucune valeur manquante\n")
  }
}

# 2.2 Doublons
cat("\n   2.2. Analyse des doublons:\n")
doublons <- sum(duplicated(donnees))
cat("      ", ifelse(doublons > 0, "", ""), 
    "Nombre de doublons:", doublons, "\n")
if(doublons > 0) {
  donnees <- donnees[!duplicated(donnees), ]
  cat("       Doublons supprimés\n")
}

# 3. NETTOYAGE BASIQUE
cat("\n ÉTAPE 3: NETTOYAGE ET PRÉPARATION DES DONNÉES...\n")

# Conversion de la date
donnees$date <- as.Date(donnees$date)

# Remplacer les NA par 0 dans les colonnes numériques
colonnes_numeriques <- c("cumulative_total_cases", "daily_new_cases", "active_cases", 
                         "cumulative_total_deaths", "daily_new_deaths", "total_tests")
for(col in colonnes_numeriques) {
  if(col %in% names(donnees)) {
    donnees[[col]][is.na(donnees[[col]])] <- 0
    cat("      ", col, ": NA remplacés par 0\n")
  }
}

# Ajouter une colonne de guérisons estimées
donnees$estimated_recovered <- donnees$cumulative_total_cases - 
  donnees$active_cases - donnees$cumulative_total_deaths
donnees$estimated_recovered[donnees$estimated_recovered < 0] <- 0

cat(" Nettoyage terminé\n\n")

# 4. INSTALLATION DES PACKAGES NÉCESSAIRES
cat(" ÉTAPE 4: VÉRIFICATION DES PACKAGES...\n")

packages_necessaires <- c("ggplot2", "dplyr", "lubridate", "scales", "gridExtra", "zoo")

for (pkg in packages_necessaires) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("   Installation de", pkg, "...\n"))
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}

cat(" Packages chargés\n\n")

# 5. PAYS À ANALYSER
pays_a_analyser <- c("Afghanistan", "Albania", "Algeria")






# 6. ANALYSE PAR PAYS AVEC VISUALISATION DIRECTE
cat(" ÉTAPE 6: ANALYSE PAR PAYS AVEC VISUALISATIONS DIRECTES...\n")
cat("   (Les graphiques s'affichent dans le Viewer de RStudio)\n\n")

for (pays in pays_a_analyser) {
  cat("\n    Analyse de:", pays, "\n")
  cat("   " , rep("=", 50), "\n", sep = "")
  
  # Filtrer les données du pays
  donnees_pays <- donnees[donnees$country == pays, ]
  donnees_pays <- donnees_pays[order(donnees_pays$date), ]
  
  # Calculer la moyenne mobile 7 jours
  donnees_pays$ma7_cas <- rollmean(donnees_pays$daily_new_cases, 7, fill = NA, align = "right")
  donnees_pays$ma7_deces <- rollmean(donnees_pays$daily_new_deaths, 7, fill = NA, align = "right")
  
  # Statistiques du pays
  cas_totaux <- max(donnees_pays$cumulative_total_cases, na.rm = TRUE)
  deces_totaux <- max(donnees_pays$cumulative_total_deaths, na.rm = TRUE)
  pic_cas <- max(donnees_pays$daily_new_cases, na.rm = TRUE)
  pic_deces <- max(donnees_pays$daily_new_deaths, na.rm = TRUE)
  taux_mortalite <- round(deces_totaux / cas_totaux * 100, 2)
  
  cat("      . Cas totaux:", format(cas_totaux, big.mark = " "), "\n")
  cat("      . Décès totaux:", format(deces_totaux, big.mark = " "), "\n")
  cat("      . Taux mortalité:", taux_mortalite, "%\n")
  cat("      . Période:", min(donnees_pays$date), "à", max(donnees_pays$date), "\n")
  
  
  # AFFICHAGE DIRECT DES GRAPHIQUES DANS RSTUDIO
  
  
  # 6.1 DIAGRAMME D'ÉVOLUTION DES CAS
  cat("\n       Affichage du graphique d'évolution des cas...\n")
  
  # Calculer la moyenne mobile 7 jours seulement si assez de données
  if (nrow(donnees_pays) >= 7) {
    donnees_pays$ma7_cas <- rollmean(donnees_pays$daily_new_cases, 7, fill = NA, align = "right")
    
    p1 <- ggplot(donnees_pays, aes(x = date)) +
      geom_area(aes(y = daily_new_cases), fill = "#bbdefb", alpha = 0.4) +
      geom_line(aes(y = ma7_cas), color = "#1a237e", size = 1.2, na.rm = TRUE) +
      labs(
        title = paste("Évolution des nouveaux cas -", pays),
        subtitle = "Zone bleue: nouveaux cas quotidiens | Ligne bleue: moyenne mobile 7 jours",
        x = "Date",
        y = "Nouveaux cas"
      ) +
      scale_x_date(labels = date_format("%b %Y")) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90", size = 0.2)
      )
  } else {
    # Version sans moyenne mobile si pas assez de données
    p1 <- ggplot(donnees_pays, aes(x = date, y = daily_new_cases)) +
      geom_area(fill = "#bbdefb", alpha = 0.4) +
      labs(
        title = paste("Évolution des nouveaux cas -", pays),
        subtitle = "Nouveaux cas quotidiens (données insuffisantes pour moyenne mobile)",
        x = "Date",
        y = "Nouveaux cas"
      ) +
      scale_x_date(labels = date_format("%b %Y")) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90", size = 0.2)
      )
  }
  print(p1)  
  
  # 6.2 HISTOGRAMME DES DISTRIBUTIONS
  cat("       Affichage de l'histogramme de distribution...\n")
  p2 <- ggplot(donnees_pays, aes(x = daily_new_cases)) +
    geom_histogram(fill = "#1a237e", alpha = 0.7, bins = 30) +
    geom_vline(aes(xintercept = mean(daily_new_cases, na.rm = TRUE)), 
               color = "#d32f2f", linetype = "dashed", size = 1) +
    annotate("text", x = mean(donnees_pays$daily_new_cases, na.rm = TRUE), 
             y = 5, label = paste("Moyenne:", round(mean(donnees_pays$daily_new_cases, na.rm = TRUE))), 
             color = "#d32f2f", hjust = -0.1) +
    labs(
      title = paste("Distribution des nouveaux cas -", pays),
      subtitle = "Histogramme des cas quotidiens",
      x = "Nouveaux cas quotidiens",
      y = "Fréquence"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      panel.grid.minor = element_blank()
    )
  print(p2)  
  
  # 6.3 DIAGRAMME D'ÉVOLUTION DES DÉCÈS
  cat("       Affichage du graphique d'évolution des décès...\n")
  
  # Vérifier s'il y a des données de décès
  if (sum(donnees_pays$daily_new_deaths, na.rm = TRUE) > 0) {
    # Calculer la moyenne mobile 7 jours pour les décès si assez de données
    if (nrow(donnees_pays) >= 7) {
      donnees_pays$ma7_deces <- rollmean(donnees_pays$daily_new_deaths, 7, fill = NA, align = "right")
      
      p3 <- ggplot(donnees_pays, aes(x = date)) +
        geom_col(aes(y = daily_new_deaths), fill = "#ffcdd2", alpha = 0.6) +
        geom_line(aes(y = ma7_deces), color = "#d32f2f", size = 1.2, na.rm = TRUE) +
        labs(
          title = paste("Évolution des décès -", pays),
          subtitle = "Barres roses: nouveaux décès | Ligne rouge: moyenne mobile 7 jours",
          x = "Date",
          y = "Nombre de décès"
        ) +
        scale_x_date(labels = date_format("%b %Y")) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#d32f2f"),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10)
        )
    } else {
      # Version sans moyenne mobile
      p3 <- ggplot(donnees_pays, aes(x = date, y = daily_new_deaths)) +
        geom_col(fill = "#ffcdd2", alpha = 0.6) +
        labs(
          title = paste("Évolution des décès -", pays),
          subtitle = "Nouveaux décès quotidiens",
          x = "Date",
          y = "Nombre de décès"
        ) +
        scale_x_date(labels = date_format("%b %Y")) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#d32f2f"),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10)
        )
    }
    print(p3)  
  } else {
    cat("        Pas de données de décès pour", pays, "\n")
  }
  
  # 6.4 DIAGRAMME CIRCULAIRE - RÉPARTITION DES CAS
  cat("       Affichage du diagramme circulaire...\n")
  if (cas_totaux > 0) {
    data_pie <- data.frame(
      categorie = c("Actifs", "Décès", "Guéris estimés"),
      valeur = c(
        mean(donnees_pays$active_cases, na.rm = TRUE),
        deces_totaux,
        mean(donnees_pays$estimated_recovered, na.rm = TRUE)
      )
    )
    
    # Calculer les pourcentages
    data_pie$pourcentage <- round(data_pie$valeur / sum(data_pie$valeur) * 100, 1)
    data_pie$label <- paste0(data_pie$categorie, "\n", data_pie$pourcentage, "%")
    
    p4 <- ggplot(data_pie, aes(x = "", y = valeur, fill = categorie)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("Actifs" = "#FFA726", 
                                   "Décès" = "#EF5350", 
                                   "Guéris estimés" = "#66BB6A")) +
      labs(
        title = paste("Répartition des cas -", pays),
        subtitle = "Distribution actifs/décès/guéris",
        fill = "Catégorie"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = "right",
        legend.title = element_text(face = "bold")
      )
    print(p4)  
  }
  
  cat("      Analyse terminée pour", pays, "\n")
}


# 7. ANALYSE COMPARATIVE AVEC VISUALISATION DIRECTE

cat("\n ÉTAPE 7: ANALYSE COMPARATIVE ENTRE PAYS...\n")
cat("   (Les histogrammes en bandes s'affichent ci-dessous)\n")

donnees_comparaison <- donnees[donnees$country %in% pays_a_analyser, ]

# 7.1 HISTOGRAMMES EN BANDES - COMPARAISON DES CAS
cat("\n   7.1. Histogrammes en bandes - Comparaison des cas...\n")

# Préparer les données pour l'histogramme en bandes
donnees_bandes <- donnees_comparaison %>%
  group_by(country) %>%
  summarise(
    cas_totaux = max(cumulative_total_cases, na.rm = TRUE),
    deces_totaux = max(cumulative_total_deaths, na.rm = TRUE),
    cas_moyens = mean(daily_new_cases, na.rm = TRUE),
    deces_moyens = mean(daily_new_deaths, na.rm = TRUE),
    pic_cas = max(daily_new_cases, na.rm = TRUE),
    pic_deces = max(daily_new_deaths, na.rm = TRUE)
  ) %>%
  arrange(desc(cas_totaux))

cat("\n       Tableau comparatif des 3 pays:\n")
print(donnees_bandes)

# Graphique 1: Histogramme en bandes - Cas totaux
cat("\n       Affichage: Histogramme en bandes - Cas totaux\n")
p_bandes1 <- ggplot(donnees_bandes, aes(x = reorder(country, -cas_totaux), y = cas_totaux, fill = country)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = format(cas_totaux, big.mark = " ")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Afghanistan" = "#1a237e", 
                               "Albania" = "#d32f2f", 
                               "Algeria" = "#388e3c")) +
  labs(
    title = "Comparaison des cas totaux entre pays",
    subtitle = "Histogramme en bandes - Données cumulées",
    x = "Pays",
    y = "Cas totaux",
    fill = "Pays"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
print(p_bandes1)  

# Graphique 2: Histogramme en bandes - Décès totaux
cat("\n       Affichage: Histogramme en bandes - Décès totaux\n")
p_bandes2 <- ggplot(donnees_bandes, aes(x = reorder(country, -deces_totaux), y = deces_totaux, fill = country)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = format(deces_totaux, big.mark = " ")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Afghanistan" = "#1a237e", 
                               "Albania" = "#d32f2f", 
                               "Algeria" = "#388e3c")) +
  labs(
    title = "Comparaison des décès totaux entre pays",
    subtitle = "Histogramme en bandes - Données cumulées",
    x = "Pays",
    y = "Décès totaux",
    fill = "Pays"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#d32f2f"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
print(p_bandes2)  

# Graphique 3: Histogramme en bandes - Cas moyens quotidiens
cat("\n       Affichage: Histogramme en bandes - Cas moyens quotidiens\n")
p_bandes3 <- ggplot(donnees_bandes, aes(x = reorder(country, -cas_moyens), y = cas_moyens, fill = country)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(cas_moyens)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Afghanistan" = "#1a237e", 
                               "Albania" = "#d32f2f", 
                               "Algeria" = "#388e3c")) +
  labs(
    title = "Comparaison des cas moyens quotidiens",
    subtitle = "Histogramme en bandes - Moyenne par jour",
    x = "Pays",
    y = "Cas moyens quotidiens",
    fill = "Pays"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
print(p_bandes3)  

# 7.2 DIAGRAMME CIRCULAIRE COMPARATIF - RÉPARTITION TOTALE
cat("\n   7.2. Diagramme circulaire comparatif...\n")

# Données pour le diagramme circulaire comparatif
data_pie_comparatif <- donnees_bandes %>%
  select(country, cas_totaux) %>%
  mutate(
    pourcentage = round(cas_totaux / sum(cas_totaux) * 100, 1),
    label = paste0(country, "\n", format(cas_totaux, big.mark = " "), "\n(", pourcentage, "%)")
  )

cat("\n       Affichage: Diagramme circulaire comparatif\n")
p_pie_comparatif <- ggplot(data_pie_comparatif, aes(x = "", y = cas_totaux, fill = country)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Afghanistan" = "#1a237e", 
                               "Albania" = "#d32f2f", 
                               "Algeria" = "#388e3c")) +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 5) +
  labs(
    title = "Répartition des cas totaux entre les 3 pays",
    subtitle = "Diagramme circulaire comparatif",
    fill = "Pays"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12, margin = margin(b = 20)),
    legend.position = "none"
  )
print(p_pie_comparatif)  # ??? AFFICHAGE DIRECT DANS RSTUDIO


# 8. MODÈLES STATISTIQUES AVEC VISUALISATION DIRECTE

cat("\n ÉTAPE 8: MODÉLISATION STATISTIQUE AVEC ÉQUATIONS...\n")
cat("   (Les modèles linéaires s'affichent ci-dessous)\n")

# Fonction pour créer un modèle linéaire avec équation
creer_modele_lineaire <- function(donnees_pays, pays_nom) {
  # Créer une variable de temps
  donnees_pays <- donnees_pays[order(donnees_pays$date), ]
  donnees_pays$temps <- 1:nrow(donnees_pays)
  
  # Modèle linéaire sur les 30 derniers jours
  derniers_jours <- tail(donnees_pays, 30)
  modele <- lm(daily_new_cases ~ temps, data = derniers_jours)
  
  # Extraire les coefficients
  intercept <- coef(modele)[1]
  pente <- coef(modele)[2]
  r2 <- summary(modele)$r.squared
  
  # Créer l'équation
  equation <- paste0("y = ", round(pente, 2), "x + ", round(intercept, 2))
  equation_affichage <- paste0("Équation: ", equation, "\nR² = ", round(r2, 3))
  
  # Graphique avec la droite de régression
  p <- ggplot(derniers_jours, aes(x = temps, y = daily_new_cases)) +
    geom_point(color = "#1a237e", size = 2, alpha = 0.6) +
    geom_smooth(method = "lm", formula = y ~ x, 
                color = "#d32f2f", se = FALSE, size = 1.5) +
    geom_text(x = max(derniers_jours$temps) * 0.7, 
              y = max(derniers_jours$daily_new_cases) * 0.9,
              label = equation_affichage,
              hjust = 0, vjust = 1, size = 4.5,
              color = "#d32f2f", fontface = "bold") +
    labs(
      title = paste("Modèle linéaire -", pays_nom),
      subtitle = "Régression linéaire sur les 30 derniers jours",
      x = "Temps (jours)",
      y = "Nouveaux cas",
      caption = paste("Pente =", round(pente, 3), 
                      ifelse(pente > 0, "(tendance à la hausse)", "(tendance à la baisse)"))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40")
    )
  
  return(list(graphique = p, modele = modele, equation = equation, r2 = r2))
}

# Appliquer les modèles aux 3 pays avec affichage direct
modeles <- list()
for (pays in pays_a_analyser) {
  cat("\n   . Modélisation pour:", pays, "\n")
  
  donnees_pays <- donnees[donnees$country == pays, ]
  resultat_modele <- creer_modele_lineaire(donnees_pays, pays)
  modeles[[pays]] <- resultat_modele
  
  # AFFICHAGE DIRECT DU GRAPHIQUE
  print(resultat_modele$graphique)  # ??? AFFICHAGE DIRECT DANS RSTUDIO
  
  cat("       Équation:", resultat_modele$equation, "\n")
  cat("       R²:", round(resultat_modele$r2, 3), "\n")
  if (coef(resultat_modele$modele)[2] > 0) {
    cat("       Tendance: À la hausse\n")
  } else {
    cat("       Tendance: À la baisse\n")
  }
}




# INTERFACE ILLUSTRANT LES DIFFERENTS CAS

library(shiny)
library(shinydashboard)
library(shinyWidgets)  # Pour des éléments UI modernes
library(ggplot2)
library(plotly)        # Pour des graphiques interactifs
library(DT)            # Pour des tables interactives
library(scales)        # Pour de meilleures échelles
library(viridis)       # Pour des palettes de couleurs
library(lubridate)     # Pour la manipulation des dates

# Palette de couleurs modernes
couleurs <- list(
  primaire = "#3498db",
  secondaire = "#2ecc71",
  accent = "#e74c3c",
  warning = "#f39c12",
  dark = "#2c3e50",
  light = "#ecf0f1"
)

# Interface utilisateur améliorée
ui <- dashboardPage(
  skin = "blue",  # Thème bleu moderne
  
  # Header avec logo et titre
  dashboardHeader(
    title = tags$div(
      tags$img(src = "https://img.icons8.com/color/48/000000/coronavirus.png", 
               height = "40px", style = "margin-right: 10px;"),
      "COVID-19 ANALYTICS DASHBOARD"
    ),
    titleWidth = 400
  ),
  
  # Sidebar amélioré
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem(" Tableau de Bord", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE),
      menuItem(" Analyses Comparatives", tabName = "comparisons", icon = icon("chart-line")),
      menuItem("??????? Vue Mondiale", tabName = "global", icon = icon("globe")),
      menuItem(" Données Brutes", tabName = "data", icon = icon("database")),
      menuItem(" Paramètres", tabName = "settings", icon = icon("cog"))
    ),
    
    br(),
    
    # Sélecteur de pays amélioré
    pickerInput(
      inputId = "pays",
      label = tags$h4(" Sélectionnez un pays", style = "color: #2c3e50;"),
      choices = unique(donnees$country),
      selected = unique(donnees$country)[1],
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE,
        `selected-text-format` = "count > 3",
        `count-selected-text` = "{0} pays sélectionnés",
        `none-selected-text` = "Aucun pays sélectionné",
        `style` = "btn-primary"
      ),
      multiple = FALSE
    ),
    
    # Période améliorée
    airDatepickerInput(
      inputId = "dates",
      label = tags$h4(" Sélectionnez une période", style = "color: #2c3e50; margin-top: 20px;"),
      range = TRUE,
      value = c(min(donnees$date), max(donnees$date)),
      minDate = min(donnees$date),
      maxDate = max(donnees$date),
      addon = "left",
      clearButton = TRUE,
      todayButton = TRUE
    ),
    
    # Boutons d'action
    br(),
    actionButton("update", " Actualiser les données", 
                 icon = icon("sync"),
                 class = "btn-primary btn-block",
                 style = "background-color: #3498db; color: white;"),
    
    hr(),
    
    # Indicateurs rapides
    div(
      style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; margin: 10px;",
      tags$h5(" Indicateurs Rapides", style = "color: #2c3e50;"),
      uiOutput("quick_stats")
    )
  ),
  
  # Body amélioré avec onglets
  dashboardBody(
    # CSS personnalisé pour un look moderne
    tags$head(
      tags$style(HTML("
        /* Style général */
        body {
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        }
        
        /* Cartes améliorées */
        .box {
          border-radius: 10px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          border: none;
          margin-bottom: 20px;
        }
        
        .box-header {
          border-top-left-radius: 10px;
          border-top-right-radius: 10px;
          background-color: #f8f9fa !important;
          border-bottom: 2px solid #3498db;
        }
        
        /* Value boxes améliorés */
        .small-box {
          border-radius: 10px;
          box-shadow: 0 3px 5px rgba(0,0,0,0.2);
          transition: transform 0.3s;
        }
        
        .small-box:hover {
          transform: translateY(-5px);
        }
        
        /* Boutons */
        .btn-primary {
          background: linear-gradient(45deg, #3498db, #2980b9);
          border: none;
          border-radius: 25px;
        }
        
        /* Graphiques */
        .plot-container {
          background: white;
          border-radius: 10px;
          padding: 15px;
        }
        
        /* Titres */
        h3 {
          color: #2c3e50;
          font-weight: 600;
        }
      "))
    ),
    
    # Contenu principal
    tabItems(
      # Tab 1: Tableau de bord principal
      tabItem(
        tabName = "dashboard",
        fluidRow(
          # KPI Cards avec animations
          valueBoxOutput("vbox_cas", width = 4),
          valueBoxOutput("vbox_deces", width = 4),
          valueBoxOutput("vbox_taux", width = 4)
        ),
        
        fluidRow(
          # Graphique principal amélioré
          box(
            title = tags$h3(" Évolution des Nouveaux Cas Quotidiens", style = "color: #2c3e50;"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("graphique_cas_ameliore", height = "400px")
          )
        ),
        
        fluidRow(
          # Graphiques secondaires
          box(
            title = tags$h3(" Distribution des Cas", style = "color: #2c3e50;"),
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("graphique_distribution", height = "300px")
          ),
          
          box(
            title = tags$h3(" Tendance sur 7 Jours", style = "color: #2c3e50;"),
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("graphique_tendance", height = "300px")
          )
        )
      ),
      
      # Tab 2: Analyses comparatives
      tabItem(
        tabName = "comparisons",
        fluidRow(
          box(
            title = tags$h3(" Comparaison Internationale", style = "color: #2c3e50;"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("graphique_comparaison_avance", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = tags$h3(" Classement des Pays", style = "color: #2c3e50;"),
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            DTOutput("table_classement")
          )
        )
      ),
      
      # Tab 3: Données brutes
      tabItem(
        tabName = "data",
        box(
          title = tags$h3(" Données Détailées", style = "color: #2c3e50;"),
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          DTOutput("table_donnees"),
          style = "overflow-x: auto;"
        )
      )
    )
  )
)

# Serveur amélioré
server <- function(input, output, session) {
  
  # Données réactives avec gestion d'erreurs
  donnees_filtrees <- reactive({
    req(input$pays, input$dates)
    
    data <- subset(donnees, 
                   country == input$pays & 
                     date >= input$dates[1] & 
                     date <= input$dates[2])
    
    # Calculer des indicateurs supplémentaires
    if(nrow(data) > 0) {
      data$date <- as.Date(data$date)
      data <- data[order(data$date), ]
      
      # Moyenne mobile sur 7 jours
      data$moyenne_mobile <- stats::filter(data$daily_new_cases, 
                                           rep(1/7, 7), 
                                           sides = 2)
      
      # Taux de croissance
      data$taux_croissance <- c(NA, diff(data$daily_new_cases) / 
                                  data$daily_new_cases[-length(data$daily_new_cases)] * 100)
    }
    
    return(data)
  })
  
  # KPI Cards avec meilleur design
  output$vbox_cas <- renderValueBox({
    data <- donnees_filtrees()
    if(nrow(data) > 0) {
      valeur <- max(data$cumulative_total_cases, na.rm = TRUE)
      variation <- ifelse(nrow(data) > 7, 
                          round((tail(data$daily_new_cases, 7) - head(data$daily_new_cases, 7)) / 
                                  head(data$daily_new_cases, 7) * 100, 1),
                          0)
    } else {
      valeur <- 0
      variation <- 0
    }
    
    valueBox(
      value = tags$div(
        tags$h2(format(valeur, big.mark = " "), style = "font-weight: bold;"),
        tags$span(paste0(ifelse(variation > 0, "+", ""), variation, "%"), 
                  style = paste0("font-size: 14px; color: ", 
                                 ifelse(variation > 0, "#e74c3c", "#2ecc71"), ";"))
      ),
      subtitle = tags$h4("Cas Cumulés", style = "font-weight: 600;"),
      icon = icon("virus-covid"),
      color = "blue",
      width = 4
    )
  })
  
  output$vbox_deces <- renderValueBox({
    data <- donnees_filtrees()
    if(nrow(data) > 0) {
      valeur <- max(data$cumulative_total_deaths, na.rm = TRUE)
    } else {
      valeur <- 0
    }
    
    valueBox(
      value = tags$h2(format(valeur, big.mark = " "), style = "font-weight: bold;"),
      subtitle = tags$h4("Décès Cumulés", style = "font-weight: 600;"),
      icon = icon("heart-pulse"),
      color = "red",
      width = 4
    )
  })
  
  output$vbox_taux <- renderValueBox({
    data <- donnees_filtrees()
    if(nrow(data) > 0) {
      cas <- max(data$cumulative_total_cases, na.rm = TRUE)
      deces <- max(data$cumulative_total_deaths, na.rm = TRUE)
      taux <- ifelse(cas > 0, round(deces / cas * 100, 2), 0)
    } else {
      taux <- 0
    }
    
    # Déterminer la couleur selon le taux
    couleur <- ifelse(taux < 1, "green", 
                      ifelse(taux < 3, "yellow", "red"))
    
    valueBox(
      value = tags$h2(paste0(taux, "%"), style = "font-weight: bold;"),
      subtitle = tags$h4("Taux de Létalité", style = "font-weight: 600;"),
      icon = icon("chart-pie"),
      color = couleur,
      width = 4
    )
  })
  
  # Graphique principal amélioré (interactif)
  output$graphique_cas_ameliore <- renderPlotly({
    data <- donnees_filtrees()
    
    if(nrow(data) > 0) {
      p <- plot_ly(data, x = ~date) %>%
        add_bars(y = ~daily_new_cases, 
                 name = "Nouveaux cas",
                 marker = list(color = "rgba(52, 152, 219, 0.7)",
                               line = list(color = "rgba(52, 152, 219, 1)", width = 1))) %>%
        add_lines(y = ~moyenne_mobile, 
                  name = "Moyenne mobile (7j)",
                  line = list(color = "#e74c3c", width = 3),
                  yaxis = "y") %>%
        layout(
          title = list(
            text = paste("Évolution des cas -", input$pays),
            font = list(size = 20, color = "#2c3e50")
          ),
          xaxis = list(
            title = "Date",
            gridcolor = "#ecf0f1",
            showgrid = TRUE
          ),
          yaxis = list(
            title = "Nouveaux cas quotidiens",
            gridcolor = "#ecf0f1",
            showgrid = TRUE
          ),
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          hovermode = "x unified",
          legend = list(
            orientation = "h",
            x = 0.5,
            y = -0.2,
            xanchor = "center"
          )
        )
      
      return(p)
    }
  })
  
  # Graphique de distribution
  output$graphique_distribution <- renderPlotly({
    data <- donnees_filtrees()
    
    if(nrow(data) > 0) {
      p <- plot_ly(
        x = data$daily_new_cases,
        type = "histogram",
        marker = list(
          color = "rgba(46, 204, 113, 0.7)",
          line = list(color = "rgba(46, 204, 113, 1)", width = 1)
        ),
        nbinsx = 30
      ) %>%
        layout(
          title = list(
            text = "Distribution des nouveaux cas",
            font = list(size = 16, color = "#2c3e50")
          ),
          xaxis = list(title = "Nombre de cas"),
          yaxis = list(title = "Fréquence"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
      
      return(p)
    }
  })
  
  # Graphique de tendance
  output$graphique_tendance <- renderPlotly({
    data <- donnees_filtrees()
    
    if(nrow(data) >= 7) {
      # Prendre les 7 derniers jours
      derniers_jours <- tail(data, 7)
      
      p <- plot_ly(derniers_jours, x = ~date, y = ~daily_new_cases, 
                   type = 'scatter', mode = 'lines+markers',
                   line = list(color = "#9b59b6", width = 3),
                   marker = list(size = 10, color = "#9b59b6"),
                   text = ~paste("Date:", date, "<br>Cas:", daily_new_cases),
                   hoverinfo = 'text') %>%
        layout(
          title = list(
            text = "Tendance sur 7 jours",
            font = list(size = 16, color = "#2c3e50")
          ),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Nouveaux cas"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
      
      return(p)
    }
  })
  
  # Graphique de comparaison avancé
  output$graphique_comparaison_avance <- renderPlotly({
    # Sélectionner plusieurs pays pour comparaison
    pays_comparaison <- c(input$pays, 
                          setdiff(unique(donnees$country), input$pays)[1:4])
    data_comparaison <- subset(donnees, country %in% pays_comparaison)
    
    if(nrow(data_comparaison) > 0) {
      # Agréger par pays et date
      data_agg <- aggregate(daily_new_cases ~ country + date, 
                            data = data_comparaison, 
                            FUN = sum)
      
      p <- plot_ly(data_agg, x = ~date, y = ~daily_new_cases, 
                   color = ~country, type = 'scatter', mode = 'lines',
                   colors = viridis::viridis(length(unique(data_agg$country)))) %>%
        layout(
          title = list(
            text = "Comparaison internationale des nouveaux cas",
            font = list(size = 18, color = "#2c3e50")
          ),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Nouveaux cas (échelle log)", type = "log"),
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          hovermode = "x unified"
        )
      
      return(p)
    }
  })
  
  # Table de classement
  output$table_classement <- renderDT({
    # Calculer les totaux par pays
    classement <- aggregate(cbind(cumulative_total_cases, cumulative_total_deaths) ~ country,
                            data = donnees, 
                            FUN = max)
    
    # Calculer le taux de létalité
    classement$taux_letalite <- round(classement$cumulative_total_deaths / 
                                        classement$cumulative_total_cases * 100, 2)
    
    # Trier par nombre de cas
    classement <- classement[order(-classement$cumulative_total_cases), ]
    
    # Ajouter le classement
    classement$rang <- 1:nrow(classement)
    
    # Formater pour affichage
    datatable(
      classement[, c("rang", "country", "cumulative_total_cases", 
                     "cumulative_total_deaths", "taux_letalite")],
      colnames = c("Rang", "Pays", "Cas Cumulés", "Décès Cumulés", "Taux (%)"),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE
    ) %>%
      formatRound(c("cumulative_total_cases", "cumulative_total_deaths"), 0) %>%
      formatRound("taux_letalite", 2) %>%
      formatStyle(
        'taux_letalite',
        background = styleColorBar(classement$taux_letalite, 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Table des données brutes
  output$table_donnees <- renderDT({
    data <- donnees_filtrees()
    if(nrow(data) > 0) {
      datatable(
        data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = 'cell-border stripe hover',
        rownames = FALSE
      ) %>%
        formatRound(c("daily_new_cases", "cumulative_total_cases", 
                      "cumulative_total_deaths"), 0)
    }
  })
  
  # Indicateurs rapides dans la sidebar
  output$quick_stats <- renderUI({
    data <- donnees_filtrees()
    if(nrow(data) > 0) {
      cas_max <- max(data$daily_new_cases, na.rm = TRUE)
      cas_min <- min(data$daily_new_cases, na.rm = TRUE)
      cas_moy <- mean(data$daily_new_cases, na.rm = TRUE)
      
      tagList(
        tags$p(icon("arrow-up"), " Maximum: ", format(cas_max, big.mark = " ")),
        tags$p(icon("arrow-down"), " Minimum: ", format(cas_min, big.mark = " ")),
        tags$p(icon("calculator"), " Moyenne: ", format(round(cas_moy), big.mark = " "))
      )
    }
  })
  
  # Observateur pour le bouton d'actualisation
  observeEvent(input$update, {
    showNotification(" Données actualisées !", 
                     type = "success",
                     duration = 3)
  })
}

# Lancer l'application
shinyApp(ui, server)


