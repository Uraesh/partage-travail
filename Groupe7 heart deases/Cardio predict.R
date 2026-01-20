library(shiny)
library(shinydashboard)
library(plotly)
library(randomForest)
library(DT)
library(caret)
library(dplyr)
library(tidyr)

# ============================================================================
# CHARGEMENT DES MODÈLES ET DONNÉES
# ============================================================================
model_logistic <- tryCatch(readRDS("final_model_logistic.rds"), error = function(e) NULL)
model_rf <- tryCatch(readRDS("final_model_rf.rds"), error = function(e) NULL)
preproc_rules <- tryCatch(readRDS("preproc_rules.rds"), error = function(e) NULL)
heart_data <- tryCatch(readRDS("heart_ready.rds"), error = function(e) NULL)

# ============================================================================
# PALETTE DE COULEURS
# ============================================================================
colors <- list(
  background = "#2d1b4e",
  secondary_surface = "#ffffff",
  text = "#1f2937",
  primary_action = "#8b5cf6",
  secondary_action = "#06b6d4",
  danger = "#ef4444",
  warning = "#f59e0b",
  success = "#10b981",
  card_shadow = "0 4px 6px rgba(0, 0, 0, 0.1)"
)

# ============================================================================
# FONCTIONS UTILITAIRES
# ============================================================================

# Fonction pour calculer les valeurs par défaut selon âge et sexe
get_default_values <- function(age, sex) {
  # Fréquence cardiaque max théorique = 220 - âge
  thalach_default <- max(120, 220 - age - 10)
  
  # Valeurs moyennes ajustées
  list(
    trestbps = if(age < 40) 120 else if(age < 60) 130 else 140,
    chol = if(age < 40) 200 else if(age < 60) 220 else 240,
    thalach = thalach_default,
    oldpeak = 0.5
  )
}

# ============================================================================
# CSS PERSONNALISÉ
# ============================================================================
custom_css <- sprintf("
  body, .content-wrapper, .main-sidebar {
    background-color: %s !important;
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  }
  
  .main-header .navbar {
    background-color: %s !important;
    border-bottom: 3px solid %s;
    box-shadow: %s;
  }
  
  .main-header .navbar-custom-menu > .navbar-nav > li > a,
  .main-header .logo {
    color: white !important;
  }
  
  .main-sidebar {
    background-color: #1a0f2e !important;
    padding-top: 20px;
  }
  
  .sidebar-menu > li > a {
    color: #cbd5e0 !important;
    border-left: 3px solid transparent;
    transition: all 0.3s ease;
  }
  
  .sidebar-menu > li.active > a,
  .sidebar-menu > li:hover > a {
    background-color: rgba(59, 130, 246, 0.1) !important;
    border-left-color: %s !important;
    color: white !important;
  }
  
  .custom-card {
    background: %s;
    border-radius: 12px;
    padding: 24px;
    margin-bottom: 24px;
    box-shadow: %s;
    border: 2px solid #e2e8f0;
    transition: transform 0.2s ease, box-shadow 0.2s ease;
  }
  
  .custom-card:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 16px rgba(0, 0, 0, 0.15);
  }
  
  .card-title {
    color: #2d1b4e;
    font-size: 20px;
    font-weight: 600;
    margin-bottom: 16px;
    display: flex;
    align-items: center;
    gap: 10px;
  }
  
  .help-text {
    color: #6b7280;
    font-size: 14px;
    font-style: italic;
    margin-top: 4px;
    line-height: 1.4;
  }
  
  .help-icon {
    color: %s;
    cursor: help;
    margin-left: 6px;
    font-size: 16px;
  }
  
  .warning-box {
    background: #fef3c7;
    border-left: 4px solid %s;
    padding: 16px;
    border-radius: 8px;
    margin: 20px 0;
  }
  
  .warning-box strong {
    color: #78350f;
  }
  
  .info-box {
    background: #dbeafe;
    border-left: 4px solid %s;
    padding: 16px;
    border-radius: 8px;
    margin: 20px 0;
  }
  
  .form-group label {
    color: %s;
    font-weight: 600;
    margin-bottom: 8px;
    font-size: 15px;
  }
  
  .form-control, .selectize-input {
    border: 2px solid #e2e8f0;
    border-radius: 8px;
    padding: 12px 14px;
    font-size: 15px;
    transition: border-color 0.2s ease;
  }
  
  .form-control:focus, .selectize-input.focus {
    border-color: %s;
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
  }
  
  .btn-predict {
    background: linear-gradient(135deg, %s, #2563eb);
    color: white;
    border: none;
    border-radius: 8px;
    padding: 14px 32px;
    font-size: 16px;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.3s ease;
    box-shadow: 0 4px 12px rgba(59, 130, 246, 0.3);
    width: 100%%;
    margin-top: 20px;
  }
  
  .btn-predict:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 16px rgba(59, 130, 246, 0.4);
    background: linear-gradient(135deg, #2563eb, %s);
  }
  
  .prediction-result {
    background: linear-gradient(135deg, #f8fafc, #e2e8f0);
    border-radius: 12px;
    padding: 28px;
    margin-top: 24px;
    border-left: 6px solid;
    animation: slideIn 0.4s ease;
  }
  
  @keyframes slideIn {
    from {
      opacity: 0;
      transform: translateY(10px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }
  
  .risk-low { border-left-color: %s; }
  .risk-medium { border-left-color: %s; }
  .risk-high { border-left-color: %s; }
  
  .prediction-title {
    font-size: 1.5rem;
    font-weight: 700;
    margin-bottom: 12px;
  }
  
  .prediction-probability {
    font-size: 2.5rem;
    font-weight: 800;
    margin: 16px 0;
  }
  
  .recommendation-box {
    margin-top: 24px;
    padding: 20px;
    background: white;
    border-radius: 8px;
    border: 2px solid #e2e8f0;
  }
  
  .recommendation-box h4 {
    color: %s;
    margin-bottom: 12px;
  }
  
  .recommendation-list {
    list-style: none;
    padding: 0;
  }
  
  .recommendation-list li {
    padding: 8px 0;
    padding-left: 24px;
    position: relative;
  }
  
  .recommendation-list li:before {
    content: '✓';
    position: absolute;
    left: 0;
    color: %s;
    font-weight: bold;
  }
  
  @media (max-width: 768px) {
    .custom-card {
      padding: 16px;
      margin-bottom: 16px;
    }
    
    .card-title {
      font-size: 18px;
    }
    
    .form-group label {
      font-size: 14px;
    }
    
    .prediction-probability {
      font-size: 2rem;
    }
  }
", 
                      colors$background, colors$background, colors$primary_action, colors$card_shadow,
                      colors$primary_action, colors$secondary_surface, colors$card_shadow, 
                      colors$primary_action, colors$warning, colors$primary_action, colors$text,
                      colors$primary_action, colors$primary_action, colors$primary_action,
                      colors$success, colors$warning, colors$danger, colors$text, colors$success)

# ============================================================================
# UI
# ============================================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = span(icon("heartbeat"), "CardioPredict"),
    titleWidth = 280
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Évaluation Simple", tabName = "prediction_simple", icon = icon("heart")),
      menuItem("Évaluation Complète", tabName = "prediction_medical", icon = icon("file-medical")),
      menuItem("À propos", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css))
    ),
    
    tabItems(
      # =========== ACCUEIL ===========
      tabItem(
        tabName = "home",
        fluidRow(
          column(12,
                 div(class = "custom-card",
                     div(class = "card-title", 
                         "Bienvenue sur CardioPredict",
                         icon("heart", style = "color: #e74c3c;")),
                     p("Évaluez votre risque cardiovasculaire en quelques minutes.", 
                       style = "font-size: 18px; line-height: 1.8; color: #4a5568;"),
                     hr(style = "border-color: #e2e8f0; margin: 24px 0;"),
                     
                     div(class = "info-box",
                         p(strong("Comment ça marche ?"), style = "margin: 0; font-size: 15px;"),
                         tags$ol(style = "margin-top: 12px; color: #4a5568;",
                                 tags$li("Répondez aux questions sur votre santé"),
                                 tags$li("Obtenez une estimation de votre risque"),
                                 tags$li("Recevez des recommandations personnalisées")
                         )
                     ),
                     
                     div(style = "text-align: center; margin-top: 24px;",
                         actionButton("go_to_simple", "Commencer l'évaluation", 
                                      class = "btn-predict",
                                      style = "max-width: 400px;",
                                      icon = icon("arrow-right"))
                     )
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 div(class = "warning-box",
                     p(icon("exclamation-triangle"), strong(" Important : "), 
                       "Cet outil est destiné à l'information uniquement. Il ne remplace en aucun cas une consultation médicale. En cas de douleurs thoraciques ou de symptômes inquiétants, consultez immédiatement un professionnel de santé.",
                       style = "margin: 0; color: #78350f;")
                 )
          )
        )
      ),
      
      # =========== ÉVALUATION SIMPLE ===========
      tabItem(
        tabName = "prediction_simple",
        fluidRow(
          column(12,
                 div(class = "custom-card",
                     h3("Évaluation Rapide de Votre Risque Cardiaque", 
                        style = "color: #2d1b4e; margin-bottom: 24px;"),
                     
                     p("Répondez aux questions suivantes. Si vous ne connaissez pas une valeur, laissez la valeur proposée.",
                       style = "color: #6b7280; margin-bottom: 24px;"),
                     
                     # Informations de base
                     fluidRow(
                       column(6,
                              div(
                                numericInput("age_simple", 
                                             span("Votre âge", 
                                                  tags$i(class = "fa fa-question-circle help-icon",
                                                         title = "Nombre d'années complètes")),
                                             value = 50, min = 18, max = 100, step = 1),
                                div(class = "help-text", "En années")
                              )
                       ),
                       column(6,
                              selectInput("sex_simple", 
                                          "Votre sexe", 
                                          choices = c("Femme" = "0", "Homme" = "1"),
                                          selected = "1")
                       )
                     ),
                     
                     # Symptômes
                     div(style = "margin-top: 20px;",
                         h4("Vos symptômes", style = "color: #2d1b4e; font-size: 18px;"),
                         
                         selectInput("symptoms", 
                                     span("Ressentez-vous des douleurs dans la poitrine ?",
                                          tags$i(class = "fa fa-question-circle help-icon",
                                                 title = "Douleur, pression ou gêne dans la poitrine")),
                                     choices = c(
                                       "Jamais" = "0",
                                       "Parfois pendant un effort (sport, escaliers)" = "1",
                                       "Souvent, même au repos" = "2",
                                       "Très fréquentes et intenses" = "3"
                                     )),
                         
                         selectInput("breathless",
                                     "Êtes-vous essoufflé(e) facilement ?",
                                     choices = c(
                                       "Non, pas plus que d'habitude" = "0",
                                       "Oui, à l'effort modéré" = "1"
                                     ))
                     ),
                     
                     # Mesures médicales
                     div(style = "margin-top: 20px;",
                         h4("Vos mesures (si vous les connaissez)", 
                            style = "color: #2d1b4e; font-size: 18px;"),
                         
                         fluidRow(
                           column(6,
                                  div(
                                    numericInput("bp_simple", 
                                                 span("Tension artérielle (chiffre du haut)",
                                                      tags$i(class = "fa fa-question-circle help-icon",
                                                             title = "Exemple: si votre tension est 14/9, entrez 140")),
                                                 value = 130, min = 80, max = 200, step = 5),
                                    div(class = "help-text", "En mm Hg (exemple: 120, 140, 160)")
                                  )
                           ),
                           column(6,
                                  div(
                                    numericInput("chol_simple", 
                                                 span("Cholestérol total",
                                                      tags$i(class = "fa fa-question-circle help-icon",
                                                             title = "Valeur de votre dernière prise de sang")),
                                                 value = 220, min = 100, max = 400, step = 10),
                                    div(class = "help-text", "En mg/dl (normal: 150-200)")
                                  )
                           )
                         )
                     ),
                     
                     # Facteurs de risque
                     div(style = "margin-top: 20px;",
                         h4("Vos facteurs de risque", 
                            style = "color: #2d1b4e; font-size: 18px;"),
                         
                         checkboxGroupInput("risk_factors", 
                                            "Cochez ce qui s'applique à vous :",
                                            choices = list(
                                              "Je suis diabétique (ou pré-diabétique)" = "diabete",
                                              "Je fume (ou j'ai fumé récemment)" = "fumeur",
                                              "Quelqu'un dans ma famille proche a eu des problèmes cardiaques" = "famille",
                                              "Je fais peu d'exercice physique" = "sedentaire",
                                              "Je me sens souvent stressé(e)" = "stress"
                                            ),
                                            selected = NULL)
                     ),
                     
                     actionButton("predict_simple", "Évaluer mon risque", 
                                  class = "btn-predict",
                                  icon = icon("heartbeat")),
                     
                     uiOutput("prediction_results_simple")
                 )
          )
        )
      ),
      
      # =========== ÉVALUATION MÉDICALE ===========
      tabItem(
        tabName = "prediction_medical",
        fluidRow(
          column(12,
                 div(class = "custom-card",
                     h3("Évaluation Complète (avec résultats médicaux)", 
                        style = "color: #2d1b4e; margin-bottom: 16px;"),
                     
                     div(class = "info-box",
                         p(icon("info-circle"), strong(" Cette section est pour vous si : "),
                           "Vous avez récemment passé des examens cardiaques (ECG, test d'effort, etc.) et vous disposez des résultats détaillés.",
                           style = "margin: 0; color: #1e40af;")
                     ),
                     
                     # Informations de base
                     fluidRow(
                       column(4, numericInput("age", "Âge", value = 50, min = 18, max = 100)),
                       column(4, selectInput("sex", "Sexe", c("Femme" = "0", "Homme" = "1"))),
                       column(4, selectInput("cp", "Type de douleur thoracique", 
                                             choices = 0:3, selected = 1))
                     ),
                     
                     fluidRow(
                       column(4, numericInput("trestbps", "Tension artérielle (mm Hg)", 
                                              value = 130, min = 80, max = 200)),
                       column(4, numericInput("chol", "Cholestérol (mg/dl)", 
                                              value = 220, min = 100, max = 400)),
                       column(4, selectInput("fbs", "Glycémie à jeun > 120 mg/dl", 
                                             c("Normal", "Élevé")))
                     ),
                     
                     fluidRow(
                       column(4, selectInput("restecg", "Résultat ECG au repos", 
                                             choices = 0:2, selected = 0)),
                       column(4, numericInput("thalach", "Fréquence cardiaque max", 
                                              value = 150, min = 60, max = 220)),
                       column(4, selectInput("exang", "Angine à l'effort", c("Non", "Oui")))
                     ),
                     
                     fluidRow(
                       column(4, numericInput("oldpeak", "Dépression ST", 
                                              value = 1, min = 0, max = 6, step = 0.1)),
                       column(4, selectInput("slope", "Pente du segment ST", 
                                             choices = 0:2, selected = 1)),
                       column(4, selectInput("ca", "Nombre de vaisseaux colorés", 
                                             choices = 0:4, selected = 0))
                     ),
                     
                     fluidRow(
                       column(4, selectInput("thal", "Thalassémie", 
                                             choices = 0:3, selected = 2))
                     ),
                     
                     actionButton("predict_btn", "Calculer le risque", 
                                  class = "btn-predict",
                                  icon = icon("calculator")),
                     
                     uiOutput("prediction_results")
                 )
          )
        )
      ),
      
      # =========== À PROPOS ===========
      tabItem(
        tabName = "about",
        fluidRow(
          column(12,
                 div(class = "custom-card",
                     div(class = "card-title", "À propos de CardioPredict"),
                     
                     h4("Qu'est-ce que c'est ?", style = "color: #2d3748; margin-top: 20px;"),
                     p("CardioPredict est un outil d'aide à l'évaluation du risque cardiovasculaire. Il utilise l'intelligence artificielle pour analyser vos informations de santé et estimer votre niveau de risque.", 
                       style = "color: #4a5568; line-height: 1.8;"),
                     
                     h4("Comment ça marche ?", style = "color: #2d3748; margin-top: 24px;"),
                     p("L'application utilise deux modèles d'apprentissage automatique entraînés sur des milliers de cas médicaux. Ces modèles analysent plusieurs facteurs de santé pour calculer une estimation de risque.",
                       style = "color: #4a5568; line-height: 1.8;"),
                     
                     h4("Qui peut l'utiliser ?", style = "color: #2d3748; margin-top: 24px;"),
                     p("Toute personne adulte souhaitant avoir une première idée de son risque cardiaque. L'outil propose deux niveaux d'utilisation selon vos connaissances médicales.",
                       style = "color: #4a5568; line-height: 1.8;"),
                     
                     h4("Créateurs", style = "color: #2d3748; margin-top: 24px;"),
                     p(strong("FEBON S. Daniel & BODI - SAMA Souweba"), 
                       style = "color: #3b82f6; font-size: 1.1rem;"),
                     
                     div(class = "warning-box", style = "margin-top: 24px;",
                         p(strong("⚠️ Avertissement Médical"), style = "font-size: 16px; margin-bottom: 8px;"),
                         tags$ul(style = "color: #78350f; margin: 0; padding-left: 20px;",
                                 tags$li("Cet outil est à but informatif uniquement"),
                                 tags$li("Il ne remplace PAS un diagnostic médical professionnel"),
                                 tags$li("En cas de symptômes inquiétants, consultez immédiatement"),
                                 tags$li("Ne modifiez jamais votre traitement sans avis médical")
                         )
                     )
                 )
          )
        )
      )
    ),
    
    tags$footer(style = "background-color: #1a0f2e; color: #cbd5e0; padding: 20px; text-align: center; margin-top: 40px;",
                p("© 2025 CardioPredict | Un outil d'information, pas de diagnostic", 
                  style = "margin: 0; font-size: 14px;")
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  
  # Navigation vers l'évaluation simple
  observeEvent(input$go_to_simple, {
    updateTabItems(session, "tabs", "prediction_simple")
  })
  
  # ========== PRÉDICTION SIMPLE ==========
  observeEvent(input$predict_simple, {
    if(is.null(model_logistic) || is.null(model_rf) || is.null(preproc_rules)) {
      output$prediction_results_simple <- renderUI({
        div(class = "prediction-result risk-high",
            h3("Service temporairement indisponible", class = "prediction-title"),
            p("Les modèles de prédiction ne sont pas disponibles. Veuillez réessayer plus tard.")
        )
      })
      return(NULL)
    }
    
    # Récupération des valeurs par défaut
    defaults <- get_default_values(input$age_simple, input$sex_simple)
    
    # Conversion des inputs simples en format médical
    # Type de douleur thoracique (cp)
    cp_value <- as.numeric(input$symptoms)
    
    # Glycémie à jeun (fbs) - basé sur diabète
    fbs_value <- if("diabete" %in% input$risk_factors) "Élevé" else "Normal"
    
    # ECG au repos (restecg) - valeur par défaut normale
    restecg_value <- 0
    
    # Fréquence cardiaque max (thalach)
    thalach_value <- defaults$thalach
    
    # Angine induite par l'effort (exang) - basé sur essoufflement
    exang_value <- if(input$breathless == "1") "Oui" else "Non"
    
    # Dépression ST (oldpeak) - estimation basée sur symptômes
    oldpeak_value <- if(cp_value >= 2) 1.5 else 0.5
    
    # Pente du segment ST (slope) - valeur par défaut
    slope_value <- 1
    
    # Nombre de vaisseaux colorés (ca) - estimation basée sur facteurs de risque
    ca_value <- min(length(input$risk_factors), 4)
    
    # Thalassémie (thal) - valeur par défaut normale
    thal_value <- 2
    
    # Préparation des données pour le modèle
    data_a_predire <- data.frame(
      age = input$age_simple,
      sex = factor(input$sex_simple, levels = c("0", "1"), labels = c("Femme", "Homme")),
      cp = factor(cp_value, levels = 0:3),
      trestbps = input$bp_simple,
      chol = input$chol_simple,
      fbs = factor(fbs_value, levels = c("Normal", "Élevé")),
      restecg = factor(restecg_value, levels = 0:2),
      thalach = thalach_value,
      exang = factor(exang_value, levels = c("Non", "Oui")),
      oldpeak = oldpeak_value,
      slope = factor(slope_value, levels = 0:2),
      ca = factor(ca_value, levels = 0:4),
      thal = factor(thal_value, levels = 0:3)
    )
    
    # Prédiction avec les vrais modèles
    data_prep <- predict(preproc_rules, data_a_predire)
    pred_log <- predict(model_logistic, data_prep, type = "prob")
    pred_rf <- predict(model_rf, data_prep, type = "prob")
    
    prob_log <- round(pred_log$Malade * 100, 1)
    prob_rf <- round(pred_rf$Malade * 100, 1)
    prob_estimate <- round(mean(c(prob_log, prob_rf)), 1)
    
    # Détermination du niveau de risque
    risk_level <- if(prob_estimate < 30) "low" else if(prob_estimate < 60) "medium" else "high"
    risk_text <- if(prob_estimate < 30) "Faible" else if(prob_estimate < 60) "Modéré" else "Élevé"
    risk_color <- if(prob_estimate < 30) colors$success else if(prob_estimate < 60) colors$warning else colors$danger
    risk_emoji <- if(prob_estimate < 30) "😊" else if(prob_estimate < 60) "😐" else "😟"
    
    output$prediction_results_simple <- renderUI({
      div(class = paste0("prediction-result risk-", risk_level),
          h3(paste("Votre Évaluation de Risque", risk_emoji), 
             class = "prediction-title", 
             style = paste0("color: ", colors$text)),
          
          div(style = "text-align: center; margin: 24px 0;",
              h4(paste("Risque", risk_text), 
                 style = paste0("color: ", risk_color, "; font-weight: 700; font-size: 1.8rem;"))
          ),
          
          div(class = "recommendation-box",
              h4(icon("lightbulb"), " Que faire maintenant ?"),
              
              if(prob_estimate < 30) {
                tags$ul(class = "recommendation-list",
                        tags$li("Continuez vos bonnes habitudes de vie"),
                        tags$li("Faites de l'exercice régulièrement (30 min/jour)"),
                        tags$li("Maintenez une alimentation équilibrée"),
                        tags$li("Consultez votre médecin pour un bilan de routine")
                )
              } else if(prob_estimate < 60) {
                tags$ul(class = "recommendation-list",
                        tags$li(strong("Prenez rendez-vous avec votre médecin"), " pour un bilan approfondi"),
                        tags$li("Discutez de vos facteurs de risque"),
                        tags$li("Envisagez des changements de mode de vie"),
                        tags$li("Un suivi médical régulier est recommandé")
                )
              } else {
                tags$ul(class = "recommendation-list",
                        tags$li(strong(style = paste0("color: ", colors$danger), 
                                       "Consultez rapidement un cardiologue")),
                        tags$li("Un bilan cardiaque complet est nécessaire"),
                        tags$li("Ne négligez pas les symptômes"),
                        tags$li("En cas de douleur thoracique intense : appelez le 15")
                )
              }
          ),
          
          div(class = "warning-box", style = "margin-top: 20px;",
              p(icon("info-circle"), " Cette évaluation est indicative. Seul un professionnel de santé peut établir un diagnostic précis et vous prescrire un traitement adapté.",
                style = "margin: 0; font-size: 14px;")
          )
      )
    })
  })
  
  # ========== PRÉDICTION MÉDICALE COMPLÈTE ==========
  observeEvent(input$predict_btn, {
    if(is.null(model_logistic) || is.null(model_rf)) {
      output$prediction_results <- renderUI({
        div(class = "prediction-result risk-high",
            h3("Modèles non disponibles", class = "prediction-title"),
            p("Les modèles de prédiction n'ont pas pu être chargés.")
        )
      })
      return(NULL)
    }
    
    # Préparation des données
    data_a_predire <- data.frame(
      age = input$age,
      sex = factor(input$sex, levels = c("0", "1"), labels = c("Femme", "Homme")),
      cp = factor(input$cp, levels = 0:3),
      trestbps = input$trestbps,
      chol = input$chol,
      fbs = factor(input$fbs, levels = c("Normal", "Élevé")),
      restecg = factor(input$restecg, levels = 0:2),
      thalach = input$thalach,
      exang = factor(input$exang, levels = c("Non", "Oui")),
      oldpeak = input$oldpeak,
      slope = factor(input$slope, levels = 0:2),
      ca = factor(input$ca, levels = 0:4),
      thal = factor(input$thal, levels = 0:3)
    )
    
    data_prep <- predict(preproc_rules, data_a_predire)
    pred_log <- predict(model_logistic, data_prep, type = "prob")
    pred_rf <- predict(model_rf, data_prep, type = "prob")
    
    prob_log <- round(pred_log$Malade * 100, 1)
    prob_rf <- round(pred_rf$Malade * 100, 1)
    prob_mean <- round(mean(c(prob_log, prob_rf)), 1)
    
    risk_level <- if(prob_mean < 30) "low" else if(prob_mean < 60) "medium" else "high"
    risk_text <- if(prob_mean < 30) "Faible" else if(prob_mean < 60) "Modéré" else "Élevé"
    risk_color <- if(prob_mean < 30) colors$success else if(prob_mean < 60) colors$warning else colors$danger
    
    output$prediction_results <- renderUI({
      div(class = paste0("prediction-result risk-", risk_level),
          h3("Résultat de l'Analyse Détaillée", class = "prediction-title", 
             style = paste0("color: ", colors$text)),
          
          div(style = "text-align: center; margin: 24px 0;",
              div(class = "prediction-probability", 
                  style = paste0("color: ", risk_color),
                  paste0(prob_mean, "%")),
              h4(paste("Risque", risk_text), 
                 style = paste0("color: ", risk_color, "; font-weight: 600;"))
          ),
          
          hr(style = "border-color: #cbd5e0; margin: 24px 0;"),
          
          h5("Détails par Modèle", style = paste0("color: ", colors$text, "; margin-bottom: 16px;")),
          
          div(style = "display: flex; justify-content: space-around; flex-wrap: wrap; gap: 16px;",
              div(style = paste0("flex: 1; min-width: 200px; padding: 16px; background: white; 
                             border-radius: 8px; border-left: 4px solid ", colors$primary_action),
                  h6("Régression Logistique", style = paste0("color: ", colors$primary_action)),
                  p(paste0(prob_log, "%"), 
                    style = "font-size: 1.8rem; font-weight: 700; margin: 8px 0;")
              ),
              div(style = paste0("flex: 1; min-width: 200px; padding: 16px; background: white; 
                             border-radius: 8px; border-left: 4px solid ", colors$secondary_action),
                  h6("Random Forest", style = paste0("color: ", colors$secondary_action)),
                  p(paste0(prob_rf, "%"), 
                    style = "font-size: 1.8rem; font-weight: 700; margin: 8px 0;")
              )
          ),
          
          div(class = "recommendation-box",
              h4(icon("user-md"), " Recommandation Médicale"),
              p(if(prob_mean < 30) {
                "Les résultats sont plutôt rassurants, mais consultez votre médecin pour confirmation et suivi préventif."
              } else if(prob_mean < 60) {
                "Risque modéré détecté. Une consultation cardiologique approfondie est recommandée pour explorer ces résultats."
              } else {
                "Risque élevé identifié. Consultez rapidement un cardiologue pour des examens complémentaires et une prise en charge adaptée."
              }, style = "color: #1f2937; line-height: 1.6;")
          )
      )
    })
  })
}

shinyApp(ui, server)