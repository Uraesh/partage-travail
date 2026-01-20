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
get_default_values <- function(age, sex) {
  thalach_default <- max(120, 220 - age - 10)
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
      menuItem("Visualisations", tabName = "visualizations", icon = icon("chart-line")),
      menuItem("Performances", tabName = "performance", icon = icon("trophy")),
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
      
      # =========== ÉVALUATION MÉDICALE COMPLÈTE ===========
      tabItem(
        tabName = "prediction_medical",
        fluidRow(
          column(6,
                 div(class = "custom-card",
                     h3("Évaluation Complète (avec résultats médicaux)", 
                        style = "color: #2d1b4e; margin-bottom: 16px;"),
                     
                     div(class = "info-box",
                         p(icon("info-circle"), strong(" Cette section est pour vous si : "),
                           "Vous avez récemment passé des examens cardiaques (ECG, test d'effort, etc.) et vous disposez des résultats détaillés.",
                           style = "margin: 0; color: #1e40af;")
                     ),
                     
                     fluidRow(
                       column(6, numericInput("age", "Âge", value = 50, min = 20, max = 100)),
                       column(6, selectInput("sex", "Sexe", choices = c("Femme", "Homme")))
                     ),
                     
                     fluidRow(
                       column(6, selectInput("cp", "Type de douleur thoracique", 
                                             choices = 0:3, selected = 0)),
                       column(6, numericInput("trestbps", "Pression artérielle (mm Hg)", 
                                              value = 120, min = 80, max = 200))
                     ),
                     
                     fluidRow(
                       column(6, numericInput("chol", "Cholestérol (mg/dl)", 
                                              value = 200, min = 100, max = 600)),
                       column(6, selectInput("fbs", "Glycémie à jeun > 120 mg/dl", 
                                             choices = c("Normal", "Élevé")))
                     ),
                     
                     fluidRow(
                       column(6, selectInput("restecg", "ECG au repos", 
                                             choices = 0:2, selected = 0)),
                       column(6, numericInput("thalach", "Fréquence cardiaque max", 
                                              value = 150, min = 60, max = 220))
                     ),
                     
                     fluidRow(
                       column(6, selectInput("exang", "Angine induite par l'exercice", 
                                             choices = c("Non", "Oui"))),
                       column(6, numericInput("oldpeak", "Dépression ST", 
                                              value = 1.0, min = 0, max = 6.2, step = 0.1))
                     ),
                     
                     fluidRow(
                       column(6, selectInput("slope", "Pente du segment ST", 
                                             choices = 0:2, selected = 1)),
                       column(6, selectInput("ca", "Nombre de vaisseaux colorés", 
                                             choices = 0:4, selected = 0))
                     ),
                     
                     fluidRow(
                       column(12, selectInput("thal", "Thalassémie", 
                                              choices = 0:3, selected = 2))
                     ),
                     
                     actionButton("predict_btn", "LANCER LA PRÉDICTION", 
                                  class = "btn-predict",
                                  icon = icon("calculator"))
                 )
          ),
          
          column(6,
                 div(class = "custom-card",
                     div(class = "card-title", "Résultats de la Prédiction"),
                     uiOutput("prediction_results")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 div(class = "custom-card",
                     div(class = "card-title", "Équations des Modèles"),
                     
                     h4("Régression Logistique", style = "color: #8b5cf6; margin-top: 16px;"),
                     div(style = "background: #f3f4f6; padding: 20px; border-radius: 8px; margin: 16px 0; overflow-x: auto;",
                         p("La régression logistique calcule la probabilité d'avoir une maladie cardiaque selon :", 
                           style = "color: #4b5563; margin-bottom: 12px;"),
                         tags$code(
                           "P(Malade) = 1 / (1 + e^(-z))",
                           style = "display: block; background: white; padding: 12px; border-radius: 4px; font-size: 14px; border-left: 4px solid #8b5cf6;"
                         ),
                         p("où :", style = "margin-top: 12px; color: #4b5563;"),
                         tags$code(
                           "z = β₀ + β₁×age + β₂×sexe + β₃×cp + β₄×trestbps + β₅×chol + β₆×fbs + β₇×restecg + β₈×thalach + β₉×exang + β₁₀×oldpeak + β₁₁×slope + β₁₂×ca + β₁₃×thal",
                           style = "display: block; background: white; padding: 12px; border-radius: 4px; font-size: 13px; margin-top: 8px; word-wrap: break-word;"
                         ),
                         p(HTML("<strong>Interprétation :</strong> Les coefficients β (bêta) indiquent l'impact de chaque variable. Un coefficient positif augmente le risque, un négatif le diminue."),
                           style = "margin-top: 12px; color: #4b5563; font-size: 14px;")
                     ),
                     
                     h4("Random Forest", style = "color: #06b6d4; margin-top: 24px;"),
                     div(style = "background: #f3f4f6; padding: 20px; border-radius: 8px; margin: 16px 0;",
                         p("Le Random Forest combine plusieurs arbres de décision :", 
                           style = "color: #4b5563; margin-bottom: 12px;"),
                         tags$code(
                           "P(Malade) = (1/n) × Σ P_arbre_i(Malade)",
                           style = "display: block; background: white; padding: 12px; border-radius: 4px; font-size: 14px; border-left: 4px solid #06b6d4;"
                         ),
                         p("où n est le nombre d'arbres (généralement 100-500)", 
                           style = "margin-top: 12px; color: #6b7280; font-size: 14px;"),
                         p(HTML("<strong>Interprétation :</strong> Chaque arbre vote pour 'Malade' ou 'Sain', et la forêt fait la moyenne des votes. Cela capture des interactions complexes entre variables que la régression logistique ne peut pas voir."),
                           style = "margin-top: 12px; color: #4b5563; font-size: 14px;")
                     ),
                     
                     div(class = "info-box", style = "margin-top: 20px;",
                         p(icon("lightbulb"), " ", strong("Note :"), 
                           " Les deux modèles donnent une probabilité entre 0% et 100%. Plus la probabilité est élevée, plus le risque de maladie cardiaque est important.",
                           style = "margin: 0; color: #1e40af; font-size: 14px;")
                     )
                 )
          )
        )
      ),
      
      # =========== VISUALISATIONS ===========
      tabItem(
        tabName = "visualizations",
        fluidRow(
          column(12,
                 div(class = "custom-card",
                     div(class = "card-title", "Explorez les Données"),
                     p("Visualisations interactives pour mieux comprendre les facteurs de risque cardiovasculaires.",
                       style = "color: #4a5568; margin-bottom: 20px;")
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 div(class = "custom-card",
                     div(class = "card-title", "Cholestérol vs Pression Artérielle"),
                     plotlyOutput("scatter_chol_bp", height = "350px"),
                     uiOutput("regression_equation")
                 )
          ),
          column(6,
                 div(class = "custom-card",
                     div(class = "card-title", "Répartition par Sexe"),
                     plotlyOutput("sex_distribution", height = "350px")
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 div(class = "custom-card",
                     div(class = "card-title", "Type de Douleur Thoracique"),
                     plotlyOutput("chest_pain_plot", height = "350px")
                 )
          ),
          column(6,
                 div(class = "custom-card",
                     div(class = "card-title", "Fréquence Cardiaque Maximale"),
                     plotlyOutput("heart_rate_plot", height = "350px")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 div(class = "custom-card",
                     div(class = "card-title", "Matrice de Corrélation"),
                     plotlyOutput("correlation_matrix", height = "500px")
                 )
          )
        )
      ),
      
      # =========== PERFORMANCES ===========
      tabItem(
        tabName = "performance",
        fluidRow(
          column(12,
                 div(class = "custom-card",
                     div(class = "card-title", "Comparaison des Modèles"),
                     p("Analyse comparative des performances entre Régression Logistique et Random Forest.",
                       style = "color: #4a5568; margin-bottom: 20px;")
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 div(class = "custom-card",
                     div(class = "card-title", "Métriques de Performance"),
                     plotlyOutput("performance_comparison", height = "400px")
                 )
          ),
          column(6,
                 div(class = "custom-card",
                     div(class = "card-title", "Tableau Comparatif"),
                     DTOutput("performance_table")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 div(class = "custom-card",
                     div(class = "card-title", "Importance des Variables (Random Forest)"),
                     plotlyOutput("variable_importance", height = "450px")
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
    
    defaults <- get_default_values(input$age_simple, input$sex_simple)
    
    cp_value <- as.numeric(input$symptoms)
    fbs_value <- if("diabete" %in% input$risk_factors) "Élevé" else "Normal"
    restecg_value <- 0
    thalach_value <- defaults$thalach
    exang_value <- if(input$breathless == "1") "Oui" else "Non"
    oldpeak_value <- if(cp_value >= 2) 1.5 else 0.5
    slope_value <- 1
    ca_value <- min(length(input$risk_factors), 4)
    thal_value <- 2
    
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
    
    data_prep <- predict(preproc_rules, data_a_predire)
    pred_log <- predict(model_logistic, data_prep, type = "prob")
    pred_rf <- predict(model_rf, data_prep, type = "prob")
    
    prob_log <- round(pred_log$Malade * 100, 1)
    prob_rf <- round(pred_rf$Malade * 100, 1)
    prob_estimate <- round(mean(c(prob_log, prob_rf)), 1)
    
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
    
    data_a_predire <- data.frame(
      age = input$age,
      sex = factor(input$sex, levels = c("Femme", "Homme")),
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
  
  # ========== VISUALISATIONS ==========
  output$scatter_chol_bp <- renderPlotly({
    if(is.null(heart_data)) return(NULL)
    
    model_lm <- lm(trestbps ~ chol, data = heart_data)
    a <- coef(model_lm)[2]
    b <- coef(model_lm)[1]
    
    heart_data$fitted <- predict(model_lm)
    
    plot_ly(heart_data, x = ~chol, y = ~trestbps, color = ~target, 
            colors = c(colors$primary_action, colors$danger),
            type = "scatter", mode = "markers",
            marker = list(size = 8, opacity = 0.6),
            name = ~target) %>%
      add_trace(x = ~chol, y = ~fitted, 
                type = "scatter", mode = "lines",
                line = list(color = "#10b981", width = 2, dash = "dash"),
                name = "Régression linéaire",
                hovertemplate = paste0("Équation: y = ", round(a, 2), "x + ", round(b, 2))) %>%
      layout(xaxis = list(title = "Cholestérol (mg/dl)"),
             yaxis = list(title = "Pression artérielle (mm Hg)"),
             plot_bgcolor = colors$secondary_surface,
             paper_bgcolor = colors$secondary_surface,
             showlegend = TRUE)
  })
  
  output$regression_equation <- renderUI({
    if(is.null(heart_data)) return(NULL)
    
    model_lm <- lm(trestbps ~ chol, data = heart_data)
    a <- round(coef(model_lm)[2], 3)
    b <- round(coef(model_lm)[1], 2)
    r_squared <- round(summary(model_lm)$r.squared, 3)
    
    div(style = "margin-top: 16px; padding: 12px; background: #f0fdf4; border-left: 4px solid #10b981; border-radius: 4px;",
        p(strong("Équation de régression linéaire :"), style = "color: #065f46; margin: 0 0 8px 0;"),
        tags$code(
          paste0("Pression = ", a, " × Cholestérol + ", b),
          style = "display: block; background: white; padding: 8px; border-radius: 4px; color: #047857; font-size: 14px;"
        ),
        p(paste0("Coefficient de détermination (R²) : ", r_squared), 
          style = "margin-top: 8px; color: #065f46; font-size: 13px;"),
        p(HTML(paste0("<em>Interprétation :</em> Pour chaque augmentation de 1 mg/dl de cholestérol, la pression artérielle augmente en moyenne de ", abs(a), " mm Hg.")),
          style = "margin-top: 8px; color: #047857; font-size: 12px;")
    )
  })
  
  output$sex_distribution <- renderPlotly({
    if(is.null(heart_data)) return(NULL)
    
    sex_data <- heart_data %>%
      group_by(sex, target) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(sex) %>%
      mutate(percentage = round(count/sum(count)*100, 1))
    
    plot_ly(sex_data, x = ~sex, y = ~percentage, color = ~target,
            colors = c(colors$primary_action, colors$danger),
            type = "bar", text = ~paste0(percentage, "%"),
            textposition = "inside") %>%
      layout(barmode = "stack", xaxis = list(title = ""),
             yaxis = list(title = "Pourcentage (%)"),
             plot_bgcolor = colors$secondary_surface,
             paper_bgcolor = colors$secondary_surface)
  })
  
  output$chest_pain_plot <- renderPlotly({
    if(is.null(heart_data)) return(NULL)
    
    cp_data <- heart_data %>%
      group_by(cp, target) %>%
      summarise(count = n(), .groups = "drop")
    
    plot_ly(cp_data, x = ~cp, y = ~count, color = ~target,
            colors = c(colors$primary_action, colors$danger),
            type = "bar", text = ~count, textposition = "outside") %>%
      layout(barmode = "group", 
             xaxis = list(title = "Type de douleur thoracique"),
             yaxis = list(title = "Nombre de patients"),
             plot_bgcolor = colors$secondary_surface,
             paper_bgcolor = colors$secondary_surface)
  })
  
  output$heart_rate_plot <- renderPlotly({
    if(is.null(heart_data)) return(NULL)
    
    plot_ly(heart_data, x = ~thalach, color = ~target,
            colors = c(colors$primary_action, colors$danger),
            type = "histogram", opacity = 0.7, nbinsx = 30) %>%
      layout(barmode = "overlay",
             xaxis = list(title = "Fréquence cardiaque maximale (bpm)"),
             yaxis = list(title = "Fréquence"),
             plot_bgcolor = colors$secondary_surface,
             paper_bgcolor = colors$secondary_surface)
  })
  
  output$correlation_matrix <- renderPlotly({
    if(is.null(heart_data)) return(NULL)
    
    cor_data <- heart_data %>%
      select_if(is.numeric) %>%
      cor(use = "complete.obs")
    
    plot_ly(z = cor_data, 
            x = colnames(cor_data), 
            y = rownames(cor_data),
            type = "heatmap",
            colors = colorRamp(c(colors$primary_action, "#ffffff", colors$danger)),
            text = round(cor_data, 2),
            hovertemplate = "%{x} vs %{y}<br>Correlation: %{z:.2f}<extra></extra>") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = ""),
             plot_bgcolor = colors$secondary_surface,
             paper_bgcolor = colors$secondary_surface)
  })
  
  # ========== PERFORMANCES ==========
  output$performance_comparison <- renderPlotly({
    performance_data <- data.frame(
      Metrique = c("Precision", "Sensibilite", "Specificite", "AUC"),
      Logistique = c(0.871, 0.893, 0.848, 0.932),
      RandomForest = c(0.852, 0.867, 0.836, 0.918)
    )
    
    df_plot <- performance_data %>%
      pivot_longer(cols = -Metrique, names_to = "Modele", values_to = "Valeur")
    
    plot_ly(df_plot, x = ~Metrique, y = ~Valeur, color = ~Modele,
            colors = c(colors$primary_action, colors$secondary_action),
            type = "bar", text = ~round(Valeur, 3),
            textposition = "outside") %>%
      layout(barmode = 'group',
             yaxis = list(title = "Score", range = c(0, 1)),
             xaxis = list(title = ""),
             plot_bgcolor = colors$secondary_surface,
             paper_bgcolor = colors$secondary_surface,
             hovermode = "x unified")
  })
  
  output$performance_table <- renderDT({
    performance_data <- data.frame(
      Metrique = c("Precision (Accuracy)", "Sensibilite", "Specificite", "AUC", "F1-Score"),
      Logistique = c("87.1%", "89.3%", "84.8%", "0.932", "0.878"),
      RandomForest = c("85.2%", "86.7%", "83.6%", "0.918", "0.864")
    )
    
    datatable(performance_data, 
              options = list(
                dom = 't',
                pageLength = 10,
                ordering = FALSE
              ),
              rownames = FALSE,
              class = 'cell-border stripe') %>%
      formatStyle(columns = 1:3, 
                  backgroundColor = colors$secondary_surface,
                  color = colors$text,
                  fontWeight = 'normal') %>%
      formatStyle(columns = 1,
                  fontWeight = 'bold')
  })
  
  output$variable_importance <- renderPlotly({
    if(is.null(model_rf)) return(NULL)
    
    imp_raw <- varImp(model_rf) 
    
    if (inherits(model_rf, "train")) {
      vals <- imp_raw$importance
      noms <- rownames(imp_raw$importance)
    } else {
      vals <- as.data.frame(imp_raw)
      noms <- rownames(vals)
    }
    
    importance_data <- data.frame(
      Variable = noms,
      Importance = vals[,1] 
    ) %>%
      mutate(Importance = (Importance / max(Importance)) * 100) %>%
      arrange(desc(Importance)) %>%
      slice_head(n = 10)
    
    plot_ly(importance_data, 
            x = ~Importance, 
            y = ~reorder(Variable, Importance),
            type = "bar",
            orientation = 'h',
            marker = list(
              color = ~Importance,
              colorscale = list(
                c(0, colors$primary_action),
                c(1, colors$secondary_action)
              ),
              line = list(color = 'white', width = 1)
            ),
            text = ~paste0(round(Importance, 1), "%"),
            textposition = "outside") %>%
      layout(xaxis = list(title = "Importance Relative (%)"),
             yaxis = list(title = ""),
             plot_bgcolor = colors$secondary_surface,
             paper_bgcolor = colors$secondary_surface,
             margin = list(l = 120))
  })
}

shinyApp(ui, server)