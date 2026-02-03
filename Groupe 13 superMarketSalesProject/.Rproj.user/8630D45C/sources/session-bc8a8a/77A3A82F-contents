library(shiny)
library(tidyverse)
library(bslib)
library(plotly) # Pour des graphiques qui s'adaptent mieux au redimensionnement

# --- Préparation du modèle ---
modele_ventes <- lm(Sales ~ `Unit price` + Quantity + `Product line`, data = df_clean)

ui <- page_sidebar(
  title = "📊 Pilotage de Rentabilité Commerciale",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Configuration de la barre latérale (Sidebar)
  sidebar = sidebar(
    title = "Paramètres d'analyse",
    width = 350,
    
    accordion(
      accordion_panel(
        "1. Configuration Panier",
        icon = icon("shopping-cart"),
        sliderInput("price", "Prix unitaire moyen ($)", min = 10, max = 100, value = 50),
        sliderInput("qty", "Quantité moyenne / client", min = 1, max = 10, value = 5),
        selectInput("prod", "Rayon principal", choices = levels(df_clean$`Product line`))
      ),
      accordion_panel(
        "2. Volume & Objectifs",
        icon = icon("chart-line"),
        numericInput("n_clients", "Nombre de clients / JOUR :", value = 100, min = 1),
        numericInput("costs", "Coûts d'exploitation / JOUR ($) :", value = 25000, min = 0),
        selectInput("periode", "Période d'analyse :", 
                    choices = c("Jour" = 1, "Mois (30j)" = 30, "An (365j)" = 365), selected = 30)
      )
    )
  ),
  
  # Corps principal (Main Content)
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Panier Moyen",
      value = textOutput("panier_moyen"),
      showcase = icon("tag"),
      theme = "secondary"
    ),
    value_box(
      title = "CA Prévisionnel",
      value = textOutput("ca_total"),
      showcase = icon("dollar-sign"),
      theme = "primary"
    ),
    uiOutput("boite_rentabilite")
  ),
  
  card(
    full_screen = TRUE,
    card_header("Évolution des revenus vs Coûts cumulés"),
    plotOutput("graphique_renta", height = "450px")
  )
)

server <- function(input, output) {
  
  # Calcul du panier moyen prédit
  panier_val <- reactive({
    predict(modele_ventes, data.frame(
      `Unit price` = input$price, 
      Quantity = input$qty, 
      `Product line` = input$prod, 
      check.names = FALSE
    ))
  })
  
  # Calculs financiers
  ca_global <- reactive({ panier_val() * input$n_clients * as.numeric(input$periode) })
  couts_totaux <- reactive({ input$costs * as.numeric(input$periode) })
  est_rentable <- reactive({ ca_global() >= couts_totaux() })
  
  output$panier_moyen <- renderText({ paste0(round(panier_val(), 2), " $") })
  
  output$ca_total <- renderText({ 
    format(round(ca_global(), 0), big.mark = " ", suffix = " $") 
  })
  
  # Rectangle de rentabilité adaptatif
  output$boite_rentabilite <- renderUI({
    statut <- if(est_rentable()) "RENTABLE" else "DÉFICITAIRE"
    theme_color <- if(est_rentable()) "success" else "danger"
    icon_box <- if(est_rentable()) icon("check-circle") else icon("exclamation-triangle")
    
    value_box(
      title = "Statut Financier",
      value = statut,
      showcase = icon_box,
      theme = theme_color
    )
  })
  
  output$graphique_renta <- renderPlot({
    jours <- 1:as.numeric(input$periode)
    df_plot <- data.frame(
      Temps = jours,
      CA = jours * (panier_val() * input$n_clients),
      Seuil = jours * input$costs
    )
    
    ggplot(df_plot, aes(x = Temps)) +
      geom_area(aes(y = CA, fill = "Chiffre d'Affaires"), alpha = 0.2) +
      geom_line(aes(y = CA, color = "Chiffre d'Affaires"), size = 1.2) +
      geom_line(aes(y = Seuil, color = "Seuil de Rentabilité (Coûts)"), size = 1, linetype = "dashed") +
      scale_fill_manual(values = c("Chiffre d'Affaires" = "#2c3e50")) +
      scale_color_manual(values = c("Chiffre d'Affaires" = "#2c3e50", 
                                    "Seuil de Rentabilité (Coûts)" = "#e74c3c")) +
      scale_y_continuous(labels = scales::label_dollar()) +
      labs(x = "Nombre de jours", y = "Montant cumulé", fill = "", color = "") +
      theme_minimal() +
      theme(legend.position = "bottom", text = element_text(size = 14))
  })
}

shinyApp(ui, server)