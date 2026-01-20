################### Library necessaire ###################


library(shiny)
library(shinydashboard)
library(text2vec)
library(e1071)
library(DT)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(tidyverse)
library(gridExtra)


#################### CHARGEMENT DU PIPELINE ###################


pipeline <- readRDS("naive_bayes_sentiment_model.rds")

cat("=== PIPELINE CHARGÉ ===\n")
cat("Date de création:", as.character(pipeline$creation_date), "\n")
cat("Accuracy:", round(pipeline$performance$accuracy, 4), "\n")
cat("F1-Score:", round(pipeline$performance$f1, 4), "\n")



########## FONCTION DE PREPROCESSING (IDENTIQUE À L'ENTRAÎNEMENT) #########



clean_text_optimized <- function(text) {
  text <- tolower(text)
  text <- gsub("<br\\s*/?>", " ", text, ignore.case = TRUE)
  text <- gsub("<[^>]+>", " ", text)
  
  text <- gsub("won't", "will not", text)
  text <- gsub("can't", "cannot", text)
  text <- gsub("n't\\b", " not", text)
  text <- gsub("'ll", " will", text)
  text <- gsub("'re", " are", text)
  text <- gsub("'ve", " have", text)
  text <- gsub("'m", " am", text)
  text <- gsub("'d", " would", text)
  
  text <- gsub("[^a-z0-9\\s]", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  
  return(text)
}


#################### FONCTION DE PRÉDICTION ###################



predict_sentiment <- function(texts, pipeline) {
  clean_texts <- sapply(texts, clean_text_optimized)
  
  it_new <- itoken(clean_texts, progressbar = FALSE)
  dtm_new <- create_dtm(it_new, pipeline$tfidf$vectorizer)
  dtm_new_tfidf <- transform(dtm_new, pipeline$tfidf$tfidf_model)
  
  X_new <- as.matrix(dtm_new_tfidf)
  X_new[X_new > 0] <- 1
  
  predictions <- predict(pipeline$model, X_new, type = "class")
  probabilities <- predict(pipeline$model, X_new, type = "raw")
  
  results <- data.frame(
    original_text = texts,
    cleaned_text = clean_texts,
    prediction = as.character(predictions),
    prob_negative = probabilities[, "negative"],
    prob_positive = probabilities[, "positive"],
    confidence = apply(probabilities, 1, max),
    stringsAsFactors = FALSE
  )
  
  return(results)
}


#################### FONCTION POUR EXTRAIRE LES TOP TERMES ###################



get_top_terms_from_vocab <- function(vocab, sentiment_type, n = 20) {
  # Utiliser le vocabulaire sauvegardé dans le pipeline
  vocab_df <- pipeline$tfidf$vocab
  
  # Trier par fréquence et prendre le top N
  top_terms <- vocab_df %>%
    arrange(desc(term_count)) %>%
    head(n * 3)  # Prendre plus pour filtrer ensuite
  
  # Filtrer les mots génériques
  generic_words <- c("film", "movie", "one", "get", "like", "just", "time", "really", "see", "made")
  
  top_terms <- top_terms %>%
    filter(!term %in% generic_words) %>%
    head(n)
  
  return(top_terms)
}



#################### FONCTION POUR CRÉER WORD CLOUD ###################



create_wordcloud_from_vocab <- function(vocab_df, color_palette) {
  if (nrow(vocab_df) == 0) {
    plot.new()
    text(0.5, 0.5, "Pas de données disponibles", cex = 1.5)
    return()
  }
  
  wordcloud(
    words = vocab_df$term,
    freq = vocab_df$term_count,
    min.freq = 1,
    max.words = 100,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, color_palette),
    scale = c(3, 0.5)
  )
}



#################### INTERFACE UTILISATEUR ###################



ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Sentiment Analysis - Naive Bayes",
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Prédiction", tabName = "prediction", icon = icon("search")),
      menuItem("Batch Analysis", tabName = "batch", icon = icon("table")),
      menuItem("Analyse des Termes", tabName = "terms", icon = icon("chart-bar")),
      menuItem("Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .positive-box {
          background-color: #d4edda;
          border: 2px solid #2ecc71;
          border-radius: 5px;
          padding: 15px;
          margin: 10px 0;
        }
        .negative-box {
          background-color: #f8d7da;
          border: 2px solid #e74c3c;
          border-radius: 5px;
          padding: 15px;
          margin: 10px 0;
        }
      "))
    ),
    
    tabItems(
      # TAB 1: DASHBOARD
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("accuracy_box", width = 3),
          valueBoxOutput("precision_box", width = 3),
          valueBoxOutput("recall_box", width = 3),
          valueBoxOutput("f1_box", width = 3)
        ),
        fluidRow(
          box(
            title = "Informations du Modèle",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("model_info")
          ),
          box(
            title = "Matrice de Confusion",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("confusion_matrix", height = 300)
          )
        ),
        fluidRow(
          box(
            title = "Courbe ROC",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("roc_curve", height = 300)
          ),
          box(
            title = "Métriques de Performance",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotOutput("metrics_plot", height = 300)
          )
        )
      ),
      
      # TAB 2: PRÉDICTION
      tabItem(
        tabName = "prediction",
        fluidRow(
          box(
            title = "Texte à analyser",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            textAreaInput(
              "text_input",
              label = NULL,
              placeholder = "Entrez votre texte ici (ex: This movie was absolutely amazing!)",
              height = "150px",
              width = "100%"
            ),
            actionButton("predict_btn", "Analyser", 
                         icon = icon("play"), 
                         class = "btn-primary btn-lg")
          )
        ),
        fluidRow(
          uiOutput("result_box")
        ),
        fluidRow(
          box(
            title = "Détails de l'Analyse",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            verbatimTextOutput("analysis_details")
          )
        )
      ),
      
      # TAB 3: BATCH ANALYSIS
      tabItem(
        tabName = "batch",
        fluidRow(
          box(
            title = "Analyse Multiple",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            textAreaInput(
              "batch_input",
              "Entrez plusieurs textes (un par ligne)",
              height = "200px",
              placeholder = "This movie was great\nI hated this film\nAbsolutely wonderful"
            ),
            actionButton("batch_predict_btn", "Analyser Tout", 
                         icon = icon("list"), 
                         class = "btn-success btn-lg")
          )
        ),
        fluidRow(
          box(
            title = "Résultats",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("batch_results_table")
          )
        ),
        fluidRow(
          box(
            title = "Distribution des Sentiments",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("batch_distribution")
          ),
          box(
            title = "Confiance Moyenne",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotOutput("batch_confidence")
          )
        )
      ),
      
      # TAB 4: ANALYSE DES TERMES
      tabItem(
        tabName = "terms",
        fluidRow(
          box(
            title = "Top 20 Termes du Vocabulaire",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotOutput("top_terms_plot", height = 500)
          )
        ),
        fluidRow(
          box(
            title = "Word Cloud - Vocabulaire",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput("wordcloud_vocab", height = 400)
          )
        )
      ),
      
      # TAB 5: PERFORMANCE
      tabItem(
        tabName = "performance",
        fluidRow(
          box(
            title = "Matrice de Confusion Détaillée",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("conf_matrix_detailed")
          )
        ),
        fluidRow(
          box(
            title = "Informations sur le ROC",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("roc_info")
          )
        )
      ),
      
      # TAB 6: ABOUT
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "À propos de cette application",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h4("Modèle Naive Bayes avec TF-IDF"),
            p("Cette application utilise un modèle Naive Bayes optimisé avec vectorisation TF-IDF et bi-grammes pour l'analyse de sentiments."),
            hr(),
            h4("Caractéristiques:"),
            tags$ul(
              tags$li("Prétraitement optimisé du texte"),
              tags$li("TF-IDF avec bi-grammes (1-2)"),
              tags$li("Naive Bayes avec lissage de Laplace"),
              tags$li("Analyse en temps réel et par batch"),
              tags$li("Visualisation des termes les plus fréquents")
            ),
            hr(),
            h4("Performance du Modèle:"),
            verbatimTextOutput("performance_summary")
          )
        )
      )
    )
  )
)


#################### SERVEUR ###################


server <- function(input, output, session) {
  
  # --- VALUE BOXES ---
  output$accuracy_box <- renderValueBox({
    valueBox(
      value = paste0(round(pipeline$performance$accuracy * 100, 1), "%"),
      subtitle = "Accuracy",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$precision_box <- renderValueBox({
    valueBox(
      value = paste0(round(pipeline$performance$precision * 100, 1), "%"),
      subtitle = "Precision",
      icon = icon("crosshairs"),
      color = "blue"
    )
  })
  
  output$recall_box <- renderValueBox({
    valueBox(
      value = paste0(round(pipeline$performance$recall * 100, 1), "%"),
      subtitle = "Recall",
      icon = icon("sync"),
      color = "purple"
    )
  })
  
  output$f1_box <- renderValueBox({
    valueBox(
      value = paste0(round(pipeline$performance$f1 * 100, 1), "%"),
      subtitle = "F1-Score",
      icon = icon("balance-scale"),
      color = "orange"
    )
  })
  
  # --- MODEL INFO ---
  output$model_info <- renderPrint({
    cat("Type de Modèle: Naive Bayes (Multinomial)\n")
    cat("Date de création:", as.character(pipeline$creation_date), "\n\n")
    cat("Caractéristiques:\n")
    cat("- N-grammes: 1-2\n")
    cat("- Lissage de Laplace: 1\n")
    cat("- Vectorisation: TF-IDF\n")
    cat("- Features:", nrow(pipeline$tfidf$vocab), "\n")
  })
  
  # --- CONFUSION MATRIX ---
  output$confusion_matrix <- renderPlot({
    conf_df <- as.data.frame(pipeline$conf_matrix$table)
    
    ggplot(conf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Freq), size = 10, color = "white", fontface = "bold") +
      scale_fill_gradient(low = "#3498db", high = "#e74c3c") +
      theme_minimal() +
      labs(title = "Matrice de Confusion", x = "Valeur Réelle", y = "Prédiction") +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  })
  
  # --- ROC CURVE ---
  output$roc_curve <- renderPlot({
    plot(pipeline$roc, 
         main = paste0("Courbe ROC (AUC = ", round(pipeline$performance$auc, 4), ")"),
         col = "#3498db", 
         lwd = 3,
         cex.main = 1.2)
    abline(a = 0, b = 1, lty = 2, col = "gray")
  })
  
  # --- METRICS PLOT ---
  output$metrics_plot <- renderPlot({
    metrics_df <- data.frame(
      Metrique = c("Accuracy", "Precision", "Recall", "F1-Score"),
      Score = c(
        pipeline$performance$accuracy,
        pipeline$performance$precision,
        pipeline$performance$recall,
        pipeline$performance$f1
      )
    )
    
    ggplot(metrics_df, aes(x = reorder(Metrique, Score), y = Score)) +
      geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
      geom_text(aes(label = round(Score, 3)), hjust = -0.1, size = 5) +
      coord_flip() +
      ylim(0, 1) +
      theme_minimal() +
      labs(title = "Métriques de Performance", x = "", y = "Score") +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  })
  
  # --- PRÉDICTION SIMPLE ---
  prediction_result <- eventReactive(input$predict_btn, {
    req(input$text_input)
    if (nchar(trimws(input$text_input)) == 0) {
      return(NULL)
    }
    predict_sentiment(input$text_input, pipeline)
  })
  
  output$result_box <- renderUI({
    result <- prediction_result()
    req(result)
    
    box_class <- if(result$prediction == "positive") "positive-box" else "negative-box"
    sentiment_emoji <- if(result$prediction == "positive") "😊" else "😞"
    sentiment_color <- if(result$prediction == "positive") "#2ecc71" else "#e74c3c"
    
    tags$div(
      class = box_class,
      h2(paste(sentiment_emoji, toupper(result$prediction)), 
         style = paste0("color: ", sentiment_color, "; text-align: center;")),
      hr(),
      h4("Confiance:", paste0(round(result$confidence * 100, 1), "%"), 
         style = "text-align: center;"),
      hr(),
      fluidRow(
        column(6, 
               h5("Probabilité Positive:"),
               h4(paste0(round(result$prob_positive * 100, 1), "%"), 
                  style = "color: #2ecc71;")
        ),
        column(6,
               h5("Probabilité Négative:"),
               h4(paste0(round(result$prob_negative * 100, 1), "%"), 
                  style = "color: #e74c3c;")
        )
      )
    )
  })
  
  output$analysis_details <- renderPrint({
    result <- prediction_result()
    req(result)
    
    cat("=== DÉTAILS DE L'ANALYSE ===\n\n")
    cat("Texte original:\n", result$original_text, "\n\n")
    cat("Texte nettoyé:\n", result$cleaned_text, "\n\n")
    cat("Prédiction:", result$prediction, "\n")
    cat("Confiance:", round(result$confidence * 100, 2), "%\n")
  })
  
  # --- BATCH ANALYSIS ---
  batch_results <- eventReactive(input$batch_predict_btn, {
    req(input$batch_input)
    texts <- strsplit(input$batch_input, "\n")[[1]]
    texts <- texts[nchar(trimws(texts)) > 0]
    
    if (length(texts) == 0) return(NULL)
    
    results <- predict_sentiment(texts, pipeline)
    
    results$Texte <- substr(results$original_text, 1, 100)
    results$Sentiment <- results$prediction
    results$Confiance <- results$confidence
    results$Prob_Positive <- results$prob_positive
    results$Prob_Negative <- results$prob_negative
    
    results[, c("Texte", "Sentiment", "Confiance", "Prob_Positive", "Prob_Negative")]
  })
  
  output$batch_results_table <- renderDT({
    datatable(
      batch_results(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatPercentage(c("Confiance", "Prob_Positive", "Prob_Negative"), 1)
  })
  
  output$batch_distribution <- renderPlot({
    results <- batch_results()
    req(results)
    
    ggplot(results, aes(x = Sentiment, fill = Sentiment)) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("negative" = "#e74c3c", "positive" = "#2ecc71")) +
      theme_minimal() +
      labs(title = "Distribution des Sentiments", y = "Nombre") +
      theme(legend.position = "none", plot.title = element_text(face = "bold"))
  })
  
  output$batch_confidence <- renderPlot({
    results <- batch_results()
    req(results)
    
    ggplot(results, aes(x = Sentiment, y = Confiance, fill = Sentiment)) +
      geom_boxplot() +
      scale_fill_manual(values = c("negative" = "#e74c3c", "positive" = "#2ecc71")) +
      theme_minimal() +
      labs(title = "Distribution de la Confiance", y = "Confiance") +
      theme(legend.position = "none", plot.title = element_text(face = "bold"))
  })
  
  # --- ANALYSE DES TERMES ---
  output$top_terms_plot <- renderPlot({
    top_terms <- get_top_terms_from_vocab(pipeline$tfidf$vocab, "all", 20)
    
    ggplot(top_terms, aes(x = reorder(term, term_count), y = term_count)) +
      geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
      geom_text(aes(label = term_count), hjust = -0.2, size = 3.5) +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Top 20 Termes du Vocabulaire",
        x = "Terme",
        y = "Fréquence"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 11, face = "bold")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  output$wordcloud_vocab <- renderPlot({
    create_wordcloud_from_vocab(pipeline$tfidf$vocab, "Blues")
  })
  
  # --- PERFORMANCE TAB ---
  output$conf_matrix_detailed <- renderPrint({
    print(pipeline$conf_matrix)
  })
  
  output$roc_info <- renderPrint({
    cat("=== INFORMATIONS SUR LA COURBE ROC ===\n\n")
    cat("AUC (Area Under Curve):", round(pipeline$performance$auc, 4), "\n\n")
    cat("Interprétation:\n")
    cat("- AUC = 0.5: Modèle aléatoire\n")
    cat("- 0.7 < AUC < 0.8: Acceptable\n")
    cat("- 0.8 < AUC < 0.9: Excellent\n")
    cat("- AUC > 0.9: Exceptionnel\n\n")
    cat("Notre modèle: ", 
        if(pipeline$performance$auc > 0.9) "Exceptionnel" 
        else if(pipeline$performance$auc > 0.8) "Excellent"
        else if(pipeline$performance$auc > 0.7) "Acceptable"
        else "À améliorer", "\n")
  })
  
  # --- ABOUT TAB ---
  output$performance_summary <- renderPrint({
    cat("Accuracy: ", round(pipeline$performance$accuracy * 100, 2), "%\n")
    cat("Precision:", round(pipeline$performance$precision * 100, 2), "%\n")
    cat("Recall:   ", round(pipeline$performance$recall * 100, 2), "%\n")
    cat("F1-Score: ", round(pipeline$performance$f1 * 100, 2), "%\n")
    cat("AUC:      ", round(pipeline$performance$auc, 4), "\n")
  })
}


########## LANCEMENT DE L'APPLICATION ##########


shinyApp(ui = ui, server = server)