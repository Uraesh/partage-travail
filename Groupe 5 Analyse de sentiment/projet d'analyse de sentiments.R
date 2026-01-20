#########  INSTALLATION DES PACKAGES  ######### 


install.packages("readr")
install.packages("tidyverse")
install.packages("tm")
install.packages("caret")
install.packages("e1071")
install.packages("pROC")
install.packages("ggplot2")
install.packages("text2vec")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("gridExtra")

library(readr)
library(tidyverse)
library(tm)
library(caret)
library(e1071)
library(pROC)
library(ggplot2)
library(text2vec)
library(wordcloud)
library(RColorBrewer)
library(gridExtra)

set.seed(42)

cat("=== ANALYSE DE SENTIMENTS - NAIVE BAYES OPTIMISÉ ===\n\n")



##########  CHARGEMENT ET NETTOYAGE DES DONNÉES  ######### 



load_and_clean_data <- function(sample_size = 10000, balance_classes = TRUE) {
  cat("=== CHARGEMENT DES DONNÉES ===\n")
  
  df_raw <- read.csv(file.choose(), stringsAsFactors = FALSE)
  cat("Taille du dataset brut:", nrow(df_raw), "lignes\n")
  
  df <- df_raw %>% 
    distinct() %>% 
    filter(!is.na(review), !is.na(sentiment)) %>%
    mutate(
      sentiment = tolower(trimws(sentiment)),
      review = as.character(review)
    ) %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    filter(nchar(review) > 10)
  
  cat("Après nettoyage initial:", nrow(df), "lignes\n")
  
  if (balance_classes) {
    min_size <- min(table(df$sentiment))
    cat("Équilibrage - taille par classe:", min_size, "\n")
    df <- df %>% 
      group_by(sentiment) %>% 
      sample_n(min(min_size, sample_size/2)) %>% 
      ungroup()
  }
  
  cat("\nDistribution finale:", nrow(df), "lignes\n")
  print(table(df$sentiment))
  
  return(df)
}

df <- load_and_clean_data(sample_size = 10000, balance_classes = TRUE)



####################  VISUALISATION DES DONNÉES BRUTES  ###################



cat("\n=== VISUALISATIONS DES DONNÉES ===\n")

# Distribution des sentiments
p1 <- ggplot(df, aes(x = sentiment, fill = sentiment)) +
  geom_bar(width = 0.6) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("negative" = "#e74c3c", "positive" = "#2ecc71")) +
  theme_minimal() +
  labs(
    title = "Distribution des Sentiments",
    x = "Sentiment",
    y = "Nombre d'avis"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text = element_text(size = 12)
  )

# Longueur des avis
df$review_length <- nchar(df$review)

p2 <- ggplot(df, aes(x = sentiment, y = review_length, fill = sentiment)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("negative" = "#e74c3c", "positive" = "#2ecc71")) +
  theme_minimal() +
  labs(
    title = "Distribution de la Longueur des Avis",
    x = "Sentiment",
    y = "Nombre de caractères"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text = element_text(size = 12)
  )

# Histogramme de longueur
p3 <- ggplot(df, aes(x = review_length, fill = sentiment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("negative" = "#e74c3c", "positive" = "#2ecc71")) +
  theme_minimal() +
  labs(
    title = "Histogramme de la Longueur des Avis",
    x = "Nombre de caractères",
    y = "Fréquence",
    fill = "Sentiment"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "top"
  )

grid.arrange(p1, p2, p3, ncol = 2, nrow = 2)



####################  NETTOYAGE DE TEXTE  ################### 



clean_text_optimized <- function(text) {
  text <- tolower(text)
  text <- gsub("<br\\s*/?>", " ", text, ignore.case = TRUE)
  text <- gsub("<[^>]+>", " ", text)
  
  # Contractions courantes
  text <- gsub("won't", "will not", text)
  text <- gsub("can't", "cannot", text)
  text <- gsub("n't\\b", " not", text)
  text <- gsub("'ll", " will", text)
  text <- gsub("'re", " are", text)
  text <- gsub("'ve", " have", text)
  text <- gsub("'m", " am", text)
  text <- gsub("'d", " would", text)
  
  # Ponctuation et espaces
  text <- gsub("[^a-z0-9\\s]", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  
  return(text)
}

cat("\n=== NETTOYAGE DU TEXTE ===\n")
df$clean_review <- clean_text_optimized(df$review)
df <- df %>% filter(nchar(clean_review) > 5)
cat("Lignes après nettoyage:", nrow(df), "\n")



####################  WORD CLOUDS  ################### 



cat("\n=== CRÉATION DES WORD CLOUDS ===\n")

create_wordcloud <- function(text_vector, title, color_palette) {
  corpus <- Corpus(VectorSource(text_vector))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  
  stop_words <- setdiff(
    stopwords("english"),
    c("no", "not", "never", "bad", "good", "great", "terrible", "worst", "best")
  )
  corpus <- tm_map(corpus, removeWords, stop_words)
  corpus <- tm_map(corpus, stripWhitespace)
  
  tdm <- TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  
  wordcloud(
    words = names(word_freqs),
    freq = word_freqs,
    min.freq = 5,
    max.words = 100,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, color_palette),
    scale = c(4, 0.5)
  )
  title(main = title, cex.main = 1.5, font.main = 2)
}

# Word Cloud pour avis positifs
par(mfrow = c(1, 2))
positive_text <- df %>% filter(sentiment == "positive") %>% pull(clean_review)
create_wordcloud(positive_text, "Avis POSITIFS", "Greens")

# Word Cloud pour avis négatifs
negative_text <- df %>% filter(sentiment == "negative") %>% pull(clean_review)
create_wordcloud(negative_text, "Avis NÉGATIFS", "Reds")

par(mfrow = c(1, 1))



#####################  ANALYSE DES MOTS FRÉQUENTS  ####################



cat("\n=== ANALYSE DES MOTS FRÉQUENTS ===\n")

get_top_words <- function(text_vector, n = 20) {
  corpus <- Corpus(VectorSource(text_vector))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  
  stop_words <- setdiff(
    stopwords("english"),
    c("no", "not", "never", "bad", "good", "great", "terrible", "worst", "best")
  )
  corpus <- tm_map(corpus, removeWords, stop_words)
  
  tdm <- TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  
  data.frame(
    word = names(word_freqs)[1:n],
    freq = word_freqs[1:n],
    row.names = NULL
  )
}

# Top mots positifs
top_positive <- get_top_words(positive_text, 20)
top_positive$sentiment <- "Positive"

# Top mots négatifs
top_negative <- get_top_words(negative_text, 20)
top_negative$sentiment <- "Negative"

# Filtrer les mots communs pour ne garder que les plus distinctifs
common_words <- intersect(top_positive$word, top_negative$word)

# Garder top 15 après filtrage
top_positive_clean <- top_positive %>%
  filter(!word %in% c("film", "movie", "one")) %>%
  head(15)

top_negative_clean <- top_negative %>%
  filter(!word %in% c("film", "movie", "one")) %>%
  head(15)

# Visualisation des mots fréquents
p_pos <- ggplot(top_positive_clean, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#2ecc71", alpha = 0.8) +
  geom_text(aes(label = freq), hjust = -0.2, size = 3.5) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 15 Mots Distinctifs - Avis POSITIFS",
    x = "",
    y = "Fréquence"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

p_neg <- ggplot(top_negative_clean, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#e74c3c", alpha = 0.8) +
  geom_text(aes(label = freq), hjust = -0.2, size = 3.5) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 15 Mots Distinctifs - Avis NÉGATIFS",
    x = "",
    y = "Fréquence"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

grid.arrange(p_pos, p_neg, ncol = 2)



#####################  DIVISION DES DONNÉES  ####################



split_data <- function(data, train_ratio = 0.8) {
  idx <- createDataPartition(data$sentiment, p = train_ratio, list = FALSE)
  
  list(
    train_text = data$clean_review[idx],
    test_text  = data$clean_review[-idx],
    y_train    = factor(data$sentiment[idx], levels = c("negative", "positive")),
    y_test     = factor(data$sentiment[-idx], levels = c("negative", "positive"))
  )
}

split <- split_data(df, train_ratio = 0.8)
train_text <- split$train_text
test_text  <- split$test_text
y_train    <- split$y_train
y_test     <- split$y_test

cat("\n=== DIVISION DES DONNÉES ===\n")
cat("Train:", length(train_text), " | Test:", length(test_text), "\n")



#####################  CRÉATION TF-IDF OPTIMISÉE  ####################



create_tfidf_matrix <- function(train_text, test_text) {
  
  cat("\n=== CRÉATION TF-IDF ===\n")
  start_time <- Sys.time()
  
  # Stopwords optimisés
  stop_words <- setdiff(
    stopwords("english"), 
    c("no", "not", "never", "none", "nothing", "nor", "neither", 
      "nobody", "nowhere", "but", "very", "too", "so", "more", "most",
      "good", "bad", "great", "terrible", "worst", "best")
  )
  
  # Création du vocabulaire avec bi-grammes
  it_train <- itoken(train_text, progressbar = FALSE)
  
  vocab <- create_vocabulary(
    it_train,
    ngram = c(1, 2),
    stopwords = stop_words
  )
  
  cat("Vocabulaire initial:", nrow(vocab), "termes\n")
  
  # Pruning optimisé
  vocab <- prune_vocabulary(
    vocab,
    term_count_min = 5,
    doc_proportion_max = 0.8,
    doc_proportion_min = 0.001
  )
  
  cat("Après pruning:", nrow(vocab), "termes\n")
  
  # Limitation à 5000 features pour rapidité
  if (nrow(vocab) > 5000) {
    vocab <- vocab %>% 
      arrange(desc(term_count)) %>% 
      slice(1:5000)
  }
  
  cat("Features finales:", nrow(vocab), "\n")
  
  # Vectorizer
  vectorizer <- vocab_vectorizer(vocab)
  
  # DTM Train
  dtm_train <- create_dtm(it_train, vectorizer)
  
  # TF-IDF
  tfidf_model <- TfIdf$new()
  dtm_train_tfidf <- fit_transform(dtm_train, tfidf_model)
  
  # DTM Test
  it_test <- itoken(test_text, progressbar = FALSE)
  dtm_test <- create_dtm(it_test, vectorizer)
  dtm_test_tfidf <- transform(dtm_test, tfidf_model)
  
  cat("Dimensions Train:", dim(dtm_train_tfidf), "\n")
  cat("Dimensions Test:", dim(dtm_test_tfidf), "\n")
  cat("Temps:", round(difftime(Sys.time(), start_time, units = "secs"), 2), "sec\n")
  
  list(
    X_train = dtm_train_tfidf,
    X_test = dtm_test_tfidf,
    vocab = vocab,
    vectorizer = vectorizer,
    tfidf_model = tfidf_model
  )
}



#####################  ENTRAÎNEMENT NAIVE BAYES OPTIMISÉ  ####################



cat("\n=== ENTRAÎNEMENT NAIVE BAYES ===\n")
start_time <- Sys.time()

# Création TF-IDF
tfidf <- create_tfidf_matrix(train_text, test_text)

X_train <- as.matrix(tfidf$X_train)
X_test <- as.matrix(tfidf$X_test)

# Binarisation pour Naive Bayes
X_train_binary <- X_train
X_train_binary[X_train_binary > 0] <- 1

X_test_binary <- X_test
X_test_binary[X_test_binary > 0] <- 1

# Entraînement avec laplace optimal
model <- naiveBayes(
  x = X_train_binary,
  y = y_train,
  laplace = 1
)

cat("Temps d'entraînement:", round(difftime(Sys.time(), start_time, units = "secs"), 2), "sec\n")



#####################  ÉVALUATION DU MODÈLE  ####################



cat("\n=== ÉVALUATION DU MODÈLE ===\n")

# Prédictions
pred <- predict(model, X_test_binary)
pred_prob <- predict(model, X_test_binary, type = "raw")

# Matrice de confusion
conf <- confusionMatrix(pred, y_test, positive = "positive")
print(conf)

# Courbe ROC
roc_obj <- roc(as.numeric(y_test) - 1, pred_prob[, 2], quiet = TRUE)

cat("\nPERFORMANCES:\n")
cat("  Accuracy:  ", round(conf$overall['Accuracy'], 4), "\n")
cat("  Precision: ", round(conf$byClass['Precision'], 4), "\n")
cat("  Recall:    ", round(conf$byClass['Sensitivity'], 4), "\n")
cat("  F1-Score:  ", round(conf$byClass['F1'], 4), "\n")
cat("  AUC:       ", round(auc(roc_obj), 4), "\n")



#####################  VISUALISATIONS DES RÉSULTATS  ####################



cat("\n=== VISUALISATIONS DES RÉSULTATS ===\n")

# Matrice de confusion visuelle
conf_df <- as.data.frame(conf$table)

ggplot(conf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 10, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#3498db", high = "#e74c3c") +
  theme_minimal() +
  labs(
    title = "Matrice de Confusion",
    x = "Valeur Réelle",
    y = "Prédiction"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    legend.position = "right"
  )

# Courbe ROC
plot(roc_obj, 
     main = paste0("Courbe ROC\nAUC = ", round(auc(roc_obj), 4)),
     col = "#3498db", 
     lwd = 3,
     print.auc = FALSE,
     cex.main = 1.5,
     cex.lab = 1.2)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Distribution des probabilités prédites
pred_df <- data.frame(
  Probabilite = pred_prob[, 2],
  Reel = y_test,
  Correct = pred == y_test
)

ggplot(pred_df, aes(x = Probabilite, fill = Reel)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "black", size = 1) +
  scale_fill_manual(values = c("negative" = "#e74c3c", "positive" = "#2ecc71")) +
  theme_minimal() +
  labs(
    title = "Distribution des Probabilités Prédites",
    x = "Probabilité d'être Positif",
    y = "Fréquence",
    fill = "Sentiment Réel"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "top"
  )

# Métriques par classe
metrics_df <- data.frame(
  Metrique = c("Accuracy", "Precision", "Recall", "F1-Score", "AUC"),
  Score = c(
    conf$overall['Accuracy'],
    conf$byClass['Precision'],
    conf$byClass['Sensitivity'],
    conf$byClass['F1'],
    as.numeric(auc(roc_obj))
  )
)

ggplot(metrics_df, aes(x = reorder(Metrique, Score), y = Score)) +
  geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
  geom_text(aes(label = round(Score, 3)), hjust = -0.1, size = 5) +
  coord_flip() +
  ylim(0, 1) +
  theme_minimal() +
  labs(
    title = "Performance du Modèle Naive Bayes",
    x = "Métrique",
    y = "Score"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12)
  )


#####################  SAUVEGARDE DU MODÈLE  ####################



cat("\n=== SAUVEGARDE DU MODÈLE ===\n")

pipeline <- list(
  model = model,
  tfidf = tfidf,
  performance = list(
    accuracy = conf$overall['Accuracy'],
    precision = conf$byClass['Precision'],
    recall = conf$byClass['Sensitivity'],
    f1 = conf$byClass['F1'],
    auc = as.numeric(auc(roc_obj))
  ),
  conf_matrix = conf,
  roc = roc_obj,
  creation_date = Sys.time()
)

saveRDS(pipeline, "naive_bayes_sentiment_model.rds")
cat(" Modèle sauvegardé: naive_bayes_sentiment_model.rds\n")



#####################  FONCTION DE PRÉDICTION  ####################



predict_sentiment <- function(new_texts, pipeline) {
  cat("\n=== PRÉDICTION SUR NOUVEAUX TEXTES ===\n")
  
  # Nettoyage
  clean_texts <- clean_text_optimized(new_texts)
  
  # Vectorisation
  it_new <- itoken(clean_texts, progressbar = FALSE)
  dtm_new <- create_dtm(it_new, pipeline$tfidf$vectorizer)
  dtm_new_tfidf <- transform(dtm_new, pipeline$tfidf$tfidf_model)
  
  X_new <- as.matrix(dtm_new_tfidf)
  X_new[X_new > 0] <- 1  # Binarisation
  
  # Prédiction
  predictions <- predict(pipeline$model, X_new)
  probabilities <- predict(pipeline$model, X_new, type = "raw")
  
  results <- data.frame(
    Texte = substr(new_texts, 1, 60),
    Prediction = predictions,
    Prob_Negative = round(probabilities[, 1], 3),
    Prob_Positive = round(probabilities[, 2], 3),
    Confiance = round(apply(probabilities, 1, max), 3),
    stringsAsFactors = FALSE
  )
  
  print(results)
  return(results)
}

# =============================================================================
# 13. TESTS SUR NOUVEAUX AVIS
# =============================================================================

cat("\n=== TESTS SUR NOUVEAUX AVIS ===\n")

test_reviews <- c(
  "This movie was absolutely wonderful and inspiring!",
  "Worst film I've ever seen, complete waste of time",
  "The acting was decent but the plot was confusing",
  "I love this movie so much, it's amazing!",
  "Terrible, boring, predictable. Would not recommend",
  "Not bad, but not great either. Just average",
  "Outstanding performance! A masterpiece!",
  "Disappointing and underwhelming",
  "This exceeded all my expectations",
  "I fell asleep halfway through",
  "I love this movie",
  "best movie of the year",
  "I hate this movie",
  "I don't like the actor"
)

final_predictions <- predict_sentiment(test_reviews, pipeline)



#####################  RÉSUMÉ FINAL  ####################



cat("\n", strrep("=", 80), "\n")
cat("=== RÉSUMÉ FINAL ===\n")
cat(strrep("=", 80), "\n\n")
cat(" MODÈLE: Naive Bayes avec TF-IDF bi-grammes\n\n")
cat(" PERFORMANCES:\n")
cat("  Accuracy:  ", round(pipeline$performance$accuracy, 4), "\n")
cat("  Precision: ", round(pipeline$performance$precision, 4), "\n")
cat("  Recall:    ", round(pipeline$performance$recall, 4), "\n")
cat("  F1-Score:  ", round(pipeline$performance$f1, 4), "\n")
cat("  AUC:       ", round(pipeline$performance$auc, 4), "\n\n")
cat(" CONFIGURATION:\n")
cat("  Features:  ", ncol(X_train), "\n")
cat("  Laplace:   ", 1, "\n")
cat("  N-grammes: ", "1-2", "\n")
cat("  Train size:", length(y_train), "\n")
cat("  Test size: ", length(y_test), "\n\n")
cat(" Fichier sauvegardé: naive_bayes_sentiment_model.rds\n\n")
cat("PIPELINE TERMINÉ AVEC SUCCÈS! \n")
cat(strrep("=", 80), "\n")