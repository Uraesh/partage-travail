# Importations des données

library(readr)

students <- read_csv("StudentsPerformance.csv")

# structure des data
str(students)


# Afficher les premières lignes
head(students)

# Vérifier les dimensions
dim(students)

# Nettoyage des données
# Vérifier les valeurs manquantes
colSums(is.na(students))

sum(duplicated(students))

names(students)

# Convertir les colonnes catégorielles en facteurs
library(dplyr)
students <- students %>%
  mutate(
    gender = as.factor(gender),
    `race/ethnicity` = as.factor(`race/ethnicity`),
    `parental level of education` = as.factor(`parental level of education`),
    lunch = as.factor(lunch),
    `test preparation course` = as.factor(`test preparation course`)
  )
# Renommer les colonnes
students <- students %>%
  rename(
    race_ethnicity = `race/ethnicity`,
    parental_education = `parental level of education`,
    test_prep = `test preparation course`,
    math_score = `math score`,
    reading_score = `reading score`,
    writing_score = `writing score`
  )

# Créer un score total et une moyenne
students <- students %>%
  mutate(
    total_score = math_score + reading_score + writing_score,
    average_score = total_score / 3
  )

#Visualisation
library(dplyr)
library(ggplot2)

# Histogramme des scores moyens
ggplot(students, aes(x = average_score)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution des Scores Moyens", x = "Score Moyen", y = "Fréquence") +
  theme_minimal()

# Boxplot par genre
ggplot(students, aes(x = gender, y = average_score, fill = gender)) +
  geom_boxplot() +
  labs(title = "Comparaison des Scores Moyens par Genre", 
       x = "Genre", y = "Score Moyen") +
  theme_minimal()

# Relation entre les différents scores
pairs(students %>% select(math_score, reading_score, writing_score))

# l'impact du niveau d'éducation des parents
# Visualisation par catégorie

ggplot(students, aes(x = parental_education, y = average_score, fill = parental_education)) +
  geom_boxplot() +
  labs(title = "Scores selon le Niveau d'Éducation des Parents",
       x = "Niveau d'Éducation", y = "Score Moyen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Analyse statistique

# STATISTIQUES DESCRIPTIVES GLOBALES

# Résumé des scores
score_summary <- students %>%
  summarise(
    Observations = n(),
    `Moyenne Math` = round(mean(math_score), 1),
    `Moyenne Lecture` = round(mean(reading_score), 1),
    `Moyenne Écriture` = round(mean(writing_score), 1),
    `Moyenne Totale` = round(mean(total_score), 1),
    `Écart-type Total` = round(sd(total_score), 1),
    `Minimum Total` = min(total_score),
    `Maximum Total` = max(total_score)
  )

print(score_summary)

score_summary %>%
  select(`Moyenne Totale`, `Écart-type Total`, `Minimum Total`, `Maximum Total`)

#test d'hypothese

#diff entre genre
test_genre <- t.test(average_score ~ gender, data = students)
print(test_genre)

# Visualisation correspondante
ggplot(students, aes(x = gender, y = average_score, fill = gender)) +
  geom_boxplot() +
  labs(title = "Scores moyens par genre",
       subtitle = paste("p-value =", format.pval(test_genre$p.value, digits = 3)),
       x = "Genre", y = "Score moyen") +
  theme_minimal()

#impact de la prepa au test

t_test_prep <- t.test(average_score ~ test_prep, data = students)
print(t_test_prep)

# Visualisation correspondante
ggplot(students, aes(x = test_prep, y = average_score, fill = test_prep)) +
  geom_boxplot() +
  labs(title = "Impact de la préparation au test",
       subtitle = paste("p-value =", format.pval(t_test_prep$p.value, digits = 3)),
       x = "Préparation au test", y = "Score moyen") +
  theme_minimal()

# Impact de l'éducation parentale
anova_parent <- aov(average_score ~ parental_education, data = students)
summary(anova_parent)

# Visualisation correspondante
ggplot(students, aes(x = parental_education, y = average_score, fill = parental_education)) +
  geom_boxplot() +
  labs(title = "Impact de l'éducation parentale",
       subtitle = paste("ANOVA p-value =", 
                        format.pval(summary(anova_parent)[[1]][["Pr(>F)"]][1], digits = 3)),
       x = "Niveau d'éducation des parents", y = "Score moyen") +
  theme_minimal()

# Calcul des moyennes et intervalles de confiance par niveau d'éducation parentale

library(dplyr)

parent_means <- students %>%
  group_by(parental_education) %>%
  summarise(
    Moyenne = mean(average_score),
    IC_inf = mean(average_score) - qt(0.975, df = n()-1) * sd(average_score)/sqrt(n()),
    IC_sup = mean(average_score) + qt(0.975, df = n()-1) * sd(average_score)/sqrt(n()),
    .groups = "drop"
  )

print(parent_means)

# Barplot avec intervalles de confiance
ggplot(parent_means, aes(x = parental_education, y = Moyenne, fill = parental_education)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2) +
  labs(title = "Scores moyens selon l'éducation parentale",
       subtitle = "Barplot avec intervalles de confiance (95%)",
       x = "Niveau d'éducation des parents", y = "Score moyen") +
  theme_minimal() +
  theme(legend.position = "none")


 #Matrice de corrélation
 
 cor_matrix <- cor(students %>% 
                     select(math_score, reading_score, writing_score))
 cor_matrix
 
 # Visualisation de la corrélation
 
 library(tidyr)
 library(tibble)
 
 
 melted_cor <- cor_matrix %>%
   as.data.frame() %>%
   rownames_to_column("Var1") %>%
   pivot_longer(-Var1, names_to = "Var2", values_to = "value")
 
 ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
   geom_tile() +
   scale_fill_gradient2(low = "blue", high = "skyblue", mid = "white", midpoint = 0) +
   geom_text(aes(label = round(value, 2))) +
   labs(title = "Matrice de Corrélation entre les Scores") +
   theme_minimal()
 
 #Modélisation simple
 
 # Régression linéaire
 
 library(broom)
 
 #Modèle 1 : Lecture et écriture
 
 model1 <- lm(math_score ~ reading_score + writing_score, data = students)
 tidy(model1)
 glance(model1)
 
 #Modèle 2 :Ajout du genre et préparation au test
 
 model2 <- lm(math_score ~ reading_score + writing_score + gender + test_prep, data = students)
 tidy(model2)
 glance(model2)
 
 #Modèle 3 : Ajout de l’éducation parentale
 
 model3 <- lm(math_score ~ reading_score + writing_score + gender + test_prep + parental_education, data = students)
 tidy(model3)
 glance(model3)
 
 #graphe des Modèle
 
 augment(model1) %>% 
   ggplot(aes(.fitted, .resid)) +
   geom_point() +
   geom_hline(yintercept = 0, linetype = "dashed") +
   theme_minimal()
 
 
 
 augment(model2) %>% 
   ggplot(aes(.fitted, .resid)) +
   geom_point() +
   geom_hline(yintercept = 0, linetype = "dashed") +
   theme_minimal()
 
 
 augment(model3) %>% 
   ggplot(aes(.fitted, .resid)) +
   geom_point() +
   geom_hline(yintercept = 0, linetype = "dashed") +
   theme_minimal()
 
 # Comparaison des modèles
 
 library(purrr)
 
 model_comparison <- list(model1 = model1, model2 = model2, model3 = model3) %>%
   map_df(glance, .id = "model")
 
 model_comparison %>%
   select(model, r.squared, adj.r.squared, AIC, BIC, sigma, df.residual)
 
 model_comparison %>%
   ggplot(aes(x = model, y = r.squared, fill = model)) +
   geom_col() +
   labs(title = "Comparaison des R² des modèles",
        x = "Modèle", y = "R²") +
   theme_minimal()
 
 #comparaison resudis des 3 modele
 
 bind_rows(
   augment(model1) %>% mutate(model = "Modèle 1"),
   augment(model2) %>% mutate(model = "Modèle 2"),
   augment(model3) %>% mutate(model = "Modèle 3")
 ) %>%
   ggplot(aes(x = .resid, fill = model)) +
   geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
   labs(title = "Distribution des résidus par modèle",
        x = "Résidus", y = "Fréquence") +
   theme_minimal()
 
 #verification des residus
 
 qqnorm(resid(model1)); qqline(resid(model1))
 qqnorm(resid(model2)); qqline(resid(model2))
 qqnorm(resid(model3)); qqline(resid(model3))
