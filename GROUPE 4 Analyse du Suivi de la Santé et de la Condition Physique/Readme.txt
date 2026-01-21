# README.txt - Projet Groupe 4 : Analyse du Suivi de la Santé et de la Condition Physique: Étude des Habitudes de Sommeil et d’Activité Physique et Leur Impact sur le Stress.


## Description du Projet
Ce projet analyse les habitudes de sommeil et d'activité physique, ainsi que leur impact sur le niveau de stress, en utilisant le langage R. Il s'inscrit dans le cours de Langage R à l'ESGIS lors du cadre de mini projet lors du cours de Langage R dispensé par Mr SAMA (Intelligence Artificielle et Big Data, Promotion 2025-2026).

- **Thème principal** : Suivi de la santé et condition physique: Étude des Habitudes de Sommeil et d’Activité Physique et Leur Impact sur le Stress.
- **Dataset utilisé** : Sleep Health and Lifestyle Dataset (Kaggle).
- **Outils** : R, ggplot2, corrplot, tidyverse, etc.
- **Objectifs** : Explorer les données, évaluer les corrélations, modéliser le stress via régression linéaire, et formuler des recommandations.

Le projet démontre des corrélations fortes (ex. : qualité du sommeil et stress : -0.90) et propose des recommandations pratiques pour réduire le stress (prioriser 7-8h de sommeil, activité modérée).

## Membres du Groupe
- KENKOU Marê Dave Christian
- MENSAH Deborah Winner
- TOGBENOU Elikplim Daniel

## Fichiers Inclus
Dans le dossier code
- **RapportMarkdownGroupe4.Rmd** : Fichier source principal R Markdown (rapport complet avec code et analyses).
- **RapportMarkdownGroupe4.html** : Version HTML du rapport générée (pour visualisation web).
Dans le dossier presentataion
- **presentation2.qmd** : Fichier Quarto pour la présentation reveal.js (slides interactifs).
- **presentation2.html** : Version HTML de la présentation (à ouvrir dans un navigateur).
- **logo.png** : Logo ESGI utilisé dans les documents.
- **custom.scss** : Fichier SCSS pour personnaliser le thème de la présentation.
- **style.css** : Fichier CSS pour styles additionnels.
Dans le dossier Data
- **Sleep_health_and_lifestyle_dataset.csv** : Dataset source utilisé pour les analyses.
Et un fichier 
- **README.txt** : Ce fichier (instructions et description).

## Instructions pour Exécuter
1. Installez R et RStudio.
2. Installez les packages nécessaires : tidyverse, corrplot, knitr, kableExtra, ggplot2, gridExtra (via `install.packages()`).
3. Ouvrez **Rmark2.Rmd** ou **presentation2.qmd** dans RStudio.
4. Cliquez sur "Knit" (pour Rmd) ou "Render" (pour qmd) pour générer les outputs HTML/PDF.
5. Assurez-vous que le dataset CSV est dans le même dossier ou adaptez le chemin dans le code.

Pour la présentation : Ouvrez presentation2.html dans un navigateur (plein écran recommandé).

## Résultats clés
- Corrélation très forte : Qualité du sommeil ↔ Stress = **-0.90**  
- Sommeil : facteur déterminant (réduction significative du stress)  
- Activité physique : effet faible / non significatif dans le modèle

Bonne lecture !