Analyse COVID-19 avec R



I. Description



Ce script R complet analyse les données COVID-19 de Worldometer. Il génère des visualisations statistiques et lance un tableau de bord interactif.



\* Comment utiliser ce projet



1\. Téléchargez les fichiers nécessaires

Vous avez besoin de 2 fichiers dans le même dossier :



text

votre-dossier/

├──  \*\*projet\_covid\_analysis.R\*\*      - CECI EST LE SCRIPT PRINCIPAL

└──  worldometer\_coronavirus\_daily\_data.csv



2\. Ouvrez le bon fichier dans RStudio



NE PAS COPIER-COLLER LE CODE !



À la place, faites ceci :



Ouvrez RStudio



Allez dans File → Open File...



Sélectionnez projet\_covid\_analysis.R



Cliquez sur Open



3\. Exécutez le script



Dans RStudio, avec le fichier ouvert :



Méthode 1 (recommandée) :



Cliquez sur le bouton "Source" en haut à droite de l'éditeur



Méthode 2 :



Appuyez sur Ctrl+Shift+S (Windows) ou Cmd+Shift+S (Mac)



Méthode 3 :



Dans la console R, tapez : source("projet\_covid\_analysis.R")



\* Ce que fait le script

Lorsque vous exécutez projet\_covid\_analysis.R, voici ce qui se passe :



Étape 1 : Importation des données



Vérifie la présence du fichier CSV



Importe les données COVID-19



Étape 2-5 : Analyses statistiques



Nettoyage des données



Calcul des indicateurs (moyennes mobiles, taux, etc.)



Génération de graphiques pour 3 pays :



Afghanistan



Albania



Algeria



Étape 6 : Lancement du Dashboard



Un tableau de bord interactif s'ouvre automatiquement dans votre navigateur web.



* Tableau de Bord Interactif - Guide Complet
* Comment le dashboard se lance
* 

Automatiquement :



Après l'analyse statistique, le dashboard s'ouvre automatiquement



Une fenêtre de navigateur s'ouvre avec l'adresse : http://127.0.0.1:XXXX



Manuellement (si besoin) :

Si le dashboard ne s'ouvre pas automatiquement :





\# Dans la console R, tapez :

shiny::runApp()



\*\* Comment utiliser le Dashboard?



1\. Interface principale



URL d'accès : http://127.0.0.1:XXXX (affiché dans la console R)



Port par défaut : Généralement 4242, 8100, ou autre



2\. Navigation



Sidebar gauche (barre latérale) :



* Sélecteur de pays : Choisissez un pays dans la liste



* Sélecteur de dates : Définissez la période d'analyse



* Bouton "Actualiser" : Met à jour les données



* Indicateurs rapides : Statistiques immédiates



Onglets principaux :



* Tableau de Bord : Vue d'ensemble



* Analyses Comparatives : Comparaisons entre pays



* Données Brutes : Données complètes



3\. Fonctionnalités interactives



Dans l'onglet "Tableau de Bord" :



Cartes KPI (en haut) :



🔵 Cas Cumulés : Nombre total de cas



🔴 Décès Cumulés : Nombre total de décès



🟡 Taux de Létalité : Pourcentage décès/cas



Graphique principal :



* Évolution des cas : Graphique interactif Plotly



* Zoom : Cliquez et glissez pour zoomer



* Survol : Passez la souris pour voir les valeurs



* Sélection : Sélectionnez une zone pour zoomer



Graphiques secondaires :



* Distribution : Histogramme des nouveaux cas



* Tendance 7 jours : Évolution récente



Dans l'onglet "Analyses Comparatives" :



* Comparaison internationale : Plusieurs pays sur même graphique



* Classement des pays : Tableau interactif avec :



* Recherche : Cherchez un pays spécifique



* Export : Boutons pour exporter en CSV, Excel, PDF



* Tri : Cliquez sur les en-têtes pour trier



Dans l'onglet "Données Brutes" :



* Table complète : Toutes les données filtrées



* Filtres : Par pays et période



* Pagination : Naviguez entre les pages



* Export : Téléchargez les données



4\. Interactions avec les graphiques Plotly



Sur les graphiques interactifs :



Zoom :



Rectangle : Cliquez-glissez pour sélectionner une zone



Molette : Zoom avant/arrière avec la molette



Boutons : Icônes en haut à droite du graphique



Navigation :



* Retour : Bouton maison pour réinitialiser la vue



* Déplacement : Cliquez-glissez pour se déplacer



Informations :



Tooltip : Survol d'un point pour voir la valeur exacte



Légende : Cliquez sur un élément de légende pour le masquer/afficher



Comparaison : Maintenez Shift pour comparer plusieurs points



5\. Personnalisation



Filtres disponibles :



Pays : Menu déroulant avec recherche



Période : Sélecteur de dates avec calendrier



Affichage : Certains graphiques ont des options de visualisation



\*\* Commandes utiles dans RStudio pendant l'exécution



Pour contrôler le dashboard :



Arrêter : Bouton rouge "Stop" dans RStudio



Relancer : Ré-exécutez source("projet\_covid\_analysis.R")



Forcer l'arrêt : Ctrl+C dans la console R



Pour vérifier l'état :





\# Vérifier si Shiny tourne



shiny::isRunning()



\# Voir le port utilisé


shiny:::getShinyOption("port", NULL)



* Résolution des problèmes du dashboard

Si le dashboard ne s'ouvre pas :



Vérifiez la console R pour les messages d'erreur



Essayez un port différent :





\# Spécifiez un port manuellement



shiny::runApp(port = 4242)



Si le dashboard est lent :



Réduisez la période d'analyse



Choisissez moins de pays



Fermez d'autres applications



Si les graphiques ne s'affichent pas :



Actualisez la page du navigateur (F5)



Vérifiez la connexion internet (pour Plotly)



Redémarrez RStudio



\*\* Compatibilité



Navigateurs supportés :



* Chrome (recommandé)



* Firefox



* Edge



* Safari



Appareils :



* Ordinateur (optimisé)



* Tablette (fonctionnel)



* Smartphone (vue adaptée)



\*\* Sauvegarde des résultats



Pour sauvegarder :



Graphiques :



Capture d'écran



Ou utilisez l'export Plotly (caméra en haut à droite)



Données :



Via les boutons d'export dans les tableaux



CSV, Excel, PDF disponibles



Session :



Les filtres sont conservés pendant la session



Actualisez la page pour réinitialiser



\*\* Cycle de vie du dashboard

Lancement : Automatique après l'analyse



Exécution : Tourne jusqu'à arrêt manuel



Arrêt :



Bouton stop RStudio



Fermeture de l'onglet navigateur + Ctrl+C



Fermeture de RStudio



Relance : Ré-exécutez le script



\*\* Prérequis IMPORTANTS

Avant d'exécuter le script :



Avoir R et RStudio installés



R : https://cran.r-project.org/



RStudio : https://posit.co/download/rstudio-desktop/



Avoir le fichier CSV



Nom exact : worldometer\_coronavirus\_daily\_data.csv



Dans le même dossier que projet\_covid\_analysis.R



Connexion internet



Pour installer les packages R nécessaires



Les packages s'installent automatiquement :

Le script installe automatiquement :



ggplot2, dplyr, lubridate (analyse)



shiny, shinydashboard, plotly (dashboard)



Et 6 autres packages nécessaires



\- Problèmes courants



Si vous voyez cette erreur :





ERREUR: Le fichier 'worldometer\_coronavirus\_daily\_data.csv' est introuvable!

Solution :



Vérifiez que le fichier CSV est dans le bon dossier



Vérifiez l'orthographe exacte du nom de fichier



Ré-ouvrez projet\_covid\_analysis.R dans RStudio



Si les graphiques ne s'affichent pas :



Attendez que l'analyse se termine



Vérifiez l'onglet "Plots" dans RStudio



Redimensionnez le panneau des graphiques



Si le dashboard ne se lance pas :



Vérifiez que vous avez internet



Attendez l'installation des packages



Redémarrez RStudio et ré-exécutez



II. Structure du projet après exécution

Après avoir lancé projet\_covid\_analysis.R :





votre-dossier/

├── projet\_covid\_analysis.R          (votre script)

├── worldometer\_coronavirus\_daily\_data.csv  (vos données)

├── Plusieurs graphiques statiques   (dans RStudio)

└── Dashboard interactif             (dans votre navigateur)



III. Conseils importants



N'éditez pas projet\_covid\_analysis.R sauf si vous savez ce que vous faites



La première exécution prend 2-3 minutes (installation des packages)



Les exécutions suivantes sont plus rapides



Pour arrêter le dashboard :



Cliquez sur le bouton stop dans RStudio



OU fermez l'onglet du navigateur et tapez Ctrl+C dans la console R



Pour relancer le dashboard : ré-exécutez projet\_covid\_analysis.R



Gardez RStudio ouvert pendant l'utilisation du dashboard



IV. Support



Si vous avez des problèmes avec projet\_covid\_analysis.R :



Vérifiez que vous avez bien ouvert le fichier dans RStudio (pas copié-collé)



Vérifiez la console R pour les messages d'erreur



Assurez-vous que R est à jour (version 4.0+)



Contactez-moi avec :



Le message d'erreur exact



La version de R (version dans la console)



Ce que vous avez fait exactement



V. Résumé rapide



Pour utiliser ce projet :



Placer les 2 fichiers dans un dossier



Ouvrir projet\_covid\_analysis.R dans RStudio



Cliquer sur "Source"



Attendre que le dashboard s'ouvre



Utiliser le dashboard dans votre navigateur



Arrêter avec le bouton stop dans RStudio



Fichier principal à ouvrir : projet\_covid\_analysis.R

