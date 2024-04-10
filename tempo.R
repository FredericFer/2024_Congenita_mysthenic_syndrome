library(xlsx)
library(dplyr)
library(stringr)
library(dplyr)
library(tidyr)
# Load data
data = read.xlsx(file = "/Users/f.fer/Documents/1_Projets/20230801_julianT/Tableur CMS 2023_V2.xlsx",sheetIndex = 1,startRow = 2)
Data_demographique = data[,c(1:23)]
Onset_symptom = data[,c(24:49)]
seveti_max = data[,c(50:55)]
severi_last_visit = data[,c(56:77)]
evolution = data[,c(78:85)]
Treatments =  data[,c(86:92)]
Treatments_init = data[,c(93:98)] 
Treatments_end = data[,c(99:104)]
Treatments_eff = data[,c(105:110)]
Treatments_EI = data[,c(111:116)]
Treatments_LAST = data[,c(117:122)]
EMG_Biopsie = data[,-c(1:122)]
colnames(data)
#===============================================================================
# Données démographique 
#===============================================================================
Data_demographique = data[,c(1:23)]

Data_demographique <- Data_demographique %>%
  mutate(inheritance = str_replace_all(inheritance,
                                       c("Recessive " = "Recessive", 
                                         "Recesive" = "Recessive",
                                         "Récessive" = "Recessive")))
Data_demographique$Décès..age. <- trimws(sub("\\(.*\\)", "", Data_demographique$Décès..age.))
Data_demographique$Décès..age. <- tolower(Data_demographique$Décès..age.)
# Renommer les Origines
Data_demographique[Data_demographique$Family.origin%in%c("France","Suisse","Portugal","Italie","Roumanie","Kosovo",
                                                          "Espagne","Argentine","Pologne+France","Réunion","Esapgne+Guadeloupe",
                                                          "Luxembourg","Maurice"),]$Family.origin = 	"Caucasian" 
Data_demographique[Data_demographique$Family.origin%in%c("France (Gitan)"),]$Family.origin = 	"Roma (Gypsie)" 
Data_demographique[Data_demographique$Family.origin%in%c("Guinée","Afrique noire","Sénégal"),]$Family.origin = "Sub-Saharan Africa"
Data_demographique[Data_demographique$Family.origin%in%c("Maghreb","Maroc","Algérie","Algérie+Maroc","Tunisie" ),]$Family.origin = 	"North Africa"	
Data_demographique[Data_demographique$Family.origin%in%c("Cambodge","Comores","Cap Vert"),]$Family.origin = 	"Others"
Data_demographique[Data_demographique$Family.origin%in%c("Liban","Turquie","Arabie Saoudite","Qatar","Kurde","Turguie"),]$Family.origin = 	"Middle East"
#===============================================================================
# Données Génétiques
#===============================================================================
Data_demographique$Gene[grep(pattern = "FCCMS",x =Data_demographique$Gene )] = "FCCMS" 
Data_demographique$Gene[grep(pattern = "SCCMS",x =Data_demographique$Gene )] = "SCCMS" 
Data_demographique$Gene[grep(pattern = "CHRNE",x =Data_demographique$Gene )] = "CHRNE" 
Data_demographique$Gene[grep(pattern = "CHAT",x =Data_demographique$Gene )] = "CHAT" 
Data_demographique$Gene[grep(pattern = "CHRNA1",x =Data_demographique$Gene )] = "CHRNA1" 
Data_demographique$Gene_2 = Data_demographique$Gene
Data_demographique$Gene_2[Data_demographique$Gene%in%names(table(Data_demographique$Gene)[table(Data_demographique$Gene)<4])] = "Others"
Data_demographique$Gene_2[is.na(Data_demographique$Gene_2)] = "Others"
#===============================================================================
# Données age 1r symptome
#===============================================================================
Age.first.symptoms_cat = Data_demographique$Age.first.symptoms..y.
Age.first.symptoms_cat[grep(pattern = "Petite" ,x = Age.first.symptoms_cat)] = "2"
Age.first.symptoms_cat[grep(pattern = "Enfance" ,x = Age.first.symptoms_cat)] = "5"
Age.first.symptoms_cat = gsub(x = Age.first.symptoms_cat,pattern = " .*$","")
Age.first.symptoms_cat_value = as.numeric(Age.first.symptoms_cat)
Data_demographique$Age.first.symptoms..y.[(!(is.na(as.numeric(Age.first.symptoms_cat))))&(as.numeric(Age.first.symptoms_cat)==0)] = "congenital"
Data_demographique$Age.first.symptoms..y.[(!(is.na(as.numeric(Age.first.symptoms_cat))))&(as.numeric(Age.first.symptoms_cat)>0)&(as.numeric(Age.first.symptoms_cat)<3)] = "Infancy"
Data_demographique$Age.first.symptoms..y.[(!(is.na(as.numeric(Age.first.symptoms_cat))))&(as.numeric(Age.first.symptoms_cat)>2)&(as.numeric(Age.first.symptoms_cat)<11)] = "Childhood"
Data_demographique$Age.first.symptoms..y.[(!(is.na(as.numeric(Age.first.symptoms_cat))))&(as.numeric(Age.first.symptoms_cat)>10)&(as.numeric(Age.first.symptoms_cat)<18)] = "Adolescence"
Data_demographique$Age.first.symptoms..y.[(!(is.na(as.numeric(Age.first.symptoms_cat))))&(as.numeric(Age.first.symptoms_cat)>17)&(as.numeric(Age.first.symptoms_cat)<40)] = "Adult"
Data_demographique$Age.first.symptoms..y.[(!(is.na(as.numeric(Age.first.symptoms_cat))))&(as.numeric(Age.first.symptoms_cat)>39)] = "Late"
Data_demographique$Age.first.symptoms..y.[is.na(Data_demographique$Age.first.symptoms..y.)] = "NaN"
#===============================================================================
# Etablissement du premier jeu de donnée
#===============================================================================
Pousse = data$Poussées
Course = data$Course
Course[Course%in%c("Worsening, stable","Worsening, Stable")] = "Worsening => stable"

# Fonction pour regrouper les maladies
regroup_misdiagnosis <- function(misdiagnosis) {
  misdiagnosis <- tolower(misdiagnosis)
  if (is.na(misdiagnosis)) {
    return(NA)
  } else if (grepl("myasthénie ai", misdiagnosis)) {
    return("Myasthénie AI")
  } else if (grepl("myopathie congén", misdiagnosis)) { # englobe congénitale et congétinale
    return("Myopathie Congénitale")
  } else if (grepl("mitochondriopathie", misdiagnosis)) {
    return("Mitochondriopathie")
  } else if (grepl("sma", misdiagnosis)) {
    return("SMA")
  } else if (grepl("dystrophie musculaire", misdiagnosis)) {
    return("Dystrophie Musculaire")
  } else if (grepl("lambert-eaton", misdiagnosis)) {
    return("Lambert-Eaton")
  } else if (grepl("non", misdiagnosis)) {
    return("Non")
  } else if (grepl("oui", misdiagnosis)) {
    return("Oui")
  } else {
    return("Autre")
  }
}
relabellise_misdiagnosis <- function(diagnosis) {
  if (is.na(diagnosis)) return(NA)
  if (diagnosis == "Oui (errance)") return("Non")
  
  # Remplacement des myopathies congénitales
  if (grepl("Myopathie cong.tinale", diagnosis, ignore.case = TRUE)) {
    diagnosis <- "Myopathie congénitale"
  }
  
  # Remplacement des dystrophies musculaires
  if (grepl("Dystrophie musculaire|sarcoglycanopathie", diagnosis, ignore.case = TRUE)) {
    diagnosis <- "Dystrophie musculaire"
  }
  
  # Remplacement des myosites
  if (grepl("Polymyosite|Myosite", diagnosis, ignore.case = TRUE)) {
    diagnosis <- "Myosite/Polymyosite"
  }
  
  # Remplacement des myopathies métaboliques
  if (grepl("Myopathie m.tabolique|Pompe", diagnosis, ignore.case = TRUE)) {
    diagnosis <- "Myopathie métabolique"
  }
  
  return(diagnosis)
}
data$Misdiagnosis_relabeled <- sapply(data$Misdiagnosis, relabellise_misdiagnosis)
clean_diagnosis <- function(diagnosis) {
  diagnosis <- gsub("Myopathie congétinale", "Myopathie congénitale", diagnosis)
  diagnosis <- gsub("Myopathie congétinales", "Myopathie congénitale", diagnosis)
  return(diagnosis)
}
data$Misdiagnosis <- sapply(data$Misdiagnosis, clean_diagnosis)


disease_patterns <- list(
  "Myasthénie AI" = "Myasth.nie AI",
  "Myopathie congénitale" = "Myopathie cong",
  "SLA" = "SLA",
  "Myopathie distale" = "Myopathie distale",
  "Canalopathie/Paralysie périodique" = "Canalopathie/Paralysie périodique",
  "Mitochondriopathie" = "Mitochondriopathie",
  "Syndrome de Moebius" = "Syndrome de Moebius",
  "Lyme chronique" = "Lyme chronique",
  "Dystrophie musculaire" = "Dystrophie musculaire|sarcoglycanopathie",
  "Myosite" = "Polymyosite|Myosite",
  "Myopathie métabolique" = "Myopathie m.tabolique|Pompe",
  "SMA" = "SMA",
  "Lambert-Eaton" = "Lambert-Eaton",
  "Fibromyalgie" = "Fibromyalgie",
  "Non" = "Non|^NA$|Oui \\(errance\\)"
)
data$new_label <- sapply(data$Misdiagnosis, function(misdiagnosis) {
  found <- NA
  for (label in names(disease_patterns)) {
    if (grepl(disease_patterns[[label]], misdiagnosis, ignore.case = TRUE)) {
      found <- label
      break
    }
  }
  return(found)
})
disease_counts <- table(data$new_label)
# Fonction qui vérifie la présence d'un motif dans la misdiagnosis d'un patient
presence_function <- function(patient_diagnosis, pattern) {
  as.integer(grepl(pattern, patient_diagnosis, ignore.case = TRUE))
}

# Création du tableau croisé
cross_table <- as.data.frame(sapply(names(disease_patterns), function(pattern) {
  sapply(data$Misdiagnosis, presence_function, pattern = disease_patterns[[pattern]])
}))


# Nommage des colonnes
colnames(cross_table) <- names(disease_patterns)

# Affichage du tableau
print(cross_table)
data$Nom[is.na(data$Nom)] = "NaN"
rownames(cross_table) =   paste0(c(1:nrow(data)),"-",paste(substr(Data_demographique$Nom,1,1),substr(sub(x = Data_demographique$Nom,pattern = "^.* ",""),1,1)),sep="")

# Appliquer la fonction de nettoyage
data$Misdiagnosis_relabeled <- sapply(data$Misdiagnosis, relabellise_misdiagnosis)
data$Misdiagnosis <- sapply(data$Misdiagnosis, clean_diagnosis)
diagnoses <- unlist(strsplit(data$Misdiagnosis, ", "))
diagnosis_counts <- table(diagnoses)
# Convertir en DataFrame
diagnosis_df <- as.data.frame(diagnosis_counts)
# Renommer les colonnes
colnames(diagnosis_df) <- c("Diagnosis", "Count")

# Calculer les pourcentages
total_patients <- 232
diagnosis_df$Percentage <- (diagnosis_df$Count / total_patients) * 100

# Afficher le DataFrame
print(diagnosis_df)






# Application de la fonction à la colonne Misdiagnosis
#data$Misdiagnosis <- sapply(data$Misdiagnosis, regroup_misdiagnosis)

# Afficher les nouvelles catégories
print(unique(data$Misdiagnosis))
Data_demographique$ATCDs.familiaux[Data_demographique$ATCDs.familiaux=="NA"] = NA
Data_demographique$ATCDs.familiaux = ifelse(Data_demographique$ATCDs.familiaux=="Oui","YES","NO")
Data_demographique$consanguinity =  ifelse(Data_demographique$consanguinity=="Oui","YES","NO")
Data_demographique$Cas.index = ifelse(Data_demographique$Cas.index=="Oui","YES","NO")
Data_xsl = data.frame(
  "Initials" =  paste(substr(Data_demographique$Nom,1,1),substr(sub(x = Data_demographique$Nom,pattern = "^.* ",""),1,1),sep=""),
  "Sex" = ifelse(Data_demographique$Sexe=="M","Male","Female"),
  "Family origin" = Data_demographique$Family.origin,
  "Involved gene" = Data_demographique$Gene_2,
  "flare-up" = Pousse,
  "Evolution" = Course,
  "Period first symptom" = Data_demographique$Age.first.symptoms..y.,
  "Age at First visit" =  Data_demographique$age.first.visit..dispo.,
  "Age at clinic Diagnostic)" =  Data_demographique$Age.au.diagnostic.clinique.EMG,
  "Age at Genetic Diagnostic" =  Data_demographique$age.first.visit..dispo.,
  "Age at First Symptome" = Age.first.symptoms_cat_value,
  "Age Last visit" = Data_demographique$Age.last.visit,
  "Follow up CS" = Data_demographique$Follow.up.cs..y.,
  "Follow up since first sympt" = Data_demographique$Follow.up.since.first.symp..y.,
  "Delay clinic diag EMG" =  as.numeric(Data_demographique$Age.au.diagnostic.clinique.EMG) - Age.first.symptoms_cat_value,
  "Delay Genetic diag" =  as.numeric(Data_demographique$Age.au.diagnostic.génétique) - Age.first.symptoms_cat_value,
  "Transmission" = Data_demographique$inheritance,
  "Statut" = Data_demographique$Statut,
  "family medical history" = Data_demographique$ATCDs.familiaux,
  "Consanguinity" = Data_demographique$consanguinity,
  "City" = sub(x = Data_demographique$Ville,pattern = "[/].*$",replacement = ""),
  "First_Misdiagnosis" = as.vector(sapply(sub(x = data$Misdiagnosis,pattern = ",.*$",""), regroup_misdiagnosis)),
  "Index" = Data_demographique$Cas.index
)
colnames(Data_xsl) = c("Initials",
"Sex",
"Family origin",
"Involved gene" ,
"flare-up"  ,
"Evolution"  ,
"Period first symptom"  ,
"Age at First visit" ,
"Age at clinic Diagnostic" ,
"Age at Genetic Diagnostic" ,
"Age at First Symptome",
"Age Last visit",
"Follow up",
"Follow up since first sympt",
"Delay clinic diag EMG",
"Delay Genetic diag",
"Transmission",
"Statut",
"family medical history" ,
"Consanguinity",
"City" ,
"Misdiagnosis",
"Index" )
df = Data_xsl

# Enregistrer le jeu de données sur le premier onglet
write.xlsx(df, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Data", row.names = FALSE)

# =============================================================================
# Tableau % 
# =============================================================================
# Liste des colonnes quantitatives et qualitatives
quant_cols <- c("Age at First visit" ,
                "Age at clinic Diagnostic" ,
                "Age at Genetic Diagnostic",
                "Age at First Symptome",
                "Delay clinic diag EMG",
                "Delay Genetic diag",
                "Age Last visit",
                "Follow up",
                "Follow up since first sympt")
qual_cols <- setdiff(names(df), c("Involved gene", quant_cols))

# Initialiser une liste pour stocker les résultats
results_p <-results_pp <- results_c <- results_age <-  list()

# Traitement des colonnes qualitatives
for (col in qual_cols[-c(1)]) {
  
  # Supprime les lignes avec NA dans la colonne en cours ou 'Involved.gene'
  df_filtered <- df[!is.na(df[[col]]) & !is.na(df$`Involved gene`), ]
  
  # Crée une table de croisement
  cross_tab <- table(df_filtered$`Involved gene`, df_filtered[[col]])
  cross_tab2 <- cross_tab / rowSums(cross_tab) #* 100  # Convertir en pourcentage
  cross_tab3 <- cross_tab / colSums(cross_tab) #* 100  # Convertir en pourcentage
  
  # Transformer le tableau en un data.frame
  cross_tab <- as.data.frame.matrix(cross_tab)
  cross_tab2 <- as.data.frame.matrix(cross_tab2)
  cross_tab3 <- as.data.frame.matrix(cross_tab3)
  
  # Ajouter le tableau de croisement à la liste de résultats
  results_c[[col]] <- cross_tab
  results_p[[col]] <- cross_tab2
  results_pp[[col]] <- cross_tab3
}

# Traitement des colonnes quantitatives
for (col in quant_cols) {
  df[col] = as.numeric(df[[col]])
  # Supprime les lignes avec NA dans la colonne en cours ou 'Involved.gene'
  df_filtered <- df[!is.na(df[[col]]) & !is.na(df$`Involved gene`), ]
  
  # Calcule la moyenne pour chaque gène
  mean_tab <- aggregate(df_filtered[[col]], by = list(df_filtered$`Involved gene`), FUN = mean)
  sd_tab <- aggregate(df_filtered[[col]], by = list(df_filtered$`Involved gene`), FUN = sd)
  # Ajouter le tableau de moyennes à la liste de résultats
  tempo = cbind(mean_tab,sd_tab[,-c(1)])
  colnames(tempo) =  c("Gene",paste0(col,c("_mean","_sd")))
  results_age[[col]] <- tempo
}
final_table <- Reduce(function(...) merge(..., by = "Gene", all = TRUE), results_age)
write.xlsx(final_table, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Age", append = TRUE, row.names = TRUE)

tempo = Data_xsl$Involved.gene
for(tab in names(results_c)){
  #write.xlsx(results_c[tab], "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = paste0("cpt ",tab), append = TRUE, row.names = TRUE)
}
write.xlsx(cbind.data.frame(results_c), "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Comptages", append = TRUE, row.names = TRUE)


for(tab in names(results_p)){
  #write.xlsx(results_p[tab], "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = paste0("perc1  ",tab), append = TRUE, row.names = TRUE)
}
write.xlsx(cbind.data.frame(results_p), "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Percentage1", append = TRUE, row.names = TRUE)


for(tab in names(results_pp)){
 # write.xlsx(results_pp[tab], "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = paste0("perc2  ",tab), append = TRUE, row.names = TRUE)
}
write.xlsx(cbind.data.frame(results_pp), "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Percentage2", append = TRUE, row.names = TRUE)

write.xlsx(cbind.data.frame(cross_table), "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Misdiag", append = TRUE, row.names = TRUE)


#===============================================================================
# Ville
#===============================================================================
# Installer et charger les packages nécessaires si ce n'est pas déjà fait
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(ggmap)) {
  install.packages("ggmap")
}
library(ggplot2)
library(ggmap)
# Authentifier avec Geonames
options(geonamesUsername = "bleii")
# Extraire les villes uniques et compter le nombre de patients par ville
villes <- Data_demographique$Ville
# Gérer les entrées avec plusieurs villes
villes <- unlist(strsplit(villes, "/"))
# Supprimer les valeurs manquantes
villes <- na.omit(villes)
# Compter le nombre de patients par ville
nb_patients_par_ville <- table(villes)
# Récupérer les coordonnées des villes
coord_villes <- lapply(names(nb_patients_par_ville), function(ville) {
  Sys.sleep(1)
  tryCatch({
    c(GNsearch(name = ville, country = "FR", maxRows = 1)[c("lat", "lng")], nb_patients = nb_patients_par_ville[[ville]])
  }, error = function(e) {
    NULL
  })
})
# Convertir en data frame et supprimer les valeurs manquantes
coord_villes <- do.call(rbind, coord_villes)
coord_villes <- na.omit(coord_villes)
# Convertir en numérique
coord_villes = data.frame(coord_villes)
coord_villes$lat <- as.numeric(coord_villes$lat)
coord_villes$lng <- as.numeric(coord_villes$lng)
coord_villes$nb_patients <- as.numeric(coord_villes$nb_patients)
# Tracer la carte de la France
map('france', fill=TRUE, col="white", bg="lightblue", lwd=0.05)
# Ajouter les points pour les villes
points(coord_villes$lng, coord_villes$lat, col="red", pch=20, cex = sqrt(coord_villes$nb_patients))
# Installer et charger le package 'ggthemes' si nécessaire
# Installer et charger le package 'ggthemes' si nécessaire
if (!require(ggthemes)) {
  install.packages("ggthemes")
}
library(ggthemes)
p = ggplot() +
  geom_polygon(data = france_map, aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_point(data = coord_villes, aes(x = lng, y = lat, size = nb_patients), colour = "blue", alpha = 0.5) +
  geom_text(data = coord_villes, aes(x = lng, y = lat, label = nb_patients), size = 3, vjust = 1.5, hjust = 0.5) +
  scale_size_continuous(range = c(1, 10), guide = FALSE) +
  coord_quickmap() +
  labs(title = "Distribution of patients by city in France",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
ggsave("/Users/f.fer/Documents/1_Projets/distribution_of_patients.pdf", plot = p, width = 10, height = 10)




library(maps)
library(mapdata)
map("worldHires","Canada", xlim=c(-141,-53), ylim=c(90,85), col="gray90", fill=TRUE)
map('worldHires', "France", col='gray80', fill=F)
map.scale(134,26,metric=T, relwidth=0.3)

map("worldHires", "france", xlim=c(-5,10), ylim=c(35,55))


some.eu.countries <- c(
  "France"
)
# Récupérer les données cartographiques
require(maps)
require(viridis)
world_map <- map_data("world")
some.eu.maps <- map_data("world", region = some.eu.countries)

# Calculer le centroïde comme étant la longitude et la lattitude moyennes
# Utilisé comme coordonnée pour les noms de pays
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

#===============================================================================
# Pays d'origine
#===============================================================================
# Installer et charger les packages si ce n'est pas déjà fait
if(!require(rnaturalearth)) { install.packages("rnaturalearth"); library(rnaturalearth) }
if(!require(rnaturalearthdata)) { install.packages("rnaturalearthdata"); library(rnaturalearthdata) }
if(!require(dplyr)) { install.packages("dplyr"); library(dplyr) }
if(!require(ggplot2)) { install.packages("ggplot2"); library(ggplot2) }

# Obtenir la carte du monde
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Compter le nombre de patients par pays
patient_count <- table(Data_demographique$Family.origin)

# Convertir le tableau de comptage en dataframe
patient_df <- data.frame(Country = names(patient_count), Count = as.integer(patient_count))

# Renommer les pays pour correspondre à ceux de la carte du monde
patient_df$Country[patient_df$Country == "France (Gitan)"] <- "France"
patient_df$Country[patient_df$Country == "Maghreb"] <- "Morocco" # Hypothèse
patient_df$Country[patient_df$Country == "Turguie"] <- "Turkey"

# Fusionner les données avec la carte du monde
world_map <- left_join(world_map, patient_df, by = c("admin" = "Country"))

# Remplacer NA par 0
world_map$Count[is.na(world_map$Count)] <- 0 

# Créer la visualisation
ggplot() +
  geom_sf(data = world_map, aes(fill = Count)) +
  scale_fill_continuous(low = "white", high = "blue", name = "Patient Count") +
  coord_sf(xlim = c(-10, 10), ylim = c(30, 50), expand = FALSE) + # Ajuster ces limites pour obtenir la vue souhaitée
  labs(title = "Distribution of patients by country of origin",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

#=========================================================================================

# Retraitement des données
Data_demographique$"Age.first.symptoms..y." <- as.character(Data_demographique$"Age.first.symptoms..y.")
Data_demographique$"Age.first.symptoms..y."[grepl("\\(EC\\)", Data_demographique$"Age.first.symptoms..y.")] <- gsub("\\s*\\(.*\\)", "", Data_demographique$"Age.first.symptoms..y."[grepl("\\(EC\\)", Data_demographique$"Age.first.symptoms..y.")])
Data_demographique$"Age.first.symptoms..y." <- as.numeric(Data_demographique$"Age.first.symptoms..y.")
Data_demographique$age_category <- cut(Data_demographique$"Age.first.symptoms..y.", breaks = c(-Inf, 0, 3, 12, 20, 40, Inf), labels = c("Congénitale", "Petite Enfance", "Enfance", "Adolescence", "Adulte", "Après 40"), include.lowest = TRUE, right = FALSE)

# Fonctions pour calculer le pourcentage de chaque catégorie d'âge
# Les fonctions de calcul des pourcentages
percentage_congenitale <- function(x) sum(x == "Congénitale", na.rm = TRUE) / length(x) * 100
percentage_petite_enfance <- function(x) sum(x == "Petite Enfance", na.rm = TRUE) / length(x) * 100
percentage_enfance <- function(x) sum(x == "Enfance", na.rm = TRUE) / length(x) * 100
percentage_adolescence <- function(x) sum(x == "Adolescence", na.rm = TRUE) / length(x) * 100
percentage_adulte <- function(x) sum(x == "Adulte", na.rm = TRUE) / length(x) * 100
percentage_apres_40 <- function(x) sum(x == "Après 40", na.rm = TRUE) / length(x) * 100
percentage_consanguinity <- function(x) sum(x == "Oui", na.rm = TRUE) / length(x) * 100
percentage_family_history <- function(x) sum(x == "Oui", na.rm = TRUE) / length(x) * 100
percentage_heterozygous <- function(x) sum(x != "Homozygous", na.rm = TRUE) / length(x) * 100
percentage_homozygous <- function(x) sum(x == "Homozygous", na.rm = TRUE) / length(x) * 100
percentage_recessive <- function(x) sum(x == "Recessive", na.rm = TRUE) / length(x) * 100
# Simplifier les noms de gènes
Data_demographique$SimpleGene <- gsub("\\s*\\(.*\\)", "", Data_demographique$Gene)

gene_summary_simple <- do.call(rbind, lapply(split(Data_demographique, Data_demographique$SimpleGene), function(sub_data) {
  data.frame(
    Gene = unique(sub_data$SimpleGene),
    Number_of_Males = sum(sub_data$"Sexe" == "M"),
    Number_of_Females = sum(sub_data$"Sexe" == "F"),
    Average_Age_at_Clinical_EMG_Diagnosis = mean(sub_data$"Age.au.diagnostic.clinique.EMG", na.rm = TRUE),
    SD_Age_at_Clinical_EMG_Diagnosis = sd(sub_data$"Age.au.diagnostic.clinique.EMG", na.rm = TRUE),
    Median_Age_at_Clinical_EMG_Diagnosis = median(sub_data$"Age.au.diagnostic.clinique.EMG", na.rm = TRUE),
    Average_Age_at_Genetic_Diagnosis = mean(sub_data$"Age.au.diagnostic.génétique", na.rm = TRUE),
    SD_Age_at_Genetic_Diagnosis = sd(sub_data$"Age.au.diagnostic.génétique", na.rm = TRUE),
    Median_Age_at_Genetic_Diagnosis = median(sub_data$"Age.au.diagnostic.génétique", na.rm = TRUE),
    Average_Age_at_First_Visit = mean(sub_data$"age.first.visit..dispo.", na.rm = TRUE),
    SD_Age_at_First_Visit = sd(sub_data$"age.first.visit..dispo.", na.rm = TRUE),
    Median_Age_at_First_Visit = median(sub_data$"age.first.visit..dispo.", na.rm = TRUE),
    Percentage_Heterozygous = percentage_heterozygous(sub_data$"Statut"),
    Percentage_Homozygous = percentage_homozygous(sub_data$"Statut"),
    Percentage_Recessive = percentage_recessive(sub_data$"inheritance"),
    Percentage_Consanguinity = percentage_consanguinity(sub_data$"consanguinity"),
    Percentage_Family_History = percentage_family_history(sub_data$"ATCDs.familiaux"),
    Percentage_Congenitale_First_Symptoms = percentage_congenitale(sub_data$"age_category"),
    Percentage_Petite_Enfance_First_Symptoms = percentage_petite_enfance(sub_data$"age_category"),
    Percentage_Enfance_First_Symptoms = percentage_enfance(sub_data$"age_category"),
    Percentage_Adulte_First_Symptoms = percentage_adulte(sub_data$"age_category"),
    Percentage_Apres_40_First_Symptoms = percentage_apres_40(sub_data$"age_category"),
    Percentage_Heterozygous = percentage_heterozygous(sub_data$"Statut"),
    Percentage_Homozygous = percentage_homozygous(sub_data$"Statut"),
    Percentage_Recessive = percentage_recessive(sub_data$"inheritance"),
    Percentage_Consanguinity = percentage_consanguinity(sub_data$"consanguinity"),
    Percentage_Family_History = percentage_family_history(sub_data$"ATCDs.familiaux")
  )
}))

names(gene_summary_simple) <- c("Gene", "Number of Males", "Number of Females", 
                                "Average Age at Clinical EMG Diagnosis", "SD Age at Clinical EMG Diagnosis",
                                "Median Age at Clinical EMG Diagnosis", "Average Age at Genetic Diagnosis",
                                "SD Age at Genetic Diagnosis", "Median Age at Genetic Diagnosis",
                                "Average Age at First Visit", "SD Age at First Visit",
                                "Median Age at First Visit", "Percentage Heterozygous", 
                                "Percentage Homozygous", "Percentage Recessive",
                                "Percentage Consanguinity", "Percentage Family History",
                                "Percentage Congenital at First Symptoms", 
                                "Percentage Early Childhood at First Symptoms", 
                                "Percentage Childhood at First Symptoms", 
                                "Percentage Adult at First Symptoms", 
                                "Percentage After 40yo at First Symptoms",  "Percentage Heterozygous", "Percentage Homozygous", 
                                "Percentage Recessive", "Percentage Consanguinity", 
                                "Percentage Family History")
                                

# Enregistrez le tableau de résumé dans un fichier Excel
write.xlsx(gene_summary_simple, "/Users/f.fer/Documents/1_Projets/gene_summary.xlsx")
#===============================================================================
# camenbert
#===============================================================================

# Charger la bibliothèque
library(ggplot2)
library(dplyr)
library(rlang)
library(RColorBrewer)
#install.packages("viridis")
library(viridis)




# Créer une fonction pour générer des graphiques camembert
generate_pie_chart <- function(df, column_name, title) {
  column_name <- rlang::sym(column_name)
  
  # Utiliser la palette "RdYlBu" pour des graphiques accessibles
  cols <- viridis::viridis_pal()(length(unique(df[[column_name]])))
  
  
  df %>%
    group_by(!!column_name) %>%
    summarise(Count = n()) %>%
    mutate(Perc = paste0(round(Count/sum(Count)*100, 1), "%")) %>%
    ggplot(aes(x = "", y = Count, fill = factor(!!column_name))) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    geom_text(aes(label = paste0(Count)), position = position_stack(vjust = 0.5), color = "black", size = 4) +
    scale_fill_manual(values = cols) +  # Utiliser notre palette de couleurs personnalisée
    coord_polar("y", start=0) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Modifier la police du titre
      legend.title = element_text(face="bold", size=10),  # Ajouter un titre en gras à la légende
      legend.text = element_text(size = 8)  # Modifier la police de la légende
    ) +
    labs(title = title, fill = column_name)
}

# Générer des graphiques camembert pour chaque paramètre
generate_pie_chart(Data_demographique, "Gene", "Distribution by Gene")

Data_demographique$Region <- Data_demographique$Family.origin

# Définir une liste de pays pour chaque région
France <-c("France")
Gitan <-c("France (Gitan)")
europe <- c("Suisse", "Portugal", "Italie", "Roumanie", "Kosovo", "Espagne", "Pologne+France", "Luxembourg")
africa <- c("Maroc", "Maghreb", "Qatar", "Sénégal", "Algérie", "Algérie+Maroc", "Tunisie", "Afrique noire", "Cap Vert")
asia <- c("Turquie", "Arabie Saoudite", "Turguie", "Cambodge", "Liban")
other <- c("Guinée", "Maurice", "Argentine", "Kurde", "Réunion", "Esapgne+Guadeloupe")

# Remplacer les pays par leurs régions respectives
Data_demographique$Region[Data_demographique$Region %in% France] <- "France"
Data_demographique$Region[Data_demographique$Region %in% Gitan] <- "Gitan"
Data_demographique$Region[Data_demographique$Region %in% europe] <- "Europe"
Data_demographique$Region[Data_demographique$Region %in% africa] <- "Africa"
Data_demographique$Region[Data_demographique$Region %in% asia] <- "Asia"
Data_demographique$Region[Data_demographique$Region %in% other] <- "Other"


Data_demographique$age_category <- as.character(Data_demographique$age_category)

Data_demographique$age_category[Data_demographique$age_category == "Petite Enfance"] <- "Early Childhood"
Data_demographique$age_category[Data_demographique$age_category == "Enfance"] <- "Childhood"
Data_demographique$age_category[Data_demographique$age_category == "Adolescence"] <- "Adolescence"
Data_demographique$age_category[Data_demographique$age_category == "Adulte"] <- "Adult"
Data_demographique$age_category[Data_demographique$age_category == "Après 40"] <- "After 40"


# Vérifier le résultat
table(Data_demographique$Region)

pdf("/Users/f.fer/Documents/1_Projets/pie_chart.pdf",width = 10,height = 10)
Data_demographique$Gene = sub(x = Data_demographique$Gene,pattern = "[ (/].*$",replacement = " ")
Data_demographique$Gene = gsub(x = Data_demographique$Gene," ","")
generate_pie_chart(Data_demographique, "Gene", "Distribution by Gene")
generate_pie_chart(Data_demographique, "Region", "Distribution by Patient Origin")
generate_pie_chart(Data_demographique, "Ville", "Distribution by City")
generate_pie_chart(Data_demographique, "Cas.index", "Distribution by Index Case")
generate_pie_chart(Data_demographique, "inheritance", "Distribution by Inheritance")
generate_pie_chart(Data_demographique, "Décès.(age)", "Living or Not")
generate_pie_chart(Data_demographique, "age_category", "Age at Onset of Symptoms")
dev.off()


# boxplot

library(rlang)

library(viridis)

generate_boxplot <- function(data, age_column, title = age_column, subtitle = "Boxplot by Gene") {
  
  # Check if age_column exists in the data
  if (!(age_column %in% colnames(data))) {
    stop(paste("The column", age_column, "does not exist in the provided data frame."))
  }
  
  # Filter out the NA values
  data <- data[!is.na(data[[age_column]]), ]
  
  # Get number of unique genes
  n <- length(unique(data$Gene))
  
  # Create a boxplot
  p <- ggplot(data, aes(y = Gene, x = !!sym(age_column))) +
    geom_boxplot(aes(fill = Gene)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face="bold", size=16),
          plot.subtitle = element_text(hjust = 0.5, face="italic", size=12),
          legend.position="none") +
    labs(title = title,
         subtitle = subtitle,
         x = age_column,
         y = "Gene") +
    scale_fill_viridis(discrete = TRUE, begin = 0.1, end = 0.9, direction = 1, option = "D", alpha = 1) 
  
  return(p)
}

pdf("/Users/f.fer/Documents/1_Projets/bar_plot.pdf",width = 10,height = 10)
p <- generate_boxplot(Data_demographique, "Age.first.symptoms..y.", "Age of First Symptoms", "Boxplot by Gene")
print(p)
Data_demographique$Age.au.diagnostic.clinique.EMG = as.numeric(Data_demographique$Age.au.diagnostic.clinique.EMG)
p <- generate_boxplot(Data_demographique, "Age.au.diagnostic.clinique.EMG", "Age at clinical diagnostic", "Boxplot by Gene")
print(p)
Data_demographique$Age.au.diagnostic.génétique = as.numeric(Data_demographique$Age.au.diagnostic.génétique)
p <- generate_boxplot(Data_demographique, "Age.au.diagnostic.génétique", "Age at genetic diagnostic", "Boxplot by Gene")
print(p)
p <- generate_boxplot(Data_demographique, "age.first.visit..dispo.", "Age at first visit", "Boxplot by Gene")
print(p)
dev.off()

#===============================================================================
# Données symptomes
#===============================================================================
df = data.frame(initial = Onset_symptom$MGFA,max = seveti_max$MGFA.1,severity=severi_last_visit$MGFA.2)
# Codification 
# Assurez-vous d'avoir la bibliothèque dplyr
if (!require(dplyr)) install.packages('dplyr')

# Utilisation de la fonction recode
df$MGFA_num_initial <- recode(df$initial,
                              "1" = 1,
                              "2a" = 2,
                              "2b" = 2,
                              "3a" = 3,
                              "3b" = 3,
                              "4a" = 4,
                              "4b" = 4,.default = NA_real_)

df$MGFA_num_max <- recode(df$max,
                          "1" = 1,
                          "2a" = 2,
                          "2b" = 2,
                          "3a" = 3,
                          "3b" = 3,
                          "4a" = 4,
                          "4b" = 4,.default = NA_real_)

df$MGFA_num_severity <- recode(df$severity,
                               "1" = 1,
                               "2a" = 2.5,
                               "2b" = 2,
                               "3a" = 3.5,
                               "3b" = 3,
                               "4a" = 4.5,
                               "4b" = 4,.default = NA_real_)

Data_analys = data.frame(
  Sex = Data_demographique$Sexe,
  Gene = sub(x = Data_demographique$Gene,pattern = "[ ].*$",replacement = " "),
  Age_first_sympt = Data_demographique$Age.first.symptoms..y.,
  Age_First_visit = Data_demographique$age.first.visit..dispo.,
  MGFA_ini = df$MGFA_num_initial,
  MGFA_sev = df$MGFA_num_severity
)
Data_analys$Gene = gsub(x = Data_analys$Gene," ","")

library(ggplot2)

ggplot(Data_analys, aes(x=Age_first_sympt)) +
  geom_histogram(binwidth=5, fill="blue", color="black") +
  theme_minimal() +
  xlab("Age at First Symptom") +
  ylab("Count") +
  ggtitle("Histogram of Age at First Symptom")

ggplot(Data_analys, aes(x=Sex, y=Age_first_sympt, fill=Sex)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Sex") +
  ylab("Age at First Symptom") +
  ggtitle("Boxplot of Age at First Symptom by Sex")

model <- lm(MGFA_sev ~ Age_first_sympt + Age_First_visit + Sex + Gene +Data_analys$MGFA_ini, data = Data_analys)
summary(model)

Data_analys$MGFA_delta <- Data_analys$MGFA_sev - Data_analys$MGFA_ini
Data_analys$disease_duration = Data_analys$Age_First_visit -Data_analys$Age_first_sympt
model <- lm(MGFA_sev~ disease_duration + Sex + Gene, data = Data_analys)
summary(model)

library(ggplot2)

ggplot(Data_analys, aes(x=Gene, y=MGFA_sev, fill=Gene)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution de MGFA_delta par gène", 
       x = "Gène", 
       y = "MGFA_delta", 
       fill = "Gène")


ggplot(Data_analys, aes(x=disease_duration, y=MGFA_delta, color=Gene)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "Évolution de MGFA_delta avec la durée de la maladie par gène", 
       x = "Durée de la maladie", 
       y = "MGFA_delta", 
       color = "Gène")


library(tidyverse)

# Remodelage de la dataframe
Data_analys_long <- Data_analys %>%
  gather(key = "MGFA_type", value = "MGFA_value", MGFA_ini, MGFA_sev)

# Création du boxplot
ggplot(Data_analys_long, aes(x = Gene, y = MGFA_value, fill = MGFA_type)) +
  geom_boxplot() +
  labs(title = "Distribution de MGFA initial et sévère par gène", 
       x = "Gène", 
       y = "Valeur de MGFA", 
       fill = "Type de MGFA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#==============================================================================
# traitement 
#==============================================================================
library(tidyverse)

# Restructuration des données de traitement
Treatments_long <- Treatments_init %>%
  mutate(Individual = row_number()) %>% # Ajout d'un identifiant d'individu
  gather(key = "Treatment", value = "Init_time", -Individual) %>% # Transformation en format long
  mutate(Treatment = gsub("\\.1", "", Treatment)) # Suppression du suffixe ".1" dans les noms de traitement

# Combinaison des données
full_data <- Data_analys %>%
  mutate(Individual = row_number()) %>%
  left_join(Treatments_long, by = "Individual")

library(lme4)

# Ajustement d'un modèle linéaire mixte
model <- lmer(MGFA_delta ~ Gene + Sex + (1 | Individual), data = full_data)
summary(model)

#======================
# Assurez-vous que le package "tidyverse" est installé et chargé
# install.packages("tidyverse")
library(tidyverse)

# Transformer les données
Treatments_init <- Treatments_init %>%
  pivot_wider(names_from = Treatment, values_from = Treatment, 
              values_fill = NA, values_fn = function(x) "True") %>%
  mutate(across(where(is.character), ~replace_na(., "False")))

head(full_data_one_hot)


model <- lm(MGFA_delta ~ disease_duration + Sex + Gene+AChE+X3.4.DAP+Fluoxetine+Quinidine+Ephedrine, data = full_data_one_hot)
summary(model)

#====================
df_transformed <- Treatments_init %>%
  mutate_all(~ifelse(. == "Non" | is.na(.), FALSE, TRUE))

# Affiche le dataframe transformé
df_transformed = cbind(Data_analys,df_transformed)
model <- lm(MGFA_delta ~ disease_duration + Sex + Gene+AChE.1+X3.4.DAP.1+Fluoxetine.1+Quinidine.1+Ephedrine.1+Salbutamol.1, data = df_transformed)
summary(model)

# Assume df is your data frame
# First, reshape the data
df_long <- df_transformed  %>%
  pivot_longer(
    cols = AChE.1:X3.4.DAP.1:Fluoxetine.1:Quinidine.1:Ephedrine.1:Salbutamol.1, 
    names_to = "treatment",
    values_to = "treatment_used"
  )

# Plot the data
ggplot(df_long, aes(x = treatment, y = MGFA_delta)) +
  geom_boxplot(aes(fill = treatment_used)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#===========================================================================================
#===========================================================================================
#===========================================================================================
#===========================================================================================
#===========================================================================================
#===========================================================================================

# white border
cell_border_fun <- function(j, i, x, y, width, height, fill) {
  grid.rect(x, y, width, height, gp = gpar(fill = fill, col = "black", lwd = 1))
}

# Creation de nouveles variables
Onset_symptom$Limbs
unique(Onset_symptom$If.yes..prox..or.dist.)
Limbs_proximal = grepl(x = Onset_symptom$If.yes..prox..or.dist.,pattern = "^Prox")
Limbs_dist = grepl(x = Onset_symptom$If.yes..prox..or.dist.,pattern = "Dist")
Limbs_dist[grepl(x = Onset_symptom$If.yes..prox..or.dist.,pattern = ">Dist")]  = NA
Limbs = Onset_symptom$Limbs
Limbs[Limbs=="NA"] = NA
Limbs = ifelse(Limbs=="Oui",TRUE,FALSE)
library(colorRamp2)
#===========================================================================================
#Tableau des valeurs qualitatives
#===========================================================================================
cpt_perc = function(x,y=Data_xsl$`Involved gene`){
  x[x=="NA"] = NA
  x[x == "Oui"] = TRUE
  x[x=="Non"] = FALSE
  x[x == "YES"] = TRUE
  x[x=="NO"] = FALSE
  return(table(y,x)[,"TRUE"]/rowSums(table(y,x)))
}
Is_stridor = function(x){
  return(grepl(x = x,pattern = "[Ss]tridor")||grepl(x = x,pattern = "[Vv]ocal"))
}
Is_laryn = function(x){
  return(grepl(x = x,pattern = "[Ll]aryn"))
}
Is_strid_laryn = function(x){
  return(Is_stridor(x)|Is_laryn(x))
}
#===============================================================================
# Metadata
#===============================================================================
metadata = data.frame(
  "Mean age at first symptome" = final_table$`Age at First Symptome_mean`,
  #"Mean age at last visit" = final_table$`Age Last visit_mean`,
 # "Mean diagnostic wandering time" = final_table$`Delay clinic diag EMG_mean`,
  "Proportion of Family history" = cpt_perc(Data_demographique$ATCDs.familiaux)
)
if(FALSE){
AFS_color =colorRamp2(range(metadata$Mean.age.at.first.symptome),colors =  brewer.pal(n = 9,name = "Oranges")[c(1,9)])
ALS_color =colorRamp2(range(metadata$Mean.age.at.last.visit),colors =  brewer.pal(n = 9,name = "Blues")[c(1,9)])
diag_color =colorRamp2(range(metadata$Mean.diagnostic.wandering.time),colors =  brewer.pal(n = 9,name = "YlOrRd")[c(1,9)])
FMH_color =colorRamp2(range(metadata$`Proportion of Family history`),colors =  brewer.pal(n = 9,name = "Greys")[c(1,9)])
}

AFS_color =colorRamp2(range(metadata$Mean.age.at.first.symptome),colors = c("white","#B31A15"))
ALS_color =colorRamp2(range(metadata$Mean.age.at.last.visit),colors =  c("white","#4F7ABB"))
diag_color =colorRamp2(range(metadata$Mean.diagnostic.wandering.time),colors =  c("white","#FA9203"))
FMH_color =colorRamp2(range(metadata$Proportion.of.Family.history),colors =  c("white","#6D0E4E"))


metadata = metadata[rownames(metadata)!="Others",]
colnames(metadata) = gsub(x = colnames(metadata),pattern = "[.]",replacement = " ")
row_ha <- rowAnnotation(  # 'row_ha' is an object that holds all the annotation (metadata) that will be added to the heatmap
  df = metadata,  
  # This parameter specifies the data frame containing the annotation data
  col = list( 
    "Mean age at first symptome" = AFS_color,
    #"Mean age at last visit" = ALS_color,
    #"Mean diagnostic wandering time" = diag_color,
    "Proportion of Family history" = FMH_color
  ),gp = gpar(col = "black")
  )


colnames(Onset_symptom)[11] = "Ptosis"
colnames(Onset_symptom)[12] = "Oculomotor"


#===============================================================================
# Symptome comparaison
#===============================================================================


data_tempo_list = list(Onset_symptom,severi_last_visit)
#pdf(file = "/Users/f.fer/Documents/1_Projets/20230801_julianT/2023_08_18_Heatmap_symptome_onset_final.pdf")
list_ht = list()
 i = 0
 #-----------------------------------------------------------------------------
#for(data_tempo in data_tempo_list){
# Iteration 0
 #-----------------------------------------------------------------------------
data_tempo = data_tempo_list[[1]]
i = i+1
#data_tempo = Onset_symptom
#data_tempo =severi_last_visit

unique(data_tempo$If.yes..prox..or.dist.)
Limbs_proximal = grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = "^Prox")
Limbs_dist = grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = "Dist")
Limbs_dist[grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = ">Dist")]  = NA
Limbs = data_tempo$Limbs
Limbs[Limbs=="NA"] = NA
Limbs = ifelse(Limbs=="Oui",TRUE,FALSE)

Symptome_init = data.frame(
  "Limb weakness" =  cpt_perc(data_tempo$Limbs),
  "Proximal weakness" =  cpt_perc(Limbs_proximal),
  "Distal weakness" = cpt_perc(Limbs_dist),
  "Axial muscle weakness" =cpt_perc(data_tempo$Axial),
  "Respiratoire Symptoms" =cpt_perc(data_tempo$Respiratory),
  #"Episodic Apnea" =cpt_perc(data_tempo$Apnée.épisodique),
  #"Respiratory childhood insuffisiency" =cpt_perc(data_tempo$Episodes.de.DRA.dans.l.enfance),
  "Fatigability" = cpt_perc(data_tempo$Fatigability),
  "Ptosis" = cpt_perc(data_tempo$Ptosis),
  "Ophtalmoparesis" = cpt_perc(data_tempo$Oculomotor),
  #"Arthrogryposis" =cpt_perc(data_tempo$arthrogryposis),
  #"Delayed Muscle Milestone" =cpt_perc(data_tempo$Delayed.MM),
  #"Intellectual Disability" = cpt_perc(data_tempo$Déficit.intellectuel),
  "Facial weakness" =cpt_perc(data_tempo$Facial.weakness),
  #"Scoliosis" = cpt_perc(data_tempo$Scoliosis),
  #"Stridor" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN =Is_strid_laryn))),#|unlist(lapply(data_tempo$Other.features,FUN = Is_stridor))),
  #"Larynx" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN = Is_laryn)),
  "Bulbar symptoms"= cpt_perc(data_tempo$Bulbar)
)
colnames(Symptome_init) = gsub(x = colnames(Symptome_init),pattern = "[.]",replacement = " ")
library(dplyr)
library(reshape2)
library(ComplexHeatmap)  # For creating heatmaps
library(colorRamp2)  # For creating color gradients
library(ggplot2)  # For general plotting
library(RColorBrewer)  # For color palettes
my_palette <- colorRampPalette(brewer.pal(9, "Greens"))(n = 299) 
# white border
Symptome_init = Symptome_init[!(rownames(Symptome_init)%in%"Others"),]
list_ht[i] <- Heatmap(as.matrix(Symptome_init)*100,  # The Heatmap function takes a matrix as input, here we convert our data frame (mat) to a matrix
              name = "Proportion of Symptome (%)",  # This parameter provides a name for the heatmap legend
              col = my_palette,  # Defines the color scheme of the heatmap using the 'my_palette' object we created earlier. It'll map the scores in our data to the colors in this palette
              #left_annotation = row_ha,  # This parameter is used to add annotations (metadata) to the rows of the heatmap. The annotation object 'row_ha' that we created earlier is passed here
              show_row_names = TRUE,  # This parameter controls whether the row names of the matrix (which correspond to the muscle names) should be shown on the heatmap
              show_column_names = TRUE,  # This parameter controls whether the column names of the matrix (which correspond to the patients) should be shown on the heatmap
              row_title = "Gene",  # This parameter is used to specify a title for the row names of the heatmap
             # column_title = "",  # This parameter is used to specify a title for the column names of the heatmap. It's left empty here
              cluster_rows = TRUE,  # This parameter controls whether to hierarchically cluster the rows (muscles), i.e., group together muscles that have similar data patterns across patients
              clustering_distance_rows = "manhattan",
              clustering_distance_columns = "manhattan", 
              clustering_method_columns ="ward.D2",
              clustering_method_rows  ="ward.D2",
              column_title = "First Visit",column_title_side = "bottom",column_title_gp = gpar(fontsize = 20, fontface = "bold"),
              cluster_columns = TRUE,  # This parameter controls whether to hierarchically cluster the columns (patients), i.e., group together patients that have similar muscle score patterns
              row_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 10),  # This parameter allows us to specify graphical parameters for the row names. Here, the font family is set to "serif", the font style is "italic", and the font size is 10
              column_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 10),
              cell_fun = cell_border_fun,  # This parameter allows us to specify graphical parameters for the column names. Similar to the row names, the font family is "serif", the font style is "italic", and the font size is 10
             heatmap_legend_param = list(
               border = "black", # Bordure fine autour de la légende
               title_gp = gpar(fontsize = 12, fontface = "bold"), # Police du titre
               labels_gp = gpar(fontsize = 10), # Police des étiquettes
               legend_width = unit(1, "cm") # Décalage de la légende
             ))
tempo = column_order(list_ht[[1]])
tempo2 = row_order(list_ht[[1]])
S1 =Symptome_init


row_ha <- rowAnnotation(  # 'row_ha' is an object that holds all the annotation (metadata) that will be added to the heatmap
  df = metadata,  
  # This parameter specifies the data frame containing the annotation data
  col = list( 
    "Mean age at first symptome" = AFS_color,
    #"Mean age at last visit" = ALS_color,
    #"Mean diagnostic wandering time" = diag_color,
    "Proportion of Family history" = FMH_color
  ),gp = gpar(col = "black")
)


colnames(Onset_symptom)[11] = "Ptosis"
colnames(Onset_symptom)[12] = "Oculomotor"





i = i+1
Symptome_init = data.frame(
  #"Limb weakness" =  cpt_perc(data_tempo$Limbs),
  #"Proximal weakness" =  cpt_perc(Limbs_proximal),
  #"Distal weakness" = cpt_perc(Limbs_dist),
  #"Axial muscle weakness" =cpt_perc(data_tempo$Axial),
  #"Respiratory" =cpt_perc(data_tempo$Respiratory),
  #"Episodic Apnea" =cpt_perc(data_tempo$Apnée.épisodique),
  "Sudden childhood respiratory insuffisiency" =cpt_perc(data_tempo$Episodes.de.DRA.dans.l.enfance),
  #"Fatigability" = cpt_perc(data_tempo$Fatigability),
  #"Ptosis" = cpt_perc(data_tempo$Ptosis),
  #"Ophtalmoparesis" = cpt_perc(data_tempo$Oculomotor),
  "Arthrogryposis" =cpt_perc(data_tempo$arthrogryposis),
  "Delayed Motor Milestones" =cpt_perc(data_tempo$Delayed.MM),
  "Intellectual Disability" = cpt_perc(data_tempo$Déficit.intellectuel),
  #"Facial weakness" =cpt_perc(data_tempo$Facial.weakness),
  "Scoliosis" = cpt_perc(data_tempo$Scoliosis)
  #"Stridor" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN =Is_strid_laryn))),#|unlist(lapply(data_tempo$Other.features,FUN = Is_stridor))),
  #"Larynx" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN = Is_laryn)),
  #"Bulbar symptoms"= cpt_perc(data_tempo$Bulbar)
  )
colnames(Symptome_init) = gsub(x = colnames(Symptome_init),pattern = "[.]",replacement = " ")
  
list_ht[i] <- Heatmap(as.matrix(Symptome_init[!(rownames(Symptome_init)%in%"Others"),])*100,  # The Heatmap function takes a matrix as input, here we convert our data frame (mat) to a matrix
                      name = "Proportion of Symptome (%)",  # This parameter provides a name for the heatmap legend
                      col = my_palette,  # Defines the color scheme of the heatmap using the 'my_palette' object we created earlier. It'll map the scores in our data to the colors in this palette
                      left_annotation = row_ha,  # This parameter is used to add annotations (metadata) to the rows of the heatmap. The annotation object 'row_ha' that we created earlier is passed here
                      show_row_names = TRUE,  # This parameter controls whether the row names of the matrix (which correspond to the muscle names) should be shown on the heatmap
                      show_column_names = TRUE,  # This parameter controls whether the column names of the matrix (which correspond to the patients) should be shown on the heatmap
                      row_title = "Gene",  # This parameter is used to specify a title for the row names of the heatmap
                      # column_title = "",  # This parameter is used to specify a title for the column names of the heatmap. It's left empty here
                      cluster_rows = FALSE,  # This parameter controls whether to hierarchically cluster the rows (muscles), i.e., group together muscles that have similar data patterns across patients
                      clustering_distance_rows = "manhattan",
                      clustering_distance_columns = "manhattan", 
                      clustering_method_columns ="ward.D2",
                      clustering_method_rows  ="ward.D2",
                      column_title = "",column_title_side = "bottom",row_order = tempo2,
                      cluster_columns = TRUE,  # This parameter controls whether to hierarchically cluster the columns (patients), i.e., group together patients that have similar muscle score patterns
                      row_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 10),  # This parameter allows us to specify graphical parameters for the row names. Here, the font family is set to "serif", the font style is "italic", and the font size is 10
                      column_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 10),
                      cell_fun = cell_border_fun,  # This parameter allows us to specify graphical parameters for the column names. Similar to the row names, the font family is "serif", the font style is "italic", and the font size is 10
                      heatmap_legend_param = list(
                        border = "black", # Bordure fine autour de la légende
                        title_gp = gpar(fontsize = 12, fontface = "bold"), # Police du titre
                        labels_gp = gpar(fontsize = 10), # Police des étiquettes
                        legend_width = unit(1, "cm") # Décalage de la légende
                      ),
                      )



#}
#-----------------------------------------------------------------------------
#for(data_tempo in data_tempo_list){
# Iteration 1
#-----------------------------------------------------------------------------
#for(data_tempo in data_tempo_list){
# Iteration 1
data_tempo = data_tempo_list[[2]]
i = i+1
#data_tempo = Onset_symptom
#data_tempo =severi_last_visit

unique(data_tempo$If.yes..prox..or.dist.)
Limbs_proximal = grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = "^Prox")
Limbs_dist = grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = "Dist")
Limbs_dist[grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = ">Dist")]  = NA
Limbs = data_tempo$Limbs
Limbs[Limbs=="NA"] = NA
Limbs = ifelse(Limbs=="Oui",TRUE,FALSE)

Symptome_init = data.frame(
  "Limb weakness" =  cpt_perc(data_tempo$Limbs),
  "Proximal weakness" =  cpt_perc(Limbs_proximal),
  "Distal weakness" = cpt_perc(Limbs_dist),
  "Axial muscle weakness" =cpt_perc(data_tempo$Axial),
  "Respiratoire Symptoms" =cpt_perc(data_tempo$Respiratory),
  #"Episodic Apnea" =cpt_perc(data_tempo$Apnée.épisodique),
  #"Respiratory childhood insuffisiency" =cpt_perc(data_tempo$Episodes.de.DRA.dans.l.enfance),
  "Fatigability" = cpt_perc(data_tempo$Fatigability),
  "Ptosis" = cpt_perc(data_tempo$Ptosis),
  "Ophtalmoparesis" = cpt_perc(data_tempo$Oculomotor),
  #"Arthrogryposis" =cpt_perc(data_tempo$arthrogryposis),
  #"Delayed Motor Milestone" =cpt_perc(data_tempo$Delayed.MM),
  #"Intellectual Disability" = cpt_perc(data_tempo$Déficit.intellectuel),
  "Facial weakness" =cpt_perc(data_tempo$Facial.weakness),
  #"Scoliosis" = cpt_perc(data_tempo$Scoliosis),
  #"Stridor" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN =Is_strid_laryn))),#|unlist(lapply(data_tempo$Other.features,FUN = Is_stridor))),
  #"Larynx" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN = Is_laryn)),
  "Bulbar symptoms"= cpt_perc(data_tempo$Bulbar)
)
colnames(Symptome_init) = gsub(x = colnames(Symptome_init),pattern = "[.]",replacement = " ")
library(dplyr)
library(reshape2)
library(ComplexHeatmap)  # For creating heatmaps
library(colorRamp2)  # For creating color gradients
library(ggplot2)  # For general plotting
library(RColorBrewer)  # For color palettes
my_palette <- colorRampPalette(brewer.pal(9, "Greens"))(n = 299) 

Symptome_init = Symptome_init[!(rownames(Symptome_init)%in%"Others"),]
list_ht[i] <- Heatmap(as.matrix(Symptome_init)*100,  # The Heatmap function takes a matrix as input, here we convert our data frame (mat) to a matrix
                      name = "Proportion of Symptome (%)",  # This parameter provides a name for the heatmap legend
                      col = my_palette,  # Defines the color scheme of the heatmap using the 'my_palette' object we created earlier. It'll map the scores in our data to the colors in this palette
                      #left_annotation = row_ha,  # This parameter is used to add annotations (metadata) to the rows of the heatmap. The annotation object 'row_ha' that we created earlier is passed here
                      show_row_names = TRUE,  # This parameter controls whether the row names of the matrix (which correspond to the muscle names) should be shown on the heatmap
                      show_column_names = TRUE,  # This parameter controls whether the column names of the matrix (which correspond to the patients) should be shown on the heatmap
                      row_title = "Gene",  # This parameter is used to specify a title for the row names of the heatmap
                      #column_title = "Last Visit",  # This parameter is used to specify a title for the column names of the heatmap. It's left empty here
                      cluster_rows = FALSE,  # This parameter controls whether to hierarchically cluster the rows (muscles), i.e., group together muscles that have similar data patterns across patients
                      clustering_distance_rows = "manhattan",
                      clustering_distance_columns = "manhattan", 
                      clustering_method_columns ="ward.D2",
                      clustering_method_rows  ="ward.D2",column_title_side = "bottom",
                      column_title = "Last Visit",column_order = tempo,
                      column_title_gp = gpar(fontsize = 20, fontface = "bold"),row_order = tempo2,
                      cluster_columns = FALSE,  # This parameter controls whether to hierarchically cluster the columns (patients), i.e., group together patients that have similar muscle score patterns
                      row_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 10),  # This parameter allows us to specify graphical parameters for the row names. Here, the font family is set to "serif", the font style is "italic", and the font size is 10
                      column_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 10),
                      cell_fun = cell_border_fun,  # This parameter allows us to specify graphical parameters for the column names. Similar to the row names, the font family is "serif", the font style is "italic", and the font size is 10
                      heatmap_legend_param = list(
                        border = "black", # Bordure fine autour de la légende
                        title_gp = gpar(fontsize = 12, fontface = "bold"), # Police du titre
                        labels_gp = gpar(fontsize = 10), # Police des étiquettes
                        legend_width = unit(1, "cm") # Décalage de la légende
                      ))


#}

ht_opt(legend_border = "black",annotation_border = TRUE)
#-----------------------------------------------------------------------------
ht_list =   list_ht[[2]] + list_ht[[1]]+list_ht[[3]]  
draw(ht_list, ht_gap = unit(c(0.1,2), "cm") ,merge_legend = TRUE, column_title_gp = gpar(fontsize = 16))
#-----------------------------------------------------------------------------



ht_list = row_ha + list_ht[[2]]  



S3 =(S1-S2)

Heatmap(as.matrix(Symptome_init)*100,  # The Heatmap function takes a matrix as input, here we convert our data frame (mat) to a matrix
        name = "Proportion of Symptome (%)",  # This parameter provides a name for the heatmap legend
        #col = my_palette,  # Defines the color scheme of the heatmap using the 'my_palette' object we created earlier. It'll map the scores in our data to the colors in this palette
        #left_annotation = row_ha,  # This parameter is used to add annotations (metadata) to the rows of the heatmap. The annotation object 'row_ha' that we created earlier is passed here
        show_row_names = TRUE,  # This parameter controls whether the row names of the matrix (which correspond to the muscle names) should be shown on the heatmap
        show_column_names = TRUE,  # This parameter controls whether the column names of the matrix (which correspond to the patients) should be shown on the heatmap
        row_title = "Gene",  # This parameter is used to specify a title for the row names of the heatmap
        #column_title = "Last Visit",  # This parameter is used to specify a title for the column names of the heatmap. It's left empty here
        cluster_rows = TRUE,  # This parameter controls whether to hierarchically cluster the rows (muscles), i.e., group together muscles that have similar data patterns across patients
        clustering_distance_rows = "manhattan",
        clustering_distance_columns = "manhattan", 
        clustering_method_columns ="ward.D2",
        clustering_method_rows  ="ward.D2",column_title_side = "bottom",
        column_title = "Last Visit",column_order = tempo,
        cluster_columns = FALSE,  # This parameter controls whether to hierarchically cluster the columns (patients), i.e., group together patients that have similar muscle score patterns
        row_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 10),  # This parameter allows us to specify graphical parameters for the row names. Here, the font family is set to "serif", the font style is "italic", and the font size is 10
        column_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 10),
        cell_fun = cell_border_fun)  # This parameter allows us to specify graphical parameters for the column names. Similar to the row names, the font family is "serif", the font style is "italic", and the font size is 10
#



S3  = (S2-S1)/S1
S3[is.na(S3)] = 0
S3[S3==Inf] = 1
S3[S3>1] = 1
S3[S3<(-1)] = -1
ht <- Heatmap(as.matrix(S3)*100,  # The Heatmap function takes a matrix as input, here we convert our data frame (mat) to a matrix
                      name = "Symptom Evolutions (%) ",  # This parameter provides a name for the heatmap legend
                      #col = my_palette,  # Defines the color scheme of the heatmap using the 'my_palette' object we created earlier. It'll map the scores in our data to the colors in this palette
                      #left_annotation = row_ha,  # This parameter is used to add annotations (metadata) to the rows of the heatmap. The annotation object 'row_ha' that we created earlier is passed here
                      show_row_names = TRUE,  # This parameter controls whether the row names of the matrix (which correspond to the muscle names) should be shown on the heatmap
                      show_column_names = TRUE,  # This parameter controls whether the column names of the matrix (which correspond to the patients) should be shown on the heatmap
                      row_title = "Gene",  # This parameter is used to specify a title for the row names of the heatmap
                      #column_title = "Last Visit",  # This parameter is used to specify a title for the column names of the heatmap. It's left empty here
                      cluster_rows = TRUE,  # This parameter controls whether to hierarchically cluster the rows (muscles), i.e., group together muscles that have similar data patterns across patients
                      clustering_distance_rows = "manhattan",
                      clustering_distance_columns = "manhattan", 
                      clustering_method_columns ="ward.D2",
                      clustering_method_rows  ="ward.D2",
                      column_title_side = "bottom",
                      column_title = expression(Delta['Symptom'] == frac({'Onset'}['[%]'] - {'Last visit'}['[%]'],{'Onset'}['[%]'])),#"Evolution of symptom proportions\n between the first and the last visit",#column_order = tempo,
                      cluster_columns = TRUE,  # This parameter controls whether to hierarchically cluster the columns (patients), i.e., group together patients that have similar muscle score patterns
                      row_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 10),  # This parameter allows us to specify graphical parameters for the row names. Here, the font family is set to "serif", the font style is "italic", and the font size is 10
                      column_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 10),
                      cell_fun = cell_border_fun)  # This parameter allows us to specify graphical parameters for the column names. Similar to the row names, the font family is "serif", the font style is "italic", and the font size is 10
#




#final graph
#-----------------------------------------------------------------------------

#draw(ht)
dev.off()

#===============================================================================
# Full Initial
#===============================================================================
data_tempo = Onset_symptom
pdf(file = "/Users/f.fer/Documents/1_Projets/20230801_julianT/2023_08_18_Heatmap_symptome_onset_full.pdf")

  
  data_tempo = Onset_symptom
  #data_tempo =severi_last_visit
  
  unique(data_tempo$If.yes..prox..or.dist.)
  Limbs_proximal = grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = "^Prox")
  Limbs_dist = grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = "Dist")
  Limbs_dist[grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = ">Dist")]  = NA
  Limbs = data_tempo$Limbs
  Limbs[Limbs=="NA"] = NA
  Limbs = ifelse(Limbs=="Oui",TRUE,FALSE)
  
  Symptome_init = data.frame(
    "Limb weakness" =  cpt_perc(data_tempo$Limbs),
    "Proximal weakness" =  cpt_perc(Limbs_proximal),
    "Distal weakness" = cpt_perc(Limbs_dist),
    "Axial muscle weakness" =cpt_perc(data_tempo$Axial),
    "Respiratory" =cpt_perc(data_tempo$Respiratory),
    "Episodic Apnea" =cpt_perc(data_tempo$Apnée.épisodique),
    "Respiratory childhood insuffisiency" =cpt_perc(data_tempo$Episodes.de.DRA.dans.l.enfance),
    "Fatigability" = cpt_perc(data_tempo$Fatigability),
    "Ptosis" = cpt_perc(data_tempo$ptosis),
    "Ophtalmoparesis" = cpt_perc(data_tempo$oculomotor),
    "Arthrogryposis" =cpt_perc(data_tempo$arthrogryposis),
    "Delayed Muscle Milestone" =cpt_perc(data_tempo$Delayed.MM),
    "Intellectual Disability" = cpt_perc(data_tempo$Déficit.intellectuel),
    "Facial weakness" =cpt_perc(data_tempo$Facial.weakness),
    "Scoliosis" = cpt_perc(data_tempo$Scoliosis),
    "Stridor" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN =Is_strid_laryn))),#|unlist(lapply(data_tempo$Other.features,FUN = Is_stridor))),
    #"Larynx" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN = Is_laryn)),
    "Bulbar symptoms"= cpt_perc(data_tempo$Bulbar)
  )
  colnames(Symptome_init) = gsub(x = colnames(Symptome_init),pattern = "[.]",replacement = " ")
  library(dplyr)
  library(reshape2)
  library(ComplexHeatmap)  # For creating heatmaps
  library(colorRamp2)  # For creating color gradients
  library(ggplot2)  # For general plotting
  library(RColorBrewer)  # For color palettes
  my_palette <- colorRampPalette(brewer.pal(9, "Greens"))(n = 299) 
  # white border
  Symptome_init = Symptome_init[!(rownames(Symptome_init)%in%"Others"),]
  ht <- Heatmap(as.matrix(Symptome_init)*100,  # The Heatmap function takes a matrix as input, here we convert our data frame (mat) to a matrix
                name = "Proportion of Symptome (%)",  # This parameter provides a name for the heatmap legend
                col = my_palette,  # Defines the color scheme of the heatmap using the 'my_palette' object we created earlier. It'll map the scores in our data to the colors in this palette
                left_annotation = row_ha,  # This parameter is used to add annotations (metadata) to the rows of the heatmap. The annotation object 'row_ha' that we created earlier is passed here
                show_row_names = TRUE,  # This parameter controls whether the row names of the matrix (which correspond to the muscle names) should be shown on the heatmap
                show_column_names = TRUE,  # This parameter controls whether the column names of the matrix (which correspond to the patients) should be shown on the heatmap
                row_title = "Gene",  # This parameter is used to specify a title for the row names of the heatmap
                column_title = "",  # This parameter is used to specify a title for the column names of the heatmap. It's left empty here
                cluster_rows = TRUE,  # This parameter controls whether to hierarchically cluster the rows (muscles), i.e., group together muscles that have similar data patterns across patients
                clustering_distance_rows = "manhattan",
                clustering_distance_columns = "manhattan", 
                clustering_method_columns ="ward.D2",
                clustering_method_rows  ="ward.D2",column_title_side = "bottom",
                cluster_columns = TRUE,  # This parameter controls whether to hierarchically cluster the columns (patients), i.e., group together patients that have similar muscle score patterns
                row_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 10),  # This parameter allows us to specify graphical parameters for the row names. Here, the font family is set to "serif", the font style is "italic", and the font size is 10
                column_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 10),
                heatmap_legend_param = ,
                cell_fun = cell_border_fun)  # This parameter allows us to specify graphical parameters for the column names. Similar to the row names, the font family is "serif", the font style is "italic", and the font size is 10
  #
  draw(ht)
  
dev.off()

data$Libre[unlist(lapply(data$Libre,FUN = Is_stridor))]
Is_laryn = function(x){
  return(grepl(x = x,pattern = "[Ll]aryn"))
}
data$Libre[unlist(lapply(data$Libre,FUN = Is_laryn))]
Is_laryn = function(x){
  return(grepl(x = x,pattern = "[Ll]aryn"))
}
Is_hypo = function(x){
  return(grepl(x = x,pattern = "[Hh]ypo"))
}

#===============================================================================
Symptome_init = data.frame(
  "Patient" = c(Data_demographique$Nom,Data_demographique$Nom),
  "Gene" = c(Data_demographique$Gene_2,Data_demographique$Gene_2),
  "Limb weakness" =  c(Onset_symptom$Limbs,severi_last_visit$Limbs),
  "Axial muscle weakness" =c(Onset_symptom$Axial,severi_last_visit$Axial),
  "Respiratory" =c(Onset_symptom$Respiratory,severi_last_visit$Respiratory.1),
  "Fatigability" =c(Onset_symptom$Respiratory,severi_last_visit$Respiratory.1),
  "Ptosis" = cpt_perc(data_tempo$ptosis),
  "Ophtalmoparesi" = cpt_perc(data_tempo$oculomotor),
  "Arthrogryposis" =cpt_perc(data_tempo$arthrogryposis),
  "Delayed Muscle Milestone" =cpt_perc(data_tempo$Delayed.MM),
  "Intellectual Disability" = cpt_perc(data_tempo$Déficit.intellectuel),
  "Facial weakness" =cpt_perc(data_tempo$Facial.weakness),
  "Scoliosis" = cpt_perc(data_tempo$Scoliosis),
  "Stridor" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN =Is_strid_laryn))),#|unlist(lapply(data_tempo$Other.features,FUN = Is_stridor))),
  "Hypotonia" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN =Is_hypo))),
  #"Larynx" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN = Is_laryn)),
  "Bulbar symptoms"= cpt_perc(data_tempo$Bulbar)
)
  



