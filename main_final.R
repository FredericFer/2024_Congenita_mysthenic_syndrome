#==============================================================================================================================================================
# Integration des bibliothèques
#==============================================================================================================================================================
library(xlsx)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(ComplexHeatmap)  # For creating heatmaps
library(colorRamp2)  # For creating color gradients
library(ggplot2)  # For general plotting
library(RColorBrewer)  # For color palettes
library(colorRamp2)
library(dendextend)
#==============================================================================================================================================================
# Intégration et retraitement des données
#==============================================================================================================================================================
#data = read.xlsx(file = "/Users/f.fer/Documents/1_Projets/20230801_julianT/Tableur CMS 20-09-2023_v3.xlsx",sheetIndex = 1,startRow = 2)
#data = read.xlsx(file = "/Users/f.fer/Documents/1_Projets/20230801_julianT/Tableur CMS 20-09-2023_v4.xlsx",sheetIndex = 1,startRow = 2)
#data = read.xlsx(file = "C:/Users/f.fer/Documents/1_Projets/20230801_julianT/Tableur CMS V9_2.xlsx",sheetIndex = 1,startRow = 2)
data = read.xlsx(file = "Tableur CMS V9_2.xlsx",sheetIndex = 1,startRow = 2)
#===============================================================================
# Général
#===============================================================================
Data_demographique = data[,c(1:23)]
Onset_symptom = data[,c(24:49)]
colnames(Onset_symptom)[11] = "Ptosis"
colnames(Onset_symptom)[12] = "Oculomotor"
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
# Courses
#===============================================================================
Pousse = data$Poussées
Course = data$Course
Course[Course%in%c("Worsening, stable","Worsening, Stable")] = "Worsening => stable"
#===============================================================================
# Misdiag
#===============================================================================
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
#===============================================================================
# Divers
#===============================================================================
Data_demographique$ATCDs.familiaux[Data_demographique$ATCDs.familiaux=="NA"] = NA
Data_demographique$ATCDs.familiaux = ifelse(Data_demographique$ATCDs.familiaux=="Oui","YES","NO")
Data_demographique$consanguinity =  ifelse(Data_demographique$consanguinity=="Oui","YES","NO")
Data_demographique$Cas.index = ifelse(Data_demographique$Cas.index=="Oui","YES","NO")
#==============================================================================================================================================================
# Enregistrement dans des tables
#==============================================================================================================================================================
#===============================================================================
# Onglet 1 : Data
#===============================================================================
Data_xsl = data.frame(
  "Initials" =  paste(substr(Data_demographique$Nom,1,1),substr(sub(x = Data_demographique$Nom,pattern = "^.* ",""),1,1),sep=""),
  "Sex" = ifelse(Data_demographique$Sexe=="M","Male","Female"),
  "Family origin" = Data_demographique$Family.origin,
  "Involved gene" = Data_demographique$Gene_2,
  "flare-up" = Pousse,
  "Evolution" = Course,
  "Period First Symptom" = Data_demographique$Age.first.symptoms..y.,
  "Age at First visit" =  Data_demographique$age.first.visit..dispo.,
  "Age at clinic Diagnostic)" =  Data_demographique$Age.au.diagnostic.clinique.EMG,
  "Age at Genetic Diagnostic" =  Data_demographique$age.first.visit..dispo.,
  "Age at first Symptome" = Age.first.symptoms_cat_value,
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
##write.xlsx(df, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Data", row.names = FALSE)
#write.xlsx(df, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Data", row.names = FALSE)
#===============================================================================
# Onglet 2 : Age
#===============================================================================
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
##write.xlsx(final_table, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Age", append = TRUE, row.names = TRUE)
#write.xlsx(final_table, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Age", append = TRUE, row.names = TRUE)
#===============================================================================
# Onglet 3 : Comptage
#===============================================================================
tempo = Data_xsl$Involved.gene
for(tab in names(results_c)){
  ##write.xlsx(results_c[tab], "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = paste0("cpt ",tab), append = TRUE, row.names = TRUE)
}
##write.xlsx(cbind.data.frame(results_c), "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Comptages", append = TRUE, row.names = TRUE)
#write.xlsx(cbind.data.frame(results_c), "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Comptages", append = TRUE, row.names = TRUE)
#===============================================================================
# Onglet 4 : Pourcentage V1
#===============================================================================
for(tab in names(results_p)){
  ##write.xlsx(results_p[tab], "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = paste0("perc1  ",tab), append = TRUE, row.names = TRUE)
}
##write.xlsx(cbind.data.frame(results_p), "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Percentage1", append = TRUE, row.names = TRUE)
#write.xlsx(cbind.data.frame(results_p), "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Percentage1", append = TRUE, row.names = TRUE)
#===============================================================================
# Onglet 5 : Pourcentage V2
#===============================================================================
for(tab in names(results_pp)){
  # #write.xlsx(results_pp[tab], "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = paste0("perc2  ",tab), append = TRUE, row.names = TRUE)
}
##write.xlsx(cbind.data.frame(results_pp), "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Percentage2", append = TRUE, row.names = TRUE)
#write.xlsx(cbind.data.frame(results_pp), "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Percentage2", append = TRUE, row.names = TRUE)
#===============================================================================
# Onglet 6 : Misdiag
#===============================================================================
##write.xlsx(cbind.data.frame(cross_table), "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Misdiag", append = TRUE, row.names = TRUE)
#write.xlsx(cbind.data.frame(cross_table), "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "Misdiag", append = TRUE, row.names = TRUE)

#==============================================================================================================================================================
# Analyses statistiques
#==============================================================================================================================================================


#==============================================================================================================================================================
# Heatmaps
#==============================================================================================================================================================
#===============================================================================
# Fonction gérant les bordures des cellules dans la heatmap
#===============================================================================
# white border
cell_border_fun <- function(j, i, x, y, width, height, fill) {
  grid.rect(x, y, width, height, gp = gpar(fill = fill, col = "black", lwd = 1))
}

#===============================================================================
# Creation de nouveles variables
#===============================================================================
Limbs_proximal = grepl(x = Onset_symptom$If.yes..prox..or.dist.,pattern = "^Prox")
Limbs_dist = grepl(x = Onset_symptom$If.yes..prox..or.dist.,pattern = "Dist")
Limbs_dist[grepl(x = Onset_symptom$If.yes..prox..or.dist.,pattern = ">Dist")]  = NA
Limbs = Onset_symptom$Limbs
Limbs[Limbs=="NA"] = NA
Limbs = ifelse(Limbs=="Oui",TRUE,FALSE)
#===============================================================================
#fonctions permettant d'obtenir les proportions par gène pour 
# Les variables de la décoration à droite
#===============================================================================
cpt_perc = function(x,y=Data_xsl$`Involved gene`){
  x[x=="NA"] = NA
  x[x == "Oui"] = TRUE
  x[x=="Non"] = FALSE
  x[x == "YES"] = TRUE
  x[x=="NO"] = FALSE
  x[x == "Yes"] = TRUE
  x[x=="No"] = FALSE
  return(table(y,x)[,"TRUE"]/rowSums(table(y,x)))
}
cpt_perc2 = function(x,y=Data_xsl$`Involved gene`){
  x[x=="NA"] = NA
  x[x == "Oui"] = TRUE
  x[x=="Non"] = FALSE
  x[x == "YES"] = TRUE
  x[x=="NO"] = FALSE
  x[x == "Yes"] = TRUE
  x[x=="No"] = FALSE
  return(table(y,x)[,"TRUE"])
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
Is_hypo = function(x){
  return(grepl(x = x,pattern = "[Hh]ypo"))
}
#===============================================================================
#  Création du tableau de Metadata (décoration à droite)
#===============================================================================
# au moins deux valeurs pour que le type de variable reste dataframe
metadata = data.frame(
  "Mean age at first symptoms" = final_table$`Age at First Symptome_mean`,
  #"Mean age at last visit" = final_table$`Age Last visit_mean`,
  # "Mean diagnostic wandering time" = final_table$`Delay clinic diag EMG_mean`,
  "Proportion of Family history" = cpt_perc(Data_demographique$ATCDs.familiaux)
)
metadata = metadata[rownames(metadata)!="Others",]
colnames(metadata) = gsub(x = colnames(metadata),pattern = "[.]",replacement = " ")
metadata2 = metadata$`Mean age at first symptoms`
names(metadata2) = rownames(metadata)
metadata = data.frame("Mean age at first symptoms" = metadata2)
colnames(metadata) = "Mean age at first symptoms"
#===============================================================================
#  Choix des couleurs (décoration à droite + general)
#===============================================================================
my_palette <- colorRampPalette(brewer.pal(9, "Blues"))(n = 299) 
AFS_color =colorRamp2(range(metadata$`Mean age at first symptoms`),colors = c("white","#B31A15"))
#ALS_color =colorRamp2(range(metadata$Mean.age.at.last.visit),colors =  c("white","#4F7ABB"))
#diag_color =colorRamp2(range(metadata$Mean.diagnostic.wandering.time),colors =  c("white","#FA9203"))
#FMH_color =colorRamp2(range(metadata$Proportion.of.Family.history),colors =  c("white","#6D0E4E"))
#===============================================================================
#  Objet final (décoration à droite)
#===============================================================================
row_ha <- rowAnnotation(  # 'row_ha' is an object that holds all the annotation (metadata) that will be added to the heatmap
  df = metadata,  
  # This parameter specifies the data frame containing the annotation data
  col = list( 
    "Mean age at first symptoms" = AFS_color#,
    #"Mean age at last visit" = ALS_color,
    #"Mean diagnostic wandering time" = diag_color,
    #"Proportion of Family history" = FMH_color
  ),gp = gpar(col = "black"),
  annotation_legend_param = list(
    "Mean age at first symptoms" = list(
      title_gp = gpar(fontfamily = "Times New Roman",fontface = "bold", fontsize = 12),
      labels_gp = gpar(fontfamily = "Times New Roman", fontsize = 10)
    ))
)
#===============================================================================
# Première Heatmap (avec classification)
#===============================================================================
# Creation de la liste de heatmap
list_ht = list()
i = 1
data_tempo = Onset_symptom
# Inutile ?
unique(data_tempo$If.yes..prox..or.dist.)
Limbs_proximal = grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = "^Prox")
Limbs_dist = grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = "Dist")
Limbs_dist[grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = ">Dist")]  = NA
Limbs = data_tempo$Limbs
Limbs[Limbs=="NA"] = NA
Limbs = ifelse(Limbs=="Oui",TRUE,FALSE)
Contracture = 
# Creation du data.frame 
Symptome_init = data.frame(
  #"Limb weakness" =  cpt_perc(data_tempo$Limbs),
  "Proximal weakness" =  cpt_perc(Limbs_proximal),
  "Distal weakness" = cpt_perc(Limbs_dist),
  "Axial muscle weakness" =cpt_perc(data_tempo$Axial),
  "Respiratory symptoms" =cpt_perc(data_tempo$Respiratory),
  #"Episodic Apnea" =cpt_perc(data_tempo$Apnée.épisodique),
  "Sudden childhood respiratory insufficiency" =cpt_perc(data_tempo$Episodes.de.DRA.dans.l.enfance),
  "Fatigability" = cpt_perc(data_tempo$Fatigability),
  "Ptosis" = cpt_perc(data_tempo$Ptosis),
  "Ophtalmoparesis" = cpt_perc(data_tempo$Oculomotor),
  "Arthrogryposis" =cpt_perc(data_tempo$arthrogryposis),
  "Delayed motor milestones" =cpt_perc(data_tempo$Delayed.MM),
  "Intellectual disability" = cpt_perc(data_tempo$Déficit.intellectuel),
  "Facial weakness" =cpt_perc(data_tempo$Facial.weakness),
  "Scoliosis" = cpt_perc(data_tempo$Scoliosis),
  #"Stridor" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN =Is_strid_laryn))),#|unlist(lapply(data_tempo$Other.features,FUN = Is_stridor))),
  #"Larynx" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN = Is_laryn)),
  "Bulbar symptoms"= cpt_perc(data_tempo$Bulbar),
  "Contractures" = cpt_perc(Onset_symptom$Retractions),
  "Neonatal hypotonia"= cpt_perc(unlist(lapply(data_tempo$Libre,FUN =Is_hypo)))
)
#write.xlsx(Symptome_init, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "heatmap_1_perc", append = TRUE, row.names = TRUE)

Symptome_init_cpt = data.frame(
  "Proximal weakness" =  cpt_perc2(Limbs_proximal),
  "Distal weakness" = cpt_perc2(Limbs_dist),
  "Axial muscle weakness" =cpt_perc2(data_tempo$Axial),
  "Respiratory symptoms" =cpt_perc2(data_tempo$Respiratory),
  "Sudden childhood respiratory insufficiency" =cpt_perc2(data_tempo$Episodes.de.DRA.dans.l.enfance),
  "Fatigability" = cpt_perc2(data_tempo$Fatigability),
  "Ptosis" = cpt_perc2(data_tempo$Ptosis),
  "Ophtalmoparesis" = cpt_perc2(data_tempo$Oculomotor),
  "Arthrogryposis" =cpt_perc2(data_tempo$arthrogryposis),
  "Delayed motor milestones" =cpt_perc2(data_tempo$Delayed.MM),
  "Intellectual disability" = cpt_perc2(data_tempo$Déficit.intellectuel),
  "Facial weakness" =cpt_perc2(data_tempo$Facial.weakness),
  "Scoliosis" = cpt_perc2(data_tempo$Scoliosis),
  "Bulbar symptoms"= cpt_perc2(data_tempo$Bulbar),
  "Contractures" = cpt_perc2(Onset_symptom$Retractions),
  "Neonatal hypotonia"= cpt_perc2(unlist(lapply(data_tempo$Libre,FUN =Is_hypo)))
)
#-------------------------------colnames(Symptome_init) = gsub(x = colnames(Symptome_init),pattern = "[.]",replacement = " ")
# white border
Symptome_init = Symptome_init[!(rownames(Symptome_init)%in%"Others"),]
Symptome_init_cpt = Symptome_init_cpt[!(rownames(Symptome_init_cpt)%in%"Others"),]
##write.xlsx(Symptome_init_cpt, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "heatmap_1_cpt", append = TRUE, row.names = TRUE)


# Calcul des pvalues
p_values_col <- apply(X = Symptome_init_cpt,MARGIN = 2, FUN = function(x,total) {
  With = x
  Without = total - x
  M = as.table(rbind(With,Without))
  Xsq <- chisq.test(M)
  return(Xsq$p.value)
},total = table(Data_xsl$`Involved gene`)[-c(10)])
p_values_col <- p.adjust(p_values_col,method = "BY")
names(p_values_col) <- colnames(Symptome_init_cpt)

is_sig = p_values_col < 0.01
pch = rep("*", length(is_sig ))
pch[!is_sig] = NA
pvalue_col_fun = colorRamp2(c(0, 2, 3), c("blue", "white", "#B31A15")) 
ha2 = HeatmapAnnotation(
  pvalue = anno_simple(-log10(p_values_col), col = pvalue_col_fun, pch = pch),
  annotation_name_side = "left")

# Bidouillage de dendrogramme qui est crée à la volée 
row_hclust = hclust(dist(as.matrix(Symptome_init)*100,method = "maximum"), method = "ward.D2")
row_dend = as.dendrogram(row_hclust)
# Manipulation dendrogramme pour inverser :
#   "la position des groupes AGRN/MUSK, avec le groupe RAPSN à GFPT1, qui sont au même niveau de branche,
#   mais cela nous arrangerait pour le visuel/narratif au vu de ce que l'on connait de la littérature"
str(row_dend)
# Créer une liste qui contient les feuilles de chaque groupe
group1 <- c("CHRNE", "FCCMS")
group2 <- "CHRND"
group3 <- c("SCCMS")
group4 <- c("MUSK","AGRN")
group5 <- c("RAPSN",  "COLQ","DOK7")
group6 <- c("GFPT1", "GMPPB")
# Combiner tous ces groupes dans un vecteur
new_order <- c(group1, group2, group3, group4, group5,group6)
#new_order
# Utiliser la fonction rotate pour réorganiser les feuilles selon le nouvel ordre
new_dend <- dendextend::rotate(row_dend, order = new_order)
#new_dend = row_dend
rownames(Symptome_init) <-  paste0(rownames(Symptome_init ), "     ")
rownames(Symptome_init_cpt) <-  paste0(rownames(Symptome_init ), "     ")
row.names(Symptome_init)[3] = "CHRNE-LE     "
colnames(Symptome_init) = gsub(x = colnames(Symptome_init),pattern = "[.]",replacement = " ")
colnames(Symptome_init) = gsub(x = colnames(Symptome_init),pattern = "childhood ",replacement = "childhood\n")
Inital_ht <- Heatmap(as.matrix(Symptome_init)*100,cluster_rows = new_dend,  # The Heatmap function takes a matrix as input, here we convert our data frame (mat) to a matrix
                     #top_annotation = ha2, 
                     name = "Proportion of symptoms (%)",  # This parameter provides a name for the heatmap legend
                      col = my_palette,  # Defines the color scheme of the heatmap using the 'my_palette' object we created earlier. It'll map the scores in our data to the colors in this palette
                      left_annotation = row_ha,  # This parameter is used to add annotations (metadata) to the rows of the heatmap. The annotation object 'row_ha' that we created earlier is passed here
                      show_row_names = TRUE,  # This parameter controls whether the row names of the matrix (which correspond to the muscle names) should be shown on the heatmap
                      show_column_names = TRUE,  # This parameter controls whether the column names of the matrix (which correspond to the patients) should be shown on the heatmap
                      row_title = "Gene",  # This parameter is used to specify a title for the row names of the heatmap
                      # column_title = "",  # This parameter is used to specify a title for the column names of the heatmap. It's left empty here
                      #cluster_rows = TRUE,  # This parameter controls whether to hierarchically cluster the rows (muscles), i.e., group together muscles that have similar data patterns across patients
                      clustering_distance_rows = "maximum",
                      clustering_distance_columns = "manhattan", 
                      clustering_method_columns ="ward.D2",
                      clustering_method_rows  ="ward.D2",
                      column_title = "Initial phenotype",column_title_side = "bottom",column_title_gp = gpar(fontsize = 20, fontface = "bold"),
                      cluster_columns = TRUE,  # This parameter controls whether to hierarchically cluster the columns (patients), i.e., group together patients that have similar muscle score patterns
                      row_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 12),  # This parameter allows us to specify graphical parameters for the row names. Here, the font family is set to "serif", the font style is "italic", and the font size is 10
                      column_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 12),
                      cell_fun = cell_border_fun,  # This parameter allows us to specify graphical parameters for the column names. Similar to the row names, the font family is "serif", the font style is "italic", and the font size is 10
                      heatmap_legend_param = list(
                        border = "black", # Bordure fine autour de la légende
                        title_gp = gpar(fontsize = 12, fontface = "bold"), # Police du titre
                        labels_gp = gpar(fontsize = 12), # Police des étiquettes
                        legend_width = unit(1, "cm") # Décalage de la légende
                      ))
lgd_pvalue = Legend(title = "P-values [initial Phenotype]", col_fun = pvalue_col_fun, at = c(0, 1, 2, 3), 
                    labels = c("1", "0.1", "0.01", "0.001"),
                    border = "black", # Bordure fine autour de la légende
                    title_gp = gpar(fontsize = 12, fontface = "bold"), # Police du titre
                    labels_gp = gpar(fontsize = 12), # Police des étiquettes
                    legend_width = unit(1, "cm")) # Décalage de la légende)
lgd_sig = Legend(pch = "*", type = "points", labels = "< 0.01")


list_ht[[1]] = Inital_ht
# enregistrement heatmap
tempo = column_order(list_ht[[1]])
# enregistrement de l'ordre des colonnes
tempo3 = colnames(Symptome_init)
tempo3 = gsub(x = tempo3,pattern = " ",replacement = ".")
tempo3  = gsub(x = tempo3,pattern = "childhood\n",replacement = "childhood ")
# enregistrement de l'ordre des ligne
tempo2 = row_order(list_ht[[1]])
# enregistrement du jeu de données
S1 =Symptome_init

#===============================================================================
# Seconde Heatmap ( last visite)
#===============================================================================
data_tempo = severi_last_visit
i = i+1 


unique(data_tempo$If.yes..prox..or.dist.)
Limbs_proximal = grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = "^Prox")
Limbs_dist = grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = "Dist")
Limbs_dist[grepl(x = data_tempo$If.yes..prox..or.dist.,pattern = ">Dist")]  = NA
Limbs = data_tempo$Limbs
Limbs[Limbs=="NA"] = NA
Limbs = ifelse(Limbs=="Oui",TRUE,FALSE)

Symptome_init = data.frame(
  "Proximal weakness" =  cpt_perc(Limbs_proximal),
  "Distal weakness" = cpt_perc(Limbs_dist),
  "Axial muscle weakness" =cpt_perc(data_tempo$Axial),
  "Respiratory symptoms" =cpt_perc(data_tempo$Respiratory),
  "Fatigability" = cpt_perc(data_tempo$Fatigability),
  "Ptosis" = cpt_perc(data_tempo$Ptosis),
  "Ophtalmoparesis" = cpt_perc(data_tempo$Oculomotor),
  "Facial weakness" =cpt_perc(data_tempo$Facial.weakness),
  "Bulbar symptoms"= cpt_perc(data_tempo$Bulbar)#,
 # "Hypotonia"= cpt_perc(unlist(lapply(data_tempo$Libre,FUN =Is_hypo)))
)
#write.xlsx(Symptome_init, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "heatmap_2_perc", append = TRUE, row.names = TRUE)

# white border
Symptome_init = Symptome_init[!(rownames(Symptome_init)%in%"Others"),]
Symptome_init_cpt = Symptome_init_cpt[!(rownames(Symptome_init_cpt)%in%"Others"),]
#===============================================================================
# Calcul PVAL
#===============================================================================
Symptome_init_cpt2 = data.frame(
  "Proximal weakness" =  cpt_perc2(Limbs_proximal),
  "Distal weakness" = cpt_perc2(Limbs_dist),
  "Axial muscle weakness" =cpt_perc2(data_tempo$Axial),
  "Respiratory symptoms" =cpt_perc2(data_tempo$Respiratory),
  "Fatigability" = cpt_perc2(data_tempo$Fatigability),
  "Ptosis" = cpt_perc2(data_tempo$Ptosis),
  "Ophtalmoparesis" = cpt_perc2(data_tempo$Oculomotor),
  "Facial weakness" =cpt_perc2(data_tempo$Facial.weakness),
  "Bulbar symptoms"= cpt_perc2(data_tempo$Bulbar)#,
  #"Hypotonia"= cpt_perc2(unlist(lapply(data_tempo$Libre,FUN =Is_hypo)))
)
Symptome_init_cpt2 = Symptome_init_cpt2[!(rownames(Symptome_init_cpt2)%in%"Others"),]
rownames(Symptome_init_cpt2) <-  paste0(rownames(Symptome_init_cpt2), "     ")
Symptome_init_cpt = Symptome_init_cpt[,colnames(Symptome_init_cpt)%in%colnames(Symptome_init_cpt2)]
tempo_a = Symptome_init_cpt
tempo_a[,] = 1
for(j in c(1:ncol(tempo_a))){
  for(k in c(1:nrow(tempo_a))){
    tempo_a[k,j] = prop.test(x = c(Symptome_init_cpt[k,j],Symptome_init_cpt2[k,j]),n =  rep(table(Data_xsl$`Involved gene`)[-c(10)][k],2))$p.value
  }
}
tempo_a[is.na(tempo2)] = 1
#write.xlsx(Symptome_init_cpt2, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "heatmap_2_cpt", append = TRUE, row.names = TRUE)


#===============================================================================
# Seconde Heatmap ( last visite)
#===============================================================================
colnames(Symptome_init) = gsub(x = colnames(Symptome_init),pattern = "[.]",replacement = " ")
#tempo = match(tempo3[tempo][tempo3[tempo]%in%colnames(Symptome_init)],colnames(Symptome_init))
tempo = match(tempo3[tempo][tempo3[tempo]%in%gsub(x = colnames(Symptome_init),pattern = " ",".")],gsub(x = colnames(Symptome_init),pattern = " ","."))
  
  

Symptome_init = Symptome_init[!(rownames(Symptome_init)%in%"Others"),]
total_data <- as.matrix(table(Data_xsl$`Involved gene`)[-c(10)])
colnames(total_data) <- "Total"
ha = rowAnnotation("Cohort Size" = anno_barplot(total_data, 
                                        add_numbers = TRUE, 
                                        gp = gpar(col = "blue")), annotation_name_rot = 90)
                   
                   
rownames(Symptome_init) <-  paste0(rownames(Symptome_init ), "     ")
row.names(Symptome_init)[3] = "CHRNE-LE     "
list_ht[i] <- Heatmap(as.matrix(Symptome_init)*100,  # The Heatmap function takes a matrix as input, here we convert our data frame (mat) to a matrix
                      #right_annotation = ha,
                      name = "Proportion of symptoms (%)",  # This parameter provides a name for the heatmap legend
                      col = my_palette,  # Defines the color scheme of the heatmap using the 'my_palette' object we created earlier. It'll map the scores in our data to the colors in this palette
                      #left_annotation = row_ha,  # This parameter is used to add annotations (metadata) to the rows of the heatmap. The annotation object 'row_ha' that we created earlier is passed here
                      show_row_names = TRUE,  # This parameter controls whether the row names of the matrix (which correspond to the muscle names) should be shown on the heatmap
                      show_column_names = TRUE,  # This parameter controls whether the column names of the matrix (which correspond to the patients) should be shown on the heatmap
                      row_title = "Gene",  # This parameter is used to specify a title for the row names of the heatmap
                      #column_title = "Last Visit",  # This parameter is used to specify a title for the column names of the heatmap. It's left empty here
                      cluster_rows = FALSE,  # This parameter controls whether to hierarchically cluster the rows (muscles), i.e., group together muscles that have similar data patterns across patients
                      clustering_distance_rows = "euclidean",
                      clustering_distance_columns = "manhattan", 
                      clustering_method_columns ="ward.D2",
                      clustering_method_rows  ="ward.D2",column_title_side = "bottom",
                      column_title = "Phenotype at last visit",column_order = tempo,
                      column_title_gp = gpar(fontsize = 20, fontface = "bold"),row_order = tempo2,
                      cluster_columns = FALSE,  # This parameter controls whether to hierarchically cluster the columns (patients), i.e., group together patients that have similar muscle score patterns
                      row_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 12),  # This parameter allows us to specify graphical parameters for the row names. Here, the font family is set to "serif", the font style is "italic", and the font size is 10
                      column_names_gp = gpar(fontfamily = "serif", fontface = "italic", fontsize = 12),
                      cell_fun = cell_border_fun,  # This parameter allows us to specify graphical parameters for the column names. Similar to the row names, the font family is "serif", the font style is "italic", and the font size is 10
                      heatmap_legend_param = list(
                        border = "black", # Bordure fine autour de la légende
                        title_gp = gpar(fontsize = 12, fontface = "bold"), # Police du titre
                        labels_gp = gpar(fontsize = 12), # Police des étiquettes
                        legend_width = unit(1, "cm") # Décalage de la légende
                      ))


#}
#data_tempo = data_tempo_list[[1]]


ht_opt(legend_border = "black",annotation_border = TRUE)
#-----------------------------------------------------------------------------
ht_list =   list_ht[[1]] +  list_ht[[2]]#+list_ht[[3]]  
tiff(filename = "heatmap_final3.tiff",units = "cm",width = 50,height = 20,pointsize = 0.001,res = 300)
draw(ht_list, ht_gap = unit(c(1,2), "cm") ,merge_legend = TRUE, column_title_gp = gpar(fontsize = 11))#,annotation_legend_list = list(lgd_pvalue,lgd_sig))
dev.off()
#==============================================================================================================================================================
# Autres graphiques
#==============================================================================================================================================================
# elevation des CK

cpt_perc(EMG_Biopsie$R.CMAP)
cpt_perc(EMG_Biopsie$CK.élevée)
EMG_Biopsie$Décrément[grep(pattern = "^NA",x = EMG_Biopsie$Décrément)] = NA
cpt_perc(EMG_Biopsie$Décrément)
cpt_perc(Treatments_eff$Ephedrine.2)

EMG = data.frame(
  RCMAP = cpt_perc(EMG_Biopsie$R.CMAP),
  CK = cpt_perc(EMG_Biopsie$CK.élevée),
  Decrement = cpt_perc(EMG_Biopsie$Décrément),
  Ephredine = cpt_perc(EMG_Biopsie$Ephedrine.5),
  Sabumatol = cpt_perc(EMG_Biopsie$Salbutamol.5)
)
#write.xlsx(EMG, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "EMG_perc", append = TRUE, row.names = TRUE)

EMG = data.frame(
  RCMAP = cpt_perc2(EMG_Biopsie$R.CMAP),
  CK = cpt_perc2(EMG_Biopsie$CK.élevée),
  Decrement = cpt_perc2(EMG_Biopsie$CK.élevée),
  Ephredine = cpt_perc2(EMG_Biopsie$Ephedrine.5),
  Sabumatol = cpt_perc2(EMG_Biopsie$Salbutamol.5)
)
#write.xlsx(final_table, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "EMG_cpt", append = TRUE, row.names = TRUE)
EMG = data.frame(
  gene = Data_xsl$`Involved gene`,
  RCMAP = EMG_Biopsie$R.CMAP,
  CK = EMG_Biopsie$CK.élevée
  #Decrement = EMG_Biopsie$Décrément
  )
EMG[EMG=="Oui"] = TRUE
EMG[EMG=="Non"] = FALSE

df_count <- EMG %>%
  gather(key = "variable", value = "value", RCMAP, CK) %>%
  group_by(gene, variable, value) %>%
  summarise(count = n()) %>%
  filter(!is.na(value))

p_values <- EMG %>%
  gather(key = "variable", value = "value", RCMAP, CK) %>%
  filter(!is.na(value)) %>%
  group_by(variable) %>%
  do(chi2 = chisq.test(.$value, .$gene)) %>%
  summarise(variable, p.value = chi2$p.value)




#===============================================================================
# graph 1
#===============================================================================
# Create the ggplot graph
ggplot(df_count, aes(x = gene, y = count, fill = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  
  # Add titles and axis labels
  ggtitle("Distribution of Values by Gene and Variable") +
  xlab("Gene") +
  ylab("Number of Occurrences") +
  
  # Use a more pleasant color palette
  scale_fill_brewer(palette = "Set1") +
  
  # Facet wrap with free scales for the y-axis
  facet_wrap(~variable, scales = "free_y") +
  
  # Add legends
  labs(fill = "Value") +
  
  # Use a theme to improve the overall appearance
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#===============================================================================
# graph2
#===============================================================================
df_normalized <- df_count %>%
  group_by(gene, variable) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup()

# Create stacked bar chart displaying proportions
ggplot(df_normalized, aes(x = gene, y = proportion, fill = value)) +
  geom_bar(stat = "identity") +
  
  # Add titles and axis labels
  ggtitle("Proportional Distribution of Values by Gene and Variable") +
  xlab("Gene") +
  ylab("Proportion of Occurrences") +
  
  # Use a more pleasant color palette
  scale_fill_brewer(palette = "Set1") +
  
  # Facet wrap with free scales for the y-axis
  facet_wrap(~variable, scales = "free_y") +
  
  # Add legends
  labs(fill = "Value") +
  
  # Use a theme to improve the overall appearance
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#===============================================================================
# Medicament
#===============================================================================

Treatments_eff = data[,107:112]
colnames(Treatments_eff) = c("Acetylcholinesterase_Inhibitor","3.4-diaminopyridine","Fluoxetine","Quinidine","Ephedrine" ,"Salbutamol")

Treatments_eff[Treatments_eff == "Non"] = "No effect"
Treatments_eff[Treatments_eff == "Oui"] = "Improvement"
Treatments_eff[Treatments_eff == "En cours"] = "Ongoing"
Treatments_eff[Treatments_eff == "Aggra"] = "Worsening"
mat = Treatments_eff
# Identifier et remplacer les cellules contenant des chiffres ou des nombres
mat_new <- apply(mat, c(1, 2), function(x) {
  if (grepl("\\d", x)) {  # Utiliser l'expression régulière "\\d" pour identifier les chiffres
    return("Improvement")
  } else {
    return(x)
  }
})

# Convertir le résultat en matrice (car apply retourne un tableau)
mat_new <- matrix(mat_new, nrow = dim(mat)[1], ncol = dim(mat)[2])
colnames(mat_new) = gsub(x = gsub(x = colnames(Treatments_eff),pattern = "\\d",replacement = ""),pattern = "[.]",replacement = "")
mat_new = data.frame(mat_new)
mat_new$gene = Data_xsl$`Involved gene`
colnames(mat_new) = c("AChE Inhibitors","3.4-diaminopyridine","Fluoxetine","Quinidine","Ephedrine" ,"Salbutamol","gene")
mat_new = mat_new[,c(1,2,6,5,3,4,7)]
mat_new = mat_new[mat_new$gene!="Others",]


library(tidyr)
long_data <- gather(mat_new, key = "Treatment", value = "Outcome", -gene)

library(dplyr)
long_data = long_data[long_data$Outcome!="NA",]





proportion_data <- long_data %>%
  group_by(gene, Treatment, Outcome) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()
# graphique
#proportion_data = proportion_data[proportion_data$Outcome]
# Définir une palette de couleurs plus neutre
neutral_palette <- c("Improvement" = "green3", "No Effect" = "grey90", "Ongoing" =  "grey90", "Worsening"  = "red")

proportion_data$gene[proportion_data$gene=="CHRNE"] = "CHRNE-LE"

# Créer le graphique
ggplot(proportion_data, aes(x = "", y = prop, fill = Outcome)) +
  
  # Barres de graphique circulaire
  geom_bar(stat = "identity", width = 1) +
  
  # Utiliser des coordonnées polaires
  coord_polar("y") +
  
  # Grille pour les traitements et les gènes (inverse des axes)
  facet_grid(Treatment ~ gene) +
  
  # Titres et légendes
  labs(
    title = "Proportion of Treatment Outcomes by Gene and Treatment",
    x = NULL,
    y = NULL,
    fill = "Outcome"
  ) +
  
  # Palette de couleurs plus neutre
  scale_fill_manual(values = neutral_palette) +
  
  # Thème esthétique
  theme_void() +
  
  # Personnalisation du thème
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  )
#===============================================================================
proportion_data$Treatment <- factor(proportion_data$Treatment, levels = c("AChE Inhibitors","3.4-diaminopyridine","Salbutamol","Ephedrine" ,"Fluoxetine","Quinidine"))
proportion_data$gene <- factor(proportion_data$gene, levels = c("CHRNE-LE", "FCCMS", "CHRND","SCCMS","MUSK","AGRN","RAPSN","COLQ","DOK7","GFPT1","GMPPB"))
proportion_data$gene[is.na(proportion_data$gene)] = "CHRNE-LE"
#===============================================================================
if (!requireNamespace("ggpattern", quietly = TRUE)) {
  install.packages("ggpattern")
}
library(ggpattern)

custom_labeller <- function(variable, value) {
  return(as.character(value))
}
neutral_palette <- c("Improvement" = "#0080ff", "No effect" = "grey90", "Ongoing" =  "grey90", "Worsening"  = "#ff00ff")
tiff(filename = "20241004_graph3.tiff",units = "cm",width = 50,height = 20,pointsize = 0.001,res = 600)
ggplot(proportion_data, aes(x = "", y = prop, fill = Outcome)) +
  
  # Barres de graphique circulaire
    geom_bar(stat = "identity", width = 1, color = "black") +  # Bordures noires ajoutées
    #geom_bar_pattern(stat = "identity", width = 1, color = "black", 
    #               pattern = ifelse(proportion_data$Outcome == "Worsening", "stripe", "none"),
    #               pattern_fill = "red", pattern_density = 0.1, pattern_spacing = 0.02,
    #               pattern_angle = 45) +  # Angle de hachurage
  geom_bar_pattern(
    aes(pattern = ifelse(Outcome == "Worsening", "", "Stripe")),
    stat = "identity", width = 1, color = "black", 
    pattern_fill = "#ff00ff", pattern_density = 0.1, pattern_spacing = 0.02,
    pattern_angle = 45
  )+
  # Utiliser des coordonnées polaires
  coord_polar("y") +
  
  # Grille pour les traitements et les gènes (inverse des axes)
  facet_grid(Treatment ~ gene, 
             switch = 'y',  # Ajoute des étiquettes à l'intérieur de la grille
             #labeller = label_both  # Utilise les noms des deux variables pour les étiquettes
             labeller = custom_labeller
  ) +
  
  # Suppression du titre
  labs(
    title = NULL,  # Enlever le titre
    x = NULL,
    y = NULL,
    fill = "Outcome"
  ) +
  
  # Palette de couleurs plus neutre

  scale_fill_manual(values = neutral_palette) +
  
  # Thème esthétique
  theme_void() +
  
  # Personnalisation du thème
  theme(
    strip.text.x = element_text(face = "bold.italic", size = 14, angle = 0),  # Titre des colonnes
    strip.text.y = element_text(face = "bold", size = 14, angle = 0),  # Titre des lignes
    strip.background = element_rect(fill = "white", color = "grey10", size = 1),  # Fond des titres des facettes
    legend.title = element_text(face = "bold", size = 12),  # Titre de la légende
    legend.text = element_text(size = 10),  # Texte de la légende
    #panel.grid.major = element_line(color = "grey50", size = 0.2)  # Lignes majeures de la grille
    panel.grid.minor = element_line(color = "grey90", size = 0.1)  # Lignes mineures de la grille
  )
dev.off()
tiff(filename = "20241004_graph3.tiff",units = "cm",width = 50,height = 20,pointsize = 10,res = 600)
ggplot(proportion_data, aes(x = "", y = prop, fill = Outcome)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  #geom_col_pattern(
  #  aes(fill = Outcome, pattern = Outcome),
  #  pattern_density = 0.1, # Densité des motifs ajustée pour plus de discrétion
  #  pattern_spacing = 0.05, # Espacement des motifs ajusté pour plus de discrétion
  #  color = "black"
 # ) +
  coord_polar("y") +
  facet_grid(Treatment ~ gene, switch = 'y', labeller = custom_labeller) +
  labs(title = NULL, x = NULL, y = NULL, fill = "Outcome") +
  scale_fill_manual(values = neutral_palette) +
  scale_pattern_manual(values = c("Improvement" = "none", "No effect" = "none", "Ongoing" = "none", "Worsening" = "stripe")) +
  theme_void() +
  theme(
    strip.text.x = element_text(face = "bold.italic", size = 14, angle = 0),
    strip.text.y = element_text(face = "bold", size = 14, angle = 0),
    strip.background = element_rect(fill = "white", color = "grey10", size = 1),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    #legend.position = "bottom",
    # add stride in the legend
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(1, "cm"),
    # modifie strip density in the legend
    # modifie color of the legend
    #legend.key = element_rect(fill = "white", color = "grey10", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.1),
    #  # pattern_density = 0.5 pour la legende
  )
dev.off()










d
tiff(filename = "20241004_graph3.tiff", units = "cm", width = 50, height = 20, pointsize = 10, res = 600)
ggplot(proportion_data, aes(x = "", y = prop, fill = Outcome)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  geom_col_pattern(
    aes(fill = Outcome, pattern = Outcome),
    pattern_density = 0.1,  # Densité des motifs ajustée pour plus de discrétion
    pattern_spacing = 0.05,  # Espacement des motifs ajusté pour plus de discrétion
    color = "black"
  ) +
  coord_polar("y") +
  facet_grid(Treatment ~ gene, switch = 'y', labeller = custom_labeller) +
  labs(title = NULL, x = NULL, y = NULL, fill = "Outcome") +
  scale_fill_manual(values = neutral_palette, guide = FALSE) +  # Désactiver la légende automatique
  scale_pattern_manual(values = c("Improvement" = "none", "No effect" = "none", "Ongoing" = "none", "Worsening" = "stripe"), guide = FALSE) +  # Désactiver la légende automatique
  theme_void() +
  theme(
    strip.text.x = element_text(face = "bold.italic", size = 14, angle = 0),
    strip.text.y = element_text(face = "bold", size = 14, angle = 0),
    strip.background = element_rect(fill = "white", color = "grey10", size = 1),
    panel.grid.minor = element_line(color = "grey90", size = 0.1)
   

  ) +
  # Ajouter les éléments de la légende manuellement
  annotate("rect", xmin = Inf, xmax = Inf, ymin = Inf, ymax = Inf, fill = "color1") +  # Remplacer 'Inf' par des positions spécifiques
  annotate("text", x = Inf, y = Inf, label = "Improvement", hjust = 0) +  # Ajustez 'Inf' et le texte
  # Répétez pour chaque élément de légende nécessaire
dev.off()
#==============================================================================
# osserman
tempo = data.frame(
gene = Data_xsl$`Involved gene`,
y = as.numeric(severi_last_visit$Osserman.2)
)
# Suppression des données manquantes (NA)
tempo_clean <- tempo[!is.na(tempo$y),]

# Effectuer une ANOVA pour examiner si le score Osserman dépend du type génétique
anova_result <- aov(y ~ gene, data = tempo_clean)

# Afficher les résultats de l'ANOVA
summary(anova_result)
#==============================================================================
# osserman
tempo = data.frame(
  gene = Data_xsl$`Involved gene`,
  y = as.numeric(substr(x = severi_last_visit$MGFA.2,1,1))
)
# Suppression des données manquantes (NA)
tempo_clean <- tempo[!is.na(tempo$y),]

# Effectuer une ANOVA pour examiner si le score Osserman dépend du type génétique
anova_result <- aov(y ~ gene, data = tempo_clean)

# Afficher les résultats de l'ANOVA
summary(anova_result)

#===============================================================================
evolution$age.at.ventilation = ifelse(evolution$age.at.ventilation=="Non","NO","YES")
EMG = data.frame(
  A = cpt_perc2(EMG_Biopsie$R.CMAP),
  B = cpt_perc2(EMG_Biopsie$CK.élevée),
  C = cpt_perc2(severi_last_visit$Wheelchair),
  D = cpt_perc2(evolution$age.at.ventilation),
  E = cpt_perc2(evolution$Passage.en.réa.SI.à.un.moment)
)
#write.xlsx(final_table, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output.xlsx", sheetName = "EMG_cpt", append = TRUE, row.names = TRUE)
EMG = data.frame(
  gene = Data_xsl$`Involved gene`,
  RCMAP = v,
  CK = EMG_Biopsie$CK.élevée
  #Decrement = EMG_Biopsie$Décrément
)
EMG[EMG=="Oui"] = TRUE
EMG[EMG=="Non"] = FALSE

df_count <- EMG %>%
  gather(key = "variable", value = "value", RCMAP, CK) %>%
  group_by(gene, variable, value) %>%
  summarise(count = n()) %>%
  filter(!is.na(value))

p_values <- EMG %>%
  gather(key = "variable", value = "value", A,B,C,D,E) %>%
  filter(!is.na(value)) %>%
  group_by(variable) %>%
  do(chi2 = chisq.test(.$value, .$gene)) %>%
  summarise(variable, p.value = chi2$p.value)

cpt_perc3 = function(x,y=Data_xsl$`Involved gene`){
  x[x=="NA"] = NA
  x[x == "Improvement"] = TRUE
  x[x=="Improvement"] = FALSE
  return(table(y,x)[,"TRUE"])
}
Data_xsl$`Involved gene`

#===============================================================================
# Création de la table de contingence
# cpt_perc2(severi_last_visit$Wheelchair) est le nombre de patients en fauteuil roulant pour chaque groupe génétique
# Data_xsl$`Involved gene` est la liste des groupes génétiques des patients

# Supposons que cpt_perc2(severi_last_visit$Wheelchair) soit stocké dans une variable appelée 'wheelchair_count'
wheelchair_count <- cpt_perc2(severi_last_visit$Wheelchair)

# Supposons que Data_xsl$`Involved gene` soit stocké dans une variable appelée 'involved_gene'
involved_gene <- Data_xsl$`Involved gene`

# Création du tableau de contingence
contingency_table <- as.table(as.matrix(data.frame(wheelchair_count)))

# Réalisation du test du chi-carré
chi2_test <- chisq.test(contingency_table)

# Affichage des résultats
print(chi2_test)

# Analyse des résidus pour identifier les groupes qui diffèrent significativement
residuals <- as.data.frame(as.table(chi2_test$residuals))
colnames(residuals) <- c("Involved_Gene", "Residual")
print(residuals)

cpt_perc2 = function(x,y=Data_xsl$`Involved gene`){
  x[x=="NA"] = NA
  x[x == "Oui"] = TRUE
  x[x=="Non"] = FALSE
  x[x == "YES"] = TRUE
  x[x=="NO"] = FALSE
  x[x == "Yes"] = TRUE
  x[x=="No"] = FALSE
  return(table(y,x)[,"TRUE"])
}

















# rownames(df) <- c("Gene1", "Gene2", "Gene3")
df = Symptome_init_cpt


# Calcul de la moyenne et de l'écart-type pour chaque symptôme
mean_values <- colMeans(df, na.rm = TRUE)
sd_values <- apply(df, 2, sd, na.rm = TRUE)

# Identification des gènes avec des valeurs > moyenne + 2*EC pour chaque symptôme
# Identification des gènes avec des valeurs > moyenne + 2*EC pour chaque symptôme
outliers <- lapply(1:ncol(df), function(i) {
  symptom <- colnames(df)[i]
  threshold <- mean_values[i] + 2 * sd_values[i]
  outlier_genes <- rownames(df)[df[,i] > threshold]
  
  # Vérification si outlier_genes est vide
  if(length(outlier_genes) == 0) {
    return(data.frame(Symptom = symptom, Outlier_Genes = NA))
  } else {
    return(data.frame(Symptom = symptom, Outlier_Genes = outlier_genes))
  }
})

# Conversion de la liste en un dataframe pour une meilleure lisibilité
outliers_df <- do.call(rbind, outliers)

# Affichage des résultats
print(outliers_df)


# Conversion de la liste en un dataframe pour une meilleure lisibilité
outliers_df <- do.call(rbind, outliers)

# Affichage des résultats
print(outliers_df)



#===============================================================================
# final

chi2_test_gene <- function(total_gene, specific_count, target_gene) {
  
  # Calcul des proportions pour le gène cible et pour les autres gènes
  target_wheelchair <- specific_count[target_gene]
  target_total <- total_gene[target_gene]
  other_wheelchair <- sum(specific_count) - target_wheelchair
  other_total <- sum(total_gene) - target_total
  
  # Tableau de contingence
  contingency_table <- matrix(c(target_wheelchair, target_total - target_wheelchair, other_wheelchair, other_total - other_wheelchair), nrow=2)
  rownames(contingency_table) <- c("SpecificState", "NoSpecificState")
  colnames(contingency_table) <- c(target_gene, "Others")
  
  # Test du chi-carré
  chi2_test_result <- chisq.test(contingency_table)
  
  return(chi2_test_result$p.value)
}
chi2_test_gene(total_gene, wheelchair_count, "GFPT1")
chi2_test_gene(total_gene, wheelchair_count, "DOK7")
chi2_test_gene(total_gene, wheelchair_count, "GMPPB")


chi2_test_gene(total_gene, cpt_perc2(evolution$Passage.en.réa.SI.à.un.moment), "RAPSN")
chi2_test_gene(total_gene, cpt_perc2(evolution$Passage.en.réa.SI.à.un.moment), "MUSK")
chi2_test_gene(total_gene, cpt_perc2(evolution$Passage.en.réa.SI.à.un.moment), "DOK7")
chi2_test_gene(total_gene, cpt_perc2(evolution$Passage.en.réa.SI.à.un.moment), "AGRN")



evolution$age.at.ventilation[!(is.na(evolution$age.at.ventilation))&(evolution$age.at.ventilation!="Non")] = "YES"
evolution$age.at.ventilation[!(is.na(evolution$age.at.ventilation))&(evolution$age.at.ventilation!="YES")] = "NO"

chi2_test_gene(total_gene, cpt_perc2(evolution$age.at.ventilation), "DOK7")
chi2_test_gene(total_gene, cpt_perc2(evolution$age.at.ventilation), "SCCMS")


chi2_test_gene(total_gene, cpt_perc2(evolution$age.at.ventilation), "DOK7")
chi2_test_gene(total_gene, cpt_perc2(evolution$age.at.ventilation), "SCCMS")


cpt_perc2(EMG_Biopsie$R.CMAP)

chi2_test_gene(total_gene,cpt_perc2(EMG_Biopsie$R.CMAP), "COLQ")
chi2_test_gene(total_gene,cpt_perc2(EMG_Biopsie$R.CMAP), "SCCMS")


cpt_perc2(EMG_Biopsie$CK.élevée)


chi2_test_gene(total_gene,cpt_perc2(EMG_Biopsie$CK.élevée), "GMPPB")
chi2_test_gene(total_gene,cpt_perc2(EMG_Biopsie$R.CMAP), "SCCMS")


Course[Course%in%c("Improvement","Stable","Worsening")] = "NO"
Course[Course!="NO"] = "YES"

Course = data$Course
Course[Course%in%c("Worsening, stable","Worsening, Stable")] = "Worsening => stable"
Course[Course%in%c("Improvement","Stable","Worsening")] = "NO"
Course[Course!="NO"] = "YES"
chi2_test_gene(total_gene, cpt_perc2(Course), "COLQ")
chi2_test_gene(total_gene, cpt_perc2(Course), "SCCMS")


Course[Course%in%c("Improvement")] = "YES"
Course[Course!="YES"] = "NO"
chi2_test_gene(total_gene, cpt_perc2(Course), "RAPSN")
chi2_test_gene(total_gene, cpt_perc2(Course), "MUSK")


# Supposons que cpt_perc2(severi_last_visit$Wheelchair) soit stocké dans une variable appelée 'wheelchair_count'
wheelchair_count <- cpt_perc2(severi_last_visit$Wheelchair)
total_gene = table(Data_xsl$`Involved gene`)
total_gene =total_gene[names(total_gene)%in%rownames(tempo_perc)]


chi2_test_gene(total_gene, wheelchair_count, "DOK7")


cpt_perc2(EMG_Biopsie$CK.élevée)



by(data = Data_demographique$age.first.visit..dispo.,INDICES = Data_xsl$`Involved gene`,mean)
resultat.anova <- aov(Data_demographique$age.first.visit..dispo. ~ Data_xsl$`Involved gene`)
summary(resultat.anova)
p.adjust(p = 0.0074,n = 15,method = "BH")


by(data = Data_demographique$Age.last.visit,INDICES = Data_xsl$`Involved gene`,mean)
resultat.anova <- aov(Data_demographique$Age.last.visit ~ Data_xsl$`Involved gene`)
summary(resultat.anova)
p.adjust(p = 0.11,n = 15,method = "BH")

by(data = Data_demographique$Follow.up.since.first.symp..y.,INDICES = Data_xsl$`Involved gene`,mean)
resultat.anova <- aov(Data_demographique$Follow.up.since.first.symp..y. ~ Data_xsl$`Involved gene`)
summary(resultat.anova)
p.adjust(p = 0.0074,n = 15,method = "BH")


