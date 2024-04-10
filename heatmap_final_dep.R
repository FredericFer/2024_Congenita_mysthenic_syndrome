#===========================================================================================
# V1
library(dplyr)
library(reshape2)
library(ComplexHeatmap)  # For creating heatmaps
library(colorRamp2)  # For creating color gradients
library(ggplot2)  # For general plotting
library(RColorBrewer)  # For color palettes
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
Is_hypo = function(x){
  return(grepl(x = x,pattern = "[Hh]ypo"))
}
#===============================================================================
# Metadata
#===============================================================================
metadata = data.frame(
  "Mean age at first symptoms" = final_table$`Age at First Symptome_mean`,
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

AFS_color =colorRamp2(range(metadata$Mean.age.at.first.symptoms),colors = c("white","#B31A15"))
ALS_color =colorRamp2(range(metadata$Mean.age.at.last.visit),colors =  c("white","#4F7ABB"))
diag_color =colorRamp2(range(metadata$Mean.diagnostic.wandering.time),colors =  c("white","#FA9203"))
FMH_color =colorRamp2(range(metadata$Proportion.of.Family.history),colors =  c("white","#6D0E4E"))


metadata = metadata[rownames(metadata)!="Others",]
colnames(metadata) = gsub(x = colnames(metadata),pattern = "[.]",replacement = " ")
metadata2 = metadata$`Mean age at first symptoms`
names(metadata2) = rownames(metadata)
metadata = data.frame("Mean age at first symptoms" = metadata2)
colnames(metadata) = "Mean age at first symptoms"
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
  #"Limb weakness" =  cpt_perc(data_tempo$Limbs),
  "Proximal weakness" =  cpt_perc(Limbs_proximal),
  "Distal weakness" = cpt_perc(Limbs_dist),
  "Axial muscle weakness" =cpt_perc(data_tempo$Axial),
  "Respiratory involvement" =cpt_perc(data_tempo$Respiratory),
  #"Episodic Apnea" =cpt_perc(data_tempo$Apnée.épisodique),
  "Sudden childhood respiratory insuffisiency" =cpt_perc(data_tempo$Episodes.de.DRA.dans.l.enfance),
  "Fatigability" = cpt_perc(data_tempo$Fatigability),
  "Ptosis" = cpt_perc(data_tempo$Ptosis),
  "Ophtalmoparesis" = cpt_perc(data_tempo$Oculomotor),
  "Arthrogryposis" =cpt_perc(data_tempo$arthrogryposis),
  "Delayed Motor Milestone" =cpt_perc(data_tempo$Delayed.MM),
  "Intellectual Disability" = cpt_perc(data_tempo$Déficit.intellectuel),
  "Facial weakness" =cpt_perc(data_tempo$Facial.weakness),
  "Scoliosis" = cpt_perc(data_tempo$Scoliosis),
  #"Stridor" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN =Is_strid_laryn))),#|unlist(lapply(data_tempo$Other.features,FUN = Is_stridor))),
  #"Larynx" = cpt_perc(unlist(lapply(data_tempo$Libre,FUN = Is_laryn)),
  "Bulbar symptoms"= cpt_perc(data_tempo$Bulbar),
  "Hypotonia"= cpt_perc(unlist(lapply(data_tempo$Libre,FUN =Is_hypo)))
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
                      name = "Proportion of Symptoms (%)",  # This parameter provides a name for the heatmap legend
                      col = my_palette,  # Defines the color scheme of the heatmap using the 'my_palette' object we created earlier. It'll map the scores in our data to the colors in this palette
                      left_annotation = row_ha,  # This parameter is used to add annotations (metadata) to the rows of the heatmap. The annotation object 'row_ha' that we created earlier is passed here
                      show_row_names = TRUE,  # This parameter controls whether the row names of the matrix (which correspond to the muscle names) should be shown on the heatmap
                      show_column_names = TRUE,  # This parameter controls whether the column names of the matrix (which correspond to the patients) should be shown on the heatmap
                      row_title = "Gene",  # This parameter is used to specify a title for the row names of the heatmap
                      # column_title = "",  # This parameter is used to specify a title for the column names of the heatmap. It's left empty here
                      cluster_rows = TRUE,  # This parameter controls whether to hierarchically cluster the rows (muscles), i.e., group together muscles that have similar data patterns across patients
                      clustering_distance_rows = "maximum",
                      clustering_distance_columns = "manhattan", 
                      clustering_method_columns ="ward.D2",
                      clustering_method_rows  ="ward.D2",
                      column_title = "Initial phenotype",column_title_side = "bottom",column_title_gp = gpar(fontsize = 20, fontface = "bold"),
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
tempo3 = colnames(Symptome_init)
tempo2 = row_order(list_ht[[1]])
S1 =Symptome_init


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
#  "Limb weakness" =  cpt_perc(data_tempo$Limbs),
  "Proximal weakness" =  cpt_perc(Limbs_proximal),
  "Distal weakness" = cpt_perc(Limbs_dist),
  "Axial muscle weakness" =cpt_perc(data_tempo$Axial),
  "Respiratory involvement" =cpt_perc(data_tempo$Respiratory),
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
  "Bulbar symptoms"= cpt_perc(data_tempo$Bulbar),
  "Hypotonia"= cpt_perc(unlist(lapply(data_tempo$Libre,FUN =Is_hypo)))
)

colnames(Symptome_init) = gsub(x = colnames(Symptome_init),pattern = "[.]",replacement = " ")
tempo = match(tempo3[tempo][tempo3[tempo]%in%colnames(Symptome_init)],colnames(Symptome_init))



library(dplyr)
library(reshape2)
library(ComplexHeatmap)  # For creating heatmaps
library(colorRamp2)  # For creating color gradients
library(ggplot2)  # For general plotting
library(RColorBrewer)  # For color palettes
my_palette <- colorRampPalette(brewer.pal(9, "Greens"))(n = 299) 

Symptome_init = Symptome_init[!(rownames(Symptome_init)%in%"Others"),]
list_ht[i] <- Heatmap(as.matrix(Symptome_init)*100,  # The Heatmap function takes a matrix as input, here we convert our data frame (mat) to a matrix
                      name = "Proportion of Symptoms (%)",  # This parameter provides a name for the heatmap legend
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
data_tempo = data_tempo_list[[1]]
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
i=i+1
list_ht[i] <- Heatmap(as.matrix(Symptome_init[!(rownames(Symptome_init)%in%"Others"),])*100,  # The Heatmap function takes a matrix as input, here we convert our data frame (mat) to a matrix
                      name = "Proportion of Symptoms (%)",  # This parameter provides a name for the heatmap legend
                      col = my_palette,  # Defines the color scheme of the heatmap using the 'my_palette' object we created earlier. It'll map the scores in our data to the colors in this palette
                      #left_annotation = row_ha,  # This parameter is used to add annotations (metadata) to the rows of the heatmap. The annotation object 'row_ha' that we created earlier is passed here
                      show_row_names = TRUE,  # This parameter controls whether the row names of the matrix (which correspond to the muscle names) should be shown on the heatmap
                      show_column_names = TRUE,  # This parameter controls whether the column names of the matrix (which correspond to the patients) should be shown on the heatmap
                      row_title = "Gene",  # This parameter is used to specify a title for the row names of the heatmap
                       column_title = "Medical history",  # This parameter is used to specify a title for the column names of the heatmap. It's left empty here
                      cluster_rows = FALSE,  # This parameter controls whether to hierarchically cluster the rows (muscles), i.e., group together muscles that have similar data patterns across patients
                      clustering_distance_rows = "manhattan",
                      clustering_distance_columns = "manhattan", 
                      clustering_method_columns ="ward.D2",
                      clustering_method_rows  ="ward.D2",
                      #column_title = "",
                      column_title_side = "bottom",row_order = tempo2,
                      cluster_columns = FALSE,  # This parameter controls whether to hierarchically cluster the columns (patients), i.e., group together patients that have similar muscle score patterns
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



ht_opt(legend_border = "black",annotation_border = TRUE)
#-----------------------------------------------------------------------------
ht_list =   list_ht[[1]] +  list_ht[[2]]#+list_ht[[3]]  
draw(ht_list, ht_gap = unit(c(1,2), "cm") ,merge_legend = TRUE, column_title_gp = gpar(fontsize = 16))




