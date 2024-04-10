tempo_cpt  =   Symptome_init_cpt
tempo_perc =   Symptome_init



# Initialisation des matrices
tempo_cpt  = Symptome_init_cpt
tempo_perc = Symptome_init
total_gene = table(Data_xsl$`Involved gene`)
total_gene =total_gene[names(total_gene)%in%rownames(tempo_perc)]


# Initialisation d'une matrice pour stocker les p-values
p_values_matrix = matrix(NA, nrow = nrow(tempo_cpt), ncol = ncol(tempo_cpt))
rownames(p_values_matrix) = rownames(tempo_cpt)
colnames(p_values_matrix) = colnames(tempo_cpt)

# Boucle à travers chaque symptôme (colonne)
for (symptome in seq_len(ncol(tempo_cpt))) {
  
  # Boucle à travers chaque groupe génétique (ligne)
  for (groupe in seq_len(nrow(tempo_cpt))) {
    
    # Création d'une matrice de contingence
    observed = c(tempo_cpt[groupe, symptome], sum(tempo_cpt[, symptome]) - tempo_cpt[groupe, symptome])
    expected = c(total_gene[groupe] - tempo_cpt[groupe, symptome], sum(total_gene) - total_gene[groupe] - sum(tempo_cpt[, symptome]) + tempo_cpt[groupe, symptome])
    contingency_matrix = matrix(c(observed, expected), nrow = 2)
    
    # Vérification des petits effectifs
    if (min(contingency_matrix) < 5) {
      # Utilisation du test exact de Fisher
      fisher_test = fisher.test(contingency_matrix,alternative = "greater")
      p_values_matrix[groupe, symptome] = fisher_test$p.value
    } else {
      # Utilisation du test du chi-carré
      chi_test = chisq.test(contingency_matrix, correct = FALSE)
      p_values_matrix[groupe, symptome] = chi_test$p.value
    }
  }
}

# Affichage des p-values
print(p_values_matrix)
p_val_2 = p_values_matrix
p_val_2[p_values_matrix < 0.05] = "*"
p_val_2[p_values_matrix < 0.01] = "**"
p_val_2[p_values_matrix < 0.001] = "***"
p_val_2[p_values_matrix > 0.05] = ""

write.xlsx(p_values_matrix, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output3.xlsx", sheetName = "heatmap_1_pval", append = TRUE, row.names = TRUE)
write.xlsx(p_val_2, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output3.xlsx", sheetName = "heatmap_2_pval", append = TRUE, row.names = TRUE)

dim(p_val_2)
# calcul des zscores de la matrice 
calculer_zscore <- function(matrice) {
  # Initialiser une matrice pour stocker les z-scores
  zscores <- matrix(0, nrow = nrow(matrice), ncol = ncol(matrice))
  # Pour chaque colonne de la matrice
  for (i in 1:ncol(matrice)) {zscores[,i] = (matrice[,i] - mean(matrice[,i]))/sd(matrice[,i])}
  return(zscores)
}

# Utilisation de la fonction
matrice_zscores <- calculer_zscore(tempo_perc)
colnames(matrice_zscores) = colnames(tempo_perc)
rownames(matrice_zscores) = rownames(tempo_perc)

write.xlsx(matrice_zscores, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output3.xlsx", sheetName = "Zscores_new2", append = TRUE, row.names = TRUE)
write.xlsx(tempo_cpt, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output3.xlsx", sheetName = "comptages", append = TRUE, row.names = TRUE)
write.xlsx(tempo_perc, "/Users/f.fer/Documents/1_Projets/20230801_julianT/output3.xlsx", sheetName = "pourcentages", append = TRUE, row.names = TRUE)
