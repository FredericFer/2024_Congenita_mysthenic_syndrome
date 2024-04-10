library(xlsx)
data1 = read.xlsx(file = "D:/output.xlsx",sheetName = "heatmap_1_cpt")
results <- list()

for (symptom in colnames(data1)[2:(ncol(data1)-1)]) {
  contingency_table <- table(rownames(data1), data1[,symptom])
  chi_test <- chisq.test(contingency_table)
  results[[symptom]] <- chi_test
}

# Correction pour Tests Multiples : Lors de l'exécution de tests multiples, le taux d'erreur de type I peut augmenter. Une correction de Bonferroni ou une autre méthode peut être utilisée pour contrôler le taux d'erreur familiale.

# Correction de Bonferroni
p_values <- sapply(results, function(x) x$p.value)
adjusted_p_values <- p.adjust(p_values, method = "bonferroni")

# Interprétation des Résultats : Après avoir obtenu les p-valeurs corrigées, il est possible d'identifier les symptômes qui sont significativement associés à des groupes génétiques en utilisant un seuil prédéfini (souvent \( \alpha = 0.05 \)).

# Identification des symptômes significativement associés
significant_symptoms <- names(adjusted_p_values)[adjusted_p_values < 0.05]
