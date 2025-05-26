library(readxl)
library(pROC)
library(dplyr)
file_path <- "Unity Class/Friday/cancer.table.xlsx"
cancer_table <- read_excel(file_path, sheet = "Table S6")
protein_cols <- names(cancer_table)[6:44]
cancer_table$IsCase <- as.numeric(as.character(cancer_table$IsCase))
normal_samples <- cancer_table %>% filter(TumorType == "Normal")
auc_results <- list()
for (cancer in unique(cancer_table$TumorType)) {
  if (is.na(cancer) || cancer == "Normal") next
  stages <- unique(cancer_table$`AJCC Stage`[!is.na(cancer_table$`AJCC Stage`)])
  for (stage in stages) {
    cancer_samples <- cancer_table %>% filter(TumorType == cancer, `AJCC Stage` == stage)
    combined_samples <- bind_rows(cancer_samples, normal_samples)
    if (length(unique(combined_samples$IsCase)) < 2) next # Check if there are two levels in IsCase
    for (protein in protein_cols) {
      y_true <- combined_samples$IsCase
      y_scores <- combined_samples[[protein]]
      y_scores <- as.numeric(y_scores)
      auc_value <- suppressMessages(auc(y_true, y_scores))
      auc_results <- rbind(auc_results, data.frame(Cancer = cancer, Stage = stage, Protein = protein, AUC = auc_value))
    }
  }
}
auc_df <- as.data.frame(auc_results)
auc_df <- auc_df%>% arrange(Cancer, Stage)
print_chunks <- function(df, chunk_size = 234) {
  n <- nrow(df)
  for (i in seq(1, n, by = chunk_size)) {
    chunk <- df[i:min(i + chunk_size - 1, n), ]
    print(chunk)
  }
}
print_chunks(auc_df)

