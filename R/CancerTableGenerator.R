library(ggalluvial)
library(tidyverse)
library(readxl)
cancer_table <- read_excel("C:/Downloads/cancer.table.xlsx", 
                           sheet = "Table S6")

#breast
breast_cancer_counts <- cancer_table %>%
  filter(TumorType == "Breast") %>%
  group_by(`AJCC Stage`) %>%
  summarise(total_cancers = n()) %>%
  ungroup()

stage_1_b <- breast_cancer_counts %>% filter(`AJCC Stage` == "I") %>% pull(total_cancers)
stage_2_b <- breast_cancer_counts %>% filter(`AJCC Stage` == "II") %>% pull(total_cancers)
stage_3_b <- breast_cancer_counts %>% filter(`AJCC Stage` == "III") %>% pull(total_cancers)

#stage shift percentages
breast_stage_shifts <- data.frame(
  SEER = factor(c('III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III')),
  intercept = factor(c('III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III')),
  Freq = c(
    round(stage_3_b * 0.608),
    round(stage_3_b * 0.228),
    round(stage_3_b * 0.164),
    round(stage_2_b * 0.836),
    round(stage_2_b * 0.164),
    stage_1_b * 1
  )
)

death_probabilities_breast <- c("I" = 0.01, "II" = 0.07, "III" = 0.13)  

breast_stage_shifts <- breast_stage_shifts %>%
  mutate(
    Deaths = round(Freq * death_probabilities_breast[as.character(SEER)]),
    Alive = Freq - Deaths
  ) %>%
  pivot_longer(
    cols = c("Alive", "Deaths"),
    names_to = "Outcome",
    values_to = "Outcome_Freq"
  )

stage_colors_breast <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#e41a1c")

sankey_seer_to_intercept_breast <- breast_stage_shifts %>%
  group_by(SEER, intercept) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show stage shift
ggplot(sankey_seer_to_intercept_breast, aes(axis1 = SEER, axis2 = intercept, y = Freq)) +
  geom_alluvium(aes(fill = SEER), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("SEER", "Intercept")) +
  scale_fill_manual(values = stage_colors_breast) +
  labs(
    title = "Sankey Diagram: SEER to Intercept (Breast Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()

sankey_intercept_to_outcome_breast <- breast_stage_shifts %>%
  group_by(intercept, Outcome) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show deaths
ggplot(sankey_intercept_to_outcome_breast, aes(axis1 = intercept, axis2 = Outcome, y = Freq)) +
  geom_alluvium(aes(fill = intercept), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Intercept", "Outcome")) +
  scale_fill_manual(values = stage_colors_breast) +
  labs(
    title = "Sankey Diagram: Intercept to Outcome (Breast Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()






#colon/rectum
colorectum_cancer_counts <- cancer_table %>%
  filter(TumorType == "Colorectum") %>%
  group_by(`AJCC Stage`) %>%
  summarise(total_cancers = n()) %>%
  ungroup()

stage_1_c <- colorectum_cancer_counts %>% filter(`AJCC Stage` == "I") %>% pull(total_cancers)
stage_2_c <- colorectum_cancer_counts %>% filter(`AJCC Stage` == "II") %>% pull(total_cancers)
stage_3_c <- colorectum_cancer_counts %>% filter(`AJCC Stage` == "III") %>% pull(total_cancers)

#stage shift percentages
colon_stage_shifts <- data.frame(
  SEER = factor(c('III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III')),
  intercept = factor(c('III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III')),
  Freq = c(
    round(stage_3_c * 0.313),
    round(stage_3_c * 0.349),
    round(stage_3_c * 0.338),
    round(stage_2_c * 0.662),
    round(stage_2_c * 0.338),
    stage_1_c * 1
  )
)

death_probabilities_colon <- c("I" = 0.09, "II" = 0.175, "III" = 0.27)  

colon <- colon_stage_shifts %>%
  mutate(
    Deaths = round(Freq * death_probabilities_colon[as.character(SEER)]),
    Alive = Freq - Deaths
  ) %>%
  pivot_longer(
    cols = c("Alive", "Deaths"),
    names_to = "Outcome",
    values_to = "Outcome_Freq"
  )

stage_colors_colon <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#e41a1c")

sankey_seer_to_intercept_colon <- colon %>%
  group_by(SEER, intercept) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show stage shift
ggplot(sankey_seer_to_intercept_colon, aes(axis1 = SEER, axis2 = intercept, y = Freq)) +
  geom_alluvium(aes(fill = SEER), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("SEER", "Intercept")) +
  scale_fill_manual(values = stage_colors_colon) +
  labs(
    title = "Sankey Diagram: SEER to Intercept (Colon Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()

sankey_intercept_to_outcome_colon <- colon %>%
  group_by(intercept, Outcome) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

ggplot(sankey_intercept_to_outcome_colon, aes(axis1 = intercept, axis2 = Outcome, y = Freq)) +
  geom_alluvium(aes(fill = intercept), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Intercept", "Outcome")) +
  scale_fill_manual(values = stage_colors_colon) +
  labs(
    title = "Sankey Diagram: Intercept to Outcome (Colon Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()





#esophagus
esophagus_cancer_counts <- cancer_table %>%
  filter(TumorType == "Esophagus") %>%
  group_by(`AJCC Stage`) %>%
  summarise(total_cancers = n()) %>%
  ungroup()

stage_1_e <- esophagus_cancer_counts %>% filter(`AJCC Stage` == "I") %>% pull(total_cancers)
stage_2_e <- esophagus_cancer_counts %>% filter(`AJCC Stage` == "II") %>% pull(total_cancers)
stage_3_e <- esophagus_cancer_counts %>% filter(`AJCC Stage` == "III") %>% pull(total_cancers)

#stage shift percentages
esophagus_stage_shifts <- data.frame(
  SEER = factor(c('III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III')),
  intercept = factor(c('III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III')),
  Freq = c(
    round(stage_3_e * 0.287),
    round(stage_3_e * 0.406),
    round(stage_3_e * 0.307),
    round(stage_2_e * 0.694),
    round(stage_2_e * 0.306),
    stage_1_e * 1
  )
)

death_probabilities_esophagus <- c("I" = 0.52, "II" = 0.7, "III" = 0.72)  

esophagus_stage_shifts <- esophagus_stage_shifts %>%
  mutate(
    Deaths = round(Freq * death_probabilities_esophagus[as.character(SEER)]),
    Alive = Freq - Deaths
  ) %>%
  pivot_longer(
    cols = c("Alive", "Deaths"),
    names_to = "Outcome",
    values_to = "Outcome_Freq"
  )

stage_colors_esophagus <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#e41a1c")

sankey_seer_to_intercept_esophagus <- esophagus_stage_shifts %>%
  group_by(SEER, intercept) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show stage shift
ggplot(sankey_seer_to_intercept_esophagus, aes(axis1 = SEER, axis2 = intercept, y = Freq)) +
  geom_alluvium(aes(fill = SEER), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("SEER", "Intercept")) +
  scale_fill_manual(values = stage_colors_esophagus) +
  labs(
    title = "Sankey Diagram: SEER to Intercept (Esophageal Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()

sankey_intercept_to_outcome_esophagus <- esophagus_stage_shifts %>%
  group_by(intercept, Outcome) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show deaths
ggplot(sankey_intercept_to_outcome_esophagus, aes(axis1 = intercept, axis2 = Outcome, y = Freq)) +
  geom_alluvium(aes(fill = intercept), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Intercept", "Outcome")) +
  scale_fill_manual(values = stage_colors_esophagus) +
  labs(
    title = "Sankey Diagram: Intercept to Outcome (Esophageal Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()





#lung
lung_cancer_counts <- cancer_table %>%
  filter(TumorType == "Lung") %>%
  group_by(`AJCC Stage`) %>%
  summarise(total_cancers = n()) %>%
  ungroup()

stage_1_l <- lung_cancer_counts %>% filter(`AJCC Stage` == "I") %>% pull(total_cancers)
stage_2_l <- lung_cancer_counts %>% filter(`AJCC Stage` == "II") %>% pull(total_cancers)
stage_3_l <- lung_cancer_counts %>% filter(`AJCC Stage` == "III") %>% pull(total_cancers)

#stage shift percentages
lung_stage_shifts <- data.frame(
  SEER = factor(c('III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III')),
  intercept = factor(c('III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III')),
  Freq = c(
    round(stage_3_l * 0.278),
    round(stage_3_l * 0.372),
    round(stage_3_l * 0.35),
    round(stage_2_l * 0.65),
    round(stage_2_l * 0.35),
    stage_1_l * 1
  )
)

death_probabilities_lung <- c("I" = 0.35, "II" = .5, "III" = 0.63)  

lung_stage_shifts <- lung_stage_shifts %>%
  mutate(
    Deaths = round(Freq * death_probabilities_lung[as.character(SEER)]),
    Alive = Freq - Deaths
  ) %>%
  pivot_longer(
    cols = c("Alive", "Deaths"),
    names_to = "Outcome",
    values_to = "Outcome_Freq"
  )

stage_colors_lung <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#e41a1c")

sankey_seer_to_intercept_lung <- lung_stage_shifts %>%
  group_by(SEER, intercept) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show stage shift
ggplot(sankey_seer_to_intercept_lung, aes(axis1 = SEER, axis2 = intercept, y = Freq)) +
  geom_alluvium(aes(fill = SEER), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("SEER", "Intercept")) +
  scale_fill_manual(values = stage_colors_lung) +
  labs(
    title = "Sankey Diagram: SEER to Intercept (Lung Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()

sankey_intercept_to_outcome_lung <- lung_stage_shifts %>%
  group_by(intercept, Outcome) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show deaths
ggplot(sankey_intercept_to_outcome_lung, aes(axis1 = intercept, axis2 = Outcome, y = Freq)) +
  geom_alluvium(aes(fill = intercept), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Intercept", "Outcome")) +
  scale_fill_manual(values = stage_colors_lung) +
  labs(
    title = "Sankey Diagram: Intercept to Outcome (Lung Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()





#liver
liver_cancer_counts <- cancer_table %>%
  filter(TumorType == "Liver") %>%
  group_by(`AJCC Stage`) %>%
  summarise(total_cancers = n()) %>%
  ungroup()

stage_1_li <- liver_cancer_counts %>% filter(`AJCC Stage` == "I") %>% pull(total_cancers)
stage_2_li <- liver_cancer_counts %>% filter(`AJCC Stage` == "II") %>% pull(total_cancers)
stage_3_li <- liver_cancer_counts %>% filter(`AJCC Stage` == "III") %>% pull(total_cancers)

#stage shift percentages
liver_stage_shifts <- data.frame(
  SEER = factor(c('III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III')),
  intercept = factor(c('III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III')),
  Freq = c(
    round(stage_3_li * 0.451),
    round(stage_3_li * 0.391),
    round(stage_3_li * 0.158),
    round(stage_2_li * 0.64),
    round(stage_2_li * 0.36),
    stage_1_li * 1
  )
)

death_probabilities_liver <- c("I" = .63, "II" = .675, "III" = .87)  

liver_stage_shifts <- liver_stage_shifts %>%
  mutate(
    Deaths = round(Freq * death_probabilities_liver[as.character(SEER)]),
    Alive = Freq - Deaths
  ) %>%
  pivot_longer(
    cols = c("Alive", "Deaths"),
    names_to = "Outcome",
    values_to = "Outcome_Freq"
  )

stage_colors_liver <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#e41a1c")

sankey_seer_to_intercept_liver <- liver_stage_shifts %>%
  group_by(SEER, intercept) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show stage shift
ggplot(sankey_seer_to_intercept_liver, aes(axis1 = SEER, axis2 = intercept, y = Freq)) +
  geom_alluvium(aes(fill = SEER), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("SEER", "Intercept")) +
  scale_fill_manual(values = stage_colors_liver) +
  labs(
    title = "Sankey Diagram: SEER to Intercept (Liver Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()

sankey_intercept_to_outcome_liver <- liver_stage_shifts %>%
  group_by(intercept, Outcome) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show deaths
ggplot(sankey_intercept_to_outcome_liver, aes(axis1 = intercept, axis2 = Outcome, y = Freq)) +
  geom_alluvium(aes(fill = intercept), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Intercept", "Outcome")) +
  scale_fill_manual(values = stage_colors_liver) +
  labs(
    title = "Sankey Diagram: Intercept to Outcome (Liver Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()





#ovary
ovary_cancer_counts <- cancer_table %>%
  filter(TumorType == "Ovary") %>%
  group_by(`AJCC Stage`) %>%
  summarise(total_cancers = n()) %>%
  ungroup()

stage_1_o <- ovary_cancer_counts %>% filter(`AJCC Stage` == "I") %>% pull(total_cancers)
stage_2_o <- ovary_cancer_counts %>% filter(`AJCC Stage` == "II") %>% pull(total_cancers)
stage_3_o <- ovary_cancer_counts %>% filter(`AJCC Stage` == "III") %>% pull(total_cancers)

#stage shift percentages
ovary_stage_shifts <- data.frame(
  SEER = factor(c('III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III')),
  intercept = factor(c('III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III')),
  Freq = c(
    round(stage_3_o * 0.483),
    round(stage_3_o * 0.314),
    round(stage_3_o * 0.203),
    round(stage_2_o * 0.797),
    round(stage_2_o * 0.203),
    stage_1_o * 1
  )
)

death_probabilities_ovary <- c("I" = 0.07, "II" = .25, "III" = 0.55)  

ovary_stage_shifts <- ovary_stage_shifts %>%
  mutate(
    Deaths = round(Freq * death_probabilities_ovary[as.character(SEER)]),
    Alive = Freq - Deaths
  ) %>%
  pivot_longer(
    cols = c("Alive", "Deaths"),
    names_to = "Outcome",
    values_to = "Outcome_Freq"
  )

stage_colors_ovary <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#e41a1c")

sankey_seer_to_intercept_ovary <- ovary_stage_shifts %>%
  group_by(SEER, intercept) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show stage shift
ggplot(sankey_seer_to_intercept_ovary, aes(axis1 = SEER, axis2 = intercept, y = Freq)) +
  geom_alluvium(aes(fill = SEER), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("SEER", "Intercept")) +
  scale_fill_manual(values = stage_colors_ovary) +
  labs(
    title = "Sankey Diagram: SEER to Intercept (Ovarian Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()

sankey_intercept_to_outcome_ovary <- ovary_stage_shifts %>%
  group_by(intercept, Outcome) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show deaths
ggplot(sankey_intercept_to_outcome_ovary, aes(axis1 = intercept, axis2 = Outcome, y = Freq)) +
  geom_alluvium(aes(fill = intercept), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Intercept", "Outcome")) +
  scale_fill_manual(values = stage_colors_ovary) +
  labs(
    title = "Sankey Diagram: Intercept to Outcome (Ovarian Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()





#pancreas
pancreas_cancer_counts <- cancer_table %>%
  filter(TumorType == "Pancreas") %>%
  group_by(`AJCC Stage`) %>%
  summarise(total_cancers = n()) %>%
  ungroup()

stage_1_p <- pancreas_cancer_counts %>% filter(`AJCC Stage` == "I") %>% pull(total_cancers)
stage_2_p <- pancreas_cancer_counts %>% filter(`AJCC Stage` == "II") %>% pull(total_cancers)
stage_3_p <- pancreas_cancer_counts %>% filter(`AJCC Stage` == "III") %>% pull(total_cancers)

#stage shift percentages
pancreas_stage_shifts <- data.frame(
  SEER = factor(c('III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III')),
  intercept = factor(c('III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III')),
  Freq = c(
    round(stage_3_p * 0.494),
    round(stage_3_p * 0.294),
    round(stage_3_p * 0.212),
    round(stage_2_p * 0.788),
    round(stage_2_p * 0.212),
    stage_1_p * 1
  )
)

death_probabilities_pancreas <- c("I" = .56, "II" = .66, "III" = .84)  

pancreas_stage_shifts <- pancreas_stage_shifts %>%
  mutate(
    Deaths = round(Freq * death_probabilities_pancreas[as.character(SEER)]),
    Alive = Freq - Deaths
  ) %>%
  pivot_longer(
    cols = c("Alive", "Deaths"),
    names_to = "Outcome",
    values_to = "Outcome_Freq"
  )

stage_colors_pancreas <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#e41a1c")

sankey_seer_to_intercept_pancreas <- pancreas_stage_shifts %>%
  group_by(SEER, intercept) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show stage shift
ggplot(sankey_seer_to_intercept_pancreas, aes(axis1 = SEER, axis2 = intercept, y = Freq)) +
  geom_alluvium(aes(fill = SEER), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("SEER", "Intercept")) +
  scale_fill_manual(values = stage_colors_pancreas) +
  labs(
    title = "Sankey Diagram: SEER to Intercept (Pancreatic Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()

sankey_intercept_to_outcome_pancreas <- pancreas_stage_shifts %>%
  group_by(intercept, Outcome) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show deaths
ggplot(sankey_intercept_to_outcome_pancreas, aes(axis1 = intercept, axis2 = Outcome, y = Freq)) +
  geom_alluvium(aes(fill = intercept), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Intercept", "Outcome")) +
  scale_fill_manual(values = stage_colors_pancreas) +
  labs(
    title = "Sankey Diagram: Intercept to Outcome (Pancreatic Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()





#stomach
stomach_cancer_counts <- cancer_table %>%
  filter(TumorType == "Stomach") %>%
  group_by(`AJCC Stage`) %>%
  summarise(total_cancers = n()) %>%
  ungroup()

stage_1_s <- stomach_cancer_counts %>% filter(`AJCC Stage` == "I") %>% pull(total_cancers)
stage_2_s <- stomach_cancer_counts %>% filter(`AJCC Stage` == "II") %>% pull(total_cancers)
stage_3_s <- stomach_cancer_counts %>% filter(`AJCC Stage` == "III") %>% pull(total_cancers)

#stage shift percentages
stomach_stage_shifts <- data.frame(
  SEER = factor(c('III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III')),
  intercept = factor(c('III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III')),
  Freq = c(
    round(stage_3_s * 0.3),
    round(stage_3_s * 0.368),
    round(stage_3_s * 0.332),
    round(stage_2_s * 0.668),
    round(stage_2_s * 0.332),
    stage_1_s * 1
  )
)

death_probabilities_stomach <- c("I" = .25, "II" = .65, "III" = .8)  

stomach_stage_shifts <- stomach_stage_shifts %>%
  mutate(
    Deaths = round(Freq * death_probabilities_stomach[as.character(SEER)]),
    Alive = Freq - Deaths
  ) %>%
  pivot_longer(
    cols = c("Alive", "Deaths"),
    names_to = "Outcome",
    values_to = "Outcome_Freq"
  )

stage_colors_stomach <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#e41a1c")

sankey_seer_to_intercept_stomach <- stomach_stage_shifts %>%
  group_by(SEER, intercept) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show stage shift
ggplot(sankey_seer_to_intercept_stomach, aes(axis1 = SEER, axis2 = intercept, y = Freq)) +
  geom_alluvium(aes(fill = SEER), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("SEER", "Intercept")) +
  scale_fill_manual(values = stage_colors_stomach) +
  labs(
    title = "Sankey Diagram: SEER to Intercept (Stomach Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()

sankey_intercept_to_outcome_stomach <- stomach_stage_shifts %>%
  group_by(intercept, Outcome) %>%
  summarise(Freq = sum(Outcome_Freq), .groups = "drop")

#create Sankey plot to show deaths
ggplot(sankey_intercept_to_outcome_stomach, aes(axis1 = intercept, axis2 = Outcome, y = Freq)) +
  geom_alluvium(aes(fill = intercept), width = 0.2) +
  geom_stratum(width = 0.2, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Intercept", "Outcome")) +
  scale_fill_manual(values = stage_colors_stomach) +
  labs(
    title = "Sankey Diagram: Intercept to Outcome (Stomach Cancer)",
    x = "Stages",
    y = "Number of Cases"
  ) +
  theme_minimal()

