library(ggalluvial)
library(tidyverse)
library(readxl)

#import dataset
X20200113_external_train_sens <- read_excel("20200113_external_train_sens.xlsx")
View(X20200113_external_train_sens)
data <- X20200113_external_train_sens




#breast
breast_cancer_counts <- data %>%
  filter(cancer_type_tfl == "Breast") %>%
  group_by(cstage) %>%
  summarise(total_cancers = sum(total_cancers)) %>%
  ungroup()

stage_1_b <- breast_cancer_counts %>% filter(cstage == "I") %>% pull(total_cancers)
stage_2_b <- breast_cancer_counts %>% filter(cstage == "II") %>% pull(total_cancers)
stage_3_b <- breast_cancer_counts %>% filter(cstage == "III") %>% pull(total_cancers)
stage_4_b <- breast_cancer_counts %>% filter(cstage == "IV") %>% pull(total_cancers)

#stage shift percentages
breast_stage_shifts <- data.frame(
  SEER = factor(c('IV', 'IV', 'IV', 'IV', 'III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III', 'IV')),
  intercept = factor(c('IV', 'III', 'II', 'I', 'III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III', 'IV')),
  Freq = c(
    round(stage_4_b * 0.100175656),
    round(stage_4_b * 0.059757075),
    round(stage_4_b * 0.036157488),
    round(stage_4_b * 0.803909781),
    round(stage_3_b * 0.054050821),
    round(stage_3_b * 0.040714771),
    round(stage_3_b * 0.905234408),
    round(stage_2_b * 0.03846395),
    round(stage_2_b * 0.96153605),
    stage_1_b * 1
  )
)

death_probabilities_breast <- c("I" = 0.01, "II" = 0.07, "III" = 0.13, "IV" = 0.68)  

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

stage_colors_breast <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#ff7f00", "IV" = "#e41a1c")

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
colon_cancer_counts <- data %>%
  filter(cancer_type_tfl == "Colon/Rectum") %>%
  group_by(cstage) %>%
  summarise(total_cancers = sum(total_cancers)) %>%
  ungroup()

stage_1_c <- colon_cancer_counts %>% filter(cstage == "I") %>% pull(total_cancers)
stage_2_c <- colon_cancer_counts %>% filter(cstage == "II") %>% pull(total_cancers)
stage_3_c <- colon_cancer_counts %>% filter(cstage == "III") %>% pull(total_cancers)
stage_4_c <- colon_cancer_counts %>% filter(cstage == "IV") %>% pull(total_cancers)

#stage shift percentages
colon_stage_shifts <- data.frame(
  SEER = factor(c('IV', 'IV', 'IV', 'IV', 'III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III', 'IV')),
  intercept = factor(c('IV', 'III', 'II', 'I', 'III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III', 'IV')),
  Freq = c(
    round(stage_4_c * 0.03427468),
    round(stage_4_c * 0.017037564),
    round(stage_4_c * 0.099550606),
    round(stage_4_c * 0.849137149),
    round(stage_3_c * 0.012133914),
    round(stage_3_c * 0.103661787),
    round(stage_3_c * 0.884204299),
    round(stage_2_c * 0.086067541),
    round(stage_2_c * 0.913932459),
    stage_1_c * 1
  )
)

death_probabilities_colon <- c("I" = 0.09, "II" = 0.175, "III" = 0.27, "IV" = 0.87)  

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

stage_colors_colon <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#ff7f00", "IV" = "#e41a1c")

sankey_seer_to_intercept_colon <- breast_stage_shifts %>%
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
esophagus_cancer_counts <- data %>%
  filter(cancer_type_tfl == "Esophagus") %>%
  group_by(cstage) %>%
  summarise(total_cancers = sum(total_cancers)) %>%
  ungroup()

stage_1_e <- esophagus_cancer_counts %>% filter(cstage == "I") %>% pull(total_cancers)
stage_2_e <- esophagus_cancer_counts %>% filter(cstage == "II") %>% pull(total_cancers)
stage_3_e <- esophagus_cancer_counts %>% filter(cstage == "III") %>% pull(total_cancers)
stage_4_e <- esophagus_cancer_counts %>% filter(cstage == "IV") %>% pull(total_cancers)

#stage shift percentages
esophagus_stage_shifts <- data.frame(
  SEER = factor(c('IV', 'IV', 'IV', 'IV', 'III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III', 'IV')),
  intercept = factor(c('IV', 'III', 'II', 'I', 'III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III', 'IV')),
  Freq = c(
    round(stage_4_e * 0.001748817),
    round(stage_4_e * 0.0149404),
    round(stage_4_e * 0.145814499),
    round(stage_4_e * 0.837496284),
    round(stage_3_e * 0.012057639),
    round(stage_3_e * 0.146501312),
    round(stage_3_e * 0.841441049),
    round(stage_2_e * 0.134087741),
    round(stage_2_e * 0.865912259),
    stage_1_e * 1
  )
)

death_probabilities_esophagus <- c("I" = 0.52, "II" = 0.7, "III" = 0.72, "IV" = 0.95)  

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

stage_colors_esophagus <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#ff7f00", "IV" = "#e41a1c")

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
lung_cancer_counts <- data %>%
  filter(cancer_type_tfl == "Lung") %>%
  group_by(cstage) %>%
  summarise(total_cancers = sum(total_cancers)) %>%
  ungroup()

stage_1_l <- lung_cancer_counts %>% filter(cstage == "I") %>% pull(total_cancers)
stage_2_l <- lung_cancer_counts %>% filter(cstage == "II") %>% pull(total_cancers)
stage_3_l <- lung_cancer_counts %>% filter(cstage == "III") %>% pull(total_cancers)
stage_4_l <- lung_cancer_counts %>% filter(cstage == "IV") %>% pull(total_cancers)

#stage shift percentages
lung_stage_shifts <- data.frame(
  SEER = factor(c('IV', 'IV', 'IV', 'IV', 'III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III', 'IV')),
  intercept = factor(c('IV', 'III', 'II', 'I', 'III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III', 'IV')),
  Freq = c(
    round(stage_4_l * 0.013056265),
    round(stage_4_l * 0.016476781),
    round(stage_4_l * 0.096273947),
    round(stage_4_l * 0.874193007),
    round(stage_3_l * 0.011478794),
    round(stage_3_l * 0.098064997),
    round(stage_3_l * 0.89045621),
    round(stage_2_l * 0.081273149),
    round(stage_2_l * 0.918726851),
    stage_1_l * 1
  )
)

death_probabilities_lung <- c("I" = 0.35, "II" = .5, "III" = 0.63, "IV" = 0.91)  

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

stage_colors_lung <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#ff7f00", "IV" = "#e41a1c")

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
liver_cancer_counts <- data %>%
  filter(cancer_type_tfl == "Liver/Bile-duct") %>%
  group_by(cstage) %>%
  summarise(total_cancers = sum(total_cancers)) %>%
  ungroup()

stage_1_li <- liver_cancer_counts %>% filter(cstage == "I") %>% pull(total_cancers)
stage_2_li <- liver_cancer_counts %>% filter(cstage == "II") %>% pull(total_cancers)
stage_3_li <- liver_cancer_counts %>% filter(cstage == "III") %>% pull(total_cancers)
stage_4_li <- liver_cancer_counts %>% filter(cstage == "IV") %>% pull(total_cancers)

#stage shift percentages
liver_stage_shifts <- data.frame(
  SEER = factor(c('IV', 'IV', 'IV', 'IV', 'III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III', 'IV')),
  intercept = factor(c('IV', 'III', 'II', 'I', 'III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III', 'IV')),
  Freq = c(
    round(stage_4_li * 0.013056265),
    round(stage_4_li * 0.016476781),
    round(stage_4_li * 0.096273947),
    round(stage_4_li * 0.874193007),
    round(stage_3_li * 0.011478794),
    round(stage_3_li * 0.098064997),
    round(stage_3_li * 0.89045621),
    round(stage_2_li * 0.081273149),
    round(stage_2_li * 0.918726851),
    stage_1_li * 1
  )
)

death_probabilities_liver <- c("I" = .63, "II" = .675, "III" = .87, "IV" = .97)  

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

stage_colors_liver <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#ff7f00", "IV" = "#e41a1c")

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
ovary_cancer_counts <- data %>%
  filter(cancer_type_tfl == "Ovary") %>%
  group_by(cstage) %>%
  summarise(total_cancers = sum(total_cancers)) %>%
  ungroup()

stage_1_o <- ovary_cancer_counts %>% filter(cstage == "I") %>% pull(total_cancers)
stage_2_o <- ovary_cancer_counts %>% filter(cstage == "II") %>% pull(total_cancers)
stage_3_o <- ovary_cancer_counts %>% filter(cstage == "III") %>% pull(total_cancers)
stage_4_o <- ovary_cancer_counts %>% filter(cstage == "IV") %>% pull(total_cancers)

#stage shift percentages
ovary_stage_shifts <- data.frame(
  SEER = factor(c('IV', 'IV', 'IV', 'IV', 'III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III', 'IV')),
  intercept = factor(c('IV', 'III', 'II', 'I', 'III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III', 'IV')),
  Freq = c(
    round(stage_4_o * 0.001748817),
    round(stage_4_o * 0.0149404),
    round(stage_4_o * 0.145814499),
    round(stage_4_o * 0.837496284),
    round(stage_3_o * 0.012057639),
    round(stage_3_o * 0.146501312),
    round(stage_3_o * 0.841441049),
    round(stage_2_o * 0.134087741),
    round(stage_2_o * 0.865912259),
    stage_1_o * 1
  )
)

death_probabilities_ovary <- c("I" = 0.07, "II" = .25, "III" = 0.55, "IV" = 0.69)  

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

stage_colors_ovary <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#ff7f00", "IV" = "#e41a1c")

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
pancreas_cancer_counts <- data %>%
  filter(cancer_type_tfl == "Pancreas") %>%
  group_by(cstage) %>%
  summarise(total_cancers = sum(total_cancers)) %>%
  ungroup()

stage_1_p <- pancreas_cancer_counts %>% filter(cstage == "I") %>% pull(total_cancers)
stage_2_p <- pancreas_cancer_counts %>% filter(cstage == "II") %>% pull(total_cancers)
stage_3_p <- pancreas_cancer_counts %>% filter(cstage == "III") %>% pull(total_cancers)
stage_4_p <- pancreas_cancer_counts %>% filter(cstage == "IV") %>% pull(total_cancers)

#stage shift percentages
pancreas_stage_shifts <- data.frame(
  SEER = factor(c('IV', 'IV', 'IV', 'IV', 'III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III', 'IV')),
  intercept = factor(c('IV', 'III', 'II', 'I', 'III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III', 'IV')),
  Freq = c(
    round(stage_4_p * 0.006831624),
    round(stage_4_p * 0.027151904),
    round(stage_4_p * 0.046183052),
    round(stage_4_p * 0.91983342),
    round(stage_3_p * 0.01886101),
    round(stage_3_p * 0.046906025),
    round(stage_3_p * 0.934232965),
    round(stage_2_p * 0.038766927),
    round(stage_2_p * 0.961233073),
    stage_1_p * 1
  )
)

death_probabilities_pancreas <- c("I" = .56, "II" = .66, "III" = .84, "IV" = .97)  

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

stage_colors_pancreas <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#ff7f00", "IV" = "#e41a1c")

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
stomach_cancer_counts <- data %>%
  filter(cancer_type_tfl == "Stomach") %>%
  group_by(cstage) %>%
  summarise(total_cancers = sum(total_cancers)) %>%
  ungroup()

stage_1_s <- stomach_cancer_counts %>% filter(cstage == "I") %>% pull(total_cancers)
stage_2_s <- stomach_cancer_counts %>% filter(cstage == "II") %>% pull(total_cancers)
stage_3_s <- stomach_cancer_counts %>% filter(cstage == "III") %>% pull(total_cancers)
stage_4_s <- stomach_cancer_counts %>% filter(cstage == "IV") %>% pull(total_cancers)

#stage shift percentages
stomach_stage_shifts <- data.frame(
  SEER = factor(c('IV', 'IV', 'IV', 'IV', 'III', 'III', 'III', 'II', 'II', 'I'),
                levels = c('I', 'II', 'III', 'IV')),
  intercept = factor(c('IV', 'III', 'II', 'I', 'III', 'II', 'I', 'II', 'I', 'I'),
                     levels = c('I', 'II', 'III', 'IV')),
  Freq = c(
    round(stage_4_s * 0.035174653),
    round(stage_4_s * 0.03872409),
    round(stage_4_s * 0.074180367),
    round(stage_4_s * 0.805735533),
    round(stage_3_s * 0.029219955),
    round(stage_3_s * 0.081840571),
    round(stage_3_s * 0.888939474),
    round(stage_2_s * 0.068860539),
    round(stage_2_s * 0.931139461),
    stage_1_s * 1
  )
)

death_probabilities_stomach <- c("I" = .25, "II" = .65, "III" = .8, "IV" = .93)  

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

stage_colors_stomach <- c("I" = "#4daf4a", "II" = "#377eb8", "III" = "#ff7f00", "IV" = "#e41a1c")

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