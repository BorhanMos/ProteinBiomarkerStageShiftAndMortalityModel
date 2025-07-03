library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

xStage <- c("I", "II", "III", "IV")

# Collect inputs
cancer_type <- tolower(readline(prompt = "Enter the cancer type (breast, lung, colorectal, ovarian): "))
p_values <- list(
  breast = 0.7,
  colorectal = 0.39,
  lung = 0.92,
  ovarian = 0.99
)
if (!(cancer_type %in% names(p_values))) {
  stop("Invalid cancer type. Please enter one of: breast, lung, colorectal, ovarian.")
}
p <- p_values[[cancer_type]]

sensitivity_input <- as.numeric(readline(prompt = "Enter the protein's sensitivity (0 to 1): "))
if (is.na(sensitivity_input) || sensitivity_input < 0 || sensitivity_input > 1) {
  stop("Invalid input. Please enter a number between 0 and 1.")
}

# Supporting functions
reconstruct_flow <- function(cumulative_sens, dwell_detect_rate) {
  nn <- length(cumulative_sens)
  box_detect <- diamond_detect <- circle_preclinical <- arrow_slip <- arrow_arrive <- arrow_evolve <- arrow_clinical <- rep(0, nn)
  miss <- 0
  remain <- 1
  i <- 1
  
  arrive <- cumulative_sens[i]
  arrow_arrive[i] <- arrive
  live <- arrive + miss
  diamond_detect[i] <- live
  box_detect[i] <- live * dwell_detect_rate[i]
  miss <- live * (1 - dwell_detect_rate[i])
  remain <- remain - arrive
  arrow_evolve[i] <- remain
  circle_preclinical[i] <- remain
  arrow_slip[i] <- miss
  
  if (nn > 1) {
    for (i in 2:nn) {
      arrive <- cumulative_sens[i] - cumulative_sens[i - 1]
      arrow_arrive[i] <- arrive
      live <- arrive + miss
      diamond_detect[i] <- live
      box_detect[i] <- live * dwell_detect_rate[i]
      miss <- live * (1 - dwell_detect_rate[i])
      arrow_slip[i] <- miss
      remain <- remain - arrive
      arrow_evolve[i] <- remain
      circle_preclinical[i] <- remain
    }
  }
  
  box_clinical <- rep(0, nn)
  arrow_miss <- box_clinical
  box_clinical[nn] <- remain + miss
  arrow_clinical[nn] <- remain
  arrow_miss[nn] <- miss
  arrow_slip[nn] <- 0
  
  tibble(
    prequel = 1:nn,
    box_detect = box_detect,
    box_clinical = box_clinical,
    diamond_detect = diamond_detect,
    circle_preclinical = circle_preclinical,
    arrow_arrive = arrow_arrive,
    arrow_slip = arrow_slip,
    arrow_evolve = arrow_evolve,
    arrow_miss = arrow_miss,
    arrow_clinical = arrow_clinical,
    arrow_detect = box_detect
  )
}

compute_effective_detection_flow <- function(incidence_sens_source, dwell_slip_df,
                                             active_slip_clinical, intercept_start_at_stage) {
  just_detection <- incidence_sens_source %>%
    mutate(number_stage = match(Stage, xStage),
           prequel = number_stage,
           detect = iso_sens) %>%
    select(Cancer, prequel, detect)
  
  just_delta <- just_detection %>%
    group_by(Cancer) %>%
    arrange(prequel, .by_group = TRUE) %>%
    mutate(delta_detect = diff(c(0, detect))) %>%
    ungroup()
  
  just_slip_delta_detailed <- just_delta %>%
    left_join(dwell_slip_df %>%
                select(Cancer, prequel = number_stage, slip, slip_clinical),
              by = c("Cancer", "prequel")) %>%
    filter(!is.na(prequel)) %>%
    mutate(unroll = 4) %>%
    uncount(unroll, .id = "clinical") %>%
    filter(clinical >= prequel) %>%
    mutate(modified_slip = case_when(
      prequel < clinical ~ slip,
      prequel == clinical & active_slip_clinical ~ slip_clinical,
      prequel == clinical & !active_slip_clinical ~ slip,
      TRUE ~ 1.0
    )) %>%
    mutate(modified_slip = case_when(
      prequel <= intercept_start_at_stage ~ modified_slip,
      TRUE ~ 1.0
    )) %>%
    arrange(Cancer, clinical, prequel) %>%
    group_by(Cancer, clinical) %>%
    summarize(sens_slip = list(reconstruct_flow(detect, 1 - modified_slip)), .groups = "drop") %>%
    unnest(cols = c(sens_slip))
  
  just_slip_delta_detailed
}

intercept_with_flow <- function(incidence_sens_source, dwell_slip_df,
                                active_slip_clinical = TRUE, intercept_start_at_stage = 0) {
  incidence_set <- incidence_sens_source %>%
    filter(Stage %in% xStage) %>%
    select(Cancer, Stage, IR) %>%
    mutate(number_stage = match(Stage, xStage),
           clinical = number_stage,
           unroll = number_stage) %>%
    uncount(unroll, .id = "prequel")
  
  just_slip_delta_flow <- compute_effective_detection_flow(
    incidence_sens_source, dwell_slip_df,
    active_slip_clinical, intercept_start_at_stage
  )
  
  total_flow <- just_slip_delta_flow %>%
    pivot_longer(cols = 4:13, names_to = "state", values_to = "flow") %>%
    left_join(incidence_set %>%
                select(Cancer, clinical, prequel, IR),
              by = c("Cancer", "clinical", "prequel")) %>%
    mutate(flow = flow * IR) %>%
    select(Cancer, clinical, prequel, state, flow) %>%
    pivot_wider(names_from = state, values_from = flow)
  
  total_flow
}

# Main simulation runner
simulate_mortality_reduction <- function(sensitivity_input) {
  incidence_sens_source <- tibble(
    Cancer = cancer_type,
    Stage = xStage,
    IR = rep(0.25, 4),
    iso_sens = rep(sensitivity_input, 4)
  )
  
  dwell_slip_df <- tibble(
    Cancer = cancer_type,
    number_stage = 1:4,
    slip = rep(0.5, 4),
    slip_clinical = rep(0.5, 4)
  )
  
  results <- replicate(100, {
    noisy_iso <- rbeta(4, shape1 = sensitivity_input * 10, shape2 = (1 - sensitivity_input) * 10)
    incidence_sens_source$iso_sens <- noisy_iso
    flow_tbl <- intercept_with_flow(incidence_sens_source, dwell_slip_df,
                                    active_slip_clinical = TRUE,
                                    intercept_start_at_stage = 1)
    
    flow_tbl <- flow_tbl %>%
      mutate(stage_prequel = xStage[prequel],
             stage_clinical = xStage[clinical])
    
    shifted <- flow_tbl %>%
      filter(stage_clinical %in% c("III", "IV"), stage_prequel %in% c("I", "II")) %>%
      summarize(shifted_detect = sum(arrow_detect, na.rm = TRUE)) %>%
      pull(shifted_detect)
    
    total_late <- flow_tbl %>%
      filter(stage_clinical %in% c("III", "IV")) %>%
      summarize(total = sum(arrow_arrive, na.rm = TRUE)) %>%
      pull(total)
    
    a <- ifelse(total_late == 0, 0, shifted / total_late)
    p * a
  })
  
  mean(results)
}

# Wrapper to run simulation and optimize
run_stage_shift_model_with_optim <- function(cancer_type, p, sensitivity_input) {
  mortality_reduction_input <- simulate_mortality_reduction(sensitivity_input)
  
  opt_result <- optim(par = 0.5, 
                      fn = function(s) -simulate_mortality_reduction(s), 
                      method = "Brent", 
                      lower = 0, upper = 1)
  
  best_sensitivity <- opt_result$par
  best_mortality_reduction <- -opt_result$value
  
  cat("\nCancer Type:", cancer_type, "\n")
  cat("Surrogate Endpoint (p):", p, "\n")
  cat("Input Sensitivity:", round(sensitivity_input, 4), "\n")
  cat("Estimated Mortality Reduction (Mean):", round(mortality_reduction_input, 4), "\n")
  cat("Best Sensitivity to Lower Mortality:", round(best_sensitivity, 4), "\n")
  cat("Best Mortality Reduction:", round(best_mortality_reduction, 4), "\n")
  
  # Show histogram for input sensitivity
  results_input <- replicate(100, {
    noisy_iso <- rbeta(4, shape1 = sensitivity_input * 10, shape2 = (1 - sensitivity_input) * 10)
    incidence_sens_source <- tibble(
      Cancer = cancer_type,
      Stage = xStage,
      IR = rep(0.25, 4),
      iso_sens = noisy_iso
    )
    dwell_slip_df <- tibble(
      Cancer = cancer_type,
      number_stage = 1:4,
      slip = rep(0.5, 4),
      slip_clinical = rep(0.5, 4)
    )
    flow_tbl <- intercept_with_flow(incidence_sens_source, dwell_slip_df,
                                    active_slip_clinical = TRUE,
                                    intercept_start_at_stage = 1)
    
    flow_tbl <- flow_tbl %>%
      mutate(stage_prequel = xStage[prequel],
             stage_clinical = xStage[clinical])
    
    shifted <- flow_tbl %>%
      filter(stage_clinical %in% c("III", "IV"), stage_prequel %in% c("I", "II")) %>%
      summarize(shifted_detect = sum(arrow_detect, na.rm = TRUE)) %>%
      pull(shifted_detect)
    
    total_late <- flow_tbl %>%
      filter(stage_clinical %in% c("III", "IV")) %>%
      summarize(total = sum(arrow_arrive, na.rm = TRUE)) %>%
      pull(total)
    
    a <- ifelse(total_late == 0, 0, shifted / total_late)
    p * a
  })
  
  hist(results_input, main = paste("Mortality Reduction Distribution -", cancer_type),
       xlab = "Mortality Reduction", col = "skyblue", border = "white")
  abline(v = mean(results_input), col = "red", lwd = 2)
}

# Run the model with optimization
run_stage_shift_model_with_optim(cancer_type, p, sensitivity_input)

