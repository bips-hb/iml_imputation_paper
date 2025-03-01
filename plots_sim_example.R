library(dplyr)
library(tidyr)

library(data.table)
library(ggplot2)
library(patchwork)
library(foreach)

source("R/config.R")

coverage_pfi_mean = readRDS(sprintf("%s/coverage_pfi_mean.Rds", res_dir))
coverage_pdp_mean = readRDS(sprintf("%s/coverage_pdp_mean.Rds", res_dir))
coverage_shap_mean = readRDS(sprintf("%s/coverage_shap_mean.Rds", res_dir))

coverage_pfi_mean[!train_missing & !test_missing, imputation_method := "None"]
coverage_pfi_mean[!train_missing & !test_missing, missing_prob := 0]
coverage_pfi_mean[!train_missing & !test_missing, pattern := "None"]
coverage_pfi_mean[, missing := "None"]
coverage_pfi_mean[train_missing & test_missing, missing := "Both"]
coverage_pfi_mean[train_missing & !test_missing, missing := "Train"]
coverage_pfi_mean[!train_missing & test_missing, missing := "Test"]
coverage_pfi_mean[, missing := factor(missing)]

coverage_pdp_mean[!train_missing & !test_missing, imputation_method := "None"]
coverage_pdp_mean[!train_missing & !test_missing, missing_prob := 0]
coverage_pdp_mean[!train_missing & !test_missing, pattern := "None"]
coverage_pdp_mean[, missing := "None"]
coverage_pdp_mean[train_missing & test_missing, missing := "Both"]
coverage_pdp_mean[train_missing & !test_missing, missing := "Train"]
coverage_pdp_mean[!train_missing & test_missing, missing := "Test"]
coverage_pdp_mean[, missing := factor(missing)]

coverage_shap_mean[!train_missing & !test_missing, imputation_method := "None"]
coverage_shap_mean[!train_missing & !test_missing, missing_prob := 0]
coverage_shap_mean[!train_missing & !test_missing, pattern := "None"]
coverage_shap_mean[, missing := "None"]
coverage_shap_mean[train_missing & test_missing, missing := "Both"]
coverage_shap_mean[train_missing & !test_missing, missing := "Train"]
coverage_shap_mean[!train_missing & test_missing, missing := "Test"]
coverage_shap_mean[, missing := factor(missing)]

add_None <- function(df) {
  # Step 1: Extract the baseline values for imputation_method = "None"
  baseline_values <- df %>%
    filter(imputation_method == "None") %>%
    select(feature, algorithm, problem, sampling_strategy, nrefits, adjusted, n, 
           coverage, avg_width, coverage_se, bias)
  
  # Step 2: Generate all possible missing_prob and pattern combinations
  unique_conditions <- df %>%
    select(feature, algorithm, problem, sampling_strategy, nrefits, adjusted, n, 
           missing_prob, pattern) %>%
    distinct()
  
  # Step 3: Create full dataset ensuring "None" appears everywhere
  df_none_full <- baseline_values %>%
    left_join(unique_conditions, by = c("feature", "algorithm", "problem", "sampling_strategy", "nrefits", "adjusted", "n")) %>%
    mutate(imputation_method = "None", missing = "Train")
  
  # Step 4: Merge back into original dataset, ensuring no duplicates
  df_final <- df %>%
    filter(imputation_method != "None") %>%  # Keep all existing imputation methods
    bind_rows(df_none_full) %>%              # Add the corrected "None" values
    arrange(feature, algorithm, problem, sampling_strategy, nrefits, adjusted, n, missing_prob, pattern, imputation_method) # Sort for clarity
  
  df_final <- df_final[missing_prob > 0]
  return(df_final)
}

coverage_pfi_mean <- add_None(coverage_pfi_mean)
coverage_pdp_mean <- add_None(coverage_pdp_mean)
coverage_shap_mean <- add_None(coverage_shap_mean)

# Coverage ---------------------------------------------------------------------

coverage_pfi_mean <- coverage_pfi_mean[, coverage := mean(coverage), 
                                       by = list(algorithm, problem, sampling_strategy, nrefits, 
                                                 adjusted, n, missing_prob, pattern, missing, imputation_method) # train_missing, test_missing, 
]

coverage_pdp_mean <- coverage_pdp_mean[, coverage := mean(coverage), 
                                       by = list(algorithm, problem, sampling_strategy, nrefits, 
                                                 adjusted, n, missing_prob, pattern, missing, imputation_method)
]

coverage_shap_mean<- coverage_shap_mean[, coverage := mean(coverage), 
                                        by = list(algorithm, problem, sampling_strategy, nrefits, 
                                                  adjusted, n, missing_prob, pattern, missing, imputation_method)
]

coverage_pfi_mean$iml_method <- "PFI"
coverage_pdp_mean$iml_method <- "PDP"
coverage_shap_mean$iml_method <- "SHAP"


#Choose setting to be plottet and bind the data
coverage_pfi <- coverage_pfi_mean[sampling_strategy == "bootstrap" & algorithm == "xgboost" & pattern == "MAR" & missing_prob == 0.4, ]

coverage_pdp <- coverage_pdp_mean[sampling_strategy == "bootstrap" & algorithm == "xgboost" & pattern == "MAR" & missing_prob == 0.4, ]

coverage_shap <- coverage_shap_mean[sampling_strategy == "bootstrap" & algorithm == "xgboost" & pattern == "MAR" & missing_prob == 0.4, ]

coverage_mean <- rbind(coverage_pfi, coverage_pdp, coverage_shap)
coverage_mean[problem == "x12"]$problem <- "linear"
coverage_mean[problem == "x1234"]$problem <- "non-linear"

#plot coverage over different iml_methods and problem
p1 <- ggplot(coverage_mean, aes(x = nrefits, y = coverage, color = imputation_method,
                                shape = adjusted, linetype = adjusted)) +
  facet_grid(problem ~ iml_method) + 
  geom_line() + 
  scale_y_continuous(sprintf("Confidence Interval %s", "Coverage"), limits = c(0, 1)) +
  scale_x_continuous("Number of Model Refits") +
  #scale_color_discrete("Imputation Method") + 
  scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotdash"))+
  scale_color_manual("Imputation Method", 
                     values = c("None" = "red", "missForest" = "purple", "mice" = "darkorange3", "mice_rf" = "blue", "mean" = "forestgreen"),
                     labels = c("None" = "Complete Data", "mice" = "MICE PMM", "mice_rf"= "MICE RF", "missForest" = "MissForest", "mean" = "Mean")) + 
  theme_bw(base_size = 24) + 
  #theme(plot.title = element_text(size = 18))+
  geom_hline(yintercept = .95, linetype = "dashed") + 
  ggtitle("Coverage: learner = XGBoost, pattern = MAR, mis. = 0.4")#IML Methods for XGBoost with 40% Missingness (MAR)")

ggsave("plots_paper/coverage_xgb_MAR_04.pdf", width = 210, height = 120, units = "mm", scale = 2) #width = 20, height = 10, limitsize = FALSE)


# Average width ----------------------------------------------------------------

width_pfi_mean <- coverage_pfi_mean[nrefits == 15 & adjusted == TRUE]# & sampling_strategy == "bootstrap" & algorithm == "xgboost" & pattern == "MAR", ]
width_pdp_mean <- coverage_pdp_mean[nrefits == 15 & adjusted == TRUE]# & sampling_strategy == "bootstrap" & algorithm == "xgboost" & pattern == "MAR", ]
width_shap_mean <- coverage_shap_mean[nrefits == 15 & adjusted == TRUE]# & sampling_strategy == "bootstrap" & algorithm == "xgboost" & pattern == "MAR", ]


width_pfi_mean <- width_pfi_mean[,  avg_width := mean(avg_width), 
                                 by = list(algorithm, problem, sampling_strategy, #nrefits, adjusted, 
                                           n, missing_prob, pattern, missing, imputation_method) 
]

width_pdp_mean <- width_pdp_mean[, avg_width := mean(avg_width), 
                                 by = list(algorithm, problem, sampling_strategy, #nrefits, adjusted, 
                                           n, missing_prob, pattern, missing, imputation_method)
]

width_shap_mean <- width_shap_mean[, avg_width := mean(avg_width), 
                                   by = list(algorithm, problem, sampling_strategy, #nrefits, adjusted, 
                                             n, missing_prob, pattern, missing, imputation_method)
]

width_pfi_mean <- width_pfi_mean[nrefits == 15 & adjusted == TRUE & sampling_strategy == "bootstrap" & algorithm == "xgboost" & pattern == "MAR", ]
width_pdp_mean <- width_pdp_mean[nrefits == 15 & adjusted == TRUE & sampling_strategy == "bootstrap" & algorithm == "xgboost" & pattern == "MAR", ]
width_shap_mean <- width_shap_mean[nrefits == 15 & adjusted == TRUE & sampling_strategy == "bootstrap" & algorithm == "xgboost" & pattern == "MAR", ]

width_pfi_mean$iml_method <- "PFI"
width_pdp_mean$iml_method <- "PDP"
width_shap_mean$iml_method <- "SHAP"

width_mean <- rbind(width_pfi_mean, width_pdp_mean, width_shap_mean)

width_mean[problem == "x12"]$problem <- "linear"
width_mean[problem == "x1234"]$problem <- "non-linear"

p2 <- ggplot(width_mean, aes(x = missing_prob, y = avg_width, color = imputation_method)) +
  facet_grid(iml_method ~ problem, scales = "free")+#facet_grid(problem ~ iml_method, scales = "free") + #, scales = "free_y"
  geom_line() + 
  geom_point() + 
  theme_bw(base_size = 24)+
  #theme(plot.title = element_text(size = 18))+
  scale_color_discrete("Imputation Method") + 
  scale_color_manual("Imputation Method", 
                     values = c("None" = "red", "missForest" = "purple", "mice" = "darkorange3", "mice_rf" = "blue", "mean" = "forestgreen"),
                     labels = c("None" = "Complete Data", "mice" = "MICE PMM", "mice_rf"= "MICE RF", "missForest" = "MissForest", "mean" = "Mean")) + 
  labs(x = "Missingness Proportions",  y = "Average CI Width")+
  ggtitle("Average CI width: learner = XGBoost, pattern = MAR, nrefits = 15") 

ggsave("plots_paper/width_xgb_MAR.pdf", width = 210, height = 120, units = "mm", scale = 2)


# Bias -------------------------------------------------------------------------

bias_pfi_mean  <- coverage_pfi_mean[nrefits == 15 & adjusted == TRUE]
bias_pdp_mean <- coverage_pdp_mean[nrefits == 15 & adjusted == TRUE]
bias_shap_mean <- coverage_shap_mean[nrefits == 15 & adjusted == TRUE]


bias_pfi_mean <- bias_pfi_mean[,  bias := mean(bias), 
                                   by = list(algorithm, problem, sampling_strategy, 
                                             n, missing_prob, pattern, missing, imputation_method) 
]

bias_pdp_mean <- bias_pdp_mean[, bias := mean(bias), 
                                   by = list(algorithm, problem, sampling_strategy, #nrefits, adjusted, 
                                             n, missing_prob, pattern, missing, imputation_method)
]

bias_shap_mean <- bias_shap_mean[, bias := mean(bias), 
                                     by = list(algorithm, problem, sampling_strategy, #nrefits, adjusted, 
                                               n, missing_prob, pattern, missing, imputation_method)
]

bias_pfi_mean <- bias_pfi_mean[nrefits == 15 & adjusted == TRUE & sampling_strategy == "bootstrap" & algorithm == "xgboost" & pattern == "MAR", ]
bias_pdp_mean <- bias_pdp_mean[nrefits == 15 & adjusted == TRUE & sampling_strategy == "bootstrap" & algorithm == "xgboost" & pattern == "MAR", ]
bias_shap_mean <- bias_shap_mean[nrefits == 15 & adjusted == TRUE & sampling_strategy == "bootstrap" & algorithm == "xgboost" & pattern == "MAR", ]

bias_pfi_mean$iml_method <- "PFI"
bias_pdp_mean$iml_method <- "PDP"
bias_shap_mean$iml_method <- "SHAP"

bias_mean <- rbind(bias_pfi_mean, bias_pdp_mean, bias_shap_mean)

bias_mean[problem == "x12"]$problem <- "linear"
bias_mean[problem == "x1234"]$problem <- "non-linear"

p3 <- ggplot(bias_mean, aes(x = missing_prob, y = bias, color = imputation_method)) +
  facet_grid(problem ~ iml_method, scales = "free_y") + #
  geom_line() + 
  geom_point() + 
  theme_bw(base_size = 18)+
  #theme(plot.title = element_text(size = 18))+
  scale_color_discrete("Imputation Method") + 
  scale_color_manual("Imputation Method", 
                     values = c("None" = "red", "missForest" = "purple", "mice" = "darkorange3", "mice_rf" = "blue", "mean" = "forestgreen"),
                     labels = c("None" = "Complete Data", "mice" = "MICE PMM", "mice_rf"= "MICE RF", "missForest" = "MissForest", "mean" = "Mean")) + 
  labs(x = "Missingness Proportions",  y = "Bias")+
  ggtitle("Bias: learner = XGBoost, pattern = MAR, nrefits = 15")#IML Methods for XGBoost over missingness proportions for 15 refits (MAR)")

#ggsave("plots_paper/bias_xgb_MAR.pdf", width = 210, height = 120, units = "mm", scale = 2)
ggsave("plots_paper/bias_xgb_MAR.pdf", width = 210, height = 100, units = "mm", scale = 1.5)

#pp <- p1 / p2 / p3 + 
#  plot_layout(heights = c(1.4, 1, 1))  # Slightly bigger first plot

#ggsave("plot_MAR_xgb_free_scales.pdf", plot = pp, width = 210, height = 260, units = "mm", scale = 1.5)