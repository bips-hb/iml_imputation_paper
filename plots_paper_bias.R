library(dplyr)
library(tidyr)

library(data.table)
library(ggplot2)
library(patchwork)
library(foreach)

source("R/config.R")

#Set sampling parameter
sampling_strat <- "bootstrap"
#sampling_strat <- "subsampling"

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

plot_fun <- function(iml_method) {
  if (iml_method == "PFI") {
    coverage_mean = coverage_pfi_mean[nrefits == 15 & adjusted == TRUE & sampling_strategy==sampling_strat, ]
  } else if (iml_method == "PDP") {
    coverage_mean = coverage_pdp_mean[nrefits == 15 & adjusted == TRUE & sampling_strategy==sampling_strat, ]
  } else if (iml_method == "SHAP") {
    coverage_mean = coverage_shap_mean[nrefits == 15 & adjusted == TRUE & sampling_strategy==sampling_strat, ]
  } else {
    stop("Unknown iml method")
  }
  
  coverage_mean <- coverage_mean[, .(bias = mean(bias)), 
                                 by = .(n, problem, algorithm, sampling_strategy, missing,
                                        missing_prob, pattern, imputation_method)]
  
  
  coverage_mean[problem == "x12"]$problem <- "linear"
  coverage_mean[problem == "x1234"]$problem <- "non-linear"
  
  pars <- expand.grid(#n = unique(coverage_mean$n), 
    #sampling_strategy = as.character(unique(coverage_mean$sampling_strategy)), 
    missing = setdiff(unique(coverage_mean$missing), "None"), 
    algorithm = as.character(unique(coverage_mean$algorithm)),
    pattern = setdiff(as.character(unique(coverage_mean$pattern)), "None"), 
    #   missing_prob = setdiff(unique(coverage_mean$missing_prob), 0), 
    stringsAsFactors = FALSE)
  mapply(function(xmissing, xalgorithm, xpattern){#xn, , xsampling_strategy) {
    if(xalgorithm == "lm"){
      ggplot(coverage_mean[missing %in% c("None", xmissing) & 
                             algorithm %in% xalgorithm & 
                             pattern %in% c("None", xpattern) 
      ],
      aes(x = missing_prob, y = bias, color = imputation_method)) +
        facet_wrap(~ problem) + 
        geom_line() + 
        geom_point() + 
        #scale_color_discrete("Imputation Method") + 
        scale_color_manual("Imputation Method", 
                           values = c("None" = "red", "missForest" = "purple", "mice" = "darkorange3", "mice_rf" = "blue", "mean" = "forestgreen"),
                           labels = c("None" = "Complete Data", "mice" = "MICE PMM", "mice_rf"= "MICE RF", "missForest" = "MissForest", "mean" = "Mean")) + 
        theme_bw(base_size = 18) + 
        theme(plot.title = element_text(size = 18),
              legend.position = "none") + 
        labs(x = "Missingness Proportions",  y = "Bias")+
        ggtitle(sprintf("%s (learner = %s, pattern = %s, nrefits= %s)", 
                        iml_method, xalgorithm, xpattern, nrefits))
    }else{
      ggplot(coverage_mean[missing %in% c("None", xmissing) & 
                             algorithm %in% xalgorithm & 
                             pattern %in% c("None", xpattern) 
      ],
      aes(x = missing_prob, y = bias, color = imputation_method)) +
        facet_wrap(~ problem) + 
        geom_line() + 
        geom_point() + 
        #scale_color_discrete("Imputation Method") + 
        scale_color_manual("Imputation Method", 
                           values = c("None" = "red", "missForest" = "purple", "mice" = "darkorange3", "mice_rf" = "blue", "mean" = "forestgreen"),
                           labels = c("None" = "Complete Data", "mice" = "MICE PMM", "mice_rf"= "MICE RF", "missForest" = "MissForest", "mean" = "Mean")) + 
        theme_bw(base_size = 18) + 
        theme(plot.title = element_text(size = 18),axis.title.y = element_blank())+
        labs(x = "Missingness Proportions",  y = "Bias")+
        ggtitle(sprintf("%s (learner = %s, pattern = %s, nrefits= %s)", 
                        iml_method, xalgorithm, xpattern, nrefits))
    }
  },pars$missing, pars$algorithm, pars$pattern, SIMPLIFY = FALSE)
}

iml_methods <- c("PFI", "PDP", "SHAP")
pars <- data.table(expand.grid(iml_method = iml_methods, stringsAsFactors = FALSE))
#pars <- pars[!(iml_method == "SHAP" & learner == "randomForest"), ]
mapply(function(iml_method) {
  p <- plot_fun(iml_method)
  ggsave(sprintf("plots_paper/bias_paper_%s_%s.pdf", iml_method, sampling_strat), wrap_plots(p, ncol = 2), width = 20, height = 30, 
         limitsize = FALSE)
}, pars$iml_method, SIMPLIFY = FALSE)