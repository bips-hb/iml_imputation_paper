library(data.table)
library(ggplot2)
library(patchwork)
library(foreach)

source("R/config.R")

tpfis = readRDS(sprintf("%s/tpfis.Rds", res_dir))
pfis = readRDS(sprintf("%s/pfis-experiments.Rds", res_dir))

#res_pfis1 <- pfis[missing_prob == 0.4 & problem == "x1234" & algorithm == "xgboost" & pattern == "MCAR" & sampling_strategy == "bootstrap" & adjusted & imputation_method == "mean" & nrefits==15, ]

pfis_copy <- copy(pfis)

pfis_copy[!train_missing & !test_missing, imputation_method := "None"]
pfis_copy[!train_missing & !test_missing, missing_prob := 0]
pfis_copy[!train_missing & !test_missing, pattern := "None"]
pfis_copy[, missing := "None"]
pfis_copy[train_missing & test_missing, missing := "Both"]
pfis_copy[train_missing & !test_missing, missing := "Train"]
pfis_copy[!train_missing & test_missing, missing := "Test"]
pfis_copy[, missing := factor(missing)]

#pfis_copy[imputation_method=="None" & feature=="x1"&algorithm== "lm"&problem == "x12"&adjusted== FALSE,]

pfis_copy1 <- pfis_copy[problem=="x1234" & algorithm == "xgboost" & sampling_strategy== "bootstrap" & missing_prob== 0.4 & pattern == "MCAR" & adjusted == TRUE & nrefits ==15,]
pfis_copy1_None <- pfis_copy[problem=="x1234" & algorithm == "xgboost" & sampling_strategy== "bootstrap" & adjusted == TRUE & nrefits ==15 & imputation_method== "None",]
pfis_copy1_None$pattern <- "MCAR"
pfis_copy1_None$missing_prob <- 0.4

pfis_copy1 <- pfis_copy[problem=="x12" & algorithm == "lm" & sampling_strategy== "bootstrap" & missing_prob== 0.4 & pattern == "MCAR" & adjusted == TRUE & nrefits ==15,]
pfis_copy1_None <- pfis_copy[problem=="x12" & algorithm == "lm" & sampling_strategy== "bootstrap" & adjusted == TRUE & nrefits ==15 & imputation_method== "None",]
pfis_copy1_None$pattern <- "MCAR"
pfis_copy1_None$missing_prob <- 0.4

pfis_copy1 <- rbind(pfis_copy1, pfis_copy1_None)

pfis_copy1_mean = pfis_copy1[, .(var3 = mean(var3), pfi = mean(pfi),  se2 = mean( se2), lower  = mean(lower ), upper  = mean(upper)),
                                 by = list(feature, algorithm, problem, sampling_strategy, nrefits, 
                                           adjusted, n, missing_prob, pattern, train_missing, test_missing, imputation_method)]

#lvls_ordered <- res_cmplt$pfi[order(pfi), feature]
#res_pfi[, feature := factor(feature, levels = lvls_ordered)]

tpfis1 <- tpfis[problem == "x1234" & algorithm == "xgboost" ,]
tpfis1 <- tpfis[problem == "x12" & algorithm == "lm" ,]

pfis_copy1_mean[, feature := factor(feature)]
ggplot(pfis_copy1_mean, aes(x = feature, y = pfi, color = imputation_method )) + 
  geom_point(position=position_dodge2(width=1, reverse = TRUE)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position=position_dodge2(width=1, reverse = TRUE)) + 
  geom_hline(yintercept = 0, col = "red") + 
  geom_point(data = tpfis1, aes(x = feature, y = tpfi), 
             color = "blue", shape = 17, size = 3) +
  geom_hline(data = tpfis1, aes(yintercept = tpfi), col = "blue") + 
  coord_flip() + 
  theme_bw()

ggplot(pfis_copy1_mean, aes(x = feature, y = pfi, color = imputation_method)) + 
  geom_point(position = position_dodge2(width = 1, reverse = TRUE)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge2(width = 1, reverse = TRUE)) + 
  geom_segment(data = tpfis1, 
               aes(x = feature, xend = feature, 
                   y = tpfi, yend = tpfi), 
               inherit.aes = FALSE, # Prevents conflicts with main ggplot aes()
               color = "blue", linewidth = 1.2) + 
  coord_flip() + 
  theme_bw()



res_pfi <- rbind(data.table(imputation = "complete", res_cmplt$pfi), 
                 data.table(imputation = "mean", res_imp_mean$pfi),
                 #data.table(imputation = "missForest", res_imp_mf$pfi),
                 data.table(imputation = "mice", res_imp_mice$pfi))
#res_pfi[, imputation := factor(imputation, levels = c("complete", "mean", "missForest", "mice"))]
lvls_ordered <- res_cmplt$pfi[order(pfi), feature]
res_pfi[, feature := factor(feature, levels = lvls_ordered)]
ggplot(res_pfi, aes(x = feature, y = pfi, color = imputation)) + 
  geom_point(position=position_dodge2(width=1, reverse = TRUE)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position=position_dodge2(width=1, reverse = TRUE)) + 
  geom_hline(yintercept = 0, col = "red") + 
  coord_flip() + 
  theme_bw()