library(dplyr)
library(tidyr)

library(data.table)
library(foreach)

source("R/config.R")

#Calculate relative bias

N_EXPERIMENTS = 1000

tpfis = readRDS(sprintf("%s/tpfis.Rds", res_dir))
tpdps = readRDS(sprintf("%s/tpdps.Rds", res_dir))
tshaps = readRDS(sprintf("%s/tshaps.Rds", res_dir))

pfis = readRDS(sprintf("%s/pfis-experiments.Rds", res_dir))
pdps = readRDS(sprintf("%s/pdps-experiments.Rds", res_dir))
shaps = readRDS(sprintf("%s/shaps-experiments.Rds", res_dir))

cis_pfi = merge(pfis, tpfis, by = c("feature", "algorithm", "problem", "n"))
cis_pdp = merge(pdps, tpdps, by = c("feature", "algorithm", "problem", "feature_value", "n"))
cis_shap = merge(shaps, tshaps, by = c("feature", "algorithm", "problem", "n"))

# =============================================================================
# Compute coverage for PFI
# =============================================================================
cis_pfi = cis_pfi[, in_ci := (lower <= tpfi) & (tpfi <= upper)]
coverage_pfi_new = cis_pfi[,.(coverage = mean(in_ci),
                              coverage_se = (1/N_EXPERIMENTS) * sd(in_ci),
                              avg_width = mean(upper - lower), 
                              bias = mean((tpfi - pfi)/tpfi), 
                              pfi = mean(pfi)),
                           by = list(feature, algorithm, problem, max_refits, n_perm, sampling_strategy, nrefits, 
                                     adjusted, n, missing_prob, pattern, train_missing, test_missing, imputation_method)]

coverage_pfi_mean_new = coverage_pfi_new[, .(coverage = mean(coverage), avg_width = mean(avg_width), coverage_se = mean(coverage_se), bias = mean(bias)),
                                         by = list(feature, algorithm, problem, sampling_strategy, nrefits, 
                                                   adjusted, n, missing_prob, pattern, train_missing, test_missing, imputation_method)]

coverage_pfi_mean_new
saveRDS(coverage_pfi_mean_new, sprintf("%s/coverage_pfi_mean_new.Rds", res_dir))

# =============================================================================
# Compute coverage  for PDP
# =============================================================================
cis_pdp = cis_pdp[, in_ci := (lower <= tpdp) & (tpdp <= upper)]
coverage_pdp_new = cis_pdp[,.(coverage = mean(in_ci),
                              coverage_se = (1/N_EXPERIMENTS) * sd(in_ci),
                              avg_width = mean(upper - lower), 
                              bias = mean((tpdp - mpdp)/tpdp)),
                           by = list(feature, feature_value, algorithm, problem, sampling_strategy, max_refits, nrefits, 
                                     adjusted, n, missing_prob, pattern, train_missing, test_missing, imputation_method)]

coverage_pdp_mean_new = coverage_pdp_new[, .(coverage = mean(coverage), avg_width = mean(avg_width), coverage_se = mean(coverage_se), bias = mean(bias)),
                                         by = list(feature, algorithm, problem, sampling_strategy, nrefits, 
                                                   adjusted, n, missing_prob, pattern, train_missing, test_missing, imputation_method)]

print(coverage_pdp_mean_new)
saveRDS(coverage_pdp_mean_new, sprintf("%s/coverage_pdp_mean_new.Rds", res_dir))

# =============================================================================
# Compute coverage for SHAP
# =============================================================================
cis_shap = cis_shap[, in_ci := (lower <= tshap) & (tshap <= upper)]
coverage_shap_new = cis_shap[,.(coverage = mean(in_ci),
                                coverage_se = (1/N_EXPERIMENTS) * sd(in_ci),
                                avg_width = mean(upper - lower), 
                                bias = mean((tshap - shap)/tshap)),
                             by = list(feature, algorithm, problem, max_refits, n_perm, sampling_strategy, nrefits, 
                                       adjusted, n, missing_prob, pattern, train_missing, test_missing, imputation_method)]

coverage_shap_mean_new = coverage_shap_new[, .(coverage = mean(coverage), avg_width = mean(avg_width), coverage_se = mean(coverage_se), bias = mean(bias)),
                                           by = list(feature, algorithm, problem, sampling_strategy, nrefits, 
                                                     adjusted, n, missing_prob, pattern, train_missing, test_missing, imputation_method)]
print(coverage_shap_mean_new)
saveRDS(coverage_shap_mean_new, sprintf("%s/coverage_shap_mean_new.Rds", res_dir))