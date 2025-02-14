
#' Calculate bootstrap confidence interval for learner PFI/PDP/SHAP
#'
#' @param model_fun Model function
#' @param data Data (target should be named y)
#' @param nrefits Number of refits for CI calculation
#' @param nperm Number of permutations for PFI
#' @param pattern Missing data pattern
#' @param missing_prob Missingness probability
#' @param imputation_method Imputation method
#'
#' @returns List of results
#' @export
#'
#' @examples
bootstrap_cis = function(model_fun, data, nrefits = 15, nperm = 5,
                         pattern = "none", missing_prob = 0, 
                         imputation_method = "none"){

  # Introduce missing data and impute
  if (pattern == "none" | missing_prob == 0) {
    miss_fun <- function(data, cols_mis) {
     data 
    }
  } else if (pattern == "MCAR") {
    miss_fun <- function(data, cols_mis) {
      missMethods::delete_MCAR(data, p = missing_prob, cols_mis = cols_mis)
    }
  } else if (pattern == "MAR") {
    miss_fun <- function(data, cols_mis) {
      # the other half are used as control variables
      fnames = setdiff(colnames(data), "y")
      cols_ctrl <- sample(setdiff(fnames, cols_mis), floor(length(fnames)/2))
      missMethods::delete_MAR_rank(data, p = missing_prob, 
                                   cols_mis = cols_mis, cols_ctrl = cols_ctrl) 
    }
  } else if (pattern == "MNAR") {
    miss_fun <- function(data, cols_mis) {
      missMethods::delete_MNAR_rank(data, p = missing_prob, cols_mis = cols_mis)
    } 
  } else {
    stop("Unknown missing data pattern")
  }
  
  # Impute missing data
  if (imputation_method == "none") {
    impute_fun <- function(data) {
      list(data)
    } 
  } else if (imputation_method == "mean") {
    impute_fun <- function(data) {
      list(missMethods::impute_mean(data))
    } 
  } else if (imputation_method == "mice") {
    impute_fun <- function(data) {
      imp <- mice::mice(data, m = missing_prob * 100,
                        print = FALSE)
      complete(imp, "all")
    }
  } else if (imputation_method == "mice_rf") {
    impute_fun <- function(data) {
      imp <- mice::mice(data, m = missing_prob * 100,
                        print = FALSE, method = "rf")
      complete(imp, "all")
    }
  } else if (imputation_method == "missForest") {
    impute_fun <- function(data) {
      imp <- missRanger(data, verbose = 0, 
                        num.trees = 100, num.threads = 1)
      list(imp)
    } 
  } else {
    stop("Unknown imputation method")
  }
  
  # Missing data
  fnames = setdiff(colnames(data), "y")
  if (pattern == "MAR") {
    cols_mis = sample(fnames, floor(length(fnames)/2))
  } else {
    cols_mis = fnames
  }
  
  data <- miss_fun(data, cols_mis)
  data <- impute_fun(data)
  
  results = lapply(seq_along(data), function(i) {
    lapply(seq_len(nrefits), function(m) {
      dat <- data[[i]]
      train_ids = sample(1:nrow(dat), size = nrow(dat), replace = TRUE)
      train_dat = dat[train_ids,]
      test_dat = dat[setdiff(1:nrow(dat), train_ids), ]
      
      # Creates the prediction function
      mod = model_fun(data = train_dat)
      fh = function(x) pred_fun(mod, newdata = x)
      pfis = compute_pfis(test_dat, nperm, fh)
      pdps = compute_pdps(test_dat, fh, xgrid = NULL)
      shaps = compute_shaps(test_dat, train_dat, fh, mod)
      perf = data.table(mse = mean((fh(test_dat) - test_dat$y)^2))
      pfis$refit_id = pdps$refit_id = shaps$refit_id = perf$refit_id = m
      pfis$imp_id = pdps$imp_id = shaps$imp_id = perf$imp_id = i
      
      list("pfis" = pfis, "pdps" = pdps, "shaps" = shaps, "perf" = perf)
    })
  })
  pfis = rbindlist(lapply(results, function(x) rbindlist(lapply(x, function(y) y[["pfis"]]))))
  pdps = rbindlist(lapply(results, function(x) rbindlist(lapply(x, function(y) y[["pdps"]]))))
  shaps = rbindlist(lapply(results, function(x) rbindlist(lapply(x, function(y) y[["shaps"]]))))
  perf = rbindlist(lapply(results, function(x) rbindlist(lapply(x, function(y) y[["perf"]]))))
  
  # CIs
  t_alpha = qt(1 - 0.05/2, df = nrefits - 1)
  res_pfi = compute_pfi_cis(pfis, t_alpha, adjust = TRUE, type = "bootstrap")
  res_pdp = compute_pdp_cis(pdps, t_alpha, adjust = TRUE, type = "bootstrap")
  res_shap = compute_shap_cis(shaps, t_alpha, adjust = TRUE, type = "bootstrap")
  
  list("pdp" = res_pdp, "pfi" = res_pfi, "shap" = res_shap, "perf" = perf)
}