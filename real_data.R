
library(iml.relating)
library(data.table)
library(xgboost)

set.seed(2025)

# Get data
dat <- fread("data/winequality-red.csv", check.names = TRUE)
colnames(dat)[ncol(dat)] <- "y"
dat <- as.data.frame(dat)

# xgboost model
model_fun <- function(data) {
  fnames = setdiff(colnames(data), "y")
  x = as.matrix(data[, fnames])
  y = data[, "y"]
  xgboost(data = x, label = y,
          nrounds = 20, objective = "reg:squarederror",
          max_depth = 2, verbose = FALSE, nthread = 1)
}

# library(ranger)
# model_fun <- function(data) {
#   ranger(y~., data, num.trees = 10)
# }

# Check performance
train_ids <- sample(1:nrow(dat), size = 0.7 * nrow(dat), replace = FALSE)
train_dat <- dat[train_ids,]
test_dat <- dat[setdiff(1:nrow(dat), train_ids), ]
fit <- model_fun(train_dat)
pred <- iml.relating:::pred_fun(fit, test_dat)
mse <- mean((pred - test_dat$y)^2)
rsq <- 1 - mse / var(test_dat$y)
mse
rsq

# Without missing data
res_cmplt <- bootstrap_cis(model_fun = model_fun, 
                           data = dat, 
                           nrefits = 15, 
                           nperm = 5,
                           pattern = "none", 
                           missing_prob = 0, 
                           imputation_method = "none")


lvls_ordered <- res_cmplt$pfi[order(pfi), feature]
res_cmplt$pfi[, feature := factor(feature, levels = lvls_ordered)]
ggplot(res_cmplt$pfi, aes(x = feature, y = pfi)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept = 0, col = "red") + 
  coord_flip() + 
  theme_bw()

lvls_ordered <- res_cmplt$shap[order(shap), feature]
res_cmplt$shap[, feature := factor(feature, levels = lvls_ordered)]
ggplot(res_cmplt$shap, aes(x = feature, y = shap)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept = 0, col = "red") + 
  coord_flip() + 
  theme_bw()

# ggplot(res_cmplt$pdp[feature == "alcohol"], aes(x = feature_value, y = mpdp)) +
#   #geom_point() +
#   geom_line() +
#   #geom_errorbar(aes(ymin = lower, ymax = upper)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#   theme_bw()

# With mean imputation
res_imp_mean <- bootstrap_cis(model_fun = model_fun, 
                              data = dat, 
                              nrefits = 15, 
                              nperm = 5,
                              pattern = "MNAR", 
                              missing_prob = 0.4, 
                              imputation_method = "mean")


lvls_ordered <- res_imp_mean$pfi[order(pfi), feature]
res_imp_mean$pfi[, feature := factor(feature, levels = lvls_ordered)]
ggplot(res_imp_mean$pfi, aes(x = feature, y = pfi)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept = 0, col = "red") + 
  coord_flip() + 
  theme_bw()

lvls_ordered <- res_imp_mean$shap[order(shap), feature]
res_imp_mean$shap[, feature := factor(feature, levels = lvls_ordered)]
ggplot(res_imp_mean$shap, aes(x = feature, y = shap)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept = 0, col = "red") + 
  coord_flip() + 
  theme_bw()

# With missForest imputation
res_imp_mf <- bootstrap_cis(model_fun = model_fun, 
                              data = dat, 
                              nrefits = 15, 
                              nperm = 5,
                              pattern = "MNAR", 
                              missing_prob = 0.4, 
                              imputation_method = "missForest")


lvls_ordered <- res_imp_mf$pfi[order(pfi), feature]
res_imp_mf$pfi[, feature := factor(feature, levels = lvls_ordered)]
ggplot(res_imp_mf$pfi, aes(x = feature, y = pfi)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept = 0, col = "red") + 
  coord_flip() + 
  theme_bw()

lvls_ordered <- res_imp_mf$shap[order(shap), feature]
res_imp_mf$shap[, feature := factor(feature, levels = lvls_ordered)]
ggplot(res_imp_mf$shap, aes(x = feature, y = shap)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept = 0, col = "red") + 
  coord_flip() + 
  theme_bw()

# With multiple imputation
res_imp_mice <- bootstrap_cis(model_fun = model_fun, 
                              data = dat, 
                              nrefits = 15, 
                              nperm = 5,
                              pattern = "MNAR", 
                              missing_prob = 0.4, 
                              imputation_method = "mice")


lvls_ordered <- res_imp_mice$pfi[order(pfi), feature]
res_imp_mice$pfi[, feature := factor(feature, levels = lvls_ordered)]
ggplot(res_imp_mice$pfi, aes(x = feature, y = pfi)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept = 0, col = "red") + 
  coord_flip() + 
  theme_bw()

lvls_ordered <- res_imp_mice$shap[order(shap), feature]
res_imp_mice$shap[, feature := factor(feature, levels = lvls_ordered)]
ggplot(res_imp_mice$shap, aes(x = feature, y = shap)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept = 0, col = "red") + 
  coord_flip() + 
  theme_bw()
