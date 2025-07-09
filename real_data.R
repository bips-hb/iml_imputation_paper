devtools::load_all()

library(iml.relating)
library(data.table)
library(xgboost)

set.seed(2025)

pattern <- "MCAR"
missing_prob <- 0.4
nrefits <- 15
nperm <- 20

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
                           nrefits = nrefits, 
                           nperm = nperm,
                           pattern = "none", 
                           missing_prob = 0, 
                           imputation_method = "none")

# With mean imputation
res_imp_mean <- bootstrap_cis(model_fun = model_fun, 
                              data = dat, 
                              nrefits = nrefits, 
                              nperm = nperm,
                              pattern = pattern, 
                              missing_prob = missing_prob, 
                              imputation_method = "mean")

mse_mean <- mean(res_imp_mean$perf$mse) #mean((pred - test_dat$y)^2)
rsq_mean <- 1 - mse_mean / var(test_dat$y)

# # With missForest imputation
#res_imp_mf <- bootstrap_cis(model_fun = model_fun, 
#                               data = dat, 
#                               nrefits = nrefits, 
#                               nperm = nperm,
#                               pattern = pattern, 
#                               missing_prob = missing_prob, 
#                               imputation_method = "missForest")

# With multiple imputation
res_imp_mice <- bootstrap_cis(model_fun = model_fun, 
                              data = dat, 
                              nrefits = nrefits, 
                              nperm = nperm,
                              pattern = pattern, 
                              missing_prob = missing_prob, 
                              imputation_method = "mice")
# Plot PFI together
res_pfi <- rbind(data.table(imputation = "complete", res_cmplt$pfi), 
                 data.table(imputation = "mean", res_imp_mean$pfi),
                # data.table(imputation = "missForest", res_imp_mf$pfi),
                 data.table(imputation = "mice", res_imp_mice$pfi))
#res_pfi[, imputation := factor(imputation, levels = c("complete", "mean", "missForest", "mice"))]
lvls_ordered <- res_cmplt$pfi[order(pfi), feature]
res_pfi[, feature := factor(feature, levels = lvls_ordered)]
plot_pfi <- ggplot(res_pfi, aes(x = feature, y = pfi, color = imputation)) + 
  geom_point(position=position_dodge2(width=1, reverse = TRUE)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position=position_dodge2(width=1, reverse = TRUE)) + 
  geom_hline(yintercept = 0, col = "red") + 
  coord_flip() + 
  theme_bw(base_size = 15)+
  theme(legend.position = c(0.7, 0.2), legend.text = element_text(size = 12))+
  labs(x=NULL, y="PFI")
plot_pfi
# Plot SHAP together
res_shap <- rbind(data.table(imputation = "complete", res_cmplt$shap), 
                 data.table(imputation = "mean", res_imp_mean$shap),
               #  data.table(imputation = "missForest", res_imp_mf$shap),
                 data.table(imputation = "mice", res_imp_mice$shap))
#res_pfi[, imputation := factor(imputation, levels = c("complete", "mean", "missForest", "mice"))]
lvls_ordered <- res_cmplt$shap[order(shap), feature]
res_shap[, feature := factor(feature, levels = lvls_ordered)]
plot_shap <- ggplot(res_shap, aes(x = feature, y = shap, color = imputation)) + 
  geom_point(position=position_dodge2(width=1, reverse = TRUE)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position=position_dodge2(width=1, reverse = TRUE)) + 
  geom_hline(yintercept = 0, col = "red") + 
  coord_flip() + 
  theme_bw(base_size = 15)+
  theme(legend.position = c(0.7, 0.2), legend.text = element_text(size = 12))+
  labs(x = NULL, y="SHAP")
plot_shap
# Plot PDP together
res_pdp <- rbind(data.table(imputation = "complete", res_cmplt$pdp), 
                 data.table(imputation = "mean", res_imp_mean$pdp),
                # data.table(imputation = "missForest", res_imp_mf$pdp),
                data.table(imputation = "mice", res_imp_mice$pdp))
#res_pdp[, imputation := factor(imputation, levels = c("complete", "mean", "missForest", "mice"))]
p1 <- ggplot(res_pdp[feature == "alcohol"], aes(x = feature_value, y = mpdp, color = imputation, fill = imputation)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  xlab("Alcohol") + ylab("Wine Quality") +
  theme_bw(base_size = 15)+
  theme(legend.position = c(0.7, 0.2))  
p2 <- ggplot(res_pdp[feature == "volatile.acidity"], aes(x = feature_value, y = mpdp, color = imputation, fill = imputation)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  xlab("Volatile Acidity") + ylab("PD") +
  theme_bw(base_size = 15)
p1 + p2


plot_real <- plot_pfi + plot_shap +p1 

ggsave("plots_paper/real_data.pdf", width = 210, height = 100, units = "mm", scale = 1.5)

#compare order
#PFI
res_imp_mean$pfi[order(pfi, decreasing = TRUE), feature]
res_imp_mice$pfi[order(pfi, decreasing = TRUE), feature]
res_cmplt$pfi[order(pfi, decreasing = TRUE), feature]

res_imp_mice$pfi[order(pfi, decreasing = TRUE), feature] == res_cmplt$pfi[order(pfi, decreasing = TRUE), feature]
res_imp_mean$pfi[order(pfi, decreasing = TRUE), feature] == res_cmplt$pfi[order(pfi, decreasing = TRUE), feature]

#SHAP
res_imp_mean$shap[order(shap, decreasing = TRUE), feature]
res_imp_mice$shap[order(shap, decreasing = TRUE), feature]
res_cmplt$shap[order(shap, decreasing = TRUE), feature]

res_cmplt$shap[order(shap, decreasing = TRUE), feature] == res_cmplt$pfi[order(pfi, decreasing = TRUE), feature]
res_imp_mean$shap[order(shap, decreasing = TRUE), feature] == res_cmplt$shap[order(shap, decreasing = TRUE), feature]
res_imp_mice$shap[order(shap, decreasing = TRUE), feature] == res_cmplt$shap[order(shap, decreasing = TRUE), feature]
