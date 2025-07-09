# Imputation uncertainty in interpretable machine learning methods
by Pegah Golchian & Marvin N. Wright

This repository contains the code for the simulation study and real data example 
in the paper "Imputation uncertainty in interpretable machine learning methods" 
by P. Golchian, and M. Wright. It compares the effects of different imputation 
methods on the confidence interval coverage probabilities of the IML methods 
permutation feature importance (PFI), partial dependence (PD) plots and SHAP.

The repository contains: 
* Code for the confidence interval experiment (ci-experiment.R)
* Helper functions used in the scripts (folder ./R/)
* DESCRIPTION file describing the package dependencies
* Cluster setup (batchtools.conf.R)
* data (UCI wine quality dataset)

Plot results for different IML methods, missingness rates, missingness patterns, sampling strategy and imputation methods
* plots_paper file - contains all the plots mentioned in the paper
* plots_sim_example.R - coverage, width and bias for xgb, MAR, bootstrap
* real_data.R - PFI, PD and SHAP of UCI wine dataset example with different imputation methods 
* plots_paper_cov.R - coverage of all IML methods over model refits with (set parameter bootstrap or subsampling)
* plots_paper_bias.R - bias of all IML methods over different missingness rates for 15 refits (set parameter bootstrap or subsampling)
* plots_paper_width.R - width of all IML methods over different missingness rates for 15 refits (set parameter bootstrap or subsampling)
* plots-width-refits-adjusted.R - width of all IML methods over model refits > 4
* plots-performance.R - performance of IML methods with different imputation

Supplementary material:
* plots_paper file: All plots
* supplement_bootstrap.pdf: Supplementary material for the bootstrap sampling strategy (Coverage, average CI width, bias)
