# Wed Nov 15 17:23:15 2017 ------------------------------
# Clean scripts for data extraction and analysis
# Bret et al. 2017
# Right-Wing Authoritarianism Predicts weakened Attitude Change in an Evaluative Counter-conditioning Paradigm
# Script by A. Bret & B. Beffara
# OSF : https://osf.io/vz78f/
# Preprint : https://psyarxiv.com/3mfa8/
# Questions, criticisms or comments at brice.beffara@slowpen.science or amelire.bret@slowpen.science

## References and background
# Most parts of this work are inspired/motivated by/based on these great tools/tutorials. Have a look !
# https://sites.google.com/site/doingbayesiandataanalysis/
# https://mvuorre.github.io/post/2017/better-brms-forest-plots/
# https://cran.r-project.org/web/packages/brms/index.html
# http://xcelab.net/rm/statistical-rethinking/
# http://www.barelysignificant.com/post/icc/
# http://www.nicebread.de/research/bayes/index.php
# http://www.bayesianspectacles.org/

# Big thank to Ladislas Nalborczyk for his usefull comments on our analysis
# http://www.barelysignificant.com

##########################################################
## Theses scripts are organized in sub-sections
# script_00_general.R (this script)
# script_01_create_dfs.R
# script_02_data_loading.R
# script_03_data_extraction.R
# script_04_models.R
# script_05_HDI_plots
# script_06_forest_plots
## They must be run linearly at least once to work well
##########################################################


## !!! READ ALL THE COMMENTS BEFORE RUNNING CODES !!!
## !!! SOME ACTIONS HAVE AN IRREVERSIBLE IMPACT !!!

# These scripts contain the Bayesian (but not frequentist) data analysis performed for the paper
# These analysis only deal with factors common to all studies: RWA levels and Valence of conditioning
# ... in an integrative data analysis perspective
# For detailled (frequentist) analysis see the other folder in the OSF plateform

# Optional generic preliminaries:

graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 

# Loading packages needed (and installing if necessary) for this part

if (!"pacman" %in% installed.packages()[,"Package"] ) install.packages("pacman" )
pacman::p_load(readr,
               reshape2,
               dplyr,
               brms,
               BEST,
               data.table,
               grid,
               ggplot2,
               Rcpp,
               Matrix,
               rprime,
               cowplot)

# Be assured that all the files containing data and all the .R scripts are located in the same folder, along with The .Rproj file
# We get the current working directory and it will be used to load the data

curwd <- dirname(rstudioapi::getActiveDocumentContext()$path)

# script 01 -> create data frames

# Wed Nov 22 17:07:52 2017 this part is not fully functional yet
# do not run
# When ready, run only if you want to create new .csv file from original data
# source(script_01_create_dfs.R)

# script 02 -> data loading

source(script_02_data_loading.R)

# script 03 -> data extraction

source(script_03_data_extraction.R)

# script 04 -> models
# !!!! Very important computation time !!!!!
# You may want to run each model separately

source(script_04_models.R)

# script 05 -> HDI plots
# ! This is a generic script !
# ! Replace MoDeLNaMe by the name of the model of which you want to plot parameters !

#source(script_05_HDI_plots)

# script 06 -> Forest plots
# Change the name of the models if you want to plot simple slope parameters
# Change the number of models if you want to produce plots from IDA3
# You may need to change some parameters of the plots in order to get correct aesthetic

#source(script_06_forest_plots)