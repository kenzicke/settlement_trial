################################################################################
# PAM pipeline
################################################################################
#
# Kenzie M. Cooke
# kmc390@miami.edu
# 10.14.25
#
# Data import and cleaning pipeline for PAM data.
#
################################################################################

# Load packages
library(tidyverse)

pam_files <- list.files("data/raw/pam/02_pam/data/", pattern = ".csv$", full.names = TRUE)

map_df()
