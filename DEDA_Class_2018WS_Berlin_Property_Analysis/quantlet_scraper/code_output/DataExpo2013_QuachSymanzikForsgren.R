# Soul of the Community: An Attempt to Assess Attachment to a Community
# Anna Quach, Juergen Symanzik, and Nicole Forsgren
# 3/19/2015
# 
# To produce a pdf file of the manuscript follow these steps:
# 
# 1) Set your working directory to where all the R scripts are saved.
# 1) Run 01_data_cleaning.R first to produce the final data set which most of 
#    the scripts will depend on.
# 2) Run any of these scripts in any order to produce the figures in the 
#    manuscript:
#   
# 02_remove_cases_and_variables_heatmap.R
# 03_create_map_and_sample_size_chart.R
# 04_dotchart_top_3_variables.R
# 05_rf_all_communities.R ***Takes hours to run!!!
# 06_rf_and_pcp_gary.R
# 
# These two scripts don't need to be ran in order to produce the manuscript, but 
# the results were used to create some of the tables:
# 
# tune_parameters.R
# PCA_index_vars_derivation.R
# 
# *Note that there is no code made available to produces results for the 
# Archetypal Analysis section due to privacy reasons. The figures are provided 
# instead.
# 
# 3) Run Soul_of_the_Community.rnw in RStudio. Make sure this file is saved in 
# the same folder where the figures are produced.

source("01_data_cleaning.R")

source("02_remove_cases_and_variables_heatmap.R")

# Install the ggmap and maps packages
source("03_create_map_and_sample_size_chart.R")

# Install the ggplot2 and plyr packages
source("04_dotchart_top_3_variables.R")

# Install the randomForest, plyr, RColorBrewer, and ggplot2 packages
source("05_rf_all_communities.R") 

# Install the RColorBrewer, GGally, randomForest, plyr, and ggplot2 packages
source("06_rf_and_pcp_gary.R")
