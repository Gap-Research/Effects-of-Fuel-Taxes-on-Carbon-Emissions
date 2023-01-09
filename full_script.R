###################################################
### Full script
###################################################

##Clean---------------------------------------------------------------------####
dev.off()     #Close plot
rm(list=ls()) #Remove all variables from environment
p_unload(all) #Unload all user packages
cat("\014")   #Clean console
##--------------------------------------------------------------------------####
require(pacman) 
#Load packages
p_load(svDialogs #pop-up window for variable input
)   
#Set working directory
path <- dlgInput("Enter the path to you code directory:", "C:/")$res
setwd(path)
##--------------------------------------------------------------------------####

#Synthetic Control Method
source("code/scm_results/scm_results.R")

source("code/scm_tests/scm_placebo_leave.R")
source("code/scm_tests/scm_placebo_space.R")
source("code/scm_tests/scm_placebo_time.R")
source("code/scm_tests/scm_placebo_vsfull.R")
source("code/scm_tests/scm_possible_confounders.R")


#Tax incidence and salience regression
source("code/tax_incidence_regression/tax_incidence_analysis_FI_final.R")
source("code/tax_salience_regression/regression_analysis_FI.R")

source("code/tax_incidence_regression/tax_incidence_analysis_SE_final.R")
source("code/tax_salience_regression/regression_analysis_SE.R")
source("code/tax_salience_regression/regression_analysis_SE_carbon.R")

source("code/tax_incidence_regression/tax_incidence_analysis_NO_final.R")
source("code/tax_salience_regression/regression_analysis_NO.R")

rm(list=ls()) #Remove all variables from environment

