##Structure OLS analysis
#1: Regression run
#2: Newey-West standart errors
#3: p-value b1=b2
##Structure IV analysis (additional)
#4:First stage regression
#5:Instrumental F-statistic with p-value

###Regression analysis - Finland
Clean-------------------------------------------------------------------------------------------------------------------------------------------------------#
dev.off()     #Close plot
rm(list=ls()) #Remove all variables from environment
p_unload(all) #Unload all user packages
cat("\014")   #Clean console
#------------------------------------------------------------------------------------------------------------------------------------------------------------#
#install.packages("pacman")
require(pacman)     
#Load packages
p_load(
  rio,        #input and output: import()- & export()-function
  sandwich,   #statistical analysis: NeweyWest()
  AER,        #package for "Applied Econometrics in R": ivreg()-function
  stargazer,  #regression tables: stargazer()-function
  car,        #linearHypothesis()-function
  gmm         #GMM for multiple endogenous variables
  )
#Set working directory
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
#------------------------------------------------------------------------------------------------------------------------------------------------------------#
#Load data
#reg.data <- import("output_reg_data/se_reg_file_excise.csv")      #Load prepared data (Finland)
#reg.data <- import("output_reg_data/se_reg_file_carbon.csv")      #Load prepared data (Finland)
#reg.data <- import("output_reg_data/se_reg_file_excise_exVAT.csv")      #Load prepared data (Finland)
#reg.data <- import("output_reg_data/se_reg_file_excise_fullVAT.csv")      #Load prepared data (Finland)
reg.data <- import("output_reg_data/no_reg_file.csv")      #Load prepared data (Finland)
#reg.data <- import("output_reg_data/fi_reg_file.csv")      #Load prepared data (Finland)
reg.data <- reg.data[1:28,]                            #Reduce time frame to 1978-2005 for Norway        
#reg.data <- reg.data[1:38,]                            #Reduce time frame to 1978-2015        
#Regression specification
colnames(reg.data) <- c("year", "log_gas_cons", "gas_exexcise_with_vat", "gas_excisetax_with_vat", "d_carbontax", "t", "real_gdp_cap_1000", "urban_pop", "unemploymentrate", "real_oil_price_sek", "real_taxdiff")  #, "real_pricediff")


##Regression-------------------------------------------------------------------------------------------------------------------------------------------------#
##Linear regression with Newey-West heteroskedasticity and auto-correlation robust standart errors
#OLSrun 0
OLS.reg.0 <- lm(log_gas_cons ~ gas_exexcise_with_vat + gas_excisetax_with_vat, data = reg.data)
NW.reg.0 <- NeweyWest(OLS.reg.0,                 #Define regression input
                     lag=NULL,                   #Define number of lags
                     prewhite = FALSE,           #Prewhitening
                     adjust=TRUE)                #Adjustment
se.reg.0 <- coeftest(OLS.reg.0,vcov=NW.reg.0)    #Calc coefficient intervals and standart errors
se.reg.0 <- se.reg.0[,2]
#OLS 0-Stats 
test.OLS.reg.0 <- linearHypothesis(model=OLS.reg.0, 
                                   hypothesis.matrix="gas_exexcise_with_vat=gas_excisetax_with_vat", 
                                   test="F", 
                                   vcov.=NW.reg.0)
p.test.OLS.reg.0 <- round(test.OLS.reg.0[2,4], digits=2)

# #OLSrun 1
# OLS.reg.1 <- lm(log_gas_cons ~ gas_exexcise_with_vat + gas_excisetax_with_vat + d_carbontax + t ,data = reg.data)         
# NW.reg.1 <- NeweyWest(OLS.reg.1, 
#                      lag=NULL,
#                      prewhite = FALSE,
#                      adjust=TRUE)
# se.reg.1 <- coeftest(OLS.reg.1,vcov=NW.reg.1)
# se.reg.1 <- se.reg.1[,2]
# test.OLS.reg.1 <- linearHypothesis(model=OLS.reg.1, hypothesis.matrix="gas_exexcise_with_vat = gas_excisetax_with_vat", test="F", vcov.=NW.reg.1)
# p.test.OLS.reg.1 <- round(test.OLS.reg.1[2,4], digits=2)
# 
# #OLSrun 2
# OLS.reg.2 <- lm(log_gas_cons ~ gas_exexcise_with_vat + gas_excisetax_with_vat + d_carbontax + t + real_gdp_cap_1000 ,data = reg.data) 
# NW.reg.2 <- NeweyWest(OLS.reg.2, lag=NULL, prewhite = FALSE, adjust=TRUE)
# se.reg.2 <- coeftest(OLS.reg.2,vcov=NW.reg.2)
# se.reg.2 <- se.reg.2[,2]
# test.OLS.reg.2 <- linearHypothesis(model=OLS.reg.2, hypothesis.matrix="gas_exexcise_with_vat=gas_excisetax_with_vat", test="F", vcov.=NW.reg.2)
# p.test.OLS.reg.2 <- round(test.OLS.reg.2[2,4], digits=2)
# 
# #OLSrun 3
# OLS.reg.3 <- lm(log_gas_cons ~ gas_exexcise_with_vat + gas_excisetax_with_vat + d_carbontax +  t + t + real_gdp_cap_1000 + urban_pop ,data = reg.data)
# NW.reg.3 <- NeweyWest(OLS.reg.3, lag=NULL, prewhite = FALSE, adjust=TRUE)
# se.reg.3 <- coeftest(OLS.reg.3,vcov=NW.reg.3)
# se.reg.3 <- se.reg.3[,2]
# test.OLS.reg.3 <- linearHypothesis(model=OLS.reg.3, hypothesis.matrix="gas_exexcise_with_vat=gas_excisetax_with_vat", test="F", vcov.=NW.reg.3)
# p.test.OLS.reg.3 <- round(test.OLS.reg.3[2,4], digits=2)          

#OLSrun 4
OLS.reg.4 <- lm(log_gas_cons ~ gas_exexcise_with_vat + gas_excisetax_with_vat + t + real_gdp_cap_1000 + urban_pop + unemploymentrate ,data = reg.data) 
NW.reg.4 <- NeweyWest(OLS.reg.4, lag=NULL, prewhite = FALSE, adjust=TRUE)
se.reg.4 <- coeftest(OLS.reg.4,vcov=NW.reg.4)
se.reg.4 <- se.reg.4[,2]
test.OLS.reg.4 <- linearHypothesis(model=OLS.reg.4, hypothesis.matrix="gas_exexcise_with_vat=gas_excisetax_with_vat", test="F", vcov.=NW.reg.4)
p.test.OLS.reg.4 <- round(test.OLS.reg.4[2,4], digits=2)


###Instrumented variables---------------------------------------------------------------------------------------------------------------------------------------#
#Outcome variable: Gasoline consumption per capita
#Endogenous variables: (Ex-tax) gasoline price
#Exogenous variables: carbonprice, dummy, (Controlvariables: trend, gdp, urbanpop, unemployment rate)
#Instrumental variables: Oilprice, Gasoline-Diesel-Difference
# 
# #IVrun 1  -  (reg4+IVoil) 
# IV.reg.1 <- ivreg(log_gas_cons ~ gas_exexcise_with_vat + gas_excisetax_with_vat + t + real_gdp_cap_1000 + urban_pop + unemploymentrate | 
#                                                          gas_excisetax_with_vat + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_oil_price_sek,
#                  data=reg.data)
# NW.IV.reg.1 <- NeweyWest(IV.reg.1, lag=NULL, prewhite = FALSE, adjust=FALSE) 
# se.IV.reg.1 <- coeftest(IV.reg.1,vcov=NW.IV.reg.1)
# se.IV.reg.1 <- se.IV.reg.1[,2]
# 
# test.IV.reg.1 <- linearHypothesis(model=IV.reg.1,  hypothesis.matrix="gas_exexcise_with_vat=gas_excisetax_with_vat", test="Chisq", vcov.=NW.IV.reg.1)
# p.test.IV.reg.1 <- round(test.IV.reg.1[2,4], digits=2)
# #IV1 - Stats
# fs.IV.reg.1 <- lm(gas_exexcise_with_vat~gas_excisetax_with_vat + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_oil_price_sek, data = reg.data)  
# fs.NW.reg.1 <- NeweyWest(fs.IV.reg.1, lag=NULL, prewhite = FALSE, adjust=TRUE)
# stat.IV.reg.1 <- waldtest(fs.IV.reg.1, gas_exexcise_with_vat~gas_excisetax_with_vat + t + real_gdp_cap_1000 + urban_pop + unemploymentrate,  
#                          vcov=fs.NW.reg.1,
#                          test="F")   
# F.IV.reg.1 <- stat.IV.reg.1$F[2]
# F.IV.reg.1 <- round(F.IV.reg.1, digits=2)
# pF.IV.reg.1 <- stat.IV.reg.1[2,4]
# pF.IV.reg.1 <- round(pF.IV.reg.1, digits=2)

#  
#  #IVrun 2  -  (reg4+Gasoline-Diesel-Price-Difference & Oilprice)     --    ex-tax
#  IV.reg.2 <- ivreg(log_gas_cons ~ gas_exexcise_with_vat + gas_excisetax_with_vat + t + real_gdp_cap_1000 + urban_pop + unemploymentrate |
#                                                           gas_excisetax_with_vat + t + real_gdp_cap_1000 + urban_pop + unemploymentrate  + real_pricediff,
#                   data=reg.data)
#  NW.IV.reg.2 <- NeweyWest(IV.reg.2, lag=NULL, prewhite = FALSE, adjust=FALSE) 
#  se.IV.reg.2 <- coeftest(IV.reg.2,vcov=NW.IV.reg.2)
#  se.IV.reg.2 <- se.IV.reg.2[,2]
#  
#  test.IV.reg.2 <- linearHypothesis(model=IV.reg.2,  hypothesis.matrix="gas_exexcise_with_vat=gas_excisetax_with_vat", test="Chisq", vcov.=NW.IV.reg.2)   #Durbin-Wu-Hausman test
#  p.test.IV.reg.2 <- round(test.IV.reg.2[2,4], digits=2)
# #First stage regression for endogenous varibales
#  fs.IV.reg.2 <- lm(gas_exexcise_with_vat~ t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_pricediff, data = reg.data)  
#  fs.NW.reg.2 <- NeweyWest(fs.IV.reg.2, lag=NULL, prewhite = FALSE, adjust=TRUE)
#  stat.IV.reg.2 <- waldtest(fs.IV.reg.2, .~.-real_pricediff , vcov=fs.NW.reg.2, test="F")   #Stock-Yogo-Test
#  F.IV.reg.2 <- stat.IV.reg.2$F[2]
#  F.IV.reg.2 <- round(F.IV.reg.2, digits=2)
#  pF.IV.reg.2 <- stat.IV.reg.2[2,4]
#  pF.IV.reg.2 <- round(pF.IV.reg.2, digits=2)
#  
# 
#  #IVrun 3  -  (reg4+Gasoline-Diesel-Price-Difference)    --   tax
#  IV.reg.3 <- ivreg(log_gas_cons ~ gas_exexcise_with_vat + gas_excisetax_with_vat + t + real_gdp_cap_1000 + urban_pop + unemploymentrate | 
#                                   gas_exexcise_with_vat                          + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_taxdiff,
#                    data=reg.data)
#  NW.IV.reg.3 <- NeweyWest(IV.reg.3, lag=NULL, prewhite = FALSE, adjust=FALSE) 
#  se.IV.reg.3 <- coeftest(IV.reg.3,vcov=NW.IV.reg.3)
#  se.IV.reg.3 <- se.IV.reg.3[,2]
#  
#  test.IV.reg.3 <- linearHypothesis(model=IV.reg.3,  hypothesis.matrix="gas_exexcise_with_vat=gas_excisetax_with_vat", test="Chisq", vcov.=NW.IV.reg.3)   #Durbin-Wu-Hausman test
#  p.test.IV.reg.3 <- round(test.IV.reg.3[2,4], digits=2)
#  #First stage regression for endogenous varibales
#  fs.IV.reg.3 <- lm(gas_excisetax_with_vat~  gas_exexcise_with_vat + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_taxdiff, data = reg.data)  
#  fs.NW.reg.3 <- NeweyWest(fs.IV.reg.3, lag=NULL, prewhite = FALSE, adjust=TRUE)
#  stat.IV.reg.3 <- waldtest(fs.IV.reg.3, .~.-real_taxdiff , vcov=fs.NW.reg.3, test="F")   #Stock-Yogo-Test
#  F.IV.reg.3 <- stat.IV.reg.3$F[2]
#  F.IV.reg.3 <- round(F.IV.reg.3, digits=2)
#  pF.IV.reg.3 <- stat.IV.reg.3[2,4]
#  pF.IV.reg.3 <- round(pF.IV.reg.3, digits=2)
#  stargazer(fs.IV.reg.1, fs.IV.reg.2, fs.IV.reg.3, se=c(list(fs.NW.reg.1), list(fs.NW.reg.2), list(fs.NW.reg.3)),
#       #     add.lines = list(c("Instrument F-Statistics", 
#                              type="text", style="aer", 
#            star.char = c("*", "**"),
#            star.cutoffs = c(0.05, 0.01),
#            notes = c("Notes: + p<0.01; * p<0.05"), 
#            out="output_reg_tables/regression_results_IV_FI.txt")

 #---------------------------------------------------------------REGRESSION-TABLE--------------------------------------------------------------------------------------#
 ###Tidy regression table with stargazer#
 stargazer(#Regression runs
   OLS.reg.0,
   OLS.reg.4,
   #Newey-West standart errors
   se = c(list(se.reg.0),  #OLS0 
          list(se.reg.4)  #OLS4 
   ),       
   c("p-value b1=b2",
     p.test.OLS.reg.0,  #OLS0
     p.test.OLS.reg.4  #OLS4
   ),   
   #Labels
   title="Estimation Results from Gasoline Consumption Regressions for Finland",
   column.labels=c("OLS1",
                   "OLS2"
   ),
   covariate.labels=c("Ex-Tax",
                      "Tax",
                      "Trend",
                      "GDP per capita",
                      "Urban population",
                      "Unemployment rate"), 
   #Table settings
   keep.stat = c("n", "adj.rsq", "rsq"),
   digits = 2,
   type="text",     #"latex"=LaTex (default), "text"=ASCII
   style="aer",     #test: aer, asq, qje, all, all2, default
   table.layout = "l-d-o-#c=t=a-s=n",      #https://rdrr.io/cran/stargazer/man/stargazer_table_layout_characters.html
   #          table.placement = "!htbp",
   #          dep.var.caption = "",
   dep.var.labels= "Gasoline Consumption",
   model.names = FALSE,
   align=TRUE,
   #          digits.extra
   colnames = FALSE,
   font.size = "small",
   header = TRUE,
   #   notes = c("", ""),
   notes.align = "l",         #align notes: "l"-left, "c"-center, "r"-right
   #          notes.append =FALSE,       #p-threshold is suppressed when false
   notes.label ="",            #change name of notes section
   star.char = c("*", "+"),
   star.cutoffs = c(0.05, 0.01),
   notes = c("Notes: + p<0.01; * p<0.05"),
   out="output_reg_tables/regression_results_NO.txt"
 )
 