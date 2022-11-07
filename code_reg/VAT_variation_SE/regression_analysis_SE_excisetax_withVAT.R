##Structure OLS analysis
#1: Regression run
#2: Newey-West standart errors
#3: p-value b1=b2
##Structure IV analysis (additional)
#4:First stage regression
#5:Instrumental F-statistic with p-value
##Clean-------------------------------------------------------------------------------------------------------------------------------####
dev.off()     #Close plot
rm(list=ls()) #Remove all variables from environment
p_unload(all) #Unload all user packages
#cat("\014")   #Clean console
#-----------------------------------HEAD----------------------------------------------------------------------------------------------------------------#
#install.packages("pacman")
require(pacman)     
#Load packages
p_load(
  rio,        #input and output: import()- & export()-function
  sandwich,   #statistical analysis: NeweyWest()
  AER,        #package for "Applied Econometrics in R": ivreg()-function
  stargazer,  #regression tables: stargazer()-function
  car         #linearHypothesis()-function
  )
#Set working directory
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#Load data
reg.data <- import("output_reg_data/se_reg_file_excise_fullVAT.csv")      #Load prepared data (Sweden)
reg.data <- reg.data[1:38,]                                       #Reduce time frame to 1978-2011           

require(dplyr)
reg.data <- mutate(reg.data, energycarbontax = gas_energytax + gas_carbontax)

##Regression--------------------------------------------------------------------------------------------------------------------------------------------------------------------#
##Linear regression with Newey-West heteroskedasticity and auto-correlation robust standart errors
#OLSrun 0
OLS.reg.0 <- lm(log_gas_cons ~ extaxprice_lessVAT + gas_totaltax, data = reg.data)
NW.reg.0 <- NeweyWest(OLS.reg.0,                 #Define regression input
                     lag=NULL,                   #Define number of lags
                     prewhite = FALSE,           #Prewhitening
                     adjust=TRUE)                #Adjustment
se.reg.0 <- coeftest(OLS.reg.0,vcov=NW.reg.0)    #Calc coefficient intervals and standart errors
se.reg.0 <- se.reg.0[,2]
#OLS 0-Stats 
test.OLS.reg.0 <- linearHypothesis(model=OLS.reg.0, 
                                   hypothesis.matrix="extaxprice_lessVAT=gas_totaltax", 
                                   test="F", 
                                   vcov.=NW.reg.0)
p.test.OLS.reg.0 <- round(test.OLS.reg.0[2,4], digits=3)

#OLSrun 1
OLS.reg.1 <- lm(log_gas_cons ~ extaxprice_lessVAT + gas_totaltax + dummy + t ,data = reg.data)         
NW.reg.1 <- NeweyWest(OLS.reg.1, 
                     lag=NULL,
                     prewhite = FALSE,
                     adjust=TRUE)
se.reg.1 <- coeftest(OLS.reg.1,vcov=NW.reg.1)
se.reg.1 <- se.reg.1[,2]
test.OLS.reg.1 <- linearHypothesis(model=OLS.reg.1, hypothesis.matrix="extaxprice_lessVAT = gas_totaltax", test="F", vcov.=NW.reg.1)
p.test.OLS.reg.1 <- round(test.OLS.reg.1[2,4], digits=3)

#OLSrun 2
OLS.reg.2 <- lm(log_gas_cons ~ extaxprice_lessVAT + gas_totaltax + dummy + t + real_gdp_cap_1000 ,data = reg.data) 
NW.reg.2 <- NeweyWest(OLS.reg.2, lag=NULL, prewhite = FALSE, adjust=TRUE)
se.reg.2 <- coeftest(OLS.reg.2,vcov=NW.reg.2)
se.reg.2 <- se.reg.2[,2]
test.OLS.reg.2 <- linearHypothesis(model=OLS.reg.2, hypothesis.matrix="extaxprice_lessVAT=gas_totaltax", test="F", vcov.=NW.reg.2)
p.test.OLS.reg.2 <- round(test.OLS.reg.2[2,4], digits=3)

#OLSrun 3
OLS.reg.3 <- lm(log_gas_cons ~ extaxprice_lessVAT + gas_totaltax + dummy + t + real_gdp_cap_1000 + urban_pop ,data = reg.data)
NW.reg.3 <- NeweyWest(OLS.reg.3, lag=NULL, prewhite = FALSE, adjust=TRUE)
se.reg.3 <- coeftest(OLS.reg.3,vcov=NW.reg.3)
se.reg.3 <- se.reg.3[,2]
test.OLS.reg.3 <- linearHypothesis(model=OLS.reg.3, hypothesis.matrix="extaxprice_lessVAT=gas_totaltax", test="F", vcov.=NW.reg.3)
p.test.OLS.reg.3 <- round(test.OLS.reg.3[2,4], digits=3)          

#OLSrun 4
OLS.reg.4 <- lm(log_gas_cons ~ extaxprice_lessVAT + gas_totaltax + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate ,data = reg.data) 
NW.reg.4 <- NeweyWest(OLS.reg.4, lag=NULL, prewhite = FALSE, adjust=TRUE)
se.reg.4 <- coeftest(OLS.reg.4,vcov=NW.reg.4)
se.reg.4 <- se.reg.4[,2]
test.OLS.reg.4 <- linearHypothesis(model=OLS.reg.4, hypothesis.matrix="extaxprice_lessVAT=gas_totaltax", test="F", vcov.=NW.reg.4)
p.test.OLS.reg.4 <- round(test.OLS.reg.4[2,4], digits=3)


###Instrumented variables----------------------------------------------------------------------------------------------------------------------------------------------------------#
#Outcome variable: Gasoline consumption per capita
#Endogenous variables: (Ex-tax) gasoline price
#Exogenous variables: carbonprice, dummy, (Controlvariables: trend, gdp, urbanpop, unemployment rate)
#Instrumental variables: Oilprice, Energytax, Carbontax, Gasoline-Diesel-Difference

#IVrun 0  -  (reg4+IVoil) 
IV.reg.0 <- ivreg(log_gas_cons ~ extaxprice_lessVAT + gas_totaltax + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate | 
                                                                   gas_totaltax + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_oil_price_sek,
                  data=reg.data)
#bwNeweyWest()
NW.IV.reg.0 <- NeweyWest(IV.reg.0, lag=NULL, prewhite = FALSE, adjust=FALSE) 
se.IV.reg.0 <- coeftest(IV.reg.0,vcov=NW.IV.reg.0)
se.IV.reg.0 <- se.IV.reg.0[,2]

test.IV.reg.0 <- linearHypothesis(model=IV.reg.0,  hypothesis.matrix="extaxprice_lessVAT=gas_totaltax", test="Chisq", vcov.=NW.IV.reg.0)
p.test.IV.reg.0 <- round(test.IV.reg.0[2,4], digits=3)

#IV0 - Stats
fs.IV.reg.0 <- lm(extaxprice_lessVAT ~ gas_totaltax + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_oil_price_sek, data = reg.data)  
fs.NW.reg.0 <- NeweyWest(fs.IV.reg.0, lag=bwNeweyWest(fs.IV.reg.0), prewhite = FALSE, adjust=TRUE)
stat.IV.reg.0 <- waldtest(fs.IV.reg.0, .~.-real_oil_price_sek, 
                          vcov=fs.NW.reg.0,
                          test="F")   
F.IV.reg.0 <- stat.IV.reg.0$F[2]
F.IV.reg.0 <- round(F.IV.reg.0, digits=3)
pF.IV.reg.0 <- stat.IV.reg.0[2,4]
pF.IV.reg.0 <- round(pF.IV.reg.0, digits=3)

bwNeweyWest(fs.IV.reg.0)
###--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------###

# #IVrun 1  -  (reg4+IVenergytax) 
# IV.reg.1 <- ivreg(log_gas_cons ~ extaxprice_lessVAT + gas_totaltax + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate | 
#                                  extaxprice_lessVAT                          + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + energycarbontax,
#                  data=reg.data)
# NW.IV.reg.1 <- NeweyWest(IV.reg.1, lag=NULL, prewhite = FALSE, adjust=FALSE) 
# se.IV.reg.1 <- coeftest(IV.reg.1,vcov=NW.IV.reg.1)
# se.IV.reg.1 <- se.IV.reg.1[,2]
# 
# test.IV.reg.1 <- linearHypothesis(model=IV.reg.1,  hypothesis.matrix="extaxprice_lessVAT=gas_totaltax", test="Chisq", vcov.=NW.IV.reg.1)
# p.test.IV.reg.1 <- round(test.IV.reg.1[2,4], digits=3)
# 
# #IV1 - Stats
# fs.IV.reg.1 <- lm(gas_totaltax~extaxprice_lessVAT + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + energycarbontax, data = reg.data)
# fs.NW.reg.1 <- NeweyWest(fs.IV.reg.1, lag=bwNeweyWest(fs.IV.reg.1), prewhite = FALSE, adjust=TRUE)
# stat.IV.reg.1 <- waldtest(fs.IV.reg.1, .~.-energycarbontax, 
#                          vcov=fs.NW.reg.1,
#                          test="F")   
# F.IV.reg.1 <- stat.IV.reg.1$F[2]
# F.IV.reg.1 <- round(F.IV.reg.1, digits=3)
# pF.IV.reg.1 <- stat.IV.reg.1[2,4]
# pF.IV.reg.1 <- round(pF.IV.reg.1, digits=3)

###--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# #IVrun 2  -  (reg4+IVcarbontax) 
# IV.reg.2 <- ivreg(log_gas_cons ~ extaxprice_lessVAT + gas_totaltax + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate | 
#                                  extaxprice_lessVAT                         + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + gas_energytax,
#                   data=reg.data)
# NW.IV.reg.2 <- NeweyWest(IV.reg.2, lag=NULL, prewhite = FALSE, adjust=FALSE) 
# se.IV.reg.2 <- coeftest(IV.reg.2,vcov=NW.IV.reg.2)
# se.IV.reg.2 <- se.IV.reg.2[,2]
# 
# test.IV.reg.2 <- linearHypothesis(model=IV.reg.2,  hypothesis.matrix="extaxprice_lessVAT=gas_totaltax", test="Chisq", vcov.=NW.IV.reg.2)
# p.test.IV.reg.2 <- round(test.IV.reg.2[2,4], digits=3)
# 
# fs.IV.reg.2 <- lm(gas_totaltax~extaxprice_lessVAT + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + gas_energytax, data = reg.data)  
# fs.NW.reg.2 <- NeweyWest(fs.IV.reg.2, lag=NULL, prewhite = FALSE, adjust=TRUE)
# stat.IV.reg.2 <- waldtest(fs.IV.reg.2, .~.-gas_energytax, vcov=fs.NW.reg.2, test="F")   
# F.IV.reg.2 <- stat.IV.reg.2$F[2]
# F.IV.reg.2 <- round(F.IV.reg.2, digits=3)
# pF.IV.reg.2 <- stat.IV.reg.2[2,4]
# pF.IV.reg.2 <- round(pF.IV.reg.2, digits=3)

###--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#IVrun 3  -  (reg4+Gasoline-Diesel-Price-Difference) 
IV.reg.3 <- ivreg(log_gas_cons ~ extaxprice_lessVAT + gas_totaltax + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate | 
                    extaxprice_lessVAT + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_taxdiff_exVAT,
                data=reg.data)
NW.IV.reg.3 <- NeweyWest(IV.reg.3, lag=NULL, prewhite = FALSE, adjust=FALSE) 
se.IV.reg.3 <- coeftest(IV.reg.3,vcov=NW.IV.reg.3)
se.IV.reg.3 <- se.IV.reg.3[,2]

test.IV.reg.3 <- linearHypothesis(model=IV.reg.3,  hypothesis.matrix="extaxprice_lessVAT=gas_totaltax", test="Chisq", vcov.=NW.IV.reg.3)
p.test.IV.reg.3 <- round(test.IV.reg.3[2,4], digits=3)

fs.IV.reg.3 <- lm( gas_totaltax~ extaxprice_lessVAT + dummy + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_taxdiff_exVAT, data = reg.data)  
fs.NW.reg.3 <- NeweyWest(fs.IV.reg.3, lag=bwNeweyWest(fs.IV.reg.3), prewhite = FALSE, adjust=TRUE)
stat.IV.reg.3 <- waldtest(fs.IV.reg.3, .~.-real_taxdiff_exVAT, vcov=fs.NW.reg.3, test="F")   
F.IV.reg.3 <- stat.IV.reg.3$F[2]
F.IV.reg.3 <- round(F.IV.reg.3, digits=3)
pF.IV.reg.3 <- stat.IV.reg.3[2,4]
pF.IV.reg.3 <- round(pF.IV.reg.3, digits=3)
  

#---------------------------------------------------------------REGRESSION-TABLE--------------------------------------------------------------------------------------#
###Tidy regression table with stargazer#
stargazer(#Regression runs
          OLS.reg.0,
          OLS.reg.1, 
          OLS.reg.2, 
          OLS.reg.3, 
          OLS.reg.4,
          IV.reg.0,
#          IV.reg.1, 
#          IV.reg.2,
          IV.reg.3,
          #Newey-West standart errors
          se = c(list(se.reg.0),  #OLS0 
                 list(se.reg.1),  #OLS1 
                 list(se.reg.2),  #OLS2 
                 list(se.reg.3),  #OLS3 
                 list(se.reg.4),  #OLS4 
                 list(se.IV.reg.0),  #IV1
 #                list(se.IV.reg.1),  #IV1
#                 list(se.IV.reg.2),   #IV2
                 list(se.IV.reg.3)   #IV3
          ),    
          #Add further analysis (Instrumental F-statistic with p-value and p-value for b1=b2)
          add.lines = list(c("Instrument F-Statistics", 
                             "",  #OLS0
                             "",  #OLS1
                             "",  #OLS2
                             "",  #OLS3
                             "",  #OLS4
                             F.IV.reg.0,
#                             F.IV.reg.1,
#                             F.IV.reg.2,    
                             F.IV.reg.3     
                           ),                                 
                           c("p-value",
                             "",  #OLS0
                             "",  #OLS1
                             "",  #OLS2
                             "",  #OLS3
                             "",  #OLS4
                             pF.IV.reg.0, #IV0
 #                            pF.IV.reg.1,  #IV1                                                
 #                            pF.IV.reg.2,   #IV2
                             pF.IV.reg.3   #IV3
 
                           ),
                           c("p-value b1=b2",
                             p.test.OLS.reg.0,  #OLS0
                             p.test.OLS.reg.1,  #OLS1
                             p.test.OLS.reg.2,  #OLS2
                             p.test.OLS.reg.3,  #OLS3
                             p.test.OLS.reg.4,  #OLS4
                             p.test.IV.reg.0,    #IV0
#                             p.test.IV.reg.1,    #IV1
#                             p.test.IV.reg.2,     #IV2
                             p.test.IV.reg.3     #IV3
                           )
          ),   
          #Labels
          title="Estimation Results from Gasoline Consumption Regressions in Sweden: Excise Tax including full VAT",
          column.labels=c("OLS1",
                          "OLS2",
                          "OLS3",
                          "OLS4",
                          "OLS5",
                          "IV(OilPrice)",
 #                         "IV(Energy&Carbon-Tax)",
                          "IV(TaxDiff)"
          ),
          covariate.labels=c("Gasoline price excluding excise tax",
                             "Excise tax",
                             "Dummy 1990",
                             "Time trend",
                             "GDP per capita",
                             "Urban population",
                             "Unemployment rate"), 
          #Table settings
          keep.stat = c("n", "adj.rsq", "rsq"),
          digits = 4,
          type="text",     #"latex"=LaTex (default), "text"=ASCII
          style="aer",     #test: aer, asq, qje, all, all2, default
          table.layout = "o-#c=t=a-s=n",      #https://rdrr.io/cran/stargazer/man/stargazer_table_layout_characters.html
#          table.placement = "!htbp",
          dep.var.caption = "",
          dep.var.labels= "",
          model.names = FALSE,
          align=TRUE,
#          digits.extra
          colnames = FALSE,
          font.size = "tiny",
          header = FALSE,
          notes = c("Notes:", 
                    "Sources:"),
          notes.align = "l",         #align notes: "l"-left, "c"-center, "r"-right
          notes.append =FALSE,       #p-threshold is suppressed when false
          notes.label ="",            #change name of notes section
#          star.char ="",          #default:*, if "", not stars shown (only when p-values given)
           out="output_reg_tables/regression_results_SE_excisetax_withVAT.txt"    #for LaTex-file .tex
#          out.header=FALSE
)
