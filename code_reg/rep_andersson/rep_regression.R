###Regression analysis - Andersson
#Clean--------------------------------------------------------------------------------------------------------------------------------------------------#
dev.off()     #Close plot
rm(list=ls()) #Remove all variables from environment
p_unload(all) #Unload all user packages
cat("\014")   #Clean console
#Head---------------------------------------------------------------------------------------------------------------------------------------------------#
#install.packages("pacman")
require(pacman)      ##library(pacman)
#Load packages
p_load(
  rio,          #import() & export()-function
  sandwich,     #NeweyWest()
  AER,          #ivreg()-function
  stargazer     #for nice regression tables 
  # lmtest,     #coeftest()
  #  car        #linearHypothesis
  )
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#Load data
reg.data <- import("code_reg/rep_andersson/data/disentangling_regression_data.dta")    #Load regression data provided by Andersson

##Regression
##Linear regression with Newey-West heteroskedasticity and auto-correlation robust standart errors
#run 1
OLS.reg1 <- lm(log_gas_cons ~ real_carbontaxexclusive_with_vat + real_carbontax_with_vat + d_carbontax + t ,data = reg.data)         
NW.reg1 <- NeweyWest(OLS.reg1, 
                     lag=16,
                     prewhite = FALSE,
                     adjust=TRUE)
se.reg1 <- coeftest(OLS.reg1,vcov=NW.reg1)
se.reg1 <- se.reg1[,2]

#run 2
OLS.reg2 <- lm(log_gas_cons ~ real_carbontaxexclusive_with_vat + real_carbontax_with_vat + d_carbontax + t + real_gdp_cap_1000 ,data = reg.data) 
NW.reg2 <- NeweyWest(OLS.reg2, lag=16, prewhite = FALSE, adjust=TRUE)
se.reg2 <- coeftest(OLS.reg2,vcov=NW.reg2)
se.reg2 <- se.reg2[,2]

#run 3
OLS.reg3 <- lm(log_gas_cons ~ real_carbontaxexclusive_with_vat + real_carbontax_with_vat + d_carbontax + t + real_gdp_cap_1000 + urban_pop ,data = reg.data)
NW.reg3 <- NeweyWest(OLS.reg3, lag=16, prewhite = FALSE, adjust=TRUE)
se.reg3 <- coeftest(OLS.reg3,vcov=NW.reg3)
se.reg3 <- se.reg3[,2]

#run4
OLS.reg4 <- lm(log_gas_cons ~ real_carbontaxexclusive_with_vat + real_carbontax_with_vat + d_carbontax + t + real_gdp_cap_1000 + urban_pop + unemploymentrate ,data = reg.data) 
NW.reg4 <- NeweyWest(OLS.reg4, lag=16, prewhite = FALSE, adjust=TRUE)
se.reg4 <- coeftest(OLS.reg4,vcov=NW.reg4)
se.reg4 <- se.reg4[,2]

###Instrumented variables---------------------------------------------------------------------------------------------------------------------------------------##
#Outcome variable: Gasoline consumption per capita
#Endogenous variables: (Ex-tax) gasoline price
#Exogenous variables: carbonprice, dummy, (Controlvariables: trend, gdp, urbanpop, unemployment rate)
#Instrumental variables: Energyprice, Oilprice
#run 5  -  (reg4+IVenergy) 
IV.reg5 <- ivreg(log_gas_cons ~ real_carbontaxexclusive_with_vat + real_carbontax_with_vat + d_carbontax + t + real_gdp_cap_1000 + urban_pop + unemploymentrate | 
                                                                   real_carbontax_with_vat + d_carbontax + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_energytax_with_vat,
                 data=reg.data)
NW.reg5 <- NeweyWest(IV.reg5, lag=16, prewhite = FALSE, adjust=FALSE)
se.reg5 <- coeftest(IV.reg5,vcov=NW.reg5)
se.reg5 <- se.reg5[,2]

#run 6  -  (reg4+IVoil) 
IV.reg6 <- ivreg(log_gas_cons ~ real_carbontaxexclusive_with_vat + real_carbontax_with_vat + d_carbontax + t + real_gdp_cap_1000 + urban_pop + unemploymentrate | 
                                                                   real_carbontax_with_vat + d_carbontax + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_oil_price_sek,
                  data=reg.data)
NW.reg6 <- NeweyWest(IV.reg6, lag=16, prewhite = FALSE, adjust=FALSE) 
se.reg6 <- coeftest(IV.reg6,vcov=NW.reg6)
se.reg6 <- se.reg6[,2]

###------Instrumental F-Statistic with p-value---------------------------------------------------------------------------------------------------------------------------------------------###
#run5
fs.IV.reg5 <- lm(real_carbontaxexclusive_with_vat~real_carbontax_with_vat + d_carbontax + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_energytax_with_vat, data = reg.data)  
fs.NW.reg5 <- NeweyWest(fs.IV.reg5, lag=16, prewhite = FALSE, adjust=TRUE)
stat.IV.reg5 <- waldtest(fs.IV.reg5, .~.-real_energytax_with_vat, 
                         vcov=fs.NW.reg5,
                         test="F")
F.reg5 <- stat.IV.reg5$F[2]
F.reg5 <- round(F.reg5, digits=3)
pF.reg5 <- stat.IV.reg5[2,4]
pF.reg5 <- round(pF.reg5, digits=3)

#run6
fs.IV.reg6 <- lm(real_carbontaxexclusive_with_vat~real_carbontax_with_vat + d_carbontax + t + real_gdp_cap_1000 + urban_pop + unemploymentrate + real_oil_price_sek, data = reg.data)  
fs.NW.reg6 <- NeweyWest(fs.IV.reg6, lag=16, prewhite = FALSE, adjust=TRUE)
stat.IV.reg6 <- waldtest(fs.IV.reg6, .~.-real_oil_price_sek, 
                         vcov=fs.NW.reg6,
                         test="F")   
F.reg6 <- stat.IV.reg6$F[2]
F.reg6 <- round(F.reg6, digits=3)
pF.reg6 <- stat.IV.reg6[2,4]
pF.reg6 <- round(pF.reg6, digits=3)
###--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------###

##p-value: b1=b2 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
test.reg.1 <- linearHypothesis(model=OLS.reg1, 
                           hypothesis.matrix="real_carbontaxexclusive_with_vat = real_carbontax_with_vat",
                           test="F",
                           vcov.=NW.reg1)
test.reg.2 <- linearHypothesis(model=OLS.reg2, hypothesis.matrix="real_carbontaxexclusive_with_vat=real_carbontax_with_vat", test="F", vcov.=NW.reg2)
test.reg.3 <- linearHypothesis(model=OLS.reg3, hypothesis.matrix="real_carbontaxexclusive_with_vat=real_carbontax_with_vat", test="F", vcov.=NW.reg3)
test.reg.4 <- linearHypothesis(model=OLS.reg4, hypothesis.matrix="real_carbontaxexclusive_with_vat=real_carbontax_with_vat", test="F", vcov.=NW.reg4)
test.reg.5 <- linearHypothesis(model=IV.reg5,  hypothesis.matrix="real_carbontaxexclusive_with_vat=real_carbontax_with_vat", test="Chisq", vcov.=NW.reg5) 
test.reg.6 <- linearHypothesis(model=IV.reg6,  hypothesis.matrix="real_carbontaxexclusive_with_vat=real_carbontax_with_vat", test="Chisq", vcov.=NW.reg6)   

p.test.reg.1 <- round(test.reg.1[2,4], digits=3)
p.test.reg.2 <- round(test.reg.2[2,4], digits=3)
p.test.reg.3 <- round(test.reg.3[2,4], digits=3)          
p.test.reg.4 <- round(test.reg.4[2,4], digits=3)
p.test.reg.5 <- round(test.reg.5[2,4], digits=3)
p.test.reg.6 <- round(test.reg.6[2,4], digits=3)
###--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------###

#---------------------------------------------------------------REGRESSION-TABLE--------------------------------------------------------------------------------------#
###Tidy regression table with stargazer#
stargazer(OLS.reg1, OLS.reg2, OLS.reg3, OLS.reg4, IV.reg5, IV.reg6,   
          type="text",     #"latex"=LaTex (default), "text"=ASCII
          title="Estimation Results from Gasoline Consumption Regressions",   
          style="aer",     #test: aer, asq, qje, all, all2, default
          dep.var.caption = "",
          dep.var.labels= "",
          model.names = FALSE,
          column.labels=c("OLS","OLS","OLS","OLS","IV(EnTax)","IV(OilPrice)"),        
          covariate.labels=c("Ex-tax gasoline price with VAT",
                             "Carbon tax with VAT",
                             "Dummy carbon tax",
                             "Time trend",
                             "GDP per capita",
                             "Urban population",
                             "Unemployment rate"),
          #order=c()     
          align=TRUE,     
          se = c(list(se.reg1), list(se.reg2), list(se.reg3), list(se.reg4), list(se.reg5), list(se.reg6)),
          digits = 4,
          colnames = FALSE,
          font.size = "tiny",
          header = FALSE,
          keep.stat = c("n", "adj.rsq", "rsq"),
          #omit.stat = "",
          #star.char ="",       #default:*, if "", not stars shown (only when p-values given)
          #star.cutoffs=,
          add.lines = list(c("Instrument F-Statistics", "", "", "", "",F.reg5, F.reg6),                     
                           c("p-value", "", "", "", "", pF.reg5, pF.reg6),                                         
                           c("p-value b1=b2", p.test.reg.1, p.test.reg.2, p.test.reg.3, p.test.reg.4, p.test.reg.5, p.test.reg.6)), 
          notes = c("Notes:", 
                    "Sources:"),
          notes.align = "l",          #align notes: "l"-left, "c"-center, "r"-right
          notes.append =FALSE,        #p-threshold is suppressed when false
          notes.label ="",            #change name of notes section
          #object.names = FALSE       #shows  the name of regressions (default = FALSE)
          table.layout = "o-#c=t=a-s=n",      #https://rdrr.io/cran/stargazer/man/stargazer_table_layout_characters.html
          #table.placement = "!htbp",
          out="results/regression_rep_andersson_SE.txt"
          #out.header=FALSE
)