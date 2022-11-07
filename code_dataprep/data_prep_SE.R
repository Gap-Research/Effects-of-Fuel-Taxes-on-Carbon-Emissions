####Data preperation - Sweden
##Clean-------------------------------------------------------------------------------------------------------------------------------####
dev.off()     #Close plot
rm(list=ls()) #Remove all variables from environment
p_unload(all) #Unload all user packages
cat("\014")   #Clean console
##Header------------------------------------------------------------------------------------------------------------------------------####
#install.packages("pacman")
require(pacman)     
#Load packages
p_load(rio,     #Input and Output: import()- & export()-function
      dplyr     #filter()-function
      )   
#Set working directory
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
##----------------------------------------------------------------------------------------------------------------------------------------#

#Currency exchange------------------------------------------------------------------------------------------------------------------------#
ex <- import("data_input/reg_exchange-rates/exrates_full.csv", dec=",")      #Load exchange rates
##Sweden adjusted prices
se.ex <- filter(ex, LOCATION=="SWE" & between(TIME, 1978, 2016))             #Cut exchange rates for country (Sweden) and time (1978-2016)


####Crude oil data------------------------------------------------------------------------------------------------------------------------#
###Nominal crude oil data from Energy Prices & Taxes (1973-2018)
##Sweden
#Load data
nominal.crude <- import("data_input/reg_crude-oil/crude_oil.csv", dec=",")   #Load crude oil data from Energy Prices & Taxes
#Cut data
nominal.crude.cut <- filter(nominal.crude[,1:2], between(year, 1978, 2016))  #Cut crude oil data for needed years
se.nominal.crude <- nominal.crude.cut[,2]*as.numeric(se.ex[,7])              #Calc nominal crude oil price in SEK (Swedish krona)


###Fuel price data------------------------------------------------------------------------------------------------------------------------#
###Nominal price data from Energy Prices & Taxes (1978-2018)
##Sweden
#Load data
se.nominal <- import("data_input/reg_fuel-prices-taxes/031119_se_data.csv", dec=",", sep=";", skip=1)      #Load fuel price data from Energy Prices & Taxes
#Cut data
se.nominal.cut <- se.nominal[1:39,1:18]                                              #row:39 is 2016 (gasoline consumption data only available until 2016)
se.nominal.tax <- se.nominal.cut[, c(1:8)]                                           #Cut nominal tax data
colnames(se.nominal.tax) <- c("year", "vat", "diesel_energytax", "diesel_carbontax", "gas_energytax", "gas_carbontax", "leaded_energytax", "leaded_carbontax")
se.nominal.diesel <- se.nominal.cut[, c(1,9:13)]                                     #Cut nominal diesel data
se.nominal.gas <- se.nominal.cut[, c(1,14:18)]                                       #Cut nominal gasoline price data


#VAT split for regression analysis
se.nominal.gas.0 <- mutate(se.nominal.gas, gas_totalprice_less_vat = gas_totalprice - gas_vat)  
se.nominal.gas.1 <- mutate(se.nominal.gas.0, share_excise = gas_excise_tax / gas_totalprice_less_vat)   
se.nominal.gas.2 <- mutate(se.nominal.gas.1, share_excisetax_exclusive = gas_extaxprice / gas_totalprice_less_vat)  
se.nominal.gas.3 <- mutate(se.nominal.gas.2, gas_excisetax_with_vat = gas_excise_tax + share_excise * gas_vat)            
se.nominal.gas.4 <- mutate(se.nominal.gas.3, gas_exexcise_with_vat = gas_extaxprice + share_excisetax_exclusive * gas_vat)
se.nominal.gas <- se.nominal.gas.4

#Add VAT to nominal prices
se.nominal.tax.1 <- mutate(se.nominal.tax, gas_carbontax_with_vat = gas_carbontax + gas_carbontax  * vat /100)
se.nominal.tax.2 <- mutate(se.nominal.tax.1, gas_energytax_with_vat = gas_energytax + gas_energytax * vat /100)

se.nominal.tax.final <- se.nominal.tax.2

###Prepare input data for tax incidence------------------------------ FINAL TAX INCIDENCE DATA (nominal data export)-----------------------------------#
se.tax.incidence <- cbind(se.nominal.gas[, c("gas_totalprice","gas_totalprice_less_vat", "gas_excise_tax", "gas_excisetax_with_vat")],       
                          se.nominal.tax.final[,c("gas_carbontax", "gas_energytax", "gas_carbontax_with_vat", "gas_energytax_with_vat")],
                          se.nominal.crude)          
colnames(se.tax.incidence) <- c("gas_totalprice", "gas_totalprice_less_vat", "gas_excise_tax", "gas_excisetax_with_vat", "gas_carbontax", "gas_energytax", "gas_carbontax_with_vat", "gas_energytax_with_vat", "crude")
export(se.tax.incidence, file = "output_reg_data/se_tax_incidence_data.csv", format = "csv")  
#-----------------------------------------------------------------------------------------------------------------------------l-------------------------#


###-------------------------------------------------------------------REGRESSION ALAYSIS DATA----------------------------------------------------------#
###CPI (Inflation) price adjustment---------------------Real price data -------------------------------------------------------------------------------#
#Reg: Prices and Taxes with adjustment###
cpi.2015 <- import("data_input/reg_cpi/CPI_Inflation_index2015_selected.csv", dec=",")      #Load CPI (Consumer price index) data from OECD
cpi.2015.cut <- cpi.2015[,c(1,6,7)]                                                         #Cut CPI data for LOCATION, TIME, Value

#Sweden
se.cpi.2015 <- filter(cpi.2015.cut, LOCATION=="SWE" & between(TIME, 1978, 2016))            #Filter for country (Sweden) and time (1978-2016)
#Set new base year - formula= newindex_t=100'(oldindex_t/oldindex_newbase)
se.cpi.2015.value.2005 <- filter(se.cpi.2015, TIME==2005)                                   #Filter index for new base year 2005
se.cpi.2005 <- as.numeric(se.cpi.2015[,3])/as.numeric(se.cpi.2015.value.2005[,3])           #Calc CPI for new base year 2005

#Calc real values (base=2005)
se.real.crude.2005 <- se.nominal.crude/se.cpi.2005         
se.real.tax.2005 <- se.nominal.tax.final/se.cpi.2005       
se.real.diesel.2005 <- se.nominal.diesel[,2:6]/se.cpi.2005   
se.real.gas.2005 <- se.nominal.gas[,2:11]/se.cpi.2005        

#Create final data
real_crude <- se.real.crude.2005
se.prices.bind <- cbind(se.nominal.tax.final, se.real.gas.2005, se.real.diesel.2005, real_crude)
se.prices.bind.1 <- mutate(se.prices.bind, real_taxdiff = gas_totaltax - diesel_totaltax)
se.prices.bind.2 <- mutate(se.prices.bind.1, real_carbontaxexclusive_with_vat = gas_totalprice - gas_carbontax_with_vat)
se.prices.bind.3 <- mutate(se.prices.bind.2, real_taxdiff_exVAT = (gas_totaltax-gas_vat) - (diesel_totaltax-diesel_vat))
se.prices.bind.4 <- mutate(se.prices.bind.3, extaxprice_lessVAT = gas_totalprice_less_vat - gas_excise_tax)
se.prices.bind.5 <- mutate(se.prices.bind.4, real_taxdiff_exVAT = (gas_totaltax-gas_vat) - (diesel_totaltax-diesel_vat))

#maybe add tax difference between ex-VAT-taxes
se.prices.final <- se.prices.bind.5

#-------------------------EXPORT DESCRIPTIVE PRICE DATA--------------#
se.des.fig.1 <- se.prices.final[, c("gas_totalprice",                         
                                    "gas_totaltax",
                                    "gas_vat",
                                    "gas_carbontax_with_vat",
                                    "gas_energytax_with_vat"
                                    )]
export(se.des.fig.1, file = "output_reg_data/se_des_fig1.csv", format = "csv")

se.des.fig.2 <- se.prices.final[, c("gas_totaltax", "diesel_totaltax", "real_taxdiff")]
export(se.des.fig.2, file = "output_reg_data/se_des_fig2.csv", format = "csv")
#--------------------------------------------------------------------#


###Reg:Gasoline consumption####----------------------------------------------------------------------------------------------------------------------#
##World Bank - WDI Data (1960-2011)
fuel.consumption.wdi <- import("data_input/reg_gasoline-consumption/fuel_consumption_f.csv", dec=",", sep=";")
#Prepare data
se.gas.cons <- filter(fuel.consumption.wdi, fuel.consumption.wdi[,2]=="SWE" & 
                      fuel.consumption.wdi[,3]=="Road sector gasoline fuel consumption per capita (kg of oil equivalent)")
se.gas.cons <- t(se.gas.cons[,25:58])
se.diesel.cons <- filter(fuel.consumption.wdi, fuel.consumption.wdi[,2]=="SWE" & 
                         fuel.consumption.wdi[,3]=="Road sector diesel fuel consumption per capita (kg of oil equivalent)")
se.diesel.cons <- t(se.diesel.cons[,25:58])

##Eurostat Data (1990-2016)   --> Adds last five years
fuel.cons.eurostat <- import("data_input/reg_gasoline-consumption/nrg_110a_work.csv", dec=",", sep=";") 

gas.cons.eurostat <- fuel.cons.eurostat[25:30,]
diesel.cons.eurostat <- fuel.cons.eurostat[53:58,]

colnames(gas.cons.eurostat) <- fuel.cons.eurostat[25,]
colnames(diesel.cons.eurostat) <- fuel.cons.eurostat[53,]

#Prepare data
se.gas.cons.late <- t(filter(gas.cons.eurostat, gas.cons.eurostat[,1]=="Sweden"))
se.diesel.cons.late <- t(filter(diesel.cons.eurostat, diesel.cons.eurostat[,1]=="Sweden"))


data.PWT <- import("data_input/reg_gdp/Penn_World_Table_csv.csv", dec=",", sep=";")  #Load PWT for population data
se.data.PWT <- filter(data.PWT, countrycode=="SWE" & between(year, 1990, 2016))      
se.pop <- se.data.PWT[, 7]                                                          
se.gas.cons.cap.late <- as.numeric(se.gas.cons.late[2:28,])/se.pop
se.diesel.cons.cap.late <- as.numeric(se.diesel.cons.late[2:28,])/se.pop

se.gas.cons.full <- c(se.gas.cons, se.gas.cons.cap.late[23:27])
se.diesel.cons.full <- c(se.diesel.cons, se.diesel.cons.cap.late[23:27])


#-------------------------EXPORT DESCRIPTIVE CONSUMPTION DATA--------------#
se.des.fig.3 <- cbind(se.gas.cons.full, se.diesel.cons.full)
export(se.des.fig.3, file = "output_reg_data/se_des_fig3.csv", format = "csv")
#--------------------------------------------------------------------#


#Add log outcome variable to final variable
se.lg.gas.cons <- log(se.gas.cons.full)
se.lg.diesel.cons <- log(se.gas.cons.full)


#Reg:Urban population####------------------------------------------------------------------------------------------------------------------------------#
urban.population <- import("data_input/reg_urban-population/share_urban_population.csv", dec=",", sep=";")
colnames(urban.population) <- urban.population[4,]	
#Sweden
se.urban <- filter(urban.population, country=="Sweden")
se.urban.cut <- t(se.urban[,c(23:61)])  #23:1978, 61:2016
se.urban.final <- se.urban.cut


##Reg:Unemployment####----------------------------------------------------------------------------------------------------------------------------------#
unemp <- import("data_input/reg_unemployment/amecoSerie_csv.csv", dec=",", sep=";")
#Sweden
se.unemp <- t(unemp[39,21:59])   #C21:1978, #C59:2016     , #R39:Sweden


##Reg:GDP-----------------------------------------------------------------------------------------------------------------------------------------------#
nominal.gdp.USdollar <- import("data_input/reg_gdp/nominal_gdp_oecd.csv", dec=".")                                                 #Load nominal GDP per capita data in current US$
se.nominal.gdp.USdollar <- filter(nominal.gdp.USdollar, LOCATION=="SWE" & between(TIME, 1978, 2016) & MEASURE=="USD_CAP")          #Cut data for country (Sweden) and time (1978-2016)
se.nominal.gdp.sek <- se.nominal.gdp.USdollar[,7]*as.numeric(se.ex[,7])                                                            #Currency change: US$-->SEK

#Real GDP per capita (without PPP)
se.real.gdp.sek.2005 <- se.nominal.gdp.sek/se.cpi.2005/1000          #Real GDP (in thousands) in 2005 SEK

#Real GDP per capita (with PPP)
ppp <- import("data_input/reg_ppp/ppp_full.csv", dec=".")            #Load nominal GDP per capita data in current US$
se.ppp <- filter(ppp, LOCATION=="SWE" & between(TIME, 1978, 2016))
se.nominal.gdp.sek.ppp <- se.nominal.gdp.USdollar[,7]*as.numeric(se.ppp[,7])          #Currency change: US$-->SEK (PPP)
se.real.gdp.sek.ppp.2005 <- se.nominal.gdp.sek.ppp/se.cpi.2005/1000                   #Inflation adjusted (real) GDP per capita in 2005 SEK


#Reg:Create trivial data####----------------------------------------------------------------------------------------------------------------------------#
year = c(1978:2016)                          #year
dummy = c(rep(0, 13), rep(1,26))             #Create dummy (0=1978:1990, 1=1991:2016)
trend = c(1:39)                              #Create trend 
#-------------------------------------------------------------------------------------------------------------------------------------------------------#


###Prepare input data------------------------------------FINAL REGRESSION DATA (data export)------------------------------------------#
#Create regression data frames
se.reg.file.excise <- cbind(year,                         
                            se.lg.gas.cons,                
                            se.prices.final[, "gas_excisetax_with_vat"],
                            se.prices.final[, "gas_exexcise_with_vat"],
                            dummy,           
                            trend, 
                            se.real.gdp.sek.ppp.2005,        
                            se.urban.final, 
                            se.unemp,
                            se.prices.final[,"real_crude"],  
                            se.prices.final[,"real_taxdiff"],    
                            se.prices.final[,"gas_energytax_with_vat"],    
                            se.prices.final[, "gas_carbontax_with_vat"])   
colnames(se.reg.file.excise) = c("year", "log_gas_cons", "gas_excisetax_with_vat", "gas_exexcise_with_vat", "dummy", "t", 
                                 "real_gdp_cap_1000", "urban_pop", "unemploymentrate", 
                                 "real_oil_price_sek", "real_taxdiff", 
                                 "gas_energytax_with_vat", "gas_carbontax_with_vat")

#Export final regression data####
export(se.reg.file.excise, file = "output_reg_data/se_reg_file_excise.csv", format = "csv")

#NEW: Ex-VAT
se.reg.file.excise.exVAT <- cbind(year,                         
                            se.lg.gas.cons,                
                            se.prices.final[, "gas_excise_tax"],
                            se.prices.final[, "gas_extaxprice"],
                            dummy,           
                            trend, 
                            se.real.gdp.sek.ppp.2005,        
                            se.urban.final, 
                            se.unemp,
                            se.prices.final[,"real_crude"],  
                            se.prices.final[,"real_taxdiff_exVAT"],    
                            se.prices.final[,"gas_energytax"],    
                            se.prices.final[, "gas_carbontax"])   
colnames(se.reg.file.excise.exVAT) = c("year", "log_gas_cons", "gas_excise_tax", "gas_extaxprice", "dummy", "t", 
                                 "real_gdp_cap_1000", "urban_pop", "unemploymentrate", 
                                 "real_oil_price_sek", "real_taxdiff_exVAT", 
                                 "gas_energytax", "gas_carbontax")

#Export final regression data####
export(se.reg.file.excise.exVAT, file = "output_reg_data/se_reg_file_excise_exVAT.csv", format = "csv")


#NEW: with-VAT
se.reg.file.excise.fullVAT <- cbind(year,
                                  se.lg.gas.cons,
                                  se.prices.final[, "gas_totaltax"],
                                  se.prices.final[, "extaxprice_lessVAT"],
                                  dummy,
                                  trend,
                                  se.real.gdp.sek.ppp.2005,
                                  se.urban.final,
                                  se.unemp,
                                  se.prices.final[,"real_crude"],
                                  se.prices.final[,"real_taxdiff_exVAT"],          #"real_taxdiff_fullVAT
                                  se.prices.final[,"gas_energytax"],
                                  se.prices.final[, "gas_carbontax"])
colnames(se.reg.file.excise.fullVAT) = c("year", "log_gas_cons", "gas_totaltax", "extaxprice_lessVAT", "dummy", "t",
                                       "real_gdp_cap_1000", "urban_pop", "unemploymentrate",
                                       "real_oil_price_sek", "real_taxdiff_exVAT",
                                       "gas_energytax", "gas_carbontax")

#Export final regression data####
export(se.reg.file.excise.fullVAT, file = "output_reg_data/se_reg_file_excise_fullVAT.csv", format = "csv")

#Carbontax
se.reg.file.carbon <- cbind(year,                        
                             se.lg.gas.cons,              
                             se.prices.final[, "real_carbontaxexclusive_with_vat"],                                              
                             se.prices.final[, "gas_carbontax_with_vat"],
                             dummy,           
                             trend, 
                             se.real.gdp.sek.ppp.2005,       
                             se.urban.final, 
                             se.unemp,
                             se.prices.final[,"real_crude"], 
                             se.prices.final[,"gas_energytax_with_vat"],
                            se.prices.final[,"real_taxdiff"]
                            )    
colnames(se.reg.file.carbon) = c("year", "log_gas_cons", "real_carbontaxexclusive_with_vat", "real_carbontax_with_vat", "d_carbontax", "t", 
                                 "real_gdp_cap_1000", "urban_pop", "unemploymentrate", 
                                 "real_oil_price_sek", "real_energytax_with_vat", "real_taxdiff")
export(se.reg.file.carbon, file = "output_reg_data/se_reg_file_carbon.csv", format = "csv")
#-------------------------------------------------------------------------------------------------------------------------------------------------------#


#Statistics on prices and taxes on Sweden
se.real.gas.tax.increase <- se.prices.final$gas_excise_tax[28]/se.prices.final$gas_excise_tax[8]
se.real.diesel.tax.increase <- se.prices.final$diesel_excise_tax[28]/se.prices.final$diesel_excise_tax[8]
se.real.crude.increase <- se.prices.final$real_crude[28]/se.prices.final$real_crude[8]
se.nominal.crude.increase <- se.nominal.crude[28]/se.nominal.crude[8]
se.nominal.gas.tax.increase <- se.nominal.gas$gas_excise_tax[28]/se.nominal.gas$gas_excise_tax[8]
se.nominal.diesel.tax.increase <- se.nominal.diesel$diesel_excise_tax[28]/se.nominal.diesel$diesel_excise_tax[8]

#Export Finland statistics
se.stat <- c(se.real.gas.tax.increase, se.real.diesel.tax.increase, se.real.crude.increase, se.nominal.crude.increase, se.nominal.gas.tax.increase, se.nominal.diesel.tax.increase)
#colnames(fi.stat) <- c("real_gas_tax", "real_diesel_tax", "real_crude","nominal_crude" ,"nominal_gas_tax", "nominal_diesel_tax")
export(data.frame(se.stat), file = "output_reg_data/se_stat.csv", format = "csv")