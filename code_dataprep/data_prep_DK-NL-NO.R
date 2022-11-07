####Data preperation - Finland
##Clean-------------------------------------------------------------------------------------------------------------------------------####X
dev.off()     #Close plot
rm(list=ls()) #Remove all variables from environment
p_unload(all) #Unload all user packages
cat("\014")   #Clean console
##Header------------------------------------------------------------------------------------------------------------------------------####X
#install.packages("pacman")
require(pacman)      
#Load packages
p_load(rio,     #Input and Output: import()- & export()-function
      dplyr     #filter()-function
      )   
#Set working directory
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
##----------------------------------------------------------------------------------------------------------------------------------------#X

#Currency exchange------------------------------------------------------------------------------------------------------------------------#X
ex <- import("data_input/reg_exchange-rates/exrates_full.csv", dec=",")        #Load exchange rates
##Finland adjusted prices
dk.ex <- filter(ex, LOCATION=="DNK" & between(TIME, 1978, 2016))               #Cut exchange rates for country (Finland) and time (1978-2016)
nl.ex <- filter(ex, LOCATION=="NLD" & between(TIME, 1978, 2016))    
no.ex <- filter(ex, LOCATION=="NOR" & between(TIME, 1978, 2016))    

####Crude oil data------------------------------------------------------------------------------------------------------------------------#
###Nominal crude oil data from Energy Prices & Taxes (1973-2018)
##Finland
#Load data
nominal.crude <- import("data_input/reg_crude-oil/crude_oil.csv", dec=",")      #Load crude oil data from Energy Prices & Taxes in US$
#Cut data
nominal.crude.cut <- filter(nominal.crude[,1:2], between(year, 1978, 2016))    #Cut crude oil data for needed years
dk.nominal.crude <- nominal.crude.cut[,2]*as.numeric(dk.ex[,7])                #Calc nominal crude oil price in EURO                              
nl.nominal.crude <- nominal.crude.cut[,2]*as.numeric(nl.ex[,7])    
no.nominal.crude <- nominal.crude.cut[,2]*as.numeric(no.ex[,7])    

###Fuel price data------------------------------------------------------------------------------------------------------------------------#
###Nominal price data from Energy Prices & Taxes (1978-2018)
#Finland
#Load data
#fi.nominal <- import("data_input/271019_fi_data.csv", dec=",", sep=";", skip=2)
dk.nominal <- import("data_input/reg_fuel-prices-taxes/030120_denmark.csv", dec=",", sep=";", skip=1)                 ##Load data mooooooooooooooooore
#colnames(fi.nominal) <- c(fi.nominal[1,])
#Cut data
dk.nominal.cut <- dk.nominal[1:39,1:18]
dk.nominal.tax <- dk.nominal.cut[, c(2:8)]                            #Cut nominal tax data
dk.nominal.diesel <- dk.nominal.cut[, c(9:13)]                        #Cut nominal diesel data
dk.nominal.gas <- dk.nominal.cut[, c(14:18)]                          #Cut nominal gasoline price data

#No currency converstion

#VAT split for regression analysis
dk.nominal.gas.0 <- mutate(dk.nominal.gas, gas_totalprice_less_vat = gas_totalprice - gas_vat)
dk.nominal.gas.1 <- mutate(dk.nominal.gas.0, share_excise = gas_excise_tax / gas_totalprice_less_vat)
dk.nominal.gas.2 <- mutate(dk.nominal.gas.1, share_exciseexcluscive = gas_extaxprice / gas_totalprice_less_vat)
dk.nominal.gas.3 <- mutate(dk.nominal.gas.2, gas_excisetax_with_vat = gas_excise_tax + share_excise * gas_vat)
dk.nominal.gas.4 <- mutate(dk.nominal.gas.3, gas_exexcise_with_vat = gas_extaxprice + share_exciseexcluscive * gas_vat)

dk.nominal.gas <- dk.nominal.gas.4

#Add VAT to nominal prices
dk.nominal.tax.vat <- dk.nominal.tax
dk.nominal.tax.vat[,c(3:7)] <- dk.nominal.tax[,c(3:7)]*(1+dk.nominal.tax[,2]/100)          #Calc nominal tax with VAT

#Netherlands
nl.nominal <- import("data_input/reg_fuel-prices-taxes/030120_netherlands.csv", dec=",", sep=";", skip=1)                 ##Load data mooooooooooooooooore
nl.nominal.cut <- nl.nominal[1:39,1:18]
nl.nominal.tax <- nl.nominal.cut[, c(2:8)]                            #Cut nominal tax data
nl.nominal.gas <- nl.nominal.cut[, c(14:18)]                          #Cut nominal gasoline price data
nl.nominal.diesel <- nl.nominal.cut[, c(9:13)]                        #Cut nominal diesel data

nl.nominal.gas[1:23,] <- nl.nominal.gas[1:23,]/1.8529                    #Goulder convertion rate for Netherlands
nl.nominal.diesel[1:23,] <- nl.nominal.diesel[1:23,]/1.8529

nl.nominal.gas.0 <- mutate(nl.nominal.gas, gas_totalprice_less_vat = gas_totalprice - gas_vat)
nl.nominal.gas.1 <- mutate(nl.nominal.gas.0, share_excise = gas_excise_tax / gas_totalprice_less_vat)
nl.nominal.gas.2 <- mutate(nl.nominal.gas.1, share_exciseexcluscive = gas_extaxprice / gas_totalprice_less_vat)
nl.nominal.gas.3 <- mutate(nl.nominal.gas.2, gas_excisetax_with_vat = gas_excise_tax + share_excise * gas_vat)
nl.nominal.gas.4 <- mutate(nl.nominal.gas.3, gas_exexcise_with_vat = gas_extaxprice + share_exciseexcluscive * gas_vat)
nl.nominal.gas <- nl.nominal.gas.4

nl.nominal.tax.vat <- nl.nominal.tax
nl.nominal.tax.vat[,c(3:7)] <- nl.nominal.tax[,c(3:7)]*(1+nl.nominal.tax[,2]/100)          #Calc nominal tax with VAT

#Norway
no.nominal <- import("data_input/reg_fuel-prices-taxes/030120_norway.csv", dec=",", sep=";", skip=1)                 ##Load data mooooooooooooooooore
no.nominal.cut <- no.nominal[1:39,1:18]
no.nominal.tax <- no.nominal.cut[, c(2:8)]                            #Cut nominal tax data
no.nominal.gas <- no.nominal.cut[, c(14:18)]                          #Cut nominal gasoline price data
no.nominal.diesel <- no.nominal.cut[, c(9:13)]                        #Cut nominal diesel data

#No currency conversion by euro introduction (Kroner)
#no.nominal.gas[1:23,] <- no.nominal.gas[1:23,]/5.94573                 
#no.nominal.diesel[1:23,] <- no.nominal.diesel[1:23,]/5.94573

no.nominal.gas.0 <- mutate(no.nominal.gas, gas_totalprice_less_vat = gas_totalprice - gas_vat)      
no.nominal.gas.1 <- mutate(no.nominal.gas.0, share_excise = gas_excise_tax / gas_totalprice_less_vat)     
no.nominal.gas.2 <- mutate(no.nominal.gas.1, share_exciseexcluscive = gas_extaxprice / gas_totalprice_less_vat)   
no.nominal.gas.3 <- mutate(no.nominal.gas.2, gas_excisetax_with_vat = gas_excise_tax + share_excise * gas_vat)             
no.nominal.gas.4 <- mutate(no.nominal.gas.3, gas_exexcise_with_vat = gas_extaxprice + share_exciseexcluscive * gas_vat)
no.nominal.gas <- no.nominal.gas.4

no.nominal.tax.vat <- no.nominal.tax
no.nominal.tax.vat[,c(3:7)] <- no.nominal.tax[,c(3:7)]*(1+no.nominal.tax[,2]/100)          #Calc nominal tax with VAT 


no.nominal.diesel.0 <- mutate(no.nominal.diesel, diesel_totalprice_less_vat = diesel_totalprice - diesel_vat)      
no.nominal.diesel.1 <- mutate(no.nominal.diesel.0, share_excise_diesel = diesel_excise_tax / diesel_totalprice_less_vat)     
no.nominal.diesel.2 <- mutate(no.nominal.diesel.1, share_exciseexcluscive_diesel = diesel_extaxprice / diesel_totalprice_less_vat)   
no.nominal.diesel.3 <- mutate(no.nominal.diesel.2, diesel_excisetax_with_vat = diesel_excise_tax + share_excise_diesel * diesel_vat)             
no.nominal.diesel.4 <- mutate(no.nominal.diesel.3, diesel_exexcise_with_vat = diesel_extaxprice + share_exciseexcluscive_diesel * diesel_vat)
no.nominal.diesel <- no.nominal.diesel.4

###Prepare input data for tax incidence------------------------------ FINAL TAX INCIDENCE DATA (nominal data export)-----------------------------------#
#Needed data: year, retail_price, energytax, carbontax, oilprice, total tax
# ##Denmark
# dk.tax.incidence <- cbind(dk.nominal.gas[, c("gas_totalprice","gas_totalprice_less_vat", "gas_excise_tax", "gas_excisetax_with_vat")], 
#                           dk.nominal.crude)          
# colnames(dk.tax.incidence) <- c("gas_totalprice", "gas_totalprice_less_vat", "gas_excise_tax", "gas_excisetax_with_vat", "crude")
# export(dk.tax.incidence, file = "output_reg_data/dk_tax_incidence_data.csv", format = "csv")  
# #Netherlands
# nl.tax.incidence <- cbind(nl.nominal.gas[, c("gas_totalprice","gas_totalprice_less_vat", "gas_excise_tax", "gas_excisetax_with_vat")], 
#                           nl.nominal.crude)          
# colnames(nl.tax.incidence) <- c("gas_totalprice", "gas_totalprice_less_vat", "gas_excise_tax", "gas_excisetax_with_vat", "crude")
# export(nl.tax.incidence, file = "output_reg_data/nl_tax_incidence_data.csv", format = "csv")  
#Norway
no.tax.incidence <- cbind(no.nominal.gas[, c("gas_totalprice","gas_totalprice_less_vat", "gas_excise_tax", "gas_excisetax_with_vat")], 
                          no.nominal.crude)          
colnames(no.tax.incidence) <- c("gas_totalprice", "gas_totalprice_less_vat", "gas_excise_tax", "gas_excisetax_with_vat", "crude")
export(no.tax.incidence, file = "output_reg_data/no_tax_incidence_data.csv", format = "csv")  
#------------------------------------------------------------------------------------------------------------------------------------------------------#


###-------------------------------------------------------------------REGRESSION ALAYSIS DATA----------------------------------------------------------#
###CPI (Inflation) price adjustment---------------------Real price data -------------------------------------------------------------------------------#
#Reg: Prices and Taxes with adjustment###
cpi.2015 <- import("data_input/reg_cpi/CPI_Inflation_index2015_selected.csv", dec=",")      #Load CPI (Consumer price index) data from OECD
cpi.2015.cut <- cpi.2015[,c(1,6,7)]                                                         #Cut CPI data for LOCATION, TIME, Value

#Denmark
dk.cpi.2015 <- filter(cpi.2015.cut, LOCATION=="DNK" & between(TIME, 1978, 2016))            #filter for country (Denmark and time (1978-2016)
dk.cpi.2015.value.2005 <- filter(dk.cpi.2015, TIME==2005)                                   #filter index for new base year 2005
dk.cpi.2005 <- as.numeric(dk.cpi.2015[,3])/as.numeric(dk.cpi.2015.value.2005[,3])           #Calc CPI for new base year 2005

dk.real.crude.2005 <- dk.nominal.crude/dk.cpi.2005           #Calc real crude oil prices
dk.real.tax.vat.2005 <- dk.nominal.tax.vat/dk.cpi.2005       #Calc real tax prices including VAT
dk.real.diesel.2005 <- dk.nominal.diesel/dk.cpi.2005         #Calc real diesel prices
dk.real.gas.2005 <- dk.nominal.gas/dk.cpi.2005               #Calc real gasoline prices

real_crude <- dk.real.crude.2005                                              ###################################################################????????????
dk.prices.bind <- cbind(dk.real.tax.vat.2005, dk.real.gas.2005, dk.real.diesel.2005, real_crude)
dk.prices.final <- mutate(dk.prices.bind, real_taxdiff = gas_totaltax - diesel_totaltax)

#Netherlands
nl.cpi.2015 <- filter(cpi.2015.cut, LOCATION=="NLD" & between(TIME, 1978, 2016))            #filter for country (nlnland) and time (1978-2016)
nl.cpi.2015.value.2005 <- filter(nl.cpi.2015, TIME==2005)                                   #filter index for new base year 2005
nl.cpi.2005 <- as.numeric(nl.cpi.2015[,3])/as.numeric(nl.cpi.2015.value.2005[,3])           #Calc CPI for new base year 2005

nl.real.crude.2005 <- nl.nominal.crude/nl.cpi.2005           #Calc real crude oil prices
nl.real.tax.vat.2005 <- nl.nominal.tax.vat/nl.cpi.2005       #Calc real tax prices including VAT
nl.real.diesel.2005 <- nl.nominal.diesel/nl.cpi.2005         #Calc real diesel prices
nl.real.gas.2005 <- nl.nominal.gas/nl.cpi.2005               #Calc real gasoline prices
# 
real_crude <- nl.real.crude.2005                                             ###################################################################????????????
 nl.prices.bind <- cbind(nl.real.tax.vat.2005, nl.real.gas.2005, nl.real.diesel.2005, real_crude)
 nl.prices.final <- mutate(nl.prices.bind, real_taxdiff = gas_totaltax - diesel_totaltax)

#Norway
no.cpi.2015 <- filter(cpi.2015.cut, LOCATION=="NOR" & between(TIME, 1978, 2016))            #filter for country (nonland) and time (1978-2016)
no.cpi.2015.value.2005 <- filter(no.cpi.2015, TIME==2005)                                   #filter index for new base year 2005
no.cpi.2005 <- as.numeric(no.cpi.2015[,3])/as.numeric(no.cpi.2015.value.2005[,3])           #Calc CPI for new base year 2005

no.real.crude.2005 <- no.nominal.crude/no.cpi.2005           #Calc real crude oil prices
no.real.tax.vat.2005 <- no.nominal.tax.vat/no.cpi.2005       #Calc real tax prices including VAT 
no.real.diesel.2005 <- no.nominal.diesel/no.cpi.2005         #Calc real diesel prices
no.real.gas.2005 <- no.nominal.gas/no.cpi.2005               #Calc real gasoline prices

real_crude <- no.real.crude.2005                                             ###################################################################????????????
no.prices.bind <- cbind(no.real.tax.vat.2005, no.real.gas.2005, no.real.diesel.2005, real_crude)
no.prices.final <- mutate(no.prices.bind, real_taxdiff = gas_totaltax - diesel_totaltax)


#-------------------------EXPORT DESCRIPTIVE PRICE DATA--------------#
# #Denmark
# dk.des.fig.1 <- dk.prices.final[, c("gas_totalprice",                         
#                                  "gas_totaltax",
#                                  "gas_vat")]
# export(dk.des.fig.1, file = "output_reg_data/dk_des_fig1.csv", format = "csv")
# 
# dk.des.fig.2 <- dk.prices.final[, c("gas_totaltax",
#                                     "diesel_totaltax",
#                                     "real_taxdiff")]
# export(dk.des.fig.2, file = "output_reg_data/dk_des_fig2.csv", format = "csv")
# #Netherlands
# nl.des.fig.1 <- nl.prices.final[, c("gas_totalprice",                         
#                                     "gas_totaltax",
#                                     "gas_vat")]
# export(nl.des.fig.1, file = "output_reg_data/nl_des_fig1.csv", format = "csv")
# 
# nl.des.fig.2 <- nl.prices.final[, c("gas_totaltax",
#                                     "diesel_totaltax",
#                                     "real_taxdiff")]
# export(nl.des.fig.2, file = "output_reg_data/nl_des_fig2.csv", format = "csv")
#Norway
no.des.fig.1 <- no.prices.final[, c("gas_totalprice",                         
                                    "gas_totaltax",
                                    "gas_vat")]
export(no.des.fig.1, file = "output_reg_data/no_des_fig1.csv", format = "csv")

no.des.fig.2 <- no.prices.final[, c("gas_totaltax",
                                    "diesel_totaltax",
                                    "real_taxdiff")]
export(no.des.fig.2, file = "output_reg_data/no_des_fig2.csv", format = "csv")
#--------------------------------------------------------------------#


# ###Reg:Gasoline consumption per capita####------------------------------------------------------------------------------------------------------------------------#
# ##WDI Data (1960-2011)
# fuel.consumption.wdi <- import("data_input/reg_gasoline-consumption/fuel_consumption_f.csv", dec=",", sep=";")
# #Finland
# fi.gas.cons <- filter(fuel.consumption.wdi, fuel.consumption.wdi[,2]=="FIN" & 
#                       fuel.consumption.wdi[,3]=="Road sector gasoline fuel consumption per capita (kg of oil equivalent)")
# fi.gas.cons <- t(fi.gas.cons[,25:58])
# fi.diesel.cons <- filter(fuel.consumption.wdi, fuel.consumption.wdi[,2]=="FIN" & 
#                          fuel.consumption.wdi[,3]=="Road sector diesel fuel consumption per capita (kg of oil equivalent)")
# fi.diesel.cons <- t(fi.diesel.cons[,25:58])
# 
# ##Eurostat Data (1990-2016)   --> Adds last five years
# fuel.cons.eurostat <- import("data_input/reg_gasoline-consumption/nrg_110a_work.csv", dec=",", sep=";")
# 
# gas.cons.eurostat <- fuel.cons.eurostat[25:30,]
# diesel.cons.eurostat <- fuel.cons.eurostat[53:58,]
# 
# colnames(gas.cons.eurostat) <- fuel.cons.eurostat[25,]
# colnames(diesel.cons.eurostat) <- fuel.cons.eurostat[53,]
# 
# #Finland
# fi.gas.cons.late <- t(filter(gas.cons.eurostat, gas.cons.eurostat[,1]=="Finland"))
# fi.diesel.cons.late <- t(filter(diesel.cons.eurostat, diesel.cons.eurostat[,1]=="Finland"))
# 
# 
# data.PWT <- import("data_input/reg_gdp/Penn_World_Table_csv.csv", dec=",", sep=";")  #Load PWT for population data
# fi.data.PWT <- filter(data.PWT, countrycode=="FIN" & between(year, 1990, 2016))      
# fi.pop <- fi.data.PWT[, 7]                                                           
# fi.gas.cons.cap.late <- as.numeric(fi.gas.cons.late[2:28,])/fi.pop
# fi.diesel.cons.cap.late <- as.numeric(fi.diesel.cons.late[2:28,])/fi.pop
# 
# fi.gas.cons.full <- c(fi.gas.cons, fi.gas.cons.cap.late[23:27])
# fi.diesel.cons.full <- c(fi.diesel.cons, fi.diesel.cons.cap.late[23:27])
# 
# 
# #-------------------------EXPORT DESCRIPTIVE CONSUMPTION DATA--------------#
# fi.des.fig.3 <- cbind(fi.gas.cons.full, fi.diesel.cons.full)
# export(fi.des.fig.3, file = "output_reg_data/fi_des_fig3.csv", format = "csv")
# #--------------------------------------------------------------------#
# 
# 
# #Add log outcome variable to final variable
# fi.lg.gas.cons <- log(fi.gas.cons.full)
# fi.lg.diesel.cons <- log(fi.gas.cons.full)

fuel.consumption.wdi <- import("data_input/reg_gasoline-consumption/consumption_wdi.csv", dec=".", sep=",")
fuel.cons.eurostat <- import("data_input/reg_gasoline-consumption/nrg_110a_work.csv", dec=",", sep=";")

# #Denmark
# dk.gas.cons <- filter(fuel.consumption.wdi, fuel.consumption.wdi[,2]=="DNK" & 
#                         fuel.consumption.wdi[,3]=="Road sector gasoline fuel consumption per capita (kg of oil equivalent)")
# dk.gas.cons <- t(dk.gas.cons[,25:58])
# dk.diesel.cons <- filter(fuel.consumption.wdi, fuel.consumption.wdi[,2]=="DNK" & 
#                            fuel.consumption.wdi[,3]=="Road sector diesel fuel consumption per capita (kg of oil equivalent)")
# dk.diesel.cons <- t(dk.diesel.cons[,25:58])
# 
# gas.cons.eurostat <- fuel.cons.eurostat[25:30,]
# diesel.cons.eurostat <- fuel.cons.eurostat[53:58,]
# 
# colnames(gas.cons.eurostat) <- fuel.cons.eurostat[25,]
# colnames(diesel.cons.eurostat) <- fuel.cons.eurostat[53,]
# 
# dk.gas.cons.late <- t(filter(gas.cons.eurostat, gas.cons.eurostat[,1]=="Denmark"))
# dk.diesel.cons.late <- t(filter(diesel.cons.eurostat, diesel.cons.eurostat[,1]=="Denmark"))
# 
# 
# data.PWT <- import("data_input/reg_gdp/Penn_World_Table_csv.csv", dec=",", sep=";")  #Load PWT for population data
# dk.data.PWT <- filter(data.PWT, countrycode=="DNK" & between(year, 1990, 2016))      
# dk.pop <- dk.data.PWT[, 7]                                                           
# dk.gas.cons.cap.late <- as.numeric(dk.gas.cons.late[2:28,])/dk.pop
# dk.diesel.cons.cap.late <- as.numeric(dk.diesel.cons.late[2:28,])/dk.pop
# 
# dk.gas.cons.full <- c(dk.gas.cons, dk.gas.cons.cap.late[23:27])
# dk.diesel.cons.full <- c(dk.diesel.cons, dk.diesel.cons.cap.late[23:27])
# 
# #-------------------------EXPORT DESCRIPTIVE CONSUMPTION DATA--------------#
# dk.des.fig.3 <- cbind(dk.gas.cons.full, dk.diesel.cons.full)
# export(dk.des.fig.3, file = "output_reg_data/dk_des_fig3.csv", format = "csv")
# #--------------------------------------------------------------------#
# 
# #Add log outcome variable to final variable
# dk.lg.gas.cons <- log(dk.gas.cons.full)
# dk.lg.diesel.cons <- log(dk.gas.cons.full)
# 
# #Netherlands
# nl.gas.cons <- filter(fuel.consumption.wdi, fuel.consumption.wdi[,2]=="NLD" & 
#                         fuel.consumption.wdi[,3]=="Road sector gasoline fuel consumption per capita (kg of oil equivalent)")
# nl.gas.cons <- t(nl.gas.cons[,25:58])
# nl.diesel.cons <- filter(fuel.consumption.wdi, fuel.consumption.wdi[,2]=="NLD" & 
#                            fuel.consumption.wdi[,3]=="Road sector diesel fuel consumption per capita (kg of oil equivalent)")
# nl.diesel.cons <- t(nl.diesel.cons[,25:58])
# 
# gas.cons.eurostat <- fuel.cons.eurostat[25:30,]
# diesel.cons.eurostat <- fuel.cons.eurostat[53:58,]
# 
# colnames(gas.cons.eurostat) <- fuel.cons.eurostat[25,]
# colnames(diesel.cons.eurostat) <- fuel.cons.eurostat[53,]
# 
# nl.gas.cons.late <- t(filter(gas.cons.eurostat, gas.cons.eurostat[,1]=="Netherlands"))
# nl.diesel.cons.late <- t(filter(diesel.cons.eurostat, diesel.cons.eurostat[,1]=="Netherlands"))
# 
# 
# data.PWT <- import("data_input/reg_gdp/Penn_World_Table_csv.csv", dec=",", sep=";")  #Load PWT for population data
# nl.data.PWT <- filter(data.PWT, countrycode=="NLD" & between(year, 1990, 2016))      
# nl.pop <- nl.data.PWT[, 7]                                                           
# nl.gas.cons.cap.late <- as.numeric(nl.gas.cons.late[2:28,])/nl.pop
# nl.diesel.cons.cap.late <- as.numeric(nl.diesel.cons.late[2:28,])/nl.pop
# 
# nl.gas.cons.full <- c(nl.gas.cons, nl.gas.cons.cap.late[23:27])
# nl.diesel.cons.full <- c(nl.diesel.cons, nl.diesel.cons.cap.late[23:27])
# 
# #-------------------------EXPORT DESCRIPTIVE CONSUMPTION DATA--------------#
# nl.des.fig.3 <- cbind(nl.gas.cons.full, nl.diesel.cons.full)
# export(nl.des.fig.3, file = "output_reg_data/nl_des_fig3.csv", format = "csv")
# #--------------------------------------------------------------------#
# 
# #Add log outcome variable to final variable
# nl.lg.gas.cons <- log(nl.gas.cons.full)
# nl.lg.diesel.cons <- log(nl.gas.cons.full)


#Norway
no.gas.cons <- filter(fuel.consumption.wdi, fuel.consumption.wdi[,2]=="NOR" & 
                        fuel.consumption.wdi[,3]=="Road sector gasoline fuel consumption per capita (kg of oil equivalent)")
no.gas.cons <- t(no.gas.cons[,25:58])
no.diesel.cons <- filter(fuel.consumption.wdi, fuel.consumption.wdi[,2]=="NOR" & 
                           fuel.consumption.wdi[,3]=="Road sector diesel fuel consumption per capita (kg of oil equivalent)")
no.diesel.cons <- t(no.diesel.cons[,25:58])

gas.cons.eurostat <- fuel.cons.eurostat[25:30,]
diesel.cons.eurostat <- fuel.cons.eurostat[53:58,]

colnames(gas.cons.eurostat) <- fuel.cons.eurostat[25,]
colnames(diesel.cons.eurostat) <- fuel.cons.eurostat[53,]

no.gas.cons.late <- t(filter(gas.cons.eurostat, gas.cons.eurostat[,1]=="Norway"))
no.diesel.cons.late <- t(filter(diesel.cons.eurostat, diesel.cons.eurostat[,1]=="Norway"))


data.PWT <- import("data_input/reg_gdp/Penn_World_Table_csv.csv", dec=",", sep=";")  #Load PWT for population data
no.data.PWT <- filter(data.PWT, countrycode=="NOR" & between(year, 1990, 2016))      
no.pop <- no.data.PWT[, 7]                                                           
no.gas.cons.cap.late <- as.numeric(no.gas.cons.late[2:28,])/no.pop
no.diesel.cons.cap.late <- as.numeric(no.diesel.cons.late[2:28,])/no.pop

no.gas.cons.full <- c(no.gas.cons, no.gas.cons.cap.late[23:27])
no.diesel.cons.full <- c(no.diesel.cons, no.diesel.cons.cap.late[23:27])

#-------------------------EXPORT DESCRIPTIVE CONSUMPTION DATA--------------#
no.des.fig.3 <- cbind(no.gas.cons.full, no.diesel.cons.full)
export(no.des.fig.3, file = "output_reg_data/no_des_fig3.csv", format = "csv")
#--------------------------------------------------------------------#

#Add log outcome variable to final variable
no.lg.gas.cons <- log(no.gas.cons.full)
no.lg.diesel.cons <- log(no.diesel.cons.full)


#Reg:Urban population####------------------------------------------------------------------------------------------------------------------------------#
urban.population <- import("data_input/reg_urban-population/share_urban_population.csv", dec=",", sep=";")
colnames(urban.population) <- urban.population[4,]	
# #Denmark
# dk.urban <- filter(urban.population, country=="Denmark")
# dk.urban.cut <- t(dk.urban[,c(23:61)])     #23:1978, 61:2016
# dk.urban.final <- dk.urban.cut
# #Netherlands
# nl.urban <- filter(urban.population, country=="Netherlands")
# nl.urban.cut <- t(nl.urban[,c(23:61)])     #23:1978, 61:2016
# nl.urban.final <- nl.urban.cut
#Norway
no.urban <- filter(urban.population, country=="Norway")
no.urban.cut <- t(no.urban[,c(23:61)])     #23:1978, 61:2016
no.urban.final <- no.urban.cut


##Reg:Unemployment####----------------------------------------------------------------------------------------------------------------------------------#
unemp <- import("data_input/reg_unemployment/amecoSerie_csv.csv", dec=",", sep=";")
#DK, NL, NO
# dk.unemp <- t(unemp[15,21:59])     #15 is DK
# nl.unemp <- t(unemp[31,21:59])     #31 is Netherlands
no.unemp <- t(unemp[46,21:59])     #46 is Norway

##Reg:GDP-----------------------------------------------------------------------------------------------------------------------------------------------# 
nominal.gdp.USdollar <- import("data_input/reg_gdp/nominal_gdp_oecd.csv", dec=".")                                           #Load nominal GDP per capita data in current US$
ppp <- import("data_input/reg_ppp/ppp_full.csv", dec=".")                             

# #Denmark
# dk.nominal.gdp.USdollar <- filter(nominal.gdp.USdollar, LOCATION=="DNK" & between(TIME, 1978, 2016) & MEASURE=="USD_CAP")    #Cut data for country (dknland) and time (1978-2016)
# dk.nominal.gdp.euro <- dk.nominal.gdp.USdollar[,7]*as.numeric(dk.ex[,7])                                                     #Currency change: US$-->EURO 
# #Real GDP per capita (without PPP)
# dk.real.gdp.euro.2005 <- dk.nominal.gdp.euro/dk.cpi.2005/1000               #Real GDP (in thousands) in 2005EUR
# #Real GDP per capita (with PPP)
# dk.ppp <- filter(ppp, LOCATION=="DNK" & between(TIME, 1978, 2016))
# dk.nominal.gdp.euro.ppp <- dk.nominal.gdp.USdollar[,7]*as.numeric(dk.ppp[,7])     
# dk.real.gdp.euro.ppp.2005 <- dk.nominal.gdp.euro.ppp/dk.cpi.2005/1000       
# 
# #Netherlands
# nl.nominal.gdp.USdollar <- filter(nominal.gdp.USdollar, LOCATION=="NLD" & between(TIME, 1978, 2016) & MEASURE=="USD_CAP")    #Cut data for country (nlnland) and time (1978-2016)
# nl.nominal.gdp.euro <- nl.nominal.gdp.USdollar[,7]*as.numeric(nl.ex[,7])                                                     #Currency change: US$-->EURO 
# #Real GDP per capita (without PPP)
# nl.real.gdp.euro.2005 <- nl.nominal.gdp.euro/nl.cpi.2005/1000               #Real GDP (in thousands) in 2005EUR
# #Real GDP per capita (with PPP)
# nl.ppp <- filter(ppp, LOCATION=="NLD" & between(TIME, 1978, 2016))
# nl.nominal.gdp.euro.ppp <- nl.nominal.gdp.USdollar[,7]*as.numeric(nl.ppp[,7])     
# nl.real.gdp.euro.ppp.2005 <- nl.nominal.gdp.euro.ppp/nl.cpi.2005/1000       

#Norway
no.nominal.gdp.USdollar <- filter(nominal.gdp.USdollar, LOCATION=="NOR" & between(TIME, 1978, 2016) & MEASURE=="USD_CAP")    #Cut data for country (nonoand) and time (1978-2016)
no.nominal.gdp.euro <- no.nominal.gdp.USdollar[,7]*as.numeric(no.ex[,7])                                                     #Currency change: US$-->EURO 
#Real GDP per capita (without PPP)
no.real.gdp.euro.2005 <- no.nominal.gdp.euro/no.cpi.2005/1000               #Real GDP (in thousands) in 2005EUR
#Real GDP per capita (with PPP)
no.ppp <- filter(ppp, LOCATION=="NOR" & between(TIME, 1978, 2016))
no.nominal.gdp.euro.ppp <- no.nominal.gdp.USdollar[,7]*as.numeric(no.ppp[,7])     
no.real.gdp.euro.ppp.2005 <- no.nominal.gdp.euro.ppp/no.cpi.2005/1000   


#Reg:Create trivial data####----------------------------------------------------------------------------------------------------------------------------#
year = c(1978:2016)                          #year
dummy = c(rep(0, 13), rep(1,26))             #Create dummy (0=1978:1990, 1=1991:2016)
trend = c(1:39)                              #Create trend 
#-------------------------------------------------------------------------------------------------------------------------------------------------------#


# ###Prepare input data------------------------------------FINAL REGRESSION DATA (data export)------------------------------------------#
# #Create regression data frames
# dk.reg.file <- cbind(year,            
#                      dk.lg.gas.cons,
#                      dk.prices.final[, "gas_exexcise_with_vat"],        #Price specification: 
#                      dk.prices.final[, "gas_excisetax_with_vat"],
#                      dummy,           
#                      trend, 
#                    #  dk.real.gdp.euro.ppp.2005,                    
#                      dk.real.gdp.euro.2005,       
#                      dk.urban.final, 
#                      dk.unemp,
#                      dk.prices.final[,"real_crude"], 
#                      dk.prices.final[,"real_taxdiff"]     
#                      )
# colnames(dk.reg.file) = c("year", "log_gas_cons", "gas_exexcise_with_vat", "tax", "d_carbontax", "t", 
#                           "real_gdp_cap_1000", "urban_pop", "unemploymentrate", 
#                           "real_oil_price_sek", "real_taxdiff")
# length(dk.reg.file)
# #Export final regression data####
# export(dk.reg.file, file = "output_reg_data/dk_reg_file.csv", format = "csv")
# 
# #Netherlands
# nl.reg.file <- cbind(year,                       
#                      nl.lg.gas.cons,              
#                      nl.prices.final[, "gas_exexcise_with_vat"],
#                      nl.prices.final[, "gas_excisetax_with_vat"],
#                      dummy,           
#                      trend, 
#                      nl.real.gdp.euro.ppp.2005,       
#                      nl.urban.final, 
#                      nl.unemp,
#                      nl.prices.final[,"real_crude"], 
#                      nl.prices.final[,"real_taxdiff"]     
# )
# colnames(nl.reg.file) = c("year", "log_gas_cons", "gas_exexcise_with_vat", "tax", "d_carbontax", "t", 
#                           "real_gdp_cap_1000", "urban_pop", "unemploymentrate", 
#                           "real_oil_price_sek", "real_taxdiff")
# export(nl.reg.file, file = "output_reg_data/nl_reg_file.csv", format = "csv")

#Norway
no.reg.file <- cbind(year,                       
                     no.lg.gas.cons,              
                     no.prices.final[, "gas_exexcise_with_vat"],
                     no.prices.final[, "gas_excisetax_with_vat"],
                     dummy,           
                     trend, 
                     no.real.gdp.euro.ppp.2005,  
#                     no.real.gdp.euro.2005,       
                     no.urban.final, 
                     no.unemp,
                     no.prices.final[,"real_crude"], 
                     no.prices.final[,"real_taxdiff"]     
)
colnames(no.reg.file) = c("year", "log_gas_cons", "gas_exexcise_with_vat", "tax", "d_carbontax", "t", 
                          "real_gdp_cap_1000", "urban_pop", "unemploymentrate", 
                          "real_oil_price_sek", "real_taxdiff")
export(no.reg.file, file = "output_reg_data/no_reg_file.csv", format = "csv")
#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#Norway
no.reg.file.diesel <- cbind(year,                       
                     no.lg.diesel.cons,              
                     no.prices.final[, "diesel_exexcise_with_vat"],
                     no.prices.final[, "diesel_excisetax_with_vat"],
                     dummy,           
                     trend, 
                     no.real.gdp.euro.ppp.2005,  
                     #                     no.real.gdp.euro.2005,       
                     no.urban.final, 
                     no.unemp,
                     no.prices.final[,"real_crude"], 
                     no.prices.final[,"real_taxdiff"]     
)
colnames(no.reg.file.diesel) = c("year", "log_diesel_cons", "diesel_exexcise_with_vat", "tax", "d_carbontax", "t", 
                          "real_gdp_cap_1000", "urban_pop", "unemploymentrate", 
                          "real_oil_price_sek", "real_taxdiff")
export(no.reg.file.diesel, file = "output_reg_data/no_reg_file_diesel.csv", format = "csv")
#--------------------------------------------------------------------------------------------------------------------------------------#


#Statistics on prices and taxes on Norway
no.real.gas.tax.increase <- no.prices.final$gas_excise_tax[28]/no.prices.final$gas_excise_tax[8]
no.real.diesel.tax.increase <- no.prices.final$diesel_excise_tax[28]/no.prices.final$diesel_excise_tax[8]
no.real.crude.increase <- no.prices.final$real_crude[28]/no.prices.final$real_crude[8]
no.nominal.crude.increase <- no.nominal.crude[28]/no.nominal.crude[8]
no.nominal.gas.tax.increase <- no.nominal.gas$gas_excise_tax[28]/no.nominal.gas$gas_excise_tax[8]
no.nominal.diesel.tax.increase <- no.nominal.diesel$diesel_excise_tax[28]/no.nominal.diesel$diesel_excise_tax[8]

#Export Finland statistics
no.stat <- c(no.real.gas.tax.increase, no.real.diesel.tax.increase, no.real.crude.increase, no.nominal.crude.increase, no.nominal.gas.tax.increase, no.nominal.diesel.tax.increase)
#colnames(fi.stat) <- c("real_gas_tax", "real_diesel_tax", "real_crude","nominal_crude" ,"nominal_gas_tax", "nominal_diesel_tax")
export(data.frame(no.stat), file = "output_reg_data/no_stat.csv", format = "csv")

#Statistics on prices and taxes on Denmark and the Netherlands
dk.real.gas.tax.increase <- dk.prices.final$gas_excise_tax[28]/dk.prices.final$gas_excise_tax[8]
dk.real.diesel.tax.increase <- dk.prices.final$diesel_excise_tax[28]/dk.prices.final$diesel_excise_tax[8]
dk.real.crude.increase <- dk.prices.final$real_crude[28]/dk.prices.final$real_crude[8]
dk.nominal.crude.increase <- dk.nominal.crude[28]/dk.nominal.crude[8]
dk.nominal.gas.tax.increase <- dk.nominal.gas$gas_excise_tax[28]/dk.nominal.gas$gas_excise_tax[8]
dk.nominal.diesel.tax.increase <- dk.nominal.diesel$diesel_excise_tax[28]/dk.nominal.diesel$diesel_excise_tax[8]
#Export statistics
dk.stat <- c(dk.real.gas.tax.increase, dk.real.diesel.tax.increase, dk.real.crude.increase, dk.nominal.crude.increase, dk.nominal.gas.tax.increase, dk.nominal.diesel.tax.increase)
#colnames(fi.stat) <- c("real_gas_tax", "real_diesel_tax", "real_crude","nominal_crude" ,"nominal_gas_tax", "nominal_diesel_tax")
export(data.frame(dk.stat), file = "output_reg_data/dk_stat.csv", format = "csv")

#Statistics on prices and taxes on Denmark and the Netherlands
nl.real.gas.tax.increase <- nl.prices.final$gas_excise_tax[28]/nl.prices.final$gas_excise_tax[8]
nl.real.diesel.tax.increase <- nl.prices.final$diesel_excise_tax[28]/nl.prices.final$diesel_excise_tax[8]
nl.real.crude.increase <- nl.prices.final$real_crude[28]/nl.prices.final$real_crude[8]
nl.nominal.crude.increase <- nl.nominal.crude[28]/nl.nominal.crude[8]
nl.nominal.gas.tax.increase <- nl.nominal.gas$gas_excise_tax[28]/nl.nominal.gas$gas_excise_tax[8]
nl.nominal.diesel.tax.increase <- nl.nominal.diesel$diesel_excise_tax[28]/nl.nominal.diesel$diesel_excise_tax[8]
#Export statistics
nl.stat <- c(nl.real.gas.tax.increase, nl.real.diesel.tax.increase, nl.real.crude.increase, nl.nominal.crude.increase, nl.nominal.gas.tax.increase, nl.nominal.diesel.tax.increase)
#colnames(fi.stat) <- c("real_gas_tax", "real_diesel_tax", "real_crude","nominal_crude" ,"nominal_gas_tax", "nominal_diesel_tax")
export(data.frame(nl.stat), file = "output_reg_data/nl_stat.csv", format = "csv")
