####Data preperation - Finland
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
ex <- import("data_input/reg_exchange-rates/exrates_full.csv", dec=",")        #Load exchange rates
##Finland adjusted prices
fi.ex <- filter(ex, LOCATION=="FIN" & between(TIME, 1978, 2016))               #Cut exchange rates for country (Finland) and time (1978-2016)


####Crude oil data------------------------------------------------------------------------------------------------------------------------#
###Nominal crude oil data from Energy Prices & Taxes (1973-2018)
##Finland
#Load data
nominal.crude <- import("data_input/reg_crude-oil/crude_oil.csv", dec=",")      #Load crude oil data from Energy Prices & Taxes in US$
#Cut data
nominal.crude.cut <- filter(nominal.crude[,1:2], between(year, 1978, 2016))    #Cut crude oil data for needed years
fi.nominal.crude <- nominal.crude.cut[,2]*as.numeric(fi.ex[,7])                #Calc nominal crude oil price in EURO                              


###Fuel price data------------------------------------------------------------------------------------------------------------------------#
###Nominal price data from Energy Prices & Taxes (1978-2018)
##Finland
#Load data
#fi.nominal <- import("data_input/271019_fi_data.csv", dec=",", sep=";", skip=2)  
fi.nominal <- import("data_input/reg_fuel-prices-taxes/031119_fi_data.csv", dec=",", sep=";", skip=1)
#colnames(fi.nominal) <- c(fi.nominal[1,])
#Cut data
fi.nominal.cut <- fi.nominal[1:39,1:18]
fi.nominal.tax <- fi.nominal.cut[, c(2:8)]                            #Cut nominal tax data
fi.nominal.diesel <- fi.nominal.cut[, c(9:13)]                        #Cut nominal diesel data
fi.nominal.gas <- fi.nominal.cut[, c(14:18)]                          #Cut nominal gasoline price data
                       
#Currency convertion for years with Finish Mark 
#Data prior to 01.01.1999 is converted with the exchange rate from that introduction date.
#For Finland the convertion rate is 5.94573 FM/Euro
#Inflation is done with CPI.
fi.nominal.gas[1:23,] <- fi.nominal.gas[1:23,]/5.94573
fi.nominal.diesel[1:23,] <- fi.nominal.diesel[1:23,]/5.94573
#Taxes are given in euro for full time.


#VAT split for regression analysis
fi.nominal.gas.0 <- mutate(fi.nominal.gas, gas_totalprice_less_vat = gas_totalprice - gas_vat)      
fi.nominal.gas.1 <- mutate(fi.nominal.gas.0, share_excise = gas_excise_tax / gas_totalprice_less_vat)     
fi.nominal.gas.2 <- mutate(fi.nominal.gas.1, share_exciseexcluscive = gas_extaxprice / gas_totalprice_less_vat)   
fi.nominal.gas.3 <- mutate(fi.nominal.gas.2, gas_excisetax_with_vat = gas_excise_tax + share_excise * gas_vat)             
fi.nominal.gas.4 <- mutate(fi.nominal.gas.3, gas_exexcise_with_vat = gas_extaxprice + share_exciseexcluscive * gas_vat)

fi.nominal.gas <- fi.nominal.gas.4

#Add VAT to nominal prices
fi.nominal.tax.vat <- fi.nominal.tax
fi.nominal.tax.vat[,c(3:7)] <- fi.nominal.tax[,c(3:7)]*(1+fi.nominal.tax[,2]/100)          #Calc nominal tax with VAT 

###Prepare input data for tax incidence------------------------------ FINAL TAX INCIDENCE DATA (nominal data export)-----------------------------------#
#Needed data: year, retail_price, energytax, carbontax, oilprice, total tax
##Finland
fi.tax.incidence <- cbind(fi.nominal.gas[, c("gas_totalprice","gas_totalprice_less_vat", "gas_excise_tax", "gas_excisetax_with_vat")], 
                          fi.nominal.crude)          
colnames(fi.tax.incidence) <- c("gas_totalprice", "gas_totalprice_less_vat", "gas_excise_tax", "gas_excisetax_with_vat", "crude")
export(fi.tax.incidence, file = "output_reg_data/fi_tax_incidence_data.csv", format = "csv")  
#------------------------------------------------------------------------------------------------------------------------------------------------------#


###-------------------------------------------------------------------REGRESSION ALAYSIS DATA----------------------------------------------------------#
###CPI (Inflation) price adjustment---------------------Real price data -------------------------------------------------------------------------------#
#Reg: Prices and Taxes with adjustment###
cpi.2015 <- import("data_input/reg_cpi/CPI_Inflation_index2015_selected.csv", dec=",")      #Load CPI (Consumer price index) data from OECD
cpi.2015.cut <- cpi.2015[,c(1,6,7)]                                                         #Cut CPI data for LOCATION, TIME, Value

#Finland
fi.cpi.2015 <- filter(cpi.2015.cut, LOCATION=="FIN" & between(TIME, 1978, 2016))            #Filter for country (Finland) and time (1978-2016)
#Set new base year - formula= newindex_t=100'(oldindex_t/oldindex_newbase)
fi.cpi.2015.value.2005 <- filter(fi.cpi.2015, TIME==2005)                                   #Filter index for new base year 2005
fi.cpi.2005 <- as.numeric(fi.cpi.2015[,3])/as.numeric(fi.cpi.2015.value.2005[,3])           #Calc CPI for new base year 2005

fi.real.crude.2005 <- fi.nominal.crude/fi.cpi.2005           #Calc real crude oil prices
fi.real.tax.vat.2005 <- fi.nominal.tax.vat/fi.cpi.2005       #Calc real tax prices including VAT 
fi.real.diesel.2005 <- fi.nominal.diesel/fi.cpi.2005         #Calc real diesel prices
fi.real.gas.2005 <- fi.nominal.gas/fi.cpi.2005               #Calc real gasoline prices

#Reg:Calc new variables and create real price data------------------------------------------------------------------------------------------------------#
real_crude <- fi.real.crude.2005
fi.prices.bind <- cbind(fi.real.tax.vat.2005, fi.real.gas.2005, fi.real.diesel.2005, real_crude)
fi.prices.final <- mutate(fi.prices.bind, real_taxdiff = gas_totaltax - diesel_totaltax)


#-------------------------EXPORT DESCRIPTIVE PRICE DATA--------------#
fi.des.fig.1 <- fi.prices.final[, c("gas_totalprice",                         
                                 "gas_totaltax",
                                 "gas_vat")]
export(fi.des.fig.1, file = "output_reg_data/fi_des_fig1.csv", format = "csv")

fi.des.fig.2 <- fi.prices.final[, c("gas_totaltax",
                                    "diesel_totaltax",
                                    "real_taxdiff")]
export(fi.des.fig.2, file = "output_reg_data/fi_des_fig2.csv", format = "csv")
#--------------------------------------------------------------------#


###Reg:Gasoline consumption per capita####------------------------------------------------------------------------------------------------------------------------#
##WDI Data (1960-2011)
fuel.consumption.wdi <- import("data_input/reg_gasoline-consumption/fuel_consumption_f.csv", dec=",", sep=";")
#Finland
fi.gas.cons <- filter(fuel.consumption.wdi, fuel.consumption.wdi[,2]=="FIN" & 
                      fuel.consumption.wdi[,3]=="Road sector gasoline fuel consumption per capita (kg of oil equivalent)")
fi.gas.cons <- t(fi.gas.cons[,25:58])
fi.diesel.cons <- filter(fuel.consumption.wdi, fuel.consumption.wdi[,2]=="FIN" & 
                         fuel.consumption.wdi[,3]=="Road sector diesel fuel consumption per capita (kg of oil equivalent)")
fi.diesel.cons <- t(fi.diesel.cons[,25:58])

##Eurostat Data (1990-2016)   --> Adds last five years
fuel.cons.eurostat <- import("data_input/reg_gasoline-consumption/nrg_110a_work.csv", dec=",", sep=";")

gas.cons.eurostat <- fuel.cons.eurostat[25:30,]
diesel.cons.eurostat <- fuel.cons.eurostat[53:58,]

colnames(gas.cons.eurostat) <- fuel.cons.eurostat[25,]
colnames(diesel.cons.eurostat) <- fuel.cons.eurostat[53,]

#Finland
fi.gas.cons.late <- t(filter(gas.cons.eurostat, gas.cons.eurostat[,1]=="Finland"))
fi.diesel.cons.late <- t(filter(diesel.cons.eurostat, diesel.cons.eurostat[,1]=="Finland"))


data.PWT <- import("data_input/reg_gdp/Penn_World_Table_csv.csv", dec=",", sep=";")  #Load PWT for population data
fi.data.PWT <- filter(data.PWT, countrycode=="FIN" & between(year, 1990, 2016))      
fi.pop <- fi.data.PWT[, 7]                                                           
fi.gas.cons.cap.late <- as.numeric(fi.gas.cons.late[2:28,])/fi.pop
fi.diesel.cons.cap.late <- as.numeric(fi.diesel.cons.late[2:28,])/fi.pop

fi.gas.cons.full <- c(fi.gas.cons, fi.gas.cons.cap.late[23:27])
fi.diesel.cons.full <- c(fi.diesel.cons, fi.diesel.cons.cap.late[23:27])


#-------------------------EXPORT DESCRIPTIVE CONSUMPTION DATA--------------#
fi.des.fig.3 <- cbind(fi.gas.cons.full, fi.diesel.cons.full)
export(fi.des.fig.3, file = "output_reg_data/fi_des_fig3.csv", format = "csv")
#--------------------------------------------------------------------#


#Add log outcome variable to final variable
fi.lg.gas.cons <- log(fi.gas.cons.full)
fi.lg.diesel.cons <- log(fi.gas.cons.full)


#Reg:Urban population####------------------------------------------------------------------------------------------------------------------------------#
urban.population <- import("data_input/reg_urban-population/share_urban_population.csv", dec=",", sep=";")
colnames(urban.population) <- urban.population[4,]	
#Finland
fi.urban <- filter(urban.population, country=="Finland")
fi.urban.cut <- t(fi.urban[,c(23:61)])     #23:1978, 61:2016
fi.urban.final <- fi.urban.cut


##Reg:Unemployment####----------------------------------------------------------------------------------------------------------------------------------#
unemp <- import("data_input/reg_unemployment/amecoSerie_csv.csv", dec=",", sep=";")
#Finland
fi.unemp <- t(unemp[38,21:59])   #Column: 21:1978, 59:2016      #Row: 39:Sweden,  38:Finland


##Reg:GDP-----------------------------------------------------------------------------------------------------------------------------------------------# 
nominal.gdp.USdollar <- import("data_input/reg_gdp/nominal_gdp_oecd.csv", dec=".")                                           #Load nominal GDP per capita data in current US$
fi.nominal.gdp.USdollar <- filter(nominal.gdp.USdollar, LOCATION=="FIN" & between(TIME, 1978, 2016) & MEASURE=="USD_CAP")    #Cut data for country (Finland) and time (1978-2016)
fi.nominal.gdp.euro <- fi.nominal.gdp.USdollar[,7]*as.numeric(fi.ex[,7])                                                     #Currency change: US$-->EURO 

#Real GDP per capita (without PPP)
fi.real.gdp.euro.2005 <- fi.nominal.gdp.euro/fi.cpi.2005/1000               #Real GDP (in thousands) in 2005EUR

#Real GDP per capita (with PPP)
ppp <- import("data_input/reg_ppp/ppp_full.csv", dec=".")                             
fi.ppp <- filter(ppp, LOCATION=="FIN" & between(TIME, 1978, 2016))
fi.nominal.gdp.euro.ppp <- fi.nominal.gdp.USdollar[,7]*as.numeric(fi.ppp[,7])     
fi.real.gdp.euro.ppp.2005 <- fi.nominal.gdp.euro.ppp/fi.cpi.2005/1000         


#Reg:Create trivial data####----------------------------------------------------------------------------------------------------------------------------#
year = c(1978:2016)                          #year
dummy = c(rep(0, 13), rep(1,26))             #Create dummy (0=1978:1990, 1=1991:2016)
trend = c(1:39)                              #Create trend 
#-------------------------------------------------------------------------------------------------------------------------------------------------------#

pricediff <- fi.prices.final$gas_totalprice - fi.prices.final$diesel_totalprice

###Prepare input data------------------------------------FINAL REGRESSION DATA (data export)------------------------------------------#
#Create regression data frames
fi.reg.file <- cbind(year,                       
                     fi.lg.gas.cons,              
                     fi.prices.final[, "gas_exexcise_with_vat"],
                     fi.prices.final[, "gas_excisetax_with_vat"],
                     dummy,           
                     trend, 
                     fi.real.gdp.euro.ppp.2005,       
#                     fi.real.gdp.euro.2005,       
                     fi.urban.final, 
                     fi.unemp,
                     fi.prices.final[,"real_crude"], 
                     fi.prices.final[,"real_taxdiff"],  
                     pricediff
                     )
colnames(fi.reg.file) = c("year", "log_gas_cons", "gas_exexcise_with_vat", "gas_excisetax_with_vat", "d_carbontax", "t", 
                          "real_gdp_cap_1000", "urban_pop", "unemploymentrate", 
                          "real_oil_price_sek", "real_taxdiff", "PriceDiff")

#Export final regression data####
export(fi.reg.file, file = "output_reg_data/fi_reg_file.csv", format = "csv")
#-----------------------------------------------------------------------------------------------------------------------------------#

#Statistics on prices and taxes on Finland
fi.real.gas.tax.increase <- fi.prices.final$gas_excise_tax[28]/fi.prices.final$gas_excise_tax[8]
fi.real.diesel.tax.increase <- fi.prices.final$diesel_excise_tax[28]/fi.prices.final$diesel_excise_tax[8]
fi.real.crude.increase <- fi.prices.final$real_crude[28]/fi.prices.final$real_crude[8]
fi.nominal.crude.increase <- fi.nominal.crude[28]/fi.nominal.crude[8]
fi.nominal.gas.tax.increase <- fi.nominal.gas$gas_excise_tax[28]/fi.nominal.gas$gas_excise_tax[8]
fi.nominal.diesel.tax.increase <- fi.nominal.diesel$diesel_excise_tax[28]/fi.nominal.diesel$diesel_excise_tax[8]

#Export Finland statistics
fi.stat <- c(fi.real.gas.tax.increase, fi.real.diesel.tax.increase, fi.real.crude.increase, fi.nominal.crude.increase, fi.nominal.gas.tax.increase, fi.nominal.diesel.tax.increase)
#colnames(fi.stat) <- c("real_gas_tax", "real_diesel_tax", "real_crude","nominal_crude" ,"nominal_gas_tax", "nominal_diesel_tax")
export(data.frame(fi.stat), file = "output_reg_data/fi_stat.csv", format = "csv")