####Data preperation - SCM
##-----------------------------------------------------------------------------------------------------------------------------####
##Final data structure
#country number (id)
#country (name)
#year
#CO2 transport emissions per capita
#GDP per capita
#Gasoline consumption per capita
#Verhicles per capita           
#Urban population
#Population density --> skip for now
##-------------------------------------------------------------------------------------------------------------------------------####
##Clean--------------------------------------------------------------------------------------------------------------------------####
dev.off()     #Close plot
rm(list=ls()) #Remove all variables from environment
p_unload(all) #Unload all user packages
cat("\014")   #Clean console
##Header-------------------------------------------------------------------------------------------------------------------------####
#install.packages("pacman")
##Head:pacman, packages, working directory#
require(pacman)      ##library(pacman)
#Load packages
p_load(rio,     #Input and Output: import()- & export()-function
      dplyr,    #filter()-function
      tidyr     #gather()- & spread()-function
      )   
#Set working directory
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
##----------------------------------------------------------------------------------------------------------------------------------------#
#Full sample: 1:Australia, 2:Austria, 3:Belgium, 4:Canada, 5: Denmark, 6:Finland, 7: France, 8:Germany, 9:Greece, 10:Iceland, 
#11: Ireland, 12:Italy, 13:Japan, 14:Luxembourg, 15:Netherlands, 16:New Zealand, 17:Norway, 18:Poland, 19:Portugal, 20:Spain
#21:Sweden, 22:Switzerland, 23:Turkey, 24:United Kingdom, 25:United States
#Selected data: 1:Australia, 2:Belgium, 3:Canada, 4: Denmark,  5: France, 6:Greece, 7:Iceland, 
#8:Japan, 9:New Zealand, 10:Poland, 11:Portugal, 12:Spain
#13:Sweden, 14:Switzerland, 15:United States

#Define country vectors
Country <- c("Australia", "Austria", "Belgium", "Canada", "Denmark", 
             "Finland", "France", "Germany", "Greece", "Iceland", 
             "Ireland", "Italy", "Japan", "Luxembourg", "Netherlands",
             "New Zealand", "Norway", "Poland", "Portugal", "Spain", 
             "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States") 
condition  <-  quote(country=="Australia" | country=="Austria" | country=="Belgium" | country=="Canada" | country=="Denmark" | 
                     country=="Finland" | country=="France" | country=="Germany" | country=="Greece" | country=="Iceland" |
                     country=="Ireland" | country=="Italy" | country=="Japan" | country=="Luxembourg" | country=="Netherlands" |
                     country=="New Zealand" | country=="Norway" | country=="Poland" | country=="Portugal" | country=="Spain" |
                     country=="Sweden" | country=="Switzerland" | country=="Turkey" | country=="United Kingdom" |  country=="United States")

#SCM: Trivial data-------------------------------------------------------------------------------------------------#
year = c(1960:2011)              

#Prepare variables--------------------------------------------------------------------------------------------------#

##SCM: CO2 emissions from transport per capita
#World Bank Data
scm.co2.cap <- import("data_input/scm_carbon-emissions/scm_co2_cap.csv", dec=".", sep=",")  
scm.co2.share <- import("data_input/scm_carbon-emissions/scm_co2_share.csv", dec=".", sep=",")
colnames(scm.co2.cap) <- c("country", "code", "variable", "s_code", 1960:2020)
colnames(scm.co2.share) <- c("country", "code", "variable", "s_code", 1960:2020)
#Filter for full sample
donor.pool.scm.co2.cap <- filter_(scm.co2.cap, condition)
donor.pool.scm.co2.share <- filter_(scm.co2.share, condition)
#Sort countries
donor.pool.scm.co2.cap.sort <- donor.pool.scm.co2.cap[order(donor.pool.scm.co2.cap$country),] 
donor.pool.scm.co2.share.sort <- donor.pool.scm.co2.share[order(donor.pool.scm.co2.share$country),] 
#Transporse matrix and cut for 1960-2011
donor.pool.scm.co2.cap.cut <- t(donor.pool.scm.co2.cap.sort[,5:56])
donor.pool.scm.co2.share.cut <- t(donor.pool.scm.co2.share.sort[,5:56])
#Define colnames
colnames(donor.pool.scm.co2.cap.cut) <- Country
colnames(donor.pool.scm.co2.share.cut) <- Country

#Extend World Bank data with OECD data for Germany between 1960 and 1990
scm.co2.cap.oecd <- import("data_input/scm_carbon-emissions/scm_co2_cap_oecd.csv", dec=".", sep=",")    
scm.co2.cap.oecd.ger <- filter(scm.co2.cap.oecd, LOCATION=="DEU" & between(TIME, 1960,1990))
scm.co2.cap.oecd.ger.cut <- scm.co2.cap.oecd.ger[7]
donor.pool.scm.co2.cap.cut[1:31,8] <- t(scm.co2.cap.oecd.ger.cut)

#Calculate CO2 emissions per capita from road transportation
donor.pool.scm.co2.share.cap <- donor.pool.scm.co2.cap.cut*donor.pool.scm.co2.share.cut/100
#Final variable
co2 <- data.frame(donor.pool.scm.co2.share.cap)

#----------------------------------------------------------------------------------#

###SCM:Gasoline and diesel consumption per capita              
#Data from World Bank - WDI Data (beta)
scm_WDI_data <- import("data_input/scm_fuel-consumption/scm_WDI_data.csv", dec=".", sep=",")
colnames(scm_WDI_data) <- c("country", "code", "variable", "s_code", "version", "version2", 1950:2019)
#Filter for full sample
donor.pool.scm_WDI_data <- filter_(scm_WDI_data, condition)
#Filter for variable (Road gasoline and diesel consumption)
donor.pool.gas.con <- filter(donor.pool.scm_WDI_data, variable=="Road sector gasoline fuel consumption per capita (kg of oil equivalent)")
donor.pool.diesel.con <- filter(donor.pool.scm_WDI_data, variable=="Road sector diesel fuel consumption per capita (kg of oil equivalent)")

#Gasoline preperation (Transpose matric and cut for 1960-2011)
donor.pool.gas.con.prep <- t(donor.pool.gas.con[, c(17:68)])
colnames(donor.pool.gas.con.prep) <- Country
mode(donor.pool.gas.con.prep) <- "numeric"
#Diesel preperation (Transpose matric and cut for 1960-2011)
donor.pool.diesel.con.prep <- t(donor.pool.diesel.con[,c(17:68)])
colnames(donor.pool.diesel.con.prep) <- Country
mode(donor.pool.diesel.con.prep) <- "numeric"
#Final variables
gas <- data.frame(donor.pool.gas.con.prep)
diesel <- data.frame(donor.pool.diesel.con.prep)

#----------------------------------------------------------------------------------#

#SCM:Urban population
#World Bank Data
urban.population <- import("data_input/scm_urban-population/share_urban_population.csv", dec=",", sep=";")
colnames(urban.population) <- urban.population[4,]	
#Filter for full sample
donor.pool.urban.pop <- filter_(urban.population, condition)
#Transporse matrix and cut for 1960-2011
urban.cut <- t(donor.pool.urban.pop[,c(1,5:56)])   #5:1960, #23:1978, , #50: 2005, 61:2016
mode(urban.cut) <- "numeric"
#Final variable
urban <- data.frame(urban.cut[-1,])
colnames(urban) <- Country

#----------------------------------------------------------------------------------#

##SCM: Real GDP(PPP, 2011US$)
#Use real PPP GDP values from PWT (2011US$)
data.PWT <- import("data_input/scm_gdp/Penn_World_Table_csv.csv", dec=",", sep=";") 
cut.data.PWT <- data.PWT[, c("year", "country", "rgdpe", "pop")]
#Filter for full sample
donor.pool.gdp <- filter_(cut.data.PWT, condition)
#Calculate (mutate) GDP per capita (in thousands)
donor.pool.gdp.cap <- mutate(donor.pool.gdp, gdp_cap=rgdpe/pop/1000)  #GDP per capita (PPP), 2011US$ (in thousands) 
donor.pool.gdp.cap.cut <- donor.pool.gdp.cap[, c("year", "country", "gdp_cap")]
#Data preparation (Spread, cut)
donor.pool.gdp.cap.spread <- spread(donor.pool.gdp.cap.cut, "country", "gdp_cap")          
cut.donor.pool.gdp.cap <- filter(donor.pool.gdp.cap.spread, between(year, 1960, 2011))
#Final variable
gdp.cap <- cut.donor.pool.gdp.cap[,-1]*1000

#----------------------------------------------------------------------------------#

#SCM: vehicles per 1000 capita
#Load passenger car data (UN Statistical Yearboooks, in thousands)
UN.vehicle.data <- import("data_input/scm_vehicles/UN_stat_yearbook_passenger_cars_extend.csv", dec=",")     #Data 1960-2011 (Values 0 after treatement, not needed)
UN.vehicle.data.cut <- UN.vehicle.data[,-1]
#Load and prepare population data (absolute)
WB.population.data <- import("data_input/scm_population/wb_population.csv", dec=".", sep=",") 
#install.packages("bit64")
colnames(WB.population.data) <- c("country", "code", "variable", "i_code", 1960:2020)
#Filter for full sample
donor.pool.population.country <- filter_(WB.population.data, condition)
#Prepare data (Cut, order, transpose, rename, type/mode)
donor.pool.population.cut <- donor.pool.population.country[,c(1,5:56)]
donor.pool.population.sort <- donor.pool.population.cut[order(donor.pool.population.cut$country),] 
donor.pool.population.t <- t(donor.pool.population.sort)
colnames(donor.pool.population.t) <- Country
donor.pool.population.t.cut <- donor.pool.population.t[-1,]
mode(donor.pool.population.t.cut) <- "numeric"

#Prepare intermediate final variables
donor.pool.population.final <- donor.pool.population.t.cut
donor.pool.vehicles.final <- UN.vehicle.data.cut

#Calculade vehicles per 1000 capita
donor.pool.vehicle.cap <- data.frame(donor.pool.vehicles.final*1000000/donor.pool.population.final)
#Final variable
vehicles <- donor.pool.vehicle.cap


#Prepare data for export file--------------------------------------------------------------------------------------------#
#gather
gather.co2 <- gather(co2, "country", "co2")
gather.gas <- gather(gas, "country", "gas")
gather.diesel <- gather(diesel, "country", "diesel")
gather.gdp.cap <- gather(gdp.cap, "country", "gdp")
gather.urban <- gather(urban, "country", "urban")
gather.vehicles <- gather(vehicles, "country", "vehicles")

#Create id
count <- 1
c.iter <- 1       #country counter
y.iter <- 1       #year counter
gather.id <- c(1:1300)
for (c.iter in 1:25) {
for (y.iter in 1:52) {
  gather.id[count] <- c.iter  
  count <- count + 1
}
}
###Prepare input data------------------------------------FINAL SCM DATA (data export)------------------------------------------#   
#Create SCM data frames
scm.file <- cbind(year,
                  gather.id,
                  gather.co2[,1:2],
                  gather.gas[,2],              
                  gather.diesel[,2],  
                  gather.gdp.cap[,2],
                  gather.urban[,2], 
                  gather.vehicles[,2])

colnames(scm.file) = c("year", "id", "country", "CO2_emissions_transport_cap","gas_cons_capita", "diesel_cons_capita", "real_gdp_PPP_cap", "urban_pop", "vehicles_1000cap")

scm.file <- mutate(scm.file, share = gas_cons_capita/diesel_cons_capita)
scm.file$share[!is.finite(scm.file$share)] <- 0
scm.file <- mutate(scm.file, fuel_cons_capita = gas_cons_capita+diesel_cons_capita)
scm.file <- mutate(scm.file, gas_share = gas_cons_capita/fuel_cons_capita*100)
#Export final regression data####
export(scm.file, file = "output_scm_data/scm_data.csv", format = "csv")
#-------------------------------------------------------------------------------------------------------------------------------------------------------#