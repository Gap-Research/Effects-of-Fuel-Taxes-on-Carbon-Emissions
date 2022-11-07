###################################################
### Figure 7:  Placebo in-space tests           
###################################################
##Clean-------------------------------------------------------------------------------------------####
dev.off()     #Close plot
rm(list=ls()) #Remove all variables from environment
p_unload(all) #Unload all user packages
cat("\014")   #Clean console
#-----------------------------------NEW HEAD----------------------------------------#
###Country id
#Full sample: 1:Australia, 2:Austria, 3:Belgium, 4:Canada, 5: Denmark, 6:Finland, 7: France, 8:Germany, 9:Greece, 10:Iceland, 
#11: Ireland, 12:Italy, 13:Japan, 14:Luxembourg, 15:Netherlands, 16:New Zealand, 17:Norway, 18:Poland, 19:Portugal, 20:Spain
#21:Sweden, 22:Switzerland, 23:Turkey, 24:United Kingdom, 25:United States

library(Synth)     
library(rio)
library(dplyr)
#carbontax <- read.dta(file.choose())                    #Hier muss das foo-file rein 
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
carbontax <- import("output_scm_data/scm_data.csv", dec=".", sep=",")   #Daten für Synth laden

#colnames(carbontax) <- c("year", "Countryno", "country", "CO2_transport_capita", "gas_cons_capita", "diesel_cons_capita", "GDP_per_capita", "urban_pop", "vehicles_capita", "share")
colnames(carbontax) <- c("year", "Countryno", "country", "CO2_transport_capita", "gas_cons_capita", "diesel_cons_capita", "GDP_per_capita", "urban_pop", "vehicles_capita", "cons_ratio", "fuel_cons_capita", "gas_share")

Country <- c("Australia", "Austria", "Belgium", "Canada", "Denmark", 
             "Finland", "France", "Germany", "Greece", "Iceland", 
             "Ireland", "Italy", "Japan", "Luxembourg", "Netherlands",
             "New Zealand", "Norway", "Poland", "Portugal", "Spain", 
             "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States") 
#----------------------------------------------------------------------------------------------------#

#Algorithm settings

controls.identifier.donor.pool <- c(1, 3, 4, 5, 7, 9, 10, 13, 16, 18, 19, 20, 22, 25)
controls.identifier.full.sample <- c(1:25)


#----------------------------------------------------------------------------------------------------#

#define needed runs
runs <- c(6, 17, 21, controls.identifier.donor.pool)

#Create matrix for calculated values and give column names for donor pool countries
store <- matrix(NA,length(1960:2005),length(runs))          
colnames(store) <- Country[runs]

colno <- 1   #Colnumber for value saving

#Run permutation test (test in space)
for(iter in runs)
{
  treat.ID <- iter    
  treat.name <- subset(carbontax$country, carbontax$Countryno==treat.ID & carbontax$year==1960) 
  synth.name <- paste("synthetic ", treat.name, sep="")
  
  if(iter==6 || iter==17 || iter==21){c.id <- controls.identifier.donor.pool
  } else{c.id <-  controls.identifier.donor.pool[!controls.identifier.donor.pool %in% iter]}
  
dataprep.out <-
    dataprep(foo = carbontax,
#             predictors = c("GDP_per_capita" , "gas_cons_capita" , "vehicles_capita" , "urban_pop") ,
#             predictors = c("GDP_per_capita",  "vehicles_capita", "urban_pop", "share"),
              predictors = c("GDP_per_capita", "fuel_cons_capita", "gas_share", "urban_pop"),
             predictors.op = "mean" ,
             time.predictors.prior = 1980:1989 ,
             special.predictors = list(
               list("CO2_transport_capita" , 1988 , "mean"),     
               list("CO2_transport_capita" , 1978 , "mean"),    
               list("CO2_transport_capita" , 1968 , "mean")),
             dependent = "CO2_transport_capita",
             unit.variable = "Countryno",
             unit.names.variable = "country",
             time.variable = "year",
             treatment.identifier = iter,
             controls.identifier = c.id,
       #      controls.identifier = controls.identifier.donor.pool[!controls.identifier.donor.pool %in% iter],
             time.optimize.ssr = 1960:1989,
             time.plot = 1960:2005
    )
  
  # run synth
  synth.out <- synth(
    data.prep.obj = dataprep.out,
    method =  "All"              #"BFGS"
  )
  
  # store gaps
  store[,colno] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
  colno <- colno + 1
  
  #End of iteration (data prep)  
}
#Export store
export(store, file = paste("output_scm_data/placebo_test_store.csv", sep=""), format = "csv")
#--------------------------------------------TODO--------------------------------------------------#

####Plot####
data <- import("output_scm_data/placebo_test_store.csv", dec=".", sep=",")
#rowname(space.data) <- 1960:2005

# now do figure
data <- store
rownames(data) <- 1960:2005

# Set bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
years         <- 1960:2005
gap.end.pre  <- which(rownames(data)=="1989")

#  MSPE Pre-Treatment
mse        <- apply(data[ gap.start:gap.end.pre,]^2,2,mean)
mse.FI <- as.numeric(mse[1]) 
mse.NO <- as.numeric(mse[2]) 
mse.SE <- as.numeric(mse[3])
# Exclude states with 20 times higher MSPE than Finland or Sweden (to include all countries, set value to 1000)
data <- data[,mse<20*mse.FI]
#data <- data[,mse<20*mse.SE]
#data <- data[,mse<20*mse.NO]

Cex.set <- 1

# Plot---------------------------------------------------------------------------------#
png(paste("output_scm_plot/placebo_space_permutation_test.png", sep=""))  

plot(years,data[gap.start:gap.end,which(colnames(data)=="Sweden")],
     ylim=c(-1,1),xlab="Year",
     xlim=c(1960,2005),ylab="Gap in metric tons per capita (CO2 from transport)",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")

# Add lines for control states
for (i in 1:ncol(data)) { lines(years,data[gap.start:gap.end,i],col="gray") }

## Add Finland & Sweden Line
lines(years,data[gap.start:gap.end,which(colnames(data)=="Sweden")],lwd=3,col="black", lty=1)
lines(years,data[gap.start:gap.end,which(colnames(data)=="Finland")],lwd=3,col="black", lty=2)
lines(years,data[gap.start:gap.end,which(colnames(data)=="Norway")],lwd=3,col="black", lty=3)

# Add grid
abline(v=1990,lty="dotted",lwd=2)
abline(h=0,lty="dashed",lwd=2)
legend("bottomleft",legend=c("Finland", "Sweden", "Norway", "control countries"),
       lty=c(1,2,3, 1),
       col=c("black", "black", "black", "gray"),
       lwd=c(2,1),cex=.8)
arrows(1985,-0.7,1989,-0.7,col="black",length=.1)
text(1981,-0.7,"Treatment",cex=Cex.set)
abline(v=1960)
abline(v=2005)
abline(h=-1)
abline(h=1)

dev.off()
#-------------------------------------------------------------------------------------------------#