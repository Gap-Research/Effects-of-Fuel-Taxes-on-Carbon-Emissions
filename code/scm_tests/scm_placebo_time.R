###################################################
### Placebo in-time tests 1980/1970            
###################################################
#The date of the treatment is shifted in time from 1990 to 1980 and 1970
#Time settings for key parameters are adjusted
###################################################

p_load(rio,
       Synth
) 

# setwd()

###Country id
#Full sample: 1:Australia, 2:Austria, 3:Belgium, 4:Canada, 5: Denmark, 6:Finland, 7: France, 8:Germany, 9:Greece, 10:Iceland, 
#11: Ireland, 12:Italy, 13:Japan, 14:Luxembourg, 15:Netherlands, 16:New Zealand, 17:Norway, 18:Poland, 19:Portugal, 20:Spain
#21:Sweden, 22:Switzerland, 23:Turkey, 24:United Kingdom, 25:United States

carbontax <- import("data/scm_data/scm_data.csv", dec=".", sep=",") 
colnames(carbontax) <- c("year", "Countryno", "country", "CO2_transport_capita", "gas_cons_capita", "diesel_cons_capita", "GDP_per_capita", "urban_pop", "vehicles_capita", "cons_ratio", "fuel_cons_capita", "gas_share")

treated <- c(6, 17, 21)

#-----------------------------------PLACEBO RUNS IN TIME--------------------------------#
#Runs for treated countries
for(iter in 1:3)
{ 
#Algorithm settings
treat.ID <- treated[iter]
treat.name <- subset(carbontax$country, carbontax$Countryno==treat.ID & carbontax$year==1960) 
synth.name <- paste("synthetic ", treat.name, sep="")
controls.identifier.donor.pool <- c(1, 3, 4, 5, 7, 9, 10, 13, 16, 18, 19, 20, 22, 25)
controls.identifier.full.sample <- c(1:25)
#----------------------------------------------------------------------------------#

#Placebo test in time for 1980 ----------------------------------------------------#
dataprep.out <-
  dataprep(foo = carbontax,
#           predictors = c("GDP_per_capita" , "gas_cons_capita" , "vehicles_capita" , "urban_pop") ,
           predictors = c("GDP_per_capita", "fuel_cons_capita", "gas_share", "urban_pop"),
           predictors.op = "mean" ,
           time.predictors.prior = 1970:1979 ,
           special.predictors = list(
             list("CO2_transport_capita" , 1989 , "mean"),     
             list("CO2_transport_capita" , 1979 , "mean"),    
             list("CO2_transport_capita" , 1969 , "mean")),
           dependent = "CO2_transport_capita",
           unit.variable = "Countryno",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treat.ID,
           controls.identifier = controls.identifier.donor.pool,
           time.optimize.ssr = 1960:1979,
           time.plot = 1960:1990
  )

synth.out <- synth(
  data.prep.obj = dataprep.out,
  method =  "All"
)

#Plot------------------------------------------------------------------------------#
png(paste("output/scm_plots/", treat.name, "_placebo_time_1980.png", sep=""))   
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "Metric tons per capita (CO2 from transport)",
          Xlab = "Year",
#          Ylim = c(0,3),
          Legend = c(paste(treat.name) , paste("synthetic", treat.name)),
          Legend.position = "bottomright"
)
# Add line 
abline(v=1980,lty="dotted",lwd=2)
arrows(1977,1.0,1979,1.0,col="black",length=.1)	
Cex.set <- 1
text(1974,1.0,"Placebo tax",cex=Cex.set)

dev.off()        
#----------------------------------------------------------------------------------------#

#Placebo test in time for 1970 (excluding Poland due to missing GDP data 1960-69) -------#
dataprep.out <-
  dataprep(foo = carbontax,
           predictors = c("GDP_per_capita", "fuel_cons_capita", "gas_share", "urban_pop"),
           predictors.op = "mean" ,
           time.predictors.prior = 1960:1969 ,
           special.predictors = list(
             list("CO2_transport_capita" , 1960:1970 , "mean")
           ),
           dependent = "CO2_transport_capita",
           unit.variable = "Countryno",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treat.ID,
           controls.identifier = controls.identifier.donor.pool[-10],   #10 is id18 (-->Poland)
           time.optimize.ssr = 1960:1969,
           time.plot = 1960:1990
  )

synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "All"
)

#Plot------------------------------------------------------------------------------#
png(paste("output/scm_plots/", treat.name, "_placebo_time_1970.png", sep=""))  
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "Metric tons per capita (CO2 from transport)",
          Xlab = "Year",
#          Ylim = c(0,3),
          Legend = c(paste(treat.name) , paste("synthetic", treat.name)),
          Legend.position = "bottomright"
)
# Add line 
abline(v=1970,lty="dotted",lwd=2)
arrows(1968,2.0,1969.5,2.0,col="black",length=.1)	
Cex.set <- 1
text(1965,2.0,"Placebo tax",cex=Cex.set)


dev.off()      
#----------------------------------------------------------------------------------------#

#End of iteration
}