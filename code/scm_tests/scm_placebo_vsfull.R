###################################################
### Figure 10: Path and Gap plot of per capita CO2 Emissions from Transport: Main Results vs. Full Sample 
###################################################

p_load(rio,
       Synth
) 

# setwd()

###Country id
#Full sample: 1:Australia, 2:Austria, 3:Belgium, 4:Canada, 5: Denmark, 6:Finland, 7: France, 8:Germany, 9:Greece, 10:Iceland, 
#11: Ireland, 12:Italy, 13:Japan, 14:Luxembourg, 15:Netherlands, 16:New Zealand, 17:Norway, 18:Poland, 19:Portugal, 20:Spain
#21:Sweden, 22:Switzerland, 23:Turkey, 24:United Kingdom, 25:United States
#--------------------------HEAD-----------------------------------------------#

carbontax <- import("data/scm_data/scm_data.csv", dec=".", sep=",")   
colnames(carbontax) <- c("year", "Countryno", "country", "CO2_transport_capita", "gas_cons_capita", "diesel_cons_capita", "GDP_per_capita", "urban_pop", "vehicles_capita", "cons_ratio", "fuel_cons_capita", "gas_share")
treated <- c(6, 17, 21)
#---------------------------FULL SAMPLE RUNS----------------------------------#

#Runs for treated countries
for(iter in treated)
{ 
#Algorithm settings
treat.ID <- iter
treat.name <- subset(carbontax$country, carbontax$Countryno==treat.ID & carbontax$year==1960) 
synth.name <- paste("synthetic ", treat.name, sep="")
controls.identifier.full.sample <- c(1:25)
#-----------------------------------------------------------------------------#
  
dataprep.out <-
  dataprep(foo = carbontax,
#           predictors = c("GDP_per_capita" , "gas_cons_capita" , "vehicles_capita" , "urban_pop") ,
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
           treatment.identifier = treat.ID,            
           controls.identifier = controls.identifier.full.sample[-treat.ID],
           time.optimize.ssr = 1960:1989,
           time.plot = 1960:2005
  )


synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "All")

#Plot--------------------------------------------------------------------------#
png(paste("output/scm_plots/", treat.name, "_placebo_full_sample.png", sep=""))   

path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "Metric tons per capita (CO2 from transport)",
          Xlab = "Year",
       #   Ylim = c(0,3),
          Legend = c(treat.name, paste(synth.name, "(full sample)")),
          Legend.position = "bottomright"
)
# Add line 
abline(v=1990,lty="dotted",lwd=2)
arrows(1985,1.5,1989,1.5,col="black",length=.1)	
Cex.set <- 1
text(1981,1.5,"Treatment",cex=Cex.set)     

dev.off() 
#-------------------------------------------------------------------------------#

#End of iteration
}