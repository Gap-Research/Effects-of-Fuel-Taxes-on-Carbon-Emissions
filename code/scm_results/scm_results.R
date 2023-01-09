###################################################
### SCM Analysis
###################################################
###Country id
#Full sample: 1:Australia, 2:Austria, 3:Belgium, 4:Canada, 5: Denmark, 6:Finland, 7: France, 8:Germany, 9:Greece, 10:Iceland, 
#11: Ireland, 12:Italy, 13:Japan, 14:Luxembourg, 15:Netherlands, 16:New Zealand, 17:Norway, 18:Poland, 19:Portugal, 20:Spain
#21:Sweden, 22:Switzerland, 23:Turkey, 24:United Kingdom, 25:United States

p_load(Synth,
       rio,
       dplyr
      )   

# setwd()

carbontax <- import("data/scm_data/scm_data.csv", dec=".", sep=",")   

#Adjust to old code
colnames(carbontax) <- c("year", "Countryno", "country", "CO2_transport_capita", "gas_cons_capita", "diesel_cons_capita", "GDP_per_capita", "urban_pop", "vehicles_capita", "cons_ratio", "fuel_cons_capita", "gas_share")
treated <- c(6, 17, 21)

#Complete Run
for(iter in treated)
{ 
 #Algorithm settings
 treat.ID <- iter    
 treat.name <- subset(carbontax$country, carbontax$Countryno==treat.ID & carbontax$year==1960) 
 synth.name <- paste("synthetic ", treat.name, sep="")
 controls.identifier.donor.pool <- c(1, 3, 4, 5, 7, 9, 10, 13, 16, 18, 19, 20, 22, 25) 
 controls.identifier.full.sample <- c(1:25)

 predictor.set1 <- c("GDP_per_capita",
              #      "gas_cons_capita",
              #      "diesel_cons_capita",
              #      "share",
              "fuel_cons_capita",
              "gas_share",
                    "urban_pop"
                  #  "vehicles_capita"
                     )
 special.predictors.set1 <-   list(
   list("CO2_transport_capita" , 1989 , "mean"),     
   list("CO2_transport_capita" , 1979 , "mean"),  
   list("CO2_transport_capita" , 1969 , "mean")
   )
###################################################
#Data preparation 
###################################################
 dataprep.out <-                                             
              dataprep(foo = carbontax,                    
                       # Predictor settings
                       predictors = predictor.set1,
                       predictors.op = "mean" ,            
                       time.predictors.prior = 1980:1989 ,   
                       special.predictors = special.predictors.set1,
                       # General settings
                       dependent = "CO2_transport_capita",     
                       unit.variable = "Countryno",           
                       unit.names.variable = "country",      
                       time.variable = "year",                 
                       treatment.identifier = treat.ID,            
                       controls.identifier = controls.identifier.donor.pool,     
                       time.optimize.ssr = 1960:1989,         
                       time.plot = 1960:2005                 
                      )
	
#-----SYNTH-----------------------------------------------------------#####
 synth.out <- synth(data.prep.obj = dataprep.out,
                    method = "All")                   
	
#-----SYNTH------TABLES---------------------------------------------------####
 synth.tables <- synth.tab(dataprep.res = dataprep.out,  
                           synth.res = synth.out
                           )

#Export synthetic results
synth.results <-  dataprep.out$Y0plot %*% synth.out$solution.w
export(synth.results, file = paste("output/scm_data/", treat.name, "_co2_cap_synth.csv", sep=""), format = "csv")
  

###################################################
### Table 1: CO2 Emissions From Transport Predictor Means Before Tax Reform	
###################################################
pred_table <- synth.tables$tab.pred[1:(length(predictor.set1)+length(special.predictors.set1)), ]    
write.table(pred_table, file = paste("output/scm_data/", treat.name, "_tables_predictors.csv", sep=""))

library(stargazer)
out.pred_table <- pred_table

rownames(out.pred_table) <- c("Real GDP (PPP) per capita",
                              "Fuel consumption per capita",
                              "Share of gasoline consumption",
                              "Urban population",
                       #       "Vehicles per 1000 capita",
                           #   "share",
                              "CO2 emissions from transport per capita (1989)",
                              "CO2 emissions from transport per capita (1979)",
                              "CO2 emissions from transport per capita (1969)"
                           )

stargazer(out.pred_table,
          title = paste("CO2 Emissions from Transport Predictor Means before Tax Reform in ", treat.name, sep=""),
          font.size="tiny",
           digits = 4,
          type="text",
          header = FALSE,
         #  flip=TRUE,
         out=paste("output/scm_tables/", treat.name, "_tables_predictors.txt", sep="")
         )
 
###################################################
### Table 2: Country Weights in Synthetic Sweden
###################################################
synth.tables$tab.w[1:length(controls.identifier.donor.pool), ]      
write.table(synth.tables$tab.w[1:13, ], file= paste("output/scm_data/", treat.name, "_tables_country_weights.csv", sep=""))
 
library(stargazer)
out.weight_table <- as.matrix(synth.tables$tab.w[1:length(controls.identifier.donor.pool),1:2])                    
colnames(out.weight_table) <- c("Weight", "Country")        
stargazer(out.weight_table,
         title = paste("Country Weights in Synthetic ", treat.name, sep=""),
         font.size="tiny",
         digits = 4,
         type="text",
#         type="text",
         header = FALSE,
         rownames=FALSE,
         #  flip=TRUE,
         out=paste("output/scm_tables/", treat.name, "_tables_country_weights.txt", sep="")
         )
 
###################################################
### Figure 4: Path Plot of per capita CO2 Emissions from Transport  
###################################################

png(paste("output/scm_plots/", treat.name, "_path_plot.png", sep=""))   
#cex.lab <- 2.5
#cex.axis <- 2.5
par(cex.axis=1.3)
par(cex.lab=1.3)
path.plot(synth.res = synth.out,               
           dataprep.res = dataprep.out,
           Ylab = "Metric tons per capita (CO2 from transport)",
           Xlab = "Year",
        #   Legend = NULL,    
                Legend = c(treat.name, synth.name),
          Legend.position = "bottomright",
           Main = "")
# Add vertical line for treatment
abline(v=1990,lty="dotted",lwd=2)
arrows(1987,1.0,1989,1.0,col="black",length=.1)	
Cex.set <- 1.2
text(1982,1.0,"Treatment",cex=Cex.set)
# #legend("topleft", c(treat.name, synth.name), cex=1.5, pch=1, pt.cex = 2)
# #ax.get_legend().remove("Treated", "Synth")
# legend.remove()
#  legend("topleft", legend=c(treat.name, synth.name),
#         col=c("black", "black"), lty=1:2, cex=1.2)
# # cex.lab <- 2.5
# cex.axis <- 2.5

dev.off()      

###################################################
### Figure 5: Gap in per capita CO2 Emissions from Transport between Sweden and Synthetic Sweden   
###################################################
png(paste("output/scm_plots/", treat.name, "_gap_plot.png", sep=""))
par(cex.axis=1.3)
par(cex.lab=1.3)
gaps.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "Gap in metric tons per capita (CO2 from transport)",
           Xlab = "Year",
          #Legend.position = "topleft",
          Main = ""  #treat.name
           )

# Add vertical line for treatment
abline(v=1990,lty="dotted",lwd=2)
arrows(1987,-0.4,1989,-0.4,col="black",length=.1)	
Cex.set <- 1.2
text(1982,-0.4,"Treatment",cex=Cex.set)

dev.off()    

dataprep.out$Y1plot
export(dataprep.out$Y1plot, file = paste("output/scm_data/", treat.name, "_synth_co2.csv", sep=""), format = "csv")

gaps <- dataprep.out$Y1plot[31:46,] - (dataprep.out$Y0plot[31:46,] %*% synth.out$solution.w)
mean.gaps <- mean(gaps)
mean.percent <- mean.gaps/mean((dataprep.out$Y0plot[31:46,] %*% synth.out$solution.w))                                 #mean(dataprep.out$Y1plot[31:46,])
export(gaps, file = paste("output/scm_data/", treat.name, "_gaps.csv", sep=""), format = "csv")
export(data.frame(mean.gaps), file = paste("output/scm_data/", treat.name, "_mean_gaps.csv", sep=""), format = "csv")
export(data.frame(mean.percent), file = paste("output/scm_data/", treat.name, "_mean_percent.csv", sep=""), format = "csv")

#End of iteration
}

