###################################################
### Leave-One-Out: Distribution of the Synthetic Control for Finland, Sweden & Norway
###################################################

###Country id
#Full sample: 1:Australia, 2:Austria, 3:Belgium, 4:Canada, 5: Denmark, 6:Finland, 7: France, 8:Germany, 9:Greece, 10:Iceland, 
#11: Ireland, 12:Italy, 13:Japan, 14:Luxembourg, 15:Netherlands, 16:New Zealand, 17:Norway, 18:Poland, 19:Portugal, 20:Spain
#21:Sweden, 22:Switzerland, 23:Turkey, 24:United Kingdom, 25:United States

p_load(rio,
       Synth,
       dplyr,
       tidyr,
       ggplot2
) 

# setwd()

carbontax <- import("data/scm_data/scm_data.csv", dec=".", sep=",")   

colnames(carbontax) <- c("year", "Countryno", "country", "CO2_transport_capita", "gas_cons_capita", "diesel_cons_capita", "GDP_per_capita", "urban_pop", "vehicles_capita", "cons_ratio", "fuel_cons_capita", "gas_share")

treated <- c(6, 17, 21)
excl.for.FI <- c(3, 13, 16, 22)    
excl.for.NO <- c(3, 13, 16)
excl.for.SE <- c(13, 16, 18, 22, 25)      
exclude.donor.pool <- c(2, 5, 6, 8, 11, 12, 14, 15, 17, 21, 23, 24)
controls.identifier.full.sample <- c(1:25)
controls.identifier.donor.pool <- controls.identifier.full.sample[!controls.identifier.full.sample %in% exclude.donor.pool]

#PREPARE DATA---------------------------------------------------------------####
#Iteration for treated countries
for(iter in treated)
{ 
#Algorithm settings
treat.ID <- iter
treat.name <- subset(carbontax$country, carbontax$Countryno==treat.ID & carbontax$year==1960) 
synth.name <- paste("synthetic ", treat.name, sep="")
colno <- 1

#Set country specific exclution of countries from donor pool
if (treat.ID==6) {excl <- excl.for.FI}
if (treat.ID==17) {excl <- excl.for.NO}
if (treat.ID==21) {excl <- excl.for.SE}
store <- matrix(NA,length(1960:2005),length(excl))

#Iteration for exclution of countries from donor pool
for(exclude_count in excl)
{
dataprep.out <-
  dataprep(foo = carbontax,
#          predictors = c("GDP_per_capita" , "gas_cons_capita" , "vehicles_capita" , "urban_pop") ,
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
           controls.identifier = controls.identifier.donor.pool[!controls.identifier.donor.pool %in% exclude_count],   
           time.optimize.ssr = 1960:1989,
           time.plot = 1960:2005
  )

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "All")

#Save name of excluded country
excl.name <- subset(carbontax$country, carbontax$Countryno==exclude_count & carbontax$year==1960) 

path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "Metric tons per capita (CO2 from transport)",
          Xlab = "Year",
#          Ylim = c(0,3),
          Legend = c(paste(treat.name) , paste("synthetic", treat.name, " excluding ", excl.name)),
          Legend.position = "bottomright"
)
# Add line 
abline(v=1990,lty="dotted",lwd=2)
arrows(1987,1.0,1989,1.0,col="black",length=.1)	
Cex.set <- 1
text(1981,1.5,"VAT + Carbon tax",cex=Cex.set)

#Store iteration results
store[,colno] <- dataprep.out$Y0plot %*% synth.out$solution.w
colno <- colno + 1
#End of exclution iteration
}
export(dataprep.out$Y1plot, file = paste("output/scm_data/", treat.name, "_co2_cap.csv", sep=""), format = "csv")
export(store, file = paste("output/scm_data/", treat.name, "_leave_one_out_excluded.csv", sep=""), format = "csv")
rm(store)
#End of treated countries iteration
}

#DATA-FRAME-----------------------------------------------------------------####
#Create leave_one_out_data
#Structure: Year, sweden, synth_sweden, excl_a, excl_b, ...
leave.FI <- import("output/scm_data/Finland_leave_one_out_excluded.csv", dec=".", sep=",")   
leave.NO <- import("output/scm_data/Norway_leave_one_out_excluded.csv", dec=".", sep=",")   
leave.SE <- import("output/scm_data/Sweden_leave_one_out_excluded.csv", dec=".", sep=",")   
co2.FI <- import("output/scm_data/Finland_co2_cap.csv", dec=".", sep=",", skip=1)  
co2.NO <- import("output/scm_data/Norway_co2_cap.csv", dec=".", sep=",", skip=1)  
co2.SE <- import("output/scm_data/Sweden_co2_cap.csv", dec=".", sep=",", skip=1)
co2.synth.FI <- import("output/scm_data/Finland_co2_cap_synth.csv", dec=".", sep=",", skip=1)  
co2.synth.NO <- import("output/scm_data/Norway_co2_cap_synth.csv", dec=".", sep=",", skip=1)
co2.synth.SE <- import("output/scm_data/Sweden_co2_cap_synth.csv", dec=".", sep=",", skip=1)


year <- 1960:2005
scm.data <- carbontax <- import("data/scm_data/scm_data.csv", dec=".", sep=",") 
fi.data <- filter(scm.data, country=="Finland" & between(year, 1960, 2005))
no.data <- filter(scm.data, country=="Norway" & between(year, 1960, 2005))
se.data <- filter(scm.data, country=="Sweden" & between(year, 1960, 2005))
plot.data.FI <- cbind(year, co2.FI, co2.synth.FI, leave.FI)
plot.data.NO <- cbind(year, co2.NO, co2.synth.NO, leave.NO)
plot.data.SE <- cbind(year, co2.SE, co2.synth.SE, leave.SE)
# excl.for.FI <- c(3, 13, 16, 22)    #
# excl.for.NO <- c(1, 7, 9)
# excl.for.SE <- c(13, 16, 18, 22, 25) 
colnames(plot.data.FI) <- c("year", "Finland", "synthFinland","excl_Belgium", "excl_Japan", "excl_NewZealand", "excl_Switzerland") 
colnames(plot.data.NO) <- c("year", "Norway", "synthNorway", "excl_Belgium", "excl_Japan", "excl_NewZealand")
colnames(plot.data.SE) <- c("year", "Sweden", "synthSweden", "excl_Japan", "excl_NewZealand", "excl_Poland", "excl_Switzerland", "excl_UnitedStates")
export(plot.data.FI, file = paste("output/scm_data/Finland_leave_one_out.csv", sep=""), format = "csv")
export(plot.data.NO, file = paste("output/scm_data/Norway_leave_one_out.csv", sep=""), format = "csv")
export(plot.data.SE, file = paste("output/scm_data/Sweden_leave_one_out.csv", sep=""), format = "csv")



#PLOT-----------------------------------------------------------------------####
#Load leave_one_out_data
plot.data.FI <- import("output/scm_data/Finland_leave_one_out.csv", dec=".", sep=",")   
plot.data.NO <- import("output/scm_data/Norway_leave_one_out.csv", dec=".", sep=",") 
plot.data.SE <- import("output/scm_data/Sweden_leave_one_out.csv", dec=".", sep=",") 

#Finland
fi.excl.plot <- gather(plot.data.FI, "variable", "value", 2:(length(excl.for.FI)+3))
ggplot(fi.excl.plot) +
  aes(x = year, y = as.numeric(value), colour=variable) +   
  geom_line(size=2) +
  geom_vline(xintercept = 1990, linetype="dotted") +
  labs(x = "\nYear",
       y = " Metric tons per capita (CO2 from transport) \n",
       title = "",
       colour = ""
       ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size = 25)) +
  scale_colour_manual(values = c(rep("grey", length(excl.for.FI)), "blue", "red")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
ggsave("output/scm_plots/Finland_placebo_leave_one_out.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 7, height = 5, units = "cm",
       dpi = 300) #, limitsize = TRUE)

#Norway
no.excl.plot <- gather(plot.data.NO, "variable", "value", 2:(length(excl.for.NO)+3))
ggplot(no.excl.plot) +
  aes(x = year, y = as.numeric(value), colour=variable) +  
  geom_line(size=2) +
  geom_vline(xintercept = 1990, linetype="dotted")+
  labs(x = "\nYear",
       y = " Metric tons per capita (CO2 from transport) \n",
       title = "",
       colour = ""
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size = 25)) +
  scale_colour_manual(values = c(rep("grey", length(excl.for.NO)), "blue", "red")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
ggsave("output/scm_plots/Norway_placebo_leave_one_out.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 7, height = 5, units = "cm",
       dpi = 300) #, limitsize = TRUE)

#Sweden
se.excl.plot <- gather(plot.data.SE, "variable", "value", 2:(length(excl.for.SE)+3))
ggplot(se.excl.plot) +
  aes(x = year, y = as.numeric(value), colour=variable) +  
  geom_line(size=2) +
  geom_vline(xintercept = 1990, linetype="dotted")+
  labs(x = "\nYear",
       y = " \n",
       title = "",
       colour = ""
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size = 25)) +
  scale_colour_manual(values = c(rep("grey", length(excl.for.SE)), "blue", "red")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

  
ggsave("output/scm_plots/Sweden_placebo_leave_one_out.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 7, height = 5, units = "cm",
       dpi = 300) #, limitsize = TRUE)
#-----------------------------------------------------------------------------------#