##Possible Confounder - GDP
dev.off()     #Close plot
rm(list=ls()) #Remove all variables from environment
p_unload(all) #Unload all user packages
cat("\014")   #Clean console
#HEAD------------------------------------------------------#
library(rio)
library(dplyr)
library(tidyr)
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
#--------------------------------------------------------------------#
#Load weight data
weights.fi <- import("output_scm_data/Finland_tables_country_weights.csv", dec=".", sep=",")  
weights.se <- import("output_scm_data/Sweden_tables_country_weights.csv", dec=".", sep=",") 
weights.no <- import("output_scm_data/Norway_tables_country_weights.csv", dec=".", sep=",") 

carbontax <- import("output_scm_data/scm_data.csv", dec=".", sep=",")

relevant.weights.fi <- filter(weights.fi, w.weights>0.001)
relevant.weights.se <- filter(weights.se, w.weights>0.001)
relevant.weights.no <- filter(weights.no, w.weights>0.001)

synth.gdp.fi <- c(rep(0, length(1960:2005)))
synth.gdp.se <- c(rep(0, length(1960:2005)))
synth.gdp.no <- c(rep(0, length(1960:2005)))


#Calc GDP for synthetic 
for(iter in 1:nrow(relevant.weights.fi)){
  iter.country.num <- relevant.weights.fi[iter, 4]
  iter.weight <- relevant.weights.fi[iter, 2]
  iter.carbontax.country <- filter(carbontax, id==iter.country.num & between(year, 1960, 2005))
  iter.gdp.country <- iter.carbontax.country[,"real_gdp_PPP_cap"]
  synth.gdp.fi <- synth.gdp.fi + iter.weight * iter.gdp.country
}

for(iter in 1:nrow(relevant.weights.se)){
  iter.country.num <- relevant.weights.se[iter, 4]
  iter.weight <- relevant.weights.se[iter, 2]
  iter.carbontax.country <- filter(carbontax, id==iter.country.num & between(year, 1960, 2005))
  iter.gdp.country <- iter.carbontax.country[,"real_gdp_PPP_cap"]
#  if((relevant.weights.se$unit.number)!= 18){                                          #Poland is excluded here as there is no data available for 1960-1969 resulting in NA's.
#  synth.gdp.se <- synth.gdp.se + iter.weight * iter.gdp.country
#  }else{
#  synth.gdp.se[11:46] <- synth.gdp.se[11:46] + iter.weight * iter.gdp.country[11:46]
#  }
  if((relevant.weights.se$unit.number[iter])==18){
    synth.gdp.se[11:46] <- synth.gdp.se[11:46] + iter.weight * iter.gdp.country[11:46]
  } else {synth.gdp.se <- synth.gdp.se + iter.weight * iter.gdp.country}
}

for(iter in 1:nrow(relevant.weights.no)){
  iter.country.num <- relevant.weights.no[iter, 4]
  iter.weight <- relevant.weights.no[iter, 2]
  iter.carbontax.country <- filter(carbontax, id==iter.country.num & between(year, 1960, 2005))
  iter.gdp.country <- iter.carbontax.country[,"real_gdp_PPP_cap"]
  if(relevant.weights.se$unit.names[iter]!="Poland"){                                          #Poland is excluded here as there is no data available for 1960-1969 resulting in NA's.
  synth.gdp.no <- synth.gdp.no + iter.weight * iter.gdp.country
  }else{
    synth.gdp.no[11:46] <- synth.gdp.no[11:46] + iter.weight * iter.gdp.country[11:46]
  }
}

#--------------------------------------------------------------------#

#Data preparation
cut.fi <- filter(carbontax, id==6 & between(year, 1960, 2005))
cut.se <- filter(carbontax, id==21 & between(year, 1960, 2005))
cut.no <- filter(carbontax, id==17 & between(year, 1960, 2005))
gdp.fi <- cut.fi[,"real_gdp_PPP_cap"]
gdp.se <- cut.se[,"real_gdp_PPP_cap"]
gdp.no <- cut.no[,"real_gdp_PPP_cap"]


#Plot preparation (Tidy data)
plot.confounder.fi <- cbind(synth.gdp.fi, gdp.fi)
plot.confounder.se <- cbind(synth.gdp.se, gdp.se)
plot.confounder.no <- cbind(synth.gdp.no, gdp.no)

colnames(plot.confounder.fi) <- c("Synth", "Orig")
tidy.fi <- gather(data.frame(plot.confounder.fi), "type", "FIN", 1:2)
colnames(plot.confounder.se) <- c("Synth", "Orig")
tidy.se <- gather(data.frame(plot.confounder.se), "type", "SWE", 1:2)
colnames(plot.confounder.no) <- c("Synth", "Orig")
tidy.no <- gather(data.frame(plot.confounder.no), "type", "NOR", 1:2)  

year <- 1960:2005

plot.full <- cbind(year, tidy.fi, as.numeric(tidy.se[,2]), as.numeric(tidy.no[,2]))
colnames(plot.full) <- c("year", "type", "FIN", "SWE", "NOR")
plot.tidy.gdp <- gather(plot.full, "country", "GDP", 3:5)

library(ggplot2)
#Create plot
ggplot(plot.tidy.gdp) +
  aes(x = year, y = as.numeric(GDP), colour=country, linetype=type) +
  geom_line(size=2) +
  labs(x = "\nYear",
       y = "GDP per capita (PPP) \n",
#       title = "GDP per capita in Finland, Norway, and Sweden 1960-2005",
       colour = "Country",
       linetype = "Data type") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size = 25)) +  
  scale_colour_manual(values = c( "#C4961A", "#FFDB6D", "black")) +
  geom_vline(xintercept = 1990, linetype="dotted")+
  #Define y-axis
  coord_cartesian(ylim=c(0, 45000)) + 
  scale_y_continuous(breaks=seq(0, 45000, 10000)) 
  
ggsave("output_scm_plot/pos_confounder_gdp.png", plot = last_plot(), device = "png",
  #     scale = 1, width = 10, height = 6, units = c("in", "cm", "mm"),
       dpi = 300) #, limitsize = TRUE)

#-------------------------------------EXPORT FOR CO2 PER GDP---------------------------------------------------#NEW-------------------------#
#Data preparation
cut.fi <- filter(carbontax, id==6 & between(year, 1960, 2005))
cut.se <- filter(carbontax, id==21 & between(year, 1960, 2005))
cut.no <- filter(carbontax, id==17 & between(year, 1960, 2005))
co2.fi <- cut.fi[,"CO2_emissions_transport_cap"]
co2.se <- cut.se[,"CO2_emissions_transport_cap"]
co2.no <- cut.no[,"CO2_emissions_transport_cap"]

synth.co2.fi <- import("output_scm_data/Finland_co2_cap_synth.csv", dec=".", sep=",")  
synth.co2.no <- import("output_scm_data/Norway_co2_cap_synth.csv", dec=".", sep=",")  
synth.co2.se <- import("output_scm_data/Sweden_co2_cap_synth.csv", dec=".", sep=",")  


fi.co2.gdp <- co2.fi/gdp.fi
no.co2.gdp <- co2.no/gdp.no
se.co2.gdp <- co2.se/gdp.se
fi.synth.co2.gdp <- synth.co2.fi/synth.gdp.fi*1000
no.synth.co2.gdp <- synth.co2.no/synth.gdp.no*1000
se.synth.co2.gdp <- synth.co2.se/synth.gdp.se*1000
fi.gap.co2.gdp <- fi.synth.co2.gdp-fi.co2.gdp
no.gap.co2.gdp <- no.synth.co2.gdp-no.co2.gdp
se.gap.co2.gdp <- se.synth.co2.gdp-se.co2.gdp


plot.co2gdp <- cbind(year,
                      fi.gap.co2.gdp,
                      no.gap.co2.gdp,
                      se.gap.co2.gdp
                      )

colnames(plot.co2gdp) <- c("year", "FIN", "NOR", "SWE")
#Export final regression data####
export(scm.comp.gdp, file = "output_scm_data/gap_co2_gdp.csv", format = "csv")
#-------------------------------------------------------------------------------#

plot.tidy.co2.gdp <- gather(plot.co2gdp, "country", "CO2perGDP", 2:4)

library(ggplot2)
#Create plot
ggplot(plot.tidy.co2.gdp) +
  aes(x = year, y = as.numeric(CO2perGDP), colour=country) +
  geom_line(size=2) +
  labs(x = "\nYear",
       y = "CO2 emissions from transport per GDP (PPP) \n",
#       title = "Gap in CO2 per GDP per capita in Finland, Norway, and Sweden 1960-2005",
       colour = "Country",
       linetype = "Data type") +
       theme_bw() +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
             text = element_text(size = 25)) +
          scale_colour_manual(values = c( "#C4961A", "#FFDB6D", "black"))+ #+
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
#        text = element_text(size = 25)) +
   geom_vline(xintercept = 1990, linetype="dotted")#+
#  geom_segment(data=NULL,aes(x = 1984, y = 0.075, xend = 1989, yend = 0.075),arrow = arrow(length = unit(0.5, "cm")))+
#  geom_text(data=NULL,aes(size=2, x = 1982, y = 0.075, label = "Treatment"))
  #Define y-axis
#  coord_cartesian(ylim=c(-0.2, 5)) #+ 
#  scale_y_continuous(breaks=seq(0, 45000, 10000)) 

ggsave("output_scm_plot/gap_co2_per_gdp.png", plot = last_plot(), device = "png",
       #     scale = 1, width = 10, height = 6, units = c("in", "cm", "mm"),
       dpi = 300) #, limitsize = TRUE)
