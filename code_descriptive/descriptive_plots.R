####Data preperation#
##Clean-----------------------------------------------------------------------------------####
dev.off()     #Close plot
rm(list=ls()) #Remove all variables from environment
p_unload(all) #Unload all user packages
cat("\014")   #Clean console
##Header----------------------------------------------------------------------------------####
#install.packages("pacman")
require(pacman)      ##library(pacman)
#Load packages
p_load(rio,       #Input and Output: import()- & export()-function
       tidyr,     #gather()-function for data prep
       ggplot2    #ggplot()-function for plots
       )     
#Set working directory
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
##-------------------------------------------------------------------------------------------#
year <- c(1978:2016)
##-------------------------------------------------------------------------------------------#
###Figures on prices and taxes
##Finland
#Figure 1
fi.data.fig1 <- import("output_reg_data/fi_des_fig1.csv", dec=",")
colnames(fi.data.fig1) <- c("Total price", "Total tax", "VAT")
fi.data.fig1.year <- cbind(year, fi.data.fig1)
fi.tidy.data.fig1 <- gather(fi.data.fig1.year, "variable", "value", 2:4)
ggplot(fi.tidy.data.fig1) +
    aes(x = year, y = as.numeric(value), linetype=variable) +   
    geom_line(size=2) +
    labs(x = "\nYear",
         y = "Real prices and taxes (in 2005EUR per litre) \n",
     #    title = "Prices and taxes in Finland 1978-2016",
         linetype = "Prices and Taxes") +
    theme_bw() +
    scale_linetype_manual(values = c(3,1,6)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          text = element_text(size = 25)
          ) 
#  scale_linetype_manual("Prices and Taxes",values=c("Total price"=3,"Total tax"=1, "VAT"=6))
 # scale_shape_discrete(legend=F)
    #name  ="Prices and Taxes",
     #                   breaks=c("Total price", "Total tax", "VAT"),
      #                  labels=c("Woman", "Man"))
ggsave("output_descriptive/fig1_fi.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 7, height = 7, units = "cm",
       dpi = 300) #, limitsize = TRUE)

# + geom_line(aes(y = Mean, color = "Medelvärde",linetype = "Medelvärde"), 
#             size = 1.5, alpha = 1)  

#Figure 2
fi.data.fig2 <- import("output_reg_data/fi_des_fig2.csv", dec=",")
colnames(fi.data.fig2) <- c("Total gasoline tax", "Total diesel tax", "Tax difference")
fi.data.fig2.year <- cbind(year, fi.data.fig2)
fi.tidy.data.fig2 <- gather(fi.data.fig2.year, "variable", "value", 2:4)
ggplot(fi.tidy.data.fig2) +
    aes(x = year, y = as.numeric(value), linetype=variable) +       
    geom_line(size=2) +
    labs(x = "\nYear",
         y = "Real prices and taxes (in 2005EUR per litre) \n",
     #    title = "Prices and taxes in Finland 1978-2016",
         linetype = "Prices and Taxes") +
    theme_bw() +
    scale_linetype_manual(values = c(6,3,1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          text = element_text(size = 25))
ggsave("output_descriptive/fig2_fi.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 7, height = 7, units = "cm",
       dpi = 300) #, limitsize = TRUE)

#Norway
#Figure 1
no.data.fig1 <- import("output_reg_data/no_des_fig1.csv", dec=",")
colnames(no.data.fig1) <- c("Total price", "Total tax", "VAT")
no.data.fig1.year <- cbind(year, no.data.fig1)
no.tidy.data.fig1 <- gather(no.data.fig1.year, "variable", "value", 2:4)
ggplot(no.tidy.data.fig1) +
  aes(x = year, y = as.numeric(value), linetype=variable) +   
  geom_line(size=2) +
  labs(x = "\nYear",
       y = "Real prices and taxes (in 2005NOK per litre) \n",
 #      title = "Prices and taxes in Norway 1978-2016",
       linetype = "Prices and Taxes") +
  theme_bw() +
  scale_linetype_manual(values = c(3,1,6)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size = 25))
ggsave("output_descriptive/fig1_no.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 7, height = 7, units = "cm",
       dpi = 300) #, limitsize = TRUE)

#Figure 2
no.data.fig2 <- import("output_reg_data/no_des_fig2.csv", dec=",")
colnames(no.data.fig2) <- c("Total gasoline tax", "Total diesel tax", "Tax difference")
no.data.fig2.year <- cbind(year, no.data.fig2)
no.tidy.data.fig2 <- gather(no.data.fig2.year, "variable", "value", 2:4)
ggplot(no.tidy.data.fig2) +
  aes(x = year, y = as.numeric(value), linetype=variable) +       
  geom_line(size=2) +
  labs(x = "\nYear",
       y = "Real prices and taxes (in 2005NOK per litre) \n",
  #     title = "Prices and taxes in Norway 1978-2016",
       linetype = "Prices and Taxes") +
  theme_bw() +
  scale_linetype_manual(values = c(6,3,1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size = 25))
ggsave("output_descriptive/fig2_no.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 7, height = 7, units = "cm",
       dpi = 300) #, limitsize = TRUE)


##Sweden
#Figure 1 (with energy and carbontax without vat)
se.data.fig1 <- import("output_reg_data/se_des_fig1.csv", dec=",")
colnames(se.data.fig1) <- c("Total price", "Total tax", "VAT", "Carbon tax with VAT", "Energy tax with VAT")
se.data.fig1.year <- cbind(year, se.data.fig1) 
se.tidy.data.fig1 <- gather(se.data.fig1.year, "variable", "value", 2:6)
ggplot(se.tidy.data.fig1) +
    aes(x = year, y = as.numeric(value), colour=variable, linetype=variable) +   
    geom_line(size=2) +
    labs(x = "\nYear",
         y = "Real prices and taxes (in 2005SEK per litre) \n",
    #     title = "Prices and taxes in Sweden 1978-2016",
         colour = "Prices and taxes",
    linetype="Prices and Taxes"
         ) +
    theme_bw() +
   scale_colour_manual(values = c("black", "#FFDB6D", "black", "#C4961A", "black")) +
  scale_linetype_manual(values = c(1,1,3,1,6)) + 
#  scale_linetype_discrete(breaks=c(1,3,6)) +
  labs(color  = "Prices and Taxes", linetype = "Prices and Taxes") +
#scale_colour(guide = FALSE) +
 # scale_linetype(guide = FALSE) +
#  guides(colour = guide_legend(override.aes = list(colour = c("black", "#FFDB6D", "black", "#C4961A", "black"), linetype = c(1,1,3,1,6)))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          text = element_text(size = 25))
ggsave("output_descriptive/fig1_se.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 10, height = 7, units = "cm",
       dpi = 300) #, limitsize = TRUE)

#Figure 2
se.data.fig2 <- import("output_reg_data/se_des_fig2.csv", dec=",")
colnames(se.data.fig2) <- c("Total gasoline tax", "Total diesel tax", "Tax difference")
se.data.fig2.year <- cbind(year, se.data.fig2)
se.tidy.data.fig2 <- gather(se.data.fig2.year, "variable", "value", 2:4)
ggplot(se.tidy.data.fig2) +
    aes(x = year, y = as.numeric(value), linetype=variable) +
    geom_line(size=2) +
    labs(x = "\nYear",
         y = "Real prices and taxes (in 2005SEK per litre) \n",
    #     title = "Prices and taxes in Sweden 1978-2016",
         linetype = "Prices and Taxes") +
    theme_bw() +
    scale_linetype_manual(values = c(6,3,1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          text = element_text(size = 25))
ggsave("output_descriptive/fig2_se.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 7, height = 7, units = "cm",
       dpi = 300) #, limitsize = TRUE)

#-----------------------------------------------------------------------------#
#Fig. 3: Tax comparison
se.tidy.data.fig3 <- se.tidy.data.fig2
se.tidy.data.fig3[,3] <- as.numeric(se.tidy.data.fig3[,3])/7.473088*0.80412   #2005SEK to 2005EUR
fi.tidy.data.fig3 <- fi.tidy.data.fig2
no.tidy.data.fig3 <- no.tidy.data.fig2
no.tidy.data.fig3[,3] <- as.numeric(no.tidy.data.fig3[,3])/6.4425*0.80412    #2005SEK to 2005EUR

fig3 <- cbind(se.tidy.data.fig3, fi.tidy.data.fig3$value, no.tidy.data.fig3$value)
colnames(fig3) <- c("year", "variable", "SWE", "FIN", "NOR")
tidy.fig <- gather(fig3, "country", "value", 3:5)

ggplot(tidy.fig) +
  aes(x = year, y = as.numeric(value), colour=country, linetype=variable) +
  geom_line(size=2) +
  labs(x = "\nYear",
       y = "Real fuel prices and taxes (in 2005EUR per litre) \n",
 #      title = "Real fuel prices and taxes in Finland, Norway, and Sweden 1978-2016",
       colour = "Country",
       linetype = "Prices and Taxes") +
  theme_bw() +
  scale_colour_manual(values = c("#FFDB6D", "#C4961A", "black")) +
  scale_linetype_manual(values = c(6,3,1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size = 25))
ggsave("output_descriptive/fig3.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 12, height = 7, units = "cm",
       dpi = 300) #, limitsize = TRUE)


#----------------------------------------------------CONSUMPTION DATA----------------------------------#
#-----------------------------------------------------------------------------#
#Figure 4: Gasoline and diesel consumption (per capita)
se.gas.con <- import("output_reg_data/se_des_fig3.csv", dec=",")
fi.gas.con <- import("output_reg_data/fi_des_fig3.csv", dec=",")
no.gas.con <- import("output_reg_data/no_des_fig3.csv", dec=".")
colnames(no.gas.con) <- c("Gasoline", "Diesel")
tidy.no <- gather(no.gas.con, "fuel", "NOR", 1:2)
colnames(se.gas.con) <- c("Gasoline", "Diesel")
tidy.se <- gather(se.gas.con, "fuel", "SWE", 1:2)
colnames(fi.gas.con) <- c("Gasoline", "Diesel")
tidy.fi <- gather(fi.gas.con, "fuel", "FIN", 1:2)

con.data <- cbind(year, tidy.se, as.numeric(tidy.fi[,2]), as.numeric(tidy.no[,2]))
colnames(con.data) <- c("year", "fuel", "SWE", "FIN", "NOR")
tidy.con.data <- gather(con.data, "country", "Con", 3:5)

ggplot(tidy.con.data) +
        aes(x = year, y = as.numeric(Con), colour=country, linetype=fuel) +
        geom_line(size=2) +
        labs(x = "\nYear",
             y = "Road fuel consumption per capita (in kg of oil equivalent) \n",
    #         title = "Road fuel consumption per capita in Finland, Norway, and Sweden 1978-2016",
             colour = "Country",
             linetype = "Fuel type") +
        theme_bw() +
        scale_colour_manual(values = c("#FFDB6D", "#C4961A", "black")) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
              text = element_text(size = 25))
ggsave("output_descriptive/fig4.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 12, height = 7, units = "cm",
       dpi = 300) #, limitsize = TRUE)


# #--------------------------------------------REGRESSION DATA-------------------------------------------#
# fi.reg.data <- import("output_reg_data/fi_reg_file.csv")             #Load prepared data (Finland)
# se.reg.data <- import("output_reg_data/se_reg_file_carbon.csv")      #Load prepared data (Sweden)
# no.reg.data <- import("output_reg_data/no_reg_file.csv")      #Load prepared data (Finland)
# 
# #------------------------------------------------------#
# #Urban population                                               
# urban.se.reg.data <- se.reg.data[,c(1,8)]
# urban.fi.reg.data <- fi.reg.data[,c(1,8)]
# urban.no.reg.data <- no.reg.data[,c(1,8)]
# 
# colnames(urban.se.reg.data) <- c("year", "SWE")
# colnames(urban.se.reg.data) <- c("year", "FIN")
# colnames(urban.no.reg.data) <- c("year", "NOR")
# 
# urban.data <- cbind(urban.se.reg.data, as.numeric(urban.fi.reg.data[,2]), as.numeric(urban.no.reg.data[,2]))
# colnames(urban.data) <- c("year", "SWE", "FIN", "NOR")
# tidy.urban.data <- gather(urban.data, "country", "urban_pop", 2:4)
# 
# ggplot(tidy.urban.data) +
#         aes(x = year, y = as.numeric(urban_pop), colour=country) +
#         geom_line(size=2) +
#         labs(x = "\nYear",
#              y = "Urban population (in percentage) \n",
#            #  title = "Urban population in Finland, Norway, and Sweden 1978-2016",
#              colour = "Country") +
#         theme_bw() +
#     scale_colour_manual(values = c("#FFDB6D", "#C4961A", "black")) + 
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
#           text = element_text(size = 20))
# ggsave("output_descriptive/reg_urban_pop.png", plot = last_plot(), device = "png", path = NULL,
#        #   scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
#        dpi = 300) #, limitsize = TRUE)
#------------------------------------------------------#
# #Unemployment rate                                            
# unemp.se.reg.data <- se.reg.data[,c(1,9)]
# unemp.fi.reg.data <- fi.reg.data[,c(1,9)]
# unemp.no.reg.data <- no.reg.data[,c(1,9)]
# 
# colnames(unemp.se.reg.data) <- c("year", "unemployment")
# colnames(unemp.fi.reg.data) <- c("year", "unemployment")
# colnames(unemp.no.reg.data) <- c("year", "unemployment")
# 
# unemp.data <- cbind(unemp.se.reg.data, as.numeric(unemp.fi.reg.data[,2]), as.numeric(unemp.no.reg.data[,2]))
# colnames(unemp.data) <- c("year", "SWE", "FIN", "NOR")
# tidy.unemp.data <- gather(unemp.data, "country", "unemp", 2:4)
# 
# ggplot(tidy.unemp.data) +
#         aes(x = year, y = as.numeric(unemp), colour=country) +
#         geom_line(size=2) +
#         labs(x = "\nYear",
#              y = "Unemployment rate (in percentage) \n",
#    #          title = "Unemplyoment rate in Finland, Norway, and Sweden 1978-2016",
#              colour = "Country") +
#         theme_bw() +
#     scale_colour_manual(values = c("#FFDB6D", "#C4961A", "black")) +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
# ggsave("output_reg_plot/reg_unemp.png", plot = last_plot(), device = "png", path = NULL,
#        #   scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
#        dpi = 300) #, limitsize = TRUE)
# #------------------------------------------------------#
# ##GDP
# gdp.se.reg.data <- se.reg.data[,c(1,7)]
# gdp.fi.reg.data <- fi.reg.data[,c(1,7)]
# gdp.no.reg.data <- no.reg.data[,c(1,7)]
# 
# gdp <- cbind(gdp.se.reg.data, gdp.fi.reg.data, gdp.no.reg.data)
# colnames(gdp) <- c("year", "SWE", "FIN", "NOR")
# tidy.gdp<- gather(gdp, "country", "value", 2:4)
# #Sweden
# ggplot(gdp.se.reg.data) +
#         aes(x = year, y = as.numeric(real_gdp_cap_1000)) +
#         geom_point()+
#         geom_line(size=2) +
#         labs(x = "\nYear",
#              y = "Real GDP per capita (in 2005SEK, PPP) \n",
#   #           title = "Real GDP per capita in Sweden 1978-2016",
#              colour = "Country") +
#         theme_bw() #+
# # ggsave("output_graphical/reg_gdp_se.png", plot = last_plot(), device = "png", path = NULL,
# #        #   scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
# #        dpi = 300) #, limitsize = TRUE)
# 
# #Finland
# ggplot(gdp.fi.reg.data) +
#         aes(x = year, y = as.numeric(real_gdp_cap_1000)) +
#         geom_point()+
#         geom_line(size=2) +
#         labs(x = "\nYear",
#              y = "Real GDP per capita (in 2005EUR, PPP) \n",
#           #   title = "Real GDP per capita in Finland 1978-2016",
#              colour = "Country") +
#         theme_bw() #+
# # ggsave("output_graphical/reg_gdp_fi.png", plot = last_plot(), device = "png", path = NULL,
# #        #   scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
# #        dpi = 300) #, limitsize = TRUE)
# 
# #Norway
# ggplot(gdp.no.reg.data) +
#   aes(x = year, y = as.numeric(real_gdp_cap_1000)) +
#   geom_point()+
#   geom_line(size=2) +
#   labs(x = "\nYear",
#        y = "Real GDP per capita (in 2005NOK, PPP) \n",
#   #     title = "Real GDP per capita in Norway 1978-2016",
#        colour = "Country") +
#   theme_bw() #+
# 
# #------------------------------------------------------#
# #GDP (currency adjusted) comparison
# #Adjust GDP in currency for comparison - 2005SEK --> 2005EUR               
# #0.80412EUR/USD     #7.473088SEK/USD    #6.4425NOK/USD
# gdp.se.reg.euro <- as.numeric(gdp.se.reg.data[,2])/7.473088*0.80412
# gdp.no.reg.euro <- as.numeric(gdp.no.reg.data[,2])/6.4425*0.80412  
# 
# gdp <- cbind(gdp.fi.reg.data, gdp.se.reg.euro, gdp.no.reg.euro)
# colnames(gdp) <- c("year", "FIN", "SWE", "NOR")
# tidy.gdp <- gather(gdp, "country", "gdp", 2:4)
# 
# ggplot(tidy.gdp) +                                                         
#          aes(x = year, y = as.numeric(gdp), colour = country) +
#          geom_point()+
#          geom_line(size=2) +
#          labs(x = "\nYear",
#               y = "Real GDP per capita  (in 2005EUR, PPP) \n",
#    #           title = "Real GDP per capita in Finland, Norway, and Sweden 1978-2016",
#               colour = "Country") +
#          theme_bw() +
#          scale_colour_manual(values = c("#FFDB6D", "#C4961A", "black")) +
#          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
# ggsave("output_reg_plot/reg_gdp_euro2005.png", plot = last_plot(), device = "png", path = NULL,
# #   scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
# dpi = 300) #, limitsize = TRUE)
# #------------------------------------------------------#
# dev.off()    #close plot
# #------------------------------------------------------#

#VMT and emission factors 1990-2015 (2 plots)
#Figure 1
require(dplyr)
require(tidyr)

vmt.import <- import("data_input/eu_vmt/Traffic_volume/export_enerdata_8658_030627.csv", dec=",")
colnames(vmt.import) <- c("item", "code", "unit", "year", "value", "s_code", "note", "data_type")
vmt.filter <- filter(vmt.import, data_type=="Car traffic" & between(year, 1990, 2015))

pop.import <- import("data_input/eu_vmt/Traffic_volume/sample_pop.csv", dec=",")
fi.pop <- pop.import[75,35:60]
no.pop <- pop.import[177,35:60]
se.pop <- pop.import[223,35:60]

vmt <- vmt.filter[,c(4,2,5)]


#for(iter in 1:3){                              #nrow(vmt)){
  for (xtime in 1:26){vmt$value[xtime] <- as.numeric(vmt$value[xtime])/fi.pop[xtime]*10^9}
  for (ytime in 1:26){vmt$value[26+ytime] <- as.numeric(vmt$value[26+ytime])/no.pop[ytime]*10^9}
  for (ztime in 1:26){vmt$value[52+ztime] <- as.numeric(vmt$value[52+ztime])/se.pop[ztime]*10^9}
  # if(vmt$code[iter]=="FI"){ 
  #   for (fi.time < 26){vmt$value[itime]/fi.pop[itime]
  #       fi.time <- fi.time+1
  #     }
  #   }
  # if(vmt$code[iter]=="NO"){
  #   for (itime in 1:26){vmt$value[26+itime]/no.pop[itime]}
  #   }
  # else{
  #   for (itime in 1:26){vmt$value[52+itime]/se.pop[itime]}
  #   }
#}

#colnames(vmt) <- c("year", "code", "value")
tidy.vmt <- vmt
#tidy.vmt.pop <- vmt.pop
#tidy.vmt <- gather(vmt, "code", "value", 2:3)
ggplot(tidy.vmt) +
  aes(x = year, y = as.numeric(value), colour=code) +   
  geom_line(size=2) +
  labs(x = "\nYear",
       y = "Vehicles-Miles-Travelled (VMT) per capita (in pkm) \n",
 #      title = "Prices and taxes in Finland 1978-2016",
  #     linetype = "Sample countries"
       colour = "Country") +
  theme_bw() +
  scale_colour_manual(values = c("#FFDB6D", "#C4961A", "black")) +
 # scale_linetype_manual(values = c(3,1,6)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size = 25))
ggsave("output_descriptive/vmt.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 12, height = 7, units = "cm",
       dpi = 300) #, limitsize = TRUE)

###New plot: Emission Factors
EF.import <- import("data_input/eu_EF/EF_prep_csv.csv", dec=",")
#EF.import.cut <- EF.import    #for HBEFA Version 4.2
EF.import.cut <- EF.import[1:48,1:7]    #old for HBEFA 3.2
# 


#CO2 EF Plot ------ Prepared for Version 3.2
no.EF.CO2 <- filter(EF.import.cut, Component == "CO2(rep.)" & Case== "NOR" & between(Year, 1990, 2015))
se.EF.CO2 <- filter(EF.import.cut, Component == "CO2(rep.)" & Case== "SWE" & between(Year, 1990, 2015))
no.EF.NOx <- filter(EF.import.cut, Component == "NOx" & Case== "NOR" & between(Year, 1990, 2015))
se.EF.NOx <- filter(EF.import.cut, Component == "NOx" & Case== "SWE" & between(Year, 1990, 2015))

# #CO2 EF Plot ------ Prepared for Version 4.1
# no.EF.CO2 <- filter(EF.import.cut, emission_type == "CO2(total)" & country== "NO" & (fuel_type == "petrol (4S)" | fuel_type == "diesel") & between(year, 1990, 2015))
# se.EF.CO2 <- filter(EF.import.cut, emission_type == "CO2(total)" & country== "SE" & (fuel_type == "petrol (4S)" | fuel_type == "diesel") & between(year, 1990, 2015))
# no.EF.NOx <- filter(EF.import.cut, emission_type == "NOx" & country== "NO" & (fuel_type == "petrol (4S)" | fuel_type == "diesel") & between(year, 1990, 2015))
# se.EF.NOx <- filter(EF.import.cut, emission_type == "NOx" & country== "SE" & (fuel_type == "petrol (4S)" | fuel_type == "diesel") & between(year, 1990, 2015))


# #Algorithm for weighted average   only needed for HBEFA 4.2
# count <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51)
# for (iter in count){
#      no.EF.CO2.weighted[iter] <- no.EF.CO2$share[iter]*no.EF.CO2$efa[iter] + no.EF.CO2$share[iter+1]*no.EF.CO2$efa[iter+1]
#     #  iter = iter+2
# }


ggplot(no.EF.CO2) +
  aes(x = Year, y = as.numeric(EFA), linetype=EmConcept) +   
  geom_line(size=1) +
  geom_line(aes(y = EFA_weighted, linetype="EFA_weighted")) +   #
  labs(x = "\nYear",
       y = "Emission factors for CO2 (in g per km)  \n  \n",
  #     title = "Emission factors for CO2 in Norway 1990-2015",
       linetype = "Fuel type") +
  theme_bw() +
  scale_linetype_manual(values = c(3,6,1), labels =c("Gasoline", "Diesel", "Weighted")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size = 25))
ggsave("output_descriptive/no_co2.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 7, height = 7, units = "cm",
       dpi = 300) #, limitsize = TRUE)

ggplot(se.EF.CO2) +
  aes(x = Year, y = as.numeric(EFA), linetype=EmConcept) +   
  geom_line(size=1) +
  geom_line(aes(y = EFA_weighted, linetype="EFA_weighted")) +   #
  labs(x = "\nYear",
       y = "Emission factors for CO2 (in g per km)  \n  \n",
 #      title = "Emission factors for CO2 in Sweden 1990-2015",
       linetype = "Fuel type") +
  theme_bw() +
  scale_linetype_manual(values = c(3,6,1), labels =c("Gasoline", "Diesel", "Weighted")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size = 25))
ggsave("output_descriptive/se_co2.png", plot = last_plot(), device = "png", path = NULL,
       scale = 4, 
       width = 7, height = 7, units = "cm",
       dpi = 300) #, limitsize = TRUE)

# ggplot(no.EF.NOx) +
#   aes(x = Year, y = as.numeric(EFA), linetype=EmConcept) +   
#   geom_line(size=1) +
#   geom_line(aes(y = EFA_weighted, linetype="EFA_weighted")) +   #
#   labs(x = "\nYear",
#        y = "Emission factors for NOx (in g per km)  \n",
#     #   title = "Emission factors for NOx in Norway 1990-2015",
#        linetype = "Fuel type") +
#   theme_bw() +
#   scale_linetype_manual(values = c(3,6,1), labels =c("Gasoline", "Diesel", "Weighted")) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
#         text = element_text(size = 25))
# ggsave("output_descriptive/no_nox.png", plot = last_plot(), device = "png", path = NULL,
#        scale = 4, 
#        width = 7, height = 7, units = "cm",
#        dpi = 300) #, limitsize = TRUE)
# 
# ggplot(se.EF.NOx) +
#   aes(x = Year, y = as.numeric(EFA), linetype=EmConcept) +   
#   geom_line(size=1) +
#   geom_line(aes(y = EFA_weighted, linetype="EFA_weighted")) +   #
#   labs(x = "\nYear",
#        y = "Emission factors for NOx (in g per km)  \n  \n",
#      #  title = "Emission factors for NOx in Sweden 1990-2015",
#        linetype = "Fuel type") +
#   theme_bw() +
#   scale_linetype_manual(values = c(3,6,1), labels =c("Gasoline", "Diesel", "Weighted")) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
#         text = element_text(size = 25))
# ggsave("output_descriptive/se_nox.png", plot = last_plot(), device = "png", path = NULL,
#        scale = 4, 
#        width = 7, height = 7, units = "cm",
#        dpi = 300) #, limitsize = TRUE)
# 


# #-------------------------------------------Approximation------------------------------------------------------------------------------------------#
#Calc emissions from VMT and emission factors
require(pacman)      ##library(pacman)
#Load packages
p_load(rio,       #Input and Output: import()- & export()-function
       tidyr,     #gather()-function for data prep
       ggplot2,    #ggplot()-function for plots
       zoo    
       )     
#Set working directory
setwd("C:/Users/Wulfgang/Desktop/Thesis/code/A_mycode")
require(dplyr)
require(tidyr)

vmt.import <- import("data_input/eu_vmt/Traffic_volume/export_enerdata_8658_030627.csv", dec=",")
colnames(vmt.import) <- c("item", "code", "unit", "year", "value", "s_code", "note", "data_type")
vmt.filter <- filter(vmt.import, data_type=="Car traffic" & between(year, 1990, 2015))

pop.import <- import("data_input/eu_vmt/Traffic_volume/sample_pop.csv", dec=",")
fi.pop <- pop.import[75,35:60]
no.pop <- pop.import[177,35:60]
se.pop <- pop.import[223,35:60]

vmt <- vmt.filter[,c(4,2,5)]

for (xtime in 1:26){vmt$value[xtime] <- as.numeric(vmt$value[xtime])/fi.pop[xtime]*10^9}
for (ytime in 1:26){vmt$value[26+ytime] <- as.numeric(vmt$value[26+ytime])/no.pop[ytime]*10^9}
for (ztime in 1:26){vmt$value[52+ztime] <- as.numeric(vmt$value[52+ztime])/se.pop[ztime]*10^9}

tidy.vmt <- vmt
no.vmt <- filter(tidy.vmt, code =="NO")
se.vmt <- filter(tidy.vmt, code =="SE")


###New plot: Emission Factors
EF.import <- import("data_input/eu_EF/EF_prep_csv.csv", dec=",")
#EF.import.cut <- EF.import    #for HBEFA Version 4.2
EF.import.cut <- EF.import[1:48,1:7]    #old for HBEFA 3.2
# 


#CO2 EF Plot ------ Prepared for Version 3.2
no.EF.CO2 <- filter(EF.import.cut, Component == "CO2(rep.)" & Case== "NOR" & between(Year, 1990, 2015) & EmConcept == "D")
se.EF.CO2 <- filter(EF.import.cut, Component == "CO2(rep.)" & Case== "SWE" & between(Year, 1990, 2015)& EmConcept == "D")
no.EF.NOx <- filter(EF.import.cut, Component == "NOx" & Case== "NOR" & between(Year, 1990, 2015))
se.EF.NOx <- filter(EF.import.cut, Component == "NOx" & Case== "SWE" & between(Year, 1990, 2015))

library(zoo)
# Create zoo objects
se.zc <- zoo(se.EF.CO2$EFA_weighted, se.EF.CO2$Year)    # low freq
se.zs <- zoo(as.numeric(t(se.vmt$value)), se.vmt$year)  # high freq
no.zc <- zoo(no.EF.CO2$EFA_weighted, no.EF.CO2$Year)    # low freq
no.zs <- zoo(as.numeric(t(no.vmt$value)), no.vmt$year)  # high freq
# Merge series into one object
se.z <- merge(se.zs,se.zc)
no.z <- merge(no.zs,no.zc)

# Interpolate calibration data (na.spline could also be used)
#z$zs <- na.approx(z$zc, rule=5)
se.z.ts <- ts(se.z, start=1978, end=2016, frequency=1)    
no.z.ts <- ts(no.z, start=1978, end=2016, frequency=1)    

a <- na.approx(se.z$se.zc, rule=2)
b <- na.approx(no.z$no.zc, rule=2)
# Only keep index values from sample data
Z <- z[index(a),]
Z

se.approx.co2 <- as.numeric(se.vmt$value) * a / 1000000 *0.907184  #1g/1.000.000=t=0,90718473999999 metric tons
no.approx.co2 <- as.numeric(no.vmt$value) * b / 1000000 *0.907184 #1g/1.000.000=t
se.approx.co2.EF1990 <- as.numeric(se.vmt$value) * as.numeric(a[1]) / 1000000 *0.907184 #1g/1.000.000=t
no.approx.co2.EF1990 <- as.numeric(no.vmt$value) * as.numeric(b[1]) / 1000000 *0.907184 #1g/1.000.000=t
se.approx.co2.VMT1990 <- as.numeric(se.vmt$value[1]) * a / 1000000 *0.907184 #1g/1.000.000=t
no.approx.co2.VMT1990 <- as.numeric(no.vmt$value[1]) * b / 1000000 *0.907184 #1g/1.000.000=t

se.approx <- cbind(se.approx.co2, se.approx.co2.EF1990, se.approx.co2.VMT1990)
colnames(se.approx) <- c("Estimated", "EF 1990", "VMT 1990")
no.approx <- cbind(no.approx.co2, no.approx.co2.EF1990, no.approx.co2.VMT1990)
colnames(no.approx) <- c("Estimated", "EF 1990", "VMT 1990")
year <- c(1990:2015)
se.approx.year <- cbind(year, data.frame(se.approx))
se.approx.year.tidy <- gather(se.approx.year, "model", "value", 2:4)
no.approx.year <- cbind(year, data.frame(no.approx))
no.approx.year.tidy <- gather(no.approx.year, "model", "value", 2:4)

approx <- cbind(se.approx.year.tidy, no.approx.year.tidy[,3])
colnames(approx) <- c("year", "model", "SWE", "NOR")
approx.tidy <- gather(approx, "Country", "value", 3:4)

ggplot(approx.tidy) +
  aes(x = year, y = as.numeric(value), colour=Country, linetype=model) +   
  geom_line(size=2) +
  labs(x = "\nYear",
       y = "Approximated CO2 emissions per capita from car transport (in metric tons of CO2)  \n  \n",
       #  title = "Emission factors for NOx in Sweden 1990-2015",
       linetype = "County") +
  theme_bw() +
  scale_linetype_manual(values = c(6,1,3)) +
  scale_colour_manual(values = c("#C4961A", "black")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size = 25))

# approx.co2 <- cbind(no.approx.co2, se.approx.co2)
# colnames(approx.co2) <- c("NOR", "SWE")
# year <- c(1990:2015)
# time.approx.co2 <- cbind(year, data.frame(approx.co2))
# tidy.approx.co2 <- gather(time.approx.co2, "country", "value", 2:3)
# 
# ggplot(tidy.approx.co2) +
#   aes(x = year, y = as.numeric(value), linetype=country) +   
#   geom_line(size=1) +
#   labs(x = "\nYear",
#        y = "Approximated CO2 emissions from car transport per capita (tons of CO2)  \n  \n",
#        #  title = "Emission factors for NOx in Sweden 1990-2015",
#        linetype = "County") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
#         text = element_text(size = 25))
ggsave("output_descriptive/approx.png", plot = last_plot(), device = "png", path = NULL,
       scale = 5, 
       width = 10, height = 7, units = "cm",
       dpi = 300) #, limitsize = TRUE)
