###################################################
### Ratio Test: Ratios of Post-Treatment MSPE to Pre-Treatment MSPE
###################################################
###Country id
#Full sample: 1:Australia, 2:Austria, 3:Belgium, 4:Canada, 5: Denmark, 6:Finland, 7: France, 8:Germany, 9:Greece, 10:Iceland, 
#11: Ireland, 12:Italy, 13:Japan, 14:Luxembourg, 15:Netherlands, 16:New Zealand, 17:Norway, 18:Poland, 19:Portugal, 20:Spain
#21:Sweden, 22:Switzerland, 23:Turkey, 24:United Kingdom, 25:United States

#----------------------HEAD------------------------------------------------#
Country <- c("Australia", "Austria", "Belgium", "Canada", "Denmark", 
             "Finland", "France", "Germany", "Greece", "Iceland", 
             "Ireland", "Italy", "Japan", "Luxembourg", "Netherlands",
             "New Zealand", "Norway", "Poland", "Portugal", "Spain", 
             "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States") 

#Algorithm settings
controls.identifier.donor.pool <- c(1, 3, 4, 5, 7, 9, 10, 13, 16, 18, 19, 20, 22, 25)
runs <- c(6, 17, 21, controls.identifier.donor.pool)
#----------------------------------------------------------------------------------------------------#

#Load and prepare data
library(rio)
data <- import("output_scm_data/placebo_test_store.csv", dec=".", sep=",")
store <- c(rep(0,length(data)))         

for(iter in 1:length(data)){
  mspepost <- mean((data[31:46,iter])^2)
  mspepre <- mean((data[1:30,iter])^2)
  store[iter] <- mspepost/mspepre
}

#Prepare tidy data for barchart
library(tidyr)
t.store <- t(store)
colnames(t.store) <- Country[runs]  
gather.store <- gather(data.frame(t.store), "country", "mspe")
gather.store.plot <- gather.store[order(gather.store$mspe, decreasing=TRUE),]
#rownames(gather.store.plot) <- c(1:15)
gather.store.plot$country <- factor(gather.store.plot$country, levels = gather.store.plot$country[order(gather.store.plot$mspe)])

library(ggplot2)
# Basic barplot
ggplot(data=gather.store.plot) +
  aes(x=country, y=mspe) +
  geom_bar(stat="identity")  +   #,  fill="steelblue")
  coord_flip() +
  labs(x = "Country \n",
       y = "\n Postperiod MSPE / Preperiod MSPE"
#       title = "MSPE for treated and donor pool countries"
) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()
        ,text = element_text(size = 25)
        )

#Save barchart
  ggsave("output_scm_plot/mspe.png", plot = last_plot(), device = "png", path = NULL,
         #   scale = 1, 
         #  width = 80, height = 50, units = "mm",
         dpi = 300) #, limitsize = TRUE)
#---------------------------------------------------------------------------------------------------#