#Tax incidence - Andersson
#Clean----------------------------------------------------#
dev.off()     #close plot
rm(list=ls()) #Remove all variables from environment
p_unload(all) #Unload all user packages
cat("\014")   #Clean console
#Head----------------------------------------------------#
require(pacman)
#Load packages
p_load(sandwich,   #NeweyWest()-function
       rio,        #import() & export()-function
       lmtest,     #coefci()-function
       stargazer
       )
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
###Data: Import, time series####
##Import data
tax.data <- import("code_reg/rep_andersson/data/tax_incidence_data.dta")
##Prepare data
#Define time series   #ts(vector, start=, end=, frequency=), frequency=1 --> yearly
tax.data.ts <- ts(tax.data, start=1970, end=2015, frequency=1)    


###Regression analysis: fd, coefficients, CI####
##Calculate first differences
diff.tax <- diff(tax.data.ts, differences = 1)    

##Number of lags are chosen using the Newey-West (1994) method.

##First-differences regression analysis with total tax
#retailprice = crudeoilprice + totaltax
#Define general parameters
yt <- diff.tax[, 2]    #Output variable (retailprice)
xt <- diff.tax[,5:6]   #define matrix of independet variables in time (oilprice, total tax)

#Regression
reg.tax <-  lm(yt ~ xt)  
coef(reg.tax)
#Confidence interval for Newey-West errors with 95% confidence level
coefci.totaltax <- coefci(reg.tax, vcov = NeweyWest(reg.tax, lag=16, adjust = TRUE, prewhite=FALSE), level=0.95)                                  
#bwNeweyWest(reg.tax)


##Splitting up the total tax into its energy and carbon tax part 
#retailprice = crudeoilprice + energytax + carbontax
#Define general parameters
yt <- diff.tax[, 2]    #Output variable (dependent)
xt <- diff.tax[,3:5]   #define matrix of independet variables in time
#Regression
reg.split <-  lm(yt ~ xt)    
coef(reg.split)
#Confidence interval for Newey-West errors with 95% confidence level
coefci.splittax <- coefci(reg.split, vcov = NeweyWest(reg.split, lag=16, adjust = TRUE, prewhite=FALSE), level=0.95)                                       
#bwNeweyWest(reg.split)



###Tidy coefci.totaltax table with stargazer#
stargazer(reg.tax,reg.split,    
          order = c("xtenergycarbon_tax", "xtenergytax", "xtcarbontax", "xtoilprice_SEK"),
          title="Tax incidence analysis",
          column.labels=c("Unsplit", "Splittet"),
          covariate.labels=c("Total tax",
                             "Energy tax",
                             "Carbontax",
                             "Crude oil price"),
          ci=TRUE,   
          ci.custom=c(list(coefci.totaltax),
                      list(coefci.splittax)
                      ),
          ci.level=0.95,
          #ci.separator="-",
          single.row=TRUE,       
          #Table settings
          keep.stat = c("n", "adj.rsq", "rsq"),
          digits = 2,
          type="text",     #"latex"=LaTex (default), "text"=ASCII
          style="aer",     #test: aer, asq, qje, all, all2, default
          table.layout = "o-#c=t-s=n",      #https://rdrr.io/cran/stargazer/man/stargazer_table_layout_characters.html
          #          table.placement = "!htbp",
          dep.var.caption = "",
          dep.var.labels= "",
          model.names = FALSE,
          #align=TRUE,
          #digits.extra
          colnames = FALSE,
          font.size = "tiny",
          header = FALSE,
#          notes = c("Notes: Results show first-difference regression coefficients with Newey-West standart errors for autocorrelation and heteroskedasticity robustness.", 
#                    "Sources: Prices and taxes were obtained from 'Energy Prices and Taxes' published by International Energy Agency"),
          notes.align = "l",         #align notes: "l"-left, "c"-center, "r"-right
          notes.append =FALSE,       #p-threshold is suppressed when false
          notes.label ="",            #change name of notes section   
          out="results/tax_incidence_rep_andersson_SE.txt"    #for LaTex-file .tex
          #out.header=FALSE
)