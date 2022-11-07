###Tax incidence - Finland
#Set working directory
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
#Load pacman for package load
require(pacman)
#Load packages
p_load(sandwich,  #for statistical analysis: NeweyWest()
       rio,       #R-Input and Output: import() & export()
       lmtest,    #Additional methods for regression analysis: coefci()
       stargazer
)

###Data: Import, time series####
##Import data
tax.data <- import("output_reg_data/fi_tax_incidence_data.csv", sep=",", dec=".")    #prefix: Finland

##Prepare data
tax.data.ts <- ts(tax.data, start=1978, end=2016, frequency=1)    

###Regression analysis: fd, coefficients, CI####
##Calculate first differences
diff.tax <- diff(tax.data.ts, differences = 1)    

##Number of lags are chosen using the Newey-West (1994) method.

##First-differences regression analysis with total tax
#Define general parameters
yt <- diff.tax[, "gas_totalprice"]         #Output variable (retailprice)
xt <- diff.tax[,c("gas_excise_tax", "crude")]    #Define matrix of independet variables in time (oilprice, total tax)

#Regression
reg.tax <-  lm(yt ~ xt)  
coef(reg.tax)
#Confidence interval for Newey-West errors with 95% confidence level
coefci.totaltax <- coefci(reg.tax, vcov = NeweyWest(reg.tax, adjust = TRUE, prewhite=FALSE), level=0.95)                                  
bwNeweyWest(reg.tax)

stargazer(reg.tax,      
          title="Tax incidence analysis for Finland",
          column.labels=c("First-difference OLS"),
          covariate.labels=c("Total tax",
                             "Crude oil price"),
          ci=TRUE,   
          ci.custom=list(coefci.totaltax),
          ci.level=0.95,
          #ci.separator="-",
          single.row=TRUE,          #Coefficient and CI in same row
          #Table settings
          keep.stat = c("n", "adj.rsq", "rsq"),
          digits = 2,
          type="text",     #"latex"=LaTex (default), "text"=ASCII
          style="aer",     #test: aer, asq, qje, all, all2, default
          table.layout = "l-d-o-#c=t-s=n",      #https://rdrr.io/cran/stargazer/man/stargazer_table_layout_characters.html
          dep.var.caption = "",
          dep.var.labels= "Gasoline Price",
          model.names = FALSE,
          colnames = FALSE,
          font.size = "small",
          header = FALSE,
          star.char = c("*", "+"),
          star.cutoffs = c(0.05, 0.01),
          notes = c("Notes: + p<0.01; * p<0.05"),
          notes.align = "l",         #align notes: "l"-left, "c"-center, "r"-right
          notes.append =FALSE,       #p-threshold is suppressed when false
          notes.label ="",            #change name of notes section   
          out="output_reg_tables/tax_incidence_results_FI.txt"    #for LaTex-file .tex
)
