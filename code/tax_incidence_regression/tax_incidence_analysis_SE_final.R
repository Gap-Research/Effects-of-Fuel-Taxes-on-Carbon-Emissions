###################################################
### Tax incidence - Sweden
###################################################
#Load packages
require(pacman)
p_load(sandwich,  #for statistical analysis: NeweyWest()
       rio,       #R-Input and Output: import() & export()
       lmtest,    #Additional methods for regression analysis: coefci()
       stargazer
)

#Set working directory
# setwd()

##--------------------------------------------------------------------------####

###Data: Import, time series####
##Import data
tax.data <- import("input/regression_data/se_tax_incidence_data.csv", sep=",", dec=".")    #prefix: se, fi
##Prepare data
tax.data.ts <- ts(tax.data, start=1978, end=2016, frequency=1)    

##Calculate first differences
diff.tax <- diff(tax.data.ts, differences = 1)    

##Number of lags are chosen using the Newey-West (1994) method.

##First-differences regression analysis with total tax
#retailprice = crudeoilprice + totaltax
#Define general parameters
yt <- diff.tax[, "gas_totalprice"]         #Output variable (retailprice)
xt <- diff.tax[,c("gas_excise_tax", "crude")]    #Define matrix of independet variables in time (oilprice, total tax)

#Regression
reg.tax <-  lm(yt ~ xt)  
coef(reg.tax)
#Confidence interval for Newey-West errors with 95% confidence level
coefci.totaltax <- coefci(reg.tax, vcov = NeweyWest(reg.tax, adjust = TRUE, prewhite=FALSE), level=0.95)
bwNeweyWest(reg.tax)

##Splitting up the total tax into its energy and carbon tax part 
#retailprice = crudeoilprice + energytax + carbontax
#Define general parameters
yt <- diff.tax[, "gas_totalprice"]         #Output variable 
xt <- diff.tax[,c("gas_carbontax", "gas_energytax", "crude")]    #Independet variables
#Regression
reg.split <-  lm(yt ~ xt)    
coef(reg.split)
#Confidence interval for Newey-West errors with 95% confidence level
coefci.splittax <- coefci(reg.split, vcov = NeweyWest(reg.split, lag=NULL, adjust = TRUE, prewhite=FALSE), level=0.95)                                       
bwNeweyWest(reg.split)

###Tidy coefci.totaltax table with stargazer#
stargazer(reg.tax,reg.split,    #overwrite default standart errors (se) with Newey-West se   
          title="Tax incidence analysis for Sweden",
          column.labels=c("Unsplit", "Splittet"),
          covariate.labels=c("Total tax",
                             "Carbon tax",
                             "Energy tax",
                             "Crude oil price"),
          ci=TRUE,   
          ci.custom=c(list(coefci.totaltax),
                      list(coefci.splittax)
          ),
          ci.level=0.95,
          #ci.separator="-",
          single.row=TRUE,          #Coefficient and CI (or standart error?) in same row
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
          star.char = c("*", "+"),
          star.cutoffs = c(0.05, 0.01),
          notes = c("Notes: + p<0.01; * p<0.05"),
           notes.align = "l",         #align notes: "l"-left, "c"-center, "r"-right
           notes.append =FALSE,       #p-threshold is suppressed when false
           notes.label ="",            #change name of notes section   
           out="output/regression_tables/tax_incidence_results_SE.txt"    #for LaTex-file .tex
           #out.header=FALSE
 )
