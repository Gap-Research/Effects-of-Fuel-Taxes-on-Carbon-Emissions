#Bohlin comparison
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
library(rio)
fi.gaps <- import("output_scm_data/Finland_gaps.csv", dec=".", sep=",")   
se.gaps <- import("output_scm_data/Sweden_gaps.csv", dec=".", sep=",")   

fi.gaps.ts <- ts(fi.gaps, start=1978, end=2016, frequency=1)    
se.gaps.ts <- ts(se.gaps, start=1978, end=2016, frequency=1)    

fi.gaps.diff <- diff(fi.gaps.ts, differences = 1)    
se.gaps.diff <- diff(se.gaps.ts, differences = 1)    

fi.gaps.diff.mean <- mean(fi.gaps.diff)
se.gaps.diff.mean <- mean(se.gaps.diff)

export.results <- cbind(fi.gaps.diff.mean, se.gaps.diff.mean)

export(export.results, file = "output_scm_data/lin_analysis_co2_growth_reduction.csv", format = "csv")