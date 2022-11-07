require(rio)
setwd("C:/Users/Wulfgang/Desktop/upload_rode_code")
#price and tax analysis
fi.stat <- import("output_reg_data/fi_stat.csv", dec=".", sep=",")
no.stat <- import("output_reg_data/no_stat.csv", dec=".", sep=",")
se.stat <- import("output_reg_data/se_stat.csv", dec=".", sep=",")
#stat <- cbind(fi.stat, se.stat, no.stat)
#export(stat, file = "output_reg_data/full_stat.csv", format = "csv")
#
#All
nl.stat <- import("output_reg_data/nl_stat.csv", dec=".", sep=",")
dk.stat <- import("output_reg_data/dk_stat.csv", dec=".", sep=",")
stat <- cbind(fi.stat,dk.stat, nl.stat, no.stat, se.stat)
colnames(stat) <- c("FI", "DK", "NL", "NO", "SE")
export(stat, file = "output_reg_data/full_stat.csv", format = "csv")
