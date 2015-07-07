library(stringr)
tdata <- read.csv("data/reduced_tdata.csv", as.is = T)
idata <- read.csv("data/idata.csv", as.is = T)
tdata$age_group <- as.character(tdata$age_group)
tdata$age_group <- str_trim(tdata$age_group)
idata$age_group <- as.character(idata$age_group)
idata$age_group <- str_trim(idata$age_group)
sdata <- read.csv("data/aggr_summary.csv")
# Temporarily remove baseline summary
sdata <- sdata[-1,]
sdata[is.na(sdata)] <- 0