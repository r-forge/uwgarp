
# load the crsp weekly data sets and save to the data/ folder for the GARMPFRM package

library(xts)

# wherever the data folder is saved
dir <- "~/Downloads/crsp data weekly 1997-2010/"
file <- c("largecap_weekly.csv", "midcap_weekly.csv", "smallcap_weekly.csv", "microcap_weekly.csv")


largecap_weekly <- as.xts(read.zoo(paste(dir, file[1], sep=""), format="%m/%d/%Y", header=TRUE, sep=","))
midcap_weekly <- as.xts(read.zoo(paste(dir, file[2], sep=""), format="%m/%d/%Y", header=TRUE, sep=","))
smallcap_weekly <- as.xts(read.zoo(paste(dir, file[3], sep=""), format="%m/%d/%Y", header=TRUE, sep=","))
microcap_weekly <- as.xts(read.zoo(paste(dir, file[4], sep=""), format="%m/%d/%Y", header=TRUE, sep=","))

save(largecap_weekly, midcap_weekly, smallcap_weekly, microcap_weekly, file="data/crsp_weekly.rda")
