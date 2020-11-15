require("pbapply");require("dplyr");require("data.table");require("quantmod");require("timeDate")
# ************************************************************************************************************************************
# READ IN FILES - I GET THIS DATA FROM BARCHART AND SAVE IT LOCALLY
# LIST THE PATH TO THE LOCATION WHERE THESE FILES ARE SAVED
LIST <- list.files("/Volumes/3TB/BARCHART/FUTURES/DAILIES/DATABASE",full.names = TRUE)
LIST <- rbindlist(lapply(as.list(LIST),readRDS),use.names = TRUE)

View(subset(LIST,LIST$ShortName == "CL"))
NOM2 <- as.character(unique(LIST$ShortName))

dt <- pblapply(as.list(NOM2),function(N){
  tst <- subset(LIST, LIST$ShortName == N)
  tst <- tst %>% group_by(Date) %>% filter(OpenInterest == max(OpenInterest))
  tst <- tst %>% group_by(Date) %>% filter(Volume == max(Volume))
  unique(tst)
})
dt <- rbindlist(dt,use.names = TRUE)

# function to convert to XTS
fwdXTS = function(ticker)
{
  dt0 <- subset(dt, dt$ShortName == ticker)
  toDate <- sapply(dt0[,"Date"], as.character)
  toDate <- as.Date(toDate, format="%Y-%m-%d")
  dt0 <- xts(dt0[,c("Open","High","Low","Close","Volume","OpenInterest")], 
             order.by = toDate)
  colnames(dt0) <- paste0(ticker,".",names(dt0))
  Cl(dt0)
}
# merge all Closes
futFWD = do.call(merge,lapply(as.list(NOM2), fwdXTS))
futFWD=futFWD["2019::"]
# make index unique i.e. strip time from Dates
futFWD <- make.index.unique(futFWD,drop = TRUE)

# EXCLUDE TRADING NYSE HOLIDAYS & WEEKENDS
futFWD <- futFWD[isBizday(as.timeDate(index(futFWD)), 
                          holidays=holidayNYSE(year=2019:2020),wday = 1:5)]
futFWD <- na.locf0(futFWD)
futFWD <- na.locf0(futFWD,fromLast = TRUE)
colnames(futFWD)<- gsub(".Close","",names(futFWD))






