# TODO: Add comment

##############################################################################################################
rm(list=ls(all=TRUE)) 
options(width = 438L)

library(WindR)
w.start()
#w.menu()
#w.isconnected()

#-------------------------------------------------------
Year0 = 2000
YearLengh = 20
#create a data.frame for index start and end date of a month
YearVec = rep(Year0 + seq(0, YearLengh-1), each = 12)
MonthVec = rep(formatC(seq(1, 12), width=2, format="d", flag="0"), YearLengh)
#start date of each month
MonthStart = as.Date(paste(YearVec, MonthVec, "01", sep="-"))
#c(YearVec[-1], Year0 + YearLengh)
#c(MonthVec[-1], "01")
MonthEnd = as.Date(paste(c(YearVec[-1], Year0 + YearLengh), c(MonthVec[-1], "01"), "01", sep="-")) - 1
#Year-Month data.frame
YM = data.frame(YearVec, MonthVec, MonthStart, MonthEnd)
names(YM) = c("Year", "Month", "Start", "End")

#---------------- contracts ----------------
BarSize = 15
Underlying ="I"
Exchange = "DCE"
#contract months
c.tg = c("01","05","09")
c.tg = formatC(seq(1,12), width=2, flag='0')
#c.tg = c("09")
#fields obtained from wind terminal
fd = c("close", "volume", "oi")
fd = c("close")

#---------------- Contract.DF ----------------
Contract.DF = read.csv(paste(Underlying,Exchange,"csv",sep="."), header=FALSE)
names(Contract.DF) = c("Month", "Start", "End")
Contract.DF$Contract = paste(Underlying, Contract.DF$Month, ".", Exchange, sep="")

#---------------- contract data ----------------
c.dt = data.frame(DATETIME=character(0)) 
for (i in 1:length(Contract.DF$Start)) {
	if (substr(Contract.DF$Month[i],3,4) %in% c.tg) {
		print(as.character(Contract.DF$Contract[i]))		
		ts = paste(Contract.DF$Start[i], "00:00:00")
		es = paste(Contract.DF$End[i], "23:59:59")
#		download data from wind terminal
		w_wsi_data = w.wsi(Contract.DF$Contract[i], paste(fd,collapse=","), ts, es, BarSize=BarSize)
		names(w_wsi_data$Data) = c("DATETIME", paste(Contract.DF$Contract[i],substr(fd,1,2),sep="."))
#		merge contracts into the same data.frame
		c.dt = merge(c.dt, w_wsi_data$Data, by="DATETIME", all=TRUE)		
	}
}

#export to csv file
fpath = paste(getwd(), "/dataset/", sep="")
fname = paste(Underlying, Exchange, paste(c.tg,collapse=""), BarSize, Sys.Date(), "csv", sep=".")
write.table(c.dt, file=paste(fpath,fname,sep=""), sep=",", row.names=FALSE, col.names=TRUE)

head(c.dt, n=20)
tail(c.dt, n=20)

