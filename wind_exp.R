# TODO: Add comment

##############################################################################################################
rm(list=ls(all=TRUE)) 
options(width = 438L)

library(WindR)
w.start()
#w.menu()
#w.isconnected()

#---------------- contracts ----------------
Underlying ="CU"
Exchange = "SHF"
#contract years
c.yg = formatC(seq(10,16), width=2, flag='0')
#contract months
c.tg = formatC(seq(1,12), width=2, flag='0')
#contract vector
gg = merge(c.tg, c.yg)
m.vec = paste(gg[,2], gg[,1], sep="")
c.vec = paste(Underlying, m.vec, ".", Exchange, sep="")

#contract and date vector
c.d.df = vector()
for (i in 1:length(c.vec)) {
#	i = 13
#	get the first and last trading date
	wd = w.wsd(c.vec[i], "ipo_date,lasttrade_date", Sys.Date(), Sys.Date())
	IPO_DATE = as.character(w.asDateTime(wd$Data$IPO_DATE, TRUE))
	LASTTRADE_DATE = as.character(w.asDateTime(wd$Data$LASTTRADE_DATE, TRUE))
	
	if (is.na(LASTTRADE_DATE) | is.na(IPO_DATE)) {
		print(paste(c.vec[i], "does not exist"))
	} else {
		print(paste(c.vec[i], IPO_DATE, LASTTRADE_DATE, m.vec[i]))
		c.d.df = rbind(c.d.df, c(c.vec[i], IPO_DATE, LASTTRADE_DATE, m.vec[i]))
	}
}
colnames(c.d.df) = c("Contract", "ListDate", "ExpDate", "Month")

#export to csv file
fpath = paste(getwd(), "/dataset/", sep="")
fname = paste(Underlying, Exchange, "csv", sep=".")
write.table(c.d.df, file=paste(fpath,fname,sep=""), sep=",", row.names=FALSE, col.names=TRUE)

