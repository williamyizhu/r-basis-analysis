#rm(list=ls(all=TRUE)) 
#options(width = 438L)
#
##---------------- contracts ----------------
#Exchange = "DCE"; Underlying = "I"; c.mg = c("01","05","09"); fmonth = c("01","05","09");
#Exchange = "DCE"; Underlying = "M"; c.mg = c("01","05","09"); fmonth = c("05");
#Exchange = "SHFE"; Underlying = "CU"; c.mg = formatC(seq(1,12), width=2, flag='0'); lmonth = c("02")
##Benchmark = c("S0174655");
##Underlying ="M"; Exchange = "DCE"; Benchmark = NA;
##Underlying ="RB"; Exchange = "SHF"; Benchmark = NA;
##Underlying ="P"; Exchange = "DCE"; Benchmark = NA;

#---------------- import to csv file ----------------
#choose which dataset file to use
Symbol = paste(Exchange, Underlying, sep=".")
fpath = paste(getwd(), "/quandl_dataset/", sep="")
fname = paste(Symbol, "csv", sep=".")
dtable = read.table(file=paste(fpath,fname,sep=""), header=TRUE, sep=",")
dtable$Date = as.Date(dtable$Date)

#contract years
c.yg = formatC(seq(2011,2016), width=4, flag='0')

#contract months
#leading month contract

#---------------- divide dtable into price data and benchmark data ----------------
#e.g., DATETIME, I1403.DCE.cl, I1403.DCE.vo, I1403.DCE.oi, I1404.DCE.cl, I1404.DCE.vo, I1404.DCE.oi, ...
id = names(dtable) %in% c("Date")
c.dt = dtable[!id]
rownames(c.dt) = dtable$Date
#field name of c.dt
cf = data.frame(matrix(unlist(strsplit(names(c.dt),"[.]")), ncol=5, byrow=TRUE))
names(cf) = c("Exchange", "Underlying", "Year", "Month", "Field")

#check c.dt
ind = apply(c.dt, 2, function(x){all(is.na(x))}) %in% TRUE
print("------------------------- c.dt error -------------------------")
print(matrix(colnames(c.dt)[ind]))

#choose only c.mg related month, and fd2 related fields, fd = c("close", "volume", "oi")
ind = ((cf$Year %in% c.yg) & (cf$Month %in% c.mg) & (cf$Field=="Settle"))
dataset = c.dt[,ind]
#exclude rows with all NA value
gg = apply(dataset, 1, function(x){!all(is.na(x))})
dataset = dataset[gg,]

#ind = ((cf$Year %in% c.yg) & (cf$Month %in% c.mg) & cf$Field == "oi")
#dataset.oi = c.dt[,ind]

####################################################################################################
#data analysis
####################################################################################################
#spread and butterfly analysis
tmp = dim(dataset)[2]
ntmp = names(dataset)
sp0 = dataset[,seq(1,tmp-1)] -   dataset[,seq(2,tmp)]
bf0 = dataset[,seq(1,tmp-2)] - 2*dataset[,seq(2,tmp-1)] + dataset[,seq(3,tmp)]
names(sp0) = paste(ntmp[seq(1,tmp-1)], "-",   ntmp[seq(2,tmp)])
names(bf0) = paste(ntmp[seq(1,tmp-2)], "-2*", ntmp[seq(2,tmp-1)], "+", ntmp[seq(3,tmp)])

#sp / bf column names index
sp.cf = data.frame(C1=ntmp[seq(1,tmp-1)], C2=ntmp[seq(2,tmp)])
bf.cf = data.frame(C1=ntmp[seq(1,tmp-2)], C2=ntmp[seq(2,tmp-1)], C3=ntmp[seq(3,tmp)])
for (CN in c("C1","C2")) {
	mq = data.frame(matrix(unlist(strsplit(as.character(sp.cf[,CN]),"[.]")), ncol=5, byrow=TRUE))
	names(mq) = paste(CN, c("Exchange", "Underlying", "Year", "Month", "Field"), sep=".")
	sp.cf = cbind(sp.cf, mq)
}
for (CN in c("C1","C2","C3")) {
	mq = data.frame(matrix(unlist(strsplit(as.character(bf.cf[,CN]),"[.]")), ncol=5, byrow=TRUE))
	names(mq) = paste(CN, c("Exchange", "Underlying", "Year", "Month", "Field"), sep=".")
	bf.cf = cbind(bf.cf, mq)
}

#leading month for both sp and bf
sp = sp0[,(sp.cf[,"C1.Month"]%in%fmonth)]
bf = bf0[,(bf.cf[,"C1.Month"]%in%fmonth)]

month_str = paste("ActiveMonth.", paste(c.mg,collapse="."), "_FrontMonth.", paste(fmonth,collapse="."), sep="")

####################################################################################################
#calendar spread
####################################################################################################
#---------------- time series plot ----------------
matplot(sp, type="l", col=dim(sp)[2]:1, xlab=paste(Symbol,"Day"), ylab="Calendar Spread", main=paste("TimeSeries_",month_str))
legend('topleft', legend=names(sp), text.col=c(dim(sp)[2]:1,1), cex=0.65)
grid()
dev.copy(png, width=1440, height=785, filename=paste(paste(Symbol,"CS","TS",month_str,sep="_"),"png",sep="."))
dev.off ()

#---------------- seasonal plot ----------------
mlen = max(apply(sp, 2, function(x){sum(!is.na(x))}))
sp2 = vector()
for (i in 1:dim(sp)[2]) {
	mtmp = sp[!is.na(sp[,i]),i]
	length(mtmp) = mlen
	sp2 = cbind(sp2, mtmp)
}
colnames(sp2) = names(sp)
matplot(sp2, type="l", col=dim(sp2)[2]:1, xlab=paste(Symbol,"Day"), ylab="Calendar Spread", main=paste("Seasonal_",month_str))
#find the summary of each combination, return of summary() is a list
summ = lapply(apply(sp2,2,summary), function(x){paste(paste(names(x),unlist(x),sep="="),collapse=" | ")})
lgd = paste(names(summ),unlist(summ))
legend('topleft', legend=lgd, text.col=dim(sp2)[2]:1, cex=0.65)
grid()
dev.copy(png, width=1440, height=785, filename=paste(paste(Symbol,"CS","Seasonal",month_str,sep="_"),"png",sep="."))
dev.off ()

#---------------- histgram and density plot ----------------
#check sp2, exclude all NA column
ind = apply(sp2, 2, function(x){all(is.na(x))}) %in% TRUE
print("------------------------- sp2 error -------------------------")
print(matrix(colnames(sp2)[ind]))
#density analysis
sp.den = apply(sp2[,!ind],2,density,na.rm=TRUE)
xrange = range(lapply(sp.den, function(m){range(m$x)}))
yrange = range(lapply(sp.den, function(m){range(m$y)}))
plot(NA, xlim=xrange, ylim=c(0,yrange[2]), xlab=paste(Symbol,"Calendar Spread"), ylab="Probability", main=paste("Density_",month_str))
for (i in 1:length(sp.den)) {
	lines(sp.den[[i]], col=length(sp.den)-i+1)
}
legend('topleft', legend=names(sp.den), text.col=length(sp.den):1, cex=0.65)
grid()
dev.copy(png, width=1440, height=785, filename=paste(paste(Symbol,"CS","Hist",month_str,sep="_"),"png",sep="."))
dev.off ()

next
####################################################################################################
#butterfly
####################################################################################################
#---------------- time series plot ----------------
matplot(bf, type="l", col=dim(bf)[2]:1, xlab=paste(Symbol,"Day"), ylab="Butterfly", main=paste("TimeSeries_",month_str))
legend('topleft', legend=names(bf), text.col=c(dim(bf)[2]:1,1), cex=0.65)
grid()
dev.copy(png, width=1440, height=785, filename=paste(paste(Symbol,"BF","TS",month_str,sep="_"),"png",sep="."))
dev.off ()

#---------------- seasonal plot ----------------
mlen = max(apply(bf, 2, function(x){sum(!is.na(x))}))
bf2 = vector()
for (i in 1:dim(bf)[2]) {
	mtmp = bf[!is.na(bf[,i]),i]
	length(mtmp) = mlen
	bf2 = cbind(bf2, mtmp)
}
colnames(bf2) = names(bf)
matplot(bf2, type="l", col=dim(bf2)[2]:1, xlab=paste(Symbol,"Day"), ylab="Butterfly", main=paste("Seasonal_",month_str))
#find the max and min of each combination
summ = lapply(apply(bf2,2,summary), function(x){paste(paste(names(x),unlist(x),sep="="),collapse=" | ")})
lgd = paste(names(summ),unlist(summ))
legend('topleft', legend=lgd, text.col=dim(bf2)[2]:1, cex=0.65)
grid()
dev.copy(png, width=1440, height=785, filename=paste(paste(Symbol,"BF","Seasonal",month_str,sep="_"),"png",sep="."))
dev.off ()

#---------------- histgram and density plot ----------------

#####################################################################################################
##delivery month, cornering
#####################################################################################################
##---------------- time series plot ----------------
##may need to change c.mg to get all the delivery months, e.g., c.mg = formatC(seq(1,12), width=2, flag='0')
#par(mfrow=c(1,1)) 
#
##delivery month days
#dmdays = 20
#ft = dataset
#for (j in 1:dim(ft)[2]) {
#	nlast = tail(which(is.na(ft[,j]) %in% FALSE), n=1)
#	nfirst = nlast - dmdays
#	ft[!(seq(1,dim(ft)[1]) %in% seq(nfirst, nlast)), j] = NA
#}
##time series plot to view the delivery month contract v.s. the reference price
#ft2 = cbind(ft, refline)
#matplot(ft2, type="l", col=dim(ft2)[2]:1, xlab=BarSize, ylab="Delivery Month Basis", main=paste(Underlying,".",Exchange," Time Series Plot"))
#legend('topleft', legend=names(ft2), text.col=dim(ft2)[2]:1, cex=0.65)
#grid()
#
##difference plot
#bs = ft - refline
#matplot(bs, type="l", col=dim(bs)[2]:1, xlab=BarSize, ylab="Delivery Month Basis", main=paste(Underlying,".",Exchange," Time Series Plot"))
#abline(h=0)
#legend('topleft', legend=names(bs), text.col=dim(bs)[2]:1, cex=0.65)
#grid()
#
#
##open interest
#ft.oi = log10(dataset.oi)
#for (j in 1:dim(ft.oi)[2]) {
#	nlast = tail(which(is.na(ft.oi[,j]) %in% FALSE), n=1)
#	nfirst = nlast - dmdays
#	ft.oi[!(seq(1,dim(ft.oi)[1]) %in% seq(nfirst, nlast)), j] = NA
#}
##time series plot to view the delivery month contract v.s. the reference price
#ft2.oi = cbind(ft.oi)
#matplot(ft2.oi, type="l", col=dim(ft2.oi)[2]:1, xlab=BarSize, ylab="Delivery Month Basis", main=paste(Underlying,".",Exchange," Time Series Plot"))
#legend('topleft', legend=names(ft2.oi), text.col=dim(ft2.oi)[2]:1, cex=0.65)
#grid()







#ss=zoo(dataset[,1],order.by=as.Date(rownames(dataset)))
#gg=rollapply(ss,10,mean,align="right")
#plot(ss)
#lines(gg,col="red")
#
#zz = zoo(dataset, order.by=as.Date(rownames(dataset)))
#kk = rollapply(zz,5,sd,align="right")
#
#gs=kk[,seq(1,7)] - kk[,seq(2,8)]

#par(mfrow=c(1,1))

#par(mar = c(5, 4, 4, 4) + 0.3) 
#par(new=TRUE)
#plot(refline, type="l", axes=FALSE, bty="n", xlab="", ylab="")
#axis(side=4, at=pretty(range(refline)))

##export to csv file
#fpath = paste(getwd(), "/analysis/", sep="")
#fname = paste(Underlying, Exchange, Sys.Date(), BarSize, paste(c.mg,collapse=""), paste(fd2,collapse=""), "csv", sep=".")
#write.table(dataset, file=paste(fpath,fname,sep=""), sep=",", row.names=FALSE, col.names=TRUE)


#names(sp) = paste(ntmp[seq(2,tmp-1)], ntmp[seq(3,9)], sep="-")


#length(bf[!is.na(bf[,2]),2])
#
#range(sp,na.rm=TRUE)
#
#yrange.sp = apply(sp, 2, range, na.rm=TRUE)
#
#range(bf,na.rm=TRUE)
#
#yrange.bf = apply(bf, 2, range, na.rm=TRUE)


