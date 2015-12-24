rm(list=ls(all=TRUE)) 
options(width = 438L)

#---------------- contracts ----------------
# TODO: configuration
BarSize = 15
BarSize = "DAY"

Underlying ="I"; Exchange = "DCE"; Benchmark = c("S0174655");
#Underlying ="CU"; Exchange = "SHF"; Benchmark = NA;
#Underlying ="M"; Exchange = "DCE"; Benchmark = NA;
#Underlying ="RB"; Exchange = "SHF"; Benchmark = NA;
#Underlying ="P"; Exchange = "DCE"; Benchmark = NA;

#---------------- import to csv file ----------------
#choose which dataset file to use
ds.date = "2015-12-08"
fpath = paste(getwd(), "/wind_dataset/", sep="")
fname = paste(Underlying, Exchange, ds.date, BarSize, "csv", sep=".")
dtable = read.table(file=paste(fpath,fname,sep=""), header=TRUE, sep=",")

#contract years
c.yg = formatC(seq(13,16), width=2, flag='0')
#contract months
c.tg = c("01","05","09")
#c.tg = c("03","06","09","12")
#c.tg = formatC(seq(1,12), width=2, flag='0')

#---------------- divide dtable into price data and benchmark data ----------------
#e.g., DATETIME, I1403.DCE.cl, I1403.DCE.vo, I1403.DCE.oi, I1404.DCE.cl, I1404.DCE.vo, I1404.DCE.oi, ...
id = names(dtable) %in% c("DATETIME", Benchmark)
c.dt = dtable[!id]
rownames(c.dt) = dtable$DATETIME

#field name of c.dt
cf = data.frame(matrix(unlist(strsplit(names(c.dt),"[.]")), ncol=3, byrow=TRUE))
names(cf) = c("Contract", "Exchange", "Field")
tt = nchar(Underlying)
cf$Year = substr(cf$Contract, tt+1, tt+2)
cf$Month = substr(cf$Contract, tt+3, tt+4)
#choose only c.tg related month, and fd2 related fields, fd = c("close", "volume", "oi")
ind = ((cf$Year %in% c.yg) & (cf$Month %in% c.tg) & cf$Field == "cl")
dataset = c.dt[,ind]
ind = ((cf$Year %in% c.yg) & (cf$Month %in% c.tg) & cf$Field == "oi")
dataset.oi = c.dt[,ind]

#e.g., DATETIME, S0174655, ...
bm.dt = dtable[Benchmark]
rownames(bm.dt) = dtable$DATETIME

####################################################################################################
#price movement
####################################################################################################
#---------------- data analysis ----------------
#spread and butterfly analysis
tmp = dim(dataset)[2]
ntmp = names(dataset)
sp0 = dataset[,seq(1,tmp-1)] -   dataset[,seq(2,tmp)]
bf0 = dataset[,seq(1,tmp-2)] - 2*dataset[,seq(2,tmp-1)] + dataset[,seq(3,tmp)]
names(sp0) = paste(ntmp[seq(1,tmp-1)], "-",   ntmp[seq(2,tmp)])
names(bf0) = paste(ntmp[seq(1,tmp-2)], "-2*", ntmp[seq(2,tmp-1)], "+", ntmp[seq(3,tmp)])

# TODO: sp/bf combination
#sp = sp0[,c(1,3,5,7,9,11,13)]

sp = sp0
bf = bf0

refline = bm.dt[,Benchmark] / 0.92

#---------------- time series plot ----------------
#---- calendar spread ----
par(mar = c(5, 4, 4, 4) + 0.3) 
matplot(sp, type="l", col=dim(sp)[2]:1, xlab=BarSize, ylab="Calendar Spread", main=paste(Underlying,".",Exchange," Time Series Plot"))
par(new=TRUE)
plot(refline, type="l", axes=FALSE, bty="n", xlab="", ylab="")
axis(side=4, at=pretty(range(refline)))
legend('topleft', legend=c(names(sp),"Price Reference"), text.col=c(dim(sp)[2]:1,1), cex=0.65)
grid()

#---- butterfly ----
par(mar = c(5, 4, 4, 4) + 0.3) 
matplot(bf, type="l", col=dim(bf)[2]:1, xlab=BarSize, ylab="Butterfly", main=paste(Underlying,".",Exchange," Time Series Plot"))
par(new=TRUE)
plot(refline, type="l", axes=FALSE, bty="n", xlab="", ylab="")
axis(side=4, at=pretty(range(refline)))
legend('topleft', legend=c(names(bf),"Price Reference"), text.col=c(dim(bf)[2]:1,1), cex=0.65)
grid()

#---------------- seasonal plot ----------------
par(mfrow=c(1,1)) 

#---- calendar spread ----
mlen = max(apply(sp, 2, function(x){sum(!is.na(x))}))
sp2 = vector()
for (i in 1:dim(sp)[2]) {
	mtmp = sp[!is.na(sp[,i]),i]
	length(mtmp) = mlen
	sp2 = cbind(sp2, mtmp)
}
colnames(sp2) = names(sp)

# TODO: plot range, exclude the close to expiration data
sp3 = sp2[1:min(243, dim(sp2)[1]),]
matplot(sp3, type="l", col=dim(sp3)[2]:1, xlab=BarSize, ylab="Calendar Spread", main=paste(Underlying,".",Exchange," Seasonal Plot"))
#find the max and min of each combination
mm = matrix(paste(c("min","max"), apply(sp3,2,range,na.rm=TRUE), sep="="), ncol=2, byrow=TRUE)
lgd = paste(colnames(sp3), mm[,1], mm[,2])
legend('topleft', legend=lgd, text.col=dim(sp3)[2]:1, cex=0.65)
grid()

#---- butterfly ----
mlen = max(apply(bf, 2, function(x){sum(!is.na(x))}))
bf2 = vector()
for (i in 1:dim(bf)[2]) {
	mtmp = bf[!is.na(bf[,i]),i]
	length(mtmp) = mlen
	bf2 = cbind(bf2, mtmp)
}
colnames(bf2) = names(bf)

# TODO: plot range, exclude the close to expiration data
bf3 = bf2[1:min(166, dim(bf2)[1]),]
matplot(bf3, type="l", col=dim(bf3)[2]:1, xlab=BarSize, ylab="Butterfly", main=paste(Underlying,".",Exchange," Seasonal Plot"))
#find the max and min of each combination
mm = matrix(paste(c("min","max"), apply(bf3,2,range,na.rm=TRUE), sep="="), ncol=2, byrow=TRUE)
lgd = paste(colnames(bf3), mm[,1], mm[,2])
legend('topleft', legend=lgd, text.col=dim(bf3)[2]:1, cex=0.65)
grid()

####################################################################################################
#delivery month, cornering
####################################################################################################
#---------------- time series plot ----------------
#may need to change c.tg to get all the delivery months, e.g., c.tg = formatC(seq(1,12), width=2, flag='0')
par(mfrow=c(1,1)) 

#delivery month days
dmdays = 20
ft = dataset
for (j in 1:dim(ft)[2]) {
	nlast = tail(which(is.na(ft[,j]) %in% FALSE), n=1)
	nfirst = nlast - dmdays
	ft[!(seq(1,dim(ft)[1]) %in% seq(nfirst, nlast)), j] = NA
}
#time series plot to view the delivery month contract v.s. the reference price
ft2 = cbind(ft, refline)
matplot(ft2, type="l", col=dim(ft2)[2]:1, xlab=BarSize, ylab="Delivery Month Basis", main=paste(Underlying,".",Exchange," Time Series Plot"))
legend('topleft', legend=names(ft2), text.col=dim(ft2)[2]:1, cex=0.65)
grid()

#difference plot
bs = ft - refline
matplot(bs, type="l", col=dim(bs)[2]:1, xlab=BarSize, ylab="Delivery Month Basis", main=paste(Underlying,".",Exchange," Time Series Plot"))
abline(h=0)
legend('topleft', legend=names(bs), text.col=dim(bs)[2]:1, cex=0.65)
grid()


#open interest
ft.oi = log10(dataset.oi)
for (j in 1:dim(ft.oi)[2]) {
	nlast = tail(which(is.na(ft.oi[,j]) %in% FALSE), n=1)
	nfirst = nlast - dmdays
	ft.oi[!(seq(1,dim(ft.oi)[1]) %in% seq(nfirst, nlast)), j] = NA
}
#time series plot to view the delivery month contract v.s. the reference price
ft2.oi = cbind(ft.oi)
matplot(ft2.oi, type="l", col=dim(ft2.oi)[2]:1, xlab=BarSize, ylab="Delivery Month Basis", main=paste(Underlying,".",Exchange," Time Series Plot"))
legend('topleft', legend=names(ft2.oi), text.col=dim(ft2.oi)[2]:1, cex=0.65)
grid()


##export to csv file
#fpath = paste(getwd(), "/analysis/", sep="")
#fname = paste(Underlying, Exchange, Sys.Date(), BarSize, paste(c.tg,collapse=""), paste(fd2,collapse=""), "csv", sep=".")
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
