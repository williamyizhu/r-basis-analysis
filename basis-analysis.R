rm(list=ls(all=TRUE)) 
options(width = 438L)

#library(WindR)
#w.start()
#w.menu()
#w.isconnected()

#---------------- contracts ----------------
# TODO: configuration
BarSize = 15
BarSize = "DAY"
Underlying ="I"
Exchange = "DCE"
#Underlying ="CU"
#Exchange = "SHF"
#contract years
c.yg = formatC(seq(10,16), width=2, flag='0')
#contract months
c.tg = c("01","05","09")
#c.tg = c("03","06","09","12")
#c.tg = formatC(seq(1,12), width=2, flag='0')
#fields obtained from wind terminal
#fd = c("close", "volume", "oi")
fd = c("close")
fd2 = substr(fd,1,2)
#choose which dataset file to use
ds.date = "2015-12-01"

#---------------- import to csv file ----------------
fpath = paste(getwd(), "/dataset/", sep="")
fname = paste(Underlying, Exchange, ds.date, BarSize, "csv", sep=".")
c.dt = read.table(file=paste(fpath,fname,sep=""), header=TRUE, sep=",")

#field name of c.dt
cf = data.frame(matrix(unlist(strsplit(names(c.dt)[-1],"[.]")), ncol=3, byrow=TRUE))
names(cf) = c("Contract", "Exchange", "Field")
tt = nchar(Underlying)
cf$Year = substr(cf$Contract, tt+1, tt+2)
cf$Month = substr(cf$Contract, tt+3, tt+4)

#choose only c.tg related month, and fd2 related fields
ind = c(TRUE, ((cf$Year %in% c.yg) & (cf$Month %in% c.tg) & cf$Field %in% fd2))
dataset = c.dt[,ind]

##export to csv file
#fpath = paste(getwd(), "/analysis/", sep="")
#fname = paste(Underlying, Exchange, Sys.Date(), BarSize, paste(c.tg,collapse=""), paste(fd2,collapse=""), "csv", sep=".")
#write.table(dataset, file=paste(fpath,fname,sep=""), sep=",", row.names=FALSE, col.names=TRUE)
head(dataset)
tail(dataset)

#---------------- analysis ----------------
tmp = dim(dataset)[2]
ntmp = names(dataset)
sp0 = dataset[,seq(2,tmp-1)] - dataset[,seq(3,tmp)]
bf0 = dataset[,seq(2,tmp-2)] - 2*dataset[,seq(3,tmp-1)] + dataset[,seq(4,tmp)]
names(sp0) = paste(ntmp[seq(2,tmp-1)], "-", ntmp[seq(3,tmp)])
names(bf0) = paste(ntmp[seq(2,tmp-2)], "-2*", ntmp[seq(3,tmp-1)], "+", ntmp[seq(4,tmp)])

# TODO: sp/bf combination
#sp = sp0[,c(1,3,5,7,9,11,13)]

sp = sp0


bf = bf0

#---------------- time series plot ----------------
matplot(sp, type="l", col=dim(sp)[2]:1, xlab=BarSize, ylab="Calendar Spread", main=paste(Underlying,".",Exchange," Time Series Plot"))
legend('topleft', legend=names(sp), text.col=dim(sp)[2]:1, cex=0.65)
grid()

matplot(bf, type="l", col=dim(bf)[2]:1, xlab=BarSize, ylab="Butterfly", main=paste(Underlying,".",Exchange," Time Series Plot"))
legend('topleft', legend=names(bf), text.col=dim(bf)[2]:1, cex=0.65)
grid()

#---------------- seasonal plot ----------------
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
sp3 = sp2[1:min(143, dim(sp2)[1]),]

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
bf3 = bf2[1:min(66, dim(bf2)[1]),]

matplot(bf3, type="l", col=dim(bf3)[2]:1, xlab=BarSize, ylab="Butterfly", main=paste(Underlying,".",Exchange," Seasonal Plot"))
#find the max and min of each combination
mm = matrix(paste(c("min","max"), apply(bf3,2,range,na.rm=TRUE), sep="="), ncol=2, byrow=TRUE)
lgd = paste(colnames(bf3), mm[,1], mm[,2])
legend('topleft', legend=lgd, text.col=dim(bf3)[2]:1, cex=0.65)
grid()



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


