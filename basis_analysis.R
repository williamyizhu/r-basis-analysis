#rm(list=ls(all=TRUE)) 
#options(width = 438L)
#
##---------------- contracts ----------------
#fmonth = "02"

#leading month for both sp and bf
sp = sp0[,(sp0.cf[,"C1.Month"]%in%fmonth)]
bf = bf0[,(bf0.cf[,"C1.Month"]%in%fmonth)]
#column header names
sp.cf = sp0.cf[(sp0.cf[,"C1.Month"]%in%fmonth),]
bf.cf = bf0.cf[(bf0.cf[,"C1.Month"]%in%fmonth),]

# TODO TransData = function(sp, nend, nobs)
# nend>=0, number of data points from the end; nobs is the number of obseration from nend
# TransData(sp, 0, 500) includes all data points
# TransData(sp, 20, 300) starts from the beginning and excludes the last month (e.g., delivery month)
sp2 = TransData(sp)
bf2 = TransData(bf)

last_date = tail(rownames(dataset), n=1)
month_str = paste("Active=", paste(c.mg,collapse="."), "_Front=", paste(fmonth,collapse="."), "_Data=", dsName, sep="")
gpath = paste(getwd(),"/",Symbol,"/",sep="")
####################################################################################################
#calendar spread and butterfly
####################################################################################################
xlab = paste(Symbol, last_date, "exOutlier =", exOutlier)

#---------------- seasonal plot ----------------
# TODO ssplot = function(sp2, filename, width=1440, height=785, xlab="", ylab="", main="") 
mlab = paste(month_str, "Seasonal", sep="_")
#calendar spread
pathname = paste(gpath,paste(Symbol,"CS",mlab,sep="_"),".png",sep="")
ssplot(sp2, filename=pathname, xlab=xlab, ylab="Calendar Spread", main=mlab)
#butterfly
pathname = paste(gpath,paste(Symbol,"BF",mlab,sep="_"),".png",sep="")
ssplot(bf2, filename=pathname, xlab=xlab, ylab="Butterfly", main=mlab)

#---------------- histgram and density plot ----------------
# TODO ssdensity = function(sp2, filename, width=1440, height=785, xlab="", ylab="", main="")
mlab = paste(month_str, "Density", sep="_")
pathname = paste(gpath,paste(Symbol,"CS",mlab,sep="_"),".png",sep="")
ssdensity(sp2, filename=pathname, xlab=xlab, ylab="Calendar Spread Probability", main=mlab)
pathname = paste(gpath,paste(Symbol,"BF",mlab,sep="_"),".png",sep="")
ssdensity(bf2, filename=pathname, xlab=xlab, ylab="Butterfly Probability", main=mlab)

#---------------- box plot ----------------
# TODO ssboxplot = function(sp2, imonth, filename, width=1440, height=785, names="", xlab="", ylab="", main="")
mlab = paste(month_str, "Boxplot", sep="_")
pathname = paste(gpath,paste(Symbol,"CS",mlab,sep="_"),".png",sep="")
ssboxplot(sp2, (sp.cf[,"C1.Month"]=="01"), filename=pathname, names=sp.cf$ShortName, xlab=xlab, ylab="Calendar Spread", main=mlab)
pathname = paste(gpath,paste(Symbol,"BF",mlab,sep="_"),".png",sep="")
ssboxplot(bf2, (bf.cf[,"C1.Month"]=="01"), filename=pathname, names=bf.cf$ShortName, xlab=xlab, ylab="Butterfly", main=mlab)



#colvec = rep("white", dim(bf2)[2])
#cind = bf.cf[,"C1.Month"]=="01"
#colvec[cind] = "red"
#boxplot(bf2, col=colvec, names=bf.cf$ShortName, xlab=xlab, ylab="Butterfly", main=mlab)
#lobs = tail(na.locf(bf2), n=1)
#points(seq(1,dim(bf2)[2]), lobs, col="blue", pch=19, cex=1.25)
#grid()
#dev.copy(png, width=1440, height=785, filename=paste(gpath,paste(Symbol,"BF",mlab,sep="_"),".png",sep=""))
#dev.off ()

#next
####################################################################################################
#butterfly
####################################################################################################
##---------------- time series plot ----------------
#matplot(bf, type="l", col=dim(bf)[2]:1, xlab=paste(Symbol,"Day"), ylab="Butterfly", main=paste("TimeSeries_",month_str))
#legend('topleft', legend=names(bf), text.col=c(dim(bf)[2]:1,1), cex=0.65)
#grid()
#dev.copy(png, width=1440, height=785, filename=paste(paste(Symbol,"BF","TS",month_str,sep="_"),"png",sep="."))
#dev.off ()

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


