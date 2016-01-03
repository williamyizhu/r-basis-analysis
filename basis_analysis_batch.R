rm(list=ls(all=TRUE)) 
options(width=438L)

library(lubridate)

#check if the working directory is correct
if (tail(unlist(strsplit(getwd(),"/")),n=1) != "r-basis-analysis") {
	print(paste("Wrong working directory:", getwd()))
	break
}

#transform dataset, from time series data to seasonal data
TransData = function(sp, nend, nobs) {
#	re-organize sp data	
	mlen = max(apply(sp, 2, function(x){sum(!is.na(x))}))
	sptmp = vector()
	for (i in 1:dim(sp)[2]) {
		mtmp = sp[!is.na(sp[,i]),i]
		length(mtmp) = mlen
		sptmp = cbind(sptmp, mtmp)
	}
	nn = dim(sptmp)[1]
	sp2 = sptmp[seq(max(1,nn-nend-nobs),nn-nend),]
#	sp2 = sptmp
	colnames(sp2) = names(sp)	
	sp2
}

#seasonal plot
ssplot = function(sp2, filename, width=1440, height=785, xlab="", ylab="", main="") {
	matplot(sp2, type="l", col=dim(sp2)[2]:1, xlab=xlab, ylab=ylab, main=main)
#	find the summary of each combination, return of summary() is a list, if contains NA, mgg is a list, otherwise, mgg is a matrix
	mgg = apply(sp2, 2, summary)
	if (class(mgg) == "list") {
		summ = lapply(mgg, function(x){paste(paste(names(x),unlist(x),sep="="),collapse=" | ")})
	} else {
		summ = apply(mgg, 2, function(x){paste(paste(rownames(mgg),x,sep="="),collapse=" | ")})
	}
	lgd = paste(names(summ),unlist(summ))
	legend('topleft', legend=lgd, text.col=dim(sp2)[2]:1, cex=0.65)
	grid()	
	dev.copy(png, width=1440, height=785, filename=filename)
	dev.off ()
}

#seasonal density plot
ssdensity = function(sp2, filename, width=1440, height=785, xlab="", ylab="", main="") {
#	check sp2, exclude all NA column
	ind = apply(sp2, 2, function(x){all(is.na(x))}) %in% TRUE
	if (any(ind)) {
#		print(paste("Error:",(colnames(sp2)[ind])))
		print(matrix(colnames(sp2)[ind]))
	}
#	density analysis
	sp.den = apply(sp2[,!ind],2,density,na.rm=TRUE)
	xrange = range(lapply(sp.den, function(m){range(m$x)}))
	yrange = range(lapply(sp.den, function(m){range(m$y)}))
	plot(NA, xlim=xrange, ylim=c(0,yrange[2]), xlab=xlab, ylab=ylab, main=main)
	for (i in 1:length(sp.den)) {
		lines(sp.den[[i]], col=length(sp.den)-i+1)
	}
	legend('topleft', legend=names(sp.den), text.col=length(sp.den):1, cex=0.65)
	grid()
	dev.copy(png, width=width, height=height, filename=filename)
	dev.off ()
}

#seasonal box plot
ssboxplot = function(sp2, imonth, filename, width=1440, height=785, names="", xlab="", ylab="", main="") {
#	use a different color for a particular month, e.g., Jan
	colvec = rep("white", dim(sp2)[2])
	colvec[imonth] = "red"
	boxplot(sp2, col=colvec, names=names, xlab=xlab, ylab=ylab, main=main)
#	indicate the value of last observatioin
	lobs = tail(na.locf(sp2), n=1)
	points(seq(1,dim(sp2)[2]), lobs, col="blue", pch=19, cex=1.25)
	grid()
	dev.copy(png, width=width, height=height, filename=filename)
	dev.off ()
}

#---------------- contracts ----------------
Exchange="CME"; Underlying="CL"; dsName="quandl"; exOutlier=FALSE; c.yg=formatC(seq(2001,2016),width=4,flag='0'); c.mg=formatC(seq(1,12),width=2,flag='0'); fmonth_vec=formatC(seq(1,12),width=2,flag='0');
Exchange="DCE"; Underlying="I"; dsName="wind"; exOutlier=FALSE; c.yg=formatC(seq(2001,2016),width=4,flag='0'); c.mg=c("01","05","09"); fmonth_vec=c("01","05","09");
#Exchange="DCE"; Underlying="M"; dsName="wind"; c.yg=formatC(seq(2001,2016),width=4,flag='0'); c.mg=c("01","05","09"); fmonth_vec=c("01","05","09");
#Exchange="DCE"; Underlying="C"; dsName="wind"; c.yg=formatC(seq(2001,2016),width=4,flag='0'); c.mg=c("01","05","09"); fmonth_vec=c("01","05","09");
#Exchange="SHFE"; Underlying="CU"; dsName="wind"; c.yg=formatC(seq(2011,2016),width=4,flag='0'); c.mg=formatC(seq(1,12),width=2,flag='0'); fmonth_vec=formatC(seq(1,12),width=2,flag='0');
Exchange="SHFE"; Underlying="AL"; dsName="wind"; exOutlier=FALSE; c.yg=formatC(seq(2001,2016),width=4,flag='0'); c.mg=formatC(seq(1,12),width=2,flag='0'); fmonth_vec=formatC(seq(1,12),width=2,flag='0');
#Exchange="SHFE"; Underlying="RB"; dsName="wind"; c.mg=c("01","05","10"); fmonth_vec=c("01","05","10");

ff = paste(Exchange, Underlying, sep="_")
if (!dir.exists(ff)) {
	dir.create(ff)
}

#---------------- import to csv file ----------------
#choose which dataset file to use
Symbol = paste(Exchange, Underlying, sep=".")
fpath = paste(getwd(), "/", dsName, "_dataset/", sep="")
fname = paste(Symbol, "csv", sep=".")
dtable = read.table(file=paste(fpath,fname,sep=""), header=TRUE, sep=",")
dtable$DATETIME = as.Date(dtable$DATETIME)

#---------------- divide dtable into price data and benchmark data ----------------
#e.g., DATETIME, I1403.DCE.cl, I1403.DCE.vo, I1403.DCE.oi, I1404.DCE.cl, I1404.DCE.vo, I1404.DCE.oi, ...
id = names(dtable) %in% c("DATETIME")
c.dt = dtable[!id]
rownames(c.dt) = dtable$DATETIME
#field name of original dataset c.dt
cf = data.frame(matrix(unlist(strsplit(names(c.dt),"[.]")), ncol=5, byrow=TRUE))
names(cf) = c("Exchange", "Underlying", "Year", "Month", "Field")

#check c.dt
ind = apply(c.dt, 2, function(x){all(is.na(x))}) %in% TRUE
if (any(ind)) {
	print("------------------------- c.dt error -------------------------")
	print(matrix(colnames(c.dt)[ind]))
}

#choose only c.mg related month, and fd2 related fields, fd = c("close", "volume", "oi")
ind = ((cf$Year %in% c.yg) & (cf$Month %in% c.mg) & (cf$Field=="CLOSE"))
dataset = c.dt[,ind]
#exclude rows with all NA value
gg = apply(dataset, 1, function(x){!all(is.na(x))})
dataset = dataset[gg,]

#select analysis range for futures price
dataset.cf = cf[ind,]
#first / last day of delivery month
fddm = as.Date(paste(dataset.cf[,"Year"], dataset.cf[,"Month"], "01", sep="-"))
lddm = as.Date(format(fddm+35, "%Y-%m-01")) - 1
#number of month before delivery
nmbd = 2
fd.nmbd = as.Date(format(fddm %m-% months(nmbd), "%Y-%m-01"))
ld.nmbd = as.Date(format(fd.nmbd+35, "%Y-%m-01")) - 1
#number of month for analysis, including the month before delivery month
nmfa = 4
fd.nmfa = as.Date(format(fddm %m-% months(nmbd+nmfa-1), "%Y-%m-01"))
ld.nmfa = as.Date(format(fd.nmfa+35, "%Y-%m-01")) - 1
#dates for analysis
dataset.cf$fd = fd.nmfa
dataset.cf$ld = ld.nmbd

#only include analysis data month
dd = as.Date(rownames(dataset))
for (i in 1:dim(dataset)[2]) {
#	month data for analysis
	ind = (dataset.cf[i,"fd"]<=dd) & (dd<=dataset.cf[i,"ld"])
	dataset[!ind,i] = NA
}

#
##	boxplot.stats, exclude outliers
#if (exOutlier) {
#	sg = apply(sp2,2,boxplot.stats)
#	for (k in 1:length(sg)) {
#		knd = sp2[,k] %in% sg[[k]]$out
#		sp2[knd,k] = NA
#	}
#}
#volume dataset
#vind = ((cf$Year %in% c.yg) & (cf$Month %in% c.mg) & (cf$Field=="VOLUME"))
#dataset_volume = c.dt[,vind]

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
sp0.cf = data.frame(C1=ntmp[seq(1,tmp-1)], C2=ntmp[seq(2,tmp)])
bf0.cf = data.frame(C1=ntmp[seq(1,tmp-2)], C2=ntmp[seq(2,tmp-1)], C3=ntmp[seq(3,tmp)])
for (CN in c("C1","C2")) {
	mq = data.frame(matrix(unlist(strsplit(as.character(sp0.cf[,CN]),"[.]")), ncol=5, byrow=TRUE))
	names(mq) = paste(CN, c("Exchange", "Underlying", "Year", "Month", "Field"), sep=".")
	mq[,paste(CN,"YearMonth",sep=".")] = paste(substr(mq[,paste(CN,"Year",sep=".")],3,4),mq[,paste(CN,"Month",sep=".")],sep="")
	sp0.cf = cbind(sp0.cf, mq)
}
for (CN in c("C1","C2","C3")) {
	mq = data.frame(matrix(unlist(strsplit(as.character(bf0.cf[,CN]),"[.]")), ncol=5, byrow=TRUE))
	names(mq) = paste(CN, c("Exchange", "Underlying", "Year", "Month", "Field"), sep=".")
	mq[,paste(CN,"YearMonth",sep=".")] = paste(substr(mq[,paste(CN,"Year",sep=".")],3,4),mq[,paste(CN,"Month",sep=".")],sep="")
	bf0.cf = cbind(bf0.cf, mq)
}
sp0.cf$ShortName = paste(sp0.cf[,"C1.YearMonth"], sp0.cf[,"C2.YearMonth"], sep="-")
bf0.cf$ShortName = paste(bf0.cf[,"C1.YearMonth"], bf0.cf[,"C2.YearMonth"], bf0.cf[,"C3.YearMonth"], sep="-")

####################################################################################################
#process fmonth seperately 
####################################################################################################
#single month
for (fmonth in fmonth_vec) {
	print(paste("--------------------------",Exchange,Underlying,fmonth,"(",head(c.yg,n=1),"-",tail(c.yg,n=1),")","--------------------------"))
	source("//VMWARE-HOST/Shared Folders/Documents/workspace/r-basis-analysis/basis_analysis.R", echo=FALSE, encoding="GBK")
}
#all months
fmonth = fmonth_vec
print(paste("--------------------------",Exchange,Underlying,paste(fmonth,collapse="."),"(",head(c.yg,n=1),"-",tail(c.yg,n=1),")","--------------------------"))
source("//VMWARE-HOST/Shared Folders/Documents/workspace/r-basis-analysis/basis_analysis.R", echo=FALSE, encoding="GBK")
