#check if the working directory is correct
if (tail(unlist(strsplit(getwd(),"/")),n=1) != "r-basis-analysis") {
	print(paste("Wrong working directory:", getwd()))
	break
}

rm(list=ls(all=TRUE)) 
options(width=438L)

library(lubridate)
library(zoo)
source(paste(getwd(),"func_lib.R",sep="/"), echo=FALSE, encoding="GBK")

##---------------------------------- futures symbol ----------------------------------
#ExUL = data.frame(Exchange=character(), Underlying=character())
source(paste(getwd(),"ExUL.R",sep="/"), echo=FALSE, encoding="GBK")

ExUL = ExUL[ExUL$dsName=="wind",]
ExUL = ExUL[ExUL[,"Symbol"] %in% c("SHFE.CU","SHFE.AL","SHFE.ZN"),]
ExUL = ExUL[ExUL[,"Symbol"] %in% c("SHFE.CU"),]
#ExUL = ExUL[ExUL[,"Symbol"] %in% c("SHFE.AL"),]
#ExUL = ExUL[ExUL[,"Symbol"] %in% c("SHFE.ZN"),]
#ExUL = ExUL[ExUL[,"Symbol"] %in% c("SHFE.RU"),]
#ExUL = ExUL[ExUL[,"Symbol"] %in% c("DCE.I","DCE.J","SHFE.RB"),]
#ExUL = ExUL[ExUL[,"Symbol"] %in% c("DCE.I"),]
#ExUL = ExUL[ExUL[,"Symbol"] %in% c("DCE.J"),]
#ExUL = ExUL[ExUL[,"Symbol"] %in% c("SHFE.RB"),]
#ExUL = ExUL[ExUL[,"Symbol"] %in% c("SHFE.RB","SHFE.HC"),]
#ExUL = ExUL[ExUL[,"Symbol"] %in% c("DCE.M"),]

##---------------------------------- main loop ----------------------------------
#dataset in list format, multiple symbols in one data.frame
dataset0.list = list()
dataset0.cf.list = list()
dataset.list = list()
dataset.cf.list = list()
for (ijk in 1:dim(ExUL)[1]) {
#	get underlying data from ExUL
	Symbol = ExUL[ijk,"Symbol"]
	dsName = ExUL[ijk,"dsName"]
	nmbd = ExUL[ijk,"nmbd"]
	nmfa = ExUL[ijk,"nmfa"]
	exOutlier = ExUL[ijk,"exOutlier"]
	term_length = ExUL[ijk,"term.len"]
	c.yg = unlist(strsplit(as.character(ExUL[ijk,"c.yg"]), "[.]"))
	c.mg = unlist(strsplit(as.character(ExUL[ijk,"c.mg"]), "[.]"))
	c.mg.reg = unlist(strsplit(as.character(ExUL[ijk,"c.mg.reg"]), "[.]"))
	fmonth_vec = unlist(strsplit(as.character(ExUL[ijk,"fmonth_vec"]), "[.]"))
 
#---------------- import to csv file ----------------
#	choose which dataset file to use
	fpath = paste(getwd(), "/", dsName, "_dataset/", sep="")
	fname = paste(Symbol, "csv", sep=".")
	dtable = read.table(file=paste(fpath,fname,sep=""), header=TRUE, sep=",")
	dtable$DATETIME = as.Date(dtable$DATETIME)
	
#	create export .png file directory
	if (!dir.exists(Symbol)) {
		dir.create(Symbol)
	}

#---------------- divide dtable into price data and benchmark data ----------------
#	e.g., DATETIME, I1403.DCE.cl, I1403.DCE.vo, I1403.DCE.oi, I1404.DCE.cl, I1404.DCE.vo, I1404.DCE.oi, ...
	id = names(dtable) %in% c("DATETIME")
	c.dt = dtable[!id]
	rownames(c.dt) = dtable$DATETIME
#	field name of original dataset c.dt, "YearMonth.reg" is for regression analysis
	cf = data.frame(matrix(unlist(strsplit(names(c.dt),"[.]")), ncol=5, byrow=TRUE))
	names(cf) = c("Exchange", "Underlying", "Year", "Month", "Field")
	cf[,"Symbol"] = paste(cf[,"Exchange"], cf[,"Underlying"], sep=".")
	cf[,"YearMonth"] = paste(cf[,"Year"], cf[,"Month"], sep=".")
	cf[,"Month.reg"] = as.character(cf[,"Month"])
	for (jk in 1:length(c.mg)) {
		cf[,"Month.reg"] = gsub(c.mg[jk], c.mg.reg[jk], cf[,"Month.reg"], fixed=TRUE)
	}
	cf[,"YearMonth.reg"] = paste(cf[,"Year"], cf[,"Month.reg"], sep=".")	
	
#	check c.dt, exclude columns with all NAs
	ind = apply(c.dt, 2, function(x){all(is.na(x))}) %in% TRUE
	if (any(ind)) {
		print("------------------------- c.dt error -------------------------")
		print(matrix(colnames(c.dt)[ind]))
	}

#	choose only c.mg related month, and fd2 related fields, fd = c("close", "volume", "oi")
	ind = ((cf$Year %in% c.yg) & (cf$Month %in% c.mg) & (cf$Field=="CLOSE"))
	dataset0 = c.dt[,ind]
#	exclude rows with all NA value
	gg = apply(dataset0, 1, function(x){!all(is.na(x))})
	dataset0 = dataset0[gg,]
	dataset0.cf = cf[ind,]

#---------------- dataset only include data for analysis ----------------
	dataset = dataset0
#	select analysis range for futures price
	dataset.cf = cf[ind,]
#	first / last day of delivery month
	fddm = as.Date(paste(dataset.cf[,"Year"], dataset.cf[,"Month"], "01", sep="-"))
	lddm = as.Date(format(fddm+35, "%Y-%m-01")) - 1
#	number of month before delivery
	fd.nmbd = as.Date(format(fddm %m-% months(nmbd), "%Y-%m-01"))
	ld.nmbd = as.Date(format(fd.nmbd+35, "%Y-%m-01")) - 1
#	number of month for analysis, including the month before delivery month
	fd.nmfa = as.Date(format(fddm %m-% months(nmbd+nmfa-1), "%Y-%m-01"))
	ld.nmfa = as.Date(format(fd.nmfa+35, "%Y-%m-01")) - 1
#	dates for analysis
	dataset.cf$fd = fd.nmfa
	dataset.cf$ld = ld.nmbd
#	only include analysis data month
	dd = as.Date(rownames(dataset))
	for (i in 1:dim(dataset)[2]) {
#		month data for analysis
		ind = (dataset.cf[i,"fd"]<=dd) & (dd<=dataset.cf[i,"ld"])
		dataset[!ind,i] = NA
	}

#volume dataset
#vind = ((cf$Year %in% c.yg) & (cf$Month %in% c.mg) & (cf$Field=="VOLUME"))
#dataset_volume = c.dt[,vind]

#---------------- dataset in list format for regression analysis, e.g., dataset0.list[["2016.05"]], DATETIME, DCE.I, DCE.J, SHFE.RB ----------------
	for (j in 1:dim(dataset0.cf)[1]) {
#		tt is a data.frame object
		tt = subset(dataset0, select=colnames(dataset0)[j])	
		tt$DATETIME = as.Date(rownames(tt))		
#		create YearMonth data.frame if not exist
		ymreg = dataset0.cf[j,"YearMonth.reg"]
		if (is.null(dataset0.list[[ymreg]])) {
			dataset0.list[[ymreg]] = data.frame(DATETIME=character(0)) 		
		}	
		ss = dataset0.list[[ymreg]]		
#		combine symbols dataset0 and dataset0.cf
		dataset0.list[[ymreg]] = merge(ss, tt, by="DATETIME", all=TRUE, sort=TRUE)		
		dataset0.cf.list[[ymreg]] = rbind(dataset0.cf.list[[ymreg]], dataset0.cf[j,])	
	}

	for (j in 1:dim(dataset.cf)[1]) {
#		tt is a data.frame object
		tt = subset(dataset, select=colnames(dataset)[j])	
		tt$DATETIME = as.Date(rownames(tt))		
#		create YearMonth data.frame if not exist
		ymreg = dataset.cf[j,"YearMonth.reg"]
		if (is.null(dataset.list[[ymreg]])) {
			dataset.list[[ymreg]] = data.frame(DATETIME=character(0)) 		
		}	
		ss = dataset.list[[ymreg]]		
#		combine symbols dataset and dataset.cf
		dataset.list[[ymreg]] = merge(ss, tt, by="DATETIME", all=TRUE, sort=TRUE)		
		dataset.cf.list[[ymreg]] = rbind(dataset.cf.list[[ymreg]], dataset.cf[j,])	
	}
	
#-------------------- add INDEX data if available -------------------- 
	fpath = paste(getwd(),"/INDEX/",sep="")
	fname = paste(Symbol,"csv",sep=".") 
	ymreg = "0000.00"
#	merge INDEX price with term structure prices
	if (fname %in% dir(fpath)){
		mg = read.table(file=paste(fpath,fname,sep=""), header=TRUE, sep=",")		
#		tt is a data.frame object
		tt = mg[,c("DATETIME",Symbol)]
		tt$DATETIME = as.Date(tt[,"DATETIME"])				
#		create YearMonth data.frame if not exist		
		if (is.null(dataset0.list[[ymreg]])) {
			dataset0.list[[ymreg]] = data.frame(DATETIME=character(0)) 		
		}	
		ss = dataset0.list[[ymreg]]		
#		combine symbols dataset0 and dataset0.cf
		dataset0.list[[ymreg]] = merge(ss, tt, by="DATETIME", all=TRUE, sort=TRUE)		
#		dataset0.cf.list[[ymreg]] = rbind(dataset0.cf.list[[ymreg]], dataset0.cf[j,])			
	} else {
		print(paste(Symbol, "INDEX does not exist"))
	}
#	INDEX data are same for dataset.list and dataset0.list
	dataset.list[[ymreg]] = dataset0.list[[ymreg]]
	
####################################################################################################
#data analysis
####################################################################################################
#	spread and butterfly analysis
	tmp = dim(dataset)[2]
	ntmp = names(dataset)
	sp0 = dataset[,seq(1,tmp-1)] -   dataset[,seq(2,tmp)]
	bf0 = dataset[,seq(1,tmp-2)] - 2*dataset[,seq(2,tmp-1)] + dataset[,seq(3,tmp)]
	names(sp0) = paste(ntmp[seq(1,tmp-1)], "-",   ntmp[seq(2,tmp)])
	names(bf0) = paste(ntmp[seq(1,tmp-2)], "-2*", ntmp[seq(2,tmp-1)], "+", ntmp[seq(3,tmp)])

##boxplot.stats, exclude outliers
#if (exOutlier) {
#	sg = apply(sp2,2,boxplot.stats)
#	for (k in 1:length(sg)) {
#		knd = sp2[,k] %in% sg[[k]]$out
#		sp2[knd,k] = NA
#	}
#}

#	sp / bf column names index
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
#	single month
	for (fmonth in fmonth_vec) {
		print(paste("--------------------------",Symbol,fmonth,"(",head(c.yg,n=1),"-",tail(c.yg,n=1),")","--------------------------"))
		source("basis_analysis.R", echo=FALSE, encoding="GBK")		
	}
#	all months
	fmonth = fmonth_vec
	print(paste("--------------------------",Symbol,paste(fmonth,collapse="."),"(",head(c.yg,n=1),"-",tail(c.yg,n=1),")","--------------------------"))
	source("basis_analysis.R", echo=FALSE, encoding="GBK")
	
#	term structure
	print(paste("--------------------------",Symbol,"Term Structure","--------------------------"))
	source("term_structure.R", echo=FALSE, encoding="GBK")
}

####################################################################################################
#regression analysis
####################################################################################################

#print(paste(length(dataset0.list), ))
#print(paste(length(dataset0.cf.list), ))