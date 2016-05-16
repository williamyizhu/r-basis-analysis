#check if the working directory is correct
if (tail(unlist(strsplit(getwd(),"/")),n=1) != "r-basis-analysis") {
	print(paste("Wrong working directory:", getwd()))
	break
}

rm(list=ls(all=TRUE)) 
options(width = 438L)

source("//VMWARE-HOST/Shared Folders/Documents/workspace/r-basis-analysis/ExUL.R", echo=FALSE, encoding="GBK")

#directory for original dataset downloaded from quandl, DCE.I.2014.03.Close
ff = unlist(strsplit(getwd(), "/"))
dpath = paste(c(ff[1:length(ff)-1]), collapse="/")
ExUL$fpath = paste(dpath, paste("r-",ExUL[,"dsName"],"-data",sep=""), paste(ExUL[,"Exchange"],ExUL[,"Underlying"],sep="_"), sep="/")
INDEX$fpath = paste(dpath, paste("r-",INDEX[,"dsName"],"-data",sep=""), "INDEX", paste(INDEX[,"Symbol.IND"],"csv",sep="."), sep="/")
FIXED_INCOME$fpath = paste(dpath, paste("r-",FIXED_INCOME[,"dsName"],"-data",sep=""), "FIXED_INCOME", paste(FIXED_INCOME[,"Symbol"],"csv",sep="."), sep="/")

##---------------------------------- futures symbol ----------------------------------
for (i in 1:dim(ExUL)[1]) {	
#	read orginal data files
	c.dt = data.frame(DATETIME=character(0)) 
	flist = dir(ExUL[i, "fpath"])
	for (k in 1:length(flist)) {
#		read from original quandl dataset and select required column from cList		
		xt = read.table(file=paste(ExUL[i,"fpath"],flist[k],sep="/"), header=TRUE, sep=",")
		cList = unlist(strsplit(as.character(ExUL[i,"cList"]),"[.]"))
		zt = xt[,c("DATETIME",cList)]
#		only include column list (cList)
		names(zt) = c("DATETIME", paste(substr(flist[k],1,nchar(flist[k])-3),cList,sep=""))		
		c.dt = merge(c.dt, zt, by="DATETIME", all=TRUE)
	}
	fname = paste(ExUL[i,"Exchange"], ExUL[i,"Underlying"], "csv", sep=".")
	ffile = paste(getwd(), "/", ExUL[i,"dsName"], "_dataset/", fname, sep="")
	write.table(c.dt, file=ffile, sep=",", row.names=FALSE, col.names=TRUE)
#	process status
	print(paste("(", i, "of", dim(ExUL)[1], ")", ExUL[i,"Exchange"], ExUL[i,"Underlying"], "number of files:", length(flist), "DATETIME:", tail(c.dt$DATETIME,n=1)))	
	print(ffile)
}

##---------------------------------- index symbol ----------------------------------
#find unique underlying, e.g., CU, AL, etc.
INDEX_UNDERLYING = unique(INDEX[,"Symbol.FUT"])
for (j in 1:length(INDEX_UNDERLYING)) {
#	iterate through all INDEX Underlying
	sIndex = INDEX[(INDEX[,"Symbol.FUT"]==INDEX_UNDERLYING[j]),]
#	combine same Underlying INDEX data
	c.dt = data.frame(DATETIME=character(0)) 	
	for (k in 1:dim(sIndex)[1]) {
#		same Underlying but different INDEX data from different sources
		zt = read.table(file=sIndex[k,"fpath"], header=TRUE, sep=",")
		zt$DATETIME = as.Date(zt$DATETIME)
		names(zt) = c("DATETIME", sIndex[k,"Symbol.IND"])		
		c.dt = merge(c.dt, zt, by="DATETIME", all=TRUE, sort=TRUE)	
	}
#	use average of different sources
	if (dim(c.dt)[2]<=2) {
		c.dt[,as.character(INDEX_UNDERLYING[j])] = c.dt[,-1]
	} else {
		c.dt[,as.character(INDEX_UNDERLYING[j])] = apply(c.dt[,-1], 1, mean, na.rm=TRUE)		
	}
#	export to a csv file
	fname = paste(INDEX_UNDERLYING[j], "csv", sep=".")
	ffile = paste(getwd(), "/INDEX/", fname, sep="")
	write.table(c.dt, file=ffile, sep=",", row.names=FALSE, col.names=TRUE)		
#	print results
	print(paste("(", j, "of", length(INDEX_UNDERLYING), ") INDEX:", INDEX_UNDERLYING[j], "number of files:", dim(sIndex)[1], "DATETIME:", tail(c.dt$DATETIME,n=1)))	
	print(ffile)	
}

##---------------------------------- fiexed income symbol ----------------------------------
FIXED_INCOME_Exchange = unique(FIXED_INCOME[,"Exchange"])
for (j in 1:length(FIXED_INCOME_Exchange)) {
#	iterate through all FIXED_INCOME Exchange, e.g., SHIBOR
	sIndex = FIXED_INCOME[(FIXED_INCOME[,"Exchange"]==FIXED_INCOME_Exchange[j]),]
#	combine same FIXED_INCOME Exchange data
	c.dt = data.frame(DATETIME=character(0)) 	
	for (k in 1:dim(sIndex)[1]) {
#		same Exchange but different term data
		zt = read.table(file=sIndex[k,"fpath"], header=TRUE, sep=",")
		zt$DATETIME = as.Date(zt$DATETIME)
		names(zt) = c("DATETIME", sIndex[k,"Symbol"])		
		c.dt = merge(c.dt, zt, by="DATETIME", all=TRUE, sort=TRUE)	
	}
#	export to a csv file
	fname = paste(FIXED_INCOME_Exchange[j], "csv", sep=".")
	ffile = paste(getwd(), "/FIXED_INCOME/", fname, sep="")
	write.table(c.dt, file=ffile, sep=",", row.names=FALSE, col.names=TRUE)		
#	print results
	print(paste("(", j, "of", length(FIXED_INCOME_Exchange), ") Exchange:", FIXED_INCOME_Exchange[j], "number of files:", dim(sIndex)[1], "DATETIME:", tail(c.dt$DATETIME,n=1)))	
	print(ffile)	
}

