rm(list=ls(all=TRUE)) 
options(width = 438L)

#check if the working directory is correct
if (tail(unlist(strsplit(getwd(),"/")),n=1) != "r-basis-analysis") {
	print(paste("Wrong working directory:", getwd()))
	break
}

##---------------------------------- futures symbol ----------------------------------
#ExUL = data.frame(Exchange=character(), Underlying=character())
source("//VMWARE-HOST/Shared Folders/Documents/workspace/r-basis-analysis/ExUL.R", echo=FALSE, encoding="GBK")

#directory for original dataset downloaded from quandl, DCE.I.2014.03.Close
ff = unlist(strsplit(getwd(), "/"))
dpath = paste(c(ff[1:length(ff)-1]), collapse="/")
ExUL$fpath = paste(dpath, paste("r-",ExUL[,"dsName"],"-data",sep=""), paste(ExUL[,"Exchange"],ExUL[,"Underlying"],sep="_"), sep="/")

for (i in 1:dim(ExUL)[1]) {		
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
	print(paste("(", i, "of", dim(ExUL)[1], ")", ExUL[i,"Exchange"], ExUL[i,"Underlying"], "number of files:", length(flist), "Last:", tail(c.dt$DATETIME,n=1)))
	print(ffile)
}



