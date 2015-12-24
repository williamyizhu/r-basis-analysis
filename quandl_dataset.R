rm(list=ls(all=TRUE)) 
options(width = 438L)

#download data from quandl, prepare the dataset, analysis
ExUL = data.frame(Exchange=character(), Underlying=character())
#ExUL = rbind(ExUL, data.frame(Exchange="CME", Underlying="CL", clist=c("Settle.Volume")))
#ExUL = rbind(ExUL, data.frame(Exchange="CME", Underlying="NG"))
#ExUL = rbind(ExUL, data.frame(Exchange="CBOE", Underlying="VX"))
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="I", clist=c("Close.Settle.Volume")))
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="M", clist=c("Close.Settle.Volume")))
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="CU", clist=c("Close.Settle.Volume")))
ExUL = unique(ExUL)

#directory for original dataset downloaded from quandl
ff = unlist(strsplit(getwd(), "/"))
fpath = paste(c(ff[1:length(ff)-1],"r-quandl-data"), collapse="/")
ExUL$fpath = paste(fpath, paste(ExUL[,"Exchange"],ExUL[,"Underlying"],sep="_"), sep="/")

#same combined dataset to local directory
fpath = paste(getwd(), "/quandl_dataset/", sep="")

for (i in 1:dim(ExUL)[1]) {		
	c.dt = data.frame(Date=character(0)) 
	flist = dir(ExUL[i, "fpath"])
	print(paste(ExUL[i,"Exchange"], ExUL[i,"Underlying"], "number of files :", length(flist)))
	for (k in 1:length(flist)) {
#		read from original quandl dataset and select required column from clist		
		xt = read.table(file=paste(ExUL[i,"fpath"],flist[k],sep="/"), header=TRUE, sep=",")
		clist = unlist(strsplit(as.character(ExUL[i,"clist"]),"[.]"))
		zt = xt[,c("Date",clist)]
#		only include column list (clist)
		names(zt) = c("Date", paste(substr(flist[k],1,nchar(flist[k])-3),clist,sep=""))		
		c.dt = merge(c.dt, zt, by="Date", all=TRUE)
	}
	fname = paste(ExUL[i,"Exchange"], ExUL[i,"Underlying"], "csv", sep=".")
	ffile = paste(fpath, fname, sep="")
	write.table(c.dt, file=ffile, sep=",", row.names=FALSE, col.names=TRUE)
	print(ffile)
}



