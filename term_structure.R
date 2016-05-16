library(rgl)

#create export .png file directory
ff = "Term_Structure"
if (!dir.exists(ff)) {
	dir.create(ff)
}

#length of the term structure
#term_length = 3

#local dataset, including delivery month
ldataset = dataset0
#local dataset, after adjustment for nmbd=1; nmfa=6; 
#ldataset = dataset

#add a dummy column before the 1st contract
#term structure data
tsData = vector()
#front month (M1) contract
FrontMonth = vector()
for (i in 1:(dim(ldataset)[2]-term_length)) {
#	dummy month, start month, end month
	dMonth = i
	sMonth = i + 1
	eMonth = i + term_length
#	print(paste("dMonth:", dMonth, "sMonth:", sMonth, "eMonth:", eMonth))
	
#	select data where dInd==TRUE, i.e., previous month has expired
	dInd = is.na(ldataset[,i])
#	current month and forward months, all data are available
#	currnet month may not be the latest month, e.g., DCE.I, active month is 01, 05, 09
	fData = ldataset[,seq(sMonth,eMonth)]	
	fInd = apply(fData, 1, function(x){all(!is.na(x))})	
#	most recent term, i.e., M1, M2, ...
	dd = fData[(dInd&fInd),]
	colnames(dd) = paste("M",formatC(seq(1,term_length),width=2,flag="0"),sep="")
	
#	rbind data
	tsData = rbind(tsData, dd)
#	set the front month (M1) contract name
	FrontMonth = c(FrontMonth, rep(colnames(fData[(dInd&fInd),])[1],dim(dd)[1]))
}

#-------------------- add INDEX data if available -------------------- 
fpath = paste(getwd(),"/INDEX/",sep="")
fname = paste(Symbol,"csv",sep=".") 
#merge INDEX price with term structure prices
if (fname %in% dir(fpath)){
	mg = read.table(file=paste(fpath,fname,sep=""), header=TRUE, sep=",")
	zt = data.frame(M00=mg[,Symbol], row.names=mg[,"DATETIME"])
	TermStructure = merge(zt, tsData, by="row.names", all.y=TRUE)
	rownames(TermStructure) = TermStructure[,1]
	TermStructure = TermStructure[,-1]
} else {
	TermStructure = tsData
}

#anchor month, i.e., the month that other month reference to
#number of term strucure lines, displayed on the plot
#sampling frequency, take the observation of term structure line
#data entry per line, e.g., use the average
anchMonth = "M02"
nlines = 10
sfreq = 1
dpline = 3

nTermStructure = TermStructure - TermStructure[,anchMonth]
nTermStructure[,"rLab"] = FrontMonth
xt = TransTS(nTermStructure, nlines, sfreq, dpline)

#export term structure data
pctTermStructure = TermStructure / TermStructure[,anchMonth]
pctTermStructure[,"rLab"] = FrontMonth
pct = TransTS(pctTermStructure, nlines, sfreq, dpline)
ffile = paste(getwd(), "/Term_Structure/", Symbol, ".csv", sep="")
write.table(pct, file=ffile, sep=",", row.names=TRUE, col.names=TRUE)

#-------------------- plot data and label-------------------- 
tmdata = subset(xt, select=-rLab)
tmrlab = paste(rownames(xt), xt[,"rLab"])

#-------------------- 2d plot -------------------- 
mlab = paste(Symbol, "Average Window:", dpline)
matplot(t(tmdata), type="o", col=nlines:1, lty=nlines:1, pch=nlines:1, xlab="Term Strucutre (M)", ylab="Calendar Spread", main=mlab)
legend('bottomleft', legend=tmrlab, text.col=nlines:1, col=nlines:1, lty=nlines:1, pch=nlines:1, cex=0.65, merge=FALSE)
grid()
#export to a .png file
pathname = paste(getwd(),"/",ff,"/",Symbol,".png",sep="")
dev.copy(png, width=672, height=672, filename=pathname)
dev.off()

#-------------------- 3d plot -------------------- 
clear3d()
tspersp3d(tmdata, range(tmdata), FALSE, "Date", "Term Structure", "Calendar Spread", Symbol, surface.col="white", points.col="red", lines.col="blue")
#add legend
legend3d("right", c("Date",paste(1:dim(xt)[1],tmrlab,sep=": ")))





