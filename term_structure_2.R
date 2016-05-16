#create ExUL for Exchange.Underlying
ExUL = data.frame()
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="CU", surface.col="white", points.col="green", lines.col="blue", text.col="black"))
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="AL", surface.col="red", points.col="green", lines.col="blue", text.col="red"))
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="ZN", surface.col="green", points.col="green", lines.col="blue", text.col="green"))

ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="I", surface.col="white", points.col="green", lines.col="blue", text.col="black"))
ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="J", surface.col="red", points.col="green", lines.col="blue", text.col="red"))
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="RB", surface.col="green", points.col="green", lines.col="blue", text.col="green"))

#import term structure data for ExUL
ExUL[,"Symbol"] = paste(ExUL[,"Exchange"], ExUL[,"Underlying"], sep=".")
tsList = list()
for (i in 1:dim(ExUL)[1]) {
#	print(ExUL[i,]$Symbol)	
	ffile = paste(getwd(), "/Term_Structure/", ExUL[i,]$Symbol, ".csv", sep="")
	tsList[[ExUL[i,]$Symbol]] = read.table(ffile, header=TRUE, sep=",")
}

#plot term structure surface
clear3d()
zrange = range(tsList)
mg.rownames = vector()
mg.text.col = "black"
for (j in 1:dim(ExUL)[1]) {
#	print(ExUL[j,]$Symbol)
	mg = tsList[[ExUL[j,]$Symbol]]
	mg.rownames = c(mg.rownames, paste(1:dim(mg)[1],rownames(mg),sep=": "))
	mg.text.col = c(mg.text.col, rep(as.character(ExUL[j,]$text.col),dim(mg)[1]))
	tspersp3d(mg, zrange, add=(j!=1), "", "", "", "", ExUL[j,]$surface.col, ExUL[j,]$points.col, ExUL[j,]$lines.col)	
}

#add title and legend
title3d(xlab="Date", ylab="Term Structure", zlab="Calendar Spread", main=paste(ExUL$Symbol,collapse=" vs "))
legend3d("right", c("Date",mg.rownames), text.col=mg.text.col, cex=0.75)


