library(rgl)

fpath = paste(getwd(), "/FIXED_INCOME/", sep="")
fname = paste("SHIBOR", "csv", sep=".")
TermStructure = read.table(file=paste(fpath,fname,sep=""), header=TRUE, sep=",")
rownames(TermStructure) = TermStructure[,1]
TermStructure = TermStructure[,-1]

#anchor month, i.e., the month that other month reference to
#number of term strucure lines, displayed on the plot
#sampling frequency, take the observation of term structure line
#data entry per line, e.g., use the average
anchMonth = 2
nlines = 60
sfreq = 1
dpline = 10
xt = TransTS(TermStructure, "-", anchMonth, nlines, sfreq, dpline, rep("",dim(TermStructure)[1]))


mlab = paste(Symbol, "Average Window:", dpline)
matplot(t(xt), type="o", col=nlines:1, lty=nlines:1, pch=nlines:1, xlab="Term Strucutre (M)", ylab="Calendar Spread", main=mlab)
legend('bottomleft', legend=rownames(xt), text.col=nlines:1, col=nlines:1, lty=nlines:1, pch=nlines:1, cex=0.65, merge=FALSE)
grid()