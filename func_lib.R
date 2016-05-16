#====================================================================================================
#basis_analysis_batch.R
#basis_analysis.R
#====================================================================================================
#transform dataset, from time series data to seasonal data
TransData = function(sp) {
#	re-organize sp data	
	mlen = max(apply(sp, 2, function(x){sum(!is.na(x))}))
	sptmp = vector()
	for (i in 1:dim(sp)[2]) {
		mtmp = sp[!is.na(sp[,i]),i]
		length(mtmp) = mlen
		sptmp = cbind(sptmp, mtmp)
	}
	sp2 = sptmp
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
	dev.off()
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
	dev.off()
}

#seasonal box plot
ssboxplot = function(sp2, imonth, filename, width=1440, height=785, names="", xlab="", ylab="", main="") {
#	use a different color for a particular month, e.g., Jan
	colvec = rep("white", dim(sp2)[2])
	colvec[imonth] = "red"
	boxplot(sp2, col=colvec, names=names, xlab=xlab, ylab=ylab, main=main)
#	indicate the value of last observatioin
	lobs = tail(na.locf(sp2), n=1)
	points(seq(1,dim(sp2)[2]), lobs, col="blue", pch=19)
	grid()
	dev.copy(png, width=width, height=height, filename=filename)
	dev.off()
}

#====================================================================================================
#term_structure.R
#term_structure_2.R
#====================================================================================================
#transform term structure data, i.e., create averaged and sampled term structure data
TransTS = function(nTermStructure, nlines, sfreq, dpline) { 
##	register op as foo()
#	foo = match.fun(FUN=op) 	
##	all term structure data
#	nTermStructure = foo(TermStructure, TermStructure[,anchMonth])	
	
#	data before averaging and sampling process
	ldata = tail(nTermStructure, n=nlines*sfreq*dpline)
	rLab = ldata[,"rLab"]
	
#	aggregated average of term structure data
	mt = aggregate(subset(ldata,select=-rLab), list(rep(1:(nlines*sfreq),each=dpline)), mean)[,-1]
	
#	set rownames and rLab
	mt_ind = seq(1, nlines*sfreq) * dpline
	rownames(mt) = rownames(ldata[mt_ind,])	
	mt[,"rLab"] = rLab[mt_ind]
	
#	sampled term structure data
	ind = seq(1, nlines) * sfreq
	mt[ind,]
}

#-------------------- persp3d plot for term structure -------------------- 
tspersp3d = function(xt, zlim, add=FALSE, xlab="", ylab="", zlab="", mlab="", surface.col="white", points.col="blue", lines.col="red") {
#	surface 3D plot
	x = 1:dim(xt)[1]
	y = 1:dim(xt)[2]
	z = matrix(unlist(xt), nrow=dim(xt)[1])
	persp3d(x, y, z, zlim=zlim, add=add, col=surface.col, axes=FALSE, xlab=xlab, ylab=ylab, zlab=zlab, main=mlab)		
#	draw terms
	for (i in 1:dim(xt)[1]) {
		points3d(x=i, y, z=z[i,], col=points.col)	
		lines3d(x=i, y, z=z[i,], col=lines.col)
	}
	for (j in 1:dim(xt)[2]) {
		lines3d(x, y=j, z=z[,j], col=lines.col)
	}
#	axes
	box3d()
	axis3d("x+")
	axis3d(edge=c("y+-"), labels=colnames(xt))
	axis3d("z+")
	grid3d(c("x","y","z","x+","y+","z+"))	
}

