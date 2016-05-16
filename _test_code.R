################################################################################################################
#regression analysis and model selection
################################################################################################################
#---------------------------------- prepare regression dataset ----------------------------------
dList = dataset0.list	#all dataset
dList = dataset.list	#may exclude delivery month, e.g., nmbd=1, nmfa=12

#regression data list, exclude null element in the dataset0.list or dataset0.list
#some YearMonth may not have all the ExUL contract, e.g., DCE.I is listed in 2013, which is later than DCE.J or SHFE.RB
mk = lapply(dList, function(x){if(dim(x)[2]==(dim(ExUL)[1]+1)){x}})
reg.data.list = mk[which(!sapply(mk,is.null))]
print(paste("list length", "dList:", length(dList), "reg.data.lis:", length(reg.data.list)))

#prepare regression dataset in data.frame format
reg.data = vector()
for (i in 1:length(reg.data.list)) {
#	all data entry must not contain NA values, e.g., names(xm)="DATETIME","SHFE.CU.2016.08.CLOSE","SHFE.AL.2016.08.CLOSE","SHFE.ZN.2016.08.CLOSE"
	xm = reg.data.list[[i]]
	ind = apply(!is.na(xm),1,all)
	if (any(ind)) {
		mm = cbind(xm[ind,], YearMonth=names(reg.data.list[i]))
		mm = cbind(mm, do.call(rbind, strsplit(as.character(mm$YearMonth),"[.]")))	
		colnames(mm) = c("DATETIME", ExUL[,"Symbol"], "YearMonth", "Year", "Month")
		reg.data = rbind(reg.data, mm)
	}
}

#---------------------------------- regression models ----------------------------------
model = data.frame()
model = rbind(model, data.frame(name="STEELMILL", reg.expr="SHFE.RB~DCE.I+DCE.J", trd.expr="SHFE.RB-2*DCE.I-1*DCE.J")) # 
model = rbind(model, data.frame(name="RBHC", reg.expr="SHFE.RB~SHFE.HC", trd.expr="SHFE.RB-SHFE.HC")) # 
model = rbind(model, data.frame(name="BASEMETAL", reg.expr="SHFE.AL ~ SHFE.CU + SHFE.ZN", trd.expr="")) # 
rownames(model) = model[,"name"]

#---------------------------------- regression analysis ----------------------------------
model.name = "STEELMILL"
#model.name = "BASEMETAL"
#model.name = "RBHC"

model.reg.expr = as.character(model[model.name,"reg.expr"])
model.trd.expr = as.character(model[model.name,"trd.expr"])

#regression model list
reg.model.list = list()
#group by delivery Month, YearMonth
par(mfrow=c(2,2))
for (jCol in c("Month","YearMonth")) {
#	store regression models in list format, e.g., reg.model.list[[jCol]][[j]]
	reg.model.list[[jCol]] = list()
	for (j in unique(reg.data[,jCol])) {	
		gg = glm(eval(parse(text=model.reg.expr)), data=reg.data[reg.data[,jCol]==j,])
		reg.model.list[[jCol]][[j]] = gg
		plot(gg)
#		export to a .png file
		pathname = paste(getwd(),"/",model.name,"/",paste(model.name,jCol,j,"png",sep="."),sep="")
		dev.copy(png, width=672, height=672, filename=pathname)
		dev.off()		
		print(paste("---------------------------------", jCol, j, "---------------------------------"))
		print(summary(gg))
	}
}

#all futures data, i.e., excluding INDEX data
gg = glm(eval(parse(text=model.reg.expr)), data=reg.data[reg.data[,"YearMonth"]!="0000.00",])
reg.model.list[["ALL"]] = gg
plot(gg)
summary(gg)

################################################################################################################
#plot and display
################################################################################################################
par(mfrow=c(1,1))
#---------------------------------- boxplots ----------------------------------
#model.trd.expr is the equation used for trading, which maybe different from model.reg.expr
reg.data[,model.name] = with(eval(parse(text=model.trd.expr)), data=reg.data)
reg.data[,"YearMonth"] = as.character(reg.data[,"YearMonth"])
rMonth = levels(reg.data[,"Month"])
#last observation of each YearMonth
lobs = data.frame(DATETIME=character(0))
for (ym in unique(reg.data[,"YearMonth"])) {
	lobs = rbind(lobs, tail(subset(reg.data,YearMonth==ym),n=1))
}

#boxplot month, including / excluding INDEX, i.e., spot price
bpMonth = rMonth
bpMonth = rMonth[rMonth!="00"]

#boxplot with data from bpMonth
#bx = boxplot(RBHC~YearMonth, data=reg.data, subset=(Month%in%bpMonth), main=model.name)
bx = boxplot(STEELMILL~YearMonth, data=reg.data, subset=(Month%in%bpMonth), main=model.name)
#print the point of last observation, it may need to re-order the data, i.e., lobs2
lobs2 = lobs[match(bx$names,lobs[,"YearMonth"]), model.name]
points(1:length(lobs2), lobs2, col="blue", pch=19)
grid()

#---------------------------------- time series plot ----------------------------------
#re-organize data into time series format
reg.data.ts = data.frame(DATETIME=character(0))
for (j in unique(reg.data[,"Month"])) {
	zt = reg.data[reg.data[,"Month"]==j,c("DATETIME",model.name)]
	names(zt) = c("DATETIME",paste(model.name,j,sep="."))
	reg.data.ts = merge(reg.data.ts, zt, by="DATETIME", all=TRUE, sort=TRUE)	
}
rownames(reg.data.ts) = reg.data.ts[,"DATETIME"]
reg.data.ts = subset(reg.data.ts,select=-c(DATETIME))

#matplot
nn = dim(reg.data.ts)[2]
matplot(tail(reg.data.ts,n=500), type="l", col=1:nn)
legend('topright', legend=unique(reg.data[,"Month"]), text.col=1:nn, col=1:nn, cex=0.65)
grid()




















summary(reg.model.list[["YearMonth"]][["0000.00"]])

plot(residuals(reg.model.list[["Month"]][["00"]]))
grid()
plot(residuals(reg.model.list[["Month"]][["01"]]))
grid()
plot(residuals(reg.model.list[["Month"]][["05"]]))
grid()
plot(residuals(reg.model.list[["Month"]][["09"]]))
grid()

summary(reg.model.list[["Month"]][["00"]])
summary(reg.model.list[["Month"]][["01"]])
summary(reg.model.list[["Month"]][["05"]])
summary(reg.model.list[["Month"]][["09"]])


plot(fitted(reg.model.list[["YearMonth"]][["0000.00"]]), type="l")

plot(dataset.list[["0000.00"]][["SHFE.RB"]], type="l")
lines(fitted(reg.model.list[["YearMonth"]][["0000.00"]]), type="l")


lines(dataset.list[["0000.00"]][["SHFE.RB"]])

#1 - pchisq(residual deviance, df)
#how to determine one model is better than the other?
#use physical price to predict futures
#regress physical price on physical price, theoretical model for the steel industry


#plot(reg.data[reg.data[,"Month"]=="00",model.name],ylim=range(reg.data[,model.name]),col="black")
#points(reg.data[reg.data[,"Month"]=="05",model.name],col="red")
#points(reg.data[reg.data[,"Month"]=="09",model.name],col="blue")
#points(reg.data[reg.data[,"Month"]=="01",model.name],col="green")
#grid()

#
#residuals()
#fitted(gg)
#predict(gg)
#coef(gg)
#deviance(gg)
#formula(gg)
#
#
#
#y <- c(1:3, 7, 5)
#x <- c(1:3, 6:7)
#( ee <- effects(lm(y ~ x)) )
#
#
#lapply(reg.model.list[["Month"]],summary)
#
#lapply(reg.model.list[["Month"]],function(x){plot(residuals(x))})
#
#
#
#plot(reg.model.list[[1]][[1]])
#
#
#plot(residuals(gg))
#grid()
#
#plot(gg)
#summary(gg)
