#---------------------------------- futures symbol ----------------------------------
ExUL = data.frame(Exchange=character(), Underlying=character(), cList=character(), dsName=character())
##CME
#ExUL = rbind(ExUL, data.frame(Exchange="CME", Underlying="CL", cList=c("SETTLE.VOLUME"), dsName="quandl")) # 
#ExUL = rbind(ExUL, data.frame(Exchange="CME", Underlying="NG", cList=c("SETTLE.VOLUME"), dsName="quandl")) # 
##CBOE
ExUL = rbind(ExUL, data.frame(Exchange="CBOE", Underlying="VX", cList=c("SETTLE.VOLUME"), dsName="quandl")) # 
##CFFEX
#ExUL = rbind(ExUL, data.frame(Exchange="CFFEX", Underlying="TF", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # 5-year Treasury Bond Futures
#ExUL = rbind(ExUL, data.frame(Exchange="CFFEX", Underlying="T", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # 10-year Treasury Bond Futures
#ExUL = rbind(ExUL, data.frame(Exchange="CFFEX", Underlying="IF", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # CSI 300 Index Futures
#ExUL = rbind(ExUL, data.frame(Exchange="CFFEX", Underlying="IH", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # SSE 50 Index Futures
#ExUL = rbind(ExUL, data.frame(Exchange="CFFEX", Underlying="IC", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # CSI 500 Index Futures
##DCE
#ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="A", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # No.1 Soybeans
#ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="M", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Soybean Meal
#ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="Y", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Soybean Oil
#ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="I", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Iron Ore
#ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="J", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Coke
#ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="JM", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Coking Coal
#ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="P", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # RBD Palm Olein
#ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="C", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Corn
#ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="CS", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Corn Starch
#ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="L", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # LLDPE
#ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="V", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # PVC
#ExUL = rbind(ExUL, data.frame(Exchange="DCE", Underlying="PP", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # PP
##SHFE
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="CU", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Copper
ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="AL", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Aluminum
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="ZN", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Zinc
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="PB", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Lead
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="NI", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Nickel
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="SN", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Tin
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="AU", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Gold
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="AG", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Silver
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="RB", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Rebar
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="BU", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Bitumen
#ExUL = rbind(ExUL, data.frame(Exchange="SHFE", Underlying="RU", cList=c("CLOSE.SETTLE.VOLUME"), dsName="wind")) # Natural Rubber
##CZCE
#ExUL = rbind(ExUL, data.frame(Exchange="", Underlying="")) # 
#ExUL = rbind(ExUL, data.frame(Exchange="", Underlying="")) # 
#ExUL = rbind(ExUL, data.frame(Exchange="", Underlying="")) # 
#ExUL = rbind(ExUL, data.frame(Exchange="", Underlying="")) # 
#ExUL = rbind(ExUL, data.frame(Exchange="", Underlying="")) # 
#ExUL = rbind(ExUL, data.frame(Exchange="", Underlying="")) # 
#
ExUL = unique(ExUL)