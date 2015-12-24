rm(list=ls(all=TRUE)) 
options(width = 438L)

#---------------- contracts ----------------
#Exchange = "DCE"; Underlying = "I"; c.mg = c("01","05","09"); fmonth = c("01","05","09");
#Exchange = "DCE"; Underlying = "M"; c.mg = c("01","05","09"); fmonth = c("05");
Exchange = "SHFE"; Underlying = "CU"; c.mg = formatC(seq(1,12), width=2, flag='0'); fmonth_vec = formatC(seq(1,12), width=2, flag='0');
Exchange = "CME"; Underlying = "CL"; c.mg = formatC(seq(1,12), width=2, flag='0'); fmonth_vec = formatC(seq(1,12), width=2, flag='0');
#Benchmark = c("S0174655");
#Underlying ="RB"; Exchange = "SHF"; Benchmark = NA;
#Underlying ="P"; Exchange = "DCE"; Benchmark = NA

for (fmonth in fmonth_vec) {
	source("//VMWARE-HOST/Shared Folders/Documents/workspace/r-basis-analysis/basis_analysis.R", echo=FALSE, encoding="GBK")
}
