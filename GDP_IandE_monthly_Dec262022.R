# Temporal disaggregation of GDP expenditure-based from Q to M
# Value, volume and price
# November 29, 2022; December 6, 2022; December 26, 2022

todayDate <- "Dec262022"
ResultsFolder <- "Results_Nov_2022"
theMonthC <- "November"
theMonthF <- "11"
theMonthI <- "09" # last month of the most recent quarter
theQtr <- "07"
theYear <- "2022"
tabName <- paste0(theMonthC,theYear,todayDate)
LastMonth <- theMonthC
firstqtr <- as.Date("2012-01-01")
firstqtrC <- "2012-01-01"
lastqtr <- as.Date(paste0(theYear,"-",theQtr,"-01"))
firstmon <- as.Date("2012-01-01")
firstmonC <- "2012-01-01"
lastmon <- as.Date(paste0(theYear,"-",theMonthI,"-01"))
lastmonF <- as.Date(paste0(theYear,"-",theMonthF,"-01"))

setwd(paste0("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/",
  ResultsFolder))

pkgs <- c("cansim","tidyverse","lubridate","anytime","ggthemes","ggpubr",
          "gridExtra","grid","gtable","reshape2","ggrepel","pracma",
          "seasonal","FactoMineR","RcppBDT","forecast","profvis",
          "RColorBrewer","WriteXLS","gdata","scales","ggpubr","ggtext",
          "tibble","simplecolors","stringr","tempdisagg","gt","canbank")
suppressPackageStartupMessages({
  inst <- lapply(pkgs,library,character.only=TRUE)
})
source(paste0("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/",
  "GDP_tempdisagg_functions.R"))

(Nobs <- mondf(firstmon,lastmonF)+1)

if (lastmonF<=lastmon) {
  print("lastmonF must be greater than lastmon")
  stop
}

# Create the EXTdata data frame with various input series
source(paste0("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/",
  "GDP_IandE_monthly_CreateData.R"))

# This big list contains all the meta data
GDP <- list(
  GDP=list(name="Gross domestic product at market prices",
    betterName="Gross domestic product at market prices",
    vnumber="v62305783",
    ChaVolnumber="v62305752",
    ConVolnumber="v62306896",
    FWPInumber="v62307334", # 2012=100, all data=NA before 2012
    AdjEntry="v62306897",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="FCE+I+V+X-M+SD",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  FCE=list(name="Final consumption expenditure",
    betterName="Final consumption expenditure",
    vnumber="v62305754",
    ChaVolnumber="v62305723",
    ConVolnumber="v62306858",
    FWPInumber="v62307310",
    AdjEntry="v62306869",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="HHFC+NPFC+GFC",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  HHFC=list(name="Household final consumption expenditure",
    betterName="Household final consumption expenditure",
    vnumber="v62305755",
    ChaVolnumber="v62305724",
    ConVolnumber="v62306859",
    FWPInumber="v62307311",
    AdjEntry="v62306866",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="HHFCG+HHFCS",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  HHFCG=list(name="Goods",
    betterName="Consumption expenditure on goods",
    vnumber="v62305756",
    ChaVolnumber="v62305725",
    ConVolnumber="v62306860",
    FWPInumber="v62307312",
    AdjEntry="v62306864",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="HHFCGD+HHFCGS+HHFCGN",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  HHFCGD=list(name="Durable goods",
    betterName="Consumption expenditure on durable goods",
    vnumber="v62305757",
    ChaVolnumber="v62305726",
    ConVolnumber="v62306861",
    FWPInumber="v62307313",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Equality",
    vtype="Equality",
    equation="NA",
    driver="Csystem",
    d_vnumber="EXTdata$HHFCGD",
    driver2="NA",
    d2_vnumber="NA"),
  HHFCGS=list(name="Semi-durable goods",
    betterName="Consumption expenditure on semi-durable goods",
    vnumber="v62305758",
    ChaVolnumber="v62305727",
    ConVolnumber="v62306862",
    FWPInumber="v62307314",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Equality",
    vtype="Equality",
    equation="NA",
    driver="Csystem",
    d_vnumber="EXTdata$HHFCGS",
    driver2="NA",
    d2_vnumber="NA"),
  HHFCGN=list(name="Non-durable goods",
    betterName="Consumption expenditure on non-durable goods",
    vnumber="v62305759",
    ChaVolnumber="v62305728",
    ConVolnumber="v62306863",
    FWPInumber="v62307315",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Equality",
    vtype="Equality",
    equation="NA",
    driver="Csystem",
    d_vnumber="EXTdata$HHFCGN",
    driver2="NA",
    d2_vnumber="NA"),
  HHFCS=list(name="Services",
    betterName="Consumption expenditure on services",
    vnumber="v62305760",
    ChaVolnumber="v62305729",
    ConVolnumber="v62306865",
    FWPInumber="v62307316",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Equality",
    vtype="Equality",
    equation="NA",
    driver="Csystem",
    d_vnumber="EXTdata$HHFCS",
    driver2="NA",
    d2_vnumber="NA"),
  NPFC=list(name="Non-profit institutions serving households' final consumption expenditure",
    betterName="NPISH final consumption expenditure",
    vnumber="v62305761",
    ChaVolnumber="v62305730",
    ConVolnumber="v62306867",
    FWPInumber="v62307317",
    AdjEntry="NA",
    deflator="ARIMA",
    ctype="ARIMA",
    vtype="ByDeflation",
    equation="NA",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  GFC=list(name="General governments final consumption expenditure",
    betterName="General governments final consumption expenditure",
    vnumber="v62305762",
    ChaVolnumber="v62305731",
    ConVolnumber="v62306868",
    FWPInumber="v62307318",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="GFCF+GFCO",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  GFCF=list(name="Federal government final consumption expenditure",
    betterName="Federal government final consumption expenditure",
    vnumber="v67276773",
    ChaVolnumber="v67276767",
    ConVolnumber="KEXTdataQ$GFCF",
    FWPInumber="NA",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Chow-Lin",
    vtype="Chow-Lin",
    equation="NA",
    driver="SEPH E*AWE for federal",
    d_vnumber="EXTdata$GFCF",
    Kdriver="SEPH E for federal",
    Kd_vnumber="KEXTdata$GFCF",
    driver2="NA",
    d2_vnumber="NA"),
  GFCO=list(name="Non-federal governments final consumption expenditure",
    betterName="Non-federal governments final consumption expenditure",
    vnumber="EXTdataQ$GFCO",
    ChaVolnumber="NA",
    ConVolnumber="KEXTdataQ$GFCO",
    FWPInumber="NA",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Chow-Lin",
    vtype="Chow-Lin",
    equation="NA",
    driver="SEPH E*AWE for non-federal",
    d_vnumber="EXTdata$GFCO",
    Kdriver="SEPH E for non-federal",
    Kd_vnumber="KEXTdata$GFCO",
    driver2="NA",
    d2_vnumber="NA"),
  I=list(name="Gross fixed capital formation",
    betterName="Gross fixed capital formation",
    vnumber="v62305763",
    ChaVolnumber="v62305732",
    ConVolnumber="v62306870",
    FWPInumber="v62307319",
    AdjEntry="v62306881",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="IB+INP+IG",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  IB=list(name="Business gross fixed capital formation",
    betterName="Business investment",
    vnumber="v62305764",
    ChaVolnumber="v62305733",
    ConVolnumber="v62306871",
    FWPInumber="v62307320",
    AdjEntry="v62306878",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="IBR+IBNRME+IBIP",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  IBR=list(name="Residential structures",
    betterName="Business investment in residential structures",
    vnumber="v62305765",
    ChaVolnumber="v62305734",
    ConVolnumber="v62306872",
    FWPInumber="v62307321",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="IBRC+IBRT",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  IBRC=list(name="Business construction and renovation of residential structures",
    betterName="New construction and renovation",
    vnumber="EXTdataQ$IBRC",
    ChaVolnumber="EXTdataQ$KIBRC",
    ConVolnumber="KEXTdataQ$IBRC",
    FWPInumber="NA",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Chow-Lin",
    vtype="Chow-Lin",
    equation="NA",
    driver="Investment in building construction (34-10-0175-01)",
    d_vnumber="EXTdata$IBRC",
    Kdriver="Investment in building construction in constant prices (34-10-0175-01)",
    Kd_vnumber="KEXTdata$IBRC",
    driver2="NA",
    d2_vnumber="NA"),
  IBRT=list(name="Residential ownership transfer costs",
    betterName="Ownership transfer costs",
    vnumber="v62144025",
    ChaVolnumber="v62143964",
    ConVolnumber="KEXTdataQ$IBRT",
    FWPInumber="NA",
    AdjEntry="NA",
    deflator="ARIMA",
    ctype="Chow-Lin",
    vtype="ByDeflation",
    equation="NA",
    driver="CREA resale value total",
    d_vnumber="EXTdata$IBRT",
    driver2="NHPI",
    d2_vnumber="v111955442"),
  IBNRME=list(name="Non-residential structures, machinery and equipment",
    betterName="Business investment in plant and equipment",
    vnumber="v62305766",
    ChaVolnumber="v62305735",
    ConVolnumber="v62306873",
    FWPInumber="v62307322",
    AdjEntry="v62306876",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="IBNR+IBME",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  IBNR=list(name="Non-residential structures",
    betterName="Non-residential structures",
    vnumber="v62305767",
    ChaVolnumber="v62305736",
    ConVolnumber="v62306874",
    FWPInumber="v62307323",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Chow-Lin",
    vtype="Chow-Lin",
    equation="NA",
    driver="Investment in building construction (34-10-0175-01)",
    d_vnumber="EXTdata$IBNR",
    Kdriver="Investment in building construction at constant prices (34-10-0175-01)",
    Kd_vnumber="KEXTdata$IBNR",
    driver2="NA",
    d2_vnumber="NA"),
  IBME=list(name="Machinery and equipment",
    betterName="Machinery and equipment",
    vnumber="v62305768",
    ChaVolnumber="v62305737",
    ConVolnumber="v62306875",
    FWPInumber="v62307324",
    AdjEntry="NA",
    deflator="Implicit", # v1001835521 Paasche price index, imports of industrial mach and equip, SA
    ctype="Chow-Lin-2",
    vtype="Chow-Lin-2",
    equation="NA",
    driver="Manufacturing sales",
    d_vnumber="EXTdata$IBME1", # Mach, comput and elec pdts, elec equip, transp equip
    driver2="Canadian international merchandise trade M&E imports", 
    d2_vnumber="EXTdata$IBME2", # Industrial machinery, equipment and parts imports
    Kdriver="Manufacturing sales",
    Kd_vnumber="KEXTdata$IBME1", # Mach, comput and elec pdts, elec equip, transp equip K$
    Kdriver2="Canadian international merchandise trade M&E imports", 
    Kd2_vnumber="KEXTdata$IBME2"), # Industrial machinery, equipment and parts imports K$
  IBIP=list(name="Intellectual property products",
    betterName="Business investment in intellectual property products",
    vnumber="v62305769",
    ChaVolnumber="v62305738",
    ConVolnumber="v62306877",
    FWPInumber="v62307325",
    AdjEntry="NA",
    deflator="ARIMA",
    ctype="ARIMA",
    vtype="ByDeflation",
    equation="NA",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  INP=list(name="Non-profit institutions serving households' gross fixed capital formation",
    betterName="NPISH investment",
    vnumber="v62305770",
    ChaVolnumber="v62305739",
    ConVolnumber="v62306879",
    FWPInumber="v62307326",
    AdjEntry="NA",
    deflator="ARIMA",
    ctype="ARIMA",
    vtype="ByDeflation",
    equation="",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  IG=list(name="General governments gross fixed capital formation",
    betterName="Government investment",
    vnumber="v62305771",
    ChaVolnumber="v62305740",
    ConVolnumber="v62306880",
    FWPInumber="v62307327",
    AdjEntry="NA",
    deflator="ARIMA",
    ctype="ARIMA",
    vtype="ByDeflation",
    equation="NA",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  V=list(name="Investment in inventories",
    betterName="Investment in inventories",
    vnumber="v62305772",
    ChaVolnumber="v62305741",
    ConVolnumber="v62306882",
    FWPInumber="NA",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="VB+VNB",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  VB=list(name="Of which: business investment in inventories",
    betterName="Business investment in inventories",
    vnumber="v62305773",
    ChaVolnumber="v62305742",
    ConVolnumber="v62306883",
    FWPInumber="NA",
    AdjEntry="v62306886",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="VBNF+VBF",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  VBNF=list(name="Non-farm",
    betterName="Business investment in non-farm inventories",
    vnumber="v62305774",
    ChaVolnumber="v62305743",
    ConVolnumber="v62306884",
    FWPInumber="NA",
    AdjEntry="NA",
    deflator="v1230996007", # IPPI
    ctype="Chow-Lin-2",
    vtype="Chow-Lin-2",
    equation="",
    driver="Manufacturing inventory change",
    d_vnumber="EXTdata$VBNF1",
    driver2="Wholesale inventory change",
    d2_vnumber="EXTdata$VBNF2",
    Kdriver="Manufacturing inventory change",
    Kd_vnumber="KEXTdata$VBNF1",
    Kdriver2="Wholesale inventory change",
    Kd2_vnumber="KEXTdata$VBNF2"),
  VBF=list(name="Farm",
    betterName="Business investment in farm inventories",
    vnumber="v62305775",
    ChaVolnumber="v62305744",
    ConVolnumber="v62306885",
    FWPInumber="NA",
    AdjEntry="NA",
    deflator="Implicit", # FPPI v66449852
    ctype="ARIMA",
    vtype="ARIMA",
    equation="NA",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  VNB=list(name="Non-business investment in inventories",
    betterName="Non-business investment in inventories",
    vnumber="EXTdataQ$VNB",
    ChaVolnumber="ExtdataQ$KVNB",
    ConVolnumber="KEXTdataQ$VNB",
    FWPInumber="NA",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="ARIMA",
    vtype="ARIMA",
    equation="NA",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  X=list(name="Exports of goods and services",
    betterName="Exports of goods and services",
    vnumber="v62305776",
    ChaVolnumber="v62305745",
    ConVolnumber="v62306887",
    FWPInumber="v62307328",
    AdjEntry="v62306890",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="XG+XS",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  XG=list(name="Exports of goods",
    betterName="Exports of goods",
    vnumber="v62305777",
    ChaVolnumber="v62305746",
    ConVolnumber="v62306888",
    FWPInumber="v62307329",
    AdjEntry="NA",
    deflator="Implicit", # v1001837884Paasche price index table 12-10-0128-01
    ctype="Chow-Lin",
    vtype="Chow-Lin",
    equation="NA",
    driver="Canadian international merchandise trade",
    d_vnumber="EXTdata$XG",
    Kdriver="Canadian international merchandise trade at constant prices (12-10-0124-01)",
    Kd_vnumber="KEXTdata$XG",
    driver2="NA",
    d2_vnumber="NA"),
  XS=list(name="Exports of services",
    betterName="Exports of services",
    vnumber="v62305778",
    ChaVolnumber="v62305747",
    ConVolnumber="v62306889",
    FWPInumber="v62307330",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Chow-Lin",
    vtype="ARIMA",
    equation="NA",
    driver="Canadian international trade in services",
    d_vnumber="EXTdata$XS",
    driver2="NA",
    d2_vnumber="NA"),
  M=list(name="Less: imports of goods and services",
    betterName="Imports of goods and services",
    vnumber="v62305779",
    ChaVolnumber="v62305748",
    ConVolnumber="v62306891",
    FWPInumber="v62307331",
    AdjEntry="v62306894",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="MG+MS",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  MG=list(name="Imports of goods",
    betterName="Imports of goods",
    vnumber="v62305780",
    ChaVolnumber="v62305749",
    ConVolnumber="v62306892",
    FWPInumber="v62307332",
    AdjEntry="NA",
    deflator="Implicit", # v1001835436 Paasche price index table 12-10-0128-01
    ctype="Chow-Lin",
    vtype="Chow-Lin",
    equation="NA",
    driver="Canadian international merchandise trade",
    d_vnumber="EXTdata$MG",
    Kdriver="Canadian international merchandise trade at constant prices (12-10-0124-01)",
    Kd_vnumber="KEXTdata$MG",
    driver2="NA",
    d2_vnumber="NA"),
  MS=list(name="Imports of services",
    betterName="Imports of services",
    vnumber="v62305781",
    ChaVolnumber="v62305750",
    ConVolnumber="v62306893",
    FWPInumber="v62307333",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="Chow-Lin",
    vtype="ARIMA",
    equation="NA",
    driver="Canadian international trade in services",
    d_vnumber="EXTdata$MS",
    driver2="NA",
    d2_vnumber="NA"),
  SD=list(name="Statistical discrepancy",
    betterName="Statistical discrepancy",
    vnumber="v62305782",
    ChaVolnumber="v62305751",
    ConVolnumber="v62306895",
    FWPInumber="NA",
    AdjEntry="NA",
    deflator="Implicit",
    ctype="ARIMA",
    vtype="ARIMA",
    equation="NA",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  FDD=list(name="Final domestic demand",
    betterName="Final domestic demand",
    vnumber="v62305784",
    ChaVolnumber="v62305753",
    ConVolnumber="v62306898",
    FWPInumber="v62307335",
    AdjEntry="v62306899",
    deflator="Implicit",
    ctype="Identity",
    vtype="Identity",
    equation="FCE+I",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA")
)
(N <- length(GDP))
(GDPnames <- names(GDP))
#-------------------------------------------------------------------------------
# Sort and display the cases, C for current prices and V for volume
CasesC_Identity <- character()
CasesC_Equality <- character()
CasesC_ARIMA <- character()
CasesC_ByDeflation <- character()
CasesC_ChowLin <- character()
CasesC_ChowLin2 <- character()
CasesV_Identity <- character()
CasesV_Equality <- character()
CasesV_ARIMA <- character()
CasesV_ByDeflation <- character()
CasesV_ChowLin <- character()
CasesV_ChowLin2 <- character()
for (i in 1:N) {
  if (GDP[[i]]$ctype=="Identity")    CasesC_Identity <- c(CasesC_Identity,names(GDP)[i]) 
  if (GDP[[i]]$ctype=="Equality")    CasesC_Equality <- c(CasesC_Equality,names(GDP)[i]) 
  if (GDP[[i]]$ctype=="ARIMA")       CasesC_ARIMA <- c(CasesC_ARIMA,names(GDP)[i]) 
  if (GDP[[i]]$ctype=="ByDeflation") CasesC_ByDeflation <- c(CasesC_ByDeflation,names(GDP)[i]) 
  if (GDP[[i]]$ctype=="Chow-Lin")    CasesC_ChowLin <- c(CasesC_ChowLin,names(GDP)[i]) 
  if (GDP[[i]]$ctype=="Chow-Lin-2")  CasesC_ChowLin2 <- c(CasesC_ChowLin2,names(GDP)[i]) 
  if (GDP[[i]]$vtype=="Identity")    CasesV_Identity <- c(CasesV_Identity,names(GDP)[i]) 
  if (GDP[[i]]$vtype=="Equality")    CasesV_Equality <- c(CasesV_Equality,names(GDP)[i]) 
  if (GDP[[i]]$vtype=="ARIMA")       CasesV_ARIMA <- c(CasesV_ARIMA,names(GDP)[i]) 
  if (GDP[[i]]$vtype=="ByDeflation") CasesV_ByDeflation <- c(CasesV_ByDeflation,names(GDP)[i]) 
  if (GDP[[i]]$vtype=="Chow-Lin")    CasesV_ChowLin <- c(CasesV_ChowLin,names(GDP)[i]) 
  if (GDP[[i]]$vtype=="Chow-Lin-2")  CasesV_ChowLin2 <- c(CasesV_ChowLin2,names(GDP)[i])
}
cat(paste0(
  "\nThere are ",N," time series being temporally disaggregated from quarters to months.",
  "\nHere are the cases for GDP at current prices:",
  "\n",length(CasesC_ChowLin)," are interpolated and extrapolated using one related indicator via the Chow-Lin method.",
  "\n",length(CasesC_ChowLin2)," are interpolated and extrapolated using two related indicators. ",
  "\n",length(CasesC_ByDeflation)," are calculated using the deflation method.",
  "\n",length(CasesC_Equality)," are known and require no interpolation or extrapolation.",
  "\n",length(CasesC_ARIMA)," are interpolated with the Denton-Dagum-Cholette method and extrapolated with ARIMA models.",
  "\n",length(CasesC_Identity)," are interpolated and extrapolated by applying identity equations."),sep="\n")
CasesC_ChowLin
CasesC_ChowLin2
CasesC_ByDeflation
CasesC_Equality
CasesC_ARIMA
CasesC_Identity 
cat(paste0(
  "\nHere are the cases for GDP at constant 2012 prices:",
  "\n",length(CasesV_ChowLin)," are interpolated and extrapolated using one related indicator via the Chow-Lin method.",
  "\n",length(CasesV_ChowLin2)," are interpolated and extrapolated using two related indicators. ",
  "\n",length(CasesV_ByDeflation)," are calculated using the deflation method.",
  "\n",length(CasesV_Equality)," are known and require no interpolation or extrapolation.",
  "\n",length(CasesV_ARIMA)," are interpolated with the Denton-Dagum-Cholette method and extrapolated with ARIMA models.",
  "\n",length(CasesV_Identity)," are interpolated and extrapolated by applying identity equations."),sep="\n")
CasesV_ChowLin
CasesV_ChowLin2
CasesV_ByDeflation
CasesV_Equality
CasesV_ARIMA
CasesV_Identity 

#-------------------------------------------------------------------------------
# Retrieve all the quarterly SAAR data in a df called Qdata
# covering the time period firstqtr to lastqtr
# Qdata is quarterly, current prices, SAAR
# Qdata names are in the same order as the GDP list
Qdata <- data.frame(Date=seq.Date(firstqtr,lastqtr,by="quarter"))
for (i in 2:(N+1)) {
  if (substr(GDP[[i-1]]$vnumber,1,1)=="v") {
    vec <- GDP[[i-1]]$vnumber # vnumbers for the quarterly data
    tmp <- get_cansim_vector(vec,firstqtrC)
    tmp <- filter(tmp,Date<=lastqtr)
    Qdata[,i] <- tmp$VALUE
    names(Qdata)[i] <- names(GDP)[i-1]
  } else {
    var <- substr(GDP[[i-1]]$vnumber,10,nchar(GDP[[i-1]]$vnumber))
    for (j in 1:ncol(EXTdataQ)) {
      if (names(EXTdataQ)[j]==var) colno <- j
    }
    Qdata[,i] <- EXTdataQ[,colno]
    names(Qdata)[i] <- var
  }
}
#-------------------------------------------------------------------------------
# Use Denton-Cholette to temporally disaggregate
# all of the ARIMA cases; then use ARIMA to forecast them
MODELS_ARIMA <- list()
ARIMAdata <- data.frame(Date=seq.Date(firstmon,lastmon,by="month"))
for (i in 2:(length(CasesC_ARIMA)+1)) {
  nam <- CasesC_ARIMA[i-1]
  ARIMAdata[i] <- MakeMonthlyNoDriver(Qdata[,which(colnames(Qdata)==nam)],nam,0,F)
  names(ARIMAdata)[i] <- nam
}
if (lastmonF>lastmon) {
  Fdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
  for (i in 2:(length(CasesC_ARIMA)+1)) {
    if (month(lastmonF) %in% c(1,4,7,10)) numf <- 1
    if (month(lastmonF) %in% c(2,5,8,11)) numf <- 2
    if (month(lastmonF) %in% c(3,6,9,12)) numf <- 3
    nam <- CasesC_ARIMA[i-1]
    model <- auto.arima(ARIMAdata[i])
    fc <- forecast(model,numf)
    MODELS_ARIMA[[i-1]] <- summary(model)
    names(MODELS_ARIMA)[i-1] <- nam
    if (numf==1) {
      Fdata[,i] <- c(round(ARIMAdata[,i],0),round(fc$mean[1],0))
    } else if (numf==2) {
      Fdata[,i] <- c(round(ARIMAdata[,i],0),round(fc$mean[1],0),round(fc$mean[2],0))
    } else {
      Fdata[,i] <- c(round(ARIMAdata[,i],0),round(fc$mean[1],0),round(fc$mean[2],0),
        round(fc$mean[3],0))
    }
    names(Fdata)[i] <- nam
  }
} else { Fdata <- ARIMAdata }
#-------------------------------------------------------------------------------
# Collect the data for the Equality cases in EQdata
EQdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 1:length(CasesC_Equality)) {
    var <- substr(GDP[[CasesC_Equality[i]]]$d_vnumber,9,nchar(GDP[[CasesC_Equality[i]]]$d_vnumber))
    for (j in 1:ncol(EXTdata)) {
      if (names(EXTdata)[j]==var) colno <- j
    }
    EQdata[,i+1] <- EXTdata[,colno]
    names(EQdata)[i+1] <- var
}
#-------------------------------------------------------------------------------
# Collect the Chow-Lin single driver vnumbers; then check availability dates;
# finally forecast the drivers to lastmonF with ARIMA if necessary
CLdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 1:length(CasesC_ChowLin)) {
    nam <- CasesC_ChowLin[i]
    vec <- GDP[[nam]]$d_vnumber 
    driv <- GDP[[nam]]$driver 
    if (substr(vec,1,1)=="v") {
      tmp1 <- get_cansim_vector(vec,firstmonC)
      tmpval <- tmp1$VALUE
      date1 <- tmp1$Date[1]
      date2 <- tmp1$Date[nrow(tmp1)]
    } else {
      var <- substr(vec,9,nchar(GDP[[nam]]$d_vnumber))
      for (j in 1:ncol(EXTdata)) {
        if (names(EXTdata)[j]==var) colno <- j
      }
      tmpval <- EXTdata[,colno]
      tmp1 <- data.frame(Date=seq.Date(firstmon,EXTdata$Date[nrow(EXTdata)],"month"))
      tmp1$VALUE <- tmpval
      date1 <- tmp1$Date[1]
      date2 <- tmp1$Date[nrow(tmp1)]
    }
    if (date1>firstmon) { # backcast the series to firstmon via ARIMA
      tmpval <- Backcast(tmp1$VALUE,mondf(firstmon,date1),firstmon)
    }
    if (date2>lastmonF) {
      tmp1 <- filter(tmp1,Date<=lastmonF)
      tmpval <- tmp1$VALUE
    }
    if (date2<lastmonF) {
      print(paste0("The last available date for ",nam,"-",driv," is ",date2,
        " so the indicator series will be extended with ARIMA to ",lastmonF))
      tmpval <- ArimaForecast(tmp1$VALUE,mondf(date2,lastmonF))
    }
    CLdata[,i+1] <- tmpval
    names(CLdata)[i+1] <- nam
}
#-------------------------------------------------------------------------------
# Use the Chow-Lin procedure in each of the cases where it applies,  
# to interpolate and extrapolate the cases with only one driver
MODELS_ChowLin <- list()
Cdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 2:(length(CasesC_ChowLin)+1)) {
  nam <- CasesC_ChowLin[i-1]
  NewList <- MakeMonthly1Driver(MODELS_ChowLin,nam,0,F)
  Cdata[i] <- NewList[[2]]
  MODELS_ChowLin <- NewList[[1]]
  names(Cdata)[i] <- nam
}
#-------------------------------------------------------------------------------
# Find all the Chow-Lin cases with two drivers; collect the Chow-Lin two 
# driver d2_vnumbers; check availability dates; finally forecast 
# to lastmonF with ARIMA if necessary
CLEcases <- list()
CLE1data <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month")) # extra drivers
CLE2data <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month")) # extra drivers
for (i in 2:(length(CasesC_ChowLin2)+1)) {
  # first get indicator 1
  nam <- CasesC_ChowLin2[i-1] # name of the variable in GDP
  vec <- GDP[[nam]]$d_vnumber # either a v-number of an EXTdata column
  driv <- GDP[[nam]]$driver # Description of this driver
  drivName <- substr(GDP[[CasesC_ChowLin2[i-1]]]$d_vnumber,1,
    nchar(GDP[[CasesC_ChowLin2[i-1]]]$d_vnumber))
  if (substr(drivName,1,1)=="v") {
    tmp1 <- get_cansim_vector(vec,firstmonC)
    tmpval <- tmp1$VALUE
    tmp2 <- tmp1
    date1 <- tmp1$Date[1]
    date2 <- tmp1$Date[nrow(tmp1)]
  } else {
    var <- substr(GDP[[CasesC_ChowLin2[i-1]]]$d_vnumber,9,
      nchar(GDP[[CasesC_ChowLin2[i-1]]]$d_vnumber))
    for (j in 1:ncol(EXTdata)) {
      if (names(EXTdata)[j]==var) colno <- j
    }
    date1 <- firstmon
    date2 <- lastmonF
    tmp1 <- data.frame(Date=seq.Date(firstmon,lastmonF,"month"))
    tmp1$VALUE <- EXTdata[,colno]
    tmpval <- EXTdata[,colno]
  }
  if (date1>firstmon) { # backcast the series to firstmon via ARIMA
    tmpval <- Backcast(tmp1$VALUE,mondf(firstmon,date1),firstmon) 
  }
  if (date2>lastmonF) {
    tmp1 <- filter(tmp1,Date<=lastmonF)
    tmpval <- tmp1$VALUE
    date2 <- tmp1$Date[nrow(tmp1)]
  }
  if (date2<lastmonF) {
    print(paste0("The last available date for the first indicator for ",
      nam," is ",date2," so the indicator series will be ",
      "extended with ARIMA to ",lastmonF))
    tmpval <- ArimaForecast(tmp1$VALUE,mondf(date2,lastmonF))
  }
  CLE1data[,i] <- tmpval
  colnames(CLE1data)[i] <- nam
  # now get indicator 2
  vec2 <- GDP[[nam]]$d2_vnumber # either a v-number or an EXTdata column
  driv2 <- GDP[[nam]]$driver2
  drivName <- substr(GDP[[CasesC_ChowLin2[i-1]]]$d2_vnumber,1,
    nchar(GDP[[CasesC_ChowLin2[i-1]]]$d2_vnumber))
  if (substr(drivName,1,1)=="v") {
    tmp1 <- get_cansim_vector(vec2,firstmonC)
    tmpval <- tmp1$VALUE
    tmp2 <- tmp1
    date1 <- tmp1$Date[1]
    date2 <- tmp1$Date[nrow(tmp1)]
  } else {
    var <- substr(GDP[[CasesC_ChowLin2[i-1]]]$d2_vnumber,9,
      nchar(GDP[[CasesC_ChowLin2[i-1]]]$d2_vnumber))
    for (j in 1:ncol(EXTdata)) {
      if (names(EXTdata)[j]==var) colno <- j
    }
    date1 <- firstmon
    date2 <- lastmonF
    tmp1 <- data.frame(Date=seq.Date(firstmon,lastmonF,"month"))
    tmp1$VALUE <- EXTdata[,colno]
    tmpval <- EXTdata[,colno]
  }
  if (date1>firstmon) { # backcast the series to firstmon via ARIMA
    tmpval <- Backcast(tmp1$VALUE,mondf(firstmon,date1),firstmon) 
  }
  if (date2>lastmonF) {
    tmp1 <- filter(tmp1,Date<=lastmonF)
    tmpval <- tmp1$VALUE
    date2 <- tmp1$Date[nrow(tmp1)]
  }
  if (date2<lastmonF) {
    print(paste0("The last available date for the first indicator for ",
      nam," is ",date2," so the indicator series will be ",
      "extended with ARIMA to ",lastmonF))
    tmpval <- ArimaForecast(tmp1$VALUE,mondf(date2,lastmonF))
  }
  CLE2data[,i] <- tmpval
  colnames(CLE2data)[i] <- nam
}
#-------------------------------------------------------------------------------
# Use the Chow-Lin procedure in each of the cases with two indicators. 
# The indicators are in CLEdata (which all extend to lastmonF)
# CEdata is monthly, current prices, SAAR
CEdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 1:length(CasesC_ChowLin2)) {
  nam <- CasesC_ChowLin2[i]
  NewList <- MakeMonthly2Driver(MODELS_ChowLin,nam,0,F)
  CEdata[i+1] <- NewList[[2]]
  MODELS_ChowLin <- NewList[[1]]
  colnames(CEdata)[i+1] <- nam
}
# combine 1-indicator with 2-indicators case
Cdata <- cbind(Cdata,select(CEdata,-Date)) 
#-------------------------------------------------------------------------------
# Summarize regression results
NR <- length(MODELS_ChowLin)
REGRES <- data.frame(Name=character(NR),RSQ=numeric(NR),
  ARSQ=numeric(NR),SIGMA=numeric(NR),RHO=numeric(NR),
  RSS=numeric(NR),MEAN=numeric(NR),
  SEE=numeric(NR),NUMVAR=numeric(NR),DRIVER1=character(NR),
  DRIVER2=character(NR))
for (i in 1:length(MODELS_ChowLin)) {
  df <- as.data.frame(MODELS_ChowLin[[i]]$actual)
  MEAN <- mean(df$VALUE)
  REGRES$Name[i] <- names(MODELS_ChowLin)[i]
  REGRES$RSQ[i] <- round(MODELS_ChowLin[[i]]$r.squared,3)
  REGRES$ARSQ[i] <- round(MODELS_ChowLin[[i]]$adj.r.squared,3)
  REGRES$SIGMA[i] <- round((MODELS_ChowLin[[i]]$s_2)^0.5,3)
  REGRES$RHO[i] <- round(MODELS_ChowLin[[i]]$rho,3)
  REGRES$RSS[i] <- round(MODELS_ChowLin[[i]]$rss,0)
  REGRES$MEAN[i] <- MEAN
  REGRES$SEE[i] <- 100*((MODELS_ChowLin[[i]]$s_2)^0.5)/MEAN
  REGRES$NUMVAR[i] <- nrow(MODELS_ChowLin[[i]]$coefficients)
  if (REGRES$NUMVAR[i]==2) {
    REGRES$DRIVER1[i] <- GDP[[REGRES$Name[i]]]$driver
    REGRES$DRIVER2[i] <- NA
  } else {
    REGRES$DRIVER1[i] <- GDP[[REGRES$Name[i]]]$driver
    REGRES$DRIVER2[i] <- GDP[[REGRES$Name[i]]]$driver2
  }
}
REGRESpub <- REGRES
for (i in 1:nrow(REGRES)) {
  REGRESpub$Name[i] <- GDP[[REGRES$Name[i]]]$betterName  
}
REGRESpub <- select(REGRESpub,-RSQ,-RSS,-NUMVAR)
REGRESpub$MEAN <- round(REGRESpub$MEAN,0)
REGRESpub$SIGMA <- round(REGRESpub$SIGMA,0)
REGRESpub$SEE <- round(REGRESpub$SEE,3)
saveRDS(REGRES,paste0("REGRES_",todayDate,".rds"))
saveRDS(REGRESpub,paste0("REGRESpub_",todayDate,".rds"))
#-------------------------------------------------------------------------------
# Find and check/test all the identities
IdentCases <- character()
Idents <- character()
j <- 0
for (i in 1:N) {
  if (GDP[[i]]$ctype=="Identity") {
    j <- j+1
    IdentCases[j] <- names(GDP)[i]
    Idents[j] <- GDP[[i]]$equation
  }
}
Idata <- data.frame(Date=seq.Date(firstqtr,lastqtr,by="quarter"))
IdataCHK <- data.frame(Date=seq.Date(firstqtr,lastqtr,by="quarter"))
# Apply the identities to calculate the identity results
for (i in 2:(length(IdentCases)+1)) {
  nam <- IdentCases[i-1]
  Idata[i] <- round(eval(parse(text=Idents[i-1]),Qdata),0)
  colnames(Idata)[i] <- nam
  IdataCHK[i] <- Idata[i]-Qdata[[nam]]
}
View(IdataCHK)
#-------------------------------------------------------------------------------
# Calculate the identity cases using the identity equations
# Reorder the identities recursively. 
# Do the big identity last because it depends on the others
# Edata will be the final monthly, current prices, SAAR results
Idents <- Idents[c(4,3,5,2,8,9,7,6,11,10,12,13,14,1)]
IdentCases <- IdentCases[c(4,3,5,2,8,9,7,6,11,10,12,13,14,1)]
Edata <- cbind(Cdata,select(EQdata,-Date),select(Fdata,-Date)) 
Idata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
# Apply the identities to calculate the identity results
for (i in 1:length(Idents)) {
  nam <- IdentCases[i]
  Idata[i+1] <- round(eval(parse(text=Idents[i]),Edata),0)
  colnames(Idata)[i+1] <- nam
  Edata <- cbind(Edata,Idata[i+1])
  colnames(Edata)[ncol(Edata)] <- nam
}
Edata <- select(Edata,all_of(c("Date",GDPnames))) 
# Test by converting back to quarterly and comparing
EQrtlydata <- M_to_Q(Edata)
Diffs <- data.frame(Date=seq.Date(firstqtr,lastqtr,"quarter"))
Pdiffs <- data.frame(Date=seq.Date(firstqtr,lastqtr,"quarter"))
for (i in 1:nrow(Qdata)) {
  for (j in 2:ncol(Qdata)) {
    Diffs[i,j] <- round(Qdata[i,j]-EQrtlydata[i,j],0)
    Pdiffs[i,j] <- round(100*(Qdata[i,j]-EQrtlydata[i,j])/Qdata[i,j],1)
  }
}
View(Diffs) # Good - these diffs are all between -2 and +2
cat(paste0(
  "\nThere are ",N," time series being temporally disaggregated from quarters to months.",
  "\n",length(CasesC_ChowLin)," are interpolated and extrapolated using one related indicator via the Chow-Lin method.",
  "\n",length(CasesC_ChowLin2)," are interpolated and extrapolated using two related indicators. ",
  "\n",length(CasesC_Equality)," are known and require no interpolation or extrapolation.",
  "\n",length(CasesC_ARIMA)," are interpolated with the Denton-Dagum-Cholette method and extrapolated with ARIMA models.",
  "\n",length(CasesC_Identity)," are interpolated and extrapolated by applying identity equations."),sep="\n")

# To create monthly tables and charts use this code: Make_tables_GDP_Cdollar.R

# ESTIMATES AT CONSTANT 2012 PRICES  

#-------------------------------------------------------------------------------
# The estimates at 2012 constant prices will be calculated using the interpolated
# and extrapolated price indexes.

# (1) Get the actual quarterly data (Vdata) at constant 2012 prices and compare 
# it to the value data in 2012 to make sure the volume categories line up with 
# the value ones. Then calculate the implicit prices (2012=100).

# Actual quarterly volume data
Vdata <- data.frame(Date=seq.Date(firstqtr,lastqtr,by="quarter"))
(Vnames <- names(GDP))
for (i in 1:N) {
  if (exists("ConVolnumber",GDP[[i]])) {
    vec <- GDP[[i]]$ConVolnumber
    if (substr(vec,1,4)=="KEXT") {
      var <- substr(GDP[[i]]$ConVolnumber,11,nchar(GDP[[i]]$ConVolnumber))
      for (j in 1:ncol(KEXTdataQ)) {
        if (names(KEXTdataQ)[j]==var) colno <- j
      }
      tmp1 <- data.frame(Date=seq.Date(firstqtr,lastqtr,"quarter"))
      tmp1$VALUE <- KEXTdataQ[,colno]
    } else if (substr(vec,1,1)=="v") { # if there is a cansim number for this case
      tmp1 <- get_cansim_vector(vec,firstqtrC)
    }
    tmp1 <- filter(tmp1,Date<=lastqtr)
    Vdata[,i+1] <- tmp1$VALUE
    names(Vdata)[i+1] <- names(GDP)[i]
  } else { print(paste0("ConVolnumber not provided in metadata for ",names(GDP)[i])) } 
}
Vnames <- names(Vdata) # This works

# Calculate the sum of the four quarters in 2012 (both are SAAR)
# This should be same in C$ and K$ except for rounding error
tmp <- filter(Vdata,Date>as.Date("2011-10-01") & Date<as.Date("2013-01-01"))
tmp <- tmp[2:ncol(tmp)] # remove Date column
SumVQ2012 <- colSums(tmp)
tmp <- filter(Qdata,Date>as.Date("2011-10-01") & Date<as.Date("2013-01-01"))
tmp <- tmp[2:ncol(tmp)] # remove Date column
SumCQ2012 <- colSums(tmp)
(Diff <- round(SumVQ2012/4-SumCQ2012/4,0)) # Good: These diffs are the actual diffs on Cansim

# Calculate the quarterly price indexes - these are the Paasche indexes
# so they are different from those in 36-10-0106-01
Pdata <- data.frame(Date=seq.Date(firstqtr,lastqtr,by="quarter"))
for (i in 1:nrow(Qdata)) {
  for (j in 2:ncol(Qdata)) {
    Pdata[i,j] <- round(100*Qdata[i,j]/Vdata[i,j],2)
  }
}
(colnames(Pdata) <- Vnames)
#Pnames <- names(GDP)

#-------------------------------------------------------------------------------
# Identify the ARIMA cases where the volume is directly calculated; 
# then use Denton-Cholette to temporally disaggregate
# all of them and then use ARIMA to extrapolate them
# (1) identify
MODELS_ARIMA <- list()
ARIMAcases <- list()
ARIMAnums <- numeric()
for (i in 1:N) {
  if (GDP[[i]]$vtype=="ARIMA") {
    ARIMAcases <- c(ARIMAcases,GDP[i])
    ARIMAnums <- c(ARIMAnums,i) 
  }
}
# (2) interpolate
ARIMAdata <- data.frame(Date=seq.Date(firstmon,lastmon,by="month"))
for (i in 1:length(ARIMAcases)) {
  nam <- names(ARIMAcases)[i]
  ARIMAdata[i+1] <- MakeMonthlyNoDriver(Vdata[,which(colnames(Vdata)==nam)],nam,0,F)
  names(ARIMAdata)[i+1] <- nam
}
# (3) extrapolate
if (lastmonF>lastmon) {
  FARIMAdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
  for (i in 1:length(ARIMAcases)) {
    if (month(lastmonF) %in% c(1,4,7,10)) numf <- 1 # Is the last month the first,
    if (month(lastmonF) %in% c(2,5,8,11)) numf <- 2 # second or third in the quarter
    if (month(lastmonF) %in% c(3,6,9,12)) numf <- 3
    nam <- names(ARIMAcases)[i]
    model <- auto.arima(ARIMAdata[i+1])
    fc <- forecast(model,numf)
    MODELS_ARIMA[[i]] <- summary(model)
    names(MODELS_ARIMA)[i] <- nam
    if (numf==1) {
      FARIMAdata[,i+1] <- c(round(ARIMAdata[,i+1],0),round(fc$mean[1],0))
    } else if (numf==2) {
      FARIMAdata[,i+1] <- c(round(ARIMAdata[,i+1],0),round(fc$mean[1],0),round(fc$mean[2],0))
    } else {
      FARIMAdata[,i+1] <- c(round(ARIMAdata[,i+1],0),round(fc$mean[1],0),round(fc$mean[2],0),
        round(fc$mean[3],0))
    }
    names(FARIMAdata)[i+1] <- nam
  }
  # (4) Make a table of the ARIMA model results
  (NARIMA <- length(MODELS_ARIMA))
  REGRES <- data.frame(
    Name=character(NARIMA),
    NOBS=numeric(NARIMA),
    SIGMA=numeric(NARIMA),
    ARMA1=numeric(NARIMA),
    ARMA2=numeric(NARIMA),
    ARMA3=numeric(NARIMA),
    ARMA4=numeric(NARIMA),
    ARMA5=numeric(NARIMA),
    ARMA6=numeric(NARIMA),
    ARMA7=numeric(NARIMA),
    PHI1=numeric(NARIMA),
    PHI2=numeric(NARIMA),
    PHI3=numeric(NARIMA),
    PHI4=numeric(NARIMA),
    PHI5=numeric(NARIMA),
    AIC=numeric(NARIMA), # Akaike information criterion
    AICC=numeric(NARIMA), # Akaike information criterion adjusted for small samples
    BIC=numeric(NARIMA), # Bayesian information criterion or Schwarz information criterion
    LOGLIK=numeric(NARIMA), # maximized log-likelihood (of the differenced data)
    MEAN=numeric(NARIMA),
    SEE=numeric(NARIMA))
  for (i in 1:length(MODELS_ARIMA)) {
    REGRES$Name[i] <-  names(MODELS_ARIMA)[i]
    REGRES$NOBS[i] <-  MODELS_ARIMA[[i]]$nobs
    REGRES$SIGMA[i] <- round((MODELS_ARIMA[[i]]$sigma2)^0.5,3)
    REGRES$ARMA1[i] <-  MODELS_ARIMA[[i]]$arma[1]
    REGRES$ARMA2[i] <-  MODELS_ARIMA[[i]]$arma[2]
    REGRES$ARMA3[i] <-  MODELS_ARIMA[[i]]$arma[3]
    REGRES$ARMA4[i] <-  MODELS_ARIMA[[i]]$arma[4]
    REGRES$ARMA5[i] <-  MODELS_ARIMA[[i]]$arma[5]
    REGRES$ARMA6[i] <-  MODELS_ARIMA[[i]]$arma[6]
    REGRES$ARMA7[i] <-  MODELS_ARIMA[[i]]$arma[7]
    REGRES$PHI1[i] <-  round(MODELS_ARIMA[[i]]$model$phi[1],3)
    REGRES$PHI2[i] <-  round(MODELS_ARIMA[[i]]$model$phi[2],3)
    REGRES$PHI3[i] <-  round(MODELS_ARIMA[[i]]$model$phi[3],3)
    REGRES$PHI4[i] <-  round(MODELS_ARIMA[[i]]$model$phi[4],3)
    REGRES$PHI5[i] <-  round(MODELS_ARIMA[[i]]$model$phi[5],3)
    REGRES$AIC[i] <-  round(MODELS_ARIMA[[i]]$aic,3)
    REGRES$AICC[i] <-  round(MODELS_ARIMA[[i]]$aicc,3)
    REGRES$BIC[i] <-  round(MODELS_ARIMA[[i]]$bic,3)
    REGRES$LOGLIK[i] <-  round(MODELS_ARIMA[[i]]$loglik,3)
    REGRES$MEAN[i] <- mean(MODELS_ARIMA[[i]]$x)
    REGRES$SEE[i] <- 100*((MODELS_ARIMA[[i]]$sigma2)^0.5)/MEAN
  }
  View(REGRES)
  saveRDS(REGRES,paste0("ARIMA_REGRES_",todayDate,".rds"))
} else { FARIMAdata <- ARIMAdata }

#-------------------------------------------------------------------------------
# Identify the ARIMA cases where the volume is indirectly calculated by modelling
# the price index and then deflating, then use Denton-Cholette to temporally 
# disaggregate the price indexes and then use ARIMA to extrapolate them, then
# calculate the volume estimates indirectly by deflation
# (1) identify
MODELS_ARIMA_ByDeflation <- list()
ARIMAByDeflationcases <- list()
ARIMAByDeflationnums <- numeric()
for (i in 1:N) {
  if (GDP[[i]]$vtype=="ByDeflation") {
    ARIMAByDeflationcases <- c(ARIMAByDeflationcases,GDP[i])
    ARIMAByDeflationnums <- c(ARIMAByDeflationnums,i) 
  }
}
# (2) interpolate
ARIMAByDeflationdata <- data.frame(Date=seq.Date(firstmon,lastmon,by="month"))
for (i in 1:length(ARIMAByDeflationcases)) {
  nam <- names(ARIMAByDeflationcases)[i]
  ARIMAByDeflationdata[i+1] <- MakeMonthlyNoDriver(Pdata[,which(colnames(Pdata)==nam)],nam,6,F)
  names(ARIMAByDeflationdata)[i+1] <- nam
}
if (lastmonF>lastmon) {
  # (3) extrapolate
  FARIMAByDeflationdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
  for (i in 1:length(ARIMAByDeflationcases)) {
    if (month(lastmonF) %in% c(1,4,7,10)) numf <- 1 # Is the last month the first,
    if (month(lastmonF) %in% c(2,5,8,11)) numf <- 2 # second or third in the quarter
    if (month(lastmonF) %in% c(3,6,9,12)) numf <- 3
    nam <- names(ARIMAByDeflationcases)[i]
    model <- auto.arima(ARIMAByDeflationdata[i+1])
    fc <- forecast(model,numf)
    MODELS_ARIMA_ByDeflation[[i]] <- summary(model)
    names(MODELS_ARIMA_ByDeflation)[i] <- nam
    if (numf==1) {
      FARIMAByDeflationdata[,i+1] <- c(round(ARIMAByDeflationdata[,i+1],6),round(fc$mean[1],6))
    } else if (numf==2) {
      FARIMAByDeflationdata[,i+1] <- c(round(ARIMAByDeflationdata[,i+1],6),round(fc$mean[1],6),round(fc$mean[2],6))
    } else {
      FARIMAByDeflationdata[,i+1] <- c(round(ARIMAByDeflationdata[,i+1],6),round(fc$mean[1],6),round(fc$mean[2],6),
        round(fc$mean[3],0))
    }
    names(FARIMAByDeflationdata)[i+1] <- nam
  }
  # (4) Make a table of the ARIMA model results
  (NARIMAByDeflation <- length(MODELS_ARIMA_ByDeflation))
  REGRES <- data.frame(
    Name=character(NARIMAByDeflation),
    NOBS=numeric(NARIMAByDeflation),
    SIGMA=numeric(NARIMAByDeflation),
    ARMA1=numeric(NARIMAByDeflation),
    ARMA2=numeric(NARIMAByDeflation),
    ARMA3=numeric(NARIMAByDeflation),
    ARMA4=numeric(NARIMAByDeflation),
    ARMA5=numeric(NARIMAByDeflation),
    ARMA6=numeric(NARIMAByDeflation),
    ARMA7=numeric(NARIMAByDeflation),
    PHI1=numeric(NARIMAByDeflation),
    PHI2=numeric(NARIMAByDeflation),
    PHI3=numeric(NARIMAByDeflation),
    PHI4=numeric(NARIMAByDeflation),
    PHI5=numeric(NARIMAByDeflation),
    AIC=numeric(NARIMAByDeflation), # Akaike information criterion
    AICC=numeric(NARIMAByDeflation), # Akaike information criterion adjusted for small samples
    BIC=numeric(NARIMAByDeflation), # Bayesian information criterion or Schwarz information criterion
    LOGLIK=numeric(NARIMAByDeflation), # maximized log-likelihood (of the differenced data)
    MEAN=numeric(NARIMAByDeflation),
    SEE=numeric(NARIMAByDeflation))
  for (i in 1:length(MODELS_ARIMA_ByDeflation)) {
    REGRES$Name[i] <-  names(MODELS_ARIMA_ByDeflation)[i]
    REGRES$NOBS[i] <-  MODELS_ARIMA_ByDeflation[[i]]$nobs
    REGRES$SIGMA[i] <- round((MODELS_ARIMA_ByDeflation[[i]]$sigma2)^0.5,3)
    REGRES$ARMA1[i] <-  MODELS_ARIMA_ByDeflation[[i]]$arma[1]
    REGRES$ARMA2[i] <-  MODELS_ARIMA_ByDeflation[[i]]$arma[2]
    REGRES$ARMA3[i] <-  MODELS_ARIMA_ByDeflation[[i]]$arma[3]
    REGRES$ARMA4[i] <-  MODELS_ARIMA_ByDeflation[[i]]$arma[4]
    REGRES$ARMA5[i] <-  MODELS_ARIMA_ByDeflation[[i]]$arma[5]
    REGRES$ARMA6[i] <-  MODELS_ARIMA_ByDeflation[[i]]$arma[6]
    REGRES$ARMA7[i] <-  MODELS_ARIMA_ByDeflation[[i]]$arma[7]
    REGRES$PHI1[i] <-  round(MODELS_ARIMA_ByDeflation[[i]]$model$phi[1],3)
    REGRES$PHI2[i] <-  round(MODELS_ARIMA_ByDeflation[[i]]$model$phi[2],3)
    REGRES$PHI3[i] <-  round(MODELS_ARIMA_ByDeflation[[i]]$model$phi[3],3)
    REGRES$PHI4[i] <-  round(MODELS_ARIMA_ByDeflation[[i]]$model$phi[4],3)
    REGRES$PHI5[i] <-  round(MODELS_ARIMA_ByDeflation[[i]]$model$phi[5],3)
    REGRES$AIC[i] <-  round(MODELS_ARIMA_ByDeflation[[i]]$aic,3)
    REGRES$AICC[i] <-  round(MODELS_ARIMA_ByDeflation[[i]]$aicc,3)
    REGRES$BIC[i] <-  round(MODELS_ARIMA_ByDeflation[[i]]$bic,3)
    REGRES$LOGLIK[i] <-  round(MODELS_ARIMA_ByDeflation[[i]]$loglik,3)
    REGRES$MEAN[i] <- mean(MODELS_ARIMA_ByDeflation[[i]]$x)
    REGRES$SEE[i] <- 100*((MODELS_ARIMA_ByDeflation[[i]]$sigma2)^0.5)/MEAN
  }
  View(REGRES)
  saveRDS(REGRES,paste0("ARIMA_ByDeflation_REGRES_",todayDate,".rds"))
} else { FARIMAByDeflationdata <- ARIMAByDeflationdata }
#-------------------------------------------------------------------------------
# Now calculate the monthly volume estimates by deflation and re-benchmark to 
# the quarterly volume estimates
Defldata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 2:length(FARIMAByDeflationdata)) {
  var <- colnames(FARIMAByDeflationdata)[i]
  for (j in 1:ncol(Edata)) {if (colnames(Edata)[j]==var) idx <- j}
  Defldata[,i] <- round(100*Edata[,idx]/FARIMAByDeflationdata[,i],0)
  names(Defldata)[i] <- var
}

#-------------------------------------------------------------------------------
# Find all the equality cases; then collect the data in EQdata
FEQUALPdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
FEQUALPdata$HHFCGD <- round(100*EXTdata$HHFCGD/KEXTdata$HHFCGD,8)
FEQUALPdata$HHFCGS <- round(100*EXTdata$HHFCGS/KEXTdata$HHFCGS,8)
FEQUALPdata$HHFCGN <- round(100*EXTdata$HHFCGN/KEXTdata$HHFCGN,8)
FEQUALPdata$HHFCS  <- round(100*EXTdata$HHFCS/KEXTdata$HHFCS,8)
FEQUALVdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
FEQUALVdata$HHFCGD <- round(KEXTdata$HHFCGD)
FEQUALVdata$HHFCGS <- round(KEXTdata$HHFCGS)
FEQUALVdata$HHFCGN <- round(KEXTdata$HHFCGN)
FEQUALVdata$HHFCS  <- round(KEXTdata$HHFCS)

#-------------------------------------------------------------------------------
# Collect the GDP volume drivers, check availability  
# dates and forecast to lastmonF with ARIMA if necessary
ChowLincases <- list()
ChowLinNums <- numeric()
for (i in 1:N) {
  if (GDP[[i]]$vtype=="Chow-Lin" | GDP[[i]]$vtype=="Chow-Lin-2") {
    ChowLincases <- c(ChowLincases,GDP[i])
    ChowLinNums <- c(ChowLinNums,i)
  }
}
# Store the Chow-Lin results in ChowLinInputdata
ChowLinInputdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
k <- 0 
for (i in ChowLinNums) {
    # First of (possibly) two drivers
    k <- k+1
    vec <- GDP[[i]]$Kd_vnumber
    if (substr(vec,1,4)=="KEXT") {
      var <- substr(GDP[[i]]$Kd_vnumber,10,nchar(GDP[[i]]$Kd_vnumber))
      for (j in 1:ncol(KEXTdata)) {
        if (names(KEXTdata)[j]==var) colno <- j
      }
      tmp1 <- data.frame(Date=seq.Date(firstmon,lastmonF,"month"))
      tmp1$VALUE <- KEXTdata[,colno]
      tmpval <- tmp1$VALUE
    } else if (substr(vec,1,1)=="v") {
      tmp1 <- get_cansim_vector(vec,firstmonC)
      tmpval <- tmp1$VALUE
    }
    date1 <- tmp1$Date[1]
    date2 <- tmp1$Date[nrow(tmp1)]
    if (date1>firstmon) { # backcast the series to firstmon via ARIMA
      tmpval <- Backcast(tmp1$VALUE,mondf(firstmon,date1),firstmon)
      tmp1$VALUE <- tmpval
    }
    if (date2>lastmonF) {
      tmp1 <- filter(tmp1,Date<=lastmonF)
      tmpval <- tmp1$VALUE
    }
    if (date2<lastmonF) {
      print(paste0("The last available date for ",names(GDP)[i],
        " is ",date2,
        " so the indicator series will be extended with ARIMA to ",
        lastmonF))
     tmpval <- ArimaForecast(tmp1$VALUE,mondf(date2,lastmonF))
    }
    ChowLinInputdata[,k+1] <- tmpval
    if (GDP[[i]]$vtype=="Chow-Lin") names(ChowLinInputdata)[k+1] <- names(GDP)[i]
    else names(ChowLinInputdata)[k+1] <- paste0(names(GDP)[i],"1")
    # Second of (possibly) two drivers
    if (GDP[[i]]$vtype=="Chow-Lin-2") {
      k <- k+1
      vec <- GDP[[i]]$Kd2_vnumber
      if (substr(vec,1,4)=="KEXT") {
        var <- substr(GDP[[i]]$Kd2_vnumber,10,nchar(GDP[[i]]$Kd2_vnumber))
        for (j in 1:ncol(KEXTdata)) {
          if (names(KEXTdata)[j]==var) colno <- j
        }
        tmp1 <- data.frame(Date=seq.Date(firstmon,lastmonF,"month"))
        tmp1$VALUE <- KEXTdata[,colno]
        tmpval <- tmp1$VALUE
      } else if (substr(vec,1,1)=="v") {
        tmp1 <- get_cansim_vector(vec,firstmonC)
        tmpval <- tmp1$VALUE
      }
      date1 <- tmp1$Date[1]
      date2 <- tmp1$Date[nrow(tmp1)]
      if (date1>firstmon) { # backcast the series to firstmon via ARIMA
        tmpval <- Backcast(tmp1$VALUE,mondf(firstmon,date1),firstmon)
        tmp1$VALUE <- tmpval
      }
      if (date2>lastmonF) {
        tmp1 <- filter(tmp1,Date<=lastmonF)
        tmpval <- tmp1$VALUE
      }
      if (date2<lastmonF) {
          print(paste0("The last available date for 2nd driver ",names(GDP)[i],
          " is ",date2,
          " so the indicator series will be extended with ARIMA to ",
          lastmonF))
          tmpval <- ArimaForecast(tmp1$VALUE,mondf(date2,lastmonF))
      }
      ChowLinInputdata[,k+1] <- tmpval
      names(ChowLinInputdata)[k+1] <- paste0(names(GDP)[i],"2")
    }
}
#-------------------------------------------------------------------------------
# Use the Chow-Lin procedure in each of the cases where it applies, with 
# the indicators in ChowLinInputdata (which all extend to lastmonF)
# All price indexes have one driver only
MODELS_ChowLin <- list()
ChowLinResultsdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 1:length(ChowLincases)) {
  nam <- names(ChowLincases)[i]
  if (GDP[[nam]]$vtype=="Chow-Lin") {
    NewList <- MakeMonthlyV2Driver(MODELS_ChowLin,nam,F)
  } else {
    NewList <- MakeMonthlyV22Driver(MODELS_ChowLin,nam,F)
  }
  ChowLinResultsdata[,i+1] <- NewList[[2]]
  MODELS_ChowLin <- NewList[[1]]
  colnames(ChowLinResultsdata)[i+1] <- nam
}
#-------------------------------------------------------------------------------
# Summarize regression results
NR <- length(MODELS_ChowLin)
REGRES <- data.frame(Name=character(NR),RSQ=numeric(NR),
  ARSQ=numeric(NR),SIGMA=numeric(NR),RHO=numeric(NR),
  RSS=numeric(NR),MEAN=numeric(NR),
  SEE=numeric(NR),NUMVAR=numeric(NR),DRIVER1=character(NR),
  DRIVER2=character(NR))
for (i in 1:length(MODELS_ChowLin)) {
  df <- as.data.frame(MODELS_ChowLin[[i]]$actual)
  MEAN <- mean(df$VALUE)
  REGRES$Name[i] <- names(MODELS_ChowLin)[i]
  REGRES$RSQ[i] <- round(MODELS_ChowLin[[i]]$r.squared,3)
  REGRES$ARSQ[i] <- round(MODELS_ChowLin[[i]]$adj.r.squared,3)
  REGRES$SIGMA[i] <- round((MODELS_ChowLin[[i]]$s_2)^0.5,3)
  REGRES$RHO[i] <- round(MODELS_ChowLin[[i]]$rho,3)
  REGRES$RSS[i] <- round(MODELS_ChowLin[[i]]$rss,0)
  REGRES$MEAN[i] <- MEAN
  REGRES$SEE[i] <- 100*((MODELS_ChowLin[[i]]$s_2)^0.5)/MEAN
  REGRES$NUMVAR[i] <- nrow(MODELS_ChowLin[[i]]$coefficients)
  if (REGRES$NUMVAR[i]==2) {
    REGRES$DRIVER1[i] <- GDP[[REGRES$Name[i]]]$Kdriver
    REGRES$DRIVER2[i] <- NA
  } else {
    REGRES$DRIVER1[i] <- GDP[[REGRES$Name[i]]]$Kdriver
    REGRES$DRIVER2[i] <- GDP[[REGRES$Name[i]]]$Kdriver2
  }
}
REGRESpub <- REGRES
for (i in 1:nrow(REGRES)) {
  REGRESpub$Name[i] <- GDP[[REGRES$Name[i]]]$betterName  
}
REGRESpub <- select(REGRESpub,-RSQ,-RSS,-NUMVAR)
REGRESpub$MEAN <- round(REGRESpub$MEAN,0)
REGRESpub$SIGMA <- round(REGRESpub$SIGMA,0)
REGRESpub$SEE <- round(REGRESpub$SEE,3)
View(REGRESpub)
saveRDS(REGRES,paste0("KREGRES_",todayDate,".rds"))
saveRDS(REGRESpub,paste0("KREGRESpub_",todayDate,".rds"))
#-------------------------------------------------------------------------------
# Calculate the identity cases using the identity equations
IdentCases <- character()
Idents <- character()
j <- 0
for (i in 1:N) {
  if (GDP[[i]]$vtype=="Identity") {
    j <- j+1
    IdentCases[j] <- names(GDP)[i]
    Idents[j] <- GDP[[i]]$equation
  }
}
# Apply the identities to calculate the identity results
IdentOrder <- c(4,5,8,9,11,12,13,3,7,10,2,6,14,1)
IdentCases <- IdentCases[IdentOrder]
Idents <- Idents[IdentOrder]
IDdata <- cbind(FEQUALVdata,
  select(FARIMAdata,-Date),
  select(Defldata,-Date),
  select(ChowLinResultsdata,-Date))
j <- ncol(IDdata)
for (i in 1:length(IdentCases)) {
  j <- j+1
  nam <- IdentCases[i]
  IDdata[j] <- round(eval(parse(text=Idents[i]),IDdata),8)
  colnames(IDdata)[j] <- nam
}
IDdata <- select(IDdata,c(Date,names(GDP)))
IDdataQ <- M_to_Q(IDdata)
Diff <- Vdata
for (i in 1:nrow(Vdata)) {
  for (j in 2:ncol(Vdata)) {
    Diff[i,j] <- round(IDdataQ[i,j]-Vdata[i,j],0)
  }
}
View(Diff)

# Calculate the implicit price indexes (2012=100)
IPdata <- Edata
for (i in 1:nrow(Edata)) {
  for (j in 2:ncol(Edata)) {
    IPdata[i,j] <- round(100*Edata[i,j]/IDdata[i,j],1)
  }
}

# In summary, the monthly interpolated and extrapolated GDP data
# are in these data frames:
# At current prices:       Edata
# At constant 2012 prices: IDdata
# Implicit prices:         IPdata
saveRDS(Edata, paste0("GDP_C_monthly_",todayDate,".rds"))
saveRDS(IDdata,paste0("GDP_K_monthly_",todayDate,".rds"))
saveRDS(IPdata,paste0("GDP_P_monthly_",todayDate,".rds"))

# To create monthly tables and charts use this code: Make_tables_GDP_Kdollar.R

