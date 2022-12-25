# GDP_IandE_monthly data prep
# November 29, 2022

setwd("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly")

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

CfileName <- "Oct2022_Dec122022"
FolderName <- "/Users/philipsmith/Documents/R/Households/HH_consumption/Consumption_monthly/Results_Oct_2022/"

# Res_resale_val (from CREA) and fed exp (from Finance Canada) must be updated by hand
firstMonCREA <- as.Date("2010-01-01") # CREA
lastMonCREA <- as.Date("2022-10-01") # CREA
firstMonFM <- as.Date("2010-01-01") # Fiscal Monitor
lastMonFM <- as.Date("2022-08-01") # Fiscal Monitor

# Create external data matrix EXTdata and KEXTdata with required C$ and K$ input data
EXTdata  <- data.frame(Date=seq.Date(firstmon,lastmonF,"month")) # Monthly data
EXTdataQ <- data.frame(Date=seq.Date(firstqtr,lastqtr,"quarter")) # Quarterly data
KEXTdata  <- data.frame(Date=seq.Date(firstmon,lastmonF,"month")) # Monthly data
KEXTdataQ <- data.frame(Date=seq.Date(firstqtr,lastqtr,"quarter")) # Quarterly data

# Retrieve the latest calculations for monthly HH final consumption ($C)
HHFCdata_C  <- readRDS(paste0(FolderName,
  "HFCE_C_monthly_2012_",CfileName,".rds"))
HHFCdata_K  <- readRDS(paste0(FolderName,
  "HFCE_K_monthly_2012_",CfileName,".rds"))
EXTdata$HHFC    <- HHFCdata_C$HHFC
EXTdata$HHFCGD  <- HHFCdata_C$CD
EXTdata$HHFCGS  <- HHFCdata_C$CSD
EXTdata$HHFCGN  <- HHFCdata_C$CND
EXTdata$HHFCS   <- HHFCdata_C$CS
EXTdata$KHHFC   <- HHFCdata_K$HHFC
EXTdata$KHHFCGD <- HHFCdata_K$CD
EXTdata$KHHFCGS <- HHFCdata_K$CSD
EXTdata$KHHFCGN <- HHFCdata_K$CND
EXTdata$KHHFCS  <- HHFCdata_K$CS
# Retrieve monthly CREA residential resale dollars - January 1080 to Sept 2022
Res_resale_val <- c(
  "592203443"   ,"843630567"   ,"813008552"   ,"726641186"   ,"773816689"   ,"947890096"   ,"1127900191", 
  "1089919233"  ,"1147485374"  ,"1141500821"  ,"932596279"   ,"730363394"   ,"783223346"   ,"1091842182", 
  "1598214906"  ,"1694965661"  ,"1339643005"  ,"1036657489"  ,"975057161"   ,"820143780"   ,"798610890",  
  "761333199"   ,"778464411"   ,"724758079"   ,"660002597"   ,"842143245"   ,"974466636"   ,"865771645",  
  "880059711"   ,"868406507"   ,"777191238"   ,"892923585"   ,"1087582780"  ,"1121402049"  ,"1225325893", 
  "948344482"   ,"774706286"   ,"991100859"   ,"1372729459"  ,"1274614206"  ,"1369103213"  ,"1411134163",
  "1175950933"  ,"1316174514"  ,"1186656213"  ,"1099484197"  ,"1113225272"  ,"812263895"   ,"822325153",  
  "1297122436"  ,"1458384003"  ,"1406800255"  ,"1592413237"  ,"1333284927"  ,"1207435283"  ,"1181659410", 
  "1074317031"  ,"1265112048"  ,"1222174979"  ,"923411601"   ,"1069420439"  ,"1521130257"  ,"1652507060", 
  "1745647249"  ,"2014914450"  ,"1892170751"  ,"2005416929"  ,"1838709066"  ,"1748842302"  ,"1896763620", 
  "1657929200"  ,"1237990531"  ,"1432201581"  ,"1803913113"  ,"1884082508"  ,"2506756754"  ,"2503211296", 
  "2326270482"  ,"2310860060"  ,"2190559505"  ,"2252750737"  ,"2314876149"  ,"1960889125"  ,"1593044174", 
  "1771697282"  ,"2872441306"  ,"3602481232"  ,"3225772419"  ,"2582746677"  ,"2379257757"  ,"2414075825", 
  "2264384900"  ,"2585761543"  ,"2584415383"  ,"2118257828"  ,"1703763339"  ,"1876459812"  ,"3062269772", 
  "4307383185"  ,"4213991825"  ,"3926363049"  ,"3587049263"  ,"3162663410"  ,"3557018716"  ,"3447522590", 
  "3208736719"  ,"3414875890"  ,"2575366207"  ,"3060628807"  ,"4793404922"  ,"4999416815"  ,"3528529981", 
  "3627892434"  ,"3571113882"  ,"3281615054"  ,"4078746373"  ,"4057259893"  ,"4323233858"  ,"4250509519", 
  "2950441293"  ,"3133765857"  ,"3892967976"  ,"3733130331"  ,"3120378146"  ,"2778276125"  ,"2617555080", 
  "2626184469"  ,"2970569475"  ,"2722953153"  ,"2897221448"  ,"2483062999"  ,"1911313476"  ,"1958794697", 
  "3431971130"  ,"5182381482"  ,"5613944995"  ,"5088587560"  ,"4029993794"  ,"3801124808"  ,"3363411340", 
  "2995150281"  ,"3154512950"  ,"2830360361"  ,"2348549747"  ,"2548536568"  ,"4097121032"  ,"5363679515", 
  "4779480878"  ,"4318692273"  ,"4182689139"  ,"4127689908"  ,"3959405932"  ,"4531101325"  ,"4381226663", 
  "3428685663"  ,"2433310970"  ,"2122095019"  ,"3428063635"  ,"4560021949"  ,"4858675429"  ,"4674806563", 
  "4508753444"  ,"3991029777"  ,"3800461175"  ,"3792141999"  ,"3397319343"  ,"3505526448"  ,"2872803611", 
  "2650123367"  ,"4367901550"  ,"6001351994"  ,"5816014779"  ,"4803381704"  ,"4204849130"  ,"3385880487", 
  "3541870374"  ,"3388922289"  ,"3304888057"  ,"3230374263"  ,"2305788546"  ,"2103740443"  ,"2606765173", 
  "3356340578"  ,"3240826978"  ,"4000708090"  ,"4029131328"  ,"3739115805"  ,"4185378395"  ,"3526191726", 
  "3298635406"  ,"3089568997"  ,"2168754997"  ,"2326497838"  ,"3640710625"  ,"4709659842"  ,"4812208160", 
  "4909853530"  ,"4303017203"  ,"4124342458"  ,"3843095294"  ,"3780637979"  ,"4347818368"  ,"4575904440", 
  "3534984953"  ,"3360676849"  ,"4325235730"  ,"4646865906"  ,"5646942915"  ,"5187190634"  ,"4728666445", 
  "4578696615"  ,"4036895672"  ,"4066288147"  ,"4292729702"  ,"3503540044"  ,"2794727678"  ,"2444993965", 
  "3591713115"  ,"4504534369"  ,"4971916042"  ,"5128321295"  ,"4994516975"  ,"4470688206"  ,"3895178006", 
  "4001581487"  ,"3716849952"  ,"3410427209"  ,"2797699042"  ,"2329833352"  ,"3801622328"  ,"5263393841", 
  "5623503231"  ,"5724025379"  ,"5703540858"  ,"4830934132"  ,"4545643130"  ,"4373607222"  ,"4042777457", 
  "3958363874"  ,"2841923069"  ,"2656424982"  ,"4284149467"  ,"5758903084"  ,"5293075300"  ,"5982351692", 
  "5421686210"  ,"4552789701"  ,"4705977046"  ,"4532942446"  ,"4398106002"  ,"4255691414"  ,"2811586905", 
  "3109113234"  ,"4528694772"  ,"5797166172"  ,"6065915134"  ,"7300215664"  ,"6692568080"  ,"5856650238", 
  "5850485135"  ,"4964847456"  ,"5319982022"  ,"5509617065"  ,"4405381955"  ,"4941081937"  ,"6782145361", 
  "7695967640"  ,"8699917638"  ,"8500466101"  ,"6915194316"  ,"6512501933"  ,"6120782311"  ,"6220526235", 
  "6617967422"  ,"5763478991"  ,"4195942406"  ,"4902653192"  ,"6462646734"  ,"7694278979"  ,"8238363261", 
  "8923563584"  ,"8866096134"  ,"9060815562"  ,"7635412458"  ,"7978312924"  ,"8311496383"  ,"6770744946", 
  "5126824770"  ,"5143223225"  ,"7567034717"  ,"10994852271" ,"11243234455" ,"11160214760" ,"10773180904",
  "8996333152"  ,"8454960702"  ,"8394813819"  ,"8136645871"  ,"7693217872"  ,"5615512171"  ,"5384193413", 
  "8365500718"  ,"11009064030" ,"12173141745" ,"12917616461" ,"12739431766" ,"10543874754" ,"11041298599",
  "10479494315" ,"9803845393"  ,"9276738548"  ,"6382182148"  ,"7005121534"  ,"10206079352" ,"13342189517",
  "13111728264" ,"15387813030" ,"14028344869" ,"11274985175" ,"11583830273" ,"10442120263" ,"10654755144",
  "9601558706"  ,"6909162522"  ,"8500493160"  ,"11766013194" ,"15004291341" ,"16038612932" ,"19023200150",
  "17213285471" ,"15053408545" ,"13869002334" ,"11523663176" ,"12832751792" ,"11417597504" ,"7527184816", 
  "8549908978"  ,"11413962016" ,"12753158413" ,"15609114547" ,"15957893592" ,"14567887752" ,"12799090176",
  "10500034263" ,"10632717076" ,"8406455128"  ,"5942465113"  ,"4551549761"  ,"4483489774"  ,"7094157249", 
  "10141730415" ,"13267371071" ,"15879869956" ,"17883356065" ,"16489055040" ,"13847839996" ,"14154997873",
  "14424044503" ,"12247752686" ,"9324151441"  ,"8404635720"  ,"12158680571" ,"16771954333" ,"18020678924",
  "16448358592" ,"14971331636" ,"11639392601" ,"11092511480" ,"11281057069" ,"11368220255" ,"11265271568",
  "8144589060"  ,"8190937735"  ,"12440904849" ,"17029794622" ,"16485291512" ,"18336423110" ,"18019352431",
  "14229730223" ,"13884066046" ,"13404050667" ,"13034105472" ,"12411207330" ,"8613563562"  ,"8662870568", 
  "13767128280" ,"17214249133" ,"18549227228" ,"19919241223" ,"17212253617" ,"14443097084" ,"12716669387",
  "11460748813" ,"12999791813" ,"10844411921" ,"7226654795"  ,"8327923130"  ,"11405163518" ,"14863133757",
  "18154625944" ,"20030565019" ,"17875598992" ,"17132559891" ,"15283435300" ,"14732786500" ,"15263883193",
  "12689799813" ,"9024690250"  ,"9132211041"  ,"12926081116" ,"16602053003" ,"19624671130" ,"22707835603",
  "21351062400" ,"19405704453" ,"16507078797" ,"17328108227" ,"17557759890" ,"13740748834" ,"10149670280",
  "9259632712"  ,"14019096892" ,"19829479137" ,"23494336863" ,"25111877627" ,"25887783794" ,"21858256999",
  "18556159676" ,"18337306247" ,"18952916696" ,"16746593149" ,"12520482028" ,"11754052952" ,"19435193613",
  "25789227885" ,"29349439587" ,"31242250354" ,"30215197362" ,"23285168632" ,"21666552244" ,"20973807225",
  "20553448703" ,"18204066900" ,"12296546128" ,"12008677509" ,"19587077019" ,"29637181917" ,"29935617569",
  "31996856978" ,"26704192784" ,"20397430145" ,"20151470094" ,"19203266398" ,"20546105333" ,"19208191474",
  "13541553543" ,"12127588113" ,"15607113649" ,"20800292317" ,"23090596002" ,"25798548004" ,"23839290830",
  "20543442045" ,"19830659345" ,"17785459160" ,"19679277002" ,"16437096017" ,"10467723997" ,"11118813205",
  "14215645862" ,"19542182018" ,"24166648006" ,"28045487289" ,"24443798335" ,"24101452027" ,"21754737367",
  "21674096203" ,"23607664730" ,"19893524783" ,"14062048725" ,"13832329124" ,"20818900977" ,"23611573607",
  "10196030462" ,"16689010737" ,"30059938071" ,"36354736431" ,"34656819213" ,"37073337058" ,"35968303669",
  "29913935301" ,"24211666987" ,"22969745557" ,"36367836951" ,"54584126022" ,"51344985018" ,"47167411387",
  "42919359272" ,"35585834858" ,"33783581170" ,"34841189469" ,"37644697097" ,"35479954313" ,"25529831981",
  "24954713717" ,"40278180326" ,"50504982685" ,"40812782965" ,"37841655539" ,"31901025724" ,"23867020695",
  "24389116981" ,"22065777815","24092606142") # 24092606142 = Oct 2022
Res_resale_val <- as.numeric(Res_resale_val)
Res_resale_SA <- SEASADJ(Res_resale_val,1980,12)
df <- data.frame(
  Date=seq.Date(as.Date("1980-01-01"),lastMonCREA,"month"),
  R=Res_resale_val,
  RSA=Res_resale_SA,
  R_PC12=PC12(Res_resale_val),
  R_PC01=PC01(Res_resale_SA))
df <- filter(df,Date>=firstmon & Date<=lastmonF)
RRvalSA <- df$RSA
# CREA MLS Housing price index, seasonally adjusted (October 14, 2022 vintage)
CREA_HPI <- data.frame(Date=seq.Date(firstMonCREA,lastMonCREA,"month"),
  HPI <- c(
    152.0,153.0,153.8,154.0,153.9,153.4,153.0,152.9,153.0,153.5,154.3,155.0, # 2010 1 to 12
    156.2,157.4,158.7,159.7,160.9,162.1,163.3,164.4,165.2,166.0,166.8,167.7,
    168.5,169.2,169.8,170.3,170.4,170.2,170.1,169.9,169.8,169.7,169.6,170.1,
    170.0,170.5,170.7,171.1,171.5,172.0,172.9,173.8,174.8,175.9,176.8,177.7,
    178.6,179.1,179.5,179.9,180.5,181.4,182.1,183.2,184.4,185.7,187.2,188.3,
    189.1,189.7,190.8,191.9,193.4,195.4,197.5,199.8,201.9,204.0,206.2,208.3,
    210.9,213.9,217.0,220.7,224.9,228.8,232.3,234.6,236.8,238.1,239.8,241.2,
    244.8,250.2,256.5,259.5,258.6,256.5,254.3,253.1,253.2,253.7,254.5,254.8,
    254.2,252.9,251.8,251.0,250.6,250.4,250.5,250.3,249.8,249.6,248.1,246.6,
    243.9,240.9,239.0,239.0,239.5,240.7,242.2,243.5,245.1,246.5,247.4,248.0,
    248.5,249.3,249.0,246.1,248.2,252.6,259.3,265.6,270.1,273.0,276.0,279.1,
    284.3,291.0,298.2,304.5,310.4,316.4,321.2,326.7,334.1,343.6,351.4,357.4,
    370.5,379.7,379.2,374.1,370.1,363.1,356.9,350.9,346.1,341.9)) # Oct 2022 is 341.9
CREA_HPI <- filter(CREA_HPI,Date>=firstmon & Date<=lastmonF)
EXTdata$HPI <- CREA_HPI$HPI

#------------------------------------------------------------------------------
# Retrieve the latest calculations for monthly HH final consumption ($K)
KHHFCdata <- readRDS(paste0(FolderName,
  "HFCE_K_monthly_2012_",CfileName,".rds"))
KEXTdata$HHFC   <- KHHFCdata$HHFC
KEXTdata$HHFCGD <- KHHFCdata$CD
KEXTdata$HHFCGS <- KHHFCdata$CSD
KEXTdata$HHFCGN <- KHHFCdata$CND
KEXTdata$HHFCS  <- KHHFCdata$CS
#------------------------------------------------------------------------------
# Get monthly government consumption indicators from SEPH 14-10-0220-01
GFCF_E <- RetrieveMon("v54027090",firstmon,lastmonF) # fed govt employment SA
GFCF_AWE <- RetrieveMon("v54027089",firstmon,lastmonF) # fed govt AWE, SA
EXTdata$GFCF <- GFCF_E$VALUE*GFCF_AWE$VALUE # projector for fed govt C$ SA
KEXTdata$GFCF <- GFCF_E$VALUE # projector for fed govt K$ SA
GFCO_E1 <- RetrieveMon("v54027092",firstmon,lastmonF) # prov govt employment SA
GFCO_E2 <- RetrieveMon("v54027094",firstmon,lastmonF) # loc govt employment SA
GFCO_E3 <- RetrieveMon("v54027096",firstmon,lastmonF) # abor govt employment SA
GFCO_AWE1 <- RetrieveMon("v54027091",firstmon,lastmonF) # prov govt AWE, SA
GFCO_AWE2 <- RetrieveMon("v54027093",firstmon,lastmonF) # loc govt AWE, SA
GFCO_AWE3 <- RetrieveMon("v54027095",firstmon,lastmonF) # abor govt AWE, SA
EXTdata$GFCO <- GFCO_E1$VALUE*GFCO_AWE1$VALUE+ # projector for other govt C$ SA
  GFCO_E2$VALUE*GFCO_AWE2$VALUE+
  GFCO_E3$VALUE*GFCO_AWE3$VALUE
KEXTdata$GFCO <- GFCO_E1$VALUE+GFCO_E2$VALUE+GFCO_E3$VALUE # projector for other govt K$ SA
#------------------------------------------------------------------------------
# Get monthly bus res and nonres investment indicators from 34-10-0175-01
IBRC <- RetrieveMon("v1014954064",firstmon,lastmonF) # monthly res construction C$ SA
KIBRC <- RetrieveMon("v1014954065",firstmon,lastmonF) # monthly res construction K$ SA
EXTdata$IBRC <- IBRC$VALUE
KEXTdata$IBRC <- KIBRC$VALUE
EXTdata$IBRT <- RRvalSA
KEXTdata$IBRT <- EXTdata$IBRT/CREA_HPI$HPI
IBNR <- RetrieveMon("v1014954170",firstmon,lastmonF) # monthly non-res construction C$ SA
EXTdata$IBNR <- IBNR$VALUE
KIBNR <- RetrieveMon("v1014954171",firstmon,lastmonF) # monthly non-res construction K$ SA
KEXTdata$IBNR <- KIBNR$VALUE
# Get monthly bus M&E investment indicators 
ManME1 <- RetrieveMon("v800468",firstmon,lastmonF) # Manufacturing - machinery
ManME2 <- RetrieveMon("v800469",firstmon,lastmonF) # Manufacturing - computer & elec pdts
ManME3 <- RetrieveMon("v800470",firstmon,lastmonF) # Manufacturing - elec equipment
ManME4 <- RetrieveMon("v800471",firstmon,lastmonF) # Manufacturing - transportation equipment
EXTdata$IBME1 <- ManME1$VALUE+ManME2$VALUE+ManME3$VALUE+ManME4$VALUE
KManME1 <- RetrieveMon("v123263926",firstmon,lastmonF) # Manufacturing - machinery K$, SA
KManME2 <- RetrieveMon("v123263927",firstmon,lastmonF) # Manufacturing - computer & elec pdts K$, SA
KManME3 <- RetrieveMon("v123263928",firstmon,lastmonF) # Manufacturing - elec equipment K$, SA
KManME4 <- RetrieveMon("v123263929",firstmon,lastmonF) # Manufacturing - transportation equipment K$, SA
KEXTdata$IBME1 <- KManME1$VALUE+KManME2$VALUE+KManME3$VALUE+KManME4$VALUE
M_MandE1 <- RetrieveMon("v1001826738",firstmon,lastmonF) # imports, ind mach, C$, SA
M_MandE2 <- RetrieveMon("v1001826748",firstmon,lastmonF) # imports, electronic, C$, SA
M_MandE3 <- RetrieveMon("v1001826757",firstmon,lastmonF) # imports, mv, C$, SA
M_MandE4 <- RetrieveMon("v1001826765",firstmon,lastmonF) # imports, aircraft and OTE, C$, SA
EXTdata$IBME2 <- M_MandE1$VALUE+M_MandE2$VALUE+M_MandE3$VALUE+M_MandE4$VALUE
KM_MandE1 <- RetrieveMon("v1001783946",firstmon,lastmonF) # imports, ind mach, K$, NSA
KM_MandE2 <- RetrieveMon("v1001783947",firstmon,lastmonF) # imports, electronic, K$, NSA
KM_MandE3 <- RetrieveMon("v1001783948",firstmon,lastmonF) # imports, mv, K$, NSA
KM_MandE4 <- RetrieveMon("v1001783949",firstmon,lastmonF) # imports, aircraft and OTE, K$, NSA
KEXTdata$IBME2 <- SEASADJ(KM_MandE1$VALUE,2010,12)+
  SEASADJ(KM_MandE2$VALUE,2010,12)+
  SEASADJ(KM_MandE3$VALUE,2010,12)+
  SEASADJ(KM_MandE4$VALUE,2010,12)
# Get monthly manuf and wholesale inventories indicators 
VBM <- RetrieveMon("v803227","2009-01-01",lastmonF) # Manufacturing inventories level, C$, SA
VBM$DVBM <- VBM$VALUE-lag(VBM$VALUE,1)
VBM <- filter(VBM,Date>=firstmon & Date<=lastmonF)
EXTdata$VBNF1 <- VBM$DVBM
KVBM <- RetrieveMon("v123263945","2009-01-01",lastmonF) # Manufacturing inventories level, K$, SA
KVBM$DVBM <- KVBM$VALUE-lag(KVBM$VALUE,1)
KVBM <- filter(KVBM,Date>=firstmon & Date<=lastmonF)
KEXTdata$VBNF <- KVBM$DVBM
VBW <- RetrieveMon("v52368145","2009-01-01",lastmonF) # Wholesale inventories level
VBW$DVBW <- VBW$VALUE-lag(VBW$VALUE,1)
PVBW <- RetrieveMon("v120586539","2009-01-01",lastmonF) # Wholesale sales Fisher price, SA
VBW$KDVBW <- VBW$DVBW/PVBW$VALUE-lag(VBW$DVBW,1)/lag(PVBW$VALUE,1)
VBW <- filter(VBW,Date>=firstmon & Date<=lastmonF)
EXTdata$VBNF2 <- VBW$DVBW
KEXTdata$VBNF2 <- VBW$KDVBW
# Get monthly export and import indicators
XG <- RetrieveMon("v1001827265",firstmon,lastmonF) # exports of goods, C$, SA
XS <- RetrieveMon("v1105277795",firstmon,lastmonF) # exports of serv, C$, SA
MG <- RetrieveMon("v1001826653",firstmon,lastmonF) # imports of goods, C$, SA
MS <- RetrieveMon("v1105277805",firstmon,lastmonF) # imports of serv, C$, SA
EXTdata$XG <- XG$VALUE
EXTdata$XS <- XS$VALUE
EXTdata$MG <- MG$VALUE
EXTdata$MS <- MS$VALUE
KXG <- RetrieveMon("v1001783953",firstmon,lastmonF) # exports of goods, K$, NSA
KMG <- RetrieveMon("v1001783939",firstmon,lastmonF) # imports of goods, K$, NSA
CPIS <- RetrieveMon("v41691230",firstmon,lastmonF) # CPI services, NSA
KEXTdata$XG <- KXG$VALUE
KEXTdata$XS <- 100*XS$VALUE/CPIS$VALUE
KEXTdata$MG <- KMG$VALUE
KEXTdata$MS <- 100*MS$VALUE/CPIS$VALUE
#------------------------------------------------------------------------------
# Get required C$ and K$ quarterly data
# C$ govt final consumption
GFCO1 <- get_cansim_vector("v67276772",firstqtrC) # C$ tot govt final consump expend
GFCO2 <- get_cansim_vector("v67276773",firstqtrC) # C$ fed govt final consump expend
EXTdataQ$GFCO <- GFCO1$VALUE-GFCO2$VALUE # C$ other (non-fed) govt final consump expend
# C$ bus residential investment
IBRC1 <- get_cansim_vector("v62144023",firstqtrC) # new construction, current prices, SAAR
IBRC2 <- get_cansim_vector("v62144024",firstqtrC) # renovations, current prices, SAAR
EXTdataQ$IBRC <- IBRC1$VALUE+IBRC2$VALUE # construction
# C$ non-bus inventories
VNB1 <- get_cansim_vector("v62305772",firstqtrC)
VNB2 <- get_cansim_vector("v62305773",firstqtrC)
EXTdataQ$VNB <- VNB1$VALUE-VNB2$VALUE # Non-business inventory change
# K$ govt final consumption
GFCtot <- get_cansim_vector("v62305762",firstqtrC) # C$ tot govt final consump expend
KGFCtot <- get_cansim_vector("v62306868",firstqtrC) # K$ tot govt final consump expend
GFCF <- get_cansim_vector("v67276773",firstqtrC) # C$ fed govt final consump expend
KEXTdataQ$GFCF <- round(GFCF$VALUE/(GFCtot$VALUE/KGFCtot$VALUE),0) # K$ fed govt final consump expend
KEXTdataQ$GFCO <- round(KGFCtot$VALUE-KEXTdataQ$GFCF,0) # K$ other (non-fed) govt final consump expend
# K$ res investment
IBR <- get_cansim_vector("v62305765",firstqtrC) # C$ tot res investment
KIBR <- get_cansim_vector("v62306872",firstqtrC) # K$ tot res investment
IBRC1 <- get_cansim_vector("v62144023",firstqtrC) # C$ res investment - construction, part 1
IBRC2 <- get_cansim_vector("v62144024",firstqtrC) # C$ res investment - construction, part 2
KEXTdataQ$IBRC <- round((IBRC1$VALUE+IBRC2$VALUE)/(IBR$VALUE/KIBR$VALUE),0) # K$ construction
KEXTdataQ$IBRT <- round(KIBR$VALUE-KEXTdataQ$IBRC,0) # K$ Ownership transfer costs
# K$ non-bus inventories
KV <- get_cansim_vector("v62306882",firstqtrC) # K$ total inventories
KVB <- get_cansim_vector("v62306883",firstqtrC) # K$ bus inventories
KEXTdataQ$VNB <- KV$VALUE-KVB$VALUE # K$ Non-business inventories


#------------------------------------------------------------------------------
# The following is not currently being used
# Fiscal Monitor (updated from January 2013 to July 2022)
GFCF_FiscMonitor <- data.frame(Date=seq.Date(firstMonFM,lastmonF,"month"),
  GFCF=c(5926,6100,11640,5134,5686,6110,5313,6447,6346,6757,6483,6425,
         5887,7062,10336,5282,5942,6233,6016,6254,6224,6162,6461,6109,
         6538,6352,12136,5421,6128,5611,6222,7393,5893,6684,6213,6100,
         7097,6472,10551,5609,6199,6029,6125,6259,6443,6299,6238,6324,
         6405,6219,10856,5729,5819,5832,6665,4504,5868,6315,5892,6469,
         5932,6147,10451,5301,5745,6013,6287,7599,6418,6284,6609,7129,
         6164,6625,11943,5795,6277,6725,6474,7682,6927,6876,7186,6959,
         6760,6642,12495,6112,6702,6992,7100,7315,7200,7439,7331,7308,
         7213,6640,11312,6212,7057,7263,7109,8137,6949,7760,7869,7550,
         8639,7670,13250,6181,6625,6721,7524,9084,7891,8869,7940,8451,
         8062,7902,12705,7787,6930,7693,7933,6952,7611,7522,7715,8333,
         7496,9348,16719,7401,7514,8145,8568,9365,8417,7788,8161,9129,
         9045,12757,19332,7145,8445,8461,8450,10185,9156)) # August 2022 is 10185, Sept is a guess
tmp <- SEASADJ(GFCF_FiscMonitor$GFCF,2010,12)  
#GFCF_FiscMonitor$GFCFSA <- c(tmp,NA) # Needed if these data not yet available for latest month
GFCF_FiscMonitor <- filter(GFCF_FiscMonitor,Date>=firstmon & Date<=lastmonF)
#EXTdata$GFCF <- GFCF_FiscMonitor$GFCFSA
