# Create tables and charts for constant 2012 dollar GDP expenditure-based
# October 26, 2022; November 29, 2022; December 16, 2022; December 23, 2022

# Calculate the number of observations via monnb and mondf
# This function turns a date into a 'monthnumber' relative to an origin
monnb <- function(d) { 
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"));
  y <- lt$year*12 + lt$mon 
} 
# This function computes a month difference as a difference between two monnb's
mondf <- function(d1, d2) {
  y <- monnb(d2) - monnb(d1)
}


C <- readRDS("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022/GDP_C_monthly_2012_doneDec232022to102022.rds")
K <- readRDS("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022/GDP_K_monthly_2012_doneDec232022to102022.rds")
P <- readRDS("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022/GDP_P_monthly_2012_doneDec232022to102022.rds")
#------------------------------------------------------------------------------
# (1) Make a table with the levels results: value, price, volume
tbl01 <- filter(C,Date>=as.Date("2022-07-01")) # the first month displayed in the table
tbl02 <- filter(P,Date>=as.Date("2022-07-01")) # the first month displayed in the table
tbl03 <- filter(K,Date>=as.Date("2022-07-01")) # the first month displayed in the table
tbl11 <- as.data.frame(t(tbl01))
tbl12 <- as.data.frame(t(tbl02))
tbl13 <- as.data.frame(t(tbl03))
df <- cbind(tbl11,tbl12,tbl13)
df <- df[2:nrow(df),]
nms <- rownames(df)
nm <- character()
for (i in 1:nrow(df)) {
  nm[i] <- GDP[[nms[i]]]$betterName
}
df$nms <- nm
colnames(df) <- c("V2022M7","V2022M8","V2022M9","V2022M10",
  "P2022M7","P2022M8","P2022M9","P2022M10",
  "K2022M7","K2022M8","K2022M9","K2022M10","Category")
df <- select(df,Category,everything())
df <- mutate(df,across(2:ncol(df),as.numeric))

tabName <- "October2022Dec232022"
FirstMonth <- "July"
LastMonth <- "October"

gt_tbl1 <- gt(df) %>%
tab_options(table.font.size=12,container.width = 1550) %>%
tab_header(
  title=md(html(paste0("**Gross domestic product, expenditure-based, value, price and volume<br>",
    "Seasonally adjusted at annual rates<br>",FirstMonth," 2022 to ",
    LastMonth," 2022**")))) %>%
tab_source_note(source_note=md(html("@PhilSmith26"))) %>%
cols_align(align=c("left"),columns=c(`Category`)) %>%
fmt_number(columns=all_of(c(2,3,4,5,10,11,12,13)),
    decimals=0,use_seps=TRUE) %>%
fmt_number(columns=all_of(c(6,7,8,9)),decimals=1,use_seps=FALSE) %>%
cols_label(
    `Category`="",
    `V2022M7`=md("**Jul<br>2022**"),
    `V2022M8`=md("**Aug<br>2022**"),
    `V2022M9`=md("**Sep<br>2022**"),
   `V2022M10`=md("**Oct<br>2022**"),
    `P2022M7`=md("**Jul<br>2022**"),
    `P2022M8`=md("**Aug<br>2022**"),
    `P2022M9`=md("**Sep<br>2022**"),
   `P2022M10`=md("**Oct<br>2022**"),
    `K2022M7`=md("**Jul<br>2022**"),
    `K2022M8`=md("**Aug<br>2022**"),
    `K2022M9`=md("**Sep<br>2022**"),
   `K2022M10`=md("**Oct<br>2022**")) %>%
tab_spanner(label="Millions of dollars",
  columns=c(2,3,4,5),id="Cdollars") %>%
tab_spanner(label="Price index, 2012=100",
  columns=c(6,7,8,9),id="Price") %>%
tab_spanner(label="Millions of constant 2012 dollars",
  columns=c(10,11,12,13),id="Kdollars") %>%
data_color(columns=all_of(2:13),
    colors=scales::col_numeric(palette=c("#E3F2FD"),
      domain=c(-10000000.0,1000000000.0))) %>%
tab_style(style = cell_text(indent=pct(2)),
    locations = cells_body(
      columns = 1,
      rows = c(2,13,24,29,32,35))
  ) %>%
  tab_style(style = cell_text(indent=pct(4)),
    locations = cells_body(
      columns = 1,
      rows = c(3,9,10,14,22,23,25,28,30,31,33,34))
  ) %>%
  tab_style(style = cell_text(indent=pct(6)),
    locations = cells_body(
      columns = 1,
      rows = c(4,8,11,12,15,18,21,26,27))
  ) %>%
  tab_style(style = cell_text(indent=pct(8)),
    locations = cells_body(
      columns = 1,
      rows = c(5,6,7,16,17,19,20))
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=c(Category,all_of(2:13)))) %>%
tab_style(
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
      ),
    locations = cells_title()
  ) %>%
tab_style(
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
      ),
    locations = cells_column_spanners(spanners = everything())
  ) %>%
opt_row_striping(row_striping = TRUE) %>%
opt_vertical_padding(scale = 0.25) %>%
tab_style(style=cell_borders(sides="right",color="black",
  weight=px(1.5),style="solid"),
  locations=cells_body(columns=c(5,9),rows=1:36)) %>%
tab_footnote(
    footnote = paste0("NPISH is the non-profit institutions serving households ",
       "sector. Estimates are derived using time disaggregation methods ",
       "credited to F.T. Denton, P.A. Cholette, G.C. Chow and A.-L. Lin, plus ",
       "autoregressive moving-average models and data from Statistics ",
       "Canada tables 12-10-0121-01, 12-10-0128-01, 12-10-0144-01, 16-10-0048-01, ",
       "20-10-0076-01, 34-10-0175-01, 36-10-0104-01, 36-0108-01, 36-10-0123-01, ",
       "Finance Canada's Fiscal Monitor and the ",
       "Canadian Real Estate Association. The methodology is explained here: ",
       "rpubs.com/PhilSmith26/979533. See also ",
       "https://rpubs.com/PhilSmith26/941490 for details on ",
       "how the estimates for household final consumption expenditure are ",
       "calculated. Produced ",Sys.time()),
    locations = cells_title())
gt_tbl1
gtsave(gt_tbl1,paste0("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022/GDP_table3_",tabName,".png"))
   
#------------------------------------------------------------------------------
# (2) Make a table with the percentage change results: value, price, volume
spc01 <- mutate(C,across(2:ncol(C),function(x) y <- round(100*(x-lag(x))/C$GDP,1)))
spc02 <- mutate(P,across(2:ncol(C),function(x) y <- "NA"))
spc03 <- mutate(K,across(2:ncol(C),function(x) y <- round(100*(x-lag(x))/K$GDP,1)))
tbl01 <- mutate(C,across(2:ncol(C),function(x) y <- round(100*(x/lag(x)-1),1)))
tbl02 <- mutate(P,across(2:ncol(P),function(x) y <- round(100*(x/lag(x)-1),1)))
tbl03 <- mutate(K,across(2:ncol(K),function(x) y <- round(100*(x/lag(x)-1),1)))
spc01 <- filter(spc01,Date>=as.Date("2022-07-01")) # The first month in the table
spc02 <- filter(spc02,Date>=as.Date("2022-07-01"))
spc03 <- filter(spc03,Date>=as.Date("2022-07-01"))
tbl01 <- filter(tbl01,Date>=as.Date("2022-07-01"))
tbl02 <- filter(tbl02,Date>=as.Date("2022-07-01"))
tbl03 <- filter(tbl03,Date>=as.Date("2022-07-01"))
tbl01 <- cbind(select(tbl01,1:24),select(spc01,25:29),select(tbl01,30:35),select(spc01,36),select(tbl01,37))
tbl02 <- cbind(select(tbl02,1:24),select(spc02,25:29),select(tbl02,30:35),select(spc02,36),select(tbl02,37))
tbl03 <- cbind(select(tbl03,1:24),select(spc03,25:29),select(tbl03,30:35),select(spc03,36),select(tbl03,37))
tbl11 <- as.data.frame(t(tbl01))
tbl12 <- as.data.frame(t(tbl02))
tbl13 <- as.data.frame(t(tbl03))
df <- cbind(tbl11,tbl12,tbl13)
df <- df[2:nrow(df),]
nms <- rownames(df)
nm <- character()
for (i in 1:nrow(df)) {
  nm[i] <- GDP[[nms[i]]]$betterName
}
df$nms <- nm
colnames(df) <- c("V2022M7","V2022M8","V2022M9","V2022M10",
  "P2022M7","P2022M8","P2022M9","P2022M10",
  "K2022M7","K2022M8","K2022M9","K2022M10","Category")
df <- select(df,Category,everything())
df <- mutate(df,across(2:ncol(df),as.numeric))
df[24,1] <- "Investment in inventories *"
df[25,1] <- "Business investment in inventories *"
df[26,1] <- "Business investment in non-farm inventories *"
df[27,1] <- "Business investment in farm inventories *"
df[28,1] <- "Non-business investment in inventories *"
df[35,1] <- "Statistical discrepancy *"

tabName <- "October2022Dec232022"
LastMonth <- "October"

gt_tbl1 <- gt(data=df) %>%
  tab_options(table.font.size=12,container.width = 1250) %>%
  tab_header(
    title=md(html(paste0("**Gross domestic product, expenditure-based, ",
    "value, price and volume<br>Seasonally adjusted<br>One-month ",
    "percentage change<br>",FirstMonth," 2022 to ",LastMonth," 2022**")))) %>%
  #tab_source_note(source_note=md(html("@PhilSmith26"))) %>%
  cols_align(align=c("left"),columns=c(`Category`)) %>%
  sub_missing(columns=2:13,rows=c(24:28,35),missing_text="---") %>%
  fmt_number(columns=all_of(2:13),
    decimals=1,
    use_seps=TRUE) %>%
  cols_label(
    `Category`="",
    `V2022M7`=md("**Jul<br>2022**"),
    `V2022M8`=md("**Aug<br>2022**"),
    `V2022M9`=md("**Sep<br>2022**"),
   `V2022M10`=md("**Oct<br>2022**"),
    `P2022M7`=md("**Jul<br>2022**"),
    `P2022M8`=md("**Aug<br>2022**"),
    `P2022M9`=md("**Sep<br>2022**"),
   `P2022M10`=md("**Oct<br>2022**"),
    `K2022M7`=md("**Jul<br>2022**"),
    `K2022M8`=md("**Aug<br>2022**"),
    `K2022M9`=md("**Sep<br>2022**"),
   `K2022M10`=md("**Oct<br>2022**")) %>%
  tab_spanner(label="Millions of dollars",
    columns=c(2,3,4,5),id="Cdollars") %>%
  tab_spanner(label="Price index, 2012=100",
    columns=c(6,7,8,9),id="Price") %>%
  tab_spanner(label="Millions of constant 2012 dollars",
    columns=c(10,11,12,13),id="Kdollars") %>%
  data_color(columns=all_of(2:13),
    colors=scales::col_numeric(
      palette=c(
        "#E3F2FD"),
      domain=c(-10000000.0,1000000000.0)
    )) %>%
  tab_style(style = cell_text(indent=pct(2)),
    locations = cells_body(
      columns = 1,
      rows = c(2,13,24,29,32,35))
  ) %>%
  tab_style(style = cell_text(indent=pct(4)),
    locations = cells_body(
      columns = 1,
      rows = c(3,9,10,14,22,23,25,28,30,31,33,34))
  ) %>%
  tab_style(style = cell_text(indent=pct(6)),
    locations = cells_body(
      columns = 1,
      rows = c(4,8,11,12,15,18,21,26,27))
  ) %>%
  tab_style(style = cell_text(indent=pct(8)),
    locations = cells_body(
      columns = 1,
      rows = c(5,6,7,16,17,19,20))
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=c(Category,all_of(2:13)))) %>%
  tab_style( # format for row with NAs
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "normal")
    ),
    locations = cells_body(rows=c(24:28,35),
      columns=c(all_of(2:13)))) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
      ),
    locations = cells_title()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
      ),
    locations = cells_column_spanners(spanners = everything())
  ) %>%
  opt_row_striping(row_striping = TRUE) %>%
  opt_vertical_padding(scale = 0.25) %>%
  tab_style(style=cell_borders(sides="right",color="black",
    weight=px(1.5),style="solid"),
    locations=cells_body(columns=c(5,9),rows=1:36)) %>%
  tab_source_note(source_note=md(html(paste0("* Change in dollars or in constant 2012 dollars ",
       "expressed as a percentage of GDP, rather than percentage change. ",
       "NPISH is the non-profit institutions serving households ",
       "sector. Estimates are derived using time disaggregation methods ",
       "credited to F.T. Denton, P.A. Cholette, G.C. Chow and A.-L. Lin, plus ",
       "autoregressive moving-average models and data from Statistics ",
       "Canada tables 12-10-0121-01, 12-10-0128-01, 12-10-0144-01, 16-10-0048-01, ",
       "20-10-0076-01, 34-10-0175-01, 36-10-0104-01, 36-0108-01, 36-10-0123-01, ",
       "Finance Canada's Fiscal Monitor and the ",
       "Canadian Real Estate Association. The methodology is explained here: ",
       "rpubs.com/PhilSmith26/979533. See also: ",
       "https://rpubs.com/PhilSmith26/941490 for details on ",
       "how the estimates for household final consumption expenditure are ",
       "calculated. Produced ",Sys.time(),"   @PhilSmith26"))))
  #tab_footnote(
  #  footnote = paste0("* Change in dollars or in constant 2012 dollars ",
  #     "expressed as a percentage of GDP, rather than percentage change. ",
  #     "NPISH is the non-profit institutions serving households ",
  #     "sector. Estimates are derived using time disaggregation methods ",
  #     "credited to F.T. Denton, P.A. Cholette, G.C. Chow and A.-L. Lin, plus ",
  #     "autoregressive moving-average models and data from Statistics ",
  #     "Canada tables 12-10-0121-01, 12-10-0128-01, 12-10-0144-01, 16-10-0048-01, ",
  #     "20-10-0076-01, 34-10-0175-01, 36-10-0104-01, 36-0108-01, 36-10-0123-01, ",
  #     "Finance Canada's Fiscal Monitor and the ",
  #     "Canadian Real Estate Association. See also ",
  #     "https://rpubs.com/PhilSmith26/941490 for details on ",
  #     "how the estimates for household final consumption expenditure are ",
  #     "calculated. Produced ",Sys.time()),
  #  locations = cells_title())
gt_tbl1
gtsave(gt_tbl1,paste0("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022/GDP_table4_",tabName,".png"))
      
#------------------------------------------------------------------------------
# (3) Make a chart of GDP volume
Flash <- 1.0 # This is zero % growth
GDPP01 <- mutate(K,across(2:ncol(K),function (x) round(100*(x/lag(x)-1),1)))
# Make monthly/quarterly series
qtrser <- vector()
#for (i in 1:nrow(K)) {
#  j <- floor((i-1)/3)+1
#  qtrser[i] <- EQdata[j,ncol(EQdata)]
#}
#qtrser <- unlist(qtrser)
IDX <- function(x) {y <- 100*x/x[1]}
GDPbyInd <- get_cansim_vector("v65201210")
GDPbyInd$GDPbyIndPC01 <- 100*(GDPbyInd$VALUE/lag(GDPbyInd$VALUE)-1)
GDPbyInd <- filter(GDPbyInd,Date>=as.Date("2012-01-01"))
LastDate1 <- GDPbyInd$Date[nrow(GDPbyInd)]
LastDate2 <- K$Date[nrow(K)]
(diff <- mondf(LastDate1,LastDate2))
if (diff<0) {
  for (i in 1:diff) {
    newrow <- GDPbyInd[nrow(GDPbyInd),]
    if (i==1) { newrow[2] <- GDPbyInd[nrow(GDPbyInd),2]*Flash } # Flash VALUE
    else { newrow[2] <- NA } # VALUE
    newrow[11] <- K[nrow(GDPbyInd)+i,1] # Date
    GDPbyInd <- rbind(GDPbyInd,newrow)
  }
}
df <- data.frame(Date=as.Date(K$Date),GDP=K$GDP,GDPPC01=GDPP01$GDP,
  GDPbyInd=GDPbyInd$VALUE,GDPbyIndPC01=GDPbyInd$GDPbyIndPC01)
df$GDPbyInd <- as.numeric(df$GDPbyInd)
df$GDPbyIndPC01 <- as.numeric(df$GDPbyIndPC01)
df <- filter(df,Date>=as.Date("2012-09-01"))
c1 <- ggplot(df)+
  geom_line(aes(x=Date,y=IDX(GDP)),colour="red",size=1.5)+
  geom_line(aes(x=Date,y=IDX(GDPbyInd)),colour="blue",size=1.5)+
  labs(title=paste0("Real gross domestic product, January 2012 to ",LastMonth," 2022"),
    subtitle=paste0("The red line is monthly expenditure-based GDP at constant 2012 prices\n",
      "The blue line is the monthly real industry-based GDP"),
    caption=paste0("Own calculations using Statistics Canada data. @PhilSmith26"),x="") +
  scale_x_continuous(breaks=c(seq.Date(as.Date("2012-01-01"),
    as.Date("2022-09-01"),by="year")),labels=
    c("Jan\n2012","Jan\n2013","Jan\n2014","Jan\n2015","Jan\n2016","Jan\n2017","Jan\n2018",
      "Jan\n2019","Jan\n2020","Jan\n2021","Jan\n2022"))+
    scale_y_continuous(
      position="right",
      name="Indexes, January 2012 = 100\n")+
  theme(plot.title = element_text(size=16,face="bold")) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  theme(panel.border = element_rect(fill=NA,colour="black"))+
  theme(panel.grid.minor=element_line(colour="lightgrey",size=0.5)) +
  theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
  theme(axis.text.x = element_text(angle=0,hjust=1,size=9)) +
  theme(axis.text.y = element_text(size=12))
c1
tabName <- "October2022Dec232022"
ggsave(paste0("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022/MGDP_GDPcompar_chart_",tabName,".png"),c1,height=8.5,width=11,dpi=300)

PC01 <- function(x) {100*(x/lag(x)-1)}
df <- data.frame(Date=K$Date,GDP=K$GDP,GDPPC01=PC01(K$GDP))
df <- filter(df,Date>=as.Date("2020-07-01"))

sub <- 2300000
div <- 80000
c1 <- ggplot(df)+
  geom_line(aes(x=Date,y=(GDP-sub)/div),colour="blue",size=1.5)+
  geom_point(aes(x=Date,y=(GDP-sub)/div),colour="blue",size=2.5)+
  geom_col(aes(x=Date,y=GDPPC01),fill="red")+
  labs(title=paste0("Real gross domestic product, September 2020 to ",LastMonth," 2022"),
    subtitle=paste0("The red bars are monthly percentage changes (right axis)\n",
      #"The black line is a 13-month centred moving average (right axis)\n",
      "The blue line is the real level in millions of constant 2012 dollars (left axis)"),
    caption=paste0("Own calculations using Statistics Canada data. ",Sys.time()," @PhilSmith26"),x="") +
  scale_x_continuous(breaks=c(seq.Date(as.Date("2020-01-01"),
    as.Date("2022-10-01"),by="3 months")),labels=
    c("Jan\n2020","Apr\n2020","Jul\n2020","Oct\n2020",
    "Jan\n2021","Apr\n2021","Jul\n2021","Oct\n2021","Jan\n2022","Apr\n2022",
    "Jul\n2022","Oct\n2022"))+
    scale_y_continuous(
      position="right",breaks=c(-1,-0.5,0,0.5,1,1.5,2,2.5),
      name="Monthly % change\n",
      sec.axis=sec_axis(~ . * div + sub,name="Millions of constant 2012 dollars",
        breaks=c(2050000,2100000,2150000,2200000,2250000),label=comma))+
  theme(plot.title = element_text(size=16,face="bold")) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  theme(panel.border = element_rect(fill=NA,colour="black"))+
  theme(panel.grid.minor=element_line(colour="lightgrey",size=0.5)) +
  theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
  theme(axis.text.x = element_text(angle=0,hjust=1,size=9)) +
  theme(axis.text.y = element_text(size=12))
c1
tabName <- "October2022Dec232022"
ggsave(paste0("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022/MGDP_GDPvol_chart_",tabName,".png"),c1,height=8.5,width=11,dpi=300)

PC12 <- function(x) {100*(x/lag(x,12)-1)}
CPI <- get_cansim_vector("v41690973","2012-01-01")
CPIsa <- get_cansim_vector("v41690914","2012-01-01")
LastDate1 <- CPI$Date[nrow(CPI)]
LastDate2 <- P$Date[nrow(P)]
(diff <- mondf(LastDate2,LastDate1))
if (diff>0) {
  CPI <- filter(CPI,Date<=P$Date[nrow(P)])
  CPIsa <- filter(CPIsa,Date<=P$Date[nrow(P)])
}
df <- data.frame(Date=P$Date,
  P=P$GDP,PPC01=PC01(P$GDP),PPC12=PC12(P$GDP),
  PHH=P$HHFC,PHHPC01=PC01(P$HHFC),PHHPC12=PC12(P$HHFC),
  CPI=CPI$VALUE,CPIPC01=PC01(CPI$VALUE),CPIPC12=PC12(CPI$VALUE),
  CPIsa=CPIsa$VALUE,CPIsaPC01=PC01(CPIsa$VALUE),CPIsaPC12=PC12(CPIsa$VALUE))
df <- filter(df,Date>=as.Date("2020-07-01"))

sub <- 150
div <- 10
c1 <- ggplot(df)+
  geom_line(aes(x=Date,y=(P-sub)/div),colour="blue",size=1.5)+
  geom_point(aes(x=Date,y=(P-sub)/div),colour="blue",size=2.5)+
  geom_col(aes(x=Date,y=PPC01),fill="red")+
  labs(title=paste0("Gross domestic product Paasche price index, July 2020 to ",LastMonth," 2022"),
    subtitle=paste0("The red bars are monthly percentage changes (right axis)\n",
      #"The black line is a 13-month centred moving average (right axis)\n",
      "The blue line is the price level, 2012 = 100 (left axis)"),
    caption=paste0("Own calculations using Statistics Canada data. ",Sys.time()," @PhilSmith26"),x="") +
  scale_x_continuous(breaks=c(seq.Date(as.Date("2020-07-01"),
    as.Date("2022-10-01"),by="3 months")),labels=
    c("Jul\n2020","Oct\n2020",
    "Jan\n2021","Apr\n2021","Jul\n2021","Oct\n2021","Jan\n2022","Apr\n2022",
    "Jul\n2022","Oct\n2022"))+
  scale_y_continuous(
    position="right",breaks=c(-1,-0.5,0,0.5,1,1.5,2,2.5),
    name="Monthly % change\n",
    sec.axis=sec_axis(~ . * div + sub,name="2012 = 100",
      breaks=c(95,100,105,110,115,120,125,130),label=comma))+
  theme(plot.title = element_text(size=16,face="bold")) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  theme(panel.border = element_rect(fill=NA,colour="black"))+
  theme(panel.grid.minor=element_line(colour="lightgrey",size=0.5)) +
  theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
  theme(axis.text.x = element_text(angle=0,hjust=1,size=9)) +
  theme(axis.text.y = element_text(size=12))
c1
tabName <- "October2022Dec232022"
ggsave(paste0("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022/MGDP_PGDP_chart_",tabName,".png"),c1,height=8.5,width=11,dpi=300)

c1 <- ggplot(df)+
  geom_line(aes(x=Date,y=PHHPC12),colour="blue",size=1.4)+
  geom_line(aes(x=Date,y=CPIPC12),colour="red",size=1.4)+
  labs(title=paste0("Household expenditure implicit price index vs CPI, July 2020 to ",LastMonth," 2022"),
    subtitle=paste0("The blue line is the household expenditure implicit price index\n",
      "The red line is the consumer price index"),
    caption=paste0("Own calculations using Statistics Canada data. ",Sys.time()," @PhilSmith26"),x="") +
  scale_x_continuous(breaks=c(seq.Date(as.Date("2020-07-01"),
    as.Date("2022-10-01"),by="3 months")),labels=
    c("Jul\n2020","Oct\n2020",
    "Jan\n2021","Apr\n2021","Jul\n2021","Oct\n2021","Jan\n2022","Apr\n2022",
    "Jul\n2022","Oct\n2022"))+
  scale_y_continuous(
    position="right",breaks=c(0,1,2,3,4,5,6,7,8),
    name="12-month % change\n")+
  theme(plot.title = element_text(size=16,face="bold")) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  theme(panel.border = element_rect(fill=NA,colour="black"))+
  theme(panel.grid.minor=element_line(colour="lightgrey",size=0.5)) +
  theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
  theme(axis.text.x = element_text(angle=0,hjust=1,size=9)) +
  theme(axis.text.y = element_text(size=12))
c1
tabName <- "October2022Dec232022"
ggsave(paste0("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022/MGDP_CPIcompar_chart_",tabName,".png"),c1,height=8.5,width=11,dpi=300)






#-------------------------------------------------------------------------------
ggplot(P)+
  geom_col(aes(Date,100*(HHFC/lag(HHFC,12)-1)),fill="darkgoldenrod",
    colour="black",size=0.3)+
  labs(title="Monthly implicit price indexes, 12-month percentage change",
    y="12-month % change\n")+
  scale_y_continuous(position="right")+
  geom_hline(yintercept=0)+
  geom_hline(yintercept=2,linetype="dashed")
ggplot(P)+
  geom_col(aes(Date,100*(HHFC/lag(HHFC,1)-1)),fill="darkgoldenrod",
    colour="black",size=0.3)+
  labs(title="Monthly implicit price indexes, 1-month percentage change",
    y="1-month % change\n")+
  scale_y_continuous(position="right")+
  geom_hline(yintercept=0)+
  geom_hline(yintercept=100*(1.02^(1/12)-1),linetype="dashed")
ggplot(K)+
  geom_col(aes(Date,100*(HHFC/lag(HHFC,12)-1)),fill="darkgoldenrod",
    colour="black",size=0.3)+
  labs(title=paste0("Monthly consumer expenditure at constant 2012 prices, ",
    "12-month percentage change"),y="12-month % change\n")+
  scale_y_continuous(position="right")+
  geom_hline(yintercept=0)
ggplot(K)+
  geom_col(aes(Date,100*(HHFC/lag(HHFC,1)-1)),fill="darkgoldenrod",
    colour="black",size=0.3)+
  labs(title=paste0("Monthly consumer expenditure at constant 2012 prices, ",
    "1-month percentage change"),y="1-month % change\n")+
  scale_y_continuous(position="right")+
  geom_hline(yintercept=0)
ggplot(C)+
  geom_col(aes(Date,100*(HHFC/lag(HHFC,12)-1)),fill="darkgoldenrod",
    colour="black",size=0.3)+
  labs(title=paste0("Monthly consumer expenditure at current prices, ",
    "12-month percentage change"),y="12-month % change\n")+
  scale_y_continuous(position="right")+
  geom_hline(yintercept=0)
ggplot(C)+
  geom_col(aes(Date,100*(HHFC/lag(HHFC,1)-1)),fill="darkgoldenrod",
    colour="black",size=0.3)+
  labs(title=paste0("Monthly consumer expenditure at current prices, ",
    "1-month percentage change"),y="1-month % change\n")+
  scale_y_continuous(position="right")+
  geom_hline(yintercept=0)
