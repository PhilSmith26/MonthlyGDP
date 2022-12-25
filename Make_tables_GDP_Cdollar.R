# Create tables and charts for nominal GDP expenditure-based
# October 24, 2022; December 16, 2022; December 23, 2022

#------------------------------------------------------------------------------
# (1) Make a table with the levels results
tbl0 <- filter(Edata,Date>=as.Date("2022-01-01"))
tbl1 <- as.data.frame(t(tbl0))
tbl1 <- tbl1[2:nrow(tbl1),]
nm <- character()
for (i in 1:nrow(tbl1)) {
  nm[i] <- GDP[[GDPnames[i]]]$betterName
}
tbl1$names <- nm
tbl1 <- select(tbl1,names,everything())
tbl1 <- mutate(tbl1,across(2:ncol(tbl1),as.numeric))

colls <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10") # Jan 2022 to October 2022
LastMonth <- "October"
MyTitle <- paste0("**Gross domestic product, expenditure-based<br>January 2022 to ",
      LastMonth," 2022<br>Millions of dollars**")

tabName <- "October2022Dec232022"

gt_tbl1 <- gt(data=tbl1)
gt_tbl1 <- gt_tbl1 %>% 
  tab_options(table.font.size=12,container.width = 1450) %>%
  tab_header(
    title=md(html(MyTitle))
    #subtitle=md(html("1-month percentage change"))
  ) %>% 
  tab_source_note(
    source_note=md(html("@PhilSmith26"))
  ) %>% 
  cols_align(
    align=c("left"),
    columns=c(`names`)
  ) %>%
  fmt_number(
    columns=all_of(colls),
    decimals=0,
    use_seps=TRUE
  ) %>%
  cols_label(
    `names`="",
    `V1`=md("**Jan<br>2021**"),
    `V2`=md("**Feb<br>2022**"),
    `V3`=md("**Mar<br>2022**"),
    `V4`=md("**Apr<br>2022**"),
    `V5`=md("**May<br>2022**"),
    `V6`=md("**Jun<br>2022**"),
    `V7`=md("**Jul<br>2022**"),
    `V8`=md("**Aug<br>2022**"),
    `V9`=md("**Sep<br>2022**"),
   `V10`=md("**Oct<br>2022**")
  ) %>%
  data_color(
    columns=all_of(colls),
    colors=scales::col_numeric(
      palette=c(
        "#E3F2FD"),
      domain=c(-10000000.0,1000000000.0),
    )
  ) %>%
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
    locations = cells_title()
  ) %>%
  tab_style( # column label style
    style = list(
      cell_fill(color = "#E3F2FD"), # "bisque3" or gray66" or "darkslategray2"
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=c(names,all_of(colls)))
  ) %>%
opt_row_striping(row_striping = TRUE) %>%
opt_vertical_padding(scale = 0.25) %>%
tab_footnote(
    footnote = paste0("NPISH is the non-profit institutions serving households ",
       "sector. Estimates in the table are derived using time disaggregation ",
       "methods credited to F.T. Denton, P.A. Cholette, G.C. Chow and A.-L. Lin, ",
       "autoregressive moving-average models and data from Statistics ",
       "Canada tables 36-10-0104-01, 34-10-0175-01, 16-10-0048-01, ",
       "16-10-0047-01, 20-10-0076-01, 12-10-0121-01, 12-10-0144-01. ",
       "The methodology is explained here: rpubs.com/PhilSmith26/979533. ",
       "See also rpubs.com/PhilSmith26/941490 for details on ",
       "how the estimates for household final consumption expenditure are ",
       "calculated. Produced ",Sys.time()),
    locations = cells_title()
  )
gt_tbl1
gtsave(gt_tbl1,paste0("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022/GDP_table1_",tabName,".png"))

#------------------------------------------------------------------------------
# (2) Make a table with the percentage change results
# Assumes Date in column 1
TransposeTbl <- function(df) {
  df1 <- as.data.frame(t(df))
  df1 <- df1[2:nrow(df1),]
  nm <- character()
  for (i in 1:nrow(df1)) { nm[i] <- GDP[[GDPnames[i]]]$betterName }
  df1$names <- nm
  df1 <- select(df1,names,everything())
  df1 <- mutate(df1,across(2:ncol(df1),as.numeric))
}
EQrtlydata <- filter(EQrtlydata, Date<=as.Date("2022-07-01")) #OK (quarterly)
tblQl <- TransposeTbl(EQrtlydata) #OK (quarterly)
tblQp <- mutate(EQrtlydata,across(2:ncol(EQrtlydata),function (x) round(100*(x/lag(x)-1),1))) #OK (quarterly)
tblQp <- tblQp[2:nrow(tblQp),] # remove line of NAs at the start
tblQp <- TransposeTbl(tblQp) #OK (quarterly)
tbl0 <- mutate(Edata,across(2:ncol(Edata),function (x) round(100*(x/lag(x)-1),1))) #OK (monthly)
tbl0 <- filter(tbl0,Date>=as.Date("2022-01-01")) #OK (monthly)
#tbl0 <- tbl0[,2:ncol(tbl0)] # remove Date column
tbl1 <- TransposeTbl(tbl0) # This is the required eight monthly % changes
tbl1$Q2l <- tblQl[,ncol(tblQl)] # This is a column of quarterly levels from the most recent quarter
tbl1$Q1p <- tblQp[,(ncol(tblQp)-1)] # This is the quarterly % change for latest Q minus one
tbl1$Q2p <- tblQp[,ncol(tblQp)] # This is the quarterly % change for latest Q
tbl1 <- select(tbl1,names,Q2l,Q1p,Q2p,everything())
# Now calculate 100*deltaV/GDP and 100*deltaSD/GDP
tblQp1 <- mutate(EQrtlydata,across(2:ncol(EQrtlydata),function (x) round(100*((x-lag(x))/EQrtlydata$GDP),1)))
tblQp1 <- tblQp1[2:nrow(tblQp1),] # remove line of NAs at the start
tblQp1 <- TransposeTbl(tblQp1)
tbl01 <- mutate(Edata,across(2:ncol(Edata),function (x) round(100*((x-lag(x))/Edata$GDP),1)))
tbl01 <- filter(tbl01,Date>=as.Date("2022-01-01"))
tbl11 <- TransposeTbl(tbl01)
tbl11$Q2l <- tblQl[,ncol(tblQl)]
tbl11$Q1p <- tblQp1[,(ncol(tblQp1)-1)]
tbl11$Q2p <- tblQp1[,ncol(tblQp1)]
tbl2 <- select(tbl11,names,Q2l,Q1p,Q2p,everything())

tbl3 <- tbl1
tbl3[c(24:28,35),] <- tbl2[c(24:28,35),]
nams <- tbl3[,1]
nams[24] <- "Investment in inventories *"
nams[25] <- "Business investment in inventories *"
nams[26] <- "Business investment in non-farm inventories *"
nams[27] <- "Business investment in farm inventories *"
nams[28] <- "Non-business investment in inventories *"
nams[35] <- "Statistical discrepancy *"
tbl3[,1] <- nams

tabName <- "October2022Dec232022"

gt_tbl1 <- gt(data=tbl3)
gt_tbl1 <- gt_tbl1 %>% 
  tab_options(table.font.size=12,container.width = 1350) %>%
  tab_header(
    title=md(html(paste0("**Gross domestic product, expenditure-based, at current prices<br>January 2022 to ",
      LastMonth," 2022<br>Monthly and quarterly percentage change**")))
  ) %>% 
  tab_source_note(
    source_note=md(html("@PhilSmith26"))
  ) %>% 
  cols_align(
    align=c("left"),
    columns=c(`names`)
  ) %>%
  fmt_number(
    columns=Q2l,
    decimals=0,
    use_seps=TRUE
  ) %>%
  fmt_number(
    columns=c(Q1p,Q2p,all_of(colls)),
    decimals=1,
    use_seps=TRUE
  ) %>%
  cols_label(
    `names`="",
    `Q2l`=md("**2022 Q3<br>($ million)**"),
    `Q1p`=md("**2022 Q2<br>(% change)**"),
    `Q2p`=md("**2022 Q3<br>(% change)**"),
    `V1`=md("**Jan<br>2021**"),
    `V2`=md("**Feb<br>2022**"),
    `V3`=md("**Mar<br>2022**"),
    `V4`=md("**Apr<br>2022**"),
    `V5`=md("**May<br>2022**"),
    `V6`=md("**Jun<br>2022**"),
    `V7`=md("**Jul<br>2022**"),
    `V8`=md("**Aug<br>2022**"),
    `V9`=md("**Sep<br>2022**"),
   `V10`=md("**Oct<br>2022**")
  ) %>%
  data_color(
    columns=c(Q2l,Q1p,Q2p,all_of(colls)),
    colors=scales::col_numeric(
      palette=c(
        "#E3F2FD"),
      domain=c(-10000000.0,1000000000.0),
    )
  ) %>%
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
    locations = cells_title()
  ) %>%
  tab_style( # column label style
    style = list(
      cell_fill(color = "#E3F2FD"), # "bisque3" or gray66" or "darkslategray2"
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=c(names,Q2l,Q1p,Q2p,all_of(colls)))
  ) %>%
  cols_width(
    Q1p ~ px(90),
    Q2p ~ px(90)
  ) %>%
  opt_row_striping(row_striping = TRUE) %>%
  opt_vertical_padding(scale = 0.25) %>%
  tab_style(style=cell_borders(sides="right",
    color="black",weight=px(1.5),style="solid"),
    locations=cells_body(columns=c(2,4),rows=1:36)) %>%
  #tbl <- sub_missing(tbl,rows=all_of(c(14,15,27)),missing_text="--")
  tab_source_note(
    source_note = paste0(
       "* Estimates for the change in inventories and the statistical discrepancy are ",
       "the first difference as a percentage of GDP rather than the percentage change. ",
       "NPISH is the non-profit institutions serving households sector. ",
       "Estimates derived using time disaggregation methods ",
       "credited to F.T. Denton, P.A. Cholette, G.C. Chow and A.-L. Lin, ",
       "autoregressive moving-average models and data from Statistics ",
       "Canada tables 36-10-0104-01, 34-10-0175-01, 16-10-0048-01, ",
       "16-10-0047-01, 20-10-0076-01, 12-10-0121-01, 12-10-0144-01. ",
       "The methodology is explained here: rpubs.com/PhilSmith26/979533. ",
       "See also rpubs.com/PhilSmith26/94/490 for details on ",
       "how the estimates for household final consumption expenditure are ",
       "calculated. Produced ",Sys.time())
  )
gt_tbl1
gtsave(gt_tbl1,paste0("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022/GDP_table2_",tabName,".png"))

#------------------------------------------------------------------------------
# (3) Make a chart for nominal GDP
GDPP01 <- mutate(Edata,across(2:ncol(Edata),function (x) round(100*(x/lag(x)-1),1)))
# Make monthly/quarterly series
#qtrser <- vector()
#for (i in 1:nrow(Edata)) {
#  j <- floor((i-1)/3)+1
#  qtrser[i] <- EQdata[j,ncol(EQdata)]
#}
#qtrser <- unlist(qtrser)
df <- data.frame(Date=as.Date(Edata$Date),GDP=Edata$GDP,GDPPC01=GDPP01$GDP)
#  qtrser=qtrser)
sub <- 3300000
div <- 200000
c1 <- ggplot(filter(df,Date>=as.Date("2020-10-01")))+
  geom_line(aes(x=Date,y=(GDP-sub)/div),colour="blue",size=1.5)+
  geom_point(aes(x=Date,y=(GDP-sub)/div),colour="blue",size=2.5)+
  #geom_line(aes(x=Date,y=(qtrser-sub)/div),colour="forestgreen",
  #  size=2.5,linetype="dashed")+
  geom_col(aes(x=Date,y=GDPPC01),fill="red")+
  #geom_line(aes(x=Date,y=MA),colour="black",size=1.5)+
  labs(title=paste0("Gross domestic expenditure at market prices, September 2020 to ",LastMonth," 2022"),
    subtitle=paste0("The statistics are seasonally adjusted at annual rates\n",
      "The red bars are monthly percentage changes (right axis)\n",
      #"The black line is a 13-month centred moving average (right axis)\n",
      "The blue line is the gross domestic expenditure level in millions of dollars (left axis)"),
    caption=paste0("Own calculations using Statistics Canada data. ",Sys.time()," @PhilSmith26"),x="") +
  scale_x_continuous(breaks=c(seq.Date(as.Date("2020-01-01"),
    as.Date("2022-10-01"),by="3 months")),labels=
    c("Jan\n2020","Apr\n2020","Jul\n2020","Oct\n2020",
    "Jan\n2021","Apr\n2021","Jul\n2021","Oct\n2021","Jan\n2022","Apr\n2022",
    "Jul\n2022","Oct\n2022"))+
    scale_y_continuous(
      position="right",breaks=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,3.5),
      name="Monthly % change\n",
      sec.axis=sec_axis(~ . * div + sub,name="Millions of dollars",
        breaks=c(2200000,2300000,2400000,2500000,2600000,2700000,2800000,2900000)))+
  theme(plot.title = element_text(size=16,face="bold")) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  theme(panel.border = element_rect(fill=NA,colour="black"))+
  theme(panel.grid.minor=element_line(colour="lightgrey",size=0.5)) +
  theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
  theme(axis.text.x = element_text(angle=0,hjust=1,size=9)) +
  theme(axis.text.y = element_text(size=12))
c1
ggsave(paste0("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022/MGDP_dual_chart_",tabName,".png"),c1,height=8.5,width=11,dpi=300)

