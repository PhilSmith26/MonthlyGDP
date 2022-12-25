# Write GDP results to spreadsheets
# December 11, 2022; December 23, 2022

setwd("/Users/philipsmith/Documents/R/GDP/GDP_IandE_monthly/Results_Oct_2022")

pkgs <- c("cansim","tidyverse","lubridate","anytime","ggthemes","ggpubr",
          "gridExtra","grid","gtable","reshape2","ggrepel","pracma",
          "seasonal","FactoMineR","RcppBDT","forecast","profvis",
          "RColorBrewer","WriteXLS","gdata","scales","ggpubr","ggtext",
          "tibble","simplecolors","stringr","ggpubfigs","gt","patchwork",
          "paletteer","geomtextpath","readxl")
suppressPackageStartupMessages({
  inst <- lapply(pkgs,library,character.only=TRUE)
})

FileName <- "Dec232022to102022"

C <- readRDS(paste0("GDP_C_monthly_2012_done",FileName,".rds"))
K <- readRDS(paste0("GDP_K_monthly_2012_done",FileName,".rds"))
P <- readRDS(paste0("GDP_P_monthly_2012_done",FileName,".rds"))

nams <- c("Gross domestic product at market prices",
"Final consumption expenditure",
"Household final consumption expenditure",
"Household final consumption expenditure on goods",
"Household final consumption expenditure on durable goods",
"Household final consumption expenditure on semi-durable goods",
"Household final consumption expenditure on non-durable goods",
"Household final consumption expenditure on services",
"NPISH final consumption expenditure",
"Government final consumption expenditure",   
"Federal government final consumption expenditure",
"Non-federal government final consumption expenditure",
"Gross fixed capital formation",
"Business investment",
"Business residential investment",
"Business residential construction investment",
"Business residential transfer costs investment",
"Business plant and equipment investment",
"Business non-residential construction investment",
"Business machinery and equipment investment",
"Business intellectual property investment",  
"NPISH investment",
"Government investment",
"Inventory investment",
"Business inventory investment",
"Business non-farm inventory investment",
"Business farm inventory investment",
"Non-business investment",
"Exports of goods and services",
"Exports of goods",
"Exports of services",
"Imports of goods and services",
"Imports of goods",
"Imports of services",
"Statistical discrepancy",
"Final domestic demand")

colnames(C) <- c("Date",nams)
colnames(P) <- c("Date",nams)
colnames(K) <- c("Date",nams)
library(writexl)
write_xlsx(C,"GDP_C.xlsx")
write_xlsx(P,"GDP_P.xlsx")
write_xlsx(K,"GDP_K.xlsx")

