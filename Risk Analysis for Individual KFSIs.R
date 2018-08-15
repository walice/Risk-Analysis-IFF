# Risk Analysis for Individual KFSIs
# Alice Lepissier

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Data Preparation
# VIE for Individual KFSIs
# .. Jurisdiction-level scores
# .... Calculate Total of flow/stock per reporter per year
# .... Calculate Vulnerabilities per reporter per year
# .... Calculate Intensities per reporter per year
# .... Calculate Exposures per reporter per year
# .. Region-level scores
# .... Calculate Total of flow/stock per region per year
# .... Calculate Vulnerabilities per region per year
# .... Calculate Intensities per region per year
# .... Calculate Exposures per region per year
# .. Income group-level scores
# .... Calculate Total of flow/stock per income group per year
# .... Calculate Vulnerabilities per income group per year
# .... Calculate Intensities per income group per year
# .... Calculate Exposures per income group per year
# .. Check zero values in measures due to summing over NAs
# .... For Totals
# .... For Vulnerabilities
# .... For Intensities
# .... For Exposures
# .. Remove infinite values
# Average Measures Graphs
# .. Calculate average measures across years per reporter
# .. Extract conduits
# .. Plot jurisdiction-level scores
# .... For conduits
# .... By region, including conduits
# .... By region, excluding conduits
# .... By income group, including conduits
# .... By income group, excluding conduits
# .... Export
# .. Calculate average measures across years per region
# .... Plot region-level scores
# .... Export
# .. Calculate average measures across years per income group
# .... Plot income group-level scores
# .... Export
# .. Housekeeping
# Time Series Graphs
# .. For regions
# .... Plot
# .... Export
# .. For income groups
# .... Plot
# .... Export
# .. Housekeeping
# Export results



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

setwd("C:/cloudstorage/googledrive/Projects/Tax Justice Network/Consultancy 2 - summer 18/Risk-based IFF/") # Alice work
#setwd("D:/Google Drive/Projects/Tax Justice Network/Consultancy 2 - summer 18/Risk-based IFF/") # Alice laptop
library(plyr) # Must load before dplyr
library(dplyr)
library(ggplot2)
library(gmodels)
library(RColorBrewer)
library(rebus)
library(reshape2)
library(scales)
library(tidyr)
library(WDI)
library(wesanderson)
library(xlsx)



## ## ## ## ## ## ## ## ## ## ##
# DATA PREPARATION          ####
## ## ## ## ## ## ## ## ## ## ##

#source("Scripts/Data Preparation.R")
load("Data/panel.RData")



## ## ## ## ## ## ## ## ## ## ##
# VIE FOR INDIVIDUAL KFSIs  ####
## ## ## ## ## ## ## ## ## ## ##

panelSJ_KFSI <- subset(panel, pSS != FALSE)

vars <- c("DIdI", "DII", "DIdO", "DIO",
          "PIA", "PIL", "PIdL",
          "Export", "Import",
          "Claims", "Liabilities")


# .. Average Company Ownership KFSIs
panelSJ_KFSI$pKFSI3.6 <- rowMeans(subset(panelSJ_KFSI,
                                         select = c(pKFSI3, pKFSI6)),
                                  na.rm = T)


# .. Jurisdiction-level scores ####
KFSIn <- c(rep("1", 2),
           rep("18", 2),
           rep("3.6", 6))
KFSIv <- c(rep(c("Claims", "Liabilities"), 2),
           "Export", "Import",
           "PIA", "PIdL",
           "DII", "DIdO")

V <- paste0("V", KFSIn, KFSIv)
I <- paste0("I", KFSIn, KFSIv)

# .... Calculate Total of flow/stock per reporter per year ####
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(reporter.ISO, year) %>%
  mutate_at(.vars = vars,
            .fun = funs(Tot = sum(., na.rm = T))) %>%
  ungroup()
names(panelSJ_KFSI) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ_KFSI))


# .... Calculate Vulnerabilities per reporter per year ####
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(reporter.ISO, year) %>%
  mutate_at(.vars = c("Claims", "Liabilities"),
            .fun = funs(V1 = sum((. * pKFSI1)/sum(., na.rm = T), na.rm = T),
                        V18 = sum((. * pKFSI18)/sum(., na.rm = T), na.rm = T))) %>%
  ungroup()
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(reporter.ISO, year) %>%
  mutate_at(.vars = c("Export", "Import", "PIA", "PIdL", "DII", "DIdO"),
            .fun = funs(V3.6 = sum((. * pKFSI1)/sum(., na.rm = T), na.rm = T))) %>%
  ungroup()
names(panelSJ_KFSI) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ_KFSI))


# .... Calculate Intensities per reporter per year ####
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(reporter.ISO, year) %>%
  mutate_at(.vars = paste("Tot", c("Claims", "Liabilities"), sep = ""),
            .fun = funs(I1 = abs( . / rGDP),
                        I18 = abs( . / rGDP))) %>%
  ungroup()
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(reporter.ISO, year) %>%
  mutate_at(.vars = paste("Tot", c("Export", "Import", "PIA", "PIdL", "DII", "DIdO"), sep = ""),
            .fun = funs(I3.6 = abs( . / rGDP))) %>%
  ungroup()
names(panelSJ_KFSI) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ_KFSI))
names(panelSJ_KFSI) <- gsub("I1Tot", "I1", names(panelSJ_KFSI))
names(panelSJ_KFSI) <- gsub("I18Tot", "I18", names(panelSJ_KFSI))
names(panelSJ_KFSI) <- gsub("I3.6Tot", "I3.6", names(panelSJ_KFSI))


# .... Calculate Exposures per reporter per year ####
n <- ncol(panelSJ_KFSI)
for (var in 1:length(KFSIv)){
  panelSJ_KFSI[, n + var] <- panelSJ_KFSI[[V[var]]] * panelSJ_KFSI[[I[var]]]
  colnames(panelSJ_KFSI)[n + var] <- paste0("E", KFSIn[var], KFSIv[var])
}
rm(n, var)


# .. Region-level scores ####
wrV <- paste0("wrV", KFSIn, KFSIv)
wrI <- paste0("wrI", KFSIn, KFSIv)

# ... Calculate Total of flow/stock per region per year ####
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(rRegion, year) %>%
  mutate_at(.vars = vars,
            .fun = funs(wrTot = sum(., na.rm = T))) %>%
  ungroup()
names(panelSJ_KFSI) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ_KFSI))


# .... Calculate Vulnerabilities per region per year ####
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(rRegion, year) %>%
  mutate_at(.vars = c("Claims", "Liabilities"),
            .fun = funs(wrV1 = sum((. * pKFSI1)/sum(., na.rm = T), na.rm = T),
                        wrV18 = sum((. * pKFSI18)/sum(., na.rm = T), na.rm = T))) %>%
  ungroup()
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(rRegion, year) %>%
  mutate_at(.vars = c("Export", "Import", "PIA", "PIdL", "DII", "DIdO"),
            .fun = funs(wrV3.6 = sum((. * pKFSI1)/sum(., na.rm = T), na.rm = T))) %>%
  ungroup()
names(panelSJ_KFSI) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ_KFSI))


# .... Calculate Intensities per region per year ####
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(rRegion, year) %>%
  mutate(wrrGDP = sum(rGDP, na.rm = T)) %>%
  ungroup()

panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(rRegion, year) %>%
  mutate_at(.vars = paste("wrTot", c("Claims", "Liabilities"), sep = ""),
            .fun = funs(wrI1 = abs( . / wrrGDP),
                        wrI18 = abs( . / wrrGDP))) %>%
  ungroup()
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(rRegion, year) %>%
  mutate_at(.vars = paste("wrTot", c("Export", "Import", "PIA", "PIdL", "DII", "DIdO"), sep = ""),
            .fun = funs(wrI3.6 = abs( . / wrrGDP))) %>%
  ungroup()
names(panelSJ_KFSI) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ_KFSI))
names(panelSJ_KFSI) <- gsub("wrI1wrTot", "wrI1", names(panelSJ_KFSI))
names(panelSJ_KFSI) <- gsub("wrI18wrTot", "wrI18", names(panelSJ_KFSI))
names(panelSJ_KFSI) <- gsub("wrI3.6wrTot", "wrI3.6", names(panelSJ_KFSI))


# .... Calculate Exposures per region per year ####
n <- ncol(panelSJ_KFSI)
for (var in 1:length(KFSIv)){
  panelSJ_KFSI[, n + var] <- panelSJ_KFSI[[wrV[var]]] * panelSJ_KFSI[[wrI[var]]]
  colnames(panelSJ_KFSI)[n + var] <- paste0("wrE", KFSIn[var], KFSIv[var])
}
rm(n, var)


# .. Income group-level scores ####
wiV <- paste0("wiV", KFSIn, KFSIv)
wiI <- paste0("wiI", KFSIn, KFSIv)

# ... Calculate Total of flow/stock per income group per year ####
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(rIncome, year) %>%
  mutate_at(.vars = vars,
            .fun = funs(wiTot = sum(., na.rm = T))) %>%
  ungroup()
names(panelSJ_KFSI) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ_KFSI))


# .... Calculate Vulnerabilities per income group per year ####
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(rIncome, year) %>%
  mutate_at(.vars = c("Claims", "Liabilities"),
            .fun = funs(wiV1 = sum((. * pKFSI1)/sum(., na.rm = T), na.rm = T),
                        wiV18 = sum((. * pKFSI18)/sum(., na.rm = T), na.rm = T))) %>%
  ungroup()
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(rIncome, year) %>%
  mutate_at(.vars = c("Export", "Import", "PIA", "PIdL", "DII", "DIdO"),
            .fun = funs(wiV3.6 = sum((. * pKFSI1)/sum(., na.rm = T), na.rm = T))) %>%
  ungroup()
names(panelSJ_KFSI) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ_KFSI))


# .... Calculate Intensities per income group per year ####
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(rIncome, year) %>%
  mutate(wirGDP = sum(rGDP, na.rm = T)) %>%
  ungroup()

panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(rIncome, year) %>%
  mutate_at(.vars = paste("wiTot", c("Claims", "Liabilities"), sep = ""),
            .fun = funs(wiI1 = abs( . / wirGDP),
                        wiI18 = abs( . /wirGDP))) %>%
  ungroup()
panelSJ_KFSI <- panelSJ_KFSI %>%
  group_by(rIncome, year) %>%
  mutate_at(.vars = paste("wiTot", c("Export", "Import", "PIA", "PIdL", "DII", "DIdO"), sep = ""),
            .fun = funs(wiI3.6 = abs( . / wirGDP))) %>%
  ungroup()
names(panelSJ_KFSI) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ_KFSI))
names(panelSJ_KFSI) <- gsub("wiI1wiTot", "wiI1", names(panelSJ_KFSI))
names(panelSJ_KFSI) <- gsub("wiI18wiTot", "wiI18", names(panelSJ_KFSI))
names(panelSJ_KFSI) <- gsub("wiI3.6wiTot", "wiI3.6", names(panelSJ_KFSI))


# .... Calculate Exposures per income group per year ####
n <- ncol(panelSJ_KFSI)
for (var in 1:length(KFSIv)){
  panelSJ_KFSI[, n + var] <- panelSJ_KFSI[[wiV[var]]] * panelSJ_KFSI[[wiI[var]]]
  colnames(panelSJ_KFSI)[n + var] <- paste0("wiE", KFSIn[var], KFSIv[var])
}
rm(n, var)
rm(V, I, wrV, wrI, wiV, wiI, vars, KFSIn, KFSIv)
rm(panel)


# .. Check zero values in measures due to summing over NAs ####
summary(panelSJ_KFSI)

# .... For Totals ####
grep("Tot", names(panelSJ_KFSI))
for (c in 11:19){
  z <- which(panelSJ_KFSI[, c+62] == 0) # increment is -40 if panel w/o KFSIs
  zeroTot <- panelSJ_KFSI[z, ]
  print(unique(zeroTot[, c]))
}
zeroTot <- subset(panelSJ_KFSI, TotClaims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, TotLiabilities == 0)
unique(zeroTot$Liabilities)

grep("wrTot", names(panelSJ_KFSI))
for (c in 11:19){
  z <- which(panelSJ_KFSI[, c+103] == 0)
  zeroTot <- panelSJ_KFSI[z, ]
  print(unique(zeroTot[, c]))
}
zeroTot <- subset(panelSJ_KFSI, wrTotClaims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wrTotLiabilities == 0)
unique(zeroTot$Liabilities)

grep("wiTot", names(panelSJ_KFSI))
for (c in 11:19){
  z <- which(panelSJ_KFSI[, c+145] == 0)
  zeroTot <- panelSJ_KFSI[z, ]
  print(unique(zeroTot[, c]))
}
zeroTot <- subset(panelSJ_KFSI, wiTotClaims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wiTotLiabilities == 0)
unique(zeroTot$Liabilities)

# All the 0 values in the totals for flows/stocks are due to summing only NAs with na.rm = T.
# We can convert them back to NAs.
# na.rm = T is needed when summing otherwise a single NA value will return NAs (even though there may be valid other values).
cols <- grep("Tot", names(panelSJ_KFSI))
panelSJ_KFSI[, cols][panelSJ_KFSI[, cols] == 0] <- NA
rm(zeroTot, c, cols, z)


# .... For Vulnerabilities ####
zeroTot <- subset(panelSJ_KFSI, V1Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, V1Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, V18Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, V18Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, V3.6Export == 0)
unique(zeroTot$Export)
zeroTot <- subset(panelSJ_KFSI, V3.6Import == 0)
unique(zeroTot$Import)
zeroTot <- subset(panelSJ_KFSI, V3.6PIA == 0)
unique(zeroTot$PIA)
zeroTot <- subset(panelSJ_KFSI, V3.6PIdL == 0)
unique(zeroTot$PIdL)
zeroTot <- subset(panelSJ_KFSI, V3.6DII == 0)
unique(zeroTot$DII)
zeroTot <- subset(panelSJ_KFSI, V3.6DIdO == 0)
unique(zeroTot$DIdO)

zeroTot <- subset(panelSJ_KFSI, wrV1Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wrV1Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, wrV18Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wrV18Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, wrV3.6Export == 0)
unique(zeroTot$Export)
zeroTot <- subset(panelSJ_KFSI, wrV3.6Import == 0)
unique(zeroTot$Import)
zeroTot <- subset(panelSJ_KFSI, wrV3.6PIA == 0)
unique(zeroTot$PIA)
zeroTot <- subset(panelSJ_KFSI, wrV3.6PIdL == 0)
unique(zeroTot$PIdL)
zeroTot <- subset(panelSJ_KFSI, wrV3.6DII == 0)
unique(zeroTot$DII)
zeroTot <- subset(panelSJ_KFSI, wrV3.6DIdO == 0)
unique(zeroTot$DIdO)

zeroTot <- subset(panelSJ_KFSI, wiV1Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wiV1Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, wiV18Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wiV18Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, wiV3.6Export == 0)
unique(zeroTot$Export)
zeroTot <- subset(panelSJ_KFSI, wiV3.6Import == 0)
unique(zeroTot$Import)
zeroTot <- subset(panelSJ_KFSI, wiV3.6PIA == 0)
unique(zeroTot$PIA)
zeroTot <- subset(panelSJ_KFSI, wiV3.6PIdL == 0)
unique(zeroTot$PIdL)
zeroTot <- subset(panelSJ_KFSI, wiV3.6DII == 0)
unique(zeroTot$DII)
zeroTot <- subset(panelSJ_KFSI, wiV3.6DIdO == 0)
unique(zeroTot$DIdO)

# All the 0 values in the Vulnerabilities for flows/stocks are due to summing only NAs with na.rm = T.
# We can convert them back to NAs.
# na.rm = T is needed when summing otherwise a single NA value will return NAs (even though there may be valid other values).
cols <- grep("V", names(panelSJ_KFSI))
panelSJ_KFSI[, cols][panelSJ_KFSI[, cols] == 0] <- NA
rm(zeroTot, cols)


# .... For Intensities ####
zeroTot <- subset(panelSJ_KFSI, I1Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, I1Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, I18Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, I18Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, I3.6Export == 0)
unique(zeroTot$Export)
zeroTot <- subset(panelSJ_KFSI, I3.6Import == 0)
unique(zeroTot$Import)
zeroTot <- subset(panelSJ_KFSI, I3.6PIA == 0)
unique(zeroTot$PIA)
zeroTot <- subset(panelSJ_KFSI, I3.6PIdL == 0)
unique(zeroTot$PIdL)
zeroTot <- subset(panelSJ_KFSI, I3.6DII == 0)
unique(zeroTot$DII)
zeroTot <- subset(panelSJ_KFSI, I3.6DIdO == 0)
unique(zeroTot$DIdO)

zeroTot <- subset(panelSJ_KFSI, wrI1Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wrI1Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, wrI18Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wrI18Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, wrI3.6Export == 0)
unique(zeroTot$Export)
zeroTot <- subset(panelSJ_KFSI, wrI3.6Import == 0)
unique(zeroTot$Import)
zeroTot <- subset(panelSJ_KFSI, wrI3.6PIA == 0)
unique(zeroTot$PIA)
zeroTot <- subset(panelSJ_KFSI, wrI3.6PIdL == 0)
unique(zeroTot$PIdL)
zeroTot <- subset(panelSJ_KFSI, wrI3.6DII == 0)
unique(zeroTot$DII)
zeroTot <- subset(panelSJ_KFSI, wrI3.6DIdO == 0)
unique(zeroTot$DIdO)

zeroTot <- subset(panelSJ_KFSI, wiI1Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wiI1Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, wiI18Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wiI18Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, wiI3.6Export == 0)
unique(zeroTot$Export)
zeroTot <- subset(panelSJ_KFSI, wiI3.6Import == 0)
unique(zeroTot$Import)
zeroTot <- subset(panelSJ_KFSI, wiI3.6PIA == 0)
unique(zeroTot$PIA)
zeroTot <- subset(panelSJ_KFSI, wiI3.6PIdL == 0)
unique(zeroTot$PIdL)
zeroTot <- subset(panelSJ_KFSI, wiI3.6DII == 0)
unique(zeroTot$DII)
zeroTot <- subset(panelSJ_KFSI, wiI3.6DIdO == 0)
unique(zeroTot$DIdO)

# All the 0 values in the Intensities for flows/stocks are due to summing only NAs with na.rm = T.
# We can convert them back to NAs.
# na.rm = T is needed when summing otherwise a single NA value will return NAs (even though there may be valid other values).
colnames(panelSJ_KFSI)
cols <- c(94:103,136:145,178:187)
panelSJ_KFSI[, cols][panelSJ_KFSI[, cols] == 0] <- NA
rm(zeroTot, cols)


# .... For Exposures ####
zeroTot <- subset(panelSJ_KFSI, E1Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, E1Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, E18Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, E18Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, E3.6Export == 0)
unique(zeroTot$Export)
zeroTot <- subset(panelSJ_KFSI, E3.6Import == 0)
unique(zeroTot$Import)
zeroTot <- subset(panelSJ_KFSI, E3.6PIA == 0)
unique(zeroTot$PIA)
zeroTot <- subset(panelSJ_KFSI, E3.6PIdL == 0)
unique(zeroTot$PIdL)
zeroTot <- subset(panelSJ_KFSI, E3.6DII == 0)
unique(zeroTot$DII)
zeroTot <- subset(panelSJ_KFSI, E3.6DIdO == 0)
unique(zeroTot$DIdO)

zeroTot <- subset(panelSJ_KFSI, wrE1Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wrE1Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, wrE18Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wrE18Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, wrE3.6Export == 0)
unique(zeroTot$Export)
zeroTot <- subset(panelSJ_KFSI, wrE3.6Import == 0)
unique(zeroTot$Import)
zeroTot <- subset(panelSJ_KFSI, wrE3.6PIA == 0)
unique(zeroTot$PIA)
zeroTot <- subset(panelSJ_KFSI, wrE3.6PIdL == 0)
unique(zeroTot$PIdL)
zeroTot <- subset(panelSJ_KFSI, wrE3.6DII == 0)
unique(zeroTot$DII)
zeroTot <- subset(panelSJ_KFSI, wrE3.6DIdO == 0)
unique(zeroTot$DIdO)

zeroTot <- subset(panelSJ_KFSI, wiE1Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wiE1Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, wiE18Claims == 0)
unique(zeroTot$Claims)
zeroTot <- subset(panelSJ_KFSI, wiE18Liabilities == 0)
unique(zeroTot$Liabilities)
zeroTot <- subset(panelSJ_KFSI, wiE3.6Export == 0)
unique(zeroTot$Export)
zeroTot <- subset(panelSJ_KFSI, wiE3.6Import == 0)
unique(zeroTot$Import)
zeroTot <- subset(panelSJ_KFSI, wiE3.6PIA == 0)
unique(zeroTot$PIA)
zeroTot <- subset(panelSJ_KFSI, wiE3.6PIdL == 0)
unique(zeroTot$PIdL)
zeroTot <- subset(panelSJ_KFSI, wiE3.6DII == 0)
unique(zeroTot$DII)
zeroTot <- subset(panelSJ_KFSI, wiE3.6DIdO == 0)
unique(zeroTot$DIdO)

# All the 0 values in the Exposures for flows/stocks are due to summing only NAs with na.rm = T.
# We can convert them back to NAs.
# na.rm = T is needed when summing otherwise a single NA value will return NAs (even though there may be valid other values).
colnames(panelSJ_KFSI)
cols <- c(104:113,146:155,188:197)
panelSJ_KFSI[, cols][panelSJ_KFSI[, cols] == 0] <- NA
rm(zeroTot, cols)


# .. Remove infinite values ####
panelSJ_KFSI[panelSJ_KFSI == "Inf"] <- NA

panelSJ_KFSI <- panelSJ_KFSI %>% 
  select(id, reporter, reporter.ISO, partner, partner.ISO, year, everything()) %>%
  arrange(id)
save(panelSJ_KFSI, file = "Data/panelSJ_KFSI.RData")
write.csv(panelSJ_KFSI, "Results/panelSJ_KFSI.csv", row.names = FALSE)



## ## ## ## ## ## ## ## ## ## ##
# AVERAGE MEASURES GRAPHS   ####
## ## ## ## ## ## ## ## ## ## ##

KFSI <- c("KFSI1", "KFSI18", 
          rep("KFSIs 3 and 6", 3))
KFSI.label <- c("Banking Secrecy",
                "Automatic Exchange of Information",
                rep("Company Ownership Registration and Publication", 3))         

measure <- c("xV", "xI", "xE")
wrmeasure <- c("xwrV", "xwrI", "xwrE")
wimeasure <- c("xwiV", "xwiI", "xwiE")
measure.label <- c("Vulnerability", "Intensity", "Exposure")

region <- c("af", "am", "as", "eu", "oc", "nr")
regionno <- c("afno", "amno", "asno", "euno", "ocno", "nrno")
region.label <- c("Africa", "Americas", "Asia", "Europe", "Oceania", "No Region")

incomegroup <- c("HIC", "UMC", "LMC", "LIC", "ni")
incomegroupno <- c("HICno", "UMCno", "LMCno", "LICno", "nino")
incomegroup.label <- c("HIC", "UMC", "LMC", "LIC", "No Income Group")

flowstock <- c("Banking Positions", "Banking Positions", "Trade",
               "Portfolio Investment", "Direct Investment")

vars <- list()
vars$BankingPositions1 <- c("1Claims", "1Liabilities")
vars$BankingPositions2 <- c("18Claims", "18Liabilities")
vars$Trade <- c("3.6Export", "3.6Import")
vars$PortInvestment <- c("3.6PIA", "3.6PIdL")
vars$DirectInvestment <- c("3.6DII", "3.6DIdO")

var.labels <- list()
var.labels$BankingPositions1 <- c("Claims", "Liabilities")
var.labels$BankingPositions2 <- c("Claims", "Liabilities")
var.labels$Trade <- c("Exports", "Imports")
var.labels$PortInvestment <- c("Assets", "Liabilities (derived)")
var.labels$DirectInvestment <- c("Inward", "Outward (derived)")

cols <- list()
cols$BankingPositions1 <- c("#899DA4", "#C93312")
cols$BankingPositions2 <- c("#899DA4", "#C93312")
cols$Trade <- c("#FAD510", "#800080")
cols$PortInvestment <- c("#00A08A", "#F98400")
cols$DirectInvestment <- c("#7294D4", "#FD6467")


# .. Calculate average measures across years per reporter ####
measures <- c("V1Claims", "V1Liabilities",
              "I1Claims", "I1Liabilities",
              "E1Claims", "E1Liabilities",
              "V18Claims", "V18Liabilities",
              "I18Claims", "I18Liabilities",
              "E18Claims", "E18Liabilities",
              "V3.6Export", "V3.6Import",
              "I3.6Export", "I3.6Import",
              "E3.6Export", "E3.6Import",
              "V3.6PIA", "V3.6PIdL",
              "I3.6PIA", "I3.6PIdL",
              "E3.6PIA", "E3.6PIdL",
              "V3.6DII", "V3.6DIdO",
              "I3.6DII", "I3.6DIdO",
              "E3.6DII", "E3.6DIdO")

yraverage <- panelSJ_KFSI %>% 
  group_by(reporter) %>%
  distinct(year, .keep_all = TRUE) %>%
  mutate_at(.vars = measures,
            .fun = funs(x = mean(., na.rm = T))) %>%
  ungroup()
names(yraverage) <- sub("^(.*)_(.*)$", "\\2\\1", names(yraverage))

yraverage <- yraverage %>%
  select(reporter, rRegion, rIncome, starts_with("x")) %>%
  distinct(reporter, .keep_all = TRUE) 


# .. Extract conduits ####
choose.cut <- "c90"

pctiles <- apply(yraverage[, 4:ncol(yraverage)], 2, quantile, probs = c(0.75, 0.8, 0.9, 0.95, 0.99), na.rm = TRUE)
pctiles <- as.data.frame(t(pctiles))
pctiles$variable <- row.names(pctiles)

yraverage <- melt(yraverage, 
                  id.vars = c("reporter", "rRegion", "rIncome"),
                  measure.vars = c("xV1Claims", "xV1Liabilities",
                                   "xI1Claims", "xI1Liabilities",
                                   "xE1Claims", "xE1Liabilities",
                                   "xV18Claims", "xV18Liabilities",
                                   "xI18Claims", "xI18Liabilities",
                                   "xE18Claims", "xE18Liabilities",
                                   "xV3.6Export", "xV3.6Import",
                                   "xI3.6Export", "xI3.6Import",
                                   "xE3.6Export", "xE3.6Import",
                                   "xV3.6PIA", "xV3.6PIdL",
                                   "xI3.6PIA", "xI3.6PIdL",
                                   "xE3.6PIA", "xE3.6PIdL",
                                   "xV3.6DII", "xV3.6DIdO",
                                   "xI3.6DII", "xI3.6DIdO",
                                   "xE3.6DII", "xE3.6DIdO"))
yraverage <- subset(yraverage, !is.na(value) & !is.infinite(value))

yraverage <- merge(yraverage, pctiles,
                   by = "variable",
                   all.x = TRUE)

colnames(yraverage)[colnames(yraverage) == "75%"] <- "c75"
colnames(yraverage)[colnames(yraverage) == "80%"] <- "c80"
colnames(yraverage)[colnames(yraverage) == "90%"] <- "c90"
colnames(yraverage)[colnames(yraverage) == "95%"] <- "c95"
colnames(yraverage)[colnames(yraverage) == "99%"] <- "c99"

conduits <- subset(yraverage, value > eval(as.name(paste(choose.cut))))

af <- subset(yraverage, rRegion == "Africa")
am <- subset(yraverage, rRegion == "Americas")
as <- subset(yraverage, rRegion == "Asia")
eu <- subset(yraverage, rRegion == "Europe")
oc <- subset(yraverage, rRegion == "Oceania")
nr <- subset(yraverage, rRegion == "")

HIC <- subset(yraverage, rIncome == "HIC")
UMC <- subset(yraverage, rIncome == "UMC")
LMC <- subset(yraverage, rIncome == "LMC")
LIC <- subset(yraverage, rIncome == "LIC")
ni <- subset(yraverage, rIncome == "")

afno <- subset(yraverage, rRegion == "Africa" & value <= eval(as.name(paste(choose.cut))))
amno <- subset(yraverage, rRegion == "Americas" & value <= eval(as.name(paste(choose.cut))))
asno <- subset(yraverage, rRegion == "Asia" & value <= eval(as.name(paste(choose.cut))))
euno <- subset(yraverage, rRegion == "Europe" & value <= eval(as.name(paste(choose.cut))))
ocno <- subset(yraverage, rRegion == "Oceania" & value <= eval(as.name(paste(choose.cut))))
nrno <- subset(yraverage, rRegion == "" & value <= eval(as.name(paste(choose.cut))))

HICno <- subset(yraverage, rIncome == "HIC" & value <= eval(as.name(paste(choose.cut))))
UMCno <- subset(yraverage, rIncome == "UMC" & value <= eval(as.name(paste(choose.cut))))
LMCno <- subset(yraverage, rIncome == "LMC" & value <= eval(as.name(paste(choose.cut))))
LICno <- subset(yraverage, rIncome == "LIC" & value <= eval(as.name(paste(choose.cut))))
nino <- subset(yraverage, rIncome == "" & value <= eval(as.name(paste(choose.cut))))


# .. Plot jurisdiction-level scores ####

# .... For conduits ####
for (m in 1:length(measure)){
  for (f in 1:length(flowstock)){
    g <- ggplot(conduits %>% filter((variable == paste0(measure[m], vars[[f]][1]) |
                                       variable == paste0(measure[m], vars[[f]][2])) &
                                      value != 0) %>%
                  distinct(reporter, variable, .keep_all = TRUE),
                aes(x = reorder(reporter, value, sum), y = value, fill = variable)) +
      geom_col() + coord_flip() +
      ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in Conduits"),
              subtitle = paste0(KFSI[f],": ", KFSI.label[f])) +
      xlab("Reporting country") + ylab(paste0(measure.label[m], " Score")) +
      guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
      scale_fill_manual(labels = c(var.labels[[f]][1], var.labels[[f]][2]),
                        values = c(cols[[f]][1], cols[[f]][2])) +
      scale_y_continuous(labels = comma)
    ggsave(g,
           file = paste0("Figures/Individual KFSIs/Jurisdiction scores/Conduits/", measure[m], "_", names(vars)[f], "_Conduits_", KFSI[f],".pdf"),
           width = 6, height = 5, units = "in")
  }
}


# .... By region, including conduits ####
for (m in 1:length(measure)){
  for (r in 1:length(region)){
    for (f in 1:length(flowstock)){
      g <- ggplot(get(region[r]) %>% filter((variable == paste0(measure[m], vars[[f]][1]) |
                                               variable == paste0(measure[m], vars[[f]][2])) &
                                              value != 0) %>%
                    distinct(reporter, variable, .keep_all = TRUE),
                  aes(x = reorder(reporter, value, sum), y = value, fill = variable)) +
        geom_col() + coord_flip() +
        ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in ", region.label[r]),
                subtitle = paste0(KFSI[f],": ", KFSI.label[f])) +
        xlab("Reporting country") + ylab(paste0(measure.label[m], " Score")) +
        guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
        scale_fill_manual(labels = c(var.labels[[f]][1], var.labels[[f]][2]),
                          values = c(cols[[f]][1], cols[[f]][2])) +
        scale_y_continuous(labels = comma)
      ggsave(g,
             file = paste0("Figures/Individual KFSIs/Jurisdiction scores/By region/", measure[m], "_", names(vars)[f], "_", region.label[r], "_", KFSI[f], ".pdf"),
             width = 6, height = 5, units = "in")
    }
  }
}


# .... By region, excluding conduits ####
for (m in 1:length(measure)){
  for (r in 1:length(regionno)){
    for (f in 1:length(flowstock)){
      g <- ggplot(get(regionno[r]) %>% filter((variable == paste0(measure[m], vars[[f]][1]) |
                                                 variable == paste0(measure[m], vars[[f]][2])) &
                                                value != 0) %>%
                    distinct(reporter, variable, .keep_all = TRUE),
                  aes(x = reorder(reporter, value, sum), y = value, fill = variable)) +
        geom_col() + coord_flip() +
        ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in ", region.label[r]),
                subtitle = paste0(KFSI[f],": ", KFSI.label[f])) +
        xlab("Reporting country") + ylab(paste0(measure.label[m], " Score")) +
        guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
        scale_fill_manual(labels = c(var.labels[[f]][1], var.labels[[f]][2]),
                          values = c(cols[[f]][1], cols[[f]][2])) +
        scale_y_continuous(labels = comma)
      ggsave(g,
             file = paste0("Figures/Individual KFSIs/Jurisdiction scores/By region/", measure[m], "_", names(vars)[f], "_", region.label[r], "_No Conduits_", KFSI[f], ".pdf"),
             width = 6, height = 5, units = "in")
    }
  }
}


# .... By income group, including conduits ####
for (m in 1:length(measure)){
  for (i in 1:length(incomegroup)){
    for (f in 1:length(flowstock)){
      g <- ggplot(get(incomegroup[i]) %>% filter((variable == paste0(measure[m], vars[[f]][1]) |
                                                    variable == paste0(measure[m], vars[[f]][2])) &
                                                   value != 0) %>%
                    distinct(reporter, variable, .keep_all = TRUE),
                  aes(x = reorder(reporter, value, sum), y = value, fill = variable)) +
        geom_col() + coord_flip() +
        ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in ", incomegroup.label[i]),
                subtitle = paste0(KFSI[f],": ", KFSI.label[f])) +
        xlab("Reporting country") + ylab(paste0(measure.label[m], " Score")) +
        guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
        scale_fill_manual(labels = c(var.labels[[f]][1], var.labels[[f]][2]),
                          values = c(cols[[f]][1], cols[[f]][2])) +
        scale_y_continuous(labels = comma)
      ggsave(g,
             file = paste0("Figures/Individual KFSIs/Jurisdiction scores/By income group/", measure[m], "_", names(vars)[f], "_", incomegroup.label[i], "_", KFSI[f], ".pdf"),
             width = 6, height = 5, units = "in")
    }
  }
}


# .... By income group, excluding conduits ####
for (m in 1:length(measure)){
  for (i in 1:length(incomegroupno)){
    for (f in 1:length(flowstock)){
      g <- ggplot(get(incomegroupno[i]) %>% filter((variable == paste0(measure[m], vars[[f]][1]) |
                                                      variable == paste0(measure[m], vars[[f]][2])) &
                                                     value != 0) %>%
                    distinct(reporter, variable, .keep_all = TRUE),
                  aes(x = reorder(reporter, value, sum), y = value, fill = variable)) +
        geom_col() + coord_flip() +
        ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in ", incomegroup.label[i]),
                subtitle = paste0(KFSI[f],": ", KFSI.label[f])) +
        xlab("Reporting country") + ylab(paste0(measure.label[m], " Score")) +
        guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
        scale_fill_manual(labels = c(var.labels[[f]][1], var.labels[[f]][2]),
                          values = c(cols[[f]][1], cols[[f]][2])) +
        scale_y_continuous(labels = comma)
      ggsave(g,
             file = paste0("Figures/Individual KFSIs/Jurisdiction scores/By income group/", measure[m], "_", names(vars)[f], "_", incomegroup.label[i], "_No Conduits_", KFSI[f], ".pdf"),
             width = 6, height = 5, units = "in")
    }
  }
}


# .... Export ####
yraverage <- yraverage[order(yraverage$variable, yraverage$reporter), ]
write.csv(yraverage, "Results/VIE averages_for jurisdictions_KFSI.csv", row.names = FALSE)


# .. Calculate average measures across years per region ####
measures <- c("wrV1Claims", "wrV1Liabilities",
              "wrI1Claims", "wrI1Liabilities",
              "wrE1Claims", "wrE1Liabilities",
              "wrV18Claims", "wrV18Liabilities",
              "wrI18Claims", "wrI18Liabilities",
              "wrE18Claims", "wrE18Liabilities",
              "wrV3.6Export", "wrV3.6Import",
              "wrI3.6Export", "wrI3.6Import",
              "wrE3.6Export", "wrE3.6Import",
              "wrV3.6PIA", "wrV3.6PIdL",
              "wrI3.6PIA", "wrI3.6PIdL",
              "wrE3.6PIA", "wrE3.6PIdL",
              "wrV3.6DII", "wrV3.6DIdO",
              "wrI3.6DII", "wrI3.6DIdO",
              "wrE3.6DII", "wrE3.6DIdO")

yraverage <- panelSJ_KFSI %>% 
  group_by(rRegion) %>%
  distinct(year, .keep_all = TRUE) %>%
  mutate_at(.vars = measures,
            .fun = funs(x = mean(., na.rm = T))) %>%
  ungroup()
names(yraverage) <- sub("^(.*)_(.*)$", "\\2\\1", names(yraverage))

yraverage <- yraverage %>%
  select(rRegion, starts_with("xwr")) %>%
  distinct(rRegion, .keep_all = TRUE) 

yraverage <- melt(yraverage, 
                  id.vars = c("rRegion"),
                  measure.vars = c("xwrV1Claims", "xwrV1Liabilities",
                                   "xwrI1Claims", "xwrI1Liabilities",
                                   "xwrE1Claims", "xwrE1Liabilities",
                                   "xwrV18Claims", "xwrV18Liabilities",
                                   "xwrI18Claims", "xwrI18Liabilities",
                                   "xwrE18Claims", "xwrE18Liabilities",
                                   "xwrV3.6Export", "xwrV3.6Import",
                                   "xwrI3.6Export", "xwrI3.6Import",
                                   "xwrE3.6Export", "xwrE3.6Import",
                                   "xwrV3.6PIA", "xwrV3.6PIdL",
                                   "xwrI3.6PIA", "xwrI3.6PIdL",
                                   "xwrE3.6PIA", "xwrE3.6PIdL",
                                   "xwrV3.6DII", "xwrV3.6DIdO",
                                   "xwrI3.6DII", "xwrI3.6DIdO",
                                   "xwrE3.6DII", "xwrE3.6DIdO"))
yraverage <- subset(yraverage, !is.na(value) & !is.infinite(value))


# .... Plot region-level scores ####
for (m in 1:length(wrmeasure)){
  for (f in 1:length(flowstock)){
    g <- ggplot(yraverage %>% filter((variable == paste0(wrmeasure[m], vars[[f]][1]) |
                                        variable == paste0(wrmeasure[m], vars[[f]][2])) &
                                       value != 0 & rRegion != "") %>%
                  distinct(rRegion, variable, .keep_all = TRUE),
                aes(x = reorder(rRegion, value, sum), y = value, fill = variable)) +
      geom_col() + coord_flip() +
      ggtitle(paste0(measure.label[m], " of ", flowstock[f]),
              subtitle = paste0(KFSI[f],": ", KFSI.label[f])) +
      xlab("Region") + ylab(paste0(measure.label[m], " Score")) +
      guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
      scale_fill_manual(labels = c(var.labels[[f]][1], var.labels[[f]][2]),
                        values = c(cols[[f]][1], cols[[f]][2])) +
      scale_y_continuous(labels = comma)
    ggsave(g,
           file = paste0("Figures/Individual KFSIs/Regional scores/", wrmeasure[m], "_", names(vars)[f], "_", KFSI[f], ".pdf"),
           width = 6, height = 5, units = "in")
  }
}


# .... Export ####
yraverage <- yraverage[order(yraverage$variable, yraverage$rRegion), ]
write.csv(yraverage, "Results/VIE averages_for regions_KFSI.csv", row.names = FALSE)


# .. Calculate average measures across years per income group ####
measures <- c("wiV1Claims", "wiV1Liabilities",
              "wiI1Claims", "wiI1Liabilities",
              "wiE1Claims", "wiE1Liabilities",
              "wiV18Claims", "wiV18Liabilities",
              "wiI18Claims", "wiI18Liabilities",
              "wiE18Claims", "wiE18Liabilities",
              "wiV3.6Export", "wiV3.6Import",
              "wiI3.6Export", "wiI3.6Import",
              "wiE3.6Export", "wiE3.6Import",
              "wiV3.6PIA", "wiV3.6PIdL",
              "wiI3.6PIA", "wiI3.6PIdL",
              "wiE3.6PIA", "wiE3.6PIdL",
              "wiV3.6DII", "wiV3.6DIdO",
              "wiI3.6DII", "wiI3.6DIdO",
              "wiE3.6DII", "wiE3.6DIdO")

yraverage <- panelSJ_KFSI %>% 
  group_by(rIncome) %>%
  distinct(year, .keep_all = TRUE) %>%
  mutate_at(.vars = measures,
            .fun = funs(x = mean(., na.rm = T))) %>%
  ungroup()
names(yraverage) <- sub("^(.*)_(.*)$", "\\2\\1", names(yraverage))

yraverage <- yraverage %>%
  select(rIncome, starts_with("xwi")) %>%
  distinct(rIncome, .keep_all = TRUE) 

yraverage <- melt(yraverage, 
                  id.vars = c("rIncome"),
                  measure.vars = c("xwiV1Claims", "xwiV1Liabilities",
                                   "xwiI1Claims", "xwiI1Liabilities",
                                   "xwiE1Claims", "xwiE1Liabilities",
                                   "xwiV18Claims", "xwiV18Liabilities",
                                   "xwiI18Claims", "xwiI18Liabilities",
                                   "xwiE18Claims", "xwiE18Liabilities",
                                   "xwiV3.6Export", "xwiV3.6Import",
                                   "xwiI3.6Export", "xwiI3.6Import",
                                   "xwiE3.6Export", "xwiE3.6Import",
                                   "xwiV3.6PIA", "xwiV3.6PIdL",
                                   "xwiI3.6PIA", "xwiI3.6PIdL",
                                   "xwiE3.6PIA", "xwiE3.6PIdL",
                                   "xwiV3.6DII", "xwiV3.6DIdO",
                                   "xwiI3.6DII", "xwiI3.6DIdO",
                                   "xwiE3.6DII", "xwiE3.6DIdO"))
yraverage <- subset(yraverage, !is.na(value) & !is.infinite(value))


# .... Plot income group-level scores ####
for (m in 1:length(wimeasure)){
  for (f in 1:length(flowstock)){
    g <- ggplot(yraverage %>% filter((variable == paste0(wimeasure[m], vars[[f]][1]) |
                                        variable == paste0(wimeasure[m], vars[[f]][2])) &
                                       value != 0 & rIncome != "") %>%
                  distinct(rIncome, variable, .keep_all = TRUE),
                aes(x = reorder(rIncome, value, sum), y = value, fill = variable)) +
      geom_col() + coord_flip() +
      ggtitle(paste0(measure.label[m], " of ", flowstock[f]),
              subtitle = paste0(KFSI[f],": ", KFSI.label[f])) +
      xlab("Income group") + ylab(paste0(measure.label[m], " Score")) +
      guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
      scale_fill_manual(labels = c(var.labels[[f]][1], var.labels[[f]][2]),
                        values = c(cols[[f]][1], cols[[f]][2])) +
      scale_y_continuous(labels = comma)
    ggsave(g,
           file = paste0("Figures/Individual KFSIs/Income group scores/", wimeasure[m], "_", names(vars)[f], "_", KFSI[f], ".pdf"),
           width = 6, height = 5, units = "in")
  }
}


# .... Export ####
yraverage <- yraverage[order(yraverage$variable, yraverage$rIncome), ]
write.csv(yraverage, "Results/VIE averages_for income groups_KFSI.csv", row.names = FALSE)


# .. Housekeeping ####
rm(cols, f, flowstock, g, i, incomegroup, incomegroup.label,
   m, measure, measures, wrmeasure, wimeasure, measure.label, r, region, region.label,
   var.labels, vars,
   af, am, as, eu, oc, nr, HIC, UMC, LMC, LIC, ni,
   afno, amno, asno, euno, ocno, nrno, HICno, UMCno, LMCno, LICno, nino,
   pctiles, choose.cut, regionno, incomegroupno,
   KFSI, KFSI.label)
rm(yraverage, conduits)



## ## ## ## ## ## ## ## ## ## ##
# TIME SERIES GRAPHS        ####
## ## ## ## ## ## ## ## ## ## ##

# .. For regions ####
timeseries <- panelSJ_KFSI %>%
  distinct(rRegion, year, .keep_all = TRUE) %>%
  select(rRegion, year,
         wrV1Claims:wrV3.6DIdO,
         wrI1Claims:wrI3.6DIdO,
         wrE1Claims:wrE3.6DIdO) %>%
  arrange(rRegion, year)


# .... Calculate average V for each measure in each year ####
timeseries$wrV1Banking <- rowMeans(subset(timeseries,
                                          select = c(wrV1Claims, wrV1Liabilities)),
                                   na.rm = T)
timeseries$wrV18Banking <- rowMeans(subset(timeseries,
                                           select = c(wrV18Claims, wrV18Liabilities)),
                                    na.rm = T)
timeseries$wrV3.6DirectInv <- rowMeans(subset(timeseries,
                                              select = c(wrV3.6DII, wrV3.6DIdO)),
                                       na.rm = T)
timeseries$wrV3.6PortInv <- rowMeans(subset(timeseries,
                                            select = c(wrV3.6PIA, wrV3.6PIdL)),
                                     na.rm = T)
timeseries$wrV3.6Trade <- rowMeans(subset(timeseries,
                                          select = c(wrV3.6Export, wrV3.6Import)),
                                   na.rm = T)


# .... Calculate average I for each measure in each year ####
timeseries$wrI1Banking <- rowMeans(subset(timeseries,
                                          select = c(wrI1Claims, wrI1Liabilities)),
                                   na.rm = T)
timeseries$wrI18Banking <- rowMeans(subset(timeseries,
                                           select = c(wrI18Claims, wrI18Liabilities)),
                                    na.rm = T)
timeseries$wrI3.6DirectInv <- rowMeans(subset(timeseries,
                                              select = c(wrI3.6DII, wrI3.6DIdO)),
                                       na.rm = T)
timeseries$wrI3.6PortInv <- rowMeans(subset(timeseries,
                                            select = c(wrI3.6PIA, wrI3.6PIdL)),
                                     na.rm = T)
timeseries$wrI3.6Trade <- rowMeans(subset(timeseries,
                                          select = c(wrI3.6Export, wrI3.6Import)),
                                   na.rm = T)


# .... Calculate average E for each measure in each year ####
timeseries$wrE1Banking <- rowMeans(subset(timeseries,
                                          select = c(wrE1Claims, wrE1Liabilities)),
                                   na.rm = T)
timeseries$wrE18Banking <- rowMeans(subset(timeseries,
                                           select = c(wrE18Claims, wrE18Liabilities)),
                                    na.rm = T)
timeseries$wrE3.6DirectInv <- rowMeans(subset(timeseries,
                                              select = c(wrE3.6DII, wrE3.6DIdO)),
                                       na.rm = T)
timeseries$wrE3.6PortInv <- rowMeans(subset(timeseries,
                                            select = c(wrE3.6PIA, wrE3.6PIdL)),
                                     na.rm = T)
timeseries$wrE3.6Trade <- rowMeans(subset(timeseries,
                                          select = c(wrE3.6Export, wrE3.6Import)),
                                   na.rm = T)


# .... Plot ####
timeseries <- timeseries %>% select(rRegion:year, wrV1Banking:wrE3.6Trade)
timeseries <- melt(timeseries,
                   id.vars = c("rRegion", "year"),
                   measure.vars = c("wrV1Banking", "wrV18Banking", 
                                    "wrV3.6DirectInv", "wrV3.6PortInv", "wrV3.6Trade",
                                    "wrI1Banking", "wrI18Banking", 
                                    "wrI3.6DirectInv", "wrI3.6PortInv", "wrI3.6Trade",
                                    "wrE1Banking", "wrE18Banking",
                                    "wrE3.6DirectInv", "wrE3.6PortInv", "wrE3.6Trade"))

af <- subset(timeseries, rRegion == "Africa" & value != "Inf")
am <- subset(timeseries, rRegion == "Americas" & value != "Inf")
as <- subset(timeseries, rRegion == "Asia" & value != "Inf")
eu <- subset(timeseries, rRegion == "Europe" & value != "Inf")
oc <- subset(timeseries, rRegion == "Oceania" & value != "Inf")

region <- c("af", "am", "as", "eu", "oc")
region.label <- c("Africa", "Americas", "Asia", "Europe", "Oceania")

# Vulnerability and Exposure
measure <- c("V", "E")
measure.label <- c("Vulnerability", "Exposure")

for (m in 1:length(measure)){
  for (r in 1:length(region)){
    g <- ggplot(get(region[r]) %>% filter(variable == paste0("wr", measure[m], "1Banking") |
                                            variable == paste0("wr", measure[m], "18Banking") |
                                            variable == paste0("wr", measure[m], "3.6DirectInv") |
                                            variable == paste0("wr", measure[m], "3.6PortInv") |
                                            variable == paste0("wr", measure[m], "3.6Trade")),
                aes(x = year, y = value, color = variable)) +
      geom_line(size = 1.5) +
      ggtitle(paste0(measure.label[m], " over time in ", region.label[r]),
              subtitle = "Individual KFSIs") +
      xlab("Year") + ylab(paste0(measure.label[m], " Score")) +
      scale_color_manual(labels = c("Banking Positions, \nKFSI1", "Banking Positions, \nKFSI1",
                                    "Direct Investment, \nKFSIs 3 and 6",
                                    "Portfolio Investment, \nKFSIs 3 and 6",
                                    "Trade, \nKFSIs 3 and 6"),
                         values = wes_palette("Darjeeling1")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks= pretty_breaks())
    ggsave(g,
           file = paste0("Figures/Individual KFSIs/Scores over time/By region/", measure[m], "_", region.label[r], "_KFSIs", ".pdf"),
           width = 6, height = 5, units = "in")
  }
}

# Intensity
measure <- c("I")
measure.label <- c("Intensity")

for (m in 1:length(measure)){
  for (r in 1:length(region)){
    g <- ggplot(get(region[r]) %>% filter(variable == paste0("wr", measure[m], "1Banking") |
                                            variable == paste0("wr", measure[m], "3.6DirectInv") |
                                            variable == paste0("wr", measure[m], "3.6PortInv") |
                                            variable == paste0("wr", measure[m], "3.6Trade")),
                aes(x = year, y = value, color = variable)) +
      geom_line(size = 1.5) +
      ggtitle(paste0(measure.label[m], " over time in ", region.label[r]),
              subtitle = "Individual KFSIs") +
      xlab("Year") + ylab(paste0(measure.label[m], " Score")) +
      scale_color_manual(labels = c("Banking Positions", "Direct Investment",
                                    "Portfolio Investment", "Trade"),
                         values = wes_palette("Chevalier1")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks= pretty_breaks())
    ggsave(g,
           file = paste0("Figures/Individual KFSIs/Scores over time/By region/", measure[m], "_", region.label[r], "_KFSIs", ".pdf"),
           width = 6, height = 5, units = "in")
  }
}


# .... Export ####
timeseries <- timeseries[order(timeseries$variable, timeseries$rRegion, timeseries$year), ]
write.csv(timeseries, "Results/VIE time series_for regions_KFSI.csv", row.names = FALSE)


# .. For income groups ####
timeseries <- panelSJ_KFSI %>% 
  distinct(rIncome, year, .keep_all = TRUE) %>%
  select(rIncome, year,
         wiV1Claims:wiV3.6DIdO,
         wiI1Claims:wiI3.6DIdO,
         wiE1Claims:wiE3.6DIdO) %>%
  arrange(rIncome, year)


# .... Calculate average V for each measure in each year ####
timeseries$wiV1Banking <- rowMeans(subset(timeseries,
                                          select = c(wiV1Claims, wiV1Liabilities)),
                                   na.rm = T)
timeseries$wiV18Banking <- rowMeans(subset(timeseries,
                                           select = c(wiV18Claims, wiV18Liabilities)),
                                    na.rm = T)
timeseries$wiV3.6DirectInv <- rowMeans(subset(timeseries,
                                              select = c(wiV3.6DII, wiV3.6DIdO)),
                                       na.rm = T)
timeseries$wiV3.6PortInv <- rowMeans(subset(timeseries,
                                            select = c(wiV3.6PIA, wiV3.6PIdL)),
                                     na.rm = T)
timeseries$wiV3.6Trade <- rowMeans(subset(timeseries,
                                          select = c(wiV3.6Export, wiV3.6Import)),
                                   na.rm = T)


# .... Calculate average I for each measure in each year ####
timeseries$wiI1Banking <- rowMeans(subset(timeseries,
                                          select = c(wiI1Claims, wiI1Liabilities)),
                                   na.rm = T)
timeseries$wiI18Banking <- rowMeans(subset(timeseries,
                                           select = c(wiI18Claims, wiI18Liabilities)),
                                    na.rm = T)
timeseries$wiI3.6DirectInv <- rowMeans(subset(timeseries,
                                              select = c(wiI3.6DII, wiI3.6DIdO)),
                                       na.rm = T)
timeseries$wiI3.6PortInv <- rowMeans(subset(timeseries,
                                            select = c(wiI3.6PIA, wiI3.6PIdL)),
                                     na.rm = T)
timeseries$wiI3.6Trade <- rowMeans(subset(timeseries,
                                          select = c(wiI3.6Export, wiI3.6Import)),
                                   na.rm = T)


# .... Calculate average E for each measure in each year ####
timeseries$wiE1Banking <- rowMeans(subset(timeseries,
                                          select = c(wiE1Claims, wiE1Liabilities)),
                                   na.rm = T)
timeseries$wiE18Banking <- rowMeans(subset(timeseries,
                                           select = c(wiE18Claims, wiE18Liabilities)),
                                    na.rm = T)
timeseries$wiE3.6DirectInv <- rowMeans(subset(timeseries,
                                              select = c(wiE3.6DII, wiE3.6DIdO)),
                                       na.rm = T)
timeseries$wiE3.6PortInv <- rowMeans(subset(timeseries,
                                            select = c(wiE3.6PIA, wiE3.6PIdL)),
                                     na.rm = T)
timeseries$wiE3.6Trade <- rowMeans(subset(timeseries,
                                          select = c(wiE3.6Export, wiE3.6Import)),
                                   na.rm = T)


# .... Plot ####
timeseries <- timeseries %>% select(rIncome:year, wiV1Banking:wiE3.6Trade)
timeseries <- melt(timeseries, 
                   id.vars = c("rIncome", "year"),
                   measure.vars = c("wiV1Banking", "wiV18Banking", 
                                    "wiV3.6DirectInv", "wiV3.6PortInv", "wiV3.6Trade",
                                    "wiI1Banking", "wiI18Banking", 
                                    "wiI3.6DirectInv", "wiI3.6PortInv", "wiI3.6Trade",
                                    "wiE1Banking", "wiE18Banking",
                                    "wiE3.6DirectInv", "wiE3.6PortInv", "wiE3.6Trade"))

HIC <- subset(timeseries, rIncome == "HIC" & value != "Inf")
UMC <- subset(timeseries, rIncome == "UMC" & value != "Inf")
LMC <- subset(timeseries, rIncome == "LMC" & value != "Inf")
LIC <- subset(timeseries, rIncome == "LIC" & value != "Inf")

incomegroup <- c("HIC", "UMC", "LMC", "LIC")
incomegroup.label <- c("high income countries", "upper-middle income countries", 
                       "lower-middle income countries", "lower income countries")

# Vulnerability and Exposure
measure <- c("V", "E")
measure.label <- c("Vulnerability", "Exposure")

for (m in 1:length(measure)){
  for (i in 1:length(incomegroup)){
    g <- ggplot(get(incomegroup[i]) %>% filter(variable == paste0("wi", measure[m], "1Banking") |
                                                 variable == paste0("wi", measure[m], "18Banking") |
                                                 variable == paste0("wi", measure[m], "3.6DirectInv") |
                                                 variable == paste0("wi", measure[m], "3.6PortInv") |
                                                 variable == paste0("wi", measure[m], "3.6Trade")),
                aes(x = year, y = value, color = variable)) + 
      geom_line(size = 1.5) +
      ggtitle(paste0(measure.label[m], " over time in ", incomegroup.label[i]),
              subtitle = "Individual KFSIs") + 
      xlab("Year") + ylab(paste0(measure.label[m], " Score")) +
      scale_color_manual(labels = c("Banking Positions, \nKFSI1", "Banking Positions, \nKFSI1",
                                    "Direct Investment, \nKFSIs 3 and 6",
                                    "Portfolio Investment, \nKFSIs 3 and 6",
                                    "Trade, \nKFSIs 3 and 6"),
                         values = wes_palette("Darjeeling1")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks= pretty_breaks())
    ggsave(g, 
           file = paste0("Figures/Individual KFSIs/Scores over time/By income group/", measure[m], "_", incomegroup[i], "_KFSIs", ".pdf"), 
           width = 6, height = 5, units = "in")
  }
}

# Intensity
measure <- c("I")
measure.label <- c("Intensity")

for (m in 1:length(measure)){
  for (i in 1:length(incomegroup)){
    g <- ggplot(get(incomegroup[i]) %>% filter(variable == paste0("wi", measure[m], "1Banking") |
                                                 variable == paste0("wi", measure[m], "3.6DirectInv") |
                                                 variable == paste0("wi", measure[m], "3.6PortInv") |
                                                 variable == paste0("wi", measure[m], "3.6Trade")),
                aes(x = year, y = value, color = variable)) + 
      geom_line(size = 1.5) +
      ggtitle(paste0(measure.label[m], " over time in ", incomegroup.label[i]),
              subtitle = "Individual KFSIs") + 
      xlab("Year") + ylab(paste0(measure.label[m], " Score")) +
      scale_color_manual(labels = c("Banking Positions", "Direct Investment",
                                    "Portfolio Investment", "Trade"),
                         values = wes_palette("Chevalier1")) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks= pretty_breaks())
    ggsave(g, 
           file = paste0("Figures/Individual KFSIs/Scores over time/By income group/", measure[m], "_", incomegroup[i], "_KFSIs", ".pdf"), 
           width = 6, height = 5, units = "in")
  }
}


# .... Export ####
timeseries <- timeseries[order(timeseries$variable, timeseries$rIncome, timeseries$year), ]
write.csv(timeseries, "Results/VIE time series_for income groups_KFSI.csv", row.names = FALSE)


# .. Housekeeping ####
rm(g, i, incomegroup, incomegroup.label, m, measure, measure.label, r, region, region.label,
   af, am, as, eu, oc, HIC, UMC, LMC, LIC)
rm(timeseries)



## ## ## ## ## ## ## ## ## ## ##
# EXPORT RESULTS            ####
## ## ## ## ## ## ## ## ## ## ##

missing <- which(rowSums(is.na(select(panelSJ_KFSI, V1Claims:wiE3.6DIdO))) ==
                   ncol(select(panelSJ_KFSI, V1Claims:wiE3.6DIdO)))
rm(missing)
allzero <- which(rowSums(select(panelSJ_KFSI, V1Claims:wiE3.6DIdO), na.rm = T) == 0)
rm(allzero)

results <- panelSJ_KFSI %>%
  group_by(rRegion, year) %>%
  distinct(reporter, .keep_all = TRUE) %>%
  select(reporter, reporter.ISO, year, rRegion, rIncome,
         pKFSI1,
         V1Claims:E3.6DIdO,
         wrV1Claims:wrV3.6DIdO,
         wrI1Claims:wrI3.6DIdO,
         wrE1Claims:wrE3.6DIdO,
         wiV1Claims:wiV3.6DIdO,
         wiI1Claims:wiI3.6DIdO,
         wiE1Claims:wiE3.6DIdO) %>%
  arrange(rRegion, reporter, year) %>%
  ungroup()

write.csv(results, "Results/Results_KFSI.csv", row.names = FALSE)
rm(results)