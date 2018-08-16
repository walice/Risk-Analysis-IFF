# Risk Analysis for Overall Secrecy Score
# Alice Lepissier

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Data Preparation
# VIE for Overall Secrecy
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
# .... By income group, including conduits
# .... By income group, excluding conduits
# .... Export
# .. Calculate average measures across years per region
# .... Plot region-level scores
# .... Plot region-level scores, all flows/stocks
# .... Plot region-level scores, all flows/stocks, aggregated
# .... Export
# .. Calculate average measures across years per income group
# .... Plot income group-level scores
# .... Plot income group-level scores, all flows/stocks
# .... Plot income group-level scores, all flows/stocks, aggregated
# .... Export
# .. Housekeeping
# Time Series Graphs
# .. For regions
# .... Calculate average V for each measure in each year
# .... Calculate average I for each measure in each year
# .... Calculate average E for each measure in each year
# .... Plot
# .... Export
# .. For income groups
# .... Calculate average V for each measure in each year
# .... Calculate average I for each measure in each year
# .... Calculate average E for each measure in each year
# .... Plot
# .... Export
# .. Housekeeping
# Yearly snapshot graphs
# .. Plot jurisdiction-level scores
# .... By region
# .... By income group
# .. Plot region-level scores
# .. Plot income group-level scores
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
library(tidyverse)
library(WDI)
library(wesanderson)
library(xlsx)



## ## ## ## ## ## ## ## ## ## ##
# DATA PREPARATION          ####
## ## ## ## ## ## ## ## ## ## ##

#source("Scripts/Data Preparation.R")
load("Data/panel.RData")



## ## ## ## ## ## ## ## ## ## ##
# VIE FOR OVERALL SECRECY   ####
## ## ## ## ## ## ## ## ## ## ##

panelSJ <- subset(panel, pSS != FALSE)

vars <- c("Claims", "Liabilities",
          "DIdI", "DII", "DIdO", "DIO",
          "PIA", "PIL", "PIdL",
          "Export", "Import")


# .. Jurisdiction-level scores ####
V <- paste0("V", vars)
I <- paste0("I", vars)

# .... Calculate Total of flow/stock per reporter per year ####
panelSJ <- panelSJ %>%
  group_by(reporter.ISO, year) %>%
  mutate_at(.vars = vars,
            .fun = funs(Tot = sum(., na.rm = T))) %>%
  ungroup()
names(panelSJ) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ))


# .... Calculate Vulnerabilities per reporter per year ####
panelSJ <- panelSJ %>%
  group_by(reporter.ISO, year) %>%
  mutate_at(.vars = vars,
            .fun = funs(V = sum((. * pSecrecyScore)/sum(., na.rm = T), na.rm = T))) %>%
  ungroup()
names(panelSJ) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ))


# .... Calculate Intensities per reporter per year ####
panelSJ <- panelSJ %>%
  group_by(reporter.ISO, year) %>%
  mutate_at(.vars = paste("Tot", vars, sep = ""),
            .fun = funs(I = abs( . / rGDP))) %>%
  ungroup()
names(panelSJ) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ))
names(panelSJ) <- gsub("ITot", "I", names(panelSJ))


# .... Calculate Exposures per reporter per year ####
n <- ncol(panelSJ)
for (var in 1:length(vars)){
  panelSJ[, n + var] <- panelSJ[[V[var]]] * panelSJ[[I[var]]]
  colnames(panelSJ)[n + var] <- paste0("E", vars[var])
}
rm(n, var)


# .. Region-level scores ####
wrV <- paste0("wrV", vars)
wrI <- paste0("wrI", vars)

# ... Calculate Total of flow/stock per region per year ####
panelSJ <- panelSJ %>%
  group_by(rRegion, year) %>%
  mutate_at(.vars = vars,
            .fun = funs(wrTot = sum(., na.rm = T))) %>%
  ungroup()
names(panelSJ) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ))


# .... Calculate Vulnerabilities per region per year ####
panelSJ <- panelSJ %>%
  group_by(rRegion, year) %>%
  mutate_at(.vars = vars,
            .fun = funs(wrV = sum((. * pSecrecyScore)/sum(., na.rm = T), na.rm = T))) %>%
  ungroup()
names(panelSJ) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ))


# .... Calculate Intensities per region per year ####
panelSJ <- panelSJ %>%
  group_by(rRegion, year) %>%
  mutate(wrrGDP = sum(rGDP, na.rm = T)) %>%
  ungroup()

panelSJ <- panelSJ %>%
  group_by(rRegion, year) %>%
  mutate_at(.vars = paste("wrTot", vars, sep = ""),
            .fun = funs(wrI = abs( . / wrrGDP))) %>%
  ungroup()
names(panelSJ) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ))
names(panelSJ) <- gsub("wrIwrTot", "wrI", names(panelSJ))


# .... Calculate Exposures per region per year ####
n <- ncol(panelSJ)
for (var in 1:length(vars)){
  panelSJ[, n + var] <- panelSJ[[wrV[var]]] * panelSJ[[wrI[var]]]
  colnames(panelSJ)[n + var] <- paste0("wrE", vars[var])
}
rm(n, var)


# .. Income group-level scores ####
wiV <- paste0("wiV", vars)
wiI <- paste0("wiI", vars)

# ... Calculate Total of flow/stock per income group per year ####
panelSJ <- panelSJ %>%
  group_by(rIncome, year) %>%
  mutate_at(.vars = vars,
            .fun = funs(wiTot = sum(., na.rm = T))) %>%
  ungroup()
names(panelSJ) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ))


# .... Calculate Vulnerabilities per income group per year ####
panelSJ <- panelSJ %>%
  group_by(rIncome, year) %>%
  mutate_at(.vars = vars,
            .fun = funs(wiV = sum((. * pSecrecyScore)/sum(., na.rm = T), na.rm = T))) %>%
  ungroup()
names(panelSJ) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ))


# .... Calculate Intensities per income group per year ####
panelSJ <- panelSJ %>%
  group_by(rIncome, year) %>%
  mutate(wirGDP = sum(rGDP, na.rm = T)) %>%
  ungroup()

panelSJ <- panelSJ %>%
  group_by(rIncome, year) %>%
  mutate_at(.vars = paste("wiTot", vars, sep = ""),
            .fun = funs(wiI = abs( . / wirGDP))) %>%
  ungroup()
names(panelSJ) <- sub("^(.*)_(.*)$", "\\2\\1", names(panelSJ))
names(panelSJ) <- gsub("wiIwiTot", "wiI", names(panelSJ))


# .... Calculate Exposures per income group per year ####
n <- ncol(panelSJ)
for (var in 1:length(vars)){
  panelSJ[, n + var] <- panelSJ[[wiV[var]]] * panelSJ[[wiI[var]]]
  colnames(panelSJ)[n + var] <- paste0("wiE", vars[var])
}
rm(n, var)
rm(V, I, wrV, wrI, wiV, wiI, vars)
rm(panel)


# .. Check zero values in measures due to summing over NAs ####
summary(panelSJ)

# .... For Totals ####
grep("Tot", names(panelSJ))
for (c in 11:21){
  z <- which(panelSJ[, c+61] == 0) # increment is -40 if panel w/o KFSIs
  zeroTot <- panelSJ[z, ]
  print(unique(zeroTot[, c]))
}

grep("wrTot", names(panelSJ))
for (c in 11:21){
  z <- which(panelSJ[, c+105] == 0)
  zeroTot <- panelSJ[z, ]
  print(unique(zeroTot[, c]))
}

grep("wiTot", names(panelSJ))
for (c in 11:21){
  z <- which(panelSJ[, c+150] == 0)
  zeroTot <- panelSJ[z, ]
  print(unique(zeroTot[, c]))
}

# All the 0 values in the totals for flows/stocks are due to summing only NAs with na.rm = T.
# We can convert them back to NAs.
# na.rm = T is needed when summing otherwise a single NA value will return NAs (even though there may be valid other values).
cols <- grep("Tot", names(panelSJ))
panelSJ[, cols][panelSJ[, cols] == 0] <- NA
rm(zeroTot, c, cols, z)


# .... For Vulnerabilities ####
grep("V", names(panelSJ))
for (c in 11:21){
  z <- which(panelSJ[, c+72] == 0)
  zeroTot <- panelSJ[z, ]
  print(unique(zeroTot[, c]))
}

grep("wrV", names(panelSJ))
for (c in 11:21){
  z <- which(panelSJ[, c+116] == 0)
  zeroTot <- panelSJ[z, ]
  print(unique(zeroTot[, c]))
}

grep("wiV", names(panelSJ))
for (c in 11:21){
  z <- which(panelSJ[, c+161] == 0)
  zeroTot <- panelSJ[z, ]
  print(unique(zeroTot[, c]))
}

# All the 0 values in the Vulnerabilities for flows/stocks are due to summing only NAs with na.rm = T.
# We can convert them back to NAs.
# na.rm = T is needed when summing otherwise a single NA value will return NAs (even though there may be valid other values).
cols <- grep("V", names(panelSJ))
panelSJ[, cols][panelSJ[, cols] == 0] <- NA
rm(zeroTot, c, cols, z)


# .... For Intensities ####
colnames(panelSJ)
for (c in 11:21){
  z <- which(panelSJ[, c+83] == 0)
  zeroTot <- panelSJ[z, ]
  print(unique(zeroTot[, c]))
}

grep("wrI", names(panelSJ))
for (c in 11:21){
  z <- which(panelSJ[, c+128] == 0)
  zeroTot <- panelSJ[z, ]
  print(unique(zeroTot[, c]))
}

grep("wiI", names(panelSJ))
for (c in 11:21){
  z <- which(panelSJ[, c+173] == 0)
  zeroTot <- panelSJ[z, ]
  print(unique(zeroTot[, c]))
}

# All the 0 values in the Intensities for flows/stocks are due to summing only NAs with na.rm = T.
# We can convert them back to NAs.
# na.rm = T is needed when summing otherwise a single NA value will return NAs (even though there may be valid other values).
colnames(panelSJ)
cols <- c(94:104,139:149,184:194)
panelSJ[, cols][panelSJ[, cols] == 0] <- NA
rm(zeroTot, c, cols, z)


# .... For Exposures ####
colnames(panelSJ)
for (c in 11:21){
  z <- which(panelSJ[, c+94] == 0)
  zeroTot <- panelSJ[z, ]
  print(unique(zeroTot[, c]))
}

grep("wrE", names(panelSJ))
for (c in 11:21){
  z <- which(panelSJ[, c+139] == 0)
  zeroTot <- panelSJ[z, ]
  print(unique(zeroTot[, c]))
}

grep("wiE", names(panelSJ))
for (c in 11:21){
  z <- which(panelSJ[, c+184] == 0)
  zeroTot <- panelSJ[z, ]
  print(unique(zeroTot[, c]))
}

# All the 0 values in the Exposures for flows/stocks are due to summing only NAs with na.rm = T.
# We can convert them back to NAs.
# na.rm = T is needed when summing otherwise a single NA value will return NAs (even though there may be valid other values).
colnames(panelSJ)
cols <- c(105:115,150:160,195:205)
panelSJ[, cols][panelSJ[, cols] == 0] <- NA
rm(zeroTot, c, cols, z)


# .. Remove infinite values ####
panelSJ[panelSJ == "Inf"] <- NA

panelSJ <- panelSJ %>% 
  select(id, reporter, reporter.ISO, partner, partner.ISO, year, everything()) %>%
  arrange(id)
save(panelSJ, file = "Data/panelSJ.RData")
write.csv(panelSJ, "Results/panelSJ_Secrecy Score.csv", row.names = FALSE)



## ## ## ## ## ## ## ## ## ## ##
# AVERAGE MEASURES GRAPHS   ####
## ## ## ## ## ## ## ## ## ## ##

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

flowstock <- c("Banking Positions", "Direct Investment", "Portfolio Investment", "Trade")

vars <- list()
vars$BankingPositions <- c("Claims", "Liabilities")
vars$DirectInvestment <- c("DII", "DIdO")
vars$PortInvestment <- c("PIA", "PIdL")
vars$Trade <- c("Export", "Import")

var.labels <- list()
var.labels$BankingPositions <- c("Claims", "Liabilities")
var.labels$DirectInvestment <- c("Inward", "Outward (derived)")
var.labels$PortInvestment <- c("Assets", "Liabilities (derived)")
var.labels$Trade <- c("Exports", "Imports")

cols <- list()
cols$BankingPositions <- c("#899DA4", "#C93312")
cols$DirectInvestment <- c("#7294D4", "#FD6467")
cols$PortInvestment <- c("#00A08A", "#F98400")
cols$Trade <- c("#FAD510", "#800080")


# .. Calculate average measures across years per reporter ####
measures <- c("VClaims", "VLiabilities",
              "VDIdI", "VDII", "VDIdO", "VDIO",
              "VPIA", "VPIL", "VPIdL",
              "VExport", "VImport",
              "IClaims", "ILiabilities",
              "IDIdI", "IDII", "IDIdO", "IDIO",
              "IPIA", "IPIL", "IPIdL",
              "IExport", "IImport",
              "EClaims", "ELiabilities",
              "EDIdI", "EDII", "EDIdO", "EDIO",
              "EPIA", "EPIL", "EPIdL",
              "EExport", "EImport")

yraverage <- panelSJ %>% 
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
                  measure.vars = c("xVClaims", "xVLiabilities",
                                   "xVDIdI", "xVDII", "xVDIdO", "xVDIO", 
                                   "xVPIA", "xVPIL", "xVPIdL", 
                                   "xVExport", "xVImport",
                                   "xIClaims", "xILiabilities",
                                   "xIDIdI", "xIDII", "xIDIdO", "xIDIO",
                                   "xIPIA", "xIPIL", "xIPIdL",
                                   "xIExport", "xIImport",
                                   "xEClaims", "xELiabilities",
                                   "xEDIdI", "xEDII", "xEDIdO", "xEDIO", 
                                   "xEPIA", "xEPIL", "xEPIdL",
                                   "xEExport", "xEImport"))
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
                aes(x = reorder(reporter, value, sum), y = value, fill = fct_rev(variable))) +
      geom_col() + coord_flip() +
      ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in Conduits")) +
      xlab("Reporting country") + ylab(paste0(measure.label[m], " Score")) +
      guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
      scale_fill_manual(labels = rev(c(var.labels[[f]][1], var.labels[[f]][2])),
                        values = c(cols[[f]][1], cols[[f]][2])) +
      scale_y_continuous(labels = comma)
    ggsave(g,
           file = paste0("Figures/Overall Secrecy Score/Jurisdiction scores/Conduits/", measure[m], "_", names(vars)[f], "_Conduits",".pdf"),
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
                  aes(x = reorder(reporter, value, sum), y = value, fill = fct_rev(variable))) +
        geom_col() + coord_flip() +
        ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in ", region.label[r])) +
        xlab("Reporting country") + ylab(paste0(measure.label[m], " Score")) +
        guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
        scale_fill_manual(labels = rev(c(var.labels[[f]][1], var.labels[[f]][2])),
                          values = c(cols[[f]][1], cols[[f]][2])) +
        scale_y_continuous(labels = comma)
      ggsave(g,
             file = paste0("Figures/Overall Secrecy Score/Jurisdiction scores/By region/Including conduits/", measure[m], "_", names(vars)[f], "_", region.label[r],".pdf"),
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
                  aes(x = reorder(reporter, value, sum), y = value, fill = fct_rev(variable))) +
        geom_col() + coord_flip() +
        ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in ", region.label[r])) +
        xlab("Reporting country") + ylab(paste0(measure.label[m], " Score")) +
        guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
        scale_fill_manual(labels = rev(c(var.labels[[f]][1], var.labels[[f]][2])),
                          values = c(cols[[f]][1], cols[[f]][2])) +
        scale_y_continuous(labels = comma)
      ggsave(g,
             file = paste0("Figures/Overall Secrecy Score/Jurisdiction scores/By region/Excluding conduits/", measure[m], "_", names(vars)[f], "_", region.label[r], "_No Conduits", ".pdf"),
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
                  aes(x = reorder(reporter, value, sum), y = value, fill = fct_rev(variable))) +
        geom_col() + coord_flip() +
        ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in ", incomegroup.label[i])) +
        xlab("Reporting country") + ylab(paste0(measure.label[m], " Score")) +
        guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
        scale_fill_manual(labels = rev(c(var.labels[[f]][1], var.labels[[f]][2])),
                          values = c(cols[[f]][1], cols[[f]][2])) +
        scale_y_continuous(labels = comma)
      ggsave(g,
             file = paste0("Figures/Overall Secrecy Score/Jurisdiction scores/By income group/Including conduits/", measure[m], "_", names(vars)[f], "_", incomegroup.label[i],".pdf"),
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
                  aes(x = reorder(reporter, value, sum), y = value, fill = fct_rev(variable))) +
        geom_col() + coord_flip() +
        ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in ", incomegroup.label[i])) +
        xlab("Reporting country") + ylab(paste0(measure.label[m], " Score")) +
        guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
        scale_fill_manual(labels = rev(c(var.labels[[f]][1], var.labels[[f]][2])),
                          values = c(cols[[f]][1], cols[[f]][2])) +
        scale_y_continuous(labels = comma)
      ggsave(g,
             file = paste0("Figures/Overall Secrecy Score/Jurisdiction scores/By income group/Excluding conduits/", measure[m], "_", names(vars)[f], "_", incomegroup.label[i], "_No Conduits",".pdf"),
             width = 6, height = 5, units = "in")
    }
  }
}


# .... Export ####
yraverage <- yraverage[order(yraverage$variable, yraverage$reporter), ]
write.csv(yraverage, "Results/VIE averages_for jurisdictions_Secrecy Score.csv", row.names = FALSE)


# .. Calculate average measures across years per region ####
measures <- c("wrVClaims", "wrVLiabilities",
              "wrVDIdI", "wrVDII", "wrVDIdO", "wrVDIO",
              "wrVPIA", "wrVPIL", "wrVPIdL",
              "wrVExport", "wrVImport",
              "wrIClaims", "wrILiabilities",
              "wrIDIdI", "wrIDII", "wrIDIdO", "wrIDIO",
              "wrIPIA", "wrIPIL", "wrIPIdL",
              "wrIExport", "wrIImport",
              "wrEClaims", "wrELiabilities",
              "wrEDIdI", "wrEDII", "wrEDIdO", "wrEDIO",
              "wrEPIA", "wrEPIL", "wrEPIdL",
              "wrEExport", "wrEImport")

yraverage <- panelSJ %>% 
  group_by(rRegion) %>%
  distinct(year, .keep_all = TRUE) %>%
  mutate_at(.vars = measures,
            .fun = funs(x = mean(., na.rm = T))) %>%
  ungroup()
names(yraverage) <- sub("^(.*)_(.*)$", "\\2\\1", names(yraverage))

yraverage <- yraverage %>%
  select(rRegion, starts_with("xwr")) %>%
  distinct(rRegion, .keep_all = TRUE)

yraverage$xwrVBanking <- rowMeans(subset(yraverage,
                                         select = c(xwrVClaims, xwrVLiabilities)),
                                  na.rm = T)
yraverage$xwrVDirectInv <- rowMeans(subset(yraverage,
                                           select = c(xwrVDII, xwrVDIdO)),
                                    na.rm = T)
yraverage$xwrVPortInv <- rowMeans(subset(yraverage,
                                         select = c(xwrVPIA, xwrVPIdL)),
                                  na.rm = T)
yraverage$xwrVTrade <- rowMeans(subset(yraverage,
                                       select = c(xwrVExport, xwrVImport)),
                                na.rm = T)
yraverage$xwrIBanking <- rowMeans(subset(yraverage,
                                         select = c(xwrIClaims, xwrILiabilities)),
                                  na.rm = T)
yraverage$xwrIDirectInv <- rowMeans(subset(yraverage,
                                           select = c(xwrIDII, xwrIDIdO)),
                                    na.rm = T)
yraverage$xwrIPortInv <- rowMeans(subset(yraverage,
                                         select = c(xwrIPIA, xwrIPIdL)),
                                  na.rm = T)
yraverage$xwrITrade <- rowMeans(subset(yraverage,
                                       select = c(xwrIExport, xwrIImport)),
                                na.rm = T)
yraverage$xwrEBanking <- rowMeans(subset(yraverage,
                                         select = c(xwrEClaims, xwrELiabilities)),
                                  na.rm = T)
yraverage$xwrEDirectInv <- rowMeans(subset(yraverage,
                                           select = c(xwrEDII, xwrEDIdO)),
                                    na.rm = T)
yraverage$xwrEPortInv <- rowMeans(subset(yraverage,
                                         select = c(xwrEPIA, xwrEPIdL)),
                                  na.rm = T)
yraverage$xwrETrade <- rowMeans(subset(yraverage,
                                       select = c(xwrEExport, xwrEImport)),
                                na.rm = T)

yraverage <- melt(yraverage, 
                  id.vars = c("rRegion"),
                  measure.vars = c("xwrVClaims", "xwrVLiabilities",
                                   "xwrVDIdI", "xwrVDII", "xwrVDIdO", "xwrVDIO", 
                                   "xwrVPIA", "xwrVPIL", "xwrVPIdL", 
                                   "xwrVExport", "xwrVImport",
                                   "xwrIClaims", "xwrILiabilities",
                                   "xwrIDIdI", "xwrIDII", "xwrIDIdO", "xwrIDIO",
                                   "xwrIPIA", "xwrIPIL", "xwrIPIdL",
                                   "xwrIExport", "xwrIImport",
                                   "xwrEClaims", "xwrELiabilities",
                                   "xwrEDIdI", "xwrEDII", "xwrEDIdO", "xwrEDIO", 
                                   "xwrEPIA", "xwrEPIL", "xwrEPIdL",
                                   "xwrEExport", "xwrEImport",
                                   "xwrVBanking", "xwrVDirectInv", "xwrVPortInv", "xwrVTrade",
                                   "xwrIBanking", "xwrIDirectInv", "xwrIPortInv", "xwrITrade",
                                   "xwrEBanking", "xwrEDirectInv", "xwrEPortInv", "xwrETrade"))
yraverage <- subset(yraverage, !is.na(value) & !is.infinite(value))


# .... Plot region-level scores ####
for (m in 1:length(wrmeasure)){
  for (f in 1:length(flowstock)){
    g <- ggplot(yraverage %>% filter((variable == paste0(wrmeasure[m], vars[[f]][1]) |
                                        variable == paste0(wrmeasure[m], vars[[f]][2])) &
                                       value != 0 & rRegion != "") %>%
                  distinct(rRegion, variable, .keep_all = TRUE),
                aes(x = reorder(rRegion, value, sum), y = value, fill = fct_rev(variable))) +
      geom_col() + coord_flip() +
      ggtitle(paste0(measure.label[m], " of ", flowstock[f])) +
      xlab("Region") + ylab(paste0(measure.label[m], " Score")) +
      guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
      scale_fill_manual(labels = rev(c(var.labels[[f]][1], var.labels[[f]][2])),
                        values = c(cols[[f]][1], cols[[f]][2])) +
      scale_y_continuous(labels = comma)
    ggsave(g,
           file = paste0("Figures/Overall Secrecy Score/Regional scores/", wrmeasure[m], "_", names(vars)[f],".pdf"),
           width = 6, height = 5, units = "in")
  }
}


# .... Plot region-level scores, all flows/stocks ####
for (m in 1:length(wrmeasure)){
  g <- ggplot(yraverage %>% filter((variable == paste0(wrmeasure[m], vars[[1]][1]) |
                                      variable == paste0(wrmeasure[m], vars[[1]][2]) |
                                      variable == paste0(wrmeasure[m], vars[[2]][1]) |
                                      variable == paste0(wrmeasure[m], vars[[2]][2]) |
                                      variable == paste0(wrmeasure[m], vars[[3]][1]) |
                                      variable == paste0(wrmeasure[m], vars[[3]][2]) |
                                      variable == paste0(wrmeasure[m], vars[[4]][1]) |
                                      variable == paste0(wrmeasure[m], vars[[4]][2])) &
                                     value != 0 & rRegion != "") %>%
                distinct(rRegion, variable, .keep_all = TRUE),
              aes(x = reorder(rRegion, value, sum), y = value, fill = fct_rev(variable))) +
    geom_col() + coord_flip() +
    ggtitle(paste0(measure.label[m], " of all flows/stocks")) +
    xlab("Region") + ylab(paste0(measure.label[m], " Score")) +
    guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
    scale_fill_manual(labels = rev(c(var.labels[[1]][1], var.labels[[1]][2],
                                     var.labels[[2]][1], var.labels[[2]][2],
                                     var.labels[[3]][1], var.labels[[3]][2],
                                     var.labels[[4]][1], var.labels[[4]][2])),
                      values = rev(c(cols[[1]][2], cols[[1]][1],
                                     cols[[2]][2], cols[[2]][1],
                                     cols[[3]][2], cols[[3]][1],
                                     cols[[4]][2], cols[[4]][1]))) +
    scale_y_continuous(labels = comma)
  ggsave(g,
         file = paste0("Figures/Overall Secrecy Score/Regional scores/", wrmeasure[m], "_All",".pdf"),
         width = 6, height = 5, units = "in")
}


# .... Plot region-level scores, all flows/stocks, aggregated ####
for (m in 1:length(wrmeasure)){
  g <- ggplot(yraverage %>% filter((variable == paste0(wrmeasure[m], "Trade") |
                                      variable == paste0(wrmeasure[m], "PortInv") |
                                      variable == paste0(wrmeasure[m], "DirectInv") |
                                      variable == paste0(wrmeasure[m], "Banking")) &
                                     value != 0 & rRegion != "") %>%
                distinct(rRegion, variable, .keep_all = TRUE),
              aes(x = reorder(rRegion, value, sum), y = value, fill = fct_rev(variable))) +
    geom_col() + coord_flip() +
    ggtitle(paste0(measure.label[m], " of all flows/stocks, aggregated")) +
    xlab("Region") + ylab(paste0(measure.label[m], " Score")) +
    guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
    scale_fill_manual(labels = c("Trade", "Portfolio Investment",
                                 "Direct Investment", "Banking Positions"),
                      values = wes_palette("GrandBudapest1"))  +
    scale_y_continuous(labels = comma)
  ggsave(g,
         file = paste0("Figures/Overall Secrecy Score/Regional scores/", wrmeasure[m], "_All_Aggregated",".pdf"),
         width = 6, height = 5, units = "in")
}


# .... Export ####
yraverage <- yraverage[order(yraverage$variable, yraverage$rRegion), ]
write.csv(yraverage, "Results/VIE averages_for regions_Secrecy Score.csv", row.names = FALSE)


# .. Calculate average measures across years per income group ####
measures <- c("wiVClaims", "wiVLiabilities",
              "wiVDIdI", "wiVDII", "wiVDIdO", "wiVDIO",
              "wiVPIA", "wiVPIL", "wiVPIdL",
              "wiVExport", "wiVImport",
              "wiIClaims", "wiILiabilities",
              "wiIDIdI", "wiIDII", "wiIDIdO", "wiIDIO",
              "wiIPIA", "wiIPIL", "wiIPIdL",
              "wiIExport", "wiIImport",
              "wiEClaims", "wiELiabilities",
              "wiEDIdI", "wiEDII", "wiEDIdO", "wiEDIO",
              "wiEPIA", "wiEPIL", "wiEPIdL",
              "wiEExport", "wiEImport")

yraverage <- panelSJ %>% 
  group_by(rIncome) %>%
  distinct(year, .keep_all = TRUE) %>%
  mutate_at(.vars = measures,
            .fun = funs(x = mean(., na.rm = T))) %>%
  ungroup()
names(yraverage) <- sub("^(.*)_(.*)$", "\\2\\1", names(yraverage))

yraverage <- yraverage %>%
  select(rIncome, starts_with("xwi")) %>%
  distinct(rIncome, .keep_all = TRUE)

yraverage$xwiVBanking <- rowMeans(subset(yraverage,
                                         select = c(xwiVClaims, xwiVLiabilities)),
                                  na.rm = T)
yraverage$xwiVDirectInv <- rowMeans(subset(yraverage,
                                           select = c(xwiVDII, xwiVDIdO)),
                                    na.rm = T)
yraverage$xwiVPortInv <- rowMeans(subset(yraverage,
                                         select = c(xwiVPIA, xwiVPIdL)),
                                  na.rm = T)
yraverage$xwiVTrade <- rowMeans(subset(yraverage,
                                       select = c(xwiVExport, xwiVImport)),
                                na.rm = T)
yraverage$xwiIBanking <- rowMeans(subset(yraverage,
                                         select = c(xwiIClaims, xwiILiabilities)),
                                  na.rm = T)
yraverage$xwiIDirectInv <- rowMeans(subset(yraverage,
                                           select = c(xwiIDII, xwiIDIdO)),
                                    na.rm = T)
yraverage$xwiIPortInv <- rowMeans(subset(yraverage,
                                         select = c(xwiIPIA, xwiIPIdL)),
                                  na.rm = T)
yraverage$xwiITrade <- rowMeans(subset(yraverage,
                                       select = c(xwiIExport, xwiIImport)),
                                na.rm = T)
yraverage$xwiEBanking <- rowMeans(subset(yraverage,
                                         select = c(xwiEClaims, xwiELiabilities)),
                                  na.rm = T)
yraverage$xwiEDirectInv <- rowMeans(subset(yraverage,
                                           select = c(xwiEDII, xwiEDIdO)),
                                    na.rm = T)
yraverage$xwiEPortInv <- rowMeans(subset(yraverage,
                                         select = c(xwiEPIA, xwiEPIdL)),
                                  na.rm = T)
yraverage$xwiETrade <- rowMeans(subset(yraverage,
                                       select = c(xwiEExport, xwiEImport)),
                                na.rm = T)

yraverage <- melt(yraverage, 
                  id.vars = c("rIncome"),
                  measure.vars = c("xwiVClaims", "xwiVLiabilities",
                                   "xwiVDIdI", "xwiVDII", "xwiVDIdO", "xwiVDIO", 
                                   "xwiVPIA", "xwiVPIL", "xwiVPIdL", 
                                   "xwiVExport", "xwiVImport",
                                   "xwiIClaims", "xwiILiabilities",
                                   "xwiIDIdI", "xwiIDII", "xwiIDIdO", "xwiIDIO",
                                   "xwiIPIA", "xwiIPIL", "xwiIPIdL",
                                   "xwiIExport", "xwiIImport",
                                   "xwiEClaims", "xwiELiabilities",
                                   "xwiEDIdI", "xwiEDII", "xwiEDIdO", "xwiEDIO", 
                                   "xwiEPIA", "xwiEPIL", "xwiEPIdL",
                                   "xwiEExport", "xwiEImport",
                                   "xwiVBanking", "xwiVDirectInv", "xwiVPortInv", "xwiVTrade",
                                   "xwiIBanking", "xwiIDirectInv", "xwiIPortInv", "xwiITrade",
                                   "xwiEBanking", "xwiEDirectInv", "xwiEPortInv", "xwiETrade"))
yraverage <- subset(yraverage, !is.na(value) & !is.infinite(value))


# .... Plot income group-level scores ####
for (m in 1:length(wimeasure)){
  for (f in 1:length(flowstock)){
    g <- ggplot(yraverage %>% filter((variable == paste0(wimeasure[m], vars[[f]][1]) |
                                        variable == paste0(wimeasure[m], vars[[f]][2])) &
                                       value != 0 & rIncome != "") %>%
                  distinct(rIncome, variable, .keep_all = TRUE),
                aes(x = reorder(rIncome, value, sum), y = value, fill = fct_rev(variable))) +
      geom_col() + coord_flip() +
      ggtitle(paste0(measure.label[m], " of ", flowstock[f])) +
      xlab("Income group") + ylab(paste0(measure.label[m], " Score")) +
      guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
      scale_fill_manual(labels = rev(c(var.labels[[f]][1], var.labels[[f]][2])),
                        values = c(cols[[f]][1], cols[[f]][2])) +
      scale_y_continuous(labels = comma)
    ggsave(g,
           file = paste0("Figures/Overall Secrecy Score/Income group scores/", wimeasure[m], "_", names(vars)[f],".pdf"),
           width = 6, height = 5, units = "in")
  }
}


# .... Plot income group-level scores, all flows/stocks ####
for (m in 1:length(wimeasure)){
  g <- ggplot(yraverage %>% filter((variable == paste0(wimeasure[m], vars[[1]][1]) |
                                      variable == paste0(wimeasure[m], vars[[1]][2]) |
                                      variable == paste0(wimeasure[m], vars[[2]][1]) |
                                      variable == paste0(wimeasure[m], vars[[2]][2]) |
                                      variable == paste0(wimeasure[m], vars[[3]][1]) |
                                      variable == paste0(wimeasure[m], vars[[3]][2]) |
                                      variable == paste0(wimeasure[m], vars[[4]][1]) |
                                      variable == paste0(wimeasure[m], vars[[4]][2])) &
                                     value != 0 & rIncome != "") %>%
                distinct(rIncome, variable, .keep_all = TRUE),
              aes(x = reorder(rIncome, value, sum), y = value, fill = fct_rev(variable))) +
    geom_col() + coord_flip() +
    ggtitle(paste0(measure.label[m], " of all flows/stocks")) +
    xlab("Income group") + ylab(paste0(measure.label[m], " Score")) +
    guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
    scale_fill_manual(labels = rev(c(var.labels[[1]][1], var.labels[[1]][2],
                                     var.labels[[2]][1], var.labels[[2]][2],
                                     var.labels[[3]][1], var.labels[[3]][2],
                                     var.labels[[4]][1], var.labels[[4]][2])),
                      values = rev(c(cols[[1]][2], cols[[1]][1],
                                     cols[[2]][2], cols[[2]][1],
                                     cols[[3]][2], cols[[3]][1],
                                     cols[[4]][2], cols[[4]][1]))) +
    scale_y_continuous(labels = comma)
  ggsave(g,
         file = paste0("Figures/Overall Secrecy Score/Income group scores/", wimeasure[m], "_All",".pdf"),
         width = 6, height = 5, units = "in")
}


# .... Plot income group-level scores, all flows/stocks, aggregated ####
for (m in 1:length(wimeasure)){
  g <- ggplot(yraverage %>% filter((variable == paste0(wimeasure[m], "Trade") |
                                      variable == paste0(wimeasure[m], "PortInv") |
                                      variable == paste0(wimeasure[m], "DirectInv") |
                                      variable == paste0(wimeasure[m], "Banking")) &
                                     value != 0 & rIncome != "") %>%
                distinct(rIncome, variable, .keep_all = TRUE),
              aes(x = reorder(rIncome, value, sum), y = value, fill = fct_rev(variable))) +
    geom_col() + coord_flip() +
    ggtitle(paste0(measure.label[m], " of all flows/stocks, aggregated")) +
    xlab("Income group") + ylab(paste0(measure.label[m], " Score")) +
    guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
    scale_fill_manual(labels = c("Trade", "Portfolio Investment",
                                 "Direct Investment", "Banking Positions"),
                      values = wes_palette("GrandBudapest1"))  +
    scale_y_continuous(labels = comma)
  ggsave(g,
         file = paste0("Figures/Overall Secrecy Score/Income group scores/", wimeasure[m], "_All_Aggregated",".pdf"),
         width = 6, height = 5, units = "in")
}


# .... Export ####
yraverage <- yraverage[order(yraverage$variable, yraverage$rIncome), ]
write.csv(yraverage, "Results/VIE averages_for income groups_Secrecy Score.csv", row.names = FALSE)


# .. Housekeeping ####
rm(cols, f, flowstock, g, i, incomegroup, incomegroup.label,
   m, measure, measures, wrmeasure, wimeasure, measure.label, r, region, region.label,
   var.labels, vars,
   af, am, as, eu, oc, nr, HIC, UMC, LMC, LIC, ni,
   afno, amno, asno, euno, ocno, nrno, HICno, UMCno, LMCno, LICno, nino,
   pctiles, choose.cut, regionno, incomegroupno)
rm(yraverage, conduits)



## ## ## ## ## ## ## ## ## ## ##
# TIME SERIES GRAPHS        ####
## ## ## ## ## ## ## ## ## ## ##

# .. For regions ####
timeseries <- panelSJ %>%
  distinct(rRegion, year, .keep_all = TRUE) %>%
  select(rRegion, year,
         wrVClaims:wrVImport,
         wrIClaims:wrIImport,
         wrEClaims:wrEImport) %>%
  arrange(rRegion, year)


# .... Calculate average V for each measure in each year ####
timeseries$wrVBanking <- rowMeans(subset(timeseries,
                                         select = c(wrVClaims, wrVLiabilities)),
                                  na.rm = T)
timeseries$wrVDirectInv <- rowMeans(subset(timeseries,
                                         select = c(wrVDII, wrVDIdO)),
                                  na.rm = T)
timeseries$wrVPortInv <- rowMeans(subset(timeseries,
                                       select = c(wrVPIA, wrVPIdL)),
                                na.rm = T)
timeseries$wrVTrade <- rowMeans(subset(timeseries,
                                    select = c(wrVExport, wrVImport)),
                             na.rm = T)


# .... Calculate average I for each measure in each year ####
timeseries$wrIBanking <- rowMeans(subset(timeseries,
                                         select = c(wrIClaims, wrILiabilities)),
                                  na.rm = T)
timeseries$wrIDirectInv <- rowMeans(subset(timeseries,
                                           select = c(wrIDII, wrIDIdO)),
                                    na.rm = T)
timeseries$wrIPortInv <- rowMeans(subset(timeseries,
                                         select = c(wrIPIA, wrIPIdL)),
                                  na.rm = T)
timeseries$wrITrade <- rowMeans(subset(timeseries,
                                       select = c(wrIExport, wrIImport)),
                                na.rm = T)


# .... Calculate average E for each measure in each year ####
timeseries$wrEBanking <- rowMeans(subset(timeseries,
                                         select = c(wrEClaims, wrELiabilities)),
                                  na.rm = T)
timeseries$wrEDirectInv <- rowMeans(subset(timeseries,
                                           select = c(wrEDII, wrEDIdO)),
                                    na.rm = T)
timeseries$wrEPortInv <- rowMeans(subset(timeseries,
                                         select = c(wrEPIA, wrEPIdL)),
                                  na.rm = T)
timeseries$wrETrade <- rowMeans(subset(timeseries,
                                       select = c(wrEExport, wrEImport)),
                                na.rm = T)


# .... Plot ####
timeseries <- timeseries %>% select(rRegion:year, wrVBanking:wrETrade)
timeseries <- melt(timeseries,
                   id.vars = c("rRegion", "year"),
              measure.vars = c("wrVBanking", "wrVDirectInv", "wrVPortInv", "wrVTrade",
                               "wrIBanking", "wrIDirectInv", "wrIPortInv", "wrITrade",
                               "wrEBanking", "wrEDirectInv", "wrEPortInv", "wrETrade"))

af <- subset(timeseries, rRegion == "Africa" & value != "Inf")
am <- subset(timeseries, rRegion == "Americas" & value != "Inf")
as <- subset(timeseries, rRegion == "Asia" & value != "Inf")
eu <- subset(timeseries, rRegion == "Europe" & value != "Inf")
oc <- subset(timeseries, rRegion == "Oceania" & value != "Inf")

measure <- c("V", "I", "E")
measure.label <- c("Vulnerability", "Intensity", "Exposure")

region <- c("af", "am", "as", "eu", "oc")
region.label <- c("Africa", "Americas", "Asia", "Europe", "Oceania")

for (m in 1:length(measure)){
  for (r in 1:length(region)){
    g <- ggplot(get(region[r]) %>% filter(variable == paste0("wr", measure[m], "Banking") |
                                        variable == paste0("wr", measure[m], "DirectInv") |
                                        variable == paste0("wr", measure[m], "PortInv") |
                                        variable == paste0("wr", measure[m], "Trade")),
                aes(x = year, y = value, color = variable)) +
      geom_line(size = 1.5) +
      ggtitle(paste0(measure.label[m], " over time in ", region.label[r]),
              subtitle = "Overall Secrecy Score") +
      xlab("Year") + ylab(paste0(measure.label[m], " Score")) +
      scale_color_manual(labels = c("Banking Positions","Direct Investment",
                                    "Portfolio Investment", "Trade"),
                         values = rev(wes_palette("GrandBudapest1"))) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks= pretty_breaks())
    ggsave(g,
       file = paste0("Figures/Overall Secrecy Score/Scores over time/For regions/", measure[m], "_", region.label[r],".pdf"),
       width = 6, height = 5, units = "in")
  }
}


# .... Export ####
timeseries <- timeseries[order(timeseries$variable, timeseries$rRegion, timeseries$year), ]
write.csv(timeseries, "Results/VIE time series_for regions_Secrecy Score.csv", row.names = FALSE)


# .. For income groups ####
timeseries <- panelSJ %>% 
  distinct(rIncome, year, .keep_all = TRUE) %>%
  select(rIncome, year,
         wiVClaims:wiVImport,
         wiIClaims:wiIImport,
         wiEClaims:wiEImport) %>%
  arrange(rIncome, year)


# .... Calculate average V for each measure in each year ####
timeseries$wiVBanking <- rowMeans(subset(timeseries,
                                         select = c(wiVClaims, wiVLiabilities)),
                                  na.rm = T)
timeseries$wiVDirectInv <- rowMeans(subset(timeseries,
                                           select = c(wiVDII, wiVDIdO)),
                                    na.rm = T)
timeseries$wiVPortInv <- rowMeans(subset(timeseries,
                                         select = c(wiVPIA, wiVPIdL)),
                                  na.rm = T)
timeseries$wiVTrade <- rowMeans(subset(timeseries,
                                       select = c(wiVExport, wiVImport)),
                                na.rm = T)


# .... Calculate average I for each measure in each year ####
timeseries$wiIBanking <- rowMeans(subset(timeseries,
                                         select = c(wiIClaims, wiILiabilities)),
                                  na.rm = T)
timeseries$wiIDirectInv <- rowMeans(subset(timeseries,
                                           select = c(wiIDII, wiIDIdO)),
                                    na.rm = T)
timeseries$wiIPortInv <- rowMeans(subset(timeseries,
                                         select = c(wiIPIA, wiIPIdL)),
                                  na.rm = T)
timeseries$wiITrade <- rowMeans(subset(timeseries,
                                       select = c(wiIExport, wiIImport)),
                                na.rm = T)


# .... Calculate average E for each measure in each year ####
timeseries$wiEBanking <- rowMeans(subset(timeseries,
                                         select = c(wiEClaims, wiELiabilities)),
                                  na.rm = T)
timeseries$wiEDirectInv <- rowMeans(subset(timeseries,
                                           select = c(wiEDII, wiEDIdO)),
                                    na.rm = T)
timeseries$wiEPortInv <- rowMeans(subset(timeseries,
                                         select = c(wiEPIA, wiEPIdL)),
                                  na.rm = T)
timeseries$wiETrade <- rowMeans(subset(timeseries,
                                       select = c(wiEExport, wiEImport)),
                                na.rm = T)


# .... Plot ####
timeseries <- timeseries %>% select(rIncome:year, wiVBanking:wiETrade)
timeseries <- melt(timeseries, 
                   id.vars = c("rIncome", "year"),
                   measure.vars = c("wiVBanking", "wiVDirectInv", "wiVPortInv", "wiVTrade",
                                    "wiIBanking", "wiIDirectInv", "wiIPortInv", "wiITrade",
                                    "wiEBanking", "wiEDirectInv", "wiEPortInv", "wiETrade"))

HIC <- subset(timeseries, rIncome == "HIC" & value != "Inf")
UMC <- subset(timeseries, rIncome == "UMC" & value != "Inf")
LMC <- subset(timeseries, rIncome == "LMC" & value != "Inf")
LIC <- subset(timeseries, rIncome == "LIC" & value != "Inf")

measure <- c("V", "I", "E")
measure.label <- c("Vulnerability", "Intensity", "Exposure")

incomegroup <- c("HIC", "UMC", "LMC", "LIC")
incomegroup.label <- c("high income countries", "upper-middle income countries", 
                       "lower-middle income countries", "lower income countries")

for (m in 1:length(measure)){
  for (i in 1:length(incomegroup)){
    g <- ggplot(get(incomegroup[i]) %>% filter(variable == paste0("wi", measure[m], "Banking") |
                                                 variable == paste0("wi", measure[m], "DirectInv") |
                                                 variable == paste0("wi", measure[m], "PortInv") |
                                                 variable == paste0("wi", measure[m], "Trade")),
                aes(x = year, y = value, color = variable)) + 
      geom_line(size = 1.5) +
      ggtitle(paste0(measure.label[m], " over time in ", incomegroup.label[i]),
              subtitle = "Overall Secrecy Score") + 
      xlab("Year") + ylab(paste0(measure.label[m], " Score")) +
      scale_color_manual(labels = c("Banking Positions","Direct Investment",
                                    "Portfolio Investment", "Trade"),
                         values = rev(wes_palette("GrandBudapest1"))) +
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks= pretty_breaks())
    ggsave(g, 
           file = paste0("Figures/Overall Secrecy Score/Scores over time/For income groups/", measure[m], "_", incomegroup[i],".pdf"), 
           width = 6, height = 5, units = "in")
  }
}


# .... Export ####
timeseries <- timeseries[order(timeseries$variable, timeseries$rIncome, timeseries$year), ]
write.csv(timeseries, "Results/VIE time series_for income groups_Secrecy Score.csv", row.names = FALSE)


# .. Housekeeping ####
rm(g, i, incomegroup, incomegroup.label, m, measure, measure.label, r, region, region.label,
   af, am, as, eu, oc, HIC, UMC, LMC, LIC)
rm(timeseries)



## ## ## ## ## ## ## ## ## ## ##
# YEARLY SNAPSHOT GRAPHS    ####
## ## ## ## ## ## ## ## ## ## ##

measure <- c("V", "I", "E")
wrmeasure <- c("wrV", "wrI", "wrE")
wimeasure <- c("wiV", "wiI", "wiE")
measure.label <- c("Vulnerability", "Intensity", "Exposure")

region <- c("af", "am", "as", "eu", "oc", "nr")
region.label <- c("Africa", "Americas", "Asia", "Europe", "Oceania", "No Region")

incomegroup <- c("HIC", "UMC", "LMC", "LIC", "ni")
incomegroup.label <- c("HIC", "UMC", "LMC", "LIC", "No Income Group")

flowstock <- c("Banking Positions", "Direct Investment", "Portfolio Investment", "Trade")

vars <- list()
vars$BankingPositions <- c("Claims", "Liabilities")
vars$DirectInvestment <- c("DII", "DIdO")
vars$PortInvestment <- c("PIA", "PIdL")
vars$Trade <- c("Export", "Import")

var.labels <- list()
var.labels$BankingPositions <- c("Claims", "Liabilities")
var.labels$DirectInvestment <- c("Inward", "Outward (derived)")
var.labels$PortInvestment <- c("Assets", "Liabilities (derived)")
var.labels$Trade <- c("Exports", "Imports")

cols <- list()
cols$BankingPositions <- c("#899DA4", "#C93312")
cols$DirectInvestment <- c("#7294D4", "#FD6467")
cols$PortInvestment <- c("#00A08A", "#F98400")
cols$Trade <- c("#FAD510", "#800080")

graph <- melt(panelSJ,
              id.vars = c("id", "reporter.ISO", "partner.ISO", "year",
                          "reporter", "partner", "rRegion", "rIncome", "pRegion", "pIncome"),
              measure.vars = c("Claims", "Liabilities",
                               "DIdI", "DII", "DIdO", "DIO", 
                               "PIA", "PIL", "PIdL",
                               "Export", "Import", "ReExport", "ReImport",
                               "rSecrecyScore", "pSecrecyScore", "rSS", "pSS",
                               "rGDP", "rGNIPerCap", "pGDP", "pGNIPerCap",
                               "TotClaims", "TotLiabilities",
                               "TotDIdI", "TotDII", "TotDIdO", "TotDIO", 
                               "TotPIA", "TotPIL", "TotPIdL",
                               "TotExport", "TotImport",
                               "VClaims", "VLiabilities",
                               "VDIdI", "VDII", "VDIdO", "VDIO",
                               "VPIA", "VPIL", "VPIdL",
                               "VExport", "VImport",
                               "IClaims", "ILiabilities",
                               "IDIdI", "IDII", "IDIdO", "IDIO",
                               "IPIA", "IPIL", "IPIdL",
                               "IExport", "IImport",
                               "EClaims", "ELiabilities",
                               "EDIdI", "EDII", "EDIdO", "EDIO",
                               "EPIA", "EPIL", "EPIdL",
                               "EExport", "EImport",
                               "wrTotClaims", "wrTotLiabilities",
                               "wrTotDIdI", "wrTotDII", "wrTotDIdO", "wrTotDIO",
                               "wrTotPIA", "wrTotPIL", "wrTotPIdL",
                               "wrTotExport", "wrTotImport",
                               "wrVClaims", "wrVLiabilities",
                               "wrVDIdI", "wrVDII", "wrVDIdO", "wrVDIO",
                               "wrVPIA", "wrVPIL", "wrVPIdL",
                               "wrVExport", "wrVImport",
                               "wrrGDP",
                               "wrIClaims", "wrILiabilities",
                               "wrIDIdI", "wrIDII", "wrIDIdO", "wrIDIO",
                               "wrIPIA", "wrIPIL", "wrIPIdL",
                               "wrIExport", "wrIImport",
                               "wrEClaims", "wrELiabilities",
                               "wrEDIdI", "wrEDII", "wrEDIdO", "wrEDIO",
                               "wrEPIA", "wrEPIL", "wrEPIdL",
                               "wrEExport", "wrEImport",
                               "wiTotClaims", "wiTotLiabilities",
                               "wiTotDIdI", "wiTotDII", "wiTotDIdO", "wiTotDIO",
                               "wiTotPIA", "wiTotPIL", "wiTotPIdL",
                               "wiTotExport", "wiTotImport",
                               "wiVClaims", "wiVLiabilities",
                               "wiVDIdI", "wiVDII", "wiVDIdO", "wiVDIO",
                               "wiVPIA", "wiVPIL", "wiVPIdL",
                               "wiVExport", "wiVImport",
                               "wirGDP",
                               "wiIClaims", "wiILiabilities",
                               "wiIDIdI", "wiIDII", "wiIDIdO", "wiIDIO",
                               "wiIPIA", "wiIPIL", "wiIPIdL",
                               "wiIExport", "wiIImport",
                               "wiEClaims", "wiELiabilities",
                               "wiEDIdI", "wiEDII", "wiEDIdO", "wiEDIO",
                               "wiEPIA", "wiEPIL", "wiEPIdL",
                               "wiEExport", "wiEImport"))

graph <- subset(graph, !is.na(value))

choose.year <- 2015

af <- subset(graph, rRegion == "Africa")
am <- subset(graph, rRegion == "Americas")
as <- subset(graph, rRegion == "Asia")
eu <- subset(graph, rRegion == "Europe")
oc <- subset(graph, rRegion == "Oceania")
nr <- subset(graph, rRegion == "")

HIC <- subset(graph, rIncome == "HIC")
UMC <- subset(graph, rIncome == "UMC")
LMC <- subset(graph, rIncome == "LMC")
LIC <- subset(graph, rIncome == "LIC")
ni <- subset(graph, rIncome == "")


# .. Plot jurisdiction-level scores ####

# .... By region ####
for (m in 1:length(measure)){
  for (r in 1:length(region)){
    for (f in 1:length(flowstock)){
      g <- ggplot(get(region[r]) %>% filter((variable == paste0(measure[m], vars[[f]][1]) |
                                               variable == paste0(measure[m], vars[[f]][2])) &
                                              year == choose.year & value != 0) %>%
                    distinct(reporter, variable, .keep_all = TRUE),
                  aes(x = reorder(reporter, value, sum), y = value, fill = fct_rev(variable))) +
        geom_col() + coord_flip() +
        ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in ", region.label[r], " in ", choose.year)) +
        xlab("Reporting country") + ylab(paste0(measure.label[m], " Score")) +
        guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
        scale_fill_manual(labels = rev(c(var.labels[[f]][1], var.labels[[f]][2])),
                          values = c(cols[[f]][1], cols[[f]][2])) +
        scale_y_continuous(labels = comma)
      ggsave(g,
             file = paste0("Figures/Overall Secrecy Score/Jurisdiction scores/Yearly snapshots/", measure[m], "_", names(vars)[f], "_", region.label[r], "_", choose.year,".pdf"),
             width = 6, height = 5, units = "in")
    }
  }
}


# .... By income group ####
for (m in 1:length(measure)){
  for (i in 1:length(incomegroup)){
    for (f in 1:length(flowstock)){
      g <- ggplot(get(incomegroup[i]) %>% filter((variable == paste0(measure[m], vars[[f]][1]) |
                                                    variable == paste0(measure[m], vars[[f]][2])) &
                                                   year == choose.year & value != 0) %>%
                    distinct(reporter, variable, .keep_all = TRUE),
                  aes(x = reorder(reporter, value, sum), y = value, fill = fct_rev(variable))) +
        geom_col() + coord_flip() +
        ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in ", incomegroup.label[i], " in ", choose.year)) +
        xlab("Reporting country") + ylab(paste0(measure.label[m], " Score")) +
        guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
        scale_fill_manual(labels = rev(c(var.labels[[f]][1], var.labels[[f]][2])),
                          values = c(cols[[f]][1], cols[[f]][2])) +
        scale_y_continuous(labels = comma)
      ggsave(g,
             file = paste0("Figures/Overall Secrecy Score/Jurisdiction scores/Yearly snapshots/", measure[m], "_", names(vars)[f], "_", incomegroup.label[i], "_", choose.year,".pdf"),
             width = 6, height = 5, units = "in")
    }
  }
}


# .. Plot region-level scores ####
for (m in 1:length(wrmeasure)){
  for (f in 1:length(flowstock)){
    g <- ggplot(graph %>% filter((variable == paste0(wrmeasure[m], vars[[f]][1]) |
                                    variable == paste0(wrmeasure[m], vars[[f]][2])) &
                                   year == choose.year & value != 0 & rRegion != "") %>%
                  distinct(rRegion, variable, .keep_all = TRUE),
                aes(x = reorder(rRegion, value, sum), y = value, fill = fct_rev(variable))) +
      geom_col() + coord_flip() +
      ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in ", choose.year)) +
      xlab("Region") + ylab(paste0(measure.label[m], " Score")) +
      guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
      scale_fill_manual(labels = rev(c(var.labels[[f]][1], var.labels[[f]][2])),
                        values = c(cols[[f]][1], cols[[f]][2])) +
      scale_y_continuous(labels = comma)
    ggsave(g,
           file = paste0("Figures/Overall Secrecy Score/Regional scores/Yearly snapshots/", wrmeasure[m], "_", names(vars)[f], "_", choose.year,".pdf"),
           width = 6, height = 5, units = "in")
  }
}


# .. Plot income group-level scores ####
for (m in 1:length(wimeasure)){
  for (f in 1:length(flowstock)){
    g <- ggplot(graph %>% filter((variable == paste0(wimeasure[m], vars[[f]][1]) |
                                    variable == paste0(wimeasure[m], vars[[f]][2])) &
                                   year == choose.year & value != 0 & rIncome != "") %>%
                  distinct(rIncome, variable, .keep_all = TRUE),
                aes(x = reorder(rIncome, value, sum), y = value, fill = fct_rev(variable))) +
      geom_col() + coord_flip() +
      ggtitle(paste0(measure.label[m], " of ", flowstock[f], " in ", choose.year)) +
      xlab("Income group") + ylab(paste0(measure.label[m], " Score")) +
      guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
      scale_fill_manual(labels = rev(c(var.labels[[f]][1], var.labels[[f]][2])),
                        values = c(cols[[f]][1], cols[[f]][2])) +
      scale_y_continuous(labels = comma)
    ggsave(g,
           file = paste0("Figures/Overall Secrecy Score/Income group scores/Yearly snapshots/", wimeasure[m], "_", names(vars)[f], "_", choose.year,".pdf"),
           width = 6, height = 5, units = "in")
  }
}


# .. Housekeeping ####
rm(choose.year, cols, f, flowstock, g, i, incomegroup, incomegroup.label,
   m, measure, wrmeasure, wimeasure, measure.label, r, region, region.label,
   var.labels, vars,
   af, am, as, eu, oc, nr, HIC, UMC, LMC, LIC, ni)
rm(graph)



## ## ## ## ## ## ## ## ## ## ##
# EXPORT RESULTS            ####
## ## ## ## ## ## ## ## ## ## ##

missing <- which(rowSums(is.na(select(panelSJ, VClaims:wiEImport))) ==
                   ncol(select(panelSJ, VClaims:wiEImport)))
rm(missing)
allzero <- which(rowSums(select(panelSJ, VClaims:wiEImport), na.rm = T) == 0)
rm(allzero)

results <- panelSJ %>%
  group_by(rRegion, year) %>%
  distinct(reporter, .keep_all = TRUE) %>%
  select(reporter, reporter.ISO, year, rRegion, rIncome,
         VClaims:VImport,
         IClaims:IImport,
         EClaims:EImport,
         wrVClaims:wrVImport,
         wrIClaims:wrIImport,
         wrEClaims:wrEImport,
         wiVClaims:wiVImport,
         wiIClaims:wiIImport,
         wiEClaims:wiEImport) %>%
  arrange(rRegion, reporter, year) %>%
  ungroup()

write.csv(results, "Results/Results_Secrecy Score.csv", row.names = FALSE)
rm(results)
