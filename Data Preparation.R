# Data Preparation
# Alice Lepissier

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Codes Masterlist
# LBS Data
# .. Sum quarterly data to yearly
# .. Merge reporter codes
# .. Merge partner codes
# .. Convert years to long
# .. Convert banking positions to wide
# .. Generate unique identifier
# CDIS Data
# .. Merge reporter codes
# .. Merge partner codes
# .. Generate unique identifier
# CPIS Data
# .. Merge reporter codes
# .. Merge partner codes
# .. Generate unique identifier
# Comtrade Data
# .. Generate unique identifier
# .. Convert to wide
# Merge Flow-Stock Data
# .. Complete cases
# FSI Data
# .. Merge country codes
# Merge FSI with Panel
# WDI Data
# .. Merge country codes
# Merge WDI with Panel
# .. Complete cases
# Individual KFSIs
# .. Merge country codes
# Merge KFSIs with Panel
# Import Groupings
# .. Merge reporters
# .. Merge partners
# .. Import country names
# Export Clean Data



## ## ## ## ## ## ## ## ## ## ##
# CODES MASTERLIST          ####
## ## ## ## ## ## ## ## ## ## ##

codes <- read.xlsx2("Data/Codes_Masterlist.xlsx", sheetName = "Codes")
codes$ISO3166.3 <- as.character(codes$ISO3166.3)
codes$ISO3166.2 <- as.character(codes$ISO3166.2)



## ## ## ## ## ## ## ## ## ## ##
# LBS DATA                  ####
## ## ## ## ## ## ## ## ## ## ##

LBS <- read.csv("Data/LBS/LBS.csv", fileEncoding="UTF-8-BOM")
LBS <- subset(LBS, 
              select = -c(X, Frequency, Measure, Currency.denomination,
                          Currency.type.of.reporting.country, Parent.country,
                          Type.of.reporting.institutions, Position.type, Period))
LBS <- subset(LBS, Counterparty.sector == "A:All sectors")
LBS <- subset(LBS, Type.of.instruments == "A:All instruments")
LBS <- subset(LBS, select = -c(Type.of.instruments, Counterparty.sector))


# .. Sum quarterly data to yearly ####
LBS <- LBS[, -grep(number_range(1977,2007), names(LBS))]
LBS[LBS == "\\" | LBS == "-"] <- NA
LBS[4:44] <- lapply(LBS[4:44], function(x) as.numeric(as.character(x)))
LBS$yr2008 <- rowSums(LBS[grep("2008", names(LBS))], na.rm = T)
LBS$yr2009 <- rowSums(LBS[grep("2009", names(LBS))], na.rm = T)
LBS$yr2010 <- rowSums(LBS[grep("2010", names(LBS))], na.rm = T)
LBS$yr2011 <- rowSums(LBS[grep("2011", names(LBS))], na.rm = T)
LBS$yr2012 <- rowSums(LBS[grep("2012", names(LBS))], na.rm = T)
LBS$yr2013 <- rowSums(LBS[grep("2013", names(LBS))], na.rm = T)
LBS$yr2014 <- rowSums(LBS[grep("2014", names(LBS))], na.rm = T)
LBS$yr2015 <- rowSums(LBS[grep("2015", names(LBS))], na.rm = T)
LBS$yr2016 <- rowSums(LBS[grep("2016", names(LBS))], na.rm = T)
LBS$yr2017 <- rowSums(LBS[grep("2017", names(LBS))], na.rm = T)
LBS$yr2018 <- rowSums(LBS[grep("2018", names(LBS))], na.rm = T)
LBS[grep("X", names(LBS))] <- NULL

LBS$Balance.sheet.position <- substring(LBS$Balance.sheet.position, 3)
LBS$Reporting.country <- substring(LBS$Reporting.country, 4)
LBS$Counterparty.country <- substring(LBS$Counterparty.country, 4)


# .. Merge reporter codes ####
colnames(LBS)[colnames(LBS) == "Reporting.country"] <- "Country"
LBS <- merge(LBS, codes[, c("Country", "ISO3166.3")], by = "Country", all.x = TRUE)
noISO <- subset(LBS, is.na(LBS$ISO3166.3))
unique(noISO$Country)
# Only aggregates remain, we can drop the NAs.
LBS <- subset(LBS, !is.na(LBS$ISO3166.3))
colnames(LBS)[colnames(LBS) == "Country"] <- "reporter"
colnames(LBS)[colnames(LBS) == "ISO3166.3"] <- "reporter.ISO"


# .. Merge partner codes ####
colnames(LBS)[colnames(LBS) == "Counterparty.country"] <- "Country"
LBS <- merge(LBS, codes[, c("Country", "ISO3166.3")], by = "Country", all.x = TRUE)
noISO <- subset(LBS, is.na(LBS$ISO3166.3))
unique(noISO$Country)
# Only aggregates or dissolved states remain, we can drop the NAs.
LBS <- subset(LBS, !is.na(LBS$ISO3166.3))
colnames(LBS)[colnames(LBS) == "Country"] <- "partner"
colnames(LBS)[colnames(LBS) == "ISO3166.3"] <- "partner.ISO"


# .. Convert years to long ####
LBS <- gather(LBS, year, value, yr2008:yr2018)
LBS$year <- as.numeric(substring(LBS$year, 3))
LBS <- subset(LBS, value != 0)
LBS$value <- LBS$value*10^6
# Data is given in millions


# .. Convert banking positions to wide ####
LBS[LBS == "Total claims"] <- "Claims"
LBS[LBS == "Total liabilities"] <- "Liabilities"
LBS <- spread(LBS, Balance.sheet.position, value)
missing <- apply(LBS, MARGIN = 1, function(x) sum(is.na(x))) == 2
LBS <- LBS[which(missing == FALSE),]
rm(missing)


# .. Generate unique identifier ####
LBS$id <- paste(LBS$reporter.ISO, LBS$partner.ISO, LBS$year, sep = "_")
LBS <- LBS[, c("id", "reporter", "reporter.ISO",
               "partner", "partner.ISO", "year",
               "Claims", "Liabilities")]
LBS <- LBS[order(LBS$id), ]
rm(noISO)



## ## ## ## ## ## ## ## ## ## ##
# CDIS DATA                 ####
## ## ## ## ## ## ## ## ## ## ##

CDIS <- read.csv("Data/CDIS/CDIS.csv", fileEncoding="UTF-8-BOM")
CDIS <- subset(CDIS, 
               select = -c(Status, Status.1, Status.2, Status.3, X,
                           Country.Code, Counterpart.Country.Code))
colnames(CDIS) <- c("reporter", "partner",
                    "year", 
                    "DIdI", "DII",
                    "DIdO", "DIO")
CDIS[CDIS == 0] <- NA


# .. Merge reporter codes ####
colnames(CDIS)[colnames(CDIS) == "reporter"] <- "Country"
CDIS <- merge(CDIS, codes[, c("Country", "ISO3166.3")], by = "Country", all.x = TRUE)
noISO <- subset(CDIS, is.na(CDIS$ISO3166.3))
unique(noISO$Country)
# Only aggregates remain, we can drop the NAs.
CDIS <- subset(CDIS, !is.na(CDIS$ISO3166.3))
colnames(CDIS)[colnames(CDIS) == "Country"] <- "reporter"
colnames(CDIS)[colnames(CDIS) == "ISO3166.3"] <- "reporter.ISO"


# .. Merge partner codes ####
colnames(CDIS)[colnames(CDIS) == "partner"] <- "Country"
CDIS <- merge(CDIS, codes[, c("Country", "ISO3166.3")], by = "Country", all.x = TRUE)
noISO <- subset(CDIS, is.na(CDIS$ISO3166.3))
unique(noISO$Country)
# Only aggregates remain, we can drop the NAs.
CDIS <- subset(CDIS, !is.na(CDIS$ISO3166.3))
colnames(CDIS)[colnames(CDIS) == "Country"] <- "partner"
colnames(CDIS)[colnames(CDIS) == "ISO3166.3"] <- "partner.ISO"


# .. Generate unique identifier ####
CDIS$id <- paste(CDIS$reporter.ISO, CDIS$partner.ISO, CDIS$year, sep = "_")
CDIS <- CDIS[, c("id", "reporter", "reporter.ISO",
                 "partner", "partner.ISO", "year",
                 "DIdI", "DII",
                 "DIdO", "DIO")]
CDIS <- CDIS[order(CDIS$id), ]
CDIS$reporter <- as.character(CDIS$reporter)
CDIS$partner <- as.character(CDIS$partner)
missing <- apply(CDIS, MARGIN = 1, function(x) sum(is.na(x))) == 4
CDIS <- CDIS[which(missing == FALSE),]
rm(noISO, missing)



## ## ## ## ## ## ## ## ## ## ##
# CPIS DATA                 ####
## ## ## ## ## ## ## ## ## ## ##

CPIS <- read.csv("Data/CPIS/CPIS.csv", fileEncoding="UTF-8-BOM")
CPIS <- subset(CPIS, 
               select = -c(Country.Code, Counterpart.Country.Code,
                           Counterpart.Sector.Name, Counterpart.Sector.Code,
                           Sector.Name, Sector.Code, 
                           Status, Status.1, Status.2, X))
colnames(CPIS) <- c("reporter", "partner",
                    "year", 
                    "PIA",
                    "PIdL", "PIL")
CPIS <- subset(CPIS, year >= 2008)
CPIS[CPIS == 0] <- NA


# .. Merge reporter codes ####
colnames(CPIS)[colnames(CPIS) == "reporter"] <- "Country"
CPIS <- merge(CPIS, codes[, c("Country", "ISO3166.3")], by = "Country", all.x = TRUE)
noISO <- subset(CPIS, is.na(CPIS$ISO3166.3))
unique(noISO$Country)
# Only aggregates remain, we can drop the NAs.
CPIS <- subset(CPIS, !is.na(CPIS$ISO3166.3))
colnames(CPIS)[colnames(CPIS) == "Country"] <- "reporter"
colnames(CPIS)[colnames(CPIS) == "ISO3166.3"] <- "reporter.ISO"


# .. Merge partner codes ####
colnames(CPIS)[colnames(CPIS) == "partner"] <- "Country"
CPIS <- merge(CPIS, codes[, c("Country", "ISO3166.3")], by = "Country", all.x = TRUE)
noISO <- subset(CPIS, is.na(CPIS$ISO3166.3))
unique(noISO$Country)
# Only aggregates remain, we can drop the NAs.
CPIS <- subset(CPIS, !is.na(CPIS$ISO3166.3))
colnames(CPIS)[colnames(CPIS) == "Country"] <- "partner"
colnames(CPIS)[colnames(CPIS) == "ISO3166.3"] <- "partner.ISO"


# .. Generate unique identifier ####
CPIS$id <- paste(CPIS$reporter.ISO, CPIS$partner.ISO, CPIS$year, sep = "_")
CPIS <- CPIS[, c("id", "reporter", "reporter.ISO",
                 "partner", "partner.ISO", "year",
                 "PIA", "PIL", "PIdL")]
CPIS <- CPIS[order(CPIS$id), ]
CPIS$reporter <- as.character(CPIS$reporter)
CPIS$partner <- as.character(CPIS$partner)
missing <- apply(CPIS, MARGIN = 1, function(x) sum(is.na(x))) == 3
CPIS <- CPIS[which(missing == FALSE),]
rm(noISO, missing)



## ## ## ## ## ## ## ## ## ## ##
# COMTRADE DATA             ####
## ## ## ## ## ## ## ## ## ## ##

comtrade <- read.csv("Data/Comtrade/comtrade.csv", fileEncoding="UTF-8-BOM")
summary(comtrade$pfCode)
comtrade <- subset(comtrade, pfCode != "No data matches your query or your query is too complex. Request JSON or XML format for more information.")
comtrade <- subset(comtrade,
                   select = c(TradeValue, period, pt3ISO, ptTitle,
                              rgDesc, rt3ISO, rtTitle))
colnames(comtrade) <- c("TradeValue", "year",
                        "partner.ISO", "partner", 
                        "flow", "reporter.ISO", "reporter")
comtrade[comtrade == 0] <- NA

summary(comtrade$reporter.ISO)
noreporter <- subset(comtrade, reporter.ISO == "")
summary(noreporter$reporter)
# All are "Other Asia, nes"
nopartner <- subset(comtrade, partner.ISO == "")
summary(nopartner$partner)
# Other Asia, nes, Areas, nes, Other Europe, nes, Other Africa, nes
# Bunkers, Free Zones, Special Categories, Oceania, nes,
# North America and Central America, nes, LAIA, nes,
# Neutral Zone, Br. Antarctic Terr.
rm(noreporter, nopartner)
comtrade <- subset(comtrade,
                   reporter.ISO != "" & partner.ISO != "")
sum(is.na(comtrade$TradeValue))
comtrade <- subset(comtrade, !is.na(TradeValue))


# .. Generate unique identifier ####
comtrade$id <- paste(comtrade$reporter.ISO, comtrade$partner.ISO, comtrade$year, sep = "_")
comtrade <- comtrade[, c("id", "reporter", "reporter.ISO",
                         "partner", "partner.ISO", "year",
                         "flow", "TradeValue")]
comtrade <- comtrade[order(comtrade$id), ]
comtrade$reporter <- as.character(comtrade$reporter)
comtrade$partner <- as.character(comtrade$partner)
comtrade <- comtrade[order(comtrade$id), ]


# .. Convert to wide ####
comtrade <- spread(comtrade, flow, TradeValue)
colnames(comtrade)[colnames(comtrade) == "Re-Export"] <- "ReExport"
colnames(comtrade)[colnames(comtrade) == "Re-Import"] <- "ReImport"



## ## ## ## ## ## ## ## ## ## ##
# MERGE FLOW-STOCK DATA     ####
## ## ## ## ## ## ## ## ## ## ##

panel <- merge(LBS[, c("id",
                       "Claims", "Liabilities")],
               CDIS[, c("id",
                        "DIdI", "DII", "DIdO", "DIO")],
               by = c("id"),
               all = TRUE)
               
panel <- merge(panel,
               CPIS[, c("id",
                        "PIA", "PIL", "PIdL")],
               by = c("id"),
               all = TRUE)

panel <- merge(panel, 
               comtrade[, c("id",
                            "Export", "Import", "ReExport", "ReImport")],
               by = c("id"),
               all = TRUE)

panel <- panel[order(panel$id), ]


# .. Complete cases ####
allmissing <- which(rowSums(is.na(panel)) == ncol(panel)-1)
# None
rm(allmissing)
subset(panel, 
       Claims == 0 & Liabilities == 0,
       DIdI == 0 & DII == 0 & DIdO == 0 & DIO == 0 &
       PIA == 0 & PIL == 0 & PIdL == 0 &
       Export == 0 & Import == 0 & ReExport == 0 & ReImport == 0)
# None



## ## ## ## ## ## ## ## ## ## ##
# FSI DATA                  ####
## ## ## ## ## ## ## ## ## ## ##

FSI <- read.xlsx("Data/FSI/FSI-Rankings-2018.xlsx", sheetName = "FSI Results")
FSI$Jurisdiction<- gsub("2$", "", FSI$Jurisdiction)
FSI <- subset(FSI,
              select = c(Jurisdiction, Secrecy.Score4))


# .. Merge country codes ####
colnames(FSI)[colnames(FSI) == "Jurisdiction"] <- "Country"
FSI <- merge(FSI, codes[, c("Country", "ISO3166.3")], by = "Country", all.x = TRUE)
noISO <- subset(FSI, is.na(FSI$ISO3166.3))
unique(noISO$Country)
# Only footnotes remain, we can drop the NAs.
FSI <- subset(FSI, !is.na(FSI$ISO3166.3))
FSI$reporter <- FSI$Country
FSI$partner <- FSI$Country
FSI$reporter.ISO <- as.character(FSI$ISO3166.3)
FSI$partner.ISO <- as.character(FSI$ISO3166.3)
FSI$rSecrecyScore <- FSI$Secrecy.Score4
FSI$pSecrecyScore <- FSI$Secrecy.Score4
FSI <- subset(FSI,
              select = -c(Secrecy.Score4, Country, ISO3166.3))
rm(noISO)



## ## ## ## ## ## ## ## ## ## ##
# MERGE FSI WITH PANEL      ####
## ## ## ## ## ## ## ## ## ## ##

panel$reporter.ISO <- substr(panel$id, 1, 3)
panel$partner.ISO <- substr(panel$id, 5, 7)

panel <- merge(panel,
               FSI[, c("reporter.ISO", "rSecrecyScore")],
               by = c("reporter.ISO"),
               all = TRUE)
panel <- merge(panel,
               FSI[, c("partner.ISO", "pSecrecyScore")],
               by = c("partner.ISO"),
               all = TRUE)

panel$rSS <- ifelse(!is.na(panel$rSecrecyScore), TRUE, FALSE)
panel$pSS <- ifelse(!is.na(panel$pSecrecyScore), TRUE, FALSE)

with(panel, CrossTable(reporter.ISO, rSS, missing.include = TRUE, 
                       prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE,
                       format = "SPSS"))
unique(panel$reporter.ISO[panel$rSS == FALSE])



## ## ## ## ## ## ## ## ## ## ##
# WDI DATA                  ####
## ## ## ## ## ## ## ## ## ## ##

WDI <- WDI(country = "all", indicator = c("NY.GDP.MKTP.CD", "NY.GNP.PCAP.CD"), 
           start = 2008, end = 2018)
missing <- apply(WDI, MARGIN = 1, function(x) sum(is.na(x))) == 2
WDI <- WDI[which(missing == FALSE),]
rm(missing)


# .. Merge country codes ####
colnames(WDI)[colnames(WDI) == "country"] <- "Country"
WDI <- merge(WDI, codes[, c("Country", "ISO3166.3")], by = "Country", all.x = TRUE)
noISO <- subset(WDI, is.na(WDI$ISO3166.3))
unique(noISO$Country)
# Only aggregates remain, we can drop the NAs.
WDI <- subset(WDI, !is.na(WDI$ISO3166.3))
WDI$reporter <- WDI$Country
WDI$partner <- WDI$Country
WDI$reporter.ISO <- as.character(WDI$ISO3166.3)
WDI$partner.ISO <- as.character(WDI$ISO3166.3)
WDI$rGDP <- WDI$NY.GDP.MKTP.CD
WDI$pGDP <- WDI$NY.GDP.MKTP.CD
WDI$rGNIPerCap <- WDI$NY.GNP.PCAP.CD
WDI$pGNIPerCap <- WDI$NY.GNP.PCAP.CD
WDI <- subset(WDI,
              select = -c(Country, iso2c, NY.GDP.MKTP.CD,
                          NY.GNP.PCAP.CD, ISO3166.3))
WDI <- WDI[order(WDI$reporter.ISO, WDI$year),]
rm(noISO)



## ## ## ## ## ## ## ## ## ## ##
# MERGE WDI WITH PANEL      ####
## ## ## ## ## ## ## ## ## ## ##

panel$year <- as.numeric(substr(panel$id, 9, 12))

panel <- merge(panel,
               WDI[, c("reporter.ISO", "year", "rGDP", "rGNIPerCap")],
               by = c("reporter.ISO", "year"),
               all = TRUE)
panel <- merge(panel,
               WDI[, c("partner.ISO", "year", "pGDP", "pGNIPerCap")],
               by = c("partner.ISO", "year"),
               all = TRUE)
panel <- panel %>% 
  select(id, reporter.ISO, partner.ISO, year, everything())


# .. Complete cases ####
allFSmissing <- which(rowSums(is.na(select(panel, Claims:ReImport))) == 
                        ncol(select(panel, Claims:ReImport)))
panel <- panel[-c(allFSmissing),]
rm(allFSmissing)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT GROUPINGS          ####
## ## ## ## ## ## ## ## ## ## ##

# .. Merge reporters ####
colnames(panel)[colnames(panel) == "reporter.ISO"] <- "ISO3166.3"
panel <- merge(panel, codes[match(unique(codes$ISO3166.3), codes$ISO3166.3),
                            c("ISO3166.3", "UN_Region", "WB_Income_Group_Code")], 
               by.x = "ISO3166.3", all.x = TRUE)
colnames(panel)[colnames(panel) == "ISO3166.3"] <- "reporter.ISO"
colnames(panel)[colnames(panel) == "UN_Region"] <- "rRegion"
colnames(panel)[colnames(panel) == "WB_Income_Group_Code"] <- "rIncome"


# .. Merge partners ####
colnames(panel)[colnames(panel) == "partner.ISO"] <- "ISO3166.3"
panel <- merge(panel, codes[match(unique(codes$ISO3166.3), codes$ISO3166.3),
                            c("ISO3166.3", "UN_Region", "WB_Income_Group_Code")],
               by = "ISO3166.3", all.x = TRUE)
colnames(panel)[colnames(panel) == "ISO3166.3"] <- "partner.ISO"
colnames(panel)[colnames(panel) == "UN_Region"] <- "pRegion"
colnames(panel)[colnames(panel) == "WB_Income_Group_Code"] <- "pIncome"


# .. Import country names ####
colnames(panel)[colnames(panel) == "reporter.ISO"] <- "ISO3166.3"
panel <- merge(panel, aggregate(Country ~ ISO3166.3, data = codes, head, 1),
               by = "ISO3166.3", all.x = TRUE)
colnames(panel)[colnames(panel) == "ISO3166.3"] <- "reporter.ISO"
colnames(panel)[colnames(panel) == "Country"] <- "reporter"
missing <- which(is.na(panel$reporter))
# EU27
panel <- panel[-c(missing),]
rm(missing)

colnames(panel)[colnames(panel) == "partner.ISO"] <- "ISO3166.3"
panel <- merge(panel, aggregate(Country ~ ISO3166.3, data = codes, head, 1),
               by = "ISO3166.3", all.x = TRUE)
colnames(panel)[colnames(panel) == "ISO3166.3"] <- "partner.ISO"
colnames(panel)[colnames(panel) == "Country"] <- "partner"
missing <- which(is.na(select(panel, partner)))
# WLD
panel <- subset(panel, !is.na(panel$partner))
rm(missing)

panel$reporter <- as.character(panel$reporter)
panel$partner <- as.character(panel$partner)

panel$rRegion <- factor(panel$rRegion,
                        levels = c("Africa",
                                   "Americas",
                                   "Asia",
                                   "Europe",
                                   "Oceania",
                                   ""))

panel$rIncome <- factor(panel$rIncome,
                        levels = c("LIC",
                                   "LMC",
                                   "UMC",
                                   "HIC",
                                   ""))

panel <- panel %>% 
  select(id, reporter.ISO, partner.ISO, year, reporter, partner, rRegion, rIncome, pRegion, pIncome, 
         everything()) %>%
  arrange(id)



## ## ## ## ## ## ## ## ## ## ##
# INDIVIDUAL KFSIS          ####
## ## ## ## ## ## ## ## ## ## ##

KFSI <- read.xlsx("Data/FSI/FSI-Rankings-2018.xlsx", sheetName = "SS")
KFSI <- KFSI[-1, -ncol(KFSI)]
KFSI <- cbind(KFSI[,1], select(KFSI, KFSI = starts_with("KI.")))
KFSI$Country <- as.character(KFSI$`KFSI[, 1]`)
KFSI$`KFSI[, 1]` <- NULL
KFSI[,1:20] <- lapply(KFSI[,1:20], function(x) as.numeric(as.character(x)))
KFSI[,1:20] <- lapply(KFSI[,1:20], function(x) x*100)


# .. Merge country codes ####
KFSI <- merge(KFSI, codes[, c("Country", "ISO3166.3")], by = "Country", all.x = TRUE)
noISO <- subset(KFSI, is.na(KFSI$ISO3166.3))
unique(noISO$Country)
# No ISO missing
KFSI$reporter <- KFSI$Country
KFSI$partner <- KFSI$Country
KFSI$reporter.ISO <- as.character(KFSI$ISO3166.3)
KFSI$partner.ISO <- as.character(KFSI$ISO3166.3)
colnames(KFSI)[2:21] <- paste("r", colnames(KFSI[c(2:21)]), sep = "")
for (i in 1:20) {
  KFSI[, paste0("pKFSI", i, sep="")] <- KFSI[, paste0("rKFSI", i, sep="")]
}
KFSI <- KFSI %>%
  select(reporter, reporter.ISO, partner, partner.ISO, everything())
KFSI <- subset(KFSI,
               select = -c(Country, ISO3166.3))
rm(noISO, i)



## ## ## ## ## ## ## ## ## ## ##
# MERGE KFSIS WITH PANEL    ####
## ## ## ## ## ## ## ## ## ## ##

panel <- merge(panel,
               KFSI[, c("reporter.ISO",
                        "rKFSI1", "rKFSI2", "rKFSI3", "rKFSI4", "rKFSI5",
                        "rKFSI6", "rKFSI7", "rKFSI8", "rKFSI9", "rKFSI10",
                        "rKFSI11", "rKFSI12", "rKFSI13", "rKFSI14", "rKFSI15",
                        "rKFSI16", "rKFSI17", "rKFSI18", "rKFSI19", "rKFSI20")],
               by = c("reporter.ISO"),
               all = TRUE)
panel <- merge(panel,
               KFSI[, c("partner.ISO",
                        "pKFSI1", "pKFSI2", "pKFSI3", "pKFSI4", "pKFSI5",
                        "pKFSI6", "pKFSI7", "pKFSI8", "pKFSI9", "pKFSI10",
                        "pKFSI11", "pKFSI12", "pKFSI13", "pKFSI14", "pKFSI15",
                        "pKFSI16", "pKFSI17", "pKFSI18", "pKFSI19", "pKFSI20")],
               by = c("partner.ISO"),
               all = TRUE)
panel <- panel %>% 
  select(id, reporter.ISO, partner.ISO, year, reporter, partner, rRegion, rIncome, pRegion, pIncome, 
         everything()) %>%
  arrange(id)



## ## ## ## ## ## ## ## ## ## ##
# EXPORT CLEAN DATA         ####
## ## ## ## ## ## ## ## ## ## ##

save(panel, file = "Data/panel.RData")
write.csv(panel, "Data/panel.csv", row.names = FALSE)

Missing <- apply(subset(panel, select = c(Claims, Liabilities,
                                            DII, DIdO,
                                            PIA, PIdL,
                                            Export, Import)), MARGIN = 1, function(x) sum(is.na(x)))
coverage <- cbind(subset(panel, select = c(id:pIncome,
                                           Claims, Liabilities,
                                           DII, DIdO,
                                           PIA, PIdL,
                                           Export, Import)),
                  as.data.frame(Missing))
write.csv(coverage, "Data/Data availability/Coverage transaction-level.csv", row.names = FALSE)

summary <- summary(LBS)
capture.output(summary, file = "Results/Summary statistics/Summary_LBS.txt")
reporters <- as.data.frame(unique(LBS$reporter))
names(reporters) <- "Reporting countries"
capture.output(reporters, file = "Data/Data availability/Reporting countries_LBS.txt")
write.csv(LBS, "Data/LBS/LBS_clean.csv", row.names = FALSE)
save(LBS, file = "Data/LBS/LBS_clean.Rdata")

summary <- summary(CDIS)
capture.output(summary, file = "Results/Summary statistics/Summary_CDIS.txt")
reporters <- as.data.frame(unique(CDIS$reporter))
names(reporters) <- "Reporting countries"
capture.output(reporters, file = "Data/Data availability/Reporting countries_CDIS.txt")
write.csv(CDIS, "Data/CDIS/CDIS_clean.csv", row.names = FALSE)
save(CDIS, file = "Data/CDIS/CDIS_clean.Rdata")

summary <- summary(CPIS)
capture.output(summary, file = "Results/Summary statistics/Summary_CPIS.txt")
reporters <- as.data.frame(unique(CPIS$reporter))
names(reporters) <- "Reporting countries"
capture.output(reporters, file = "Data/Data availability/Reporting countries_CPIS.txt")
write.csv(CPIS, "Data/CPIS/CPIS_clean.csv", row.names = FALSE)
save(CPIS, file = "Data/CPIS/CPIS_clean.Rdata")

summary <- summary(comtrade)
capture.output(summary, file = "Results/Summary statistics/Summary_Comtrade.txt")
reporters <- as.data.frame(unique(comtrade$reporter))
names(reporters) <- "Reporting countries"
capture.output(reporters, file = "Data/Data availability/Reporting countries_Comtrade.txt")
write.csv(comtrade, "Data/Comtrade/comtrade_clean.csv", row.names = FALSE)
save(comtrade, file = "Data/Comtrade/comtrade_clean.Rdata")

save(FSI, file = "Data/FSI/FSI_clean.Rdata")
save(WDI, file = "Data/WDI/WDI_clean.Rdata")

rm(panel, codes, LBS, CDIS, CPIS, comtrade, FSI, WDI, KFSI, summary, reporters, Missing, coverage)
