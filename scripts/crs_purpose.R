required.packages <- c("data.table", "rstudioapi")
lapply(required.packages, require, character.only=T)
path <- dirname(getActiveDocumentContext()$path)

setwd((dirname(path)))

isos <- fread("Datasets/Countrynames/isos.csv")
crs <- fread("Datasets/OECD DAC CRS/crs_full.csv")
crs <- crs[Year %in% 2019:2023]

crs <- crs[
  FlowName == "ODA Loans" 
  |
    FlowName == "ODA Grants"
  | 
    FlowName == "Equity Investment"
  #| 
  #  flow_name == "Private Development Finance"
]

crs <- crs[Year >= 2014]

crs <- crs[DEDonorcode %in% c(
  "AUS", "AUT", "BEL", "CAN", "CZE", "DNK", "EST", "4EU001", "FIN", "FRA", "DEU",
  "GRC", "HUN", "ISL", "IRL", "ITA", "JPN", "KOR", "LAT", "LUX", "NLD", "NZL",
  "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "GBR", "USA"
)]

#Classify
crs[, purpose := "Development"]
crs[PurposeCode %in% c(15210, 15220, 15230, 15240, 15250, 15261, 15113, 15130, 15150, 15151, 15152, 15153, 15160, 15170, 15180, 15190), purpose := "Peace"]
crs[PurposeCode %in% c(72010, 72040, 72050, 73010, 74020, 12264), purpose := "Humanitarian"]

crs[, climate := "Non-climate"]
crs[ClimateMitigation == 1 | ClimateAdaptation == 1, climate := "Significant climate"]
crs[ClimateMitigation == 2 | ClimateAdaptation == 2, climate := "Principal climate"]

crs_purpose <- crs[, .(disb = sum(USD_Disbursement_Defl, na.rm = T)), by = .(Year, RecipientName, purpose, climate)]

crs_purpose[RecipientName == "TÃ¼rkiye", RecipientName := "Turkey"]
crs_purpose <- merge(crs_purpose, isos[, .(countryname_oecd, iso3)], by.x = "RecipientName", by.y = "countryname_oecd", all.x = T)

setwd(path)
fwrite(crs_purpose, "crs_purpose.csv")

