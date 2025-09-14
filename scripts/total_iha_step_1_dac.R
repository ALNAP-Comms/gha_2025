## Load all the datasets from the C Drive (OECD Data Explorer not working as usual in 2024)
suppressPackageStartupMessages(lapply(c("countrycode", "data.table", "jsonlite","rstudioapi", "httr", "readxl", "XML"), require, character.only=T))

setwd(dirname(dirname(dirname(getActiveDocumentContext()$path))))

dac1 <- fread("OECD DAC Table 1 and 2a/Table1_Data.csv")
dac2afull <- fread("OECD DAC Table 1 and 2a/Table2a_Data.csv")
mumsfull <- fread("OECD DAC Multi/MultiSystem entire dataset.txt")
adv_raw <- read_excel("OECD DAC Preliminary/ADV2024.xlsx", skip = 3)
defl <- fread("Deflators/deflators_2023usd.csv")
isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv")

## Set initial datasets as data tables and vectors
dac1[DONOR == 55, Donor := "Türkiye"]
dac2afull[DONOR == 55, Donor := "Türkiye"]
mumsfull[DonorCode == 55, DonorNameE := "Türkiye"]
country_donors <- unique(dac1[!grepl(", Total", Donor)]$Donor)

dac1 <- dac1[FLOWS == 1140 & AMOUNTTYPE == "D" & Year >= 2010]
dac2a <- dac2afull[RECIPIENT == 10100 & DATATYPE == "D" & AIDTYPE %in% c(216, 240) & Year >= 2010]
dac2a_cmo <- dac2afull[RECIPIENT %in% c(913, 914, 915, 916, 905, 909, 912, 959, 974, 967, 963, 964, 966) & AIDTYPE == 240 & Year >= 2010 & DATATYPE == "D"]
mums <- mumsfull[, .(Value = sum(Amount, na.rm = T)), by = .(Year, AidToOrThru, FlowType, DonorCode, Donor = DonorNameE, Channel = ChannelNameE, AmountType)][
  AidToOrThru == "Core contributions to" & FlowType == "Disbursements" & AmountType == "Constant prices" & Channel %in% c("Adaptation Fund",
                                                                                                                               "African Development Bank",
                                                                                                                               "African Development Fund",
                                                                                                                               "Asian Development Bank",
                                                                                                                               "Asian Development Fund",
                                                                                                                               "Central Emergency Response Fund",
                                                                                                                               "Clean Technology Fund",
                                                                                                                               "Council of Europe Development Bank",
                                                                                                                               "Food and Agricultural Organisation",
                                                                                                                               "Global Environment Facility - Least Developed Countries Fund",
                                                                                                                               "Global Environment Facility - Special Climate Change Fund",
                                                                                                                               "Global Environment Facility Trust Fund",
                                                                                                                               "Inter-American Development Bank, Fund for Special Operations",
                                                                                                                               "Inter-American Development Bank, Inter-American Investment Corporation and Multilateral Investment Fund",
                                                                                                                               "International Development Association",
                                                                                                                               "International Labour Organisation - Assessed Contributions",
                                                                                                                               "International Labour Organisation - Regular Budget Supplementary Account",
                                                                                                                               "International Organisation for Migration",
                                                                                                                               "Nordic Development Fund",
                                                                                                                               "OPEC Fund for International Development",
                                                                                                                               "Strategic Climate Fund",
                                                                                                                               "United Nations Children's Fund",
                                                                                                                               "United Nations Development Programme",
                                                                                                                               "United Nations Environment Programme",
                                                                                                                               "United Nations Office of Co-ordination of Humanitarian Affairs",
                                                                                                                               "United Nations Office of the High Commissioner for Refugees",
                                                                                                                               "United Nations Peacebuilding Fund",
                                                                                                                               "United Nations Population Fund",
                                                                                                                               "United Nations Relief and Works Agency for Palestine Refugees in the Near East",
                                                                                                                               "World Food Programme",
                                                                                                                               "World Health Organisation - assessed contributions",
                                                                                                                               "World Health Organisation - core voluntary contributions account")]




## Create new tables based on datasets by subsetting
dac1_eu <- dac1[dac1$AIDTYPE == 2102,]
dac1_un <- dac1[dac1$AIDTYPE == 2101,]
dac1_ida <- dac1[dac1$AIDTYPE %in% c(547, 2103),]
dac2a_ha <- dac2a[dac2a$AIDTYPE == 216 & Donor %in% country_donors]
mums <- mums[Donor %in% country_donors]

#Current year
current_year <- year(Sys.Date())

#Establish maximum DAC1 year, DAC2a year, and establish base year
dac1_max_year <- max(dac1$Year, na.rm = TRUE)
dac2a_max_year <- max(dac2a$Year, na.rm = TRUE)
mums_max_year <- max(mums$Year, na.rm = TRUE)
dac_base_year <- 2023
min_common_year <- max(sapply(list(dac1$Year, dac2a$Year, dac2a_cmo$Year, mums$Year), min, na.rm = TRUE))

##DAC2a Multilateral HA share
dac2a_moha_share <- dac2a[, .(ha_share = Value[AIDTYPE == 216]/Value[AIDTYPE == 240]), by = .(Year, Donor)]
dac2a_moha_share[Donor %in% c("UNHCR", "UNRWA", "Central Emergency Response Fund [CERF]"), ha_share := 1]

#Standardise dac2a names
{dac2a_cmo[Recipient == "Af. D B", Recipient := "African Development Bank [AfDB]"]
  dac2a_cmo[Recipient == "Asian Dev. Bank", Recipient := "Asian Development Bank [AsDB]"]
  dac2a_cmo[Recipient == "IDA", Recipient := "International Development Association [IDA]"]
  dac2a_cmo[Recipient == "IDB Special Oper. Fund", Recipient := "Inter-American Development Bank [IDB]"]
  dac2a_cmo[Recipient == "AsDB Special Funds", Recipient := "Asian Development Bank [AsDB]"]
  dac2a_cmo[Recipient == "African Dev. Fund", Recipient := "African Development Fund [AfDF]"]
  dac2a_cmo[Recipient == "IDB", Recipient := "Inter-American Development Bank [IDB]"]}

##Calculate imputed multilateral HA from DAC2A
dac2a_cmo <- merge(dac2a_cmo, dac2a_moha_share, by.x = c("Year", "Recipient"), by.y = c("Year", "Donor"), all.x = T)
dac2a_imha <- dac2a_cmo[, .(dac2a_imputed_multi_ha = sum(Value*ha_share, na.rm = T)), by = .(Year, Donor)]

##MUMS - #Standardise MUMS names
{mums[Channel == "Food and Agricultural Organisation", Channel := "Food and Agriculture Organisation [FAO]"]
  mums[Channel == "United Nations Children’s Fund", Channel := "UNICEF"]
  mums[Channel == "United Nations Development Programme", Channel := "UNDP"]
  mums[Channel == "United Nations Environment Programme", Channel := "UNEP"]
  mums[Channel == "United Nations Office of the United Nations High Commissioner for Refugees", Channel := "UNHCR"]
  mums[Channel == "United Nations Peacebuilding Fund", Channel := "UN Peacebuilding Fund [UNPBF]"]
  mums[Channel == "United Nations Population Fund", Channel := "UNFPA"]
  mums[Channel == "United Nations Relief and Works Agency for Palestine Refugees in the Near East", Channel := "UNRWA"]
  mums[Channel == "World Food Programme", Channel := "WFP"]
  mums[Channel == "World Health Organisation - assessed contributions", Channel := "World Health Organisation [WHO]"]
  mums[Channel == "World Health Organisation - core voluntary contributions account", Channel := "World Health Organisation [WHO]"]
  mums[Channel == "International Development Association", Channel := "International Development Association [IDA]"]
  mums[Channel == "African Development Bank", Channel := "African Development Bank [AfDB]"]
  mums[Channel == "African Development Fund", Channel := "African Development Fund [AfDF]"]
  mums[Channel == "Asian Development Bank", Channel := "Asian Development Bank [AsDB]"]
  mums[Channel == "Asian Development Fund", Channel := "Asian Development Bank [AsDB]"]
  mums[Channel == "Council of Europe Development Bank", Channel := "Council of Europe Development Bank [CEB]"]
  mums[Channel == "Inter-American Development Bank, Fund for Special Operations", Channel := "Inter-American Development Bank [IDB]"]
  mums[Channel == "Inter-American Development Bank, Inter-American Investment Corporation and Multilateral Investment Fund", Channel := "Inter-American Development Bank [IDB]"]
  mums[Channel == "Clean Technology Fund", Channel := "Climate Investment Funds [CIF]"]
  mums[Channel == "Nordic Development Fund", Channel := "Nordic Development Fund [NDF]"]
  mums[Channel == "Strategic Climate Fund", Channel := "Climate Investment Funds [CIF]"]
  mums[Channel == "Central Emergency Response Fund", Channel := "Central Emergency Response Fund [CERF]"]
  mums[grepl("Global Environment Facility", Channel), Channel := "Global Environment Facility [GEF]"]
  mums[grepl("International Labour Organisation", Channel), Channel := "International Labour Organisation [ILO]"]
}

#Remove multilats which are already accounted for in DAC2A
mums_exc_dac2a <- mums[!(Channel %in% dac2a_cmo$Recipient)]

#Calculate imputed multilateral HA from MUMS
mums_exc_dac2a <- merge(mums_exc_dac2a, dac2a_moha_share, by.x = c("Year", "Channel"), by.y = c("Year", "Donor"), all.x = T)

#Manually set UNOCHA to 100%
mums_exc_dac2a[Channel == "United Nations Office of Co-ordination of Humanitarian Affairs", ha_share := 1]

mums_imha <- mums_exc_dac2a[, .(mums_imputed_multi_ha = sum(Value*ha_share, na.rm = T)), by = .(Year, Donor, DonorCode)]

#Estimate MUMS for completely missing year(s) (use deflator here)
mums_missing_guess <- mums_imha[Year == mums_max_year][rep(1:nrow(mums_imha[Year == mums_max_year]), (dac2a_max_year - as.numeric(mums_max_year)))]
mums_missing_guess[, Year := (rep((as.numeric(mums_max_year) + 1 ):(as.numeric(dac2a_max_year)), each = nrow(mums_missing_guess)/(dac2a_max_year - as.numeric(mums_max_year))))]
mums_imha <- rbind(mums_imha[(Year) <= mums_max_year], mums_missing_guess)

# mums_imha <- merge(mums_imha, defl[year == dac2a_max_year, .(ISO, year, gdp_defl)], by.x = c("ISO"), by.y = c("ISO"), all.x = T)
# mums_imha[Year != year, gdp_defl := NA] 
# mums_imha$mums_imputed_multi_ha <- ifelse(is.na(mums_imha$gdp_defl), mums_imha$mums_imputed_multi_ha, mums_imha$mums_imputed_multi_ha / mums_imha$gdp_defl)
# mums_imha$year <- NULL
# mums_imha$gdp_defl <- NULL



## Estimate 2024 for Multilaterals (DAC1 growth based on 2023 constant prices so no deflator needed)

dac1_un_growth <- dac1_un[order(Year)][, growth := Value/shift(Value), by = .(DONOR)][!is.na(growth)][, Value := NULL]
dac1_un_growth <- dac1_un_growth[, .(Donor, Year, growth)]

UN_dac2a_agencies <- c("UNDP", "UNFPA", "UNHCR", "UNICEF", "UNRWA", "WFP")
dac2a_un <- dac2a_cmo[Recipient %in% UN_dac2a_agencies]

dac2a_un_imputed <- dac2a_un[, .(dac2a_imputed_un_ha = sum(Value * ha_share, na.rm = TRUE)), by = .(Year, Donor)]
dac2a_un_imputed[, Year := Year + 1]
dac2a_un_imputed <- dac2a_un_imputed[Year == 2024]

UN_mums_agencies <- c("Food and Agriculture Organisation [FAO]", "International Labour Organisation [ILO]", "International Organisation for Migration", "UNEP", "United Nations Office of Co-ordination of Humanitarian Affairs", "UN Peacebuilding Fund [UNPBF]", "World Health Organisation [WHO]")
mums_un <- mums_exc_dac2a[Channel %in% UN_mums_agencies]
mums_un_imputed  <- mums_un[, .(mums_imputed_un_ha  = sum(Value * ha_share, na.rm = TRUE)), by = .(Year, Donor)]
mums_un_imputed [, Year := Year + 1]
mums_un_imputed  <- mums_un_imputed[Year == 2024]

dac2a_missing_un <- merge(dac1_un_growth, dac2a_un_imputed, by = c("Donor", "Year"), all = T)
dac2a_missing_un <- merge(dac2a_missing_un, mums_un_imputed, by.x = c("Donor", "Year"), by.y = c("Donor", "Year"), all = T)
dac2a_missing_un[is.na(growth), growth := 1]
dac2a_mums_missing_un <- dac2a_missing_un[, .(dac2a_mums_missing_un = growth*(dac2a_imputed_un_ha + mums_imputed_un_ha)), by = .(Donor, Year)]

## Estimate 2024 for World Bank Group

dac1_ida[, ida_share := {
  ida_value <- Value[`Aid type` == "I.B.1.3. IDA"]
  total_value <- Value[`Aid type` == "      Memo: World Bank, Total (I.B.1.3. + I.B.1.4.)"]
  percentage <- ifelse(length(total_value) > 0, ida_value / total_value, NA)
  percentage[1]
}, by = .(Donor, Year)]

dac1_ida[, ida_share_prev_year := shift(ida_share, type = "lag"), by = .(Donor, `Aid type`)]
dac1_ida[Year == 2024, ida_share := ida_share_prev_year]
dac1_ida[, ida_share_prev_year := NULL]
dac1_ida <- dac1_ida[Year == 2024]
dac1_ida <- dac1_ida[`Aid type` == "      Memo: World Bank, Total (I.B.1.3. + I.B.1.4.)"]
dac1_ida[, dac1_imputed_ida_oda := Value*ida_share]

dac2a_ida_ha_share <- dac2a_moha_share[Donor == "International Development Association [IDA]"]
dac2a_ida_ha_share <- rbind(dac2a_ida_ha_share, dac2a_ida_ha_share[Year == 2023, .(Year = 2024, Donor, ha_share)])
#setnames(dac1_ida, "Year", "Year")
setnames(dac2a_ida_ha_share, "Donor", "Org")
dac2a_missing_ida <- merge(dac1_ida, dac2a_ida_ha_share, by = "Year")
dac2a_missing_ida <- dac2a_missing_ida[, .(dac2a_missing_ida = dac1_imputed_ida_oda*as.numeric(ha_share)), by = .(Donor, Year)]

dac2a_total_missing <- merge(dac2a_mums_missing_un, dac2a_missing_ida, all.x = T, by = c("Donor", "Year"))
dac2a_total_missing[is.na(dac2a_total_missing)] <- 0
dac2a_total_missing <- dac2a_total_missing[, .(dac2a_imputed_multi_ha = dac2a_mums_missing_un + dac2a_missing_ida), by = .(Donor, Year)]
dac2a_total_missing <- dac2a_total_missing[Year == 2024]
dac2a_imha <- rbind(dac2a_imha, dac2a_total_missing)

##Prelim data (assuming not present in DAC2a)
adv <- suppressWarnings(melt(data.table(adv_raw)))
dac_prelim <- adv[...1 %in% c("a.  Humanitarian aid.", "b. EU") & variable != "...36"][, .(Donor = variable, Value = value, `Aid type` = ...1)]
if(nrow(dac_prelim[is.na(Value) & `Aid type` == "a.  Humanitarian aid."]) > 0) warning("The following donors have no preliminary humanitarian data: ", dac_prelim[is.na(Value) & `Aid type` == "a.  Humanitarian aid.", paste0(Donor, collapse = ", ")])
dac_prelim[Donor == "DAC countries", Donor := "DAC Countries, Total"]
dac_prelim[, Year := 2024]

dac_prelim[`Aid type` == "a.  Humanitarian aid.", `Aid type` := "Humanitarian Aid"]
dac_prelim[`Aid type` == "b. EU", `Aid type` := "I.B.1.2. EU institutions"]

nondac_prelim <- dac_prelim[Donor %in% c("Azerbaijan",	"Bulgaria",	"Croatia",	"Israel",	"Kuwait",	"Liechtenstein",	"Malta",	"Monaco",	"Qatar",	"Romania",	"Türkiye",	"United Arab Emirates"), .(Donor = "Non-DAC Countries, Total", Value = sum(Value, na.rm = T)), by = .(Year, `Aid type`)]
dac_prelim <- rbind(dac_prelim, nondac_prelim)
dac_prelim[is.na(dac_prelim)] <- 0

isos[countryname_oecd == "Czech Republic", countryname_oecd := "Czechia"]
isos[countryname_oecd == "Slovakia", countryname_oecd := "Slovak Republic"]

defl[, gdp_defl/gdp_defl[year == dac_base_year], by = ISO]
defl <- merge(isos[, .(iso3, countryname_oecd)], defl, by.x = "iso3", by.y = "ISO", all.y = T)

defl[iso3 == "EUI", countryname_oecd := "EU Institutions"]
defl[iso3 == "DAC", countryname_oecd := "DAC Countries, Total"]
defldac <- copy(defl[iso3 == "DAC"])
defldac[, countryname_oecd := "Non-DAC Countries, Total"]
defl <- rbind(defl, defldac)

dac_prelim <- merge(dac_prelim, defl, by.x = c("Donor", "Year"), by.y = c("countryname_oecd", "year"))

dac_prelim[, Value := Value/gdp_defl]
dac_prelim[, `:=` (iso3 = NULL, base_year = NULL, currency = NULL, source = NULL, ver = NULL, gdp_defl = NULL)]

dac2a_ha <- rbind(dac2a_ha, dac_prelim[`Aid type` == "Humanitarian Aid"], fill = T)
#dac1_eu <- rbind(dac1_eu, dac_prelim[`Aid type` == "I.B.1.2. EU institutions"], fill = T)

####
##Total Imputed HA
total_imha <- merge(dac2a_imha, mums_imha, by = c("Donor", "Year"), all = T)
total_imha[is.na(total_imha)] <- 0
total_imha[, total_imha := (dac2a_imputed_multi_ha + mums_imputed_multi_ha)]

## EU imputations
dac1_eu[, eu_value_share := Value/(as.numeric(Value[Donor == "DAC Countries, Total"]) + (Value[Donor == "Non-DAC Countries, Total"])), by = Year]
#setnames(dac1_eu, "Year", "Year")
total_eu <- merge(dac2a_ha[Donor == "EU Institutions"], total_imha[Donor == "EU Institutions", .(Year, total_imha)])
total_eu <- total_eu[, .(total_eu = Value + total_imha), by = "Year"]
dac1_eu <- merge(dac1_eu, total_eu, by = "Year", all.x = T)
eu_imha <- dac1_eu[, .(eu_imha = eu_value_share*total_eu), by = .(Year, Donor)]

##Total IHA (WITH PARTIAL CERF THROUGH MUMS)
total_iha <- merge(dac2a_ha, total_imha, by = c("Year", "Donor"), all = T)
total_iha <- merge(total_iha, eu_imha, by = c("Year", "Donor"), all = T)
total_iha <- total_iha[Year >= min_common_year]
total_iha[is.na(total_iha)] <- 0

total_iha_sep <- total_iha[, .(total_bilat = Value, total_imha, eu_imha), by = .(Year, Donor)]
total_iha <- total_iha[, .(total_iha = Value + total_imha + eu_imha), by = .(Year, Donor)]
total_iha <- total_iha[!grepl(", Total", Donor)]
write.csv(total_iha, "0. Calculated - Total IHA by Donor/Output/dac_aggregate_donors.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)
write.csv(total_iha_sep, "0. Calculated - Total IHA by Donor/Output/dac_aggregate_donors_bimulti.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)

