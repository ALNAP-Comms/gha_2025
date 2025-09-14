suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi", "httr", "XML", "lubridate", "countrycode"), require, character.only=T))

setwd(dirname(dirname(dirname(getActiveDocumentContext()$path))))

isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8")

#Establish current DAC base year
dac_base_year <- 2023

#Load deflators
defl <- fread("Deflators/deflators_2023usd.csv")
defl <- merge(defl, isos[, .(iso3, countryname_oecd)], by.x = "ISO", by.y = "iso3", all.x = T)
defl[countryname_oecd == "Russian Federation", countryname_oecd := "Russia"]
defl[countryname_oecd == "Slovakia", countryname_oecd := "Slovak Republic"]

#Load DAC IHA
dac_iha_bimulti <- fread("0. Calculated - Total IHA by Donor/Output/dac_aggregate_donors_bimulti.csv", encoding = "UTF-8")[!(grepl(", Total", Donor))]
min_common_year <- min(dac_iha_bimulti$Year)

#Working years
dac_years <- unique(dac_iha_bimulti[, min(Year):max(Year)]$Year)

#Load FTS
fts <- fread("FTS - Full Dataset/Datasets/fts_curated_2000_2029.csv")
fts <- fts[year %in% dac_years]
fts[source_org_country == "TÃ¼rkiye", source_org_country := "Türkiye"]
fts[source_org_country == "Slovakia", source_org_country := "Slovak Republic"]
fts[source_org_country == "Korea, Republic of", source_org_country := "Korea"]
fts[source_org_country == "Taiwan, Province of China", source_org_country := "Chinese Taipei"]
fts[source_org_country == "Czech Republic", source_org_country := "Czechia"]
fts[source_org_country == "EC", source_org_country := "EU Institutions"]
fts[source_org_country == "Russian Federation", source_org_country := "Russia"]

{fts[is.na(source_orgtype), source_orgtype := "ERROR"]
  fts[, source_orgtype := fifelse(source_orgtype == "EC", "Multilateral: EC",
                                  fifelse(source_orgtype == "Multilateral: Development Bank", "Multilateral: Development Bank",
                                          fifelse(sourceObjects_Organization.name == "United States Department of State", "Governments: DAC",
                                                  fifelse(sourceObjects_Organization.organizationTypes == "Governments" & source_orgtype == "DAC governments", "Governments: DAC",
                                                          fifelse(sourceObjects_Organization.organizationTypes == "Governments", "Governments: Non-DAC", 
                                                                  fifelse(sourceObjects_Organization.organizationTypes == "Multilateral Organizations" & sourceObjects_Organization.organizationSubTypes == "UN Agencies", "Multilateral: UN",
                                                                          fifelse(sourceObjects_Organization.organizationTypes == "Multilateral Organizations", "Multilateral: Other",
                                                                                  fifelse(sourceObjects_Organization.organizationTypes == "NGOs" & sourceObjects_Organization.organizationSubTypes %in% c("International NGOs", "Uncategorized NGOs"), "NGOs: International",
                                                                                          fifelse(sourceObjects_Organization.organizationTypes == "NGOs" & sourceObjects_Organization.organizationSubTypes == "National NGOs/CSOs", "NGOs: National",
                                                                                                  fifelse(sourceObjects_Organization.organizationTypes == "NGOs" & sourceObjects_Organization.organizationSubTypes == "Local NGOs/CSOs", "NGOs: Local",
                                                                                                          fifelse(sourceObjects_Organization.organizationTypes == "NGOs" & sourceObjects_Organization.organizationSubTypes == "Internationally Affiliated Organizations", "NGOs: Internationally Affiliated",
                                                                                                                  fifelse(sourceObjects_Organization.organizationTypes == "Other", "Other",
                                                                                                                          fifelse(sourceObjects_Organization.organizationTypes == "Pooled Funds" & sourceObjects_Organization.name == "Central Emergency Response Fund", "Pooled Funds: CERF",
                                                                                                                                  fifelse(sourceObjects_Organization.organizationTypes == "Pooled Funds" & sourceObjects_Organization.organizationSubTypes %in% c("Global UN Pooled Funds", "Other Pooled Funds"), "Pooled Funds: Other UN",
                                                                                                                                          fifelse(sourceObjects_Organization.organizationTypes == "Pooled Funds" & sourceObjects_Organization.organizationSubTypes == "Regional UN Pooled Funds", "Pooled Funds: Regional",
                                                                                                                                                  fifelse(sourceObjects_Organization.organizationTypes == "Pooled Funds" & sourceObjects_Organization.organizationSubTypes %in% c("Country-based UN Pooled Funds"), "Pooled Funds: Country",
                                                                                                                                                          fifelse(sourceObjects_Organization.organizationTypes == "Private Organizations" & grepl("UNICEF", sourceObjects_Organization.name), "Multilateral: UN",
                                                                                                                                                                  fifelse(sourceObjects_Organization.organizationTypes == "Private Organizations", "Private Organisations: Foundations and Other",
                                                                                                                                                                          fifelse(sourceObjects_Organization.organizationTypes == "Red Cross/Red Crescent Organizations" & sourceObjects_Organization.organizationSubTypes == "Red Cross/Red Crescent National Societies", "Red Cross: National",
                                                                                                                                                                                  fifelse(sourceObjects_Organization.organizationTypes == "Red Cross/Red Crescent Organizations" & sourceObjects_Organization.organizationSubTypes == "International Red Cross/Red Crescent Movement", "Red Cross: International", 
                                                                                                                                                                                          fifelse(sourceObjects_Organization.organizationTypes == "", "Other: Unknown",
                                                                                                                                                                                                  fifelse(grepl(";", sourceObjects_Organization.organizationTypes), "Other: Multiple Organisations",
                                                                                                                                                                                                          source_orgtype))))))))))))))))))))))]
  
  fts[, destination_orgtype := fifelse(destinationObjects_Organization.organizationTypes == "Governments" & destinationObjects_Organization.organizationSubTypes %in% c("National Governments", "International Government Entities"), "Governments: National",
                                       fifelse(destinationObjects_Organization.organizationTypes == "Governments" & destinationObjects_Organization.organizationSubTypes == "Local Governments", "Governments: Local",
                                               fifelse(destinationObjects_Organization.organizationSubTypes == "UN Agencies", "Multilateral: UN",
                                                       fifelse(destinationObjects_Organization.organizationSubTypes %in% c("International NGOs", "Uncategorized NGOs"), "NGOs: International",
                                                               fifelse(destinationObjects_Organization.organizationSubTypes == "National NGOs/CSOs", "NGOs: National",
                                                                       fifelse(destinationObjects_Organization.organizationSubTypes == "Internationally Affiliated Organizations", "NGOs: Internationally Affiliated",
                                                                               fifelse(destinationObjects_Organization.organizationSubTypes == "Local NGOs/CSOs", "NGOs: Local",
                                                                                       fifelse(destinationObjects_Organization.organizationTypes == "Other", "Other",
                                                                                               fifelse(destinationObjects_Organization.organizationSubTypes == "Other Multilateral Organizations", "Multilateral: Other",
                                                                                                       fifelse(destinationObjects_Organization.organizationSubTypes %in% c("Global UN Pooled Funds", "Other Pooled Funds"), "Pooled Funds: Other UN",
                                                                                                               fifelse(destinationObjects_Organization.organizationSubTypes %in% c("Country-based UN Pooled Funds"), "Pooled Funds: Country",
                                                                                                                       fifelse(destinationObjects_Organization.organizationSubTypes == "Regional UN Pooled Funds", "Pooled Funds: Regional",
                                                                                                                               fifelse(destinationObjects_Organization.organizationSubTypes %in% c("International Private Organizations", "Uncategorized Private Organizations"), "Private Organisations: International",
                                                                                                                                       fifelse(destinationObjects_Organization.organizationSubTypes == "Local/National Private Organizations", "Private Organisations: Local/National",                                               
                                                                                                                                               fifelse(destinationObjects_Organization.organizationSubTypes == "International Red Cross/Red Crescent Movement", "Red Cross: International",
                                                                                                                                                       fifelse(destinationObjects_Organization.organizationSubTypes == "Red Cross/Red Crescent National Societies", "Red Cross: National",
                                                                                                                                                               fifelse(destinationObjects_Organization.organizationSubTypes == "", "Other: Unknown",                                              
                                                                                                                                                                       fifelse(grepl(";", destinationObjects_Organization.organizationTypes), "Other: Multiple Organisations", "ERROR"))))))))))))))))))]
}

#Set FTS accounting year from donor perspective
fts[status == "paid", year := as.integer(year(date))]
fts <- fts[year %in% dac_years]

#Re-deflate FTS based on accounting year
fts <- merge(fts, defl[, .(source_org_iso3 = ISO, year, gdp_defl)], by = c("source_org_iso3", "year"), all.x = T)
fts[dummy == F & !is.na(gdp_defl), `:=` (amountUSD_defl = amountUSD/gdp_defl, amountUSD_defl_millions = (amountUSD/gdp_defl)/1000000)]

#Establish ODA-eligible recipients by year
oda_recipients_isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/oda_eligibility.csv", encoding = "UTF-8")
oda_recipients_isos <- oda_recipients_isos[, .(year, name, iso3)]

#Create FTS ODA-eligibility column
fts[, oda_eligible := F]
fts[paste0(destination_org_iso3, year) %in% oda_recipients_isos[, paste0(iso3, year)], oda_eligible := T]
fts[destination_org_country %in% c("Serbia and Montenegro (until 2006-2009)", "Multi-destination_country", "Global", ""), oda_eligible := T]

#Establish DAC donors
dac_donors <- isos[dac_member == 1, .(iso3, countryname_oecd)]
dac_donors <- dac_donors$countryname_oecd
dac_donors <- c(dac_donors, "EU Institutions")
dac_donors[dac_donors == "Czech Republic"] <- "Czechia"
dac_donors[dac_donors == "Slovakia"] <- "Slovak Republic"


#####
##DAC donors
fts_dac_donors <- fts[(source_orgtype == "Governments: DAC" | source_org_iso3 == "EUI" ) & domestic_response == F & newMoney == T & dummy == F & destinationObjects_Organization.name != "Central Emergency Response Fund"]
fts_dac_donors[source_org_country == "European Commission", source_org_country := "EU Institutions"]

#DAC donors directly to non-ODA eligible recipients (FTS)
fts_dac_donors_oda <- fts_dac_donors[oda_eligible == T, .(fts_oda_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = .(source_org_country, year)]
fts_dac_donors_nonoda <- fts_dac_donors[oda_eligible == F, .(fts_nonoda_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = .(source_org_country, year)]

#DAC donors imputed EU to non-ODA eligible recipients (FTS)
fts_eu <- fts[source_orgtype == "Multilateral: EC" & domestic_response == F & newMoney == T & dummy == F & destinationObjects_Organization.name != "Central Emergency Response Fund"]
fts_eu_oda <- fts_eu[oda_eligible == T, .(fts_oda_eu_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = year]
fts_eu_nonoda <- fts_eu[oda_eligible == F, .(fts_nonoda_eu_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = year]


dac1 <- fread("OECD DAC Table 1 and 2a/Table1_Data.csv")
dac1[DONOR == 55, Donor := "Türkiye"]
dac1 <- dac1[FLOWS == 1140 & AMOUNTTYPE == "D" & Year >= 2010]
dac1_eu <- dac1[dac1$AIDTYPE == 2102]
dac1_eu[, eu_value_share := Value/(as.numeric(Value[Donor == "DAC Countries, Total"]) + (Value[Donor == "Non-DAC Countries, Total"])), by = Year]

dac1_dac_donors_oda_eu <- merge(dac1_eu[Donor %in% dac_donors], fts_eu_oda, by.x = "Year", by.y = "year", all.x = T)
fts_dac_donors_oda_imeu <- dac1_dac_donors_oda_eu[, .(fts_oda_imeu_ha = eu_value_share*fts_oda_eu_ha), by = .(Year, Donor)]
fts_dac_donors_oda_imeu <- fts_dac_donors_oda_imeu[fts_dac_donors_oda_imeu$Year >= min_common_year, ]

dac1_dac_donors_nonoda_eu <- merge(dac1_eu[Donor %in% dac_donors], fts_eu_nonoda, by.x = "Year", by.y = "year", all.x = T)
fts_dac_donors_nonoda_imeu <- dac1_dac_donors_nonoda_eu[, .(fts_nonoda_imeu_ha = eu_value_share*fts_nonoda_eu_ha), by = .(Year, Donor)]
fts_dac_donors_nonoda_imeu <- fts_dac_donors_nonoda_imeu[fts_dac_donors_nonoda_imeu$Year >= min_common_year, ]

##For most recent year, decide to use DAC or FTS for ODA-eligible recipients (because preliminary DAC data is... preliminary)
fts_dac_donors_prelim <- merge(fts_dac_donors_oda[year == max(dac_years), .(Year = as.numeric(year), Donor = source_org_country, fts_oda_ha)], fts_dac_donors_oda_imeu[Year == max(dac_years), .(Year = as.numeric(Year), Donor, fts_oda_imeu_ha)], all = T)
dac1_dac_donors_prelim <- dac_iha_bimulti[Donor %in% dac_donors & Year == max(dac_years)]

dac_donors_prelim <- merge(fts_dac_donors_prelim, dac1_dac_donors_prelim, all = T)
dac_donors_prelim[is.na(dac_donors_prelim)] <- 0

##CERF
mumsfull <- fread("OECD DAC Multi/MultiSystem entire dataset.txt")
mumsfull[DonorCode == 55, DonorNameE := "Türkiye"]
mums <- mumsfull[, .(Value = sum(Amount, na.rm = T)), by = .(Year, AidToOrThru, FlowType, DonorCode, Donor = DonorNameE, Channel = ChannelNameE, AmountType)][
  AidToOrThru == "Core contributions to" & FlowType == "Disbursements" & AmountType == "Constant prices" & Channel == "Central Emergency Response Fund"]

mums[Channel == "Central Emergency Response Fund", Channel := "Central Emergency Response Fund [CERF]"]
mums <- mums[, c("Donor", "Year", "Value")]
mums <- mums[!is.na(Value)]

  
cerf <- fread("CERF/contributionschart__20250416_214937_UTC.csv")
cerf[Donor == "Czech Republic", Donor := "Czechia"]
cerf[Donor == "Vietnam", Donor := "Viet Nam"]
cerf[Donor == "Cote d'Ivoire", Donor := "Côte d'Ivoire"]
cerf[Donor == "Lao PDR", Donor := "Lao People's Democratic Republic"]
cerf[Donor == "Iran", Donor := "Iran, Islamic Republic of"]
cerf[Donor == "Russian Federation", Donor := "Russia"]
cerf[Donor == "Slovakia", Donor := "Slovak Republic"]
cerf[Donor == "Hyogo Prefecture (Japan)", Donor := "Japan"]
cerf[Donor == "Belgian Government of Flanders", Donor := "Belgium"]
cerf[Donor == "State of South Australia", Donor := "Australia"]
cerf[Donor == "Catalan Agency for Development Cooperation", Donor := "Spain"]
cerf[Donor == "Basque Agency for Development Cooperation", Donor := "Spain"]
cerf[Donor == "United States of America", Donor := "United States"]
cerf[Donor == "Turkey", Donor := "Türkiye"]
cerf[Donor == "Moldova", Donor := "Moldova, Republic of"]

cerf <- cerf[Year >= min_common_year & Year <= dac_base_year + 1]
cerf <- cerf[(`Donor type` == "Member State" | `Donor type` == "Regional/ Local Authorities") & Donor != "International Maritime Orgnization (IMO)"]
cerf <- cerf[, .(Paid = sum(Paid)), by = .(Year, Donor)]
setnames(cerf, "Paid", "Value")
cerf$Value <- cerf$Value/1000000

matching_rows <- merge(cerf, mums, by = c("Donor", "Year"))
cerf <- cerf[!paste(cerf$Donor, cerf$Year) %in% paste(matching_rows$Donor, matching_rows$Year), ]

cerf <- rbind(cerf, mums)

cerf$ISO <- countrycode(cerf$Donor, origin = "country.name", destination = "iso3c")
cerf <- merge(cerf, defl, by.x = c("ISO", "Year"), by.y = c("ISO", "year"))
cerf$cerf <- cerf$Value / cerf$gdp_defl
cerf <- cerf[, c("Donor", "Year", "cerf")]
cerf_dac <- cerf[Donor %in% dac_donors]
cerf_non_dac <- cerf[!Donor %in% dac_donors]


dac_donors_prelim <- merge(dac_donors_prelim, cerf, by = c("Donor", "Year"), all.x = T)
dac_donors_prelim[is.na(dac_donors_prelim)] <- 0
#dac_donors_prelim <- dac_donors_prelim[, .(total_bilat, total_imha, cerf, eu_imha), by = .(Donor, Year)]
dac_iha_bimulti <- merge(dac_iha_bimulti, cerf_dac, by = c("Donor", "Year"), all = TRUE)
dac_iha_bimulti <- rbind(dac_iha_bimulti[Year != max(dac_years)], dac_donors_prelim, fill = TRUE)

#Total DAC donor HA
fts_dac_donors_nonoda <- fts_dac_donors_nonoda[, .(Year = year, Donor = source_org_country, fts_nonoda_ha)]
fts_dac_donors_nonoda_imeu <- fts_dac_donors_nonoda_imeu[, .(Year, Donor, fts_nonoda_imeu_ha)]
total_dac_donor_ha <- merge(dac_iha_bimulti, fts_dac_donors_nonoda, by = c("Year", "Donor"), all = T)
total_dac_donor_ha <- merge(total_dac_donor_ha, fts_dac_donors_nonoda_imeu, by = c("Year", "Donor"), all = T)
#total_dac_donor_ha <- merge(total_dac_donor_ha, fts_dac_donors_prelim, by = c("Year", "Donor"), all = T)
total_dac_donor_ha[is.na(total_dac_donor_ha)] <- 0
total_dac_donor_ha <- total_dac_donor_ha[, `:=` (total_donor_ha = total_bilat + total_imha + cerf + fts_nonoda_ha), by = .(Donor, Year)]









total_non_dac_donor_ha <- total_dac_donor_ha[!total_dac_donor_ha$Donor %in% dac_donors]
total_non_dac_donor_ha <- total_non_dac_donor_ha[, .(Year, Donor, total_bilat, total_imha, eu_imha)]
total_dac_donor_ha <- total_dac_donor_ha[total_dac_donor_ha$Donor %in% dac_donors]


#####
##NDD
fts_ndd <- fts[source_orgtype == "Governments: Non-DAC" & domestic_response == F & newMoney == T & destinationObjects_Organization.name != "Central Emergency Response Fund"]

#NDD directly to all recipients (FTS)
fts_ndd <- fts_ndd[, .(fts_ndd_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = .(source_org_country, year)]
fts_ndd <- fts_ndd[!(source_org_country %in% dac_donors)]

#NDD imputed EU to non-ODA-eligible recipients (FTS)
dac1_ndd_eu <- merge(dac1_eu[!(Donor %in% dac_donors) & !(grepl(", Total", Donor))], fts_eu_nonoda, by.x = "Year", by.y = "year", all.x = T)
fts_ndd_nonoda_imeu <- dac1_ndd_eu[, .(fts_nonoda_imeu_ha = eu_value_share*fts_nonoda_eu_ha), by = .(Year, Donor)]

#NDD imputed EU to ODA-eligible recipients (DAC + FTS)
total_eu_ha <- total_dac_donor_ha[Donor == "EU Institutions"]
ndd_eu_share <- dac1_eu[, .(ndd_eu_share = Value[Donor == "Non-DAC Countries, Total"]/Value[Donor == "DAC Countries, Total"]), by = Year]

ndd_eu_share <- merge(ndd_eu_share[, .(Year , ndd_eu_share)], total_eu_ha[, .(Year, total_donor_ha)])
ndd_eu_ha <- ndd_eu_share[, .(ndd_eu_ha = ndd_eu_share*total_donor_ha), by = Year]

dac1_ndd_eu <- merge(dac1_eu[!(Donor %in% dac_donors) & !(grepl(", Total", Donor)), .(Donor, Year, eu_value_share)], ndd_eu_ha, by = "Year", all.x = T)
dac1_ndd_imeu <- dac1_ndd_eu[, .(dac1_ndd_imeu_ha = eu_value_share*ndd_eu_ha), by = .(Donor, Year)]
dac1_ndd_imeu <- dac1_ndd_imeu[dac1_ndd_imeu$Year >= min_common_year]

#Total NDD HA
total_ndd_ha <- merge(fts_ndd_nonoda_imeu, fts_ndd, by.x = c("Year", "Donor"), by.y = c("year", "source_org_country"), all = T)
total_ndd_ha <- merge(total_ndd_ha, dac1_ndd_imeu[, .(Donor, Year, dac1_ndd_imeu_ha)], by = c("Year", "Donor"), all = T)

total_ndd_ha[is.na(total_ndd_ha)] <- 0
total_ndd_ha <- total_ndd_ha[total_ndd_ha$Year >= min_common_year]
total_ndd_ha <- merge(total_ndd_ha, total_non_dac_donor_ha, by = c('Year', 'Donor'), all = T)
total_ndd_ha <- merge(total_ndd_ha, cerf_non_dac, by = c('Year', 'Donor'), all = TRUE)

####
##All donors



#Total donor IHA
total_dac_donor_ha <- total_dac_donor_ha[, total_donor_ha := NULL]
total_donor_ha <- rbind(total_dac_donor_ha, total_ndd_ha, fill = T)[order(Year, Donor)]
total_donor_ha <- merge(total_donor_ha, isos[, .(iso3, countryname_fts)], by.x = "Donor", by.y = "countryname_fts", all.x = T)
total_donor_ha[Donor == "EU Institutions", iso3 := "EUI"]
total_donor_ha[Donor == "Korea", iso3 := "KOR"]
total_donor_ha[Donor == "Slovak Republic", iso3 := "SVK"]
total_donor_ha[Donor == "Hong Kong", iso3 := "HKG"]
total_donor_ha[Donor == "Virgin Islands, British", iso3 := "VGB"]
total_donor_ha[Donor == "Russia", iso3 := "RUS"]
total_donor_ha[Donor == "Chinese Taipei", iso3 := "TWN"]
total_donor_ha[Donor == "Czechia", iso3 := "CZE"]
total_donor_ha[is.na(total_donor_ha)] <- 0
total_donor_ha[, DAC := ifelse(Donor %in% dac_donors, "yes", "no")]

total_donor_ha[, total_donor_ha := ifelse(Donor == "Türkiye", (fts_nonoda_imeu_ha + fts_ndd_ha + dac1_ndd_imeu_ha + fts_nonoda_ha),
                                          ifelse((!Donor %in% dac_donors), 
                                          ifelse((total_bilat + total_imha + eu_imha + cerf > (fts_ndd_ha + dac1_ndd_imeu_ha)), (total_bilat + total_imha  + eu_imha + cerf + fts_nonoda_imeu_ha + fts_nonoda_ha), (fts_nonoda_imeu_ha + fts_ndd_ha + dac1_ndd_imeu_ha + fts_nonoda_ha)),
                                   ifelse(Donor %in% dac_donors & Year != 2024,
                                          (total_bilat + total_imha + eu_imha + cerf + fts_nonoda_imeu_ha + fts_nonoda_ha),
                                   ifelse(Donor %in% dac_donors & Year == 2024,
                                          ifelse((total_bilat + total_imha + cerf) > fts_oda_ha, (total_bilat + total_imha + cerf + eu_imha + fts_nonoda_imeu_ha + fts_nonoda_ha), (fts_oda_ha + fts_oda_imeu_ha + fts_nonoda_imeu_ha + fts_nonoda_ha)),"error"))))]
                                   

total_donor_ha[, source := ifelse(Donor == "Türkiye", "FTS",
                                  ifelse(!Donor %in% dac_donors,
                                  ifelse((total_bilat + total_imha + eu_imha + cerf) > (fts_ndd_ha + dac1_ndd_imeu_ha), "DAC", "FTS"),
                                  ifelse(Donor %in% dac_donors & Year != 2024, "DAC",
                                  ifelse(Donor %in% dac_donors & Year == 2024,
                                         ifelse((total_bilat + total_imha + cerf) > fts_oda_ha, "DAC", "FTS"), "error"))))]
fwrite(total_donor_ha, "0. Calculated - Total IHA by Donor/Output/total_donor_ha_all2.csv")
fwrite(total_donor_ha[, .(Donor, Year, total_donor_ha, iso3)], "0. Calculated - Total IHA by Donor/Output/total_donor_ha2.csv")

####
##Annual total

total_donor_ha[, total_donor_ha := as.numeric(total_donor_ha)]
total_donor_ha <- total_donor_ha[Donor != "EU Institutions"]
total_public_iha <- total_donor_ha[, .(total_public_iha = sum(total_donor_ha, na.rm = T)), by = (year = Year)][order(year)]
fwrite(total_public_iha, "0. Calculated - Total IHA by Donor/Output/total_iha_by_year2.csv")


dup_indices <- duplicated(total_donor_ha[, c('Year', 'iso3')]) | duplicated(total_donor_ha[, c('Year', 'iso3')], fromLast = TRUE)
duplicates <- total_donor_ha[dup_indices, ]



