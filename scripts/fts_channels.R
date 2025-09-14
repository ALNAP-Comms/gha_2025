###Localisation script
#Required local files in 'input' folder:
#- FTS curated output ('fts_curated_xxxx_xxxx.csv')
#- CBPF project summary ('ProjectSummaryxxx....csv)
#- CERF allocations by year summary ('AllocationsByYear.csv')
#- Channels decode ('channels_decode.csv')
#- RCRC, Government and SINGO decode ('localisation_decode.csv')

#Load required packages
required.packages <- c("data.table", "rstudioapi")
install.packages(required.packages[!(required.packages %in% installed.packages())])
lapply(required.packages, require, character.only=T)

#Set working directory
setwd(dirname(dirname(dirname(getActiveDocumentContext()$path))))

fts_raw <- fread("Datasets/FTS - Full Dataset/Datasets/fts_curated_2000_2029.csv")
cbpf_raw <- fread("Datasets/CBPFs/ProjectSummary20250415094901009.csv")
unhcr_raw <- fread("Datasets/Localisation/unhcr_projects_2023_2024.csv")
local_decode <- fread("Datasets/Localisation/localisation_decode.csv")

fts <- copy(fts_raw)
{fts[is.na(source_orgtype), source_orgtype := "ERROR"]
  fts[, source_orgtype := fifelse(source_orgtype == "Multilateral: EC", "Multilateral: EC",
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

setwd(dirname(getActiveDocumentContext()$path))
channels_decode <- fread("input/channels_decode.csv")
cerf_raw <- fread("input/AllocationsByYear.csv")

#Extract DAC deflator from FTS
dac_deflator <- unique(fts[destination_org_iso3 == "DAC", .(year, deflator)])

#Separate channel decodes
dest_channels_decode <- channels_decode
names(dest_channels_decode) <- paste0("destination_", names(dest_channels_decode))

source_channels_decode <- channels_decode
names(source_channels_decode) <- paste0("source_", names(source_channels_decode))

##FTS
fts <- merge(fts, dest_channels_decode, by = "destination_orgtype", all.x = T)
fts <- merge(fts, source_channels_decode, by = "source_orgtype", all.x = T)

fts[destinationObjects_Organization.id == 4397, destination_channel := "UNHCR"]

fts_channels <- fts[(
  (source_channel == c("Public sector") & newMoney == T) | newMoney == F) #Direct only for public sector
  & dummy == F #Remove dummies
  & domestic_response == F #Remove domestic response
  & sourceObjects_Organization.id != destinationObjects_Organization.id #Remove internal reallocations
  & destinationObjects_Organization.id != "" #Remove blank destination organisations
  & !(source_orgtype %in% c("Pooled Funds: CERF", "Pooled Funds: Country") #Remove CERF and CBPFs from FTS
  & sourceObjects_Organization.id != 4397 #Remove UNHCR from fTS
  ),
  .(source = "fts", amountUSD_defl_millions = sum(amountUSD_defl_millions)),
  by = .(direct = newMoney, source_channel, destination_channel, year)][order(-direct, source_channel, destination_channel, year)]

fts_channels_cast <- dcast(fts_channels, direct + source_channel + destination_channel ~ year, value.var = "amountUSD_defl_millions")

##CBPFs
cbpf_raw <- merge(cbpf_raw, dac_deflator, by.x = "AllocationYear", by.y = "year")
cbpf_raw[, amountUSD_defl_millions := Budget/deflator/1000000]
cbpf_raw[, direct := F]

#RCRC
cbpf_rcrc <- cbpf_raw[OrganizationType == "Others" & OrganizationName != "", .(PooledFundName, OrganizationName, ChfProjectCode)]
cbpf_rcrc <- merge(cbpf_rcrc, local_decode[type == "RCRC"], by.x = "OrganizationName", by.y = "destinationObjects_Organization.name", all.x = T)

cbpf_rcrc_local_ids <- cbpf_rcrc[, ChfProjectCode]

#Hard orgtype decode
cbpf_raw[, destination_channel := OrganizationType]
cbpf_raw[destination_channel == "International NGO", destination_channel := "iNGOs"]
cbpf_raw[destination_channel == "National NGO", destination_channel := "LNNGOs"]
cbpf_raw[destination_channel == "UN Agency", destination_channel := "Multilateral organisations"]
cbpf_raw[destination_channel == "Others", destination_channel := "Other"]

cbpf_raw[ChfProjectCode %in% cbpf_rcrc_local_ids, destination_channel := "RCRC"]

cbpf_channels <- cbpf_raw[, .(source = "cbpf", source_channel = "Pooled funds", amountUSD_defl_millions = sum(amountUSD_defl_millions, na.rm = T)), by = .(year = AllocationYear, direct, destination_channel)]

cbpf_channels_cast <- dcast(cbpf_channels, direct + source_channel + destination_channel ~ year, value.var = "amountUSD_defl_millions")

##CERF
cerf_raw <- merge(cerf_raw, dac_deflator, by.x = "Year", by.y = "year")
cerf_raw[, amountUSD_defl_millions := as.numeric(gsub(",", "", `Total Funds Allocated US$`))/deflator/1000000]
cerf_raw[, direct := F]

cerf_channels <- cerf_raw[, .(source = "cerf", source_channel = "Pooled funds", destination_channel = "Multilateral organisations", amountUSD_defl_millions = sum(amountUSD_defl_millions, na.rm = T)), by = .(year = Year, direct)] #All CERF assigned to Multilats

cerf_channels_cast <- dcast(cerf_channels, direct + source_channel + destination_channel ~ year, value.var = "amountUSD_defl_millions")

##UNHCR
unhcr_raw[, direct := F]
unhcr_raw <- merge(unhcr_raw, dac_deflator, by.x = "Year", by.y = "year")
unhcr_raw[, Budget := as.numeric(gsub(",", "", `Total Budget (USD)`))]
unhcr_raw[, amountUSD_defl_millions := Budget/deflator/1000000]

#Code orgtype
unhcr_raw[, destination_channel := "Other"]
unhcr_raw[`Partner Type DI coding` %in% c("NGOs: International", "NGOs: Internationally Affiliated") | (`Partner Type` == "International NGO" & `Partner Type DI coding` == "Private Organisations: International"), destination_channel := "iNGOs"]
unhcr_raw[`Partner Type DI coding` == "NGOs: National" | (`Partner Type` == "National NGO" & `Partner Type DI coding` == "Private Organisations: Local/National"), destination_channel := "LNNGOs"]
unhcr_raw[`Partner Type DI coding` %in% c("Multilateral: UN", "Multilateral: Other"), destination_channel := "Multilateral organisations"]
unhcr_raw[`Partner Type DI coding` %in% c("Red Cross: International", "Red Cross: National"), destination_channel := "RCRC"]
unhcr_raw[`Partner Type DI coding` %in% c("Governments: National", "Governments: Local"), destination_channel := "Public sector"]

unhcr_channels <- unhcr_raw[, .(source = "unhcr", source_channel = "UNHCR", amountUSD_defl_millions = sum(amountUSD_defl_millions, na.rm = T)), by = .(year = Year, direct, destination_channel)]

unhcr_channels_cast <- dcast(unhcr_channels, direct + source_channel + destination_channel ~ year, value.var = "amountUSD_defl_millions")

##Aggregate
channels_summary <- rbind(fts_channels, cbpf_channels, cerf_channels, unhcr_channels)

channels_summary <- channels_summary[, .(amountUSD_defl_millions = sum(amountUSD_defl_millions)), by = .(year, direct, source_channel, destination_channel)]

channels_summary_cast <- dcast(channels_summary, direct + source_channel + destination_channel ~ year, value.var = "amountUSD_defl_millions")[order(-direct, source_channel, destination_channel)]

fwrite(channels_summary_cast, "channel_output.csv")
