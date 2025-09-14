###Localisation script
#Required local files in 'input' folder:
#- FTS curated output ('fts_curated_xxxx_xxxx.csv')
#- Pooled funds project summary ('ProjectSummaryxxx....csv)
#- UNHCR projects ('unhcr_projects_xxxx_xxxx.csv')
#- RCRC, Government and SINGO decode ('localisation_decode.csv')
#- FTS organisation list with GB status ('fts_orgs_gb.csv')

#Load required packages
required.packages <- c("data.table", "rstudioapi")
install.packages(required.packages[!(required.packages %in% installed.packages())])
lapply(required.packages, require, character.only=T)

#Set working directory
setwd(dirname(dirname(dirname(getActiveDocumentContext()$path))))

#Load datasets
fts_raw <- fread("Datasets/FTS - Full Dataset/Datasets/fts_curated_2000_2029.csv")
cbpf_raw <- fread("Datasets/CBPFs/ProjectSummary20250415094901009.csv")
unhcr_raw <- fread("Datasets/Localisation/unhcr_projects_2023_2024.csv")
decode <- fread("Datasets/Localisation/localisation_decode.csv")
fts_orgs_gb <- fread("Datasets/Localisation/fts_orgs_gb.csv")

fts <- copy(fts_raw)
{fts[is.na(source_orgtype), source_orgtype := "ERROR"]
fts[, source_orgtype := fifelse(source_orgtype == "Multilateral: EC", "Multilateral: EC",
                                fifelse(source_orgtype == "Multilateral: Development Bank", "Multilateral: Development Bank",
                                        fifelse(sourceObjects_Organization.name == "United States Department of State", "Governments: DAC",
                                                fifelse(sourceObjects_Organization.organizationTypes == "Governments" & source_orgtype == "DAC governments", "Governments: DAC",
                                                        fifelse(sourceObjects_Organization.organizationTypes == "Governments", "Governments: Non-DAC", 
                                                                fifelse(sourceObjects_Organization.organizationTypes == "Multilateral Organizations" & sourceObjects_Organization.organizationSubTypes == "UN Agencies", "Multilateral: UN",
                                                                        fifelse(sourceObjects_Organization.organizationTypes == "Multilateral Organizations", "Multilateral: Other",
                                                                                fifelse(sourceObjects_Organization.organizationTypes == "NGOs" & sourceObjects_Organization.organizationSubTypes == "International NGOs", "NGOs: International",
                                                                                        fifelse(sourceObjects_Organization.organizationTypes == "NGOs" & sourceObjects_Organization.organizationSubTypes == "National NGOs/CSOs", "NGOs: National",
                                                                                                fifelse(sourceObjects_Organization.organizationTypes == "NGOs" & sourceObjects_Organization.organizationSubTypes == "Local NGOs/CSOs", "NGOs: Local",
                                                                                                        fifelse(sourceObjects_Organization.organizationTypes == "NGOs" & sourceObjects_Organization.organizationSubTypes == "Internationally Affiliated Organizations", "NGOs: Internationally Affiliated",
                                                                                                                fifelse(sourceObjects_Organization.organizationTypes == "Other", "Other",
                                                                                                                        fifelse(sourceObjects_Organization.organizationTypes == "Pooled Funds" & sourceObjects_Organization.name == "Central Emergency Response Fund", "Pooled Funds: CERF",
                                                                                                                                fifelse(sourceObjects_Organization.organizationTypes == "Pooled Funds" & sourceObjects_Organization.organizationSubTypes == "Global UN Pooled Funds", "Pooled Funds: Other UN",
                                                                                                                                        fifelse(sourceObjects_Organization.organizationTypes == "Pooled Funds" & sourceObjects_Organization.organizationSubTypes == "Regional UN Pooled Funds", "Pooled Funds: Regional",
                                                                                                                                                fifelse(sourceObjects_Organization.organizationTypes == "Pooled Funds" & sourceObjects_Organization.organizationSubTypes == "Country-based UN Pooled Funds", "Pooled Funds: Country",
                                                                                                                                                        fifelse(sourceObjects_Organization.organizationTypes == "Private Organizations" & grepl("UNICEF", sourceObjects_Organization.name), "Multilateral: UN",
                                                                                                                                                                fifelse(sourceObjects_Organization.organizationTypes == "Private Organizations", "Private Organisations: Foundations and Other",
                                                                                                                                                                        fifelse(sourceObjects_Organization.organizationTypes == "Red Cross/Red Crescent Organizations" & sourceObjects_Organization.organizationSubTypes == "Red Cross/Red Crescent National Societies", "Red Cross: National",
                                                                                                                                                                                fifelse(sourceObjects_Organization.organizationTypes == "Red Cross/Red Crescent Organizations" & sourceObjects_Organization.organizationSubTypes == "International Red Cross/Red Crescent Movement", "Red Cross: International", 
                                                                                                                                                                                        fifelse(sourceObjects_Organization.organizationTypes == "", "Other: Unknown",
                                                                                                                                                                                                fifelse(grepl(";", sourceObjects_Organization.organizationTypes), "Other: Multiple Organisations",
                                                                                                                                                                                                        source_orgtype))))))))))))))))))))))]

fts[, destination_orgtype := fifelse(destinationObjects_Organization.organizationTypes == "Governments" & destinationObjects_Organization.organizationSubTypes == "National Governments", "Governments: National",
                                     fifelse(destinationObjects_Organization.organizationTypes == "Governments" & destinationObjects_Organization.organizationSubTypes == "Local Governments", "Governments: Local",
                                             fifelse(destinationObjects_Organization.organizationSubTypes == "UN Agencies", "Multilateral: UN",
                                                     fifelse(destinationObjects_Organization.organizationSubTypes == "International NGOs", "NGOs: International",
                                                             fifelse(destinationObjects_Organization.organizationSubTypes == "National NGOs/CSOs", "NGOs: National",
                                                                     fifelse(destinationObjects_Organization.organizationSubTypes == "Internationally Affiliated Organizations", "NGOs: Internationally Affiliated",
                                                                             fifelse(destinationObjects_Organization.organizationSubTypes == "Local NGOs/CSOs", "NGOs: Local",
                                                                                     fifelse(destinationObjects_Organization.organizationTypes == "Other", "Other",
                                                                                             fifelse(destinationObjects_Organization.organizationSubTypes == "Other Multilateral Organizations", "Multilateral: Other",
                                                                                                     fifelse(destinationObjects_Organization.organizationSubTypes == "Global UN Pooled Funds", "Pooled Funds: Other UN",
                                                                                                             fifelse(destinationObjects_Organization.organizationSubTypes == "Country-based UN Pooled Funds", "Pooled Funds: Country",
                                                                                                                     fifelse(destinationObjects_Organization.organizationSubTypes == "Regional UN Pooled Funds", "Pooled Funds: Regional",
                                                                                                                             fifelse(destinationObjects_Organization.organizationSubTypes == "International Private Organizations", "Private Organisations: International",
                                                                                                                                     fifelse(destinationObjects_Organization.organizationSubTypes == "Local/National Private Organizations", "Private Organisations: Local/National",                                               
                                                                                                                                             fifelse(destinationObjects_Organization.organizationSubTypes == "International Red Cross/Red Crescent Movement", "Red Cross: International",
                                                                                                                                                     fifelse(destinationObjects_Organization.organizationSubTypes == "Red Cross/Red Crescent National Societies", "Red Cross: National",
                                                                                                                                                             fifelse(destinationObjects_Organization.organizationSubTypes == "", "Other: Unknown",                                              
                                                                                                                                                                     fifelse(grepl(";", destinationObjects_Organization.organizationTypes), "Other: Multiple Organisations", "ERROR"))))))))))))))))))]
}

setwd(dirname(getActiveDocumentContext()$path))


###Identify localised projects
##FTS
#Extract DAC deflator from FTS
dac_deflator <- unique(fts_raw[destination_org_iso3 == "DAC", .(year, deflator)])

#Remove FTS dummy entries, non-country recipients, and CBPFs
fts <- fts[dummy == F]
fts <- fts[destination_org_iso3 != ""]

#Function to decode RCRC, SINGOs and national govs
local_org_decode <- function(org_type = c("RCRC", "SINGO", "Government"), fts, warn_on_missing = T){
  
  org_type <- match.arg(org_type)
  
  if(org_type == "RCRC") org_pattern <- "^Red Cross"
  if(org_type == "SINGO") org_pattern <- "^NGOs: International"
  if(org_type == "Government") org_pattern <- "^Governments"
  
  fts_temp <- fts[grepl(org_pattern, destination_orgtype), .(id, destination_org_iso3, destinationObjects_Organization.name)]
  fts_temp[, (org_type) := F]
  fts_temp <- merge(fts_temp, decode[type == org_type], by = "destinationObjects_Organization.name", all.x = T)
  if(warn_on_missing & nrow(fts_temp[is.na(decode_iso3)]) > 0){
    
    missing_orgs <- fts_temp[is.na(decode_iso3), unique(destinationObjects_Organization.name)]
    warning("There are organisations missing ", org_type, " decode: ", paste0(missing_orgs, collapse = ", "))
  }
  
  org_local <- paste0(org_type, "_local")
  
  local_ids <- fts_temp[decode_iso3 == destination_org_iso3, id]
  
  return(local_ids)
}

#Decodes
fts_rcrc_local_ids <- local_org_decode("RCRC", fts)
fts_singo_local_ids <- local_org_decode("SINGO", fts, warn_on_missing = F)
fts_gov_local_ids <- local_org_decode("Government", fts)

#Local NGOs
fts_ngo_local_ids <- fts[domestic_response == F & destination_orgtype %in% c("NGOs: Local", "NGOs: National", "Private Organisations: Local/National"), id]

##CBPFs
cbpf <- cbpf_raw
cbpf[PooledFundName == "DRC", PooledFundName := "Congo, the Democratic Republic of the"]

#Local NGOs
cbpf_ngo_local_ids <- cbpf[OrganizationType == "National NGO", ChfProjectCode]

#RCRC
cbpf_rcrc <- cbpf[OrganizationType == "Others" & OrganizationName != "", .(PooledFundName, OrganizationName, ChfProjectCode)]
cbpf_rcrc <- merge(cbpf_rcrc, decode[type == "RCRC"], by.x = "OrganizationName", by.y = "destinationObjects_Organization.name", all.x = T)

cbpf_rcrc_local_ids <- cbpf_rcrc[PooledFundName == decode_country, ChfProjectCode]

##UNHCR
unhcr_raw[, id := 1:nrow(unhcr_raw)]
unhcr <- unhcr_raw

#Local NGOs
unhcr_ngo_local_ids <- unhcr[`Partner Type DI coding` == "NGOs: National" | (`Partner Type DI coding` == "Private Organisations: Local/National" & `Partner Type` == "National NGO"), id]

#RCRC
unhcr_rcrc <- unhcr[`Partner Type DI coding` == "Red Cross: National" | `Partner Type` == "Others", .(`Country Name`, `Partner name`, `Partner Type DI coding`, id)]
unhcr_rcrc <- merge(unhcr_rcrc, decode[type == "RCRC"], by.x = "Partner name", by.y = "destinationObjects_Organization.name", all.x = T)

unhcr_rcrc_local_ids <- unhcr_rcrc[`Country Name` == decode_country | `Partner Type DI coding` == "Red Cross: National", id]

#Governments
unhcr_gov_local_ids <- unhcr[`Partner Type DI coding` == "Governments: National" | `Partner Type DI coding` == "Governments: Local" | (`Partner Type DI coding` == "Private Organisations: Local/National" & `Partner Type` == "Government"), id]

###Outputs
##FTS
fts_raw[, local := "N"]
fts_raw[id %in% fts_gov_local_ids, local := "Government"]
fts_raw[id %in% fts_rcrc_local_ids, local := "RCRC"]
fts_raw[id %in% fts_ngo_local_ids, local := "NGO"]
fts_raw[id %in% fts_singo_local_ids, local := "NGO"]

fts_local_summary <- fts_raw[local != "N" & dummy == F, .(source = "fts", amountUSD_defl_millions = sum(amountUSD_defl_millions)), by = .(year, direct = newMoney, local)]
fts_nonlocal_summary <- fts_raw[local == "N" & dummy == F & newMoney == T & domestic_response == F, .(source = "fts", amountUSD_defl_millions = sum(amountUSD_defl_millions)), by = .(year, direct = newMoney, local)]

#Exclude pooled funds for raw output
fts_raw <- fts_raw[!source_orgtype == "Pooled Funds: Country"]

#Identify GB orgs
fts_raw <- merge(fts_raw, fts_orgs_gb[, .(sourceObjects_Organization.id = as.character(id), gb_org = `GB signatory (TRUE/FALSE)`)], by = "sourceObjects_Organization.id", all.x = T)
fwrite(fts_raw, "output/fts_raw_local_output.csv")

fts_gb_summary <- fts_raw[gb_org == T & dummy == F & newMoney == T, .(source = "fts", amountUSD_defl_millions = sum(amountUSD_defl_millions)), by = .(year, direct = newMoney, local)][order(year, local)]
fwrite(fts_gb_summary, "output/fts_gb_summary.csv")

##CBPF
cbpf_raw[, local := "N"]
cbpf_raw[, direct := F]
cbpf_raw[ChfProjectCode %in% cbpf_rcrc_local_ids, local := "RCRC"]
cbpf_raw[ChfProjectCode %in% cbpf_ngo_local_ids, local := "NGO"]

cbpf_raw <- merge(cbpf_raw, dac_deflator, by.x = "AllocationYear", by.y = "year")
cbpf_raw[, amountUSD_defl_millions := Budget/deflator/1000000]

fwrite(cbpf_raw, "output/cbpf_raw_local_output.csv")

cbpf_local_summary <- cbpf_raw[local != "N", .(source = "cbpf", amountUSD_defl_millions = sum(amountUSD_defl_millions)), by = .(year = AllocationYear, direct, local)]
cbpf_nonlocal_summary <- cbpf_raw[local == "N", .(source = "cbpf", amountUSD_defl_millions = sum(amountUSD_defl_millions)), by = .(year = AllocationYear, direct, local)]

##UNHCR
unhcr_raw[, local := "N"]
unhcr_raw[, direct := F]
unhcr_raw <- merge(unhcr_raw, dac_deflator, by.x = "Year", by.y = "year")
unhcr_raw[, Budget := as.numeric(gsub(",", "", `Total Budget (USD)`))]
unhcr_raw[, amountUSD_defl_millions := Budget/deflator/1000000]

unhcr_raw[id %in% unhcr_gov_local_ids, local := "Government"]
unhcr_raw[id %in% unhcr_rcrc_local_ids, local := "RCRC"]
unhcr_raw[id %in% unhcr_ngo_local_ids, local := "NGO"]

fwrite(unhcr_raw, "output/unhcr_raw_local_output.csv")

unhcr_local_summary <- unhcr_raw[local != "N", .(source = "unhcr", amountUSD_defl_millions = sum(amountUSD_defl_millions)), by = .(year = Year, direct, local)]
unhcr_nonlocal_summary <- unhcr_raw[local == "N", .(source = "unhcr", amountUSD_defl_millions = sum(amountUSD_defl_millions)), by = .(year = Year, direct, local)]

##Aggregate
local_summary <- rbind(fts_local_summary, cbpf_local_summary, unhcr_local_summary)
nonlocal_summary <- rbind(fts_nonlocal_summary, cbpf_nonlocal_summary, unhcr_nonlocal_summary)

local_cast <- dcast(local_summary, direct + local + source ~ year, value.var = "amountUSD_defl_millions", fun.aggregate = sum)
nonlocal_cast <- dcast(nonlocal_summary, direct + local + source ~ year, value.var = "amountUSD_defl_millions", fun.aggregate = sum)

#Write
fwrite(local_cast, "output/local_output.csv")
fwrite(nonlocal_cast, "output/nonlocal_output.csv")

fts_cerf <- fts_raw[sourceObjects_Organization.name == "Central Emergency Response Fund"]
fts_cerf_2_orgs <- fts_cerf[, .(cerf_disb = sum(amountUSD_defl_millions, na.rm = T)), by = .(sourceObjects_Organization.name = destinationObjects_Organization.name, year)]
fts_cerf_2 <- merge(fts_raw[newMoney == F & dummy == F], fts_cerf_2_orgs, by = c("sourceObjects_Organization.name", "year"))
fts_cerf_2[, cerf_disb2 := cerf_disb*(amountUSD_defl_millions/sum(amountUSD_defl_millions, na.rm = T)), by = .(sourceObjects_Organization.name, year)]
fts_cerf <- rbind(fts_cerf, fts_cerf_2, fill = T)
fts_cerf_summary <- fts_cerf[, .(source = "fts", amountUSD_defl_millions = sum(amountUSD_defl_millions)), by = .(year, direct = sourceObjects_Organization.name == "Central Emergency Response Fund", local)][order(year, local)]

fwrite(fts_cerf_summary, "output/fts_cerf_summary.csv")
