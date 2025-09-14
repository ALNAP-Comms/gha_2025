suppressPackageStartupMessages(lapply(c("data.table","rstudioapi", "readxl", "arrow"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(dirname(getActiveDocumentContext()$path)))

countrynames <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8")

#crs_raw <- data.table(read_parquet("Datasets/OECD DAC CRS/CRS-reduced.parquet"))
crs_raw <- fread("Datasets/OECD DAC CRS/crs_full.csv")
setwd(dirname(getActiveDocumentContext()$path))

keep <- c(
  "crs_id",
  "year",
  "flow_name",
  "bi_multi",
  "donor_name",
  "recipient_name",
  "usd_disbursement_defl",
  "sector_code",
  "purpose_code",
  "channel_name",
  "parent_channel_code"
)

old <- c("CrsID", "Year", "FlowName", "Bi_Multi", "DonorName", "RecipientName", "USD_Disbursement_Defl", "SectorCode", "PurposeCode", "ChannelName", "ParentChannelCode")

setnames(crs_raw, old, keep, skip_absent = T)


mdbs <- c(
  #RDBs
  "African Development Bank"
  ,"African Development Fund"
  ,"Asian Development Bank"
  ,"Asian Infrastructure Investment Bank"
  ,"Caribbean Development Bank"
  ,"Council of Europe Development Bank"
  ,"Development Bank of Latin America"
  ,"Inter-American Development Bank"
  ,"Islamic Development Bank"
  ,"European Bank for Reconstruction and Development"
  ,"International Investment Bank"
  ,"IDB Invest"
  
  #WB
  ,"International Development Association"
  ,"International Bank for Reconstruction and Development"
  ,"International Finance Corporation"
  
  #IMF
  #,"IMF (Concessional Trust Funds)"
  
  #Other IFIs
  #,"Arab Bank for Economic Development in Africa"
  #,"Arab Fund (AFESD)"
  #,"OPEC Fund for International Development"
  #,"Nordic Development Fund"
  )

crs <- crs_raw[, ..keep]
crs <- crs[
  flow_name == "ODA Loans" 
  |
    flow_name == "ODA Grants"
  | 
    flow_name == "Equity Investment"
  #| 
  #  flow_name == "Private Development Finance"
]

crs <- crs[year >= 2014]

crs[, Humanitarian := F]
crs[grepl("^7", sector_code), Humanitarian := T] #All sectors codes which begin with '7' (i.e. HA)

crs[recipient_name == "TÃ¼rkiye", recipient_name := "Türkiye"]

crs <- merge(crs, countrynames[, c("iso3", "countryname_oecd")], by.x = "recipient_name", by.y = "countryname_oecd", all.x = T)

# crs_ha <- crs[Humanitarian == T]
# crs_ha_rank <- crs_ha[!is.na(iso3), .(ha_total = sum(USD_Disbursement_Defl, na.rm = T)), by = .(Year, iso3, RecipientName)][, ha_rank := frank(-ha_total), by = Year][]
# fwrite(crs_ha_rank[ha_rank <= 20], "Total IHA/Fig 0.5/top_20_ha_recipients_list.csv")

#crs <- merge(crs, crs_ha_rank[, .(Year, RecipientName, ha_rank)], by = c("RecipientName", "Year"), all.x = T)

crs_mdbs <- crs[donor_name %in% mdbs, .(oda = sum(usd_disbursement_defl, na.rm = T), hum = sum(usd_disbursement_defl[grepl("^7", purpose_code, perl = T)], na.rm = T)), by = .(year, donor_name, recipient_name, iso3, flow_name, channel_name, parent_channel_code)]

fwrite(crs_mdbs, "MDBs_ODA.csv")

