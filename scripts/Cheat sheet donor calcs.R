required.packages <- c("data.table")
lapply(required.packages, require, character.only=T)
setwd(dirname(getActiveDocumentContext()$path))

fts <- fread("FTS - Full Dataset\\Datasets\\fts_curated_2024.csv")

#Aggregate multiple specified sectors to 'multi-sector', remove unspecified after
top_clusters <- fts[, cluster := gsub("Multiple clusters specified", "Multi-sector", gsub(" -.*", "", destination_globalcluster))][dummy == F & newMoney == T, .(amount = sum(amountUSD_defl_millions, na.rm = T)), by = .(source_org_iso3, cluster)][, rank := rank(-amount), by = source_org_iso3][, share := amount/sum(amount), by = source_org_iso3][order(source_org_iso3, rank)]

#Country-allocable aid only
top_recipients <- fts[dummy == F & newMoney == T & destination_org_iso3 != "", .(amount = sum(amountUSD_defl_millions, na.rm = T)), by = .(source_org_iso3, destination_org_iso3)][, rank := rank(-amount), by = source_org_iso3][, share := amount/sum(amount), by = source_org_iso3][order(source_org_iso3, rank)]

fwrite(top_clusters, "CS_Donor_clusters.csv")
fwrite(top_recipients, "CS_Donor_recipients.csv")
