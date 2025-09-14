required.packages <- c("data.table","jsonlite","httr","readxl", "rstudioapi")
lapply(required.packages, require, character.only=T)

invisible(lapply(c(
  "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_api_appeals.R",
  "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_appeals_data.R"
), source))

fts_get_appeal_totals <- function(appeal_id, year){
  
  required.packages <- c("data.table","jsonlite","httr","XML")
  lapply(required.packages, require, character.only=T)
  
  planlink <- paste0('https://fts.unocha.org/appeals/', appeal_id, "/clusters")
  while(T){ 
    data <- htmlParse(RETRY("GET", planlink, quiet = T), encoding = "UTF-8")
    
    plan_name = xpathSApply(data, "//h1[@class='cd-page-title']", xmlValue)
    plan_name <- gsub("\\n", "", plan_name)
    
    total_table <- xpathSApply(data, "//table[contains(@class, 'header-totals')]")
    if(length(plan_name) > 0) break
    message(appeal_id, " failed to load, retrying...")
  }
  
  if(length(total_table) != 0){
    table <- data.table(readHTMLTable(total_table[[1]]))
    table <- setnames(data.table(tail(t(table), -1)), gsub("Filtered outgoing", "Outgoing", gsub("Filtered incoming", "Total incoming", table$V1)))
    table <- table[, lapply(.SD, function(x) as.numeric(gsub("US|[$]|,|%", "", x)))]
    
    out <- cbind(appeal_id = appeal_id, plan_name = plan_name, year = year, table)
  } else {
    if(plan_name == "Page not found"){
      out <- cbind(appeal_id = appeal_id, plan_name = plan_name, year = year)
    } else {
      warning(appeal_id, " did not load.")
    }
  }
  return(out)
}


setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

isos <- fread("Datasets/Countrynames/isos.csv", encoding = "UTF-8")

#Combined RRPs created manually
combined_rrps <- fread("Datasets/UNHCR RRPs/combined_rrps.csv", encoding = "UTF-8")

##UNOCHA##
unocha_plans <- fts_get_appeals(2015:2025)
unocha_plans[, `:=` (iso3 = unlist(lapply(locations, function(x) paste0(x$iso3[x$adminLevel == 0], collapse = ";"))), type = unlist(lapply(categories, function(x)  paste0(x$name[x$group == "planType"], collapse = ";"))))]

unocha_plans_list <- list()
for(i in 1:nrow(unocha_plans)){
  id <- unocha_plans[i]$id
  hrp_year <- unocha_plans[i]$years[[1]]$year
  unocha_plans_list[[i]] <- fts_get_appeal_totals(id, hrp_year)
}
unocha_plans_dt <- rbindlist(unocha_plans_list[unlist(lapply(unocha_plans_list, function(x) is.data.frame(x)))], fill = T)

unocha_plans <- merge(unocha_plans[, .(id, type, iso3)], unocha_plans_dt[, .(id = appeal_id, plan_name, year, funds = `Total incoming funding:`, req = `Total current requirements:`)], by = "id", all = T)

unhcr_plans <- combined_rrps[`Use?` == "UNHCR" & Year %in% unocha_plans$year, .(id = NA, type = "Regional response plan", iso3 = NA, plan_name = Name, year = Year, funds = Funding, req = Requirements)]
unocha_plans <- unocha_plans[!(plan_name %in% combined_rrps[`Use?` == "UNHCR"]$`UNOCHA name`) & !is.na(year)]

all_appeals <- rbind(unocha_plans, unhcr_plans)[order(year)]

# #Based on combined RRPs, select which OCHA RRPs we render
# unocha_plans <- unocha_plans[!(plan_name %in% combined_rrps[`Use?` == "UNHCR"]$`UNOCHA name`)]

# ##UNHCR##
# unhcr_rrps <- fread("datasets/UNHCR RRPs/rrp_data.csv", encoding = "UTF-8")[Year %in% c(2013:2023)]
# unhcr_rrps[Country == "Iran, Islamic Republic of", Country := "Iran (Islamic Republic of)"]
# unhcr_rrps[Country == "Burma", Country := "Myanmar"]
# unhcr_rrps[Country == "Czech Republic", Country := "Czechia"]
# unhcr_rrps[Country == "Moldova", Country := "Republic of Moldova"]
# unhcr_rrps[Country == "Slovak Republic", Country := "Slovakia"]
# unhcr_rrps[Country == "Curaçao", Country := "Curacao"]
# unhcr_rrps[Country == "United States", Country := "United States of America"]
# 
# unhcr_rrps <- merge(unhcr_rrps, isos[, .(iso3, Country = countryname_unhcr)], by = "Country", all.x = T)
# 
# unhcr_rrps <- unhcr_rrps[(paste0(RRP, Year) %in% combined_rrps[`Use?` == "UNHCR", paste0(`UNHCR name`, Year)])]
# unhcr_rrps <- unhcr_rrps[, .(country = Country, year = Year, RRP, req = as.numeric(gsub("[$]|,", "", `Funds Requested`)), funds = as.numeric(gsub("[$]|,", "", `Funds Received (From all agencies)`)))]
# 
# unhcr_rrps <- merge(isos[, .(iso3, country = countryname_unhcr)], unhcr_rrps, by = "country", all.y = T)

##All plans
#all_appeals <- rbind(unocha_plans, unhcr_rrps[, .(id = NA, year, type = "Regional response plan", iso3, plan_name = RRP, req, funds)])

all_appeals <- rbind(all_appeals, data.table(id = NA, year = 2025, type = "Humanitarian repsonse plan", iso3 = "ETH", plan_name = "Ethiopia Humanitarian Response Plan 2025", req = 2000000000, funds = 0))

all_appeals_total <- all_appeals[!is.na(year), .(funding = sum(funds, na.rm = T), requirements = sum(req, na.rm = T)), by = year][order(year)]

setwd(dirname(getActiveDocumentContext()$path))
fwrite(all_appeals, "all_appeals_new.csv")
fwrite(all_appeals_total, "all_appeals_req_funds_new.csv")
