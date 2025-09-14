#METHODOLOGY:
#1. DOWNLOAD AND FIX ISO CODE (LINES 8 - 65)
#2. CALCULATE CUMULATIVE GROWTH RATES FROM START YEAR IN DATASET (1980), EG. 1980 = 1, 1981 = 1.01, 1982 = 1.04 ETC. (LINES 83-84)
#3. NORMALISE GROWTH RATES BY DIVIDING BY BASE YEAR (LINE 85)
#4. MULTIPLY NORMALISED VALUE FOR X YEAR BY CURRENT VALUE IN BASE YEAR (LINES 87 - 89)
#5. DIVIDE CURRENT BY CONSTANT (LINE 92)

get_deflators <- function(base_year = 2022, currency = "USD", weo_ver = NULL, approximate_missing = T){
  suppressPackageStartupMessages(lapply(c("data.table", "httr", "jsonlite"), require, character.only=T))
  
  if(is.null(weo_ver)){
    
    tyear <- year(Sys.Date())
    tmonth <- month(Sys.Date())
    
    weo_month <- ifelse(tmonth <= 10 & tmonth >= 4, 4, 10)
    weo_year <- ifelse(tmonth < 4, tyear-1, tyear)
    
    weo_ver <- format(as.Date(paste("1", weo_month, weo_year, sep = "-"), "%d-%m-%Y"), "%b%Y")
    current_month <- month.name[weo_month]
  }
  
  ##WEO data
  pweo_ver <- as.Date(paste0("1", weo_ver), "%d%b%Y")
  weo_year <- year(pweo_ver)
  weo_month <- month(pweo_ver)
  
  while(T){
    url <- paste0("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/", weo_year, "/", current_month, "/WEO", weo_ver ,"all.ashx")
    response <- GET(url)
    if(response$headers$`content-type` == "application/vnd.ms-excel") break
    
    if(weo_month <= 10 & weo_month > 4){
      weo_month <- 4
    } else {
      if(weo_month <= 4){
        weo_year <- weo_year - 1
      }
      weo_month <- 10
    }
    weo_ver <- format(as.Date(paste("1", weo_month, weo_year, sep = "-"), "%d-%m-%Y"), "%b%Y")
  }
  
  message("Using IMF WEO version ", weo_ver, ".")
  
  content <- response$content
  char_content <- rawToChar(content[content !='00'])
  temp_file <- tempfile(fileext= ".csv")
  writeLines(char_content, temp_file)
  weo <- suppressWarnings(fread(temp_file, na.strings=c("n/a", "--")))
  unlink(temp_file)
  
  country_codes <- unique(weo[, .(ISO, Country)])
  
  data_cols <- c("ISO", "WEO Subject Code", grep("^\\d{4}$", names(weo), value = T))
  
  weo <- melt(weo[, ..data_cols], id.vars = c("ISO", "WEO Subject Code"), variable.factor = F)
  weo[, value := as.numeric(gsub(",", "", value))]
  
  #Fix PSE ISO code
  weo[ISO == "WBG", ISO := "PSE"]
  
  write.csv(weo, "weo_deflators_raw.csv", row.names = FALSE)

  
  #GDP in current prices
  if(currency == "USD"){
    weo_gdp_cur <- weo[`WEO Subject Code` == "NGDPD"]
  }
  if(currency == "LCU"){
    weo_gdp_cur <- weo[`WEO Subject Code` == "NGDP"]
  }
  if(currency == "PPP"){
    weo_gdp_cur <- weo[`WEO Subject Code` == "PPPGDP"]
  }
  
  weo_gdp_cur <- weo_gdp_cur[, .(ISO, variable, gdp_cur = value)]
  
  #GDP real growth rates
  weo_gdp_pcg <- weo[`WEO Subject Code` == "NGDP_RPCH"]
  
  #GDP cumulative growth rates
  weo_gdp_pcg <- weo_gdp_pcg[, gdp_cg := 1+ifelse(is.na(value), 0, value/100), by = ISO]
  weo_gdp_pcg[, gdp_cg := ifelse(!(!is.na(value) | !is.na(shift(value, -1))), NA, cumprod(gdp_cg)), by = ISO]
  weo_gdp_pcg[, gdp_cg := gdp_cg/gdp_cg[variable == base_year], by = ISO][, value := NULL]
  
  #GDP in constant prices
  weo_gdp_con <- merge(weo_gdp_pcg[, .(ISO, variable, gdp_cg)], weo_gdp_cur)
  weo_gdp_con[, `:=` (gdp_con = gdp_cg*gdp_cur[variable == base_year]), by= ISO]
  write.csv(weo_gdp_con, "weo_gdp_cur_con.csv", row.names = FALSE)
  
  #GDP deflators from WEO
  weo_deflators <- weo_gdp_con[, .(gdp_defl = gdp_cur/gdp_con), by = .(ISO, variable)]
  weo_deflators <- cbind(weo_deflators, source = "WEO", ver = weo_ver)
  write.csv(weo_deflators, "weo_deflators.csv", row.names = FALSE)
}