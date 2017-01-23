##################################################################
##################################################################
##################################################################

library(gtrendsR)
library(data.table)
library(stringi)
library(magrittr)

gconnect("user@gmail.com", "pwd")   

##################################################################
##################################################################
##################################################################

data(countries)
countries <- as.data.table(countries)
countries[, description := as.character(description)]

# get the countries with most searches
res <- gtrends(query = "/m/0rytj3p", 
               start_date = "2012-04-01",
               cat = "0",
               silent = TRUE)
regions <- res$Top.regions.for.Candy.Crush.Saga$Region
regions <- toupper(regions)

# regex for selected countries
rgx <- paste0("^\\.", regions, "$", collapse = "|")
stri_detect_regex(str = ".HONG KONG ", pattern = rgx)
stri_detect_regex(str = ".HONG KONG", pattern = rgx)

# select country codes
sel.countries <- countries[stri_detect_regex(str = description, pattern = rgx),]
sel.countries

# regions not found:
filter <- paste0(".",tolower(regions)) %in% tolower(sel.countries$description)
regions2 <- regions[!filter]
regions2[4] <- "VIET NAM"
rgx2 <- paste0("^\\.", regions2, ".*", collapse = "|")
stri_detect_regex(str = ".HONG KONG ", pattern = rgx2)

# select additional country codes
sel.countries2 <- countries[stri_detect_regex(str = description, pattern = rgx2),]
sel.countries2

sel.countries <- rbind(sel.countries, sel.countries2)

##################################################################
##################################################################
##################################################################

q.inputs <- as.character(sel.countries$country_code)

gt <- lapply(q.inputs, FUN = function(xx) {
  cat(".")  
  res <- NULL
  try(res <- gtrends(query = "/m/0rytj3p", 
                     start_date = "2012-04-01",
                     cat = "0",
                     geo = c("US",xx), silent = TRUE))
  if (is.null(res)) {
    
    # reconnect
    Sys.sleep(60)
    try(res <- gtrends(query = "/m/0rytj3p", 
                       start_date = "2012-04-01",
                       cat = "0",
                       geo = c("US",xx), silent = TRUE))
    
  }
  
  return(res)
})

##################################################################
##################################################################
##################################################################

# extract and save
# normalize to US
names(gt) <- q.inputs
dta <- lapply(gt, function(xx){
  
  tmp <- xx$trend
  max.hits.us <- max(tmp$hits[tmp$location == "US"])
  return(tmp$hits[tmp$location != "US"] * 100 / max.hits.us)
  
})
dta <- do.call(cbind, dta)
dta <- data.frame(start = gt[[1]]$trend$start[gt[[1]]$trend$location == "US"],
                  end = gt[[1]]$trend$end[gt[[1]]$trend$location == "US"], dta)
head(dta[dta$start > as.Date("2013-12-01"),])

tmp <- apply(dta[, -c(1:2)], 2, sum) %>% prop.table %>% sort
tmp * 500

cbind(q.inputs, sapply(gt, is.null))
sapply(gt, is.null) %>% table

# https://www.google.com/trends/explore?date=today%205-y,today%205-y,today%205-y&geo=HK,US,NL&q=%2Fm%2F0rytj3p,%2Fm%2F0rytj3p,%2Fm%2F0rytj3p

plot(dta[,"HK"], type = "l")
lines(dta[,"US"], type = "l")
lines(dta[,"NL"], type = "l")

write.table(dta, file = "../data/GoogleTrends/gtrends_CCS.csv", row.names = FALSE)

##################################################################
##################################################################
##################################################################

# scrap the number of internauts

# https://www.r-bloggers.com/scraping-table-from-any-web-page-with-r-or-cloudstat/
# http://www.internetlivestats.com/internet-users-by-country/

