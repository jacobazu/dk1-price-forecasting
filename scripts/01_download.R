
# downloading DK1 day ahead prices and save them to data/raw/
# hourly prices end 2025-09-30; 15-minute prices start 2025-10-01.

source("R/get_eds.R")


hourly <- get_eds("Elspotprices",   "2019-01-01", "2025-10-01")
qh     <- get_eds("DayAheadPrices", "2025-10-01", format(Sys.Date() + 2))

saveRDS(hourly, "data/raw/elspot_hourly.rds")
saveRDS(qh,     "data/raw/dayahead_15min.rds")
