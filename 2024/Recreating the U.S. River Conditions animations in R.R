library(dataRetrieval)
library(tidyverse) # includes dplyr, tidyr
library(maps)
library(sf)

# Viz dates
viz_date_start <- as.Date("2023-12-01")
viz_date_end <- as.Date("2023-12-31")

# Currently set to CONUS (to list just a few, do something like `c("MN", "WI", "IA", "IL")`)
viz_states <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
                "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
                "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# Albers Equal Area, which is good for full CONUS
viz_proj <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

# Configs that could be changed ...
param_cd <- "00060" # Parameter code for streamflow . See `dataRetrieval::parameterCdFile` for more.
service_cd <- "dv" # means "daily value". See `https://waterservices.usgs.gov/rest/` for more info.

# Can only query one state at a time, so need to loop through. 
# For one month of CONUS data, this took ~ 1 minute.

sites_available <- data.frame() # This will be table of site numbers and their locations.
for(s in viz_states) {
  # Some states might not return anything, so use `tryCatch` to allow
  # the `for` loop to keep going.
  message(sprintf("Trying %s ...", s))
  sites_available <- tryCatch(
    whatNWISsites(
      stateCd = s, 
      startDate = viz_date_start,
      endDate = viz_date_end,
      parameterCd = param_cd, 
      service = service_cd) %>%
      select(site_no, station_nm, dec_lat_va, dec_long_va), 
    error = function(e) return()
  ) %>% bind_rows(sites_available)
}

head(sites_available)

# For one month of CONUS, this took ~ 15 min

# Can only pull stats for 10 sites at a time, so loop through chunks of 10
#   sites at a time and combine into one big dataset at the end!
sites <- unique(sites_available$site_no)

# Divide request into chunks of 10
req_bks <- seq(1, length(sites), by=10)

viz_stats <- data.frame()
for(chk_start in req_bks) {
  chk_end <- ifelse(chk_start + 9 < length(sites), chk_start + 9, length(sites))
  # Note that each call throws warning about BETA service :)
  message(sprintf("Pulling down stats for %s:%s", chk_start, chk_end))
  viz_stats <- tryCatch(readNWISstat(
    siteNumbers = sites[chk_start:chk_end],
    parameterCd = param_cd,
    statType = c("P25", "P75")
  ) %>% select(site_no, month_nu, day_nu, p25_va, p75_va), 
  error = function(e) return()
  ) %>% bind_rows(viz_stats)
}

head(viz_stats)

# For all of CONUS, there are 8500+ sites.
# Just in case there is an issue, use a loop to cycle through 100
# at a time and save the output temporarily, then combine after.
# For one month of CONUS data, this took ~ 10 minutes

max_n <- 100
sites <- unique(sites_available$site_no)
sites_chunk <- seq(1, length(sites), by=max_n)
site_data_fns <- c()
tmp <- tempdir()

for(chk_start in sites_chunk) {
  chk_end <- ifelse(chk_start + (max_n-1) < length(sites), 
                    chk_start + (max_n-1), length(sites))
  
  site_data_fn_i <- sprintf(
    "%s/site_data_%s_%s_chk_%s_%s.rds", tmp, 
    format(viz_date_start, "%Y_%m_%d"), format(viz_date_end, "%Y_%m_%d"),
    # Add leading zeroes in front of numbers so they remain alphanumerically ordered
    sprintf("%02d", chk_start), 
    sprintf("%02d", chk_end))
  site_data_fns <- c(site_data_fns, site_data_fn_i)
  
  if(file.exists(site_data_fn_i)) {
    message(sprintf("ALREADY fetched sites %s:%s, skipping ...", chk_start, chk_end))
    next
  } else {
    site_data_df_i <- readNWISdata(
      siteNumbers = sites_available$site_no[chk_start:chk_end],
      startDate = viz_date_start,
      endDate = viz_date_end,
      parameterCd = param_cd,
      service = service_cd
    ) %>% renameNWISColumns() %>% 
      select(site_no, dateTime, Flow) 
    saveRDS(site_data_df_i, site_data_fn_i)
    message(sprintf("Fetched sites %s:%s out of %s", chk_start, chk_end, length(sites)))
  }
  
}

# Read and combine all of them
viz_daily_values <-  lapply(site_data_fns, readRDS) %>% bind_rows()

nrow(viz_daily_values) # ~250,000 obs for one month of CONUS data

head(viz_daily_values)


