## code to prepare `community_sites` dataset goes here

# Check if Selenium docker container is running
# If not, start Selenium container
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/system.html
# If fails, print directions to install Selenium image

# In terminal: docker run -d -p 4445:4444 selenium/standalone-chrome
# In terminal: docker run -d -p 4445:4444 selenium/standalone-firefox

system("cmd.exe", input = 'call "C:\\Program Files\\Docker\\Docker\\Docker Desktop.exe"')

Sys.sleep(10)

# Create container from image and delete when done
# system("docker run --name selchrome -d -p 4445:4444 selenium/standalone-chrome" )

# Use preexisting container
system("docker start selchrome" )

# Create firefox pofile (below doesn't work)

# fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = "D:/temp"))
# fprof.setAcceptUntrustedCertificates(TRUE)

# file_path <- getwd() %>% stringr::str_replace_all("/", "\\\\\\")
# 
# eCaps <- list(chromeOptions =
#                 list(
#                   prefs = list(
#                     # "profile.default_content_settings.popups" = 0L,
#                     # "download.prompt_for_download" = FALSE,
#                     # "directory_upgrade" = TRUE,
#                     # "download.default_directory" = "C:\\Users\\jstadni2\\Box\\FCS Data Analyst\\External Community Profile\\inepdashboard\\data-raw"
#                     # "download.default_directory" = file_path
#                   )
#                 ))

remDr <-  RSelenium::remoteDriver(
  remoteServerAddr = "192.168.1.2", # Find on C:\Windows\System32\drivers\etc\hosts
  port = 4445L,
  browserName = "chrome"#,
  # extraCapabilities = eCaps
)
remDr$open()

# # For local testing (doesn't work in test_that)
# rD <- RSelenium::rsDriver(browser="firefox", port=4545L, verbose=F)
# remDr <- rD[["client"]]
# remDr$open()

# Scrape data from DHS office locator (https://www.dhs.state.il.us/page.aspx?module=12)

wic_sites <- scrape_dhs_sites(remDr, "WIC Office")

fcrc_sites <- scrape_dhs_sites(remDr, "FCRC")

# remDr$close()

# ISBE Eligible Schools
# https://www.isbe.net/Pages/Nutrition-Data-Analytics-Maps.aspx

# Check for most recent download link

# eCaps <- list(chromeOptions = list(
#   args = c('--no-sandbox', '--disable-dev-shm-usage')
# ))
# 
# remDr <-  RSelenium::remoteDriver(
#   remoteServerAddr = "192.168.1.5",
#   port = 4445L,
#   browserName = "chrome",
#   extraCapabilities = eCaps
# )
# remDr$open()
# 
# remDr$navigate("https://www.isbe.net/Pages/Nutrition-Data-Analytics-Maps.aspx")
# 
# # remDr$findElement(using = "css selector", value = "#WebPartWPQ4 > div.ms-rtestate-field > ul")
# three_columns <-remDr$findElement(using = "css selector", value = "#WebPartWPQ4 > div:nth-child(1)")
# 
# html <- three_columns$getElementAttribute('innerHTML')[[1]]
# 
# # remDr$navigate("https://www.isbe.net/_layouts/Download.aspx?SourceUrl=/Documents/FY20-eligibility.xlsx")

download.file(
  url = "https://www.isbe.net/_layouts/Download.aspx?SourceUrl=/Documents/FY20-eligibility.xlsx",
  destfile = "./data-raw/Free and Reduced-Price Lunch Eligibility List (2020).xlsx", # rename
  mode = "wb"
)

eligible_schools <-
  readxl::read_xlsx("./data-raw/Free and Reduced-Price Lunch Eligibility List (2020).xlsx",
                    # rename, download as csv?
                    skip = 3) 

eligible_schools <- eligible_schools[eligible_schools$"Eligibility Percent" >= 50, ]

eligible_schools <-
  clean_community_sites(
    sites = eligible_schools,
    rename_cols = c("Site",
                    "Site Address",
                    "Site City",
                    # "Site County",
                    "Site Zip"), 
    site_type = "Eligible School"
  )

# IL Food Banks: Emergency Shelters
# http://www.illinoisfoodbanks.org/sites.asp

remDr$navigate("http://www.illinoisfoodbanks.org/sites.asp")

# Filter By: Type
remDr$findElement(using = "css selector", value = "#chkType")$clickElement()
# Type: Homeless Shelter
remDr$findElement(using = "css selector", value = "#sType > option:nth-child(4)")$clickElement()
# Display Type: List
remDr$findElement(using = "css selector", value = "#inputFieldBlock > table > tbody > tr > td:nth-child(3) > input[type=radio]")$clickElement()
remDr$findElement(using = "css selector", value = "#frmSubmit > div:nth-child(3) > input[type=submit]")$clickElement()

Sys.sleep(5)

table <- remDr$findElement(using = "css selector", value = "#contentPanel > table")
html <- table$getElementAttribute('outerHTML')[[1]]

emergency_shelters <- rvest::read_html(html) %>% 
  rvest::html_element("table") %>%
  rvest::html_table()

emergency_shelters <-
  clean_community_sites(
    sites = emergency_shelters,
    rename_cols = c("Site Name",
                    "Address",
                    "City",
                    # "County",
                    "ZIP"),
    site_type = "Emergency Shelter"
  )

# Combine and geocode wic_sites, fcrc_sites, eligible_schools, homeless_shelters

sites_to_geocode <-
  dplyr::bind_rows(
    wic_sites,
    fcrc_sites,
    eligible_schools, 
    emergency_shelters
    )

sites_geocoded <- sites_to_geocode %>%
  tidygeocoder::geocode_combine(
    queries = list(
      list(method = 'census', mode = 'batch'),
      list(method = 'census', mode = 'single'),
      list(method = 'osm')
    ),
    global_params = list(
      street = "site_address",
      city = "site_city",
      state = "site_state",
      postalcode = "site_zip"
    ),
    cascade = TRUE,
    lat = "latitude",
    long = "longitude"
  )

sites_geocoded <- sites_geocoded %>% dplyr::select(-query)

# df <- sites_geocoded[is.na(sites_geocoded$latitude), ]
# Took 17 mins
# 164 sites not geocoded (mostly PO boxes)
# Addresses with 2 lines aren't geocoded

# Head Start Centers
# Using Head Start API
# https://eclkc.ohs.acf.hhs.gov/developers/signup
# https://eclkc.ohs.acf.hhs.gov/developers/guide

eclkc_q <- eclkc_query(key, "IL") # store key in env?

head_start_centers <- jsonlite::fromJSON(eclkc_q)$documents

head_start_centers$zipFour <-
  gsub("-1", "", head_start_centers$zipFour)

head_start_centers$zip <-
  ifelse(
    head_start_centers$zipFour != "",
    paste(head_start_centers$zipFive, head_start_centers$zipFour, sep = "-"),
    head_start_centers$zipFive
  )

head_start_centers <-
  clean_community_sites(
    head_start_centers,
    rename_cols = c(
      "name",
      "addressLineOne",
      "city",
      # "county",
      "zip"
      ),
    site_type = "Head Start Center",
    coords = TRUE
  )

# Federally Qualified Health Centers
# https://data.hrsa.gov/geo

# remDr$navigate("https://data.hrsa.gov/geo")
# 
# # Select State/Territory > Illinois
# remDr$findElement(using = "css selector", value = "#stateSelection > option:nth-child(18)")$clickElement()
# # Click Submit
# remDr$findElement(using = "css selector", value = "#app > div > div.text-center > div > div > div > ul > li.btns.clearfix > button.btn.btn-sm.btn-primary.btn-submit")$clickElement()
# # Open Table: Health Center Service Delivery and Look-Alike Sites
# remDr$findElement(using = "css selector", value = "#DataTables_Table_1 > tbody > tr:nth-child(1) > td.text-nowrap.dataTablesCenterAligned > a")$clickElement()
# remDr$findElement(using = "css selector", value = "#DataTables_Table_1 > tbody > tr:nth-child(1) > td.text-nowrap.dataTablesCenterAligned")$clickElement()
# 
# # Navigate directly to query instead?
# remDr$navigate("https://data.hrsa.gov/tools/data-explorer?paramServiceId=EHB%20Site&paramTyp=state&paramCd=17")
# # Show / Hide Columns
# remDr$findElement(using = "css selector", value = "#app > div > div.data-portal-area.table-view > div.table-responsive > div.find-data-head.clearfix > div.find-data-search.form-group.form-group-sm > div.tutorial-tooltip-container-2.dropdown > button")$clickElement()
# 
# # Export Data XLSX

# Website/RSelenium keeps crashing while loading query

# Alternatively, download nationwide dataset here:
# https://data.hrsa.gov/DataDownload/DD_Files/Health_Center_Service_Delivery_and_LookAlike_Sites.csv

download.file(
  url = "https://data.hrsa.gov/DataDownload/DD_Files/Health_Center_Service_Delivery_and_LookAlike_Sites.csv",
  destfile = "./data-raw/Health_Center_Service_Delivery_and_LookAlike_Sites.csv", # rename
  mode = "wb"
)

# import Health_Center_Service_Delivery_and_LookAlike_Sites.csv and prep it for community_sites

# USE HRSA API INSTEAD
# https://data.hrsa.gov/data/services

# Combine all sites into community_sites

community_sites <-
  dplyr::bind_rows(
    sites_geocoded,
    head_start_centers#, 
    # fqhcs
  )

# reference
# "C:\Users\jstadni2\Box\FCS Data Analyst\Community Networks Dashboard\Community Networks Choropleth Map\Eligible Sites Data Cleaning.R"
# or 
# "C:\Users\jstadni2\Box\FCS Data Analyst\SNAP-Ed Sites List\Eligible Sites Data Cleaning.R"

usethis::use_data(community_sites, overwrite = TRUE)
