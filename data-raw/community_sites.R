## code to prepare `community_sites` dataset goes here

# Check if Selenium docker container is running
# If not, start Selenium container
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/system.html
# If fails, print directions to install Selenium image

# In terminal: docker run -d -p 4445:4444 selenium/standalone-chrome
# In terminal: docker run -d -p 4445:4444 selenium/standalone-firefox

system("cmd.exe", input = 'call "C:\\Program Files\\Docker\\Docker\\Docker Desktop.exe"')

system("docker run -d -p 4445:4444 selenium/standalone-chrome")

# Create firefox pofile (below doesn't work)

# fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = "D:/temp"))
# fprof.setAcceptUntrustedCertificates(TRUE)

remDr <-  RSelenium::remoteDriver(
  remoteServerAddr = "192.168.1.5",
  port = 4445L,
  browserName = "chrome"
)
remDr$open()

# Scrape data from DHS office locator (https://www.dhs.state.il.us/page.aspx?module=12)

wic_sites <- scrape_dhs_sites(remDr, "WIC")

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

site_cols <- c("site_name",
               "site_address",
               "site_city",
               "site_county",
               "site_zip")

eligible_schools <-
  readxl::read_xlsx("./data-raw/Free and Reduced-Price Lunch Eligibility List (2020).xlsx", # rename
                    skip = 3) %>% dplyr::rename_at(dplyr::all_of(c(
                      "Site", "Site Address", "Site City", "Site County", "Site Zip"
                    )),
                    ~ site_cols)

eligible_schools$site_state <- "IL"
eligible_schools$site_type <- "Eligible School"

eligible_schools <-
  eligible_schools %>% dplyr::select(dplyr::starts_with("site_"))


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

# Create wrapper function starting here

emergency_shelters <-
  emergency_shelters %>% dplyr::rename_at(dplyr::all_of(c(
    "Site Name", "Address", "City", "County", "ZIP"
  )),
  ~ site_cols)

emergency_shelters$site_state <- "IL"
emergency_shelters$site_type <- "Emergency Shelter"

emergency_shelters <-
  emergency_shelters %>% dplyr::select(dplyr::starts_with("site_"))

# Wrapper ends here


# rbind and geocode wic_sites, fcrc_sites, eligible_schools, homeless_shelters

# Head Start Centers
# https://eclkc.ohs.acf.hhs.gov/center-locator?latitude=40.633&longitude=-89.399&state=IL

# Federally Qualified Health Centers
# https://data.hrsa.gov/geo

# rbind all sites into community_sites
# reference
# "C:\Users\jstadni2\Box\FCS Data Analyst\Community Networks Dashboard\Community Networks Choropleth Map\Eligible Sites Data Cleaning.R"
# or 
# "C:\Users\jstadni2\Box\FCS Data Analyst\SNAP-Ed Sites List\Eligible Sites Data Cleaning.R"

# usethis::use_data(community_sites, overwrite = TRUE)
