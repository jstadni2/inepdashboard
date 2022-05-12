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

# IL Food Banks: Homeless Shelters
# http://www.illinoisfoodbanks.org/sites.asp
# Filter By: Type
# Type: Homeless Shelter
# Display Type: List

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
