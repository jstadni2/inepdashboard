## code to prepare `community_sites` dataset goes here

# Scrape data from DHS office locator (https://www.dhs.state.il.us/page.aspx?module=12)

# Can't get these to work

# ## Using docker

# # In terminal: docker run -d -p 4445:4444 selenium/standalone-chrome
# # In terminal: docker run -d -p 4445:4444 selenium/standalone-firefox
#

# Create firefox pofile

# fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = "D:/temp"))
# fprof.setAcceptUntrustedCertificates(TRUE)

remDr <-  RSelenium::remoteDriver(
  remoteServerAddr = "192.168.1.5",
  port = 4445L,
  browserName = "chrome"
)
remDr$open()
# 
# ## Using rsDriver() (https://docs.ropensci.org/RSelenium/articles/basics.html)
# 
# rD <- RSelenium::rsDriver(verbose = FALSE)
# 
# ## Using Chrome (version 101 installed, 102 required)
# 
# rD <- RSelenium::rsDriver(browser = "chrome",
#                           port = 4545L,
#                           verbose = F)

# Using local system

# rD <- RSelenium::rsDriver(browser = "firefox",
#                           port = 4545L,
#                           verbose = F)
# 
# remDr <- rD[["client"]]
# 
# remDr$open()

remDr$navigate("https://www.dhs.state.il.us/page.aspx?module=12&officetype=&county")

remDr$findElement(using = "css selector",
                  "#SearchOffice_OfficeTypeDropDownList > option:nth-child(20)")$clickElement()



# Select Office Type - WIC
remDr$findElement(using = 'xpath', "/html/body/div[2]/div[2]/form/div[3]/div[2]/div[1]/select/option[20]")$clickElement()
# Click "Find Offices"
remDr$findElement(using = 'xpath', "//*[@id='SearchOffice_FindOfficesButton']")$clickElement()

Sys.sleep(5)

# search_results <- remDr$findElements("id", "SearchResults")
search_results <- remDr$findElement(using = 'xpath', "//*[@id='OfficeList']")

html <- search_results$getElementAttribute('innerHTML')[[1]]

# Close the current window
remDr$close()

# Close all browser windows and ends the WebDriver session
remDr$quit()

# html <- remDr$findElement(using = 'xpath', "/html/body/pre/span[808]")$getPageSource()[[1]]
# 
# html <- remDr$getPageSource()[[1]]

nodes <-
  rvest::html_nodes(x = rvest::read_html(html), css = "li")

dhs_sites <- data.frame(
  site_name = nodes %>%
    rvest::html_element("h3") %>%
    rvest::html_text2(),
  full_address = nodes %>%
    rvest::html_element(css = ".OfficeAddress") %>%
    rvest::html_text2()
)

dhs_sites <-
  dhs_sites %>% tidyr::separate("full_address", c("address", "city_state_zip"), sep = "\n") %>%
  tidyr::separate("city_state_zip", c("site_city", "state_zip"), sep = ", ") %>%
  tidyr::separate("state_zip", c("site_state", "site_zip"), sep = " ")




# parse address

dhs_wic_sites <- rvest::read_html(html) %>% html_element("h3") %>% 
  html_text3()

usethis::use_data(community_sites, overwrite = TRUE)
