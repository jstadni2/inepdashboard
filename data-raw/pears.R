## code to prepare `pears` dataset goes here

sites <- readxl::read_xlsx("data-raw/Sites_Export.xlsx", sheet = "Site Data") #update file

pa <- readxl::read_xlsx("data-raw/Program_Activities_Export.xlsx", sheet = "Program Activity Data") #update file

ia <- readxl::read_xlsx("data-raw/Indirect_Activities_Export.xlsx", sheet = "Indirect Activity Data") #update file
ia_ic <- readxl::read_xlsx("data-raw/Indirect_Activities_Export.xlsx", sheet = "Intervention Channels")

pse <- readxl::read_xlsx("data-raw/PSE_Site_Activities_Export.xlsx", sheet = "PSE Data") #update file

coa <- readxl::read_xlsx("data-raw/Coalitions_Export.xlsx", sheet = "Coalition Data") #update file
coa_members <- readxl::read_xlsx("data-raw/Coalitions_Export.xlsx", sheet = "Members")

part <- readxl::read_xlsx("data-raw/Partnerships_Export.xlsx", sheet = "Partnership Data") #update file

# usethis::use_data(pears, overwrite = TRUE)
