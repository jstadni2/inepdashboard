## code to prepare `pears` dataset goes here

sites <- read_excel("Sites_Export.xlsx", sheet = "Site Data") #update file

pa <- read_excel("Program_Activities_Export.xlsx", sheet = "Program Activity Data") #update file

ia <- read_excel("Indirect_Activities_Export.xlsx", sheet = "Indirect Activity Data") #update file
ia_ic <- read_excel("Indirect_Activities_Export.xlsx", sheet = "Intervention Channels")

pse <- read_excel("PSE_Site_Activities_Export.xlsx", sheet = "PSE Data") #update file

coa <- read_excel("Coalitions_Export.xlsx", sheet = "Coalition Data") #update file
coa_members <- read_excel("Coalitions_Export.xlsx", sheet = "Members")

part <- read_excel("Partnerships_Export.xlsx", sheet = "Partnership Data") #update file

# usethis::use_data(pears, overwrite = TRUE)
