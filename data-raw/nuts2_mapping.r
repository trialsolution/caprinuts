library("tidyverse")
library("xlsx")

library("gdxrrw")
#igdx("c:/Users/Dev/24.8/")
igdx("/opt/GAMS/")


# First step was to convert Celso's .kml file (that was developed for the Qlick application)
# to a .csv file using Google FusionTables

# Then we read in the nuts2 mapping from the csv file 
# Every CAPRI region is mapped to an official NUTS2 region from a given NUTS classification version (versions indicated by their releas year)
caprinuts <- read.csv(file = "capri_map.csv", header = TRUE, sep = ",", encoding = "UTF-8", colClasses = c(rep("character",3), "integer","NULL"))
caprinuts <- as_tibble(caprinuts)


# read SAPM data on winter crops, catch crops and mulching
# provided by Angel in a .gdx file
# SAPM version 2010

sapm_data <- rgdx.param("IA_input_data.gdx", "p_greenAreaRL")
colnames(sapm_data) <- c("region","cropcategory","activity","indicator","time","value")
sapm_data <- as_tibble(sapm_data)

# do some partial merge-ing

# NOte: winter crops are available at country level in SAPM
#       catch-crops and mulching are at regional level in SAPM
#       only four winter crops covered: rye, soft wheat, barley and rapeseed

# SAPM data for countries (only winter crops)
sapm_countries   <- sapm_data %>% filter(nchar(as.character(region)) > 4)
# SAPM data for regions (only mulching and catch crops)
sapm_regions     <- sapm_data %>% filter(nchar(as.character(region)) < 5)


# countries that are modelled in CAPRI as one NUTS2 region
capri_countries  <- caprinuts %>% filter(nchar(description) == 2)
# regions that are not countries in CAPRI
capri_regions    <- caprinuts %>% filter(nchar(description) > 2)

# get catch crop-specific subset of data
catchcrop <- sapm_regions %>% filter(cropcategory == "CatchCrop")
catchcrop <- subset(catchcrop, select=-c(activity,indicator,time))


# mapping catch crop data to CAPRI
cc2CAPRI  <- capri_regions %>% left_join(catchcrop, by = c("description" = "region"))

# have a look at those regions that could not be mapped
notmapped <- cc2CAPRI %>% filter(is.na(value))
# the rest is already mapped
cc2CAPRI  <- cc2CAPRI %>% filter(!is.na(value))





# special treatment for Italy
ccIT <- catchcrop %>% filter(grepl("IT.*",region))
missingIT <- notmapped %>% filter(grepl("IT.*", description)) 


# NUTS classification changes
# here we modify the original catchcrop data frame 
# and then we rerun the merge to derive cc2CAPRI
# ITE --> ITI (nuts version 2006 --> nuts version 2010)
capri_regions <- capri_regions %>% mutate(region = gsub("ITE","ITI",description))
# similarly ITD --> ITH
capri_regions <- capri_regions %>% mutate(region = gsub("ITD","ITH",description))



# Italy solved, remove them from the list
notmapped <- notmapped %>% filter(!grepl("IT.*",description))
rm(ccIT, missingIT)



# special treatment for Finland
ccFI <- catchcrop %>% filter(grepl("FI.*",region))
missingFI <- notmapped %>% filter(grepl("FI.*", description)) 

# FI18 --> FI1B (partly)
capri_regions <- capri_regions %>% mutate(region = gsub("FI18","FI1B",description))
# FI13 --> FI1D (partly)
capri_regions <- capri_regions %>% mutate(region = gsub("FI13","FI1D",description))
# FI1A --> FI1D (partly)
capri_regions <- capri_regions %>% mutate(region = gsub("FI1A","FI1D",description))


# Finland solved, remove them from the list
notmapped <- notmapped %>% filter(!grepl("FI.*",description))
rm(ccFI, missingFI)


# special treatment for Germany
ccDE <- catchcrop %>% filter(grepl("DE.*",region))
missingDE <- notmapped %>% filter(grepl("DE.*", description)) 


# NUTS changes
# DE4 --> DE40
capri_regions <- capri_regions %>% mutate(region = gsub("DE4","DE40",description))
# DED has been split. use whatever sub-code, because cathcrop data is identical
capri_regions <- capri_regions %>% mutate(region = gsub("DED","DED2",description))
# same for DEE
capri_regions <- capri_regions %>% mutate(region = gsub("DEE","DEE0",description))
# DE4 --> DE40
capri_regions <- capri_regions %>% mutate(region = gsub("DE4","DE40",description))


# Germany solved, remove them from the list
notmapped <- notmapped %>% filter(!grepl("DE.*",description))
rm(ccDE, missingDE)


# let's map again the corrected code lists
cc2CAPRI  <- capri_regions %>% left_join(catchcrop, by = c("description" = "region"))
notmapped <- cc2CAPRI %>% filter(is.na(value))
cc2CAPRI  <- cc2CAPRI %>% filter(!is.na(value))


# no idea what to do with the remaining two IT31(typo?, no letter after IT) and ITC2(no catchrop area?)
# therefore we remove them from the mapping
notmapped <- notmapped %>% filter(!grepl("IT31",description))
notmapped <- notmapped %>% filter(!grepl("ITC2",description))

# remove Berlin capital
notmapped <- notmapped %>% filter(!grepl("DE30",description))
# remove Bremen, no data for catch crops
notmapped <- notmapped %>% filter(!grepl("DE50",description))


# exclude non-European territories
notmapped <- notmapped %>% filter(!grepl("PT.*",description))
notmapped <- notmapped %>% filter(!grepl("ES.*",description))

#exclude Turkey
notmapped <- notmapped %>% filter(!grepl("TR.*",description))

# exclude Brussels capital - no significant agric. production
notmapped <- notmapped %>% filter(!grepl("BE10",description))

# esclude UK, special treatment below
notmapped <- notmapped %>% filter(!grepl("UK.*",description))

# remove FI1A, no data
notmapped <- notmapped %>% filter(!grepl("FI1A",description))



# special treatment for UK
# reason: UK is at NUTS2 in SAPM, but at NUTS1 in CAPRI
#
ccUK <- catchcrop %>% filter(grepl("UK.*",region))
# create NUTS1 codes
ccUK <- ccUK %>% mutate(nuts1 = strtrim(region, 3))
# get CAPRI geographical classification for the UK
capriUK <- capri_regions %>% filter(grepl("UK.*", description))
capriUK <- capriUK %>% select(-c(region))

# regional mapping for UK
UK <- capriUK %>% left_join(ccUK, by = c("description" = "nuts1"))


# exclude London capital - no significant agric. production
UK <- UK %>% filter(!grepl("UKI",description))


# merging UK to the rest to get the full mapping
cc2CAPRI <- rbind(cc2CAPRI, UK)



# TEST
# ----
# there still could be NA's in cc2CAPRI simply because of missing data
# but in this case the following filtering should be empty
catchcrop %>% filter(region %in% notmapped$description)

# AVERAGING
# --------
# average catch crop area shares per NUTS1
catchcrop_CAPRI <- cc2CAPRI %>% group_by(description, name, CAPRI_NUTS_ID, NUTS_YEAR, cropcategory) %>% summarize(value = mean(value))


# REPORTING 
# ---------
# map SAPM data to CAPRI
write.xlsx(cc2CAPRI, file = "xlsx/cc2CAPRI.xlsx")
write.xlsx(catchcrop_CAPRI, file = "xlsx/catchcrop_CAPRI.xlsx")
