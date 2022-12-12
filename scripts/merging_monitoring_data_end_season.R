# Merge season's monitoring data by site
# Gaby Samaniego gaby@savehummingbirds.org
# 2022-10-24

library(tidyverse)
library(data.table)
library(lubridate)
library(pointblank)

#### Read all vetted monitoring files for each site ####

# Create a list of all files per site
# CHANGE SITE CODE IN LINE 14 ACORDING TO MONIOTIRNG SITE YOU WANT TO VET ###
monitoring.files <- list.files(path = "output/HSR/", 
                               pattern = "*.csv", 
                               full.names = TRUE)
monitoring.dat <- lapply(monitoring.files, fread, sep = ",")  

# To merge all sites together we need to remove the Date column because it is 
# causing an error when trying to merge all files together due to the different 
# format in each file

# Remove date columns before combining 
for(i in 1:length(monitoring.dat)){
  monitoring.dat[[i]]<- monitoring.dat[[i]] %>% 
    select(-c(Date))
}

# Combine all csv files in a data frame
monitoring.dat <- rbindlist(monitoring.dat)

# Create date column using year, month and day columns
monitoring.dat <- monitoring.dat %>% 
  mutate('Date' = make_date(year = Year,
                            month = Month,
                            day = Day))

# Arrange order of columns in data frame  
monitoring.dat <- monitoring.dat %>% 
  relocate(Protocol, CMR, Bander, Location, Date)


#### Data validation with pointblank package one more time ####

# Set thresholds for report 
al <- action_levels(warn_at = 1, stop_at = 1) 

# Create pattern (regex) to validate if Band.Number is a letter and five numbers
pattern <- "[0-9]{9}"

# Data validation. Most columns in the data frame will be validated in the agent    
validation <- 
  create_agent(
    tbl = monitoring.dat,
    tbl_name = "Vetted_data",
    label = "Data Validation",
    actions = al) %>% 
  col_is_date(vars(Date)) %>% 
  col_vals_in_set(vars(Bander), set = c("GS","ML","SMW","BJ","KO","TT","LY",
                                        "ER","ALS","LM","EF")) %>%    
  col_vals_in_set(vars(Location), set = c("ML","HC","SWRS","PA","FH","DGS","MV",
                                          "MPGF","HSR","CFCK","ESC","WCAT", "REDC",
                                          "RT")) %>%  
  col_vals_in_set(vars(Species), set = c("ANHU","ALHU","BBLH","BCHU","BADE",
                                         "BEHU","BTMG","BTLH","BUFH","BALO",
                                         "CAHU","COHU","LUHU","RIHU","HYHU",
                                         "RTHU","RUHU","VCHU","WEHU","UNHU")) %>%
  col_vals_in_set(vars(Sex), set = c("M","F","U",NA)) %>% 
  col_vals_in_set(vars(Age), set = c("0","1","2","5",NA)) %>% 
  col_vals_in_set(vars(Band.Status), set = c("1","R","F","4","5","6","8",NA)) %>%  
  col_vals_in_set(vars(Tarsus.Measurement), set = c("B","C","D","E","F","G","H",
                                                    "I","J","K","L","M","N","O",NA)) %>% 
  col_vals_in_set(vars(Band.Size), set = c("B","C","D","E","F","G","H","I","J",
                                           "K","L","M","N","O",NA)) %>% 
  col_vals_regex(vars(Band.Number), regex = pattern, na_pass = TRUE) %>% 
  col_vals_in_set(vars(Leg.Condition), set = c("1","2","3","4","5","6","7",NA)) %>%
  col_vals_regex(vars(Replaced.Band.Number), regex = pattern, na_pass = TRUE) %>%
  col_vals_in_set(vars(Gorget.Color), set = c("O","R","V","P","B","G","GP","NS",
                                              "LS","MS","HS",NA)) %>% 
  col_vals_between(vars(Gorget.Count),0, 99, na_pass = TRUE) %>% 
  col_vals_between(vars(Head.Count),0, 99, na_pass = TRUE) %>% 
  col_vals_in_set(vars(Grooves), set = c("0","1","2","3",NA)) %>% 
  col_vals_in_set(vars(Buffy), set = c("Y","N","S",NA)) %>%
  col_vals_in_set(vars(Bill.Trait), set = c("R", "D",NA)) %>% 
  col_vals_between(vars(Green.on.back),0, 99, na_pass = TRUE) %>%
  col_vals_between(vars(Wing.Chord),35.0, 79.0, na_pass = TRUE) %>%
  col_vals_between(vars(Culmen),12.0,34.0, na_pass = TRUE) %>%
  col_vals_in_set(vars(Fat), set = c("0","1","2","3","P","T",NA)) %>%
  col_vals_in_set(vars(CP.Breed), set = c("9","8","7","5","2",NA)) %>%
  col_vals_in_set(vars(Head.Gorget.Molt), set = c("1","2","3","F","L","M","R",NA)) %>%
  col_vals_in_set(vars(Body.Molt), set = c("1","2","3","F","L","M","R",NA)) %>%
  col_vals_in_set(vars(Primaries.Molt), set = c("1","2","3","4","5","6","7","8","9",
                                                "0","F","L","M","R",NA)) %>%
  col_vals_in_set(vars(Secondaries.Molt), set = c("1","2","3","4","5","6","F","L","M","R",NA)) %>%
  col_vals_in_set(vars(Tail.Molt), set = c("1","2","3","4","5","F","L","M","R",NA)) %>% 
  col_vals_between(vars(Weight), 2, 9, na_pass = TRUE)

# Interrogate creates a report after validation  
interrogate(validation)

# If report shows columns that didn't pass the validation (fail), get_sundered
# shows failed values by rows
interrogate(validation) %>% 
  get_sundered_data(type = "fail")

#### Compare if recaptured birds coincide with band number, species, and sex to 
#### their first capture  #### 

# Bring in all band numbers used by HMN's monitoring program 
all_bands <- read.csv("data/updated_raw_data.csv",
                      na.strings = c("",NA))

# Change band number from character to numeric before sorting the data
all_bands$Band.Number <- as.numeric(as.character((all_bands$Band.Number)))
class(all_bands$Band.Number)

# Sort data by band number, species, age and sex
all_bands <- all_bands %>% 
  arrange(Band.Number, Species, Age, Sex)

# Extract recaptures from all session's data
all_recaps <- monitoring.dat %>% 
  filter(Band.Status == "R") %>% 
  select(Band.Number, Species, Sex, Age, Location, Year)

# Check for inconsistencies with Band Numbers, species, age and sex for all
# session's recaptures
for (BN in all_recaps$Band.Number) {
  original_caps <- all_bands %>% 
    filter(Band.Number == BN) %>% 
    select(Species, Sex, Age, Location) 
  if(nrow(original_caps) == 0){
    print(paste0("Band Number ", BN, " not in main database")) # Band number provably 
    next                    # applied in a session of current monitoring year
  }
  if(!(original_caps$Species[1] %in% all_recaps$Species[all_recaps$Band.Number == BN & 
                                                        !is.na(all_recaps$Band.Number)])){ # !is.na code avoids the error when BN is NA
    print(paste0("Species code inconsistent for ", BN))
    message("Original species for ", BN , " was ",  original_caps$Species)
  }
  if(!(original_caps$Sex[1] %in% all_recaps$Sex[all_recaps$Band.Number == BN & 
                                                !is.na(all_recaps$Band.Number)])){
    print(paste0("Sex code inconsistent for ", BN))
    message("Original sex for ", BN , " was ",  original_caps$Sex)
  }
  if(!(original_caps$Age[1] %in% all_recaps$Age[all_recaps$Band.Number == BN & 
                                                !is.na(all_recaps$Band.Number)])){
    print(paste0("Age code inconsistent for ", BN))
    message("Original Age for ", BN , " was ", original_caps$Age )
  }
  if(!(original_caps$Location[1] %in% all_recaps$Location[all_recaps$Band.Number == BN & 
                                                          !is.na(all_recaps$Band.Number)])){
    print(paste0("Location code inconsistent for ", BN))
    message("Original location for ", BN , " was ",  original_caps$Location)
  }
}

# Check for inconsistencies with Band Numbers for the recaptures of current sessions 
for (BN in all_recaps$Band.Number) {
  original_caps <- monitoring.dat %>% 
    filter(Band.Number == BN)
  if(nrow(original_caps) == 0){
    print(paste0("Band Number ", BN, " not found in current year")) # Wrong band number 
    next                    
  }
  if(!(original_caps$Species[1] %in% all_recaps$Species[all_recaps$Band.Number == BN & 
                                                        !is.na(all_recaps$Band.Number)])){ # !is.na code avoids the error when BN is NA
    print(paste0("Species code inconsistent for ", BN))
    message("Original species for ", BN , " was ",  original_caps$Species)
  }
  if(!(original_caps$Sex[1] %in% all_recaps$Sex[all_recaps$Band.Number == BN & 
                                                !is.na(all_recaps$Band.Number)])){
    print(paste0("Sex code inconsistent for ", BN))
    message("Original sex for ", BN , " was ",  original_caps$Sex)
  }
  if(!(original_caps$Age[1] %in% all_recaps$Age[all_recaps$Band.Number == BN & 
                                                !is.na(all_recaps$Band.Number)])){
    print(paste0("Age code inconsistent for ", BN))
    message("Original Age for ", BN , " was ", original_caps$Age )
  }
  if(!(original_caps$Location[1] %in% all_recaps$Location[all_recaps$Band.Number == BN & 
                                                          !is.na(all_recaps$Band.Number)])){
    print(paste0("Location code inconsistent for ", BN))
    message("Original location for ", BN , " was ",  original_caps$Location)
  }
}


### Sort site's data ###
monitoring.dat <- monitoring.dat %>% 
  arrange(Band.Number, Species, Age, Sex)


#### Create new csv with the validated 2022 data for the vetted site ####

# Create csv with vetted data. Make sure to update the location code and year in 
# the output name  
write.csv(monitoring.dat,"output/Susan_2022.csv", row.names = FALSE)

  