# AZ game and Fish Department Annual Report 
# Gaby Samaniego gaby@savehummingbirds.org
# November 2022

library(tidyverse)
library(lubridate)

# Bring in all vetted data for current year
band.dat.22 <- read.csv("data/Susan_2022.csv", 
                         na.strings = c("",NA))

band.dat <- band.dat.22

# -------------------------------- Data Wrangling --------------------------- #

# Change Date column from character to date
band.dat <- band.dat %>% 
  mutate(Date = mdy(Date))

# Replace age's code to full word
band.dat$Age[band.dat$Age == 2] <- 'Juvenile'
band.dat$Age[band.dat$Age == 1] <- 'Adult'
band.dat$Age[band.dat$Age == 5] <- 'Adult'
band.dat$Age[band.dat$Age == 0] <- 'Unknown'

# Replace code species with scientific name and common name 
# Read species list 
species <- read.csv("data/species_list_code_scientific_common.csv")

# Replace Species code by scientific name
band.dat$Species <- species$Scientific[match(band.dat$Species, species$Code)]

# Create new column with common name
band.dat$Common <- species$Common[match(band.dat$Species, species$Scientific)]

# Filter AZ sites, remove band lost and band destroyed (BALO & BADE) 
band.dat <- band.dat %>% 
  filter(Location %in% c('HC', 'FH', 'ML', 'PA', 'SWRS'),
         Species != "BALO" & Species != "BADE")

# Count number of observations per site 
band.dat %>% count(Location)
# FH = 670
# HC = 999
# ML = 304
# PA = 513
# SWRS = 278
# TOTAL = 2764 which matches band.dat observations in environment 

#--------------------------- HARSHAW CREEEK DATA -----------------------------#
         
# Summarize data for hc 
HC <- band.dat %>% 
  filter(Location == "HC") %>% 
  group_by(Date, Species, Common, Sex, Age) %>% 
  summarize(N.Males = length(unique(Band.Number[Sex == 'M'])),
            N.Females = length(unique(Band.Number[Sex == 'F'])),
            Unknown.Sex = length(unique(Band.Number[Sex == 'U'])),
            N.Adults = length(unique(Band.Number[Age == 'Adult'])),
            N.Juveniles = length(unique(Band.Number[Age == 'Juvenile'])),
            Unknown.Age = length(unique(Band.Number[Age == 'Unknown'])),
            n = (N.Males + N.Females + Unknown.Sex + N.Adults + N.Juveniles + 
                   Unknown.Age)/2) %>% 
  select(-c(N.Males, N.Females, Unknown.Sex, N.Adults, N.Juveniles, Unknown.Age))

# Eliminate decimals
HC$n[HC$n == 0.5] <- 1

# Check that the sum of n matches line 46
sum(HC[["n"]])

# Add AZGFD columns to data 
HC <- HC %>% 
  mutate(Observer = 'Susan Wethington',
         County = 'Santa Cruz',
         X = 530207,
         Y = 3490442,
         Zone = '12s',
         Datum = 'NAD83',
         Coordinate = 'UTM',
         Location = '',
         Disposition = 'Released Alive', 
         Museum = '',
         Marked = '',
         Field = '',
         Habitat = 'Riparian/ Mesquite/ Oak', 
         Other = 'Private Residence, Harshaw Creek, Patagonia Mtns.') %>%
  relocate(Observer, Species, Common, n, Date, County, X, Y, Zone, Datum, Coordinate,
           Location, Age, Sex, Disposition, Museum, Marked, Field, Habitat, Other) 

#--------------------------- FORT HUACHUCA DATA -----------------------------#

# Summarize data for fh 
FH <- band.dat %>% 
  filter(Location == "FH") %>% 
  group_by(Date, Species, Common, Sex, Age) %>% 
  summarize(N.Males = length(unique(Band.Number[Sex == 'M'])),
            N.Females = length(unique(Band.Number[Sex == 'F'])),
            Unknown.Sex = length(unique(Band.Number[Sex == 'U'])),
            N.Adults = length(unique(Band.Number[Age == 'Adult'])),
            N.Juveniles = length(unique(Band.Number[Age == 'Juvenile'])),
            Unknown.Age = length(unique(Band.Number[Age == 'Unknown'])),
            n = (N.Males + N.Females + Unknown.Sex + N.Adults + N.Juveniles + 
                   Unknown.Age)/2) %>% 
  select(-c(N.Males, N.Females, Unknown.Sex, N.Adults, N.Juveniles, Unknown.Age))

# Eliminate decimals
FH$n[FH$n == 0.5] <- 1

# Check that the sum of n matches line 46
sum(FH[["n"]])

# Add AZGFD columns to data 
FH <- FH %>% 
  mutate(Observer = 'Birget Johnson',
         County = 'Cochise',
         X = 559932,
         Y = 3490442,
         Zone = '12s',
         Datum = 'NAD83',
         Coordinate = 'UTM',
         Location = '',
         Disposition = 'Released Alive', 
         Museum = '',
         Marked = '',
         Field = '',
         Habitat = 'Riparian/ Oak', 
         Other = 'Public Affairs Office, Fort Huachuca, Huachuca Mtns.') %>%
  relocate(Observer, Species, Common, n, Date, County, X, Y, Zone, Datum, Coordinate,
           Location, Age, Sex, Disposition, Museum, Marked, Field, Habitat, Other) 

#--------------------------- MOUNT LEMMON DATA -----------------------------#

# Summarize data for ml 
ML <- band.dat %>% 
  filter(Location == "ML") %>% 
  group_by(Date, Species, Common, Sex, Age) %>% 
  summarize(N.Males = length(unique(Band.Number[Sex == 'M'])),
            N.Females = length(unique(Band.Number[Sex == 'F'])),
            Unknown.Sex = length(unique(Band.Number[Sex == 'U'])),
            N.Adults = length(unique(Band.Number[Age == 'Adult'])),
            N.Juveniles = length(unique(Band.Number[Age == 'Juvenile'])),
            Unknown.Age = length(unique(Band.Number[Age == 'Unknown'])),
            n = (N.Males + N.Females + Unknown.Sex + N.Adults + N.Juveniles + 
                   Unknown.Age)/2) %>% 
  select(-c(N.Males, N.Females, Unknown.Sex, N.Adults, N.Juveniles, Unknown.Age))

# Eliminate decimals
ML$n[ML$n == 0.5] <- 1

# Check that the sum of n matches line 46
sum(ML[["n"]])

# Add AZGFD columns to data 
ML <- ML %>% 
  mutate(Observer = 'Gabriela Samaniego',
         County = 'Pima',
         X = 519873,
         Y = 3589326,
         Zone = '12s',
         Datum = 'NAD83',
         Coordinate = 'UTM',
         Location = '',
         Disposition = 'Released Alive', 
         Museum = '',
         Marked = '',
         Field = '',
         Habitat = 'Pine/ Fir', 
         Other = 'Steward Observatory, Mt. Lemmon, Catalina Mtns.') %>%
  relocate(Observer, Species, Common, n, Date, County, X, Y, Zone, Datum, Coordinate,
           Location, Age, Sex, Disposition, Museum, Marked, Field, Habitat, Other) 

#------------------------------- PARADISE DATA -------------------------------#

# Summarize data for ml 
PA <- band.dat %>% 
  filter(Location == "PA") %>% 
  group_by(Date, Species, Common, Sex, Age) %>% 
  summarize(N.Males = length(unique(Band.Number[Sex == 'M'])),
            N.Females = length(unique(Band.Number[Sex == 'F'])),
            Unknown.Sex = length(unique(Band.Number[Sex == 'U'])),
            N.Adults = length(unique(Band.Number[Age == 'Adult'])),
            N.Juveniles = length(unique(Band.Number[Age == 'Juvenile'])),
            Unknown.Age = length(unique(Band.Number[Age == 'Unknown'])),
            n = (N.Males + N.Females + Unknown.Sex + N.Adults + N.Juveniles + 
                   Unknown.Age)/2) %>% 
  select(-c(N.Males, N.Females, Unknown.Sex, N.Adults, N.Juveniles, Unknown.Age))

# Eliminate decimals
PA$n[PA$n == 0.5] <- 1

# Check that the sum of n matches line 46
sum(PA[["n"]])

# Add AZGFD columns to data 
PA <- PA %>% 
  mutate(Observer = 'Susan Wethington',
         County = 'Cochise',
         X = 668353,
         Y = 3534640,
         Zone = '12s',
         Datum = 'NAD83',
         Coordinate = 'UTM',
         Location = '',
         Disposition = 'Released Alive', 
         Museum = '',
         Marked = '',
         Field = '',
         Habitat = 'Riparian/ Pine/ Oak', 
         Other = 'George Walker House, Paradise, Chiricahua Mtns.') %>%
  relocate(Observer, Species, Common, n, Date, County, X, Y, Zone, Datum, Coordinate,
           Location, Age, Sex, Disposition, Museum, Marked, Field, Habitat, Other) 

#-------------------- SOUTHWESTERN RESEARCH STATION DATA ---------------------#

# Summarize data for ml 
SWRS <- band.dat %>% 
  filter(Location == "SWRS") %>% 
  group_by(Date, Species, Common, Sex, Age) %>% 
  summarize(N.Males = length(unique(Band.Number[Sex == 'M'])),
            N.Females = length(unique(Band.Number[Sex == 'F'])),
            Unknown.Sex = length(unique(Band.Number[Sex == 'U'])),
            N.Adults = length(unique(Band.Number[Age == 'Adult'])),
            N.Juveniles = length(unique(Band.Number[Age == 'Juvenile'])),
            Unknown.Age = length(unique(Band.Number[Age == 'Unknown'])),
            n = (N.Males + N.Females + Unknown.Sex + N.Adults + N.Juveniles + 
                   Unknown.Age)/2) %>% 
  select(-c(N.Males, N.Females, Unknown.Sex, N.Adults, N.Juveniles, Unknown.Age))

# Eliminate decimals
SWRS$n[SWRS$n == 0.5] <- 1

# Check that the sum of n matches line 46
sum(SWRS[["n"]])

# Add AZGFD columns to data 
SWRS <- SWRS %>% 
  mutate(Observer = 'Michele Lanan',
         County = 'Cochise',
         X = 669748,
         Y = 3528896,
         Zone = '12s',
         Datum = 'NAD83',
         Coordinate = 'UTM',
         Location = '',
         Disposition = 'Released Alive', 
         Museum = '',
         Marked = '',
         Field = '',
         Habitat = 'Riparian/ Oak/ Pine', 
         Other = 'Southwestern Research Station, Chiricahua Mtns.') %>%
  relocate(Observer, Species, Common, n, Date, County, X, Y, Zone, Datum, Coordinate,
           Location, Age, Sex, Disposition, Museum, Marked, Field, Habitat, Other)

#------------------------------ MERGE ALL SITES ------------------------------#

# Merge all sites in one data frame
report <- do.call("rbind", list(FH, HC, ML, PA, SWRS))

# Check that the sum of n matches line 48
sum(report[["n"]])

# Create csv with the data for report
write.csv(report,"output/AZGFD_REPORT_2022.csv", row.names = FALSE)

