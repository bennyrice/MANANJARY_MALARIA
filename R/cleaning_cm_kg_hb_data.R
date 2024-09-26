library(tidyverse)
library(patchwork)
library(googlesheets4)


##############################################################################################################################
# Reading in data
##############################################################################################################################

#Read in data from google sheets

#URLs: Primary data entry
url.N1N5  <- "https://docs.google.com/spreadsheets/d/1XXlI372z-FzEPfOYfUwME7_1gWjguNd-aMePqv_zkOM/edit?usp=drive_link"
url.S1S2  <- "https://docs.google.com/spreadsheets/d/19pzwUSzTRoz0p9CAr9dke9cldtO7dtAmO0TuyJJHi1Q/edit?usp=drive_link"
url.S3    <- "https://docs.google.com/spreadsheets/d/1XYe4griKTW36LWzT8x-djgbhsQ06eeP_lVmRt3ZTaWg/edit?usp=drive_link"
url.S5    <- "https://docs.google.com/spreadsheets/d/1L7WdgHRFLtc-zsPfVOLaqN2XTDnGpDZLK-jD-9lUHQo/edit?usp=drive_link"
url.S6    <- "https://docs.google.com/spreadsheets/d/1ojJLlADexiSp-CPujyHb38xSykFcnXZ3nmqGg2cDb-I/edit?usp=drive_link"
#Read in
df.N1N5i  <- read_sheet(url.N1N5, col_types = "c")
df.S1S2i  <- read_sheet(url.S1S2, col_types = "c")
df.S3i    <- read_sheet(url.S3, col_types = "c")
df.S5i    <- read_sheet(url.S5, col_types = "c")
df.S6i    <- read_sheet(url.S6, col_types = "c")


#Cleaning up columns
cols_to_keep <- c("unique_ind_id",
                  "T01_MO","T01_DATY","T01_KG","T01_CM","T01_PB","T01_PC","T01_TEMP","T01_RDT_TIME","T01_RDT","T01_Hb",
                  "T02_MO","T02_DATY","T02_KG","T02_CM","T02_PB","T02_PC","T02_TEMP","T02_RDT_TIME","T02_RDT","T02_Hb",
                  "T03_MO","T03_DATY","T03_KG","T03_CM","T03_PB","T03_PC","T03_TEMP","T03_RDT_TIME","T03_RDT","T03_Hb",
                  "T04_MO","T04_DATY","T04_KG","T04_CM","T04_PB","T04_PC","T04_TEMP","T04_RDT_TIME","T04_RDT","T04_Hb",
                  "T05_MO","T05_DATY","T05_KG","T05_CM","T05_PB","T05_PC","T05_TEMP","T05_RDT_TIME","T05_RDT","T05_Hb",
                  "T06_MO","T06_DATY","T06_KG","T06_CM","T06_PB","T06_PC","T06_TEMP","T06_RDT_TIME","T06_RDT","T06_Hb",
                  "T07_MO","T07_DATY","T07_KG","T07_CM","T07_PB","T07_PC","T07_TEMP","T07_RDT_TIME","T07_RDT","T07_Hb",
                  "T08_MO","T08_DATY","T08_KG","T08_CM","T08_PB","T08_PC","T08_TEMP","T08_RDT_TIME","T08_RDT","T08_Hb",
                  "T09_MO","T09_DATY","T09_KG","T09_CM","T09_PB","T09_PC","T09_TEMP","T09_RDT_TIME","T09_RDT","T09_Hb",
                  "T10_MO","T10_DATY","T10_KG","T10_CM","T10_PB","T10_PC","T10_TEMP","T10_RDT_TIME","T10_RDT","T10_Hb",
                  "T11_MO","T11_DATY","T11_KG","T11_CM","T11_PB","T11_PC","T11_TEMP","T11_RDT_TIME","T11_RDT","T11_Hb")

col_renames_lookup <- c(T01_RDT.TIME = "T01_RDT_TIME",
                        T02_RDT.TIME = "T02_RDT_TIME", 
                        T03_RDT.TIME = "T03_RDT_TIME", 
                        T04_RDT.TIME = "T04_RDT_TIME", 
                        T05_RDT.TIME = "T05_RDT_TIME", 
                        T06_RDT.TIME = "T06_RDT_TIME", 
                        T07_RDT.TIME = "T07_RDT_TIME", 
                        T08_RDT.TIME = "T08_RDT_TIME", 
                        T09_RDT.TIME = "T09_RDT_TIME", 
                        T10_RDT.TIME = "T10_RDT_TIME", 
                        T11_RDT.TIME = "T11_RDT_TIME")

df.N1N5 <- df.N1N5i %>%
  dplyr::select(any_of(cols_to_keep)) %>%
  rename(any_of(col_renames_lookup)) %>%
  pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
df.S1S2 <- df.S1S2i %>%
  dplyr::select(any_of(cols_to_keep)) %>%
  rename(any_of(col_renames_lookup)) %>%
  pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
df.S3 <- df.S3i %>%
  dplyr::select(any_of(cols_to_keep)) %>%
  rename(any_of(col_renames_lookup)) %>%
  pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
df.S5 <- df.S5i %>%
  dplyr::select(any_of(cols_to_keep)) %>%
  rename(any_of(col_renames_lookup)) %>%
  pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
df.S6 <- df.S6i %>%
  dplyr::select(any_of(cols_to_keep)) %>%
  rename(any_of(col_renames_lookup)) %>%
  pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))

#Binding together and cleaning up columns
df.primary <- rbind(df.N1N5, df.S1S2, df.S3, df.S5, df.S6) %>%
  mutate(variable = case_when(
    variable == "MO" ~ "month",
    variable == "DATY" ~ "day",
    variable == "RDT.TIME" ~ "RDT_TIME",
    variable == "RDT" ~ "rdt_result",
    .default = as.character(variable))) %>% 
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(site_code = substr(unique_ind_id, 1, 2)) %>%
  dplyr::select(site_code, unique_ind_id:Hb) %>%
  arrange(site_code, time_point, unique_ind_id) %>%
  mutate(across(month:Hb, ~ ifelse(. == "NA", NA, .))) %>%
  mutate(across(c(KG:TEMP, Hb), as.numeric)) %>%
  mutate(across(c(day, RDT_TIME), as.integer))

##############################################################################################################################
# DATA CLEANING 1: Checking for errors in data entry
##############################################################################################################################

#Read in quality control check data
#Data is piece-wise by site and time point in File 1; piecewise by site in File 2
#Have to read in each sheet 1 at a time

#URLs: Quality Control Check
#File 1: December 2023 Work
# url.qc.f1 <- "https://docs.google.com/spreadsheets/d/19bx8X9vjkCWTaTgOdBrMt0UIMbDoGZH9OrmTsqppm1I/edit?gid=0#gid=0"
#File 2: Spring 2024 Work
# url.qc.f2 <- "https://docs.google.com/spreadsheets/d/17Jyb6QF47_m5YdBO_JHlJG2lVVwuHMWXQRaigDv0zOo/edit?gid=0#gid=0"
#Read in all sheets/tabs within file using a function
#Sheet lists:
# v.f1 <- c("N1 T07", "N1 T10", "N2 T01", "N2 T07", "N2 T11", "N3 T03", "N3 T10", "N4 T06", "N4 T08", "N5 T10", "S1 T01", "S1 T02", "S1 T04", "S1 T07", "S1 T09", "S2 T04", "S2 T05", "S2 T08", "S3 T01", "S3 T04", "S3 T09", "S5 T05", "S5 T06", "S5 T10", "S6 T03", "S6 T06", "S6 T08")
# v.f2 <- c("N1", "N2", "N3")


f.qc.file_reader1 <- function(url, v.sheet.list){
  #Initialize an empty list to hold dfs
  l.out <- list(NA)
  #Loop through each sheet
  for(i in 1:length(v.sheet.list)){
    dfi <- read_sheet(url, sheet = v.sheet.list[i], col_types = "c") %>%
      #dropping dob_mo to making grabbing columns easier
      dplyr::select(-dob_mo) %>%
      #re-arranging columns and keeping columns using select(contains())
      #columns wanted: month, day, KG, CM, PB, PC, TEMP, RDT_TIME, RDT, Hb
      dplyr::select(contains("unique_ind_id"), 
                    contains("MO"), contains("DATY"), 
                    contains("KG"), contains("CM"), 
                    contains("PB"), contains("PC"), 
                    contains("TEMP"), contains("RDT_TIME"), contains("RDT"),
                    contains("Hb"))
    #Adding a time point column (and stripping the prefix from column names)
    dfi <- dfi %>% mutate(time_point = substr(names(dfi)[length(names(dfi))], 1, 3))
    names(dfi) <- c("unique_ind_id", 
                    "month", "day", 
                    "KG", "CM", 
                    "PB", "PC", 
                    "TEMP", "RDT_TIME", "rdt_result", 
                    "Hb", 
                    "time_point") 
    dfi <- dfi %>% dplyr::select("unique_ind_id", "time_point", "month", "day", "KG", "CM", "PB", "PC", "TEMP", "RDT_TIME", "rdt_result", "Hb")
     l.out[[i]] <- dfi
  }
  df.out <- bind_rows(l.out)
  return(df.out)
}

# df.f1 <- f.qc.file_reader1(url = url.qc.f1, v.sheet.list = v.f1)


#File 2: Spring 2024 Work

#Reading in
# df.f2.N1i <- read_sheet(url.qc.f2, sheet = "N1", col_types = "c")
# df.f2.N2i <- read_sheet(url.qc.f2, sheet = "N2", col_types = "c")
# df.f2.N3i <- read_sheet(url.qc.f2, sheet = "N3", col_types = "c")

#Cleaning up columns
# df.f2.N1 <- df.f2.N1i %>%
#   dplyr::select(any_of(cols_to_keep)) %>%
#   rename(any_of(col_renames_lookup)) %>%
#   pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
# df.f2.N2 <- df.f2.N2i %>%
#   dplyr::select(any_of(cols_to_keep)) %>%
#   rename(any_of(col_renames_lookup)) %>%
#   pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
# df.f2.N3 <- df.f2.N3i %>%
#   dplyr::select(any_of(cols_to_keep)) %>%
#   rename(any_of(col_renames_lookup)) %>%
#   pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
# #Binding together and cleaning up columns
# df.f2 <- rbind(df.f2.N1, df.f2.N2, df.f2.N3) %>%
#   mutate(variable = case_when(
#     variable == "MO" ~ "month",
#     variable == "DATY" ~ "day",
#     variable == "RDT.TIME" ~ "RDT_TIME",
#     variable == "RDT" ~ "rdt_result",
#     .default = as.character(variable))) %>% 
#   pivot_wider(names_from = variable, values_from = value)
# 
# #Combining both sets of work and cleaning up
# df.qc <- rbind(df.f1, df.f2) %>%
#   mutate(site_code = substr(unique_ind_id, 1, 2)) %>%
#   dplyr::select(site_code, unique_ind_id:Hb) %>%
#   arrange(site_code, time_point, unique_ind_id) %>%
#   mutate(across(month:Hb, ~ ifelse(. == "NA", NA, .))) %>%
#   mutate(across(c(KG:TEMP, Hb), as.numeric)) %>%
#   mutate(across(c(day, RDT_TIME), as.integer))

#
#write_csv(df.qc, "/Users/blrice/Library/CloudStorage/Dropbox/Lab Projects/2020 Projects/CRS2020/1 DATA/DATA_ENTRY_WORK/20240916 Data Entry Resolutions/df.qc_20240924.csv")

########################################################################################################################
#Join primary data and quality control check data
url.qc.read <- "https://docs.google.com/spreadsheets/d/18luemce_L9fVTUmWPcZGsfyAbpLXKm_xl5rTpQie2oQ/edit?gid=1889196570#gid=1889196570"

df.qc.read <- read_sheet(url.qc.read)

df.qc.read <- df.qc.read %>%
  mutate(across(month:Hb, ~ ifelse(. == "NA", NA, .))) %>%
  mutate(across(c(KG:TEMP, Hb), as.numeric)) %>%
  mutate(across(c(day, RDT_TIME), as.integer))

df.joined <- full_join(df.primary, df.qc.read, by = join_by(unique_ind_id, time_point))

#Compare for differences between primary data entry and quality control check data
df.comp.month      <- df.joined %>% filter(month.x != month.y)
df.comp.day        <- df.joined %>% filter(day.x != day.y)
df.comp.KG         <- df.joined %>% filter(KG.x != KG.y)
df.comp.CM         <- df.joined %>% filter(CM.x != CM.y)
df.comp.PB         <- df.joined %>% filter(PB.x != PB.y)
df.comp.PC         <- df.joined %>% filter(PC.x != PC.y)
df.comp.TEMP       <- df.joined %>% filter(TEMP.x != TEMP.y)
df.comp.RDT_TIME   <- df.joined %>% filter(RDT_TIME.x != RDT_TIME.y)
df.comp.rdt_result <- df.joined %>% filter(rdt_result.x != rdt_result.y)
df.comp.Hb         <- df.joined %>% filter(Hb.x != Hb.y)

#Discrepancies = 1 --> 0 [Resolved on 20240925]
df.comp.month <- df.comp.month %>% dplyr::select(site_code.x:time_point, month.x, day.x, month.y, day.y)
#Discrepancies = 66 --> 56 --> 43 --> 0 [Resolved on 20240925]
df.comp.day <- df.comp.day %>% dplyr::select(site_code.x:time_point, month.x, day.x, month.y, day.y) %>% arrange(time_point, site_code.x, unique_ind_id)

#Discrepancies = 121
df.comp.KG <- df.comp.KG %>% dplyr::select(site_code.x:time_point, KG.x, KG.y) %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 95
df.comp.CM <- df.comp.CM %>% dplyr::select(site_code.x:time_point, CM.x, CM.y) %>% arrange(time_point, site_code.x, unique_ind_id)

#Discrepancies = 16 --> 0 [Resolved on 20240925]
df.comp.PB <- df.comp.PB %>% dplyr::select(site_code.x:time_point, PB.x, PB.y) %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 15 --> 0 [Resolved on 20240925]
df.comp.PC <- df.comp.PC %>% dplyr::select(site_code.x:time_point, PC.x, PC.y) %>% arrange(time_point, site_code.x, unique_ind_id)

#Discrepancies = 69
df.comp.TEMP <- df.comp.TEMP %>% dplyr::select(site_code.x:time_point, TEMP.x, TEMP.y) %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 128 --> 0 [Resolved on 20240925]
df.comp.RDT_TIME <- df.comp.RDT_TIME %>% dplyr::select(site_code.x:time_point, RDT_TIME.x, RDT_TIME.y) %>% arrange(time_point, site_code.x, unique_ind_id)


#Discrepancies = 36 --> 21 --> 12 --> 0 [All resolved on 20240925]
df.comp.rdt_result <- df.comp.rdt_result %>% dplyr::select(site_code.x:day.x, rdt_result.x, rdt_result.y)
#Discrepancies = 9 --> 0 [All resolved on 20240924]
df.comp.Hb <- df.comp.Hb %>% dplyr::select(site_code.x:time_point, Hb.x, Hb.y)




#Test for NA discrepancies : X is NA but Y has a value
#Discrepancies = 4
df.comp.NAx.month      <- df.joined %>% filter(is.na(month.x) & !is.na(month.y))           %>% dplyr::select(site_code.x:time_point, month.x, month.y)           %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 3
df.comp.NAx.day        <- df.joined %>% filter(is.na(day.x) & !is.na(day.y))               %>% dplyr::select(site_code.x:time_point, day.x, day.y)               %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 6
df.comp.NAx.KG         <- df.joined %>% filter(is.na(KG.x) & !is.na(KG.y))                 %>% dplyr::select(site_code.x:time_point, KG.x, KG.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 9
df.comp.NAx.CM         <- df.joined %>% filter(is.na(CM.x) & !is.na(CM.y))                 %>% dplyr::select(site_code.x:time_point, CM.x, CM.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 12
df.comp.NAx.PB         <- df.joined %>% filter(is.na(PB.x) & !is.na(PB.y))                 %>% dplyr::select(site_code.x:time_point, PB.x, PB.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 6
df.comp.NAx.PC         <- df.joined %>% filter(is.na(PC.x) & !is.na(PC.y))                 %>% dplyr::select(site_code.x:time_point, PC.x, PC.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 2
df.comp.NAx.TEMP       <- df.joined %>% filter(is.na(TEMP.x) & !is.na(TEMP.y))             %>% dplyr::select(site_code.x:time_point, TEMP.x, TEMP.y)             %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 4
df.comp.NAx.RDT_TIME   <- df.joined %>% filter(is.na(RDT_TIME.x) & !is.na(RDT_TIME.y))     %>% dplyr::select(site_code.x:time_point, RDT_TIME.x, RDT_TIME.y)     %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 3
df.comp.NAx.rdt_result <- df.joined %>% filter(is.na(rdt_result.x) & !is.na(rdt_result.y)) %>% dplyr::select(site_code.x:time_point, rdt_result.x, rdt_result.y) %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 3
df.comp.NAx.Hb         <- df.joined %>% filter(is.na(Hb.x) & !is.na(Hb.y))                 %>% dplyr::select(site_code.x:time_point, Hb.x, Hb.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)


#Test for NA discrepancies : X has a value but Y is NA
#Discrepancies = 0
df.comp.NAy.month      <- df.joined %>% filter(!is.na(month.x) & is.na(month.y) & !is.na(month.y))           %>% dplyr::select(site_code.x:time_point, month.x, month.y)           %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 0
df.comp.NAy.day        <- df.joined %>% filter(!is.na(day.x) & is.na(day.y) & !is.na(month.y))               %>% dplyr::select(site_code.x:time_point, day.x, day.y)               %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 0
df.comp.NAy.KG         <- df.joined %>% filter(!is.na(KG.x) & is.na(KG.y) & !is.na(month.y))                 %>% dplyr::select(site_code.x:time_point, KG.x, KG.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 8
df.comp.NAy.CM         <- df.joined %>% filter(!is.na(CM.x) & is.na(CM.y) & !is.na(month.y))                 %>% dplyr::select(site_code.x:time_point, CM.x, CM.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 14
df.comp.NAy.PB         <- df.joined %>% filter(!is.na(PB.x) & is.na(PB.y) & !is.na(month.y))                 %>% dplyr::select(site_code.x:time_point, PB.x, PB.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 9
df.comp.NAy.PC         <- df.joined %>% filter(!is.na(PC.x) & is.na(PC.y) & !is.na(month.y))                 %>% dplyr::select(site_code.x:time_point, PC.x, PC.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 0
df.comp.NAy.TEMP       <- df.joined %>% filter(!is.na(TEMP.x) & is.na(TEMP.y) & !is.na(month.y))             %>% dplyr::select(site_code.x:time_point, TEMP.x, TEMP.y)             %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 0
df.comp.NAy.RDT_TIME   <- df.joined %>% filter(!is.na(RDT_TIME.x) & is.na(RDT_TIME.y) & !is.na(month.y))     %>% dplyr::select(site_code.x:time_point, RDT_TIME.x, RDT_TIME.y)     %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 4
df.comp.NAy.rdt_result <- df.joined %>% filter(!is.na(rdt_result.x) & is.na(rdt_result.y) & !is.na(month.y)) %>% dplyr::select(site_code.x:time_point, rdt_result.x, rdt_result.y) %>% arrange(time_point, site_code.x, unique_ind_id)
#Discrepancies = 11
df.comp.NAy.Hb         <- df.joined %>% filter(!is.na(Hb.x) & is.na(Hb.y) & !is.na(month.y))                 %>% dplyr::select(site_code.x:time_point, Hb.x, Hb.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)




###***Workflow: go through and check against data scan, correcting the QC or primary data sheet on google sheets
###*Note that QC only done on N1, N2, N3 and 25% of time points for other sites - probably need to do QC on the rest of the sites
###*Finish during peer review?




##############################################################################################################################
# DATA CLEANING 2: Checking for data recording errors
# 2A: RDT data
##############################################################################################################################

#Error cases:
# RDT result is not valid (pan, pf, panpf, neg)
# RDT result with no sample date recorded or sample date but no result

#Checking for duplicate unique_ind_ids
dfS6i[duplicated(dfS6i$unique_ind_id), ]
dfS5i[duplicated(dfS5i$unique_ind_id), ]
dfS3i[duplicated(dfS3i$unique_ind_id), ]
dfS2S1i[duplicated(dfS2S1i$unique_ind_id), ]
dfN1N5i[duplicated(dfN1N5i$unique_ind_id), ]

##############################################################################################################################
# DATA CLEANING 2: Checking for data recording errors
# 2B: Anemia-hemoglobin data
##############################################################################################################################

#Error cases:
# Hemoglobin outside of plausible range for age-sex group
# Hb result with no sample date recorded or sample date but no result


##############################################################################################################################
# DATA CLEANING 2: Checking for data recording errors
# 2C: Height data
##############################################################################################################################

#Error cases:
# Height outside of plausible range for age-sex group
# Height result with no sample date recorded or sample date but no result


##############################################################################################################################
# DATA CLEANING 2: Checking for data recording errors
# 2D: Weight data
##############################################################################################################################

#Error cases:
# Weight outside of plausible range for age-sex group
# Weight result with no sample date recorded or sample date but no result


##############################################################################################################################
# DATA CLEANING 2: Checking for data recording errors
# 2E: Height for Weight data
##############################################################################################################################

#Error cases:
# HFW outside of plausible range for age-sex group
# Using z scores for relevant age groups


##############################################################################################################################
# DATA CLEANING 2: Checking for data recording errors
# 2F: Height and Weight for Age data
##############################################################################################################################

#Error cases:
# HFA/WFA outside of plausible range








####################################################################################################################################
#Playing with height and weight data

#Reading in data
dfS6i   <- readr::read_csv("/Users/blrice/Documents/R GIT REPOS/2022_MNJ_TAZO/data_rdt/raw_data/S6_DATA_ENTRY_EST_20230702.csv")
dfS5i   <- readr::read_csv("/Users/blrice/Documents/R GIT REPOS/2022_MNJ_TAZO/data_rdt/raw_data/S5_DATA_ENTRY_EST_20230705.csv")
dfS3i   <- readr::read_csv("/Users/blrice/Documents/R GIT REPOS/2022_MNJ_TAZO/data_rdt/raw_data/S3_DATA_ENTRY_EST_20230705.csv")
dfS2S1i <- readr::read_csv("/Users/blrice/Documents/R GIT REPOS/2022_MNJ_TAZO/data_rdt/raw_data/S2S1_DATA_ENTRY_EST_20230705.csv")
dfN1N5i <- readr::read_csv("/Users/blrice/Documents/R GIT REPOS/2022_MNJ_TAZO/data_rdt/raw_data/N54321_DATA_ENTRY_EST_20230705.csv")

#Reading in RDT data (also contains age data)
df.rdt <- readr::read_csv("/Users/blrice/Documents/R GIT REPOS/2022_MNJ_TAZO/data_rdt/rdt_data_20230705.csv")

####################################################################################################################################
#Processing height and weight data

#Dropping blank rows
dfS6i   <- dfS6i   %>% filter(!(is.na(unique_ind_id)))
dfS5i   <- dfS5i   %>% filter(!(is.na(unique_ind_id)))
dfS3i   <- dfS3i   %>% filter(!(is.na(unique_ind_id)))
dfS2S1i <- dfS2S1i %>% filter(!(is.na(unique_ind_id)))
dfN1N5i <- dfN1N5i %>% filter(!(is.na(unique_ind_id)))



####################################################################################################################################

#Making a vector of column names for the columns keeping here
cols_to_keep2 <- c("unique_ind_id",
                   "T01_KG","T01_CM",
                   "T02_KG","T02_CM",
                   "T03_KG","T03_CM",
                   "T04_KG","T04_CM",
                   "T05_KG","T05_CM",
                   "T06_KG","T06_CM",
                   "T07_KG","T07_CM",
                   "T08_KG","T08_CM",
                   "T09_KG","T09_CM",
                   "T10_KG","T10_CM",
                   "T11_KG","T11_CM")
#Selecting columns
dfS6w   <- dfS6i   %>% select(all_of(cols_to_keep2))
dfS5w   <- dfS5i   %>% select(all_of(cols_to_keep2))
dfS3w   <- dfS3i   %>% select(all_of(cols_to_keep2))
dfS2S1w <- dfS2S1i %>% select(all_of(cols_to_keep2))
dfN1N5w <- dfN1N5i %>% select(all_of(cols_to_keep2))

####################################################################################################################################
#Combining data files
df1 <- rbind(dfS2S1w, dfS3w, dfS5w, dfS6w, dfN1N5w)

####################################################################################################################################
#Pivoting longer
df2 <- df1 %>% 
  #Pivoting
  pivot_longer(
    #Specify the columns to tuck under
    #NOT The columns to stay at the left
    cols = !(unique_ind_id),
    #Splitting the variable (e.g., rdt result, date of sample) from the time point (e.g., T08)
    #Using the spacer "_" that separates variable name from time_point
    names_to = c("time_point", "variable"),
    names_sep = "_",
    #sending values to a column named value
    values_to = "value"
  )
head(df2)

#Too long: Pivoting wider to get column for weight and height
df2 <- df2 %>%
  pivot_wider(names_from = variable, values_from = value)
head(dfw1)

####################################################################################################################################
#Combining height and weight data with RDT and age data

#Creating 'full_id (unique_ind_id + time_point)
df3    <- df2    %>% mutate(full_id = paste0(unique_ind_id, "_", time_point)) %>% select(-c(time_point, unique_ind_id))
df.rdt <- df.rdt %>% mutate(full_id = paste0(unique_ind_id, "_", time_point))
#Joining
df4 <- full_join(df.rdt, df3, by = join_by(full_id)) %>%
  select(full_id, unique_ind_id:time_point, rdt.result, KG, CM, sample.date:age.cat.at.sample)


####################################################################################################################################
#Exporting processed height and weight data (merged with RDT data) for downstream plotting and merging with other data

#Export in long format
write_csv(df4, "/Users/blrice/Downloads/rdt_anthro_data_20230707.csv")

