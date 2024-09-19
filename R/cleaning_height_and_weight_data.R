library(tidyverse)
library(gridExtra)
library(patchwork)

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

#Checking for duplicate unique_ind_ids
dfS6i[duplicated(dfS6i$unique_ind_id), ]
dfS5i[duplicated(dfS5i$unique_ind_id), ]
dfS3i[duplicated(dfS3i$unique_ind_id), ]
dfS2S1i[duplicated(dfS2S1i$unique_ind_id), ]
dfN1N5i[duplicated(dfN1N5i$unique_ind_id), ]

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

