library(tidyverse)
library(patchwork)

############################################################################################################
# 1 # Reading in food security questionnaire data
############################################################################################################

# Data 1 ###################################################################################################
# Follow-up survey data (T02-T11)
dfa.i <- readxl::read_excel("/Users/blrice/Library/CloudStorage/Dropbox/Lab Projects/2020 Projects/CRS2020/1 DATA/DATA/DATA_QUESTIONNAIRE/T11_Commcare Data Downloads 20230428/2_fanadiadiana/1 Follow Up/MALARIA - FANADIADIANA - Follow Up Questionnaire (created 2023-04-29) 2023-04-29.xlsx")

# Data 2 ###################################################################################################
# Baseline and new enrollee survey (T01 and some sporadic individuals at later time_points)
dfb.i <- readxl::read_excel("/Users/blrice/Library/CloudStorage/Dropbox/Lab Projects/2020 Projects/CRS2020/1 DATA/DATA/DATA_QUESTIONNAIRE/T11_Commcare Data Downloads 20230428/1_survey/1 head of household/MALARIA - Surveys - v2_Lohantokantrano (created 2023-04-29) 2023-04-29.xlsx")


############################################################################################################
# 2 # Killing lost souls and duplicates
############################################################################################################

#Loading lost soul killer file
#Make sure to format date to allow matching
df.lost.souls <- readxl::read_excel("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/raw_data/questionnaire_data/site_code_errors.xlsx") %>% mutate(date = ymd(date)) %>%
  filter(data_source %in% c("ind.survey", "fizahana_ind.survey", "hoh_survey"))

#Loading duplicate killer file
df.dups <- readxl::read_excel("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/raw_data/questionnaire_data/duplicate_errors.xlsx") %>% mutate(date = ymd(date)) %>%
  filter(data_source %in% c("ind_survey", "hoh_survey", "hoh_survey.b"))

#Function: f.lost.soul.killer()
#Inputs: data_frame
#lost souls file hard coded inside function
f.lost.soul.killer <- function(data){
  
  #Make a full code with id, time point and date to make sure to match to correct row
  #For data
  data <- data %>% mutate(full.code = paste(unique_ind_id, time_point, date, sep = "."))
  #For lost souls
  df.lost.souls <- df.lost.souls %>% mutate(full.code = paste(unique_ind_id, time_point, date, sep = "."))
  
  
  #First, create a data frame filtering for those individuals that need fixing
  df.fixers <- data %>% filter(full.code %in% df.lost.souls$full.code)
  #Separate this from a data frame where those individuals have been excised (will be re-merged later)
  df.trim   <- data %>% filter(!(full.code %in% df.lost.souls$full.code))
  
  #For the individuals needing fixing
  df.fixers <- df.fixers %>% 
    #Create a dummy variable to flag those that simply need to be deleted
    mutate(trash.flag = ifelse(full.code %in% df.lost.souls$full.code[df.lost.souls$solution == "trash"], 1, 0)) %>%
    #Filter out those that should be trashed, drop the dummy variable
    filter(trash.flag == 0) %>% select(-trash.flag) %>%
    #Because not every lost soul individual is in every data file (depends on data source), filter to those with a match
    filter(full.code %in% df.lost.souls$full.code[df.lost.souls$solution != "trash"]) %>% rowwise() %>%
    #Assign those that need updated id codes the updated id code
    mutate(unique_ind_id = df.lost.souls$assign_to[df.lost.souls$full.code == full.code])
  #Re-merge, appending the fixed individuals to those that were okay, re-arrange in order
  df.fixed <- rbind(df.trim, df.fixers) %>% select(-full.code) %>% arrange(unique_ind_id, time_point) %>%
    #Re-do site_code as it should be updated based on resolving lost souls
    mutate(site_code = substr(unique_ind_id, 1, 2))
  
  return(df.fixed)
}




#Function 2d: f.duplicate.killer()
#Inputs: data_frame (stitched together 'complete' data frame for that variable)
#lost souls file hard coded inside function
f.duplicate.killer <- function(data){
  
  #Split data into those that are okay and those that need fixing
  df.fixers <- data %>% filter(formid %in% df.dups$formid)
  df.trim   <- data %>% filter(!(formid %in% df.dups$formid))
  
  #creating a vector of formids for surveys to trash
  trashers.v <- as.character(df.dups$formid[df.dups$assign_to == "trash"])
  
  #trashing trash
  df.fixers <- df.fixers %>% filter(!(formid %in% trashers.v))
  #If formid is within those requiring duplicate resolution, replace unique_ind_id with the new assigned unique_ind_id
  df.fixers <- df.fixers %>% rowwise() %>% 
    mutate(unique_ind_id = ifelse(formid %in% df.dups$formid, df.dups$assign_to[formid == df.dups$formid], unique_ind_id))
  
  #Re-merge, appending the fixed individuals to those that were okay, re-arrange in order
  #df.fixed <- rbind(df.trim, df.fixers) %>% select(-formid) %>% arrange(unique_ind_id, time_point) %>%
  #Temporarily keeping formid
  df.fixed <- rbind(df.trim, df.fixers) %>% arrange(unique_ind_id, time_point) %>%
    #Re-do site_code as it should be updated based on resolving lost souls
    mutate(site_code = substr(unique_ind_id, 1, 2))
  
  return(df.fixed)
}


############################################################################################################
# 3.1 # Tidying data for Data 1: Follow-up survey data (T02-T11)
############################################################################################################


#Columns to select from Follow Up
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_skip_eating_days
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_reduce_meals_days
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_hunt_days
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_gather_days
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_restrict_nonworkers
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_restrict_children
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_restrict_adults
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_limit_portions
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_send_beg
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_send_to_others
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_consume_stock
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_harvest_early
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_purchase_on_credit
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_borrow_food
#* form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_less_preferred

#Names for columns used for Sites S1-S6 during T02 before survey revisions completed

#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_skip_eating_days
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_reduce_meals_days
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_hunt_days
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_gather_days
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_restrict_nonworkers
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_restrict_children
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_restrict_adults
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_limit_portions
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_send_beg
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_send_to_others
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_consume_stock
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_harvest_early
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_purchase_on_credit
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_borrow_food
#* form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_less_preferred


cols_to_keep <- c("form.ind_data.ind_code", "completed_time", "formid",
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_skip_eating_days", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_reduce_meals_days", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_hunt_days", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_gather_days", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_restrict_nonworkers", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_restrict_children", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_restrict_adults", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_limit_portions", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_send_beg", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_send_to_others", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_consume_stock", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_harvest_early", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_purchase_on_credit", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_borrow_food", 
                  "form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_less_preferred",
                  
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_skip_eating_days", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_reduce_meals_days", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_hunt_days", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_gather_days", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_restrict_nonworkers", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_restrict_children", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_restrict_adults", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_limit_portions", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_send_beg", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_send_to_others", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_consume_stock", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_harvest_early", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_purchase_on_credit", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_borrow_food", 
                  "form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.csi_less_preferred")



df1 <- dfa.i %>% dplyr::select(all_of(cols_to_keep)) 

#Cleaning up column names
names(df1) <- sub('form.lohantokontrano.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.', 'T02.S.', names(df1))
names(df1) <- sub('form.section_03_fanontaniana_mikasika_ny_fanjarian-tsakafo.', '', names(df1))

df2 <- df1 %>%
  rename(unique_ind_id = form.ind_data.ind_code) %>%
  #Adding site code
  mutate(site_code = substr(unique_ind_id, 1, 2)) %>%
  mutate(date = ymd(substr(completed_time, 1, 10))) %>% 
  dplyr::select(-completed_time) %>%
  mutate(time_point = case_when(
    date >= ymd("2021-07-21") & date <= ymd("2021-10-19") ~ "T01",
    date >= ymd("2021-11-18") & date <= ymd("2021-12-13") ~ "T02",
    date >= ymd("2022-01-11") & date <= ymd("2022-03-16") ~ "T03",
    date >= ymd("2022-03-31") & date <= ymd("2022-05-06") ~ "T04",
    date >= ymd("2022-05-12") & date <= ymd("2022-06-21") ~ "T05",
    date >= ymd("2022-07-07") & date <= ymd("2022-08-14") ~ "T06",
    date >= ymd("2022-08-24") & date <= ymd("2022-09-17") ~ "T07",
    date >= ymd("2022-10-18") & date <= ymd("2022-11-12") ~ "T08",
    date >= ymd("2022-11-23") & date <= ymd("2022-12-20") ~ "T09",
    date >= ymd("2023-01-27") & date <= ymd("2023-03-06") ~ "T10",
    date >= ymd("2023-03-15") & date <= ymd("2023-04-27") ~ "T11")) %>%
  #Trashing some rows that were from test survey submissions submitted before/after sampling time points
  filter(!is.na(time_point)) %>%
  #Converting annoying "---"s from commcare to NAs
  mutate(across(T02.S.csi_skip_eating_days:csi_less_preferred, ~na_if(., "---")))

df3.1 <- df2 %>% mutate(
  skip_eating_days    = ifelse(is.na(T02.S.csi_skip_eating_days),    csi_skip_eating_days,    T02.S.csi_skip_eating_days),
  reduce_meals_days   = ifelse(is.na(T02.S.csi_reduce_meals_days),   csi_reduce_meals_days,   T02.S.csi_reduce_meals_days),
  hunt_days           = ifelse(is.na(T02.S.csi_hunt_days),           csi_hunt_days,           T02.S.csi_hunt_days),
  gather_days         = ifelse(is.na(T02.S.csi_gather_days),         csi_gather_days,         T02.S.csi_gather_days),
  restrict_nonworkers = ifelse(is.na(T02.S.csi_restrict_nonworkers), csi_restrict_nonworkers, T02.S.csi_restrict_nonworkers),
  restrict_children   = ifelse(is.na(T02.S.csi_restrict_children),   csi_restrict_children,   T02.S.csi_restrict_children),
  restrict_adults     = ifelse(is.na(T02.S.csi_restrict_adults),     csi_restrict_adults,     T02.S.csi_restrict_adults),
  limit_portions      = ifelse(is.na(T02.S.csi_limit_portions),      csi_limit_portions,      T02.S.csi_limit_portions),
  send_beg            = ifelse(is.na(T02.S.csi_send_beg),            csi_send_beg,            T02.S.csi_send_beg),
  send_to_others      = ifelse(is.na(T02.S.csi_send_to_others),      csi_send_to_others,      T02.S.csi_send_to_others),
  consume_stock       = ifelse(is.na(T02.S.csi_consume_stock),       csi_consume_stock,       T02.S.csi_consume_stock),
  harvest_early       = ifelse(is.na(T02.S.csi_harvest_early),       csi_harvest_early,       T02.S.csi_harvest_early),
  purchase_on_credit  = ifelse(is.na(T02.S.csi_purchase_on_credit),  csi_purchase_on_credit,  T02.S.csi_purchase_on_credit),
  borrow_food         = ifelse(is.na(T02.S.csi_borrow_food),         csi_borrow_food,         T02.S.csi_borrow_food),
  less_preferred      = ifelse(is.na(T02.S.csi_less_preferred),      csi_less_preferred,      T02.S.csi_less_preferred)) %>%
  mutate(hh_id = substr(unique_ind_id, 1,5)) %>%
  dplyr::select(formid, unique_ind_id, hh_id, site_code:last_col()) %>%
  drop_na()


#Calling the above functions to remove duplicates
df3.2 <- f.duplicate.killer(df3.1)
df3.3 <- f.lost.soul.killer(df3.2)
df3 <- df3.3 %>% 
  #dplyr::select(-formid) %>% 
  #recalculate site_id and hh_id to reflect updated IDs after resolving duplicates
  mutate(site_code = substr(unique_ind_id, 1, 2)) %>%
  mutate(hh_id = substr(unique_ind_id, 1, 5)) %>%
  mutate(full.code = paste0(unique_ind_id, ".", time_point)) %>%
  dplyr::select(formid, full.code, unique_ind_id:last_col())


#Checking for duplicate individuals
df3 %>% group_by(unique_ind_id, time_point) %>% summarize(n = n()) %>% filter(n > median(n))

#Checking for mulitple responses per household (should only be head of household)
df3 %>% group_by(hh_id, time_point) %>% mutate(n = n()) %>% filter(n > 1) 

#Dropping full.code and formid columns
df3 <- df3 %>% dplyr::select(-formid)

df4 <- df3 %>% 
  pivot_longer(!(full.code:time_point), names_to = "variable", values_to = "num.days") %>%
  mutate(num.days = case_when(
    num.days == "days_0" ~ 0,
    num.days == "days_1" ~ 1,
    num.days == "days_2" ~ 2,
    num.days == "days_3" ~ 3,
    num.days == "days_4" ~ 4,
    num.days == "days_5" ~ 5,
    num.days == "days_6" ~ 6,
    num.days == "days_7" ~ 7))


#mean number of days point
df4 %>% 
  group_by(site_code, time_point, variable) %>%
  summarize(mean = mean(num.days)) %>%
  ggplot(aes(x = time_point, y = mean, color = variable)) +
  geom_point() +
  facet_grid(cols = vars(site_code), rows = vars(variable))


#%s
df4.p <- df4 %>% 
  group_by(variable, time_point, num.days) %>%
  summarize(n = n()) %>% mutate(prop = n/sum(n))

df4.p %>% 
  ggplot(aes(x=time_point, y=prop, fill = num.days)) +
  geom_bar(position = "stack", stat="identity") +
  facet_wrap(vars(variable), nrow = 3) +
  scale_fill_viridis_c(option = "mako", name = "Number of days\nper week", end = 0.95) +
  xlab("Time Point") + ylab("Proportion of households") +
  labs(title = "Food insecurity questionnaire response data",
       subtitle = "Number of days within the last week that a household reports having to use the follow coping strategies due to a lack of sufficent food",
       caption = "(see table below for definitions of the 15 indicators)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))


############################################################################################################
# 3.2 # Tidying data for Data 2: Baseline and new enrollee surve
############################################################################################################

cols_to_keep.b <- c("form.site", "form.hh_code", "form.ind_code", "form.hoh_surrogate_ind_code",
                    "completed_time", "formid",
                    
                    "form.csi_skip_eating_days",
                    "form.csi_reduce_meals_days",
                    "form.csi_hunt_days",
                    "form.csi_gather_days",
                    "form.csi_restrict_nonworkers",
                    "form.csi_restrict_children",
                    "form.csi_restrict_adults",
                    "form.csi_limit_portions",
                    "form.csi_send_beg",
                    "form.csi_send_to_others",
                    "form.csi_consume_stock",
                    "form.csi_harvest_early",
                    "form.csi_purchase_on_credit",
                    "form.csi_borrow_food",
                    "form.csi_less_preferred")

dfb1 <- dfb.i %>% dplyr::select(all_of(cols_to_keep.b))

#Cleaning up individual ID columns
dfb1 <- dfb1 %>% mutate(ind = case_when(
  form.ind_code == "---" & form.hoh_surrogate_ind_code == "---" ~ NA_character_,
  form.ind_code != "---" & form.hoh_surrogate_ind_code == "---" ~ form.ind_code,
  form.ind_code == "---" & form.hoh_surrogate_ind_code != "---" ~ form.hoh_surrogate_ind_code),
  .before = completed_time) %>% dplyr::select(-c(form.ind_code, form.hoh_surrogate_ind_code))

#Cleaning up column names
dfb1 <- dfb1 %>% rename(site = form.site, hh = form.hh_code) %>%
  filter(site != "---") %>% filter(hh != "---") %>%
  mutate(site_code = case_when(
    site == "s1_ampangalana_sud"   ~ "S1",
    site == "s2_ambalavontaka"     ~ "S2",
    site == "s3_manampano"         ~ "S3",
    site == "s5_ambinany_tanambao" ~ "S5",
    site == "s6_masindranokely"    ~ "S6",
    site == "n1_ampandimana"       ~ "N1",
    site == "n3_anosibe"           ~ "N2",
    site == "n4_ambodimanga"       ~ "N3",
    site == "n5_ampasimanjeva"     ~ "N4",
    site == "n6_mahavelona"        ~ "N5")) %>%
  mutate(hh_code  = ifelse(nchar(hh)  == 1, paste0("0", hh),  hh)) %>%
  mutate(ind_code = ifelse(ind == "---", "01", paste0("0", ind))) %>%
  mutate(unique_ind_id = paste(site_code, hh_code, ind_code, sep = ".")) %>%
  select(-c(site, site_code, hh, hh_code, ind, ind_code))

names(dfb1) <- sub('form.csi_', '', names(dfb1))

dfb2 <- dfb1 %>%
  #Adding hh code
  mutate(hh_id = substr(unique_ind_id, 1, 5)) %>%
  #Adding site code
  mutate(site_code = substr(unique_ind_id, 1, 2)) %>%
  mutate(date = ymd(substr(completed_time, 1, 10))) %>% 
  dplyr::select(-completed_time) %>%
  mutate(time_point = case_when(
    date >= ymd("2021-07-21") & date <= ymd("2021-10-19") ~ "T01",
    date >= ymd("2021-11-18") & date <= ymd("2021-12-13") ~ "T02",
    date >= ymd("2022-01-11") & date <= ymd("2022-03-16") ~ "T03",
    date >= ymd("2022-03-31") & date <= ymd("2022-05-06") ~ "T04",
    date >= ymd("2022-05-12") & date <= ymd("2022-06-21") ~ "T05",
    date >= ymd("2022-07-07") & date <= ymd("2022-08-14") ~ "T06",
    date >= ymd("2022-08-24") & date <= ymd("2022-09-17") ~ "T07",
    date >= ymd("2022-10-18") & date <= ymd("2022-11-12") ~ "T08",
    date >= ymd("2022-11-23") & date <= ymd("2022-12-20") ~ "T09",
    date >= ymd("2023-01-27") & date <= ymd("2023-03-06") ~ "T10",
    date >= ymd("2023-03-15") & date <= ymd("2023-04-27") ~ "T11")) %>%
  #Trashing some rows that were from test survey submissions submitted before/after sampling time points
  filter(!is.na(time_point)) %>%
  #Converting annoying "---"s from commcare to NAs
  mutate(across(skip_eating_days:less_preferred, ~na_if(., "---"))) %>%
  dplyr::select(formid, unique_ind_id:time_point, skip_eating_days:last_col())


#Calling the above functions to remove duplicates
dfb3.1 <- f.duplicate.killer(dfb2)
dfb3.2 <- f.lost.soul.killer(dfb3.1)
dfb3 <- dfb3.2 %>% 
  #recalculate site_id and hh_id to reflect updated IDs after resolving duplicates
  mutate(site_code = substr(unique_ind_id, 1, 2)) %>%
  mutate(hh_id = substr(unique_ind_id, 1, 5)) %>%
  mutate(full.code = paste0(unique_ind_id, ".", time_point)) %>%
  dplyr::select(formid, full.code, formid:last_col())

#Checking for duplicates (multiple samples per hh)
dfb3 %>% group_by(hh_id, time_point) %>% mutate(n = n()) %>% filter(n > 1) %>% ungroup()

#Pivot and make numeric
dfb4 <- dfb3 %>% 
  dplyr::select(-formid) %>%
  pivot_longer(!(full.code:time_point), names_to = "variable", values_to = "num.days") %>%
  mutate(num.days = case_when(
    num.days == "days_0" ~ 0,
    num.days == "days_1" ~ 1,
    num.days == "days_2" ~ 2,
    num.days == "days_3" ~ 3,
    num.days == "days_4" ~ 4,
    num.days == "days_5" ~ 5,
    num.days == "days_6" ~ 6,
    num.days == "days_7" ~ 7))

####################################################################################################################
# Binding all questionnaire data together
df5 <- rbind(dfb4, df4)

# Check for duplicates again and resolve
df5 %>% group_by(hh_id, time_point) %>% mutate(n = n()) %>% filter(n > median(n))

# Test plotting
str(df5)
#%s
df5 %>% 
  group_by(variable, time_point, num.days) %>%
  summarize(n = n()) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = time_point, y = prop, fill = num.days)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(vars(variable), nrow = 3) +
  scale_fill_viridis_c(option = "mako", name = "Number of days\nper week", end = 0.95) +
  xlab("Time Point") + ylab("Proportion of households") +
  labs(title = "Food insecurity questionnaire response data",
       subtitle = "Number of days within the last week that a household reports having to use the follow coping strategies due to a lack of sufficent food") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))


#Pivoting wider
df6 <- df5 %>% pivot_wider(names_from = variable, values_from = num.days) %>% arrange(hh_id, time_point)
#Exporting questionnaire data as a CSV file

# write_csv(df6, "/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/processed_data/food_security_questionnaire_data.csv")

####################################################################################################################
# Joining questionnaire data with the malaria + hb + anthro data

df.rdt <- readr::read_csv("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/processed_data/rdt_hb_anthro_data.csv")

df7 <- full_join(df.rdt, 
                 df6 %>% dplyr::select(-c(unique_ind_id, hh_id, site_code, time_point, date)),
                 by = join_by(full.code)) %>%
  mutate(hh_id = substr(unique_ind_id, 1, 5), .before = time_point)

#Checking for duplicated questionnaire data
df7 %>% filter(!is.na(skip_eating_days)) %>% group_by(hh_id, time_point) %>% mutate(n = n()) %>% filter(n > 1)

####################################################################################################################
# Applying questionnaire data to all individuals in a household

df8 <- df7 %>% group_by(hh_id, time_point) %>% 
  mutate(skip_eating_days    = mean(skip_eating_days, na.rm = TRUE),
         reduce_meals_days   = mean(reduce_meals_days, na.rm = TRUE),
         hunt_days           = mean(hunt_days, na.rm = TRUE),
         gather_days         = mean(gather_days, na.rm = TRUE),
         restrict_nonworkers = mean(restrict_nonworkers, na.rm = TRUE),
         restrict_children   = mean(restrict_children, na.rm = TRUE),
         restrict_adults     = mean(restrict_adults, na.rm = TRUE),
         limit_portions      = mean(limit_portions, na.rm = TRUE),
         send_beg            = mean(send_beg, na.rm = TRUE),
         send_to_others      = mean(send_to_others, na.rm = TRUE),
         consume_stock       = mean(consume_stock, na.rm = TRUE),
         harvest_early       = mean(harvest_early, na.rm = TRUE),
         purchase_on_credit  = mean(purchase_on_credit, na.rm = TRUE),
         borrow_food         = mean(borrow_food, na.rm = TRUE),
         less_preferred      = mean(less_preferred, na.rm = TRUE)) %>%
  mutate(across(skip_eating_days:less_preferred, ~ ifelse(is.nan(.), NA, .)))

#Exporting a csv file
# write_csv(df8, "/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/processed_data/malaria_nutrition_data.csv")






