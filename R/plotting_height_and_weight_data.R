library(tidyverse)
library(patchwork)

####################################################################################################################################
#Playing with height and weight data

#Reading in data
dfw1 <- readr::read_csv("/Users/blrice/Documents/R GIT REPOS/2022_MNJ_TAZO/data_rdt/rdt_anthro_data_20230707.csv") 
#Removing unnecessary column
dfw1 %>% select(-enrollment_status)


####################################################################################################################################
# INCREMENT ERRORS
####################################################################################################################################
#Testing for weight and height data entry errors by seeing if weight values are not in increments of 0.5
##Unclear why this is so difficult for R but something about the way excel stores and rounds numbers?
tester.w <- dfw1 %>% 
  select(-CM) %>%
  filter(!is.na(KG)) %>%
  mutate(tester1 = KG*100/5) %>% rowwise() %>%
  #Those with a difference that is not tiny have an issue
  #If divisible by 0.5 then KG*100/5 should give a whole number
  mutate(tester2 = abs(tester1-round(tester1, 0))) %>%
  filter(tester2 > 2e-5)

tester.w

#Doing same for height (increments of 0.1)
tester.h <- dfw1 %>% 
  select(-KG) %>%
  filter(!is.na(CM)) %>%
  mutate(tester1 = CM*100/1) %>% rowwise() %>%
  #Those with a difference that is not tiny have an issue
  #If divisible by 0.5 then KG*100/5 should give a whole number
  mutate(tester2 = abs(tester1-round(tester1, 0))) %>%
  filter(tester2 > 2e-5)

tester.h

#Problems***
# 29 incorrect weight measures: see data frames above
#   e.g., weight = 46.58 kg
# No issues with height
# SOLUTION: Correct data entry error for weights / check data sheets



####################################################################################################################################
# CRAZY BOUNCE ERRORS
####################################################################################################################################
#Checking for crazy bounces in weight
dfw3 <- dfw1 %>% drop_na() %>% group_by(site_code, unique_ind_id) %>%
  summarize(n = n(),
            mean_kg = mean(KG),
            max_kg = max(KG),
            min_kg = min(KG),
            max_min_kg = max(KG)-min(KG),
            max_min_perc = max_min_kg/mean_kg*100) %>% ungroup() %>%
  #Dropping individuals with only 1 measurement
  filter(n > 1)



pw1 <- dfw3 %>% 
  arrange(site_code, max_kg) %>% group_by(site_code) %>%
  mutate(order_for_plotting = 1:n()) %>% ungroup() %>%
  ggplot(aes(x=order_for_plotting, y=max_kg, color=mean_kg)) +
  geom_point(stat = "identity") +
  scale_color_viridis_c() +
  xlab("Individual") + ylab("Max weight (KG)") +
  labs(title    = "Checking for outliers") +
  facet_grid(cols = vars(site_code), scales = "free_x") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
        axis.text.x = element_blank())
pw1

#Problems***
# Some impossibly large swings in weight over time
#   Examples:
#   N5.48.07 jumped to 96.70 kg
#   N1.16.03 jumped to 90.80 kg
# SOLUTION: Edit data entry
#Note: T01 measures probably less reliable

pw2 <- dfw3 %>% 
  ggplot(aes(x=min_kg, y=max_min_kg, color=max_min_perc)) +
  geom_point(stat = "identity") +
  scale_color_viridis_c(option = "C") +
  labs(title    = "Checking for outliers") +
  facet_grid(cols = vars(site_code), scales = "free_x") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
        axis.text.x = element_blank())
pw2

pw3 <- dfw3 %>% 
  #Dropping obvious outliers already ID'ed above and below
  filter(max_min_perc <= 30) %>%
  ggplot(aes(x=min_kg, y=max_min_perc, color = max_min_perc)) +
  geom_point(stat = "identity") +
  scale_color_viridis_c(option = "C") +
  labs(title    = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
        axis.text.x = element_blank())
pw3
#Encouragingly, fluctuation in weight over the course of the study is larger for younger children?
#Makes sense as they are growing rapidly relative to their starting weight

#Checking with a histogram
hist(dfw3$max_min_perc[dfw3$max_min_perc < 30])

#Problems***
# 12 individuals with swings in body weight > 15 kgs
problems1 <- dfw3 %>% filter(max_min_kg > 15) 
# Approx. 200 individuals with swings in body weight over 30%
# Of these, 30 weighed over 15 kgs (large swings expected for growing young babies, but not for others)
problems2 <- dfw3 %>% filter(max_min_perc > 30) %>% filter(min_kg > 15)
# SOLUTION: Edit data entry for individuals

####################################################################################################################################
#Checking for crazy bounces in height

dfh1 <- dfw1 %>% filter(!is.na(CM)) %>% 
  group_by(site_code, unique_ind_id) %>%
  summarize(n = n(),
            min_CM = min(CM),
            max_CM = max(CM),
            mean_CM = mean(CM),
            max_min_CM = max(CM) - min(CM),
            max_min_CM_perc = max_min_CM/mean_CM * 100,
            sd = sd(CM)) %>%
  filter(n > 1) %>% arrange(desc(sd))

#***Problems
#Some impossibly large swings in height
#E.g., individual going from 15.1 to 166.2 CM

ph1.perc <- dfh1 %>% 
  ggplot(aes(x=min_CM, y=max_min_CM_perc, color=site_code)) +
  geom_point(stat = "identity", alpha = 0.7) +
  scale_color_viridis_d(option = "E") +
  xlab("Starting height") + ylab("% change in height during study") +
  labs(title = "Checking for outliers") +
  facet_grid(cols = vars(site_code), scales = "free_x") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
        #axis.text.x = element_blank()
        )
ph1.perc

#Problems***
#Approx. 3-10 obvious outliers per site
#Some might be weight tranpositions because even a newborn is more than 45cm
#SOLUTION: Edit data entry and check data sheets

# Encouragingly, variance decreases as starting height goes up (kids are growing fast, adults not)
# Possibly interesting to look at slow growing kids and see if they have more malaria

ph1.perc.all <- dfh1 %>% 
  #filtering out obvious data entry errors
  filter(min_CM > 45) %>%
  ggplot(aes(x=min_CM, y=max_min_CM_perc, color=min_CM)) +
  geom_point(stat = "identity", alpha = 0.7) +
  #geom_text(aes(label = unique_ind_id)) +
  scale_color_viridis_c(option = "B") +
  xlab("Starting height (CM)") + ylab("% change in height during study") +
  labs(title = "Checking for outliers, all sites combined") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
        #axis.text.x = element_blank()
  )
ph1.perc.all


#Also doing with standard deviation to highlight outliers
ph1.sd.all <- dfh1 %>% 
  #filtering out obvious data entry errors
  filter(min_CM > 45) %>%
  ggplot(aes(x=min_CM, y=sd, color=min_CM)) +
  geom_point(stat = "identity", alpha = 0.7) +
  #geom_text(aes(label = unique_ind_id)) +
  scale_color_viridis_c(option = "B") +
  xlab("Starting height (CM)") + ylab("Standard deviation of height measures per individual") +
  labs(title = "Checking for outliers, all sites combined") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
        #axis.text.x = element_blank()
  )
ph1.sd.all

grid.arrange(ph1.perc.all, ph1.sd.all, nrow = 1)

#Plots slightly different shapes based on percent change vs sd but from checking labels seems like flagging the same individuals


####################################################################################################################################
# CHECKING GROWTH/EVOLUTION IN HEIGHT

#For example for site S1
#For individuals with a decent number of height measures, checking height change over time for obvious issues
dfh2 <- dfw1 %>% filter(!is.na(CM)) %>%
  filter(CM > 45) %>%
  select(-KG) %>%
  group_by(unique_ind_id) %>% mutate(n = n(), min_CM = min(CM)) %>% ungroup() %>%
  filter(n > 9) %>% 
  filter(site_code == "S1")

length(unique(dfh2$unique_ind_id))

ph2 <- dfh2 %>% ggplot(aes(x=unique_ind_id, y=CM, color = time_point)) +
  geom_point(stat = "identity") +
  #geom_text(aes(x=order_for_plotting, y=min_CM-1, label=unique_ind_id)) +
  scale_color_viridis_d(option = "B") +
  xlab("Individual") + ylab("Height") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
        axis.text.x = element_text(angle = 90))
ph2

#Problem***
#Some individuals where the max height isnt the last measure and the min height isn't the first measure

#Write some code to identify changes in height that are out of order (with a margin for measurement variance and day to day variance)


####################################################################################################################################
# COMPARING HEIGHT VS WEIGHT
####################################################################################################################################

#Checking height for weight

df.hfw1 <- dfw1 %>% drop_na()

#All ages
p.hfw1 <- df.hfw1 %>%
  ggplot(aes(x=KG, y=CM, color = time_point)) +
  geom_point(stat = "identity", alpha = 0.7) +
  scale_color_viridis_d(option = "B") +
  xlab("Weight (KG)") + ylab("Height (CM)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
        )
p.hfw1

#General shape encouraging with plateauing and broadening range for adults; rapid, tight rise for children

#Now for younger kids only
p.hfw2 <- df.hfw1 %>% filter(CM < 130) %>% filter(CM > 45) %>% 
  filter(KG < 40) %>% 
  ggplot(aes(x=KG, y=CM, color = time_point)) +
  geom_point(stat = "identity", alpha = 0.7) +
  scale_color_viridis_d(option = "B") +
  xlab("Weight (KG)") + ylab("Height (CM)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )
p.hfw2

#some with weights or heights that are too low or too high to be believable

p.hfw3 <- df.hfw1 %>% mutate(hfw = CM/KG) %>% 
  filter(CM < 130) %>% filter(CM > 70) %>% 
  filter(KG < 40) %>% 
  ggplot(aes(x=KG, y=hfw, color = CM)) +
  geom_point(stat = "identity", alpha = 0.5) +
  scale_color_viridis_c(option = "E") +
  xlab("Weight (KG)") + ylab("Height for Weight (CM/KG)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )
p.hfw3

#***Problems
#Some obvious outliers, but unclear how many are due to data entry errors already noted above for height or weight individually
#Will need to recheck after editing the obivously erroneous heights and weights if there are still issues
#   Where height or weight is within reasonable range but not that height for that weight (e.g., young child with 35 kgs)


####################################################################################################################################
#CHECKING HEIGHT AND WEIGHT VS AGE
####################################################################################################################################

#Plotting

#Height for age
p.hfa1 <- dfw1 %>% filter(!is.na(age.yrs.at.sample)) %>% filter(!is.na(CM)) %>%
  ggplot(aes(x=age.yrs.at.sample, y=CM, color=site_code)) +
  geom_point(stat = "identity", alpha = 0.5) +
  scale_color_viridis_d(option = "E") +
  facet_grid(cols = vars(site_code), scales = "free_y") +
  xlab("Age (Years)") + ylab("Height (CM)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )
p.hfa1

#Height for age for children
p.hfa2 <- dfw1 %>% filter(!is.na(age.yrs.at.sample)) %>% filter(!is.na(CM)) %>%
  filter(age.yrs.at.sample < 12) %>%
  ggplot(aes(x=age.yrs.at.sample, y=CM, color=time_point)) +
  geom_point(stat = "identity", alpha = 0.5) +
  scale_color_viridis_d(option = "B") +
  facet_grid(cols = vars(site_code)) +
  xlab("Age (Years)") + ylab("Height (CM)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )
p.hfa2


#Weight for age
p.wfa1 <- dfw1 %>% filter(!is.na(age.yrs.at.sample)) %>% filter(!is.na(KG)) %>%
  ggplot(aes(x=age.yrs.at.sample, y=KG, color=site_code)) +
  geom_point(stat = "identity", alpha = 0.5) +
  scale_color_viridis_d(option = "E") +
  facet_grid(cols = vars(site_code)) +
  xlab("Age (Years)") + ylab("Weight (KG)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )
p.wfa1

#Weight for age for children
p.wfa2 <- dfw1 %>% filter(!is.na(age.yrs.at.sample)) %>% filter(!is.na(KG)) %>%
  filter(age.yrs.at.sample < 12) %>%
  ggplot(aes(x=age.yrs.at.sample, y=KG, color=site_code)) +
  geom_point(stat = "identity", alpha = 0.5) +
  scale_color_viridis_d(option = "E") +
  facet_grid(cols = vars(site_code)) +
  xlab("Age (Years)") + ylab("Weight (KG)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )
p.wfa2




#Mean height and weight for age for chidren < 12

#Problems***
#As above, some obvious outliers indicating errors in data entry
#The plots above are a bit misleading because individuals have multiple measures
#Plotting mean height and mean weight over course of study per child might help ID date of birth errors

p.hfa3 <- dfw1 %>% filter(!is.na(age.yrs.at.sample)) %>% filter(!is.na(CM)) %>%
  filter(age.yrs.at.sample < 12) %>%
  group_by(site_code, unique_ind_id) %>% summarize(age = mean(age.yrs.at.sample), mean_CM = mean(CM)) %>%
  ggplot(aes(x=age, y=mean_CM, color=site_code)) +
  geom_point(stat = "identity", alpha = 0.5) +
  scale_color_viridis_d(option = "E") +
  facet_grid(cols = vars(site_code)) +
  xlab("Age (Years)") + ylab("Mean Height (CM)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
        axis.text.x = element_text(angle = 90))
p.hfa3

#Some obvious outliers, especially those much too high for their height
#Best practice is probably to use z scores away from mean/expected height per age and flag those too far away

p.wfa3 <- dfw1 %>% filter(!is.na(age.yrs.at.sample)) %>% filter(!is.na(KG)) %>%
  filter(age.yrs.at.sample < 12) %>%
  group_by(site_code, unique_ind_id) %>% summarize(age = mean(age.yrs.at.sample), mean_KG = mean(KG)) %>%
  ggplot(aes(x=age, y=mean_KG, color=site_code)) +
  geom_point(stat = "identity", alpha = 0.5) +
  scale_color_viridis_d(option = "E") +
  facet_grid(cols = vars(site_code)) +
  xlab("Age (Years)") + ylab("Mean Weight (KG)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )
p.wfa3

#Now doing per time point: Height
p.hfa4a <- dfw1 %>% filter(!is.na(age.yrs.at.sample)) %>% filter(!is.na(CM)) %>%
  filter(age.yrs.at.sample < 12) %>%
  filter(CM > 45) %>%
  filter(time_point %in% c("T01", "T02", "T03", "T04")) %>%
  ggplot(aes(x=age.yrs.at.sample, y=CM, color=site_code)) +
  geom_point(stat = "identity", alpha = 0.5) +
  facet_grid(cols = vars(time_point)) +
  xlab("Age (Years)") + ylab("Height (CM)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )

p.hfa4b <- dfw1 %>% filter(!is.na(age.yrs.at.sample)) %>% filter(!is.na(CM)) %>%
  filter(age.yrs.at.sample < 12) %>%
  filter(CM > 45) %>%
  filter(time_point %in% c("T05", "T06", "T07", "T08")) %>%
  ggplot(aes(x=age.yrs.at.sample, y=CM, color=site_code)) +
  geom_point(stat = "identity", alpha = 0.5) +
  facet_grid(cols = vars(time_point)) +
  xlab("Age (Years)") + ylab("Height (CM)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )

p.hfa4c <- dfw1 %>% filter(!is.na(age.yrs.at.sample)) %>% filter(!is.na(CM)) %>%
  filter(age.yrs.at.sample < 12) %>%
  filter(CM > 45) %>%
  filter(time_point %in% c("T09", "T10", "T11")) %>%
  ggplot(aes(x=age.yrs.at.sample, y=CM, color=site_code)) +
  geom_point(stat = "identity", alpha = 0.5) +
  facet_grid(cols = vars(time_point)) +
  xlab("Age (Years)") + ylab("Height (CM)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )

p.hfa4a
p.hfa4b
p.hfa4c



#Now doing per time point: Weight
p.wfa4a <- dfw1 %>% filter(!is.na(age.yrs.at.sample)) %>% filter(!is.na(KG)) %>%
  filter(age.yrs.at.sample < 12) %>%
  filter(KG < 40) %>%
  filter(time_point %in% c("T01", "T02", "T03", "T04")) %>%
  ggplot(aes(x=age.yrs.at.sample, y=KG, color=site_code)) +
  geom_point(stat = "identity", alpha = 0.5) +
  facet_grid(cols = vars(time_point)) +
  xlab("Age (Years)") + ylab("Weight (KG)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )

p.wfa4b <- dfw1 %>% filter(!is.na(age.yrs.at.sample)) %>% filter(!is.na(KG)) %>%
  filter(age.yrs.at.sample < 12) %>%
  filter(KG < 40) %>%
  filter(time_point %in% c("T05", "T06", "T07", "T08")) %>%
  ggplot(aes(x=age.yrs.at.sample, y=KG, color=site_code)) +
  geom_point(stat = "identity", alpha = 0.5) +
  facet_grid(cols = vars(time_point)) +
  xlab("Age (Years)") + ylab("Weight (KG)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )

p.wfa4c <- dfw1 %>% filter(!is.na(age.yrs.at.sample)) %>% filter(!is.na(KG)) %>%
  filter(age.yrs.at.sample < 12) %>%
  filter(KG < 40) %>%
  filter(time_point %in% c("T09", "T10", "T11")) %>%
  ggplot(aes(x=age.yrs.at.sample, y=KG, color=site_code)) +
  geom_point(stat = "identity", alpha = 0.5) +
  facet_grid(cols = vars(time_point)) +
  xlab("Age (Years)") + ylab("Weight (KG)") +
  labs(title = "Checking for outliers") +
  theme_bw() + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(), #Dropping gridlines
        panel.grid.minor = element_blank(), #Dropping gridlines
  )

p.wfa4a
p.wfa4b
p.wfa4c


