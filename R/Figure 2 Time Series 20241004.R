library(tidyverse)
library(patchwork)
library(ggrepel)
library(geosphere)
library(sf)
library(terra)
library(scales)

####################################################################################################
## Read in data
####################################################################################################

#Malaria and nutrition indicators
df1 <- read_csv("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/processed_data/malaria_nutrition_data.csv")
#Sampling dates
dfs.i <- read_csv("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/raw_data/malaria_rdt_summary/sample_dates.csv")


####################################################################################################
# Trimming and tidying data # Malaria and nutrition indicators
####################################################################################################

df2 <- df1 %>% 
  #Trimming out rows with no data
  filter(!is.na(sample.date)) %>% 
  #Site codes
  mutate(site_code = factor(site_code, levels = rev(c("N1", "N2", "N3", "N4", "N5", "S1", "S2", "S3", "S5", "S6")))) %>%
  mutate(code.new = case_when(site_code == "N5" ~ "MNJ.01",
                              site_code == "N4" ~ "MNJ.02",
                              site_code == "N3" ~ "MNJ.03",
                              site_code == "N2" ~ "MNJ.04",
                              site_code == "N1" ~ "MNJ.05",
                              site_code == "S1" ~ "MNJ.06",
                              site_code == "S2" ~ "MNJ.07",
                              site_code == "S3" ~ "MNJ.08",
                              site_code == "S5" ~ "MNJ.09",
                              site_code == "S6" ~ "MNJ.10"), .before = unique_ind_id) %>%
  mutate(code.new = factor(code.new, levels = c("MNJ.01", "MNJ.02", "MNJ.03", "MNJ.04", "MNJ.05", "MNJ.06", "MNJ.07", "MNJ.08", "MNJ.09", "MNJ.10"))) %>%
  #moving baseling time point to t0 and subsequent follow up time points to T01-T10
  mutate(time_point = case_when(
    time_point == "T01" ~ "T0",
    time_point == "T02" ~ "T01",
    time_point == "T03" ~ "T02",
    time_point == "T04" ~ "T03",
    time_point == "T05" ~ "T04",
    time_point == "T06" ~ "T05",
    time_point == "T07" ~ "T06",
    time_point == "T08" ~ "T07",
    time_point == "T09" ~ "T08",
    time_point == "T10" ~ "T09",
    time_point == "T11" ~ "T10")) %>%
  #Factoring
  mutate(wasting.cat = factor(wasting.cat, levels = rev(c("normal", "MAM", "SAM"))))

####################################################################################################
# Trimming and tidying data # Sampling time series
####################################################################################################

dfs1 <- dfs.i %>%   mutate(code.new = case_when(site_code == "N5" ~ "MNJ.01",
                                                site_code == "N4" ~ "MNJ.02",
                                                site_code == "N3" ~ "MNJ.03",
                                                site_code == "N2" ~ "MNJ.04",
                                                site_code == "N1" ~ "MNJ.05",
                                                site_code == "S1" ~ "MNJ.06",
                                                site_code == "S2" ~ "MNJ.07",
                                                site_code == "S3" ~ "MNJ.08",
                                                site_code == "S5" ~ "MNJ.09",
                                                site_code == "S6" ~ "MNJ.10"), .before = site_name) %>%
  mutate(code.new = factor(code.new, levels = c("MNJ.01", "MNJ.02", "MNJ.03", "MNJ.04", "MNJ.05", "MNJ.06", "MNJ.07", "MNJ.08", "MNJ.09", "MNJ.10"))) %>%
  #Pivoting longer
  pivot_longer(!site_code:site_name, names_to = "time_point.day", values_to = "date.char") %>%
  mutate(date = dmy(date.char)) %>%
  mutate(time_point = substr(time_point.day, 1, 3)) %>%
  arrange(time_point, code.new) %>%
  #moving baseling time point to t0 and subsequent follow up time points to T01-T10
  mutate(time_point = case_when(
    time_point == "T01" ~ "T0",
    time_point == "T02" ~ "T01",
    time_point == "T03" ~ "T02",
    time_point == "T04" ~ "T03",
    time_point == "T05" ~ "T04",
    time_point == "T06" ~ "T05",
    time_point == "T07" ~ "T06",
    time_point == "T08" ~ "T07",
    time_point == "T09" ~ "T08",
    time_point == "T10" ~ "T09",
    time_point == "T11" ~ "T10"))


dfs.by_time_point <- dfs1 %>% filter(!is.na(date)) %>% group_by(time_point) %>% summarize(min.date = min(date), max.date = max(date))

dfs.by_site       <- dfs1 %>% filter(!is.na(date)) %>% group_by(code.new, time_point)


####################################################################################################
# 2A # Plot Time Series # Sampling time points and cyclones
####################################################################################################

#Making a data frame for plotting time series

dft <- tibble(all.dates = as.Date(min(dfs.by_time_point$min.date):max(dfs.by_time_point$max.date)),
              time_points = case_when(
                all.dates %in% as.Date(dfs.by_time_point$min.date[1]:dfs.by_time_point$max.date[1])   ~ "T0",
                all.dates %in% as.Date(dfs.by_time_point$min.date[2]:dfs.by_time_point$max.date[2])   ~ "T01",
                all.dates %in% as.Date(dfs.by_time_point$min.date[3]:dfs.by_time_point$max.date[3])   ~ "T02",
                all.dates %in% as.Date(dfs.by_time_point$min.date[4]:dfs.by_time_point$max.date[4])   ~ "T03",
                all.dates %in% as.Date(dfs.by_time_point$min.date[5]:dfs.by_time_point$max.date[5])   ~ "T04",
                all.dates %in% as.Date(dfs.by_time_point$min.date[6]:dfs.by_time_point$max.date[6])   ~ "T05",
                all.dates %in% as.Date(dfs.by_time_point$min.date[7]:dfs.by_time_point$max.date[7])   ~ "T06",
                all.dates %in% as.Date(dfs.by_time_point$min.date[8]:dfs.by_time_point$max.date[8])   ~ "T07",
                all.dates %in% as.Date(dfs.by_time_point$min.date[9]:dfs.by_time_point$max.date[9])   ~ "T08",
                all.dates %in% as.Date(dfs.by_time_point$min.date[10]:dfs.by_time_point$max.date[10]) ~ "T09",
                all.dates %in% as.Date(dfs.by_time_point$min.date[11]:dfs.by_time_point$max.date[11]) ~ "T10"))

#Function for finding mid point of time interval
f.mid.point <- function(start.date, end.date){
  x <- interval(ymd(start.date), ymd(end.date))
  return(ymd(round_date(x@start + as.duration(x)/2, unit = "day")))
}

dft <- dft %>% 
  #rowwise() %>% 
  mutate(mid.points = case_when(
  time_points == "T0"   ~ f.mid.point(dfs.by_time_point$min.date[1],  dfs.by_time_point$max.date[1]),
  time_points == "T01"  ~ f.mid.point(dfs.by_time_point$min.date[2],  dfs.by_time_point$max.date[2]),
  time_points == "T02"  ~ f.mid.point(dfs.by_time_point$min.date[3],  dfs.by_time_point$max.date[3]),
  time_points == "T03"  ~ f.mid.point(dfs.by_time_point$min.date[4],  dfs.by_time_point$max.date[4]),
  time_points == "T04"  ~ f.mid.point(dfs.by_time_point$min.date[5],  dfs.by_time_point$max.date[5]),
  time_points == "T05"  ~ f.mid.point(dfs.by_time_point$min.date[6],  dfs.by_time_point$max.date[6]),
  time_points == "T06"  ~ f.mid.point(dfs.by_time_point$min.date[7],  dfs.by_time_point$max.date[7]),
  time_points == "T07"  ~ f.mid.point(dfs.by_time_point$min.date[8],  dfs.by_time_point$max.date[8]),
  time_points == "T08"  ~ f.mid.point(dfs.by_time_point$min.date[9],  dfs.by_time_point$max.date[9]),
  time_points == "T09"  ~ f.mid.point(dfs.by_time_point$min.date[10], dfs.by_time_point$max.date[10]),
  time_points == "T10"  ~ f.mid.point(dfs.by_time_point$min.date[11], dfs.by_time_point$max.date[11]))) %>%
  mutate(mid.points.c = case_when(
    all.dates == mid.points ~ all.dates
  ))


v.cyc.dates <- c(ymd("2022-02-05"), ymd("2023-02-21"))

p.t1 <- dfs.by_site %>% 
  ggplot(aes(x = date, y = reorder(code.new, desc(code.new)))) + 
  #Adding cyclone timing
  geom_vline(xintercept = v.cyc.dates, color = "orange", linetype = "dashed") +
  #Adding year breaks
  geom_vline(xintercept = c(ymd("2022-01-01"), ymd("2023-01-01")), color = "grey80", linewidth = 0.5) +
  geom_point(shape = 15, size = 3) +
  geom_path(linetype = "dotted", linewidth = 0.2) +
  ylab("Site Code") + xlab(NULL) +
  scale_x_date(date_breaks = "1 month") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14))
p.t1

p.t2 <- dft %>% filter(!is.na(time_points)) %>% 
  ggplot(aes(x = all.dates, y = 1)) +
  geom_vline(xintercept = v.cyc.dates, color = "orange", linetype = "dashed") +
  geom_point(shape = 15) +
  annotate("label", x = v.cyc.dates[1], y = 0.92, label = "Cyclone Batsirai", color = "orange") +
  annotate("label", x = v.cyc.dates[2], y = 0.92, label = "Cyclone Freddy",   color = "orange") +
  geom_text(data = dft %>% filter(!is.na(time_points)) %>% filter(!is.na(mid.points.c)),
            aes(x = mid.points.c, y = 0.97, label = time_points)) +
  ylim(0.9, 1.1) +
  theme_void() +
  theme(legend.position = "none",
        axis.text = element_blank(), axis.ticks = element_blank())
p.t2

p.t1 / p.t2 + plot_layout(heights = c(3, 1))

####################################################################################################
# 2B # Change in malaria indicators over time
####################################################################################################

#Confidence intervals: using binom.test() and Clopper and Pearson (1934) method

#Writing a function to run binom.test() using number of positives and sample size
#Output of binom.test() is a htest object
#From which we extract the upper and lower confidence interval bounds
#For lower limit first
f.CI_calc_lower <- function(v.n_pos, v.n){
  v.CI_lower <- rep(NA, length(v.n_pos))
  for (i in 1:length(v.n_pos)) {
    x.i <- v.n_pos[i]
    n.i <- v.n[i]
    htest.i <- binom.test(x.i, n.i)
    v.CI_lower[i] <- htest.i$conf.int[1]
  }
  return(v.CI_lower)
}
#And now for upper limit
f.CI_calc_upper <- function(v.n_pos, v.n){
  v.CI_upper <- rep(NA, length(v.n_pos))
  for (i in 1:length(v.n_pos)) {
    x.i <- v.n_pos[i]
    n.i <- v.n[i]
    htest.i <- binom.test(x.i, n.i)
    v.CI_upper[i] <- htest.i$conf.int[2]
  }
  return(v.CI_upper)
}


v.p1.colors <- c("#3B2D5AFF", "#37659EFF", "#58CBADFF")

#All ages
df.p1.all <- df2 %>% filter(time_point != "T0") %>% filter(!is.na(rdt_result.coded)) %>%
  group_by(time_point) %>%
  summarize(n = n(), n_pos = sum(rdt_result.coded), prop = n_pos/n) %>% ungroup() %>%
  #Adding confidence intervals
  rowwise() %>% mutate(Prev.CI.lower = f.CI_calc_lower(n_pos, n)) %>% mutate(Prev.CI.upper = f.CI_calc_upper(n_pos, n))

p1.all <- df.p1.all %>% 
  ggplot(aes(x = time_point, y = prop)) + 
  geom_vline(xintercept = c(2, 9), color = "orange", linetype = "dashed") +
  geom_point(stat = "identity", size = 3, color = v.p1.colors[1]) +
  #Adding error bars
  geom_errorbar(aes(ymin=Prev.CI.lower, ymax=Prev.CI.upper),
                linewidth = 0.3,     # Thinner lines
                width = 0.06,        # Thinner horizontals on the Ts
                color = v.p1.colors[1],
                position="identity") +
  ylab("Proportion of sampled individuals positive for malaria (by RDT)\nAll ages") + xlab(NULL) +
  ylim(0, 0.25) +
  theme_bw() + 
  theme(legend.position = "right",
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
p1.all

#School aged children (SAC)
df.p1.sac <- df2 %>% filter(time_point != "T0") %>% filter(!is.na(rdt_result.coded)) %>%
  filter(age.yrs.at.sample > 6 & age.yrs.at.sample < 14) %>%
  group_by(time_point) %>%
  summarize(n = n(), n_pos = sum(rdt_result.coded), prop = n_pos/n) %>% ungroup() %>%
  #Adding confidence intervals
  rowwise() %>% mutate(Prev.CI.lower = f.CI_calc_lower(n_pos, n)) %>% mutate(Prev.CI.upper = f.CI_calc_upper(n_pos, n))

p1.sac <- df.p1.sac %>% 
  ggplot(aes(x = time_point, y = prop)) + 
  geom_vline(xintercept = c(2, 9), color = "orange", linetype = "dashed") +
  geom_point(stat = "identity", size = 3, color = v.p1.colors[2]) +
  #Adding error bars
  geom_errorbar(aes(ymin=Prev.CI.lower, ymax=Prev.CI.upper),
                linewidth = 0.3,     # Thinner lines
                width = 0.06,        # Thinner horizontals on the Ts
                color = v.p1.colors[2],
                position="identity") +
  ylab("Proportion of sampled individuals positive for malaria (by RDT)\nSchool aged children (6-13y)") + xlab(NULL) +
  ylim(0, 0.25) +
  theme_bw() + 
  theme(legend.position = "right",
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
p1.sac

#Young children (YC)
df.p1.yc <- df2 %>% filter(time_point != "T0") %>% filter(!is.na(rdt_result.coded)) %>%
  filter(age.yrs.at.sample >= 0.5 & age.yrs.at.sample <= 6) %>%
  group_by(time_point) %>%
  summarize(n = n(), n_pos = sum(rdt_result.coded), prop = n_pos/n) %>% ungroup() %>%
  #Adding confidence intervals
  rowwise() %>% mutate(Prev.CI.lower = f.CI_calc_lower(n_pos, n)) %>% mutate(Prev.CI.upper = f.CI_calc_upper(n_pos, n))

p1.yc <- df.p1.yc %>% 
  ggplot(aes(x = time_point, y = prop)) + 
  geom_vline(xintercept = c(2, 9), color = "orange", linetype = "dashed") +
  geom_point(stat = "identity", size = 3, color = v.p1.colors[3]) +
  #Adding error bars
  geom_errorbar(aes(ymin=Prev.CI.lower, ymax=Prev.CI.upper),
                linewidth = 0.3,     # Thinner lines
                width = 0.06,        # Thinner horizontals on the Ts
                color = v.p1.colors[3],
                position="identity") +
  ylab("Proportion of sampled individuals positive for malaria (by RDT)\nYoung children (0-5y)") + xlab(NULL) +
  ylim(0, 0.25) +
  theme_bw() + 
  theme(legend.position = "right",
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
p1.yc

p1.all + p1.yc + p1.sac

####################################################################################################
# 2C # Change in acute malnutrition over time
####################################################################################################

df.p2 <- df2 %>% dplyr::select(unique_ind_id, code.new, time_point, wfhz, wasting.cat, PB, MUAC.cat) %>%
  #Dropping T0
  filter(time_point != "T0") %>%
  #Dropping rows without data
  filter(!is.na(wfhz) | !is.na(MUAC.cat))

df.p2.wfh.prop <- df.p2 %>% filter(!is.na(wasting.cat)) %>%
  group_by(time_point, wasting.cat) %>%
  summarize(n = n()) %>% mutate(prop = n/sum(n)) %>%
  group_by(time_point) %>% mutate(total = sum(n), mam_sam = sum(n[wasting.cat != "normal"]), prop.mam_sam = mam_sam/total) %>%
  filter(wasting.cat != "normal") %>%
  #Adding error bars
  rowwise() %>% mutate(Prev.CI.lower = f.CI_calc_lower(mam_sam, total)) %>% mutate(Prev.CI.upper = f.CI_calc_upper(mam_sam, total))

# Plotting ###########################################################################################

#Acute Malnutrition (MAM/SAM)
p2.wfh.prop <- df.p2.wfh.prop %>%
  ggplot(aes(x = time_point, y = prop, fill = wasting.cat)) +
  geom_vline(xintercept = c(2, 9), color = "orange", linetype = "dashed") +
  geom_bar(position = "stack", stat = "identity") +
  geom_point(data = df.p2.wfh.prop %>% group_by(time_point) %>% mutate(row = row_number()) %>% filter(row == 1),
             aes(x = time_point, y = prop.mam_sam),
             color = "grey50",
             size = 0.8) +
  #Adding error bars
  geom_errorbar(data = df.p2.wfh.prop %>% group_by(time_point) %>% mutate(row = row_number()) %>% filter(row == 1),
                aes(ymin=Prev.CI.lower, ymax=Prev.CI.upper),
                linewidth = 0.3,     # Thinner lines
                width = 0.06,        # Thinner horizontals on the Ts
                color = "grey50",
                position="identity") +
  scale_fill_viridis_d(option = "mako", name = "Acute Malnutrition", 
                       end = 0.8, begin = 0.2, direction = -1) +
  ylab("Proportion of children") + xlab(NULL) + 
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
  #Removing the gray point from the fill legend
  guides(fill = guide_legend(override.aes = list(shape = NA)))
p2.wfh.prop


# viridis_pal(option = "mako")(15)

#HFW z scores
p2.wfh.z <- df.p2 %>% filter(!is.na(wfhz)) %>% filter(abs(wfhz) < 5) %>%
  ggplot(aes(x = time_point, y = wfhz)) +
  geom_vline(xintercept = c(2, 9), color = "orange", linetype = "dashed") +
  geom_jitter(alpha = 0.1, color = "#3E5095FF", width = 0.2) +
  geom_boxplot(outliers = FALSE, fill = NA, color = "#28192FFF") +
  geom_hline(yintercept = c(-2, -3), color = "grey50") +
  scale_y_continuous(breaks = c(-5:5)) +
  ylab("Height for Weight z-score") + xlab(NULL) +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
p2.wfh.z


#MUAC
p2.PB <- df.p2 %>% filter(!is.na(PB)) %>%
  ggplot(aes(x = time_point, y = PB)) +
  geom_vline(xintercept = c(2, 9), color = "orange", linetype = "dashed") +
  geom_jitter(alpha = 0.1, color = "#3E5095FF", width = 0.2, height = 0.1) +
  geom_boxplot(outliers = FALSE, fill = NA, color = "#28192FFF") +
  geom_hline(yintercept = c(12.5, 11.5), color = "grey50") +
  ylab("Mid-Upper Arm Circumference (MUAC) (cm)") + xlab(NULL) +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
p2.PB

p2.PB + p2.wfh.z + p2.wfh.prop


####################################################################################################
# 2D # Change in selected food security indicators over time
####################################################################################################


df.p3 <- df2 %>% dplyr::select(unique_ind_id, code.new, hh_id, time_point, skip_eating_days:last_col()) %>%
  pivot_longer(!(unique_ind_id:time_point), names_to = "variable", values_to = "num.days") %>%
  filter(!is.na(num.days)) %>%
  group_by(code.new, hh_id, time_point, variable) %>% summarize(num.days = median(num.days)) %>% ungroup() %>%
  group_by(variable, time_point, num.days) %>%
  summarize(n = n()) %>% mutate(prop = n/sum(n))

#All indicators
df.p3 %>%
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


#Select indicators: Less preferred, limiting portions, reduce meal days
#Subsetting to T1-T10 (dropping baseline)

panel_names.CSI <- as_labeller(c("less_preferred"    = "Relying on less preferred and\nless expensive foods",
                                 "limit_portions"    = "Limiting portion size\nat mealtimes",
                                 "reduce_meals_days" = "Having fewer meals\nper day"))

p3 <- df.p3 %>%
  filter(variable %in% c("less_preferred", "limit_portions", "reduce_meals_days")) %>%
  filter(time_point != "T0") %>%
  ggplot(aes(x = time_point, y = prop, fill = num.days)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_vline(xintercept = c(2, 9), color = "orange", linetype = "dashed") +
  facet_wrap(vars(variable), nrow = 1, strip.position = "top", labeller = panel_names.CSI) +
  scale_fill_viridis_c(option = "mako", name = "Number of days\nper week", end = 0.95) +
  ylab("Proportion of households") + xlab(NULL) +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
p3













