library(mvord)
library(tidyverse)
library(readxl)
set.seed(42)

sample <- TRUE
sample_size <- 50000
date <- '06_06_2025'
if (sample){
  sample_suffix <- ''
} else {
  sample_suffix <- '_full'
}

other_suffix <- 'full_dataset_weighted_max4_'

csv_2016 <- paste0('coef_2016_', other_suffix, date, sample_suffix, '.csv')
csv_2022 <- paste0('coef_2022_', other_suffix, date, sample_suffix, '.csv')
txt_2016 <- paste0('mvord_hhld_2016_', other_suffix, date, sample_suffix, '.txt')
txt_2022 <- paste0('mvord_hhld_2022_', other_suffix, date, sample_suffix, '.txt')
csv_combined <- paste0('coef_combined_', other_suffix, date, sample_suffix, '.csv')
txt_combined <- paste0('mvord_hhld_combined_', other_suffix, date, sample_suffix, '.txt')
csv_combined_naive <- paste0('coef_combined_naive_', other_suffix, date, sample_suffix, '.csv')
txt_combined_naive <- paste0('mvord_hhld_combined_naive_', other_suffix, date, sample_suffix, '.txt')
csv_combined_npda <- paste0('coef_combined_npda_', other_suffix, date, sample_suffix, '.csv')
txt_combined_npda <- paste0('mvord_hhld_combined_npda_', other_suffix, date, sample_suffix, '.txt')
csv_combined_josi <- paste0('coef_combined_josi_', other_suffix, date, sample_suffix, '.csv')
txt_combined_josi <- paste0('mvord_hhld_combined_josi_', other_suffix, date, sample_suffix, '.txt')
gof_2022 <- paste0('mvord_hhld_2022_gof_', other_suffix, date, sample_suffix, '.txt')
gof_2016 <- paste0('mvord_hhld_2016_gof_', other_suffix, date, sample_suffix, '.txt')
gof_naive <- paste0('mvord_hhld_combined_naive_gof_', other_suffix, date, sample_suffix, '.txt')
gof_npda <- paste0('mvord_hhld_combined_npda_gof_', other_suffix, date, sample_suffix, '.txt')
gof_josi <- paste0('mvord_hhld_combined_josi_gof_', other_suffix, date, sample_suffix, '.txt')
gof_combined <- paste0('mvord_hhld_combined_gof_', other_suffix, date, sample_suffix, '.txt')
model_2016 <- paste0('model_2016_', other_suffix, date, sample_suffix, '.rds')
model_2022 <- paste0('model_2022_', other_suffix, date, sample_suffix, '.rds')
model_combined <- paste0('model_combined_', other_suffix, date, sample_suffix, '.rds')
model_combined_naive <- paste0('model_combined_naive_', other_suffix, date, sample_suffix, '.rds')
model_combined_npda <- paste0('model_combined_npda_', other_suffix, date, sample_suffix, '.rds')
model_combined_josi <- paste0('model_combined_josi_', other_suffix, date, sample_suffix, '.rds')


pers2022 <- read_csv('C:\\Users\\kikomwen\\Documents\\pers_2022_modified.csv')
hhlds2022 <- read_csv('C:\\Users\\kikomwen\\Documents\\TTS2022\\hhld2022v11_VM.csv')
distance_to_downtown <- read_csv('C:\\Users\\kikomwen\\Documents\\Files_to_send_to_DMG_server\\Distances_to_downtown_DMG_zones.csv')
lu_entropy <- read_csv('C:\\Users\\kikomwen\\Documents\\Entropy_byHexagons.csv')
accessibility <- read_excel('C:\\Users\\kikomwen\\Documents\\Files_to_send_to_DMG_server\\TAZ_Transport_accessibility_1971-2016.xlsx')
hhlds_2016 <- read_csv('C:\\Users\\kikomwen\\Documents\\hhld_2016_1.csv')
pers2016 <- read_csv('C:\\Users\\kikomwen\\Documents\\pers_2016_modified3.csv')
subway_station_distances <- read_csv('C:\\Users\\kikomwen\\Documents\\Files_to_send_to_DMG_server\\Distance_to_closest_subway_station.csv')
bus_station_counts <- read_csv('C:\\Users\\kikomwen\\Documents\\Files_to_send_to_DMG_server\\Bus_Stations_Per_TTS_Zone.csv')


distance_to_downtown <- distance_to_downtown %>% 
  mutate(
    HubDistParsed = parse_double(HubDist)
         ) 

distance_to_downtown <- distance_to_downtown %>% 
  mutate(
    HubDistParsed = replace_na(HubDistParsed, median(distance_to_downtown$HubDistParsed, na.rm = TRUE))
  )

ind_vars_2022 <- pers2022 %>% 
  mutate(
    emp_pd1 = if_else(pd_emp == 1, 1, 0),
    free_park_modified = if_else(free_park == 'Y', 1, 0)
         ) %>% 
  group_by(hhld_num) %>% 
  summarise(
    n_emp_pd1 = sum(emp_pd1),
    n_free_park = sum(free_park_modified), 
    entropy_hhld = mean(entropy)
  )

hhlds2022 <- hhlds2022 %>%
  inner_join(distance_to_downtown, join_by(gta06_hhld == GTA06)) %>% 
  inner_join(subway_station_distances, join_by(gta06_hhld == GTA06)) %>% 
  inner_join(bus_station_counts, join_by(gta06_hhld == GTA06)) %>% 
  inner_join(ind_vars_2022)

# I want to merge accessibility indicators with my dataset. 
# But to do that, I will first have to get a gta06 - gta01 tibble from the TTS 2016 and remove ambiguous zones (i.e. where gta06 has multiple zones for a single 01 zone)
zones_16 <- hhlds_2016 %>% select(gta01_hhld, gta06_hhld) %>% distinct()
(counts <- zones_16 %>% group_by(gta06_hhld) %>% count() %>% arrange(desc(n)))
zones_16 <- zones_16 %>% 
  inner_join(counts, join_by(gta06_hhld == gta06_hhld))
zones_16 <- zones_16 %>% 
  filter(n == 1) %>% 
  mutate(n = NULL)


# zones_22 <- hhlds2022 %>% select(gta06_hhld) %>% distinct()
hhlds2022 <- hhlds2022 %>% 
  inner_join(zones_16, join_by(gta06_hhld == gta06_hhld))
hhlds2022 <- hhlds2022 %>%
  inner_join(accessibility, join_by(gta01_hhld == TAZ_O))    # Not sure if this is right, this just generates more data after merge
# as opposed to using gta06_hhld

# Reducing size of accessibility variables (currently too large)
hhlds2022 <- hhlds2022 %>%
  mutate(
    ACAR16 = ACAR16 / 10000,
    ATRAN16 = ATRAN16 / 10000,
    JACAR16 = JACAR16 / 10000,
    JATRAN16 = JATRAN16 / 10000,
    TTDIFF16 = TTCAR16 - TTRAN16,
    ADIFF16 = ACAR16 - ATRAN16,
    JDIFF16 = JACAR16 - JATRAN16
  )

hhlds2022 <- hhlds2022 %>% 
  mutate(income_0_15 = if_else(income == 1, 1, 0), 
         income_15_39 = if_else(income == 2, 1, 0),
         income_40_59 = if_else(income == 3, 1, 0),
         income_60_99 = if_else((income == 4)|(income == 5), 1, 0),
         income_100_124 = if_else(income == 6, 1, 0),
         income_more_than_125 = if_else((income >= 7)&(income != 99), 1, 0),
         income_dont_know = if_else(income == 99, 1, 0),
         car_sufficient = case_when(
           n_vehicle == 0 ~ 0,
           n_vehicle < n_licence ~ 1, 
           n_vehicle == n_licence ~ 2, 
           n_vehicle > n_licence ~ 3
           
         ), 
         region_hhld_to = if_else(region_hhld == 1, 1, 0), 
         log_distance_downtown = if_else(HubDistParsed != 0, log(HubDistParsed), -10),
         n_with_daily_commitments = n_emp_ft - n_emp_hft + n_stu_pse,
         log_dist_nearest_subway = log(Distance_to_closest_subway_station), 
         # surveymethodcombined = as.factor(surveymethod),
         surveymethodcombined = as.factor(case_when(
           surveymethod == 1 ~ 1,
           (surveymethod == 2)|(surveymethod == 3)|(surveymethod == 6) ~ 2,
           (surveymethod == 4)|(surveymethod == 5) ~ 3
         )),
         ones = 1, 
         rndom = runif(n=nrow(hhlds2022), min=0, max=1),
         n_vehicle_restricted = if_else(n_vehicle > 4, 4, n_vehicle),
         n_tranpass_restricted = if_else(n_tranpass > 4, 4, n_tranpass)
         # n_tranpass = if_else(n_tranpass <= 3, n_tranpass, 3), 
         # n_vehicle = if_else(n_vehicle <= 3, n_vehicle, 3), 
         # n_tranpass = factor(n_tranpass, ordered = TRUE), 
         # n_vehicle = factor(n_vehicle, ordered = TRUE)
         ) %>% 
  filter((!(n_vehicle == 99))&(!(n_licence == 99)))

hhlds2022 <- hhlds2022 %>% 
  replace_na(list(n_vehicle = 0, n_tranpass = 0))

hhlds2022 %>% 
  select(n_vehicle) %>% 
  group_by(n_vehicle) %>% count()

hhlds2022 %>% 
  select(n_tranpass) %>% 
  group_by(n_tranpass) %>% count()


hhlds2022 <- hhlds2022 %>% 
  inner_join(lu_entropy, join_by(gta06_hhld == GTA06))

# # Only running a sample for now because the code takes too long to run
if (sample) {
  hhlds2022 <- sample_n(hhlds2022, size = sample_size)
}


# Initial MVORD model attempt
mvord_2022 <- mvord(formula = MMO2(n_vehicle_restricted, n_tranpass_restricted) ~ income_15_39 + income_40_59 + 
                      income_60_99 + income_100_124 + income_more_than_125 + income_dont_know + region_hhld_to + n_free_park
                    + log_distance_downtown + TTDIFF16 + n_with_daily_commitments + log_dist_nearest_subway + Bus_Stations_per_Zone
                    + n_emp_pd1 + entropy_hhld + n_licence + n_stu_k12 + surveymethodcombined, data = hhlds2022, error.structure = cov_general(formula = ~ 1), weights.name = 'expf')
# mvord_cars <- mvord(formula = MMO2(n_vehicle, n_tranpass, n_licence) ~ income, data = hhlds2022)
summary(mvord_2022)


# Storing results
sink(txt_2022)
# sink('mvord_hhld_2022_fulldataset.txt')
print(summary(mvord_2022))
sink()

coef_2022 <- as.data.frame(summary(mvord_2022)$coefficients)
# write.csv(coef_2022, 'coef_2022.csv', row.names = TRUE)
write.csv(coef_2022, csv_2022, row.names = TRUE)

saveRDS(mvord_2022, file = model_2022)


## 2016 Model
stu_hft_num <- pers2016 %>% 
  mutate(
    stu_pse = if_else(((stu_stat == 'S')|((stu_stat == 'P')))&(age >= 18), 1, 0),
    emp_hft = if_else(emp_stat == 'H', 1, 0),
    metropass = if_else(tran_pass == 'M', 1, 0), 
    emp_pd1 = if_else(pd_emp == '1', 1, 0),
    free_park_modified = if_else(free_park == 'Y', 1, 0),
    stu_k12 = if_else(age < 18, 1, 0)
  ) %>% 
  group_by(hhld_num) %>% 
  summarise(
    n_stu_pse = sum(stu_pse),
    n_emp_hft = sum(emp_hft),
    n_tranpass = sum(metropass), 
    n_emp_pd1 = sum(emp_pd1), 
    n_free_park = sum(free_park_modified), 
    entropy_hhld = mean(entropy),
    n_stu_k12 = sum(stu_k12)
            )

hhlds2016 <- hhlds_2016 %>% 
  filter((!(n_vehicle == 99))&(!(n_licence == 99))&(!(surveymode == 9))) %>% 
  inner_join(distance_to_downtown, join_by(gta06_hhld == GTA06)) %>% 
  inner_join(subway_station_distances, join_by(gta06_hhld == GTA06)) %>% 
  inner_join(bus_station_counts, join_by(gta06_hhld == GTA06)) %>% 
  inner_join(accessibility, join_by(gta01_hhld == TAZ_O)) %>%
  inner_join(stu_hft_num, join_by(hhld_num == hhld_num)) %>% 
  inner_join(lu_entropy, join_by(gta06_hhld == GTA06)) %>% 
  mutate(
    ACAR16 = ACAR16 / 10000,
    ATRAN16 = ATRAN16 / 10000,
    JACAR16 = JACAR16 / 10000,
    JATRAN16 = JATRAN16 / 10000,
    TTDIFF16 = TTCAR16 - TTRAN16,
    ADIFF16 = ACAR16 - ATRAN16,
    JDIFF16 = JACAR16 - JATRAN16
  ) %>% 
  mutate(income_0_15 = if_else(income == 1, 1, 0), 
         income_15_39 = if_else(income == 2, 1, 0),
         income_40_59 = if_else(income == 3, 1, 0),
         income_60_99 = if_else(income == 4, 1, 0),
         income_100_124 = if_else(income == 5, 1, 0),
         income_more_than_125 = if_else(income == 6, 1, 0),
         income_dont_know = if_else(income == 7, 1, 0),
         car_sufficient = case_when(
           n_vehicle == 0 ~ 0,
           n_vehicle < n_licence ~ 1, 
           n_vehicle == n_licence ~ 2, 
           n_vehicle > n_licence ~ 3
           
         ), 
         region_hhld_to = if_else(region_hhld == 1, 1, 0), 
         log_distance_downtown = if_else(HubDistParsed != 0, log(HubDistParsed), -10),
         n_with_daily_commitments = n_emp_ft + n_stu_pse - n_emp_hft,
         log_dist_nearest_subway = log(Distance_to_closest_subway_station), 
         surveymethodcombined = as.factor(
           case_when(
             surveymode == 1 ~ 2,
             surveymode == 2 ~ 1,
             (surveymode == 3)|(surveymode == 4) ~ 3
           )
         ),
         ones = 1,
         n_vehicle_restricted = if_else(n_vehicle > 4, 4, n_vehicle),
         n_tranpass_restricted = if_else(n_tranpass > 4, 4, n_tranpass)
         # n_tranpass = if_else(n_tranpass <= 3, n_tranpass, 3), 
         # n_vehicle = if_else(n_vehicle <= 3, n_vehicle, 3), 
         # n_tranpass = factor(n_tranpass, ordered = TRUE), 
         # n_vehicle = factor(n_vehicle, ordered = TRUE)
  ) %>% 
  replace_na(list(n_vehicle = 0, n_tranpass = 0))

hhlds2016 <- hhlds2016 %>% 
  mutate(
    rndom = runif(n=nrow(hhlds2016), min=0, max=1)
  )
  
# # Have to copy levels of survey methods factor, otherwise transferability code won't work
# new_levels_survey_method <- levels(hhlds2022$surveymethodcombined)
# hhlds2016$surveymethodcombined <- factor(hhlds2016$surveymethodcombined, levels = new_levels_survey_method)

# Only running a sample for now because the code takes too long to run
if (sample) {
  hhlds2016 <- sample_n(hhlds2016, size = sample_size)
}

# Initial MVORD model attempt
mvord_2016 <- mvord(formula = MMO2(n_vehicle_restricted, n_tranpass_restricted) ~ income_15_39 + income_40_59 + 
                      income_60_99 + income_100_124 + income_more_than_125 + income_dont_know + region_hhld_to + n_free_park
                    + log_distance_downtown + TTDIFF16 + n_with_daily_commitments + log_dist_nearest_subway + Bus_Stations_per_Zone 
                    + n_emp_pd1 + entropy_hhld + n_licence + n_stu_k12 + surveymethodcombined, data = hhlds2016, error.structure = cov_general(formula = ~ 1), weights.name = 'expf')
summary(mvord_2016)

# Storing results
# sink('mvord_hhld_2016.txt')
sink(txt_2016)
print(summary(mvord_2016))
sink()

coef_2016 <- as.data.frame(summary(mvord_2016)$coefficients)
# write.csv(coef_2016, 'coef_2016.csv', row.names = TRUE)
write.csv(coef_2016, csv_2016, row.names = TRUE)

saveRDS(mvord_2016, file = model_2016)



###############################
### Pooled Model ###############
###############################
hhld2016combine <- hhlds2016 %>% select(n_vehicle_restricted, n_tranpass_restricted,
  income_15_39, income_40_59, 
  income_60_99, income_100_124, income_more_than_125, income_dont_know, region_hhld_to, n_free_park,
  log_distance_downtown, TTDIFF16, n_with_daily_commitments, log_dist_nearest_subway, Bus_Stations_per_Zone, 
  n_emp_pd1, entropy_hhld, n_licence, n_stu_k12, surveymethodcombined, rndom, expf
) %>% 
  mutate(
   year = 1
  )

hhlds2022combine <- hhlds2022 %>% select(n_vehicle_restricted, n_tranpass_restricted,
  income_15_39, income_40_59, 
  income_60_99, income_100_124, income_more_than_125, income_dont_know, region_hhld_to, n_free_park,
  log_distance_downtown, TTDIFF16, n_with_daily_commitments, log_dist_nearest_subway, Bus_Stations_per_Zone, 
  n_emp_pd1, entropy_hhld, n_licence, n_stu_k12, surveymethodcombined, rndom, expf
) %>% 
  mutate(
    year = 2
  )

hhldscombined <- rbind(hhld2016combine, hhlds2022combine) %>% 
  mutate(
    year = as.factor(year)
  )


# Combined mvord
mvord_combined <- mvord(formula = MMO2(n_vehicle_restricted, n_tranpass_restricted) ~ income_15_39 + income_40_59 + 
                      income_60_99 + income_100_124 + income_more_than_125 + income_dont_know + region_hhld_to + n_free_park
                    + log_distance_downtown + TTDIFF16 + n_with_daily_commitments + log_dist_nearest_subway + Bus_Stations_per_Zone 
                    + n_emp_pd1 + entropy_hhld + n_licence + n_stu_k12 + year + surveymethodcombined, data = hhldscombined, error.structure = cov_general(formula = ~ 1 + year), weights.name = 'expf')
summary(mvord_combined)

# Storing results
sink(txt_combined)
print(summary(mvord_combined))
sink()

coef_combined <- as.data.frame(summary(mvord_combined)$coefficients)
write.csv(coef_combined, csv_combined, row.names = TRUE)

saveRDS(mvord_combined, file = model_combined)


# Naive combined model
mvord_combined_naive <- mvord(formula = MMO2(n_vehicle_restricted, n_tranpass_restricted) ~ income_15_39 + income_40_59 + 
                          income_60_99 + income_100_124 + income_more_than_125 + income_dont_know + region_hhld_to + n_free_park
                        + log_distance_downtown + TTDIFF16 + n_with_daily_commitments + log_dist_nearest_subway + Bus_Stations_per_Zone 
                        + n_emp_pd1 + entropy_hhld + n_licence + n_stu_k12 + surveymethodcombined, data = hhldscombined, error.structure = cov_general(formula = ~ 1), weights.name = 'expf')
summary(mvord_combined_naive)

# Storing results
sink(txt_combined_naive)
print(summary(mvord_combined_naive))
sink()

coef_combined_naive <- as.data.frame(summary(mvord_combined_naive)$coefficients)
write.csv(coef_combined_naive, csv_combined_naive, row.names = TRUE)

saveRDS(mvord_combined_naive, file = model_combined_naive)


# Equivalent to Gozde's NPDA
mvord_combined_npda <- mvord(formula = MMO2(n_vehicle_restricted, n_tranpass_restricted) ~ income_15_39 + income_40_59 + 
                               income_60_99 + income_100_124 + income_more_than_125 + income_dont_know + region_hhld_to + n_free_park
                             + log_distance_downtown + TTDIFF16 + n_with_daily_commitments + log_dist_nearest_subway + Bus_Stations_per_Zone 
                             + n_emp_pd1 + entropy_hhld + n_licence + n_stu_k12 + year + surveymethodcombined, data = hhldscombined, error.structure = cov_general(formula = ~ 1), weights.name = 'expf')
summary(mvord_combined_npda)

# Storing results
sink(txt_combined_npda)
print(summary(mvord_combined_npda))
sink()

coef_combined_npda <- as.data.frame(summary(mvord_combined_npda)$coefficients)
write.csv(coef_combined_npda, csv_combined_npda, row.names = TRUE)

saveRDS(mvord_combined_npda, file = model_combined_npda)


# Equivalent to Gozde's JOSI
mvord_combined_josi <- mvord(formula = MMO2(n_vehicle_restricted, n_tranpass_restricted) ~ income_15_39 + income_40_59 + 
                               income_60_99 + income_100_124 + income_more_than_125 + income_dont_know + region_hhld_to + n_free_park
                             + log_distance_downtown + TTDIFF16 + n_with_daily_commitments + log_dist_nearest_subway + Bus_Stations_per_Zone 
                             + n_emp_pd1 + entropy_hhld + n_licence + n_stu_k12 + surveymethodcombined, data = hhldscombined, error.structure = cov_general(formula = ~ 1 + year), weights.name = 'expf')
summary(mvord_combined_josi)

# Storing results
sink(txt_combined_josi)
print(summary(mvord_combined_josi))
sink()

coef_combined_josi <- as.data.frame(summary(mvord_combined_josi)$coefficients)
write.csv(coef_combined_josi, csv_combined_josi, row.names = TRUE)

saveRDS(mvord_combined_josi, file = model_combined_josi)

# ########################
# ### GoF ################
# ########################
# mvord_gof_2022 <- mvord(formula = MMO2(n_vehicle_restricted, n_tranpass_restricted) ~ rndom, data = hhlds2022, error.structure = cov_general(formula = ~ 1))
# summary(mvord_gof_2022)
# 
# 
# # Storing results
# sink(gof_2022)
# print(summary(mvord_gof_2022))
# sink()
# 
# 
# mvord_gof_2016 <- mvord(formula = MMO2(n_vehicle_restricted, n_tranpass_restricted) ~ rndom, data = hhlds2016, error.structure = cov_general(formula = ~ 1))
# summary(mvord_gof_2016)
# 
# # Storing results
# sink(gof_2016)
# print(summary(mvord_gof_2016))
# sink()
# 
# mvord_gof_combined <- mvord(formula = MMO2(n_vehicle_restricted, n_tranpass_restricted) ~ rndom + year, data = hhldscombined, error.structure = cov_general(formula = ~ 1 + year))
# summary(mvord_gof_combined)
# 
# # Storing results
# sink(gof_combined)
# print(summary(mvord_gof_combined))
# sink()
# 
# 
# mvord_gof_combined_npda <- mvord(formula = MMO2(n_vehicle_restricted, n_tranpass_restricted) ~ rndom + year, data = hhldscombined, error.structure = cov_general(formula = ~ 1))
# summary(mvord_gof_combined_npda)
# 
# # Storing results
# sink(gof_npda)
# print(summary(mvord_gof_combined_npda))
# sink()
# 
# 
# mvord_gof_combined_josi <- mvord(formula = MMO2(n_vehicle_restricted, n_tranpass_restricted) ~ rndom, data = hhldscombined, error.structure = cov_general(formula = ~ 1 + year))
# summary(mvord_gof_combined_josi)
# 
# # Storing results
# sink(gof_josi)
# print(summary(mvord_gof_combined_josi))
# sink()
# 
