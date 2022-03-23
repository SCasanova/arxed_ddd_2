library(tidyverse)
library(rvest)

# temp <- read.csv('district_info.csv') %>% 
#   dplyr::select(district_name:county) %>% 
#   dplyr::filter(!is.na(district_code))
# 
# 
# for(i in 1:dim(temp)[1]){
#       query <- paste0("REPLACE INTO district_info VALUES (", 
#                       '"', temp[i,1] ,'",',
#                       temp[i,2] ,',',
#                       '"', temp[i,3] ,'",',
#                       '"', temp[i,4] ,'",',
#                       '"', temp[i,5] ,'");')
#       RMySQL::dbSendQuery(myDB,query)
#     }


# Student Needs -----------------------------------------------------------

needs_2022 <- readxl::read_xlsx('raw_data/selectedpopulations(7).xlsx') %>%
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  dplyr::mutate(year = '2021-22')

needs_2021 <- readxl::read_xlsx('raw_data/selectedpopulations.xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2020-21')

needs_2020 <- readxl::read_xlsx('raw_data/selectedpopulations(1).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2019-20')

needs_2019 <- readxl::read_xlsx('raw_data/selectedpopulations(2).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  mutate(year = '2018-19')

needs_2018 <- readxl::read_xlsx('raw_data/selectedpopulations(3).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2017-18')

needs_2017 <- readxl::read_xlsx('raw_data/selectedpopulations(4).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2016-17')

needs_2016 <- readxl::read_xlsx('raw_data/selectedpopulations(5).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2015-16')


student_needs <- bind_rows(needs_2021,needs_2022, needs_2020, needs_2019, 
                           needs_2018, needs_2017, needs_2016) %>% 
  mutate(district_code = formatC(district_code, width = 8, format = "d", flag = "0"))

# write_csv(student_needs, 'student_needs.csv')

# Student Diversity -------------------------------------------------------




diversity_2021 <- readxl::read_xlsx('raw_data/ClassSizebyRaceEthnicity.xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2020-21')

diversity_2020 <- readxl::read_xlsx('raw_data/ClassSizebyRaceEthnicity(1).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2019-20')

diversity_2019 <- readxl::read_xlsx('raw_data/ClassSizebyRaceEthnicity(2).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  mutate(year = '2018-19')

diversity_2018 <- readxl::read_xlsx('raw_data/ClassSizebyRaceEthnicity(3).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2017-18')

diversity_2017 <- readxl::read_xlsx('raw_data/ClassSizebyRaceEthnicity(4).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2016-17')

diversity_2016 <- readxl::read_xlsx('raw_data/ClassSizebyRaceEthnicity(5).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2015-16')


student_diversity <- dplyr::bind_rows(diversity_2021, diversity_2020, diversity_2019, 
                           diversity_2018, diversity_2017, diversity_2016) %>% 
  mutate(district_code = formatC(district_code, width = 8, format = "d", flag = "0"))

# readr::write_csv(student_diversity, 'student_diversity.csv')


# Student Mobility --------------------------------------------------------


mobility_2021 <- readxl::read_xlsx('raw_data/mobilityrates.xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2020-21')

mobility_2020 <- readxl::read_xlsx('raw_data/mobilityrates(1).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2019-20')

mobility_2019 <- readxl::read_xlsx('raw_data/mobilityrates(2).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  mutate(year = '2018-19')

mobility_2018 <- readxl::read_xlsx('raw_data/mobilityrates(3).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2017-18')

mobility_2017 <- readxl::read_xlsx('raw_data/mobilityrates(4).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2016-17')

mobility_2016 <- readxl::read_xlsx('raw_data/mobilityrates(5).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2015-16')


student_mobility <- bind_rows(mobility_2021, mobility_2020, mobility_2019, 
                           mobility_2018, mobility_2017, mobility_2016)%>% 
  mutate(district_code = formatC(district_code, width = 8, format = "d", flag = "0"))


# write_csv(student_mobility, 'student_mobility.csv')


# Staff Diversity ---------------------------------------------------------

diversity_2021 <- readxl::read_xlsx('raw_data/staffracegender(2).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2020-21')

diversity_2020 <- readxl::read_xlsx('raw_data/staffracegender(3).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2019-20')

diversity_2019 <- readxl::read_xlsx('raw_data/staffracegender(4).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  mutate(year = '2018-19')

diversity_2018 <- readxl::read_xlsx('raw_data/staffracegender(5).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2017-18')

diversity_2017 <- readxl::read_xlsx('raw_data/staffracegender(6).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2016-17')

diversity_2016 <- readxl::read_xlsx('raw_data/staffracegender(7).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(year = '2015-16')


staff_diversity <- dplyr::bind_rows(diversity_2021, diversity_2020, diversity_2019, 
                           diversity_2018, diversity_2017, diversity_2016)%>% 
  mutate(district_school_code = formatC(district_school_code, width = 8, format = "d", flag = "0"))


# readr::write_csv(staff_diversity, 'staff_diversity.csv')


# MCAs Data ---------------------------------------------------------------

mcas_2021 <- readxl::read_xlsx('raw_data/NextGenMCAS.xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  select(district_name, district_code, subject, m_e_number:no_of_students_included) %>% 
  pivot_wider(values_from =  m_e_number:no_of_students_included, names_from = subject) %>% 
   mutate(year = '2020-21')

mcas_2019 <- readxl::read_xlsx('raw_data/NextGenMCAS(1).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  select(district_name, district_code, subject, m_e_number:no_of_students_included) %>% 
  pivot_wider(values_from =  m_e_number:no_of_students_included, names_from = subject) %>% 
  mutate(year = '2018-19')


mcas_2019_sci <-  readxl::read_xlsx('raw_data/mcas(1).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  select(district_name, district_code, subject, p_a_number:student_included) %>% 
  mutate(subject = 'SCI',
         year = '2018-19')

mcas_2018 <- readxl::read_xlsx('raw_data/mcas(2).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(subject = case_when(subject == 'ENGLISH LANGUAGE ARTS' ~ 'ela',
                             subject == 'MATHEMATICS' ~ 'mth',
                             subject == 'SCIENCE AND TECH/ENG' ~ 'sci')) %>% 
  select(district_name, district_code, subject, p_a_number:student_included) %>% 
  pivot_wider(values_from =  p_a_number:student_included, names_from = subject) %>% 
  mutate(year = '2017-18')

mcas_2017 <- readxl::read_xlsx('raw_data/mcas(3).xlsx') %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()%>% 
  mutate(subject = case_when(subject == 'ENGLISH LANGUAGE ARTS' ~ 'ela',
                             subject == 'MATHEMATICS' ~ 'mth',
                             subject == 'SCIENCE AND TECH/ENG' ~ 'sci')) %>% 
  select(district_name, district_code, subject, p_a_number:student_included) %>% 
  pivot_wider(values_from =  p_a_number:student_included, names_from = subject) %>% 
  mutate(year = '2016-17')



new_school_info <- read_csv('school_info_base.csv') %>%
  mutate(district_code = formatC(district_code, width = 8, format = "d", flag = "0")) %>%
  left_join(mcas_2021 %>% select(-district_name), by = c('district_code', 'year')) %>%
  mutate(ela_advanced_proficient_pct = ifelse(is.na(ela_advanced_proficient_pct), m_e_percent_ELA,ela_advanced_proficient_pct),
         ela_advanced_proficient_number = ifelse(is.na(ela_advanced_proficient_number), m_e_number_ELA,ela_advanced_proficient_number),
         ela_advanced_pct = ifelse(is.na(ela_advanced_pct), m_percent_ELA,ela_advanced_pct),
         ela_advanced_number = ifelse(is.na(ela_advanced_number), m_number_ELA,ela_advanced_number),
         ela_proficient_pct = ifelse(is.na(ela_proficient_pct), e_percent_ELA,ela_proficient_pct),
         ela_proficient_number = ifelse(is.na(ela_proficient_number), e_number_ELA,ela_proficient_number),
         ela_needs_imp_pct = ifelse(is.na(ela_needs_imp_pct), pm_percent_ELA,ela_needs_imp_pct),
         ela_needs_imp_number = ifelse(is.na(ela_needs_imp_number), pm_number_ELA,ela_needs_imp_number),
         ela_warning_failing_pct = ifelse(is.na(ela_warning_failing_pct), nm_percent_ELA,ela_warning_failing_pct),
         ela_warning_failing_number = ifelse(is.na(ela_warning_failing_number), nm_number_ELA,ela_warning_failing_number),
         mth_advanced_proficient_number = ifelse(is.na(mth_advanced_proficient_number), m_e_number_MATH,mth_advanced_proficient_number),
         mth_advanced_proficient_pct = ifelse(is.na(mth_advanced_proficient_pct), m_e_percent_MATH,mth_advanced_proficient_pct),
         mth_advanced_pct = ifelse(is.na(mth_advanced_pct), m_percent_MATH,mth_advanced_pct),
         mth_advanced_number = ifelse(is.na(mth_advanced_number), m_number_MATH,mth_advanced_number),
         mth_proficient_pct = ifelse(is.na(mth_proficient_pct), e_percent_MATH,mth_proficient_pct),
         mth_proficient_number = ifelse(is.na(mth_proficient_number), e_number_MATH,mth_proficient_number),
         mth_needs_imp_pct = ifelse(is.na(mth_needs_imp_pct), pm_percent_MATH,mth_needs_imp_pct),
         mth_needs_imp_number = ifelse(is.na(mth_needs_imp_number), pm_number_MATH,mth_needs_imp_number),
         mth_warning_failing_pct = ifelse(is.na(mth_warning_failing_pct), nm_percent_MATH,mth_warning_failing_pct),
         mth_warning_failing_number = ifelse(is.na(mth_warning_failing_number), nm_number_MATH,mth_warning_failing_number),
         mth_student_included = ifelse(is.na(mth_student_included), no_of_students_included_MATH,mth_student_included),
         ela_student_included = ifelse(is.na(ela_student_included), no_of_students_included_ELA,ela_student_included)
         ) %>%
  select(-c(m_e_number_ELA:no_of_students_included_MATH)) %>%
  left_join(mcas_2019 %>% select(-district_name), by = c('district_code', 'year')) %>%
  mutate(ela_advanced_proficient_pct = ifelse(is.na(ela_advanced_proficient_pct), m_e_percent_ELA,ela_advanced_proficient_pct),
         ela_advanced_proficient_number = ifelse(is.na(ela_advanced_proficient_number), m_e_number_ELA,ela_advanced_proficient_number),
         ela_advanced_pct = ifelse(is.na(ela_advanced_pct), m_percent_ELA,ela_advanced_pct),
         ela_advanced_number = ifelse(is.na(ela_advanced_number), m_number_ELA,ela_advanced_number),
         ela_proficient_pct = ifelse(is.na(ela_proficient_pct), e_percent_ELA,ela_proficient_pct),
         ela_proficient_number = ifelse(is.na(ela_proficient_number), e_number_ELA,ela_proficient_number),
         ela_needs_imp_pct = ifelse(is.na(ela_needs_imp_pct), pm_percent_ELA,ela_needs_imp_pct),
         ela_needs_imp_number = ifelse(is.na(ela_needs_imp_number), pm_number_ELA,ela_needs_imp_number),
         ela_warning_failing_pct = ifelse(is.na(ela_warning_failing_pct), nm_percent_ELA,ela_warning_failing_pct),
         ela_warning_failing_number = ifelse(is.na(ela_warning_failing_number), nm_number_ELA,ela_warning_failing_number),
         mth_advanced_proficient_number = ifelse(is.na(mth_advanced_proficient_number), m_e_number_MATH,mth_advanced_proficient_number),
         mth_advanced_proficient_pct = ifelse(is.na(mth_advanced_proficient_pct), m_e_percent_MATH,mth_advanced_proficient_pct),
         mth_advanced_pct = ifelse(is.na(mth_advanced_pct), m_percent_MATH,mth_advanced_pct),
         mth_advanced_number = ifelse(is.na(mth_advanced_number), m_number_MATH,mth_advanced_number),
         mth_proficient_pct = ifelse(is.na(mth_proficient_pct), e_percent_MATH,mth_proficient_pct),
         mth_proficient_number = ifelse(is.na(mth_proficient_number), e_number_MATH,mth_proficient_number),
         mth_needs_imp_pct = ifelse(is.na(mth_needs_imp_pct), pm_percent_MATH,mth_needs_imp_pct),
         mth_needs_imp_number = ifelse(is.na(mth_needs_imp_number), pm_number_MATH,mth_needs_imp_number),
         mth_warning_failing_pct = ifelse(is.na(mth_warning_failing_pct), nm_percent_MATH,mth_warning_failing_pct),
         mth_warning_failing_number = ifelse(is.na(mth_warning_failing_number), nm_number_MATH,mth_warning_failing_number),
         mth_student_included = ifelse(is.na(mth_student_included), no_of_students_included_MATH,mth_student_included),
         ela_student_included = ifelse(is.na(ela_student_included), no_of_students_included_ELA,ela_student_included)
         ) %>%
  select(-c(m_e_number_ELA:no_of_students_included_MATH)) %>%
  left_join(mcas_2019_sci%>% select(-district_name), by = c('district_code', 'year')) %>%
   mutate(sci_advanced_proficient_pct = ifelse(is.na(sci_advanced_proficient_pct), p_a_percent,sci_advanced_proficient_pct),
         sci_advanced_proficient_number = ifelse(is.na(sci_advanced_proficient_number), p_a_number,sci_advanced_proficient_number),
         sci_advanced_pct = ifelse(is.na(sci_advanced_pct), p_percent,sci_advanced_pct),
         sci_advanced_number = ifelse(is.na(sci_advanced_number), p_number,sci_advanced_number),
         sci_proficient_pct = ifelse(is.na(sci_proficient_pct), a_percent,sci_proficient_pct),
         sci_proficient_number = ifelse(is.na(sci_proficient_number), a_number,sci_proficient_number),
         sci_needs_imp_pct = ifelse(is.na(sci_needs_imp_pct), ni_percent,sci_needs_imp_pct),
         sci_needs_imp_number = ifelse(is.na(sci_needs_imp_number), ni_number,sci_needs_imp_number),
         sci_warning_failing_pct = ifelse(is.na(sci_warning_failing_pct), w_f_percent,sci_warning_failing_pct),
         sci_warning_failing_number = ifelse(is.na(sci_warning_failing_number), w_f_number,sci_warning_failing_number),
         sci_student_included = ifelse(is.na(sci_student_included), student_included,sci_student_included)) %>%
  select(-c(subject:student_included)) %>%
  left_join(mcas_2018 %>% select(-district_name), by = c('district_code', 'year')) %>%
  mutate(ela_advanced_proficient_pct = ifelse(is.na(ela_advanced_proficient_pct), p_a_percent_ela,ela_advanced_proficient_pct),
         ela_advanced_proficient_number = ifelse(is.na(ela_advanced_proficient_number), p_a_number_ela,ela_advanced_proficient_number),
         ela_advanced_pct = ifelse(is.na(ela_advanced_pct), p_percent_ela,ela_advanced_pct),
         ela_advanced_number = ifelse(is.na(ela_advanced_number), p_number_ela,ela_advanced_number),
         ela_proficient_pct = ifelse(is.na(ela_proficient_pct), a_percent_ela,ela_proficient_pct),
         ela_proficient_number = ifelse(is.na(ela_proficient_number), a_number_ela,ela_proficient_number),
         ela_needs_imp_pct = ifelse(is.na(ela_needs_imp_pct), ni_percent_ela,ela_needs_imp_pct),
         ela_needs_imp_number = ifelse(is.na(ela_needs_imp_number), ni_number_ela,ela_needs_imp_number),
         ela_warning_failing_pct = ifelse(is.na(ela_warning_failing_pct), w_f_percent_ela,ela_warning_failing_pct),
         ela_warning_failing_number = ifelse(is.na(ela_warning_failing_number), w_f_number_ela,ela_warning_failing_number),
         mth_advanced_proficient_number = ifelse(is.na(mth_advanced_proficient_number), p_a_number_mth,mth_advanced_proficient_number),
         mth_advanced_proficient_pct = ifelse(is.na(mth_advanced_proficient_pct), p_a_percent_mth,mth_advanced_proficient_pct),
         mth_advanced_pct = ifelse(is.na(mth_advanced_pct), p_percent_mth,mth_advanced_pct),
         mth_advanced_number = ifelse(is.na(mth_advanced_number), p_number_mth,mth_advanced_number),
         mth_proficient_pct = ifelse(is.na(mth_proficient_pct), a_percent_mth,mth_proficient_pct),
         mth_proficient_number = ifelse(is.na(mth_proficient_number), a_number_mth,mth_proficient_number),
         mth_needs_imp_pct = ifelse(is.na(mth_needs_imp_pct), ni_percent_mth,mth_needs_imp_pct),
         mth_needs_imp_number = ifelse(is.na(mth_needs_imp_number), ni_number_mth,mth_needs_imp_number),
         mth_warning_failing_pct = ifelse(is.na(mth_warning_failing_pct), w_f_percent_mth,mth_warning_failing_pct),
         mth_warning_failing_number = ifelse(is.na(mth_warning_failing_number), w_f_number_mth,mth_warning_failing_number),
         mth_student_included = ifelse(is.na(mth_student_included), student_included_mth,mth_student_included),
         ela_student_included = ifelse(is.na(ela_student_included), student_included_ela,ela_student_included),
         sci_advanced_proficient_number = ifelse(is.na(sci_advanced_proficient_number), p_a_number_sci,sci_advanced_proficient_number),
         sci_advanced_proficient_pct = ifelse(is.na(sci_advanced_proficient_pct), p_a_percent_sci,sci_advanced_proficient_pct),
         sci_advanced_pct = ifelse(is.na(sci_advanced_pct), p_percent_sci,sci_advanced_pct),
         sci_advanced_number = ifelse(is.na(sci_advanced_number), p_number_sci,sci_advanced_number),
         sci_proficient_pct = ifelse(is.na(sci_proficient_pct), a_percent_sci,sci_proficient_pct),
         sci_proficient_number = ifelse(is.na(sci_proficient_number), a_number_sci,sci_proficient_number),
         sci_needs_imp_pct = ifelse(is.na(sci_needs_imp_pct), ni_percent_sci,sci_needs_imp_pct),
         sci_needs_imp_number = ifelse(is.na(sci_needs_imp_number), ni_number_sci,sci_needs_imp_number),
         sci_warning_failing_pct = ifelse(is.na(sci_warning_failing_pct), w_f_percent_sci,sci_warning_failing_pct),
         sci_warning_failing_number = ifelse(is.na(sci_warning_failing_number), w_f_number_sci,sci_warning_failing_number),
         sci_student_included = ifelse(is.na(sci_student_included), student_included_sci,sci_student_included)
         ) %>%
  select(-c(p_a_number_ela:student_included_sci)) %>%
  left_join(mcas_2017 %>% select(-district_name), by = c('district_code', 'year')) %>%
  mutate(ela_advanced_proficient_pct = ifelse(is.na(ela_advanced_proficient_pct), p_a_percent_ela,ela_advanced_proficient_pct),
         ela_advanced_proficient_number = ifelse(is.na(ela_advanced_proficient_number), p_a_number_ela,ela_advanced_proficient_number),
         ela_advanced_pct = ifelse(is.na(ela_advanced_pct), p_percent_ela,ela_advanced_pct),
         ela_advanced_number = ifelse(is.na(ela_advanced_number), p_number_ela,ela_advanced_number),
         ela_proficient_pct = ifelse(is.na(ela_proficient_pct), a_percent_ela,ela_proficient_pct),
         ela_proficient_number = ifelse(is.na(ela_proficient_number), a_number_ela,ela_proficient_number),
         ela_needs_imp_pct = ifelse(is.na(ela_needs_imp_pct), ni_percent_ela,ela_needs_imp_pct),
         ela_needs_imp_number = ifelse(is.na(ela_needs_imp_number), ni_number_ela,ela_needs_imp_number),
         ela_warning_failing_pct = ifelse(is.na(ela_warning_failing_pct), w_f_percent_ela,ela_warning_failing_pct),
         ela_warning_failing_number = ifelse(is.na(ela_warning_failing_number), w_f_number_ela,ela_warning_failing_number),
         mth_advanced_proficient_number = ifelse(is.na(mth_advanced_proficient_number), p_a_number_mth,mth_advanced_proficient_number),
         mth_advanced_proficient_pct = ifelse(is.na(mth_advanced_proficient_pct), p_a_percent_mth,mth_advanced_proficient_pct),
         mth_advanced_pct = ifelse(is.na(mth_advanced_pct), p_percent_mth,mth_advanced_pct),
         mth_advanced_number = ifelse(is.na(mth_advanced_number), p_number_mth,mth_advanced_number),
         mth_proficient_pct = ifelse(is.na(mth_proficient_pct), a_percent_mth,mth_proficient_pct),
         mth_proficient_number = ifelse(is.na(mth_proficient_number), a_number_mth,mth_proficient_number),
         mth_needs_imp_pct = ifelse(is.na(mth_needs_imp_pct), ni_percent_mth,mth_needs_imp_pct),
         mth_needs_imp_number = ifelse(is.na(mth_needs_imp_number), ni_number_mth,mth_needs_imp_number),
         mth_warning_failing_pct = ifelse(is.na(mth_warning_failing_pct), w_f_percent_mth,mth_warning_failing_pct),
         mth_warning_failing_number = ifelse(is.na(mth_warning_failing_number), w_f_number_mth,mth_warning_failing_number),
         mth_student_included = ifelse(is.na(mth_student_included), student_included_mth,mth_student_included),
         ela_student_included = ifelse(is.na(ela_student_included), student_included_ela,ela_student_included),
         sci_advanced_proficient_number = ifelse(is.na(sci_advanced_proficient_number), p_a_number_sci,sci_advanced_proficient_number),
         sci_advanced_proficient_pct = ifelse(is.na(sci_advanced_proficient_pct), p_a_percent_sci,sci_advanced_proficient_pct),
         sci_advanced_pct = ifelse(is.na(sci_advanced_pct), p_percent_sci,sci_advanced_pct),
         sci_advanced_number = ifelse(is.na(sci_advanced_number), p_number_sci,sci_advanced_number),
         sci_proficient_pct = ifelse(is.na(sci_proficient_pct), a_percent_sci,sci_proficient_pct),
         sci_proficient_number = ifelse(is.na(sci_proficient_number), a_number_sci,sci_proficient_number),
         sci_needs_imp_pct = ifelse(is.na(sci_needs_imp_pct), ni_percent_sci,sci_needs_imp_pct),
         sci_needs_imp_number = ifelse(is.na(sci_needs_imp_number), ni_number_sci,sci_needs_imp_number),
         sci_warning_failing_pct = ifelse(is.na(sci_warning_failing_pct), w_f_percent_sci,sci_warning_failing_pct),
         sci_warning_failing_number = ifelse(is.na(sci_warning_failing_number), w_f_number_sci,sci_warning_failing_number),
         sci_student_included = ifelse(is.na(sci_student_included), student_included_sci,sci_student_included)
         ) %>%
  select(-c(p_a_number_ela:student_included_sci))


write_csv(new_school_info, 'school_info.csv')  

new_school_info <- read_csv('school_info.csv') %>%
  mutate(district_code = formatC(district_code, width = 8, format = "d", flag = "0")) %>%
  left_join(student_mobility %>% select(-district_name), by = c('district_code', 'year')) %>%
  left_join(student_diversity %>% select(-district_name), by = c('district_code', 'year')) %>%
  left_join(staff_diversity %>% select(-district_school_name), by = c('district_code' = 'district_school_code', 'year'), suffix = c('_students', '_staff')) %>% 
  left_join(student_needs %>% select(-district_name), by = c('district_code', 'year')) %>%
  left_join(readr::read_csv("expenditures.csv") %>%
              janitor::clean_names() %>% select(-lea), by = c('district_name' = 'district', 'year')) %>%
  dplyr::mutate(
        dart_name = dplyr::case_when(
            district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_name == "Hampden Charter School of Science (District)" ~ "Hampden Charter School of Science East (District)",
            district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_name == "Brooke Charter School East Boston (District)" ~ "Brooke Charter School (District)",
            district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            district_name == "Boylston" ~ "Berlin-Boylston",
            district_name == "Berlin" ~ "Berlin-Boylston",
            TRUE ~ district_name),
        district_name = dplyr::case_when(
            district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            TRUE ~ district_name))
write_csv(new_school_info, 'school_info.csv')
# 



