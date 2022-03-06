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
  janitor::clean_names()%>% 
  janitor::row_to_names(1) %>% 
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
                           needs_2018, needs_2017, needs_2016)

write_csv(student_needs, 'student_needs.csv')

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
                           diversity_2018, diversity_2017, diversity_2016)

readr::write_csv(student_diversity, 'student_diversity.csv')


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
                           mobility_2018, mobility_2017, mobility_2016)

write_csv(student_mobility, 'student_mobility.csv')


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


student_diversity <- dplyr::bind_rows(diversity_2021, diversity_2020, diversity_2019, 
                           diversity_2018, diversity_2017, diversity_2016)

readr::write_csv(student_diversity, 'staff_diversity.csv')




