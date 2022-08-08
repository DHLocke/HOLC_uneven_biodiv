# a reviewer asked for parsing data by time and source, and we wanted to look up area.

library(tidyverse)


(comb <- read_csv(paste0(getwd(), '/input_data/main_combined_2022-05-27.csv')))

comb |> glimpse()


comb |> 
  select(id, area_holc_km2, holc_grade, city) |> 
  group_by(city, holc_grade) |> 
  summarise(sum_area_holc_km2 = sum(area_holc_km2)) |> 
  write_csv(paste0(getwd(), '/input_data/sum_area_per_city_grade_', Sys.Date(), '.csv'))
