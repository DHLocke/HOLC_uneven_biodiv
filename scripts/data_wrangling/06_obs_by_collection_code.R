# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# Uneven biodiversity sampling across redlined urban areas in the United States
#
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# Load cities: #### 
require(rgdal)
require(stringr)
require(tidyverse)
require(reshape2)
require(gridExtra)
require(sf)
require(sp)
require(raster)
require(plyr)
require(tidyverse)
require(janitor)
require(patchwork)
conflict_prefer("mutate", "dplyr")
conflict_prefer("arrange", "dplyr")

indir = '/Users/diegoellis/Desktop/HOLC_newest/Download_GBIF_HOLC'
outdir = '/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Data/Species_richness_n_obs/'
outdir = '/Users/diegoellis/Desktop/Subset_HOLC/'

# Load Holc
holc <- st_read('/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/Data/Inputs/Redlining_shp/fullshpfile/shapefile/holc_ad_data.shp') %>% 
  sf::st_cast('POLYGON') %>% # IMPORTANT
  dplyr::filter(!st_is_empty(.)) %>% 
  sf::st_make_valid(.) %>% 
  tibble::rowid_to_column() %>% 
  dplyr::mutate(  id = paste(state, city, holc_id, holc_grade, rowid, sep = '_')
                  , city_state = paste0(city, ', ', state)
                  , area_holc_km2 = as.double(st_area(.) / 1e+6)) %>% 
  dplyr::select(id, state, city, holc_id, holc_grade, city_state, area_holc_km2) 

holc_area <- read.csv('/Users/diegoellis/Desktop/main_combined_2022-05-27.csv') %>% dplyr::select(city, holc_grade, area_holc_km2) %>% dplyr::group_by(holc_grade) %>% dplyr::summarise(area_sum = sum(area_holc_km2)) %>% dplyr::filter(holc_grade != 'E')

# List the Rdata files with bird biodiversity data from GBIF for each unique HOLC polygon ID
cities_i_want = unique(holc$city)
aves_obs = list.files(indir, pattern  = paste0('*_Aves_all_observations.Rdata'), full.names = T)

message(paste0('A total of ', length(aves_obs),  ' HOLC polygons have some sort of bird data') )
# [1000] "/Users/diegoellis/Desktop/HOLC_newest/Download_GBIF_HOLC/CA_San Jose_NA_C_1105_Aves_all_observations.Rdata"      

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Loop through each city and create a new column about the collection code of observations based on a reviewers comment
# This loop stores each city as a separate .Rdata file that is later read to calculate the number of observations
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

for(i in unique(cities_i_want)){
  print(i)   # i = 'Birmingham'
  
  if(!any(str_detect(aves_obs, pattern = i))==TRUE){
    print(paste0(i, ' has no biodiversity data'))
    next
  }
  
  biodiv_data = aves_obs[str_detect(aves_obs, pattern = i)]
  
  # Load all single polygons of a city into a list: 
  results <- sapply(biodiv_data, function(x) mget(load(x)), simplify = TRUE) 
  
  mycols = c('species',
             'family',
             'genus',
             'decimalLongitude',
             'decimalLatitude',
             'collectionCode',
             'collectionID',
             'institutionCode',
             'year',
             'city',
             'city_state',
             'holc_id',
             'holc_grade',
             'species',
             'id')
  
  results <- lapply( results , "[", , mycols) 
  
  df <- do.call(rbind, results) # bind_rowsLoad all SF objects into a sngle list
  
  df =   df %>% mutate(institutionCode_review = case_when(
    collectionCode == 'GBBC'  ~  'great backyard bird count',
    str_detect(collectionCode, 'EBIRD')  ~  'ebird',
    str_detect(institutionCode, 'iNaturalist') ~ 'iNaturalist',
    TRUE ~ 'other'
  )
  )
  save(df, file = paste0(outdir, '/Raw_data/',i,'_relabelled_inst_code.Rdata'))
  
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# [2] Calculate institution code number of observation for each unique HOLC ID #### 
# This will result in less unique holc ID since not every hold id polygon was sampled in the last 20 years
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

cities <- list.files(paste0(outdir, 'Raw_data'), full.names = T)

for(i in unique(cities)){
  print(basename(i))
  
  load(i)
  
  tmp_holc_id_file = df %>% 
    st_drop_geometry() %>%
    dplyr::select(holc_grade, id, year, institutionCode_review) %>%
    mutate(institutionCode_review = replace(institutionCode_review, institutionCode_review == "great backyard bird count", 'other')) %>%
    as_tibble() %>%
    dplyr::filter(year >= 2000 & year <= 2020) %>%  
    tabyl(id, institutionCode_review) %>% 
    as_tibble() #   %>% 
  
  write.table(tmp_holc_id_file, file = paste0("/Users/diegoellis/Desktop/Subset_HOLC/holc_id_by_WITH_biodiv_collection_code.csv"), append = T, row.names = F,col.names = F, sep = ",")
}

tmp_holc_id_file = read.csv("/Users/diegoellis/Desktop/Subset_HOLC/holc_id_by_WITH_biodiv_collection_code.csv",header=F)
names(tmp_holc_id_file) <- c('id', 'ebird', 'iNaturalist', 'other')
write.csv(tmp_holc_id_file, file = "/Users/diegoellis/Desktop/Subset_HOLC/holc_id_by_WITH_biodiv_collection_code.csv")


biodiv_trend = read.csv('/Users/diegoellis/Desktop/Subset_HOLC/holc_id_by_WITH_biodiv_collection_code.csv')
biodiv_trend$holc_grade = substr(sub(".*?_", "", (sub("_.*?", "", sub("_.*?", "", biodiv_trend$id))) ), 1,1)
biodiv_trend = biodiv_trend %>% dplyr::filter(holc_grade !='E')

# Calculate the number of observations of each:
biodiv_by_obs = plyr::ddply(biodiv_trend, 'holc_grade', function(x){
  data.frame(
    holc_grade = unique(x$holc_grade),
    n_ebird = sum(x$ebird,na.rm=T),
    n_inat = sum(x$iNaturalist,na.rm=T),
    n_other =sum(x$other,na.rm=T)
  )
}) %>% left_join(holc_area, by = 'holc_grade')
