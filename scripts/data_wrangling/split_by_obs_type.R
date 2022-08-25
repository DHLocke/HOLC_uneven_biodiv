# r loop through city and unique polygon -> STORE csv with inaturalist, ebird, others ! -> then send to dexter ?! 1 hour to make .csv ile 

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# Uneven biodiversity sampling across redlined urban areas in the United States
# Store for each HOLC polygon the number of ebird observations and the number of 'others'
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

outdir = '/Users/diegoellis/Desktop/Subset_HOLC/CSV_file_w_obs_code/'
# dir.create('/Users/diegoellis/Desktop/Subset_HOLC/CSV_file_w_obs_code/')
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

# --- --- --- --- --- --- --- --- ---
# Subset for 2000-2020:
# --- --- --- --- --- --- --- --- ---


for(i in unique(aves_obs)){
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
    #   collectionCode == 'GBBC'  ~  'great backyard bird count',
    collectionCode == 'GBBC'  ~  'ebird',
    str_detect(collectionCode, 'EBIRD')  ~  'ebird',
    str_detect(institutionCode, 'iNaturalist') ~ 'iNaturalist',
    TRUE ~ 'other'
  )
  )
  # data.frame(df) %>% dplyr::filter(year >= 2000 & year <= 2020)
  df = df %>%  data.frame() %>% dplyr::filter(year >= 2000 & year <= 2020)
  
  range(df$year)
  tmp = data.frame(table(df$institutionCode_review))
  
  if(
    !"other" %in% unique(tmp$Var1)
  ){print(' No other obs making a column of type other with observation 0')
    
    # rbind(tmp, c(factor('other'), 0))
    # tmp[nrow(tmp) + 1,] <- c(as.factor('other'), 0)
    tmp2 = data.frame(Var1 = as.factor('other'), Freq = 0)
    tmp = rbind(tmp, tmp2)
    
  }
  
  if(
    !"iNaturalist" %in% unique(tmp$Var1)
  ){print(' No iNaturalist obs making a column of type other with observation 0')
    
    # rbind(tmp, c(factor('other'), 0))
    # tmp[nrow(tmp) + 1,] <- c(as.factor('other'), 0)
    tmp2 = data.frame(Var1 = as.factor('iNaturalist'), Freq = 0)
    tmp = rbind(tmp, tmp2)
    
  }
  
  if(
    !"ebird" %in% unique(tmp$Var1)
  ){print(' No ebird obs making a column of type other with observation 0')
    
    # rbind(tmp, c(factor('other'), 0))
    # tmp[nrow(tmp) + 1,] <- c(as.factor('other'), 0)
    tmp2 = data.frame(Var1 = as.factor('ebird'), Freq = 0)
    tmp = rbind(tmp, tmp2)
    
  }
  
  tmp$holc_polygon <- gsub('.Rdata','', basename(i) )
  
  # save(df, file = paste0(outdir, '/Raw_data/',i,'_relabelled_inst_code.Rdata'))
  write.table(tmp, file = paste0(outdir, "/holc_id_by_WITH_biodiv_collection_germany_2012_2020.csv"), append = T, row.names = F,col.names = F, sep = ",")
  
}
# Make geom boxplot for sampling density for ebird and inaturalist

tmp_2012_2020 = read.table(paste0(outdir, "/holc_id_by_WITH_biodiv_collection_germany_2012_2020.csv"), header= F,sep=',')
length(unique(tmp_2012_2020$V3)) # Has 0 observations

head(tmp_2012_2020)

names(tmp_2012_2020) <- c('Type', 'Sum', 'holc_polygon_id')
                            
tmp_2012_2020$holc_grade = substr(sub(".*?_", "", (sub("_.*?", "", sub("_.*?", "", tmp_2012_2020$holc_polygon_id))) ), 1,1) 

head(tmp_2012_2020)

holc_area <- read.csv('/Users/diegoellis/Desktop/main_combined_2022-05-27.csv') %>% dplyr::select(city, holc_grade, area_holc_km2) %>% dplyr::group_by(holc_grade) %>% dplyr::summarise(area_sum = sum(area_holc_km2)) %>% dplyr::filter(holc_grade != 'E')

tmp_2012_2020$holc_grade <- as.factor(tmp_2012_2020$holc_grade)


ebird  = tmp_2012_2020 %>% filter(Type =='ebird')
inat = tmp_2012_2020 %>% filter(Type =='iNaturalist')
other = tmp_2012_2020 %>% filter(Type =='other')

ebird[ebird$holc_grade =='2',]$holc_grade <- 'B'
inat[inat$holc_grade =='2',]$holc_grade <- 'B'
other[other$holc_grade =='2',]$holc_grade <- 'B'

ebird_sampling_density <- ddply(ebird, 'holc_grade', function(x){
  sampling_sum = sum(x$Sum)
})
ebird_sampling_density_df = left_join(ebird_sampling_density, holc_area)
ebird_sampling_density_df$sampling_density <- ebird_sampling_density_df$V1 / ebird_sampling_density_df$area_sum

inat_sampling_density <- ddply(inat, 'holc_grade', function(x){
  sampling_sum = sum(x$Sum)
})
inat_sampling_density_df = left_join(inat_sampling_density, holc_area)
inat_sampling_density_df$sampling_density <- inat_sampling_density_df$V1 / inat_sampling_density_df$area_sum

other_sampling_density <- ddply(other, 'holc_grade', function(x){
  sampling_sum = sum(x$Sum)
})
other_sampling_density_df = left_join(other_sampling_density, holc_area)
other_sampling_density_df$sampling_density <- other_sampling_density_df$V1 / other_sampling_density_df$area_sum

# redlining colors
holc_pal <- c('#92BC6B' # green
              , '#92C7C9' # blue
              , '#E7DC6B' # yellow
              , '#E47D67' # red
              #, '#A9A9A9'
) # dark gray)

inat_sampling_density_df %>%
  ggplot(aes(holc_grade, sampling_density, fill = holc_grade)) +
  geom_col() + 
  scale_fill_manual(values = holc_pal) + 
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 1400)) + 
 theme_bw(16) + 
  theme_classic(16) +
  labs(title='iNaturalist') + 
  theme(legend.position = 'none') + 
  ylab('Sampling density in 1km^2') +
  xlab('HOLC Grade') +
  NULL

ggsave('/Users/diegoellis/Desktop/inaturalist.png'
        , width = 3.42
        , height = 4
        , dpi = 600
        )

other_sampling_density_df %>%
  ggplot(aes(holc_grade, sampling_density, fill = holc_grade)) +
  geom_col() + 
  scale_fill_manual(values = holc_pal) + 
  #   scale_y_continuous(expand = c(0, 0), limits = c(0, 1400)) + 
  theme_bw(16) + 
  theme_classic(16) +
  labs(title='Other') + 
  theme(legend.position = 'none') + 
  ylab('Sampling density in 1km^2') +
  xlab('HOLC Grade') +
  NULL

ggsave('/Users/diegoellis/Desktop/other.png'
       , width = 3.42
       , height = 4
       , dpi = 600
)

ebird_sampling_density_df %>%
  ggplot(aes(holc_grade, sampling_density, fill = holc_grade)) +
  geom_col() + 
  scale_fill_manual(values = holc_pal) + 
  #   scale_y_continuous(expand = c(0, 0), limits = c(0, 1400)) + 
  theme_bw(16) + 
  theme_classic(16) +
  labs(title='eBird') + 
  theme(legend.position = 'none') + 
  ylab('Sampling density in 1km^2') +
  xlab('HOLC Grade') +
  NULL

ggsave('/Users/diegoellis/Desktop/ebird.png'
       , width = 3.42
       , height = 4
       , dpi = 600
)
# 
# tmp_2012_2020  %>% 
#   filter(holc_grade != 'E') |> 
#   filter(Type =='ebird')  |> 
#   group_by(holc_grade) |> 
#   tabyl()
  # mutate(holc_grade = factor(holc_grade, levels = LETTERS[1:4], ordered = TRUE)) |> 
  # arrange(holc_polygon_id, holc_grade) |> 
  summarise(Total_obs = sum(Sum))

  
  
  

  mutate(total_sampling_density = sum(Sum, na.rm = TRUE)) 


tmp_2012_2020  %>% 
filter(holc_grade != 'E') |>
  # mutate(holc_grade = factor(holc_grade, levels = LETTERS[1:4], ordered = TRUE)) |> 
  # arrange(id, holc_grade) |> 
  group_by(holc_grade) |> 
  summarise(total_sampling_density = sum(records, na.rm = TRUE)
            , total_area = sum(area_holc_km2, na.rm = TRUE))  |>  
  mutate(`Sampling Density` = total_sampling_density / total_area, `HOLC Grade` = holc_grade) |>
  ggplot(aes(`HOLC Grade`, `Sampling Density`, fill = holc_grade)) +
  geom_col() + 
  scale_fill_manual(values = holc_pal) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1400)) + 
  # theme_bw(16) + 
  theme_classic(16) + 
  theme(legend.position = 'none') + 
  NULL
) 


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# [2] Calculate institution code number of observation for each unique HOLC ID #### 
# This will result in less unique holc ID since not every hold id polygon was sampled in the last 20 years
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# 
# cities <- list.files(paste0(outdir, 'Raw_data'), full.names = T)
# 
# for(i in unique(cities)){
#   print(basename(i))
#   
#   load(i)
#   
#   tmp_holc_id_file = df %>% 
#     st_drop_geometry() %>%
#     dplyr::select(holc_grade, id, year, institutionCode_review) %>%
#     mutate(institutionCode_review = replace(institutionCode_review, institutionCode_review == "great backyard bird count", 'other')) %>%
#     as_tibble() %>%
#     dplyr::filter(year >= 2000 & year <= 2020) %>%  
#     tabyl(id, institutionCode_review) %>% 
#     as_tibble() #   %>% 
#   
#   write.table(tmp_holc_id_file, file = paste0("/Users/diegoellis/Desktop/Subset_HOLC/holc_id_by_WITH_biodiv_collection_code.csv"), append = T, row.names = F,col.names = F, sep = ",")
# }
# 
# tmp_holc_id_file = read.csv("/Users/diegoellis/Desktop/Subset_HOLC/holc_id_by_WITH_biodiv_collection_code.csv",header=F)
# names(tmp_holc_id_file) <- c('id', 'ebird', 'iNaturalist', 'other')
# write.csv(tmp_holc_id_file, file = "/Users/diegoellis/Desktop/Subset_HOLC/holc_id_by_WITH_biodiv_collection_code.csv")
# 
# 
# biodiv_trend = read.csv('/Users/diegoellis/Desktop/Subset_HOLC/holc_id_by_WITH_biodiv_collection_code.csv')
# biodiv_trend$holc_grade = substr(sub(".*?_", "", (sub("_.*?", "", sub("_.*?", "", biodiv_trend$id))) ), 1,1)
# biodiv_trend = biodiv_trend %>% dplyr::filter(holc_grade !='E')
# 
# biodiv_trend[which(duplicated(biodiv_trend$id)),]
# 
# # Calculate the number of observations of each:
# biodiv_by_obs = plyr::ddply(biodiv_trend, 'holc_grade', function(x){
#   data.frame(
#     holc_grade = unique(x$holc_grade),
#     n_ebird = sum(x$ebird,na.rm=T),
#     n_inat = sum(x$iNaturalist,na.rm=T),
#     n_other =sum(x$other,na.rm=T)
#   )
# }) %>% left_join(holc_area, by = 'holc_grade')
# 
# save(biodiv_by_obs, file = '/Users/diegoellis/Desktop/object_for_sampling_density_2000_2020.Rdata')
