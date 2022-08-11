# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# Ecological consequences of systemic racism across and within 157 US cities reveal and uneven sampling of biodiversity across residential housing segregation
#
# Diego Ellis Soto, Millie Chapman, Dexter Locker
## 
# Corresponding author: diego.ellissoto@yale.edu
#
# Aim: Estimate survey effort for each city:.
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

require(sf)
require(sp)
library(KnowBR)
library(rgdal)
library(raster)
require(tidyverse)
data("adworld")
cm.cols1=function(x,bias=1) { colorRampPalette(c('grey90','steelblue4','steelblue1','gold','red1','red4'),bias=bias)(x)}

dir.create("/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Data/Survey_completeness/")

indir = '/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Data/HOLC_single_polygon_GBIF_anno/'
indir = '/Users/diegoellis/Desktop/HOLC_newest/Download_GBIF_HOLC/'

outdir <- "/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Data/Survey_completeness/"

setwd(outdir)


# i = "/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/Data/Outputs/GBIF_all_HOLC//New Haven/"

holc <- st_read('/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/Data/Inputs/Redlining_shp/fullshpfile/shapefile/holc_ad_data.shp') %>% 
  # , as_tibble = TRUE 
  #       dplyr::filter(!is.na(holc_grade) & holc_grade != 'E') %>% 
  sf::st_cast('POLYGON') %>% # IMPORTANT
  dplyr::filter(!st_is_empty(.)) %>% 
  sf::st_make_valid(.) %>% 
  tibble::rowid_to_column() %>% 
  dplyr::mutate(  id = paste(state, city, holc_id, holc_grade, rowid, sep = '_')
                  , city_state = paste0(city, ', ', state)
                  , area_holc_km2 = as.double(st_area(.) / 1e+6)) %>% 
  dplyr::select(id, state, city, holc_id, holc_grade, city_state, area_holc_km2) 

aves_obs = list.files(indir, pattern  = paste0('*_Aves_all_observations.Rdata'), full.names = T)

# Do from stealle onwards
# Dont do houston
# for(i in list.files(indir, full.names = T)){
# i <- unique(cities_i_want)[1]

# Got an error from 1] "Jacksonville"
# Error in h(simpleError(msg, call)) : 
#   error in evaluating the argument 'x' in selecting a method for function 'addAttrToGeom': empty geometries are not supported by sp classes: conversion failed

cities_i_want = unique(holc$city)


for(i in cities_i_want ){# i <- 'Houston'
# i = 'San Antonio'
# for(i in cities_i_want[cities_i_want %in% c('Houston', 'San Antonio', 'Jacksonville', 'Wheeling')] ){# i <- 'Wheeling'
# for(i in unique(cities_i_want)[1:196]){# i <- 'Wheeling'

# for(i in list.files(indir, full.names = T)){  
  # for(i in list.files(indir, full.names = T)[160:185]){
  
  # # Houston, Jacksonville, San Antonio, Wheeling did not run ####
  
  ciudad = basename(i)
  print(ciudad)
  
  if(!any(str_detect(aves_obs, pattern = i))==TRUE){
    print(paste0(i, ' has no biodiversity data'))
    next
  }
  
  biodiv_data = aves_obs[str_detect(aves_obs, pattern = i)]
  
  results <- sapply(biodiv_data, function(x) mget(load(x)), simplify = TRUE) 
  
  mycols = c('species',
             'family',
             'genus',
             'decimalLongitude',
             'decimalLatitude',
             'collectionCode',
             'institutionCode',
             'year',
             'city',
             'city_state',
             'holc_id',
             'holc_grade',
             'species')
  
  results <- lapply( results , "[", , mycols) 
  # Load all SF objects into a sngle list
  gbif_holc_int <- do.call(rbind, results)
  
  # load(paste0(i,'/Aves_all_observations.Rdata' ))  
  # 
  # gbif_holc_int = gbif_holc_int %>% dplyr::select(holc_grade, holc_id,
  #                                  publisher, collectionCode, 
  #                                  year, month, day,
  #                                  locality, decimalLatitude, decimalLongitude,
  #                                  phylum, class, order, family,
  #                                  genus, vernacularName, datasetKey,
  #                                  species)
  #  
  gbif_holc_int$Counts <- 1
  
  city_HOLC = holc[holc$city == i,]
  
  dat <- gbif_holc_int[,c("species","decimalLongitude","decimalLatitude", "Counts",
                          'holc_id', 'holc_grade')]
  names(dat)[2:3] <- c('longitude', 'latitude')
  
  dat = data.frame(dat )%>% dplyr::select(-geometry)
  
  # Change to city working directory:
  # "/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Data/Survey_completeness/"
  # dir.create(paste0(outdir, '/', ciudad,'/'))
  dir.create(paste0("/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Data/Survey_completeness/", i))
  setwd(paste0("/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Data/Survey_completeness/", i))
  
  if(all(is.na(dat$holc_id))){next}
  data("adworld")
  
  
  # dat[dat$holc_id == 'B3',]
  # Find unique HOLC id and remove it
  # which is empty polygon
  # empty_holc_id = city_HOLC[st_is_empty(city_HOLC),]$holc_id
  # dat = dat[!dat$holc_id == empty_holc_id,]
  
  city_HOLC = city_HOLC[!st_is_empty(city_HOLC),] # remove empty polygons
  # table(st_is_empty(city_HOLC))
  city_HOLC_sp = as(city_HOLC, 'Spatial') # REMOVE NA polygons ! 
  # KnowBPolygon needs sp object
  
  KnowBPolygon(data=dat, format="A", estimator=1,
               shape=city_HOLC_sp, 
               shapenames="holc_id", # Re-run with HOLC_grade #### and how does this differ with median -> then do a scatter plot -> how does this differ from 1-1 line
               jpg=TRUE,
               Maps=TRUE,
               save="RData",
               legend=TRUE,
               colscale=cm.cols1(100))

}
# Houston
# Error in KnowBPolygon(data = dat, format = "A", estimator = 1, shape = city_HOLC_sp,  : 
# Maps are not depicted because there are less than two polygons with information about completeness (see the file Estimators)

# San Antonio
# ror in KnowBPolygon(data = dat, format = "A", estimator = 1, shape = city_HOLC_sp,  : 
#                       Maps are not depicted because there are less than two polygons with information about completeness (see the file Estimators)

#  


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Calculate Cold-Hot spots based on La Sorte et al. 2020 ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# La Sorte et al. 2020: Area is the primary correlate of annual and seasonal patterns of avian species richness in urban green spaces

# We removed poorly surveyed NYC green spaces from our annual and seasonal analyses based on the de- fault parameter recommendations from Lobo et al. (2018): the ratio between the number of occurrence records and the number of observed species was < 3, the slope of the species accumulation curve was > 0.3, and survey completeness was < 50.

indir = '/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/Data/Outputs/Survey_completeness_birds_2021/'

indir = '/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/Data/Outputs/Survey_completeness_birds/'

indir = '/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Data/Survey_completeness/'

estimator_data <- paste0(list.files(indir, full.names = T), '/Estimators.Rdata')

estimator_data[!file.exists(estimator_data)] # Houston, Jacksonville, San Antonio did not run ####

estimator_data <- estimator_data[file.exists(estimator_data)]

# fnames <- list.files(indir, '.csv')
# csv <- lapply(temp, read.csv)
# result <- do.call(rbind, csv)
listForFiles <- list()
for(i in 1:length(unique(estimator_data))){
  # i <- 1
  print(i)
  load(estimator_data[i])
  listForFiles[[i]] <- estimators
  city_name <- unique(estimator_data)[i]
  city_name <- gsub('/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Data/Survey_completeness//', '',city_name)
  city_name <- gsub('/Estimators.Rdata','',city_name)
  listForFiles[[i]]$City <- city_name
}

df <- do.call(rbind.data.frame, listForFiles)
df$holc_grade <- substr(df$Area, 1, 1)
df$holc_grade <- as.factor(df$holc_grade)
df = df[df$holc_grade %in% c('A', 'B', 'C', 'D'),]

write.csv(df, file = '/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Data/All_cities/bird_completeness_HOLC_cities_2022.csv')

# Calculate sampling density: ####

HOLC_cities_df = as.data.frame(holc)
HOLC_cities_df = HOLC_cities_df %>% select(city, holc_id, area_holc_km2) %>% rename(
  City = city, Area = holc_id
)
HOLC_cities_df <- HOLC_cities_df[HOLC_cities_df$City %in% df$City,]

df_w_area = merge(df, HOLC_cities_df, by = c('City','Area'), all.x=TRUE, all.y=FALSE)

summary_statistics = plyr::ddply(df_w_area, 'holc_grade', function(y){
  data.frame(
    T_area = sum(y$area_holc_km2, na.rm=T),
    N_records = sum(y$Records, na.rm=T),
    u_slope = mean(y$Slope, na.rm=T),
    u_completeness = mean(y$Completeness, na.rm=T),
    u_ratio = mean(y$Ratio, na.rm=T)
  )
})

summary_statistics$density = summary_statistics$N_records / summary_statistics$T_area
#####



# Biodiversity dadtasets are more complete in HOLC A areas:
ggplot(df, aes(x = factor(holc_grade), y =Completeness,  fill=factor(holc_grade)))+
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values=c("green4","dodgerblue3", "gold1", "firebrick4")) +
  ggtitle(paste0('Completeness across HOLC')) +xlab('HOLC Grade')+ylab('Sampling density')+theme_classic()

ggsave('/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Figs/All_cities/Completeness_across_holc.pdf')

# There is more sampling density in HOLC A areas:
ggplot(df, aes(x = factor(holc_grade), y =log(Records),  fill=factor(holc_grade)))+
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values=c("green4","dodgerblue3", "gold1", "firebrick4")) +
  ggtitle(paste0('Log Number of Records across HOLC')) +xlab('HOLC Grade')+ylab('Log Number of Records')+theme_classic()

ggsave('/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Figs/All_cities/Log_records.pdf')

ggplot(df, aes(x = factor(holc_grade), y =Richness,  fill=factor(holc_grade)))+
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values=c("green4","dodgerblue3", "gold1", "firebrick4")) +
  ggtitle(paste0('Mean bird richness across HOLC')) +xlab('HOLC Grade')+ylab('Bird species richness')+theme_classic()

ggsave('/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Figs/All_cities/Bird_species_richness.pdf')

ggplot(df, aes(x = factor(holc_grade), y =log(Richness),  fill=factor(holc_grade)))+
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values=c("green4","dodgerblue3", "gold1", "firebrick4")) +
  ggtitle(paste0('Log bird richness across HOLC')) +xlab('HOLC Grade')+ylab('Bird species richness')+theme_classic()

ggsave('/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Figs/All_cities/Log_Bird_species_richness.pdf')


ggplot(df, aes(x = factor(holc_grade), y =Slope,  fill=factor(holc_grade)))+
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values=c("green4","dodgerblue3", "gold1", "firebrick4")) +
  ggtitle(paste0('Slope across HOLC')) +xlab('HOLC Grade')+ylab('Slope')+theme_classic()

# What does the slope mean in Biodiversity sampling this is super interesting:
ggsave('/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Figs/All_cities/Slope.pdf')

ggplot(df, aes(x = factor(holc_grade), y =log(Ratio),  fill=factor(holc_grade)))+
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values=c("green4","dodgerblue3", "gold1", "firebrick4")) +
  ggtitle(paste0('Log Ratio across HOLC')) +xlab('HOLC Grade')+ylab('log Ratio')+theme_classic()

# What does the slope mean in Biodiversity sampling this is super interesting:
ggsave('/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Figs/All_cities/Log_ratio.pdf')

# What does each one of these mean> 

df
df_completeness = plyr::ddply(df, 'holc_grade', function(x){
  data.frame(
    Completeness = mean(x$Completeness, na.rm=T),
    Slope = mean(x$Slope, na.rm=T),
    Richness = mean(x$Richness, na.rm=T),
    Observed_.richness = mean(x$Observed.richness, na.rm=T),
    Records = mean(x$Records, na.rm=T)
  )
})
df_completeness = df_completeness[df_completeness$holc_grade %in% c('A', 'B', 'C', 'D'),]
# Load data frames in list into a single data frame :


write.csv(df_completeness, file = '/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Data/All_cities/bird_completeness_HOLC_summary.csv')


# ggplot(df_completeness, aes(x = factor(holc_grade), y =V1,  fill=factor(holc_grade)))+
#   geom_bar(stat="identity") + theme_classic() +
#   scale_fill_manual(values=c("green4","dodgerblue3", "gold1", "firebrick4")) +
#   ggtitle(paste0('Completeness across HOLC')) +xlab('HOLC Grade')+ylab('Sampling density')+theme_classic() +theme(text=element_text(size=24), legend.title = element_blank(),legend.position = 'none',  axis.line = element_line( size = 1))
# 
# ggsave('/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/Completeness_across_holc.pdf')

# After La Sorte et al. 2018 and Lobo et al. 2018 we calculate survey completeness:

# We removed poorly surveyed NYC green spaces from our annual and seasonal analyses based on the de- fault parameter recommendations from Lobo et al. (2018): the ratio between the number of occurrence records and the number of observed species was < 3, the slope of the species accumulation curve was > 0.3, and survey completeness was < 50.

# How many survets are poorly surveyed:
df$ratio_n_obs_sp_richness <- round( (df$Records / df$Observed.richness) , 2)

undersampled_ratio <- which(df$ratio_n_obs_sp_richness < 3)
# Poorly sampled: Slope is above 0.3
undersampled_slope <- which(df$Slope > 0.3)

undersampled_completeness <- which(df$Completeness < 50)
undersampled_regions <- c(undersampled_ratio, undersampled_slope, undersampled_completeness)
undersampled_regions = unique(undersampled_regions)
# A total of 2992 are undersampled
length(undersampled_regions)

coldspots <- df[undersampled_regions,]
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Look for coldspots across the united states: : 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

coldspot_regions <- plyr::ddply(coldspots, 'holc_grade', function(x){
  data.frame(
    n_row_total_n_coldspot_polygons = nrow(x),
    percent_coldspots = (  ( nrow(x) / nrow(coldspots) ) * 100 )
  )
})

ggplot(coldspot_regions, aes(x = factor(holc_grade), y =round(percent_coldspots,2),  fill=factor(holc_grade)))+
  geom_bar(stat="identity") + theme_classic() +
  scale_fill_manual(values=c("green4","dodgerblue3", "gold1", "firebrick4")) +
  ggtitle(paste0('Percent Coldspots across the United states')) +xlab('HOLC Grade')+ylab('Number of polygons with incomplete sampling')+theme_classic() 
ggsave('/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/2022/Figs/All_cities/N_incomplete_sampling.pdf')
# This has the polygons across cities that need to be filled ! Make as supplementary table:

# }


# Hasta aca llegue ########
# Look at sampled cities:

cities_i_want = list.files("/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/Data/Outputs/GBIF_all_HOLC/", full.names = T)

y  <- NULL;
for (i in unique(cities_i_want[-159]))
{ 
  print(basename(i))
  
  load(paste0(i, '/Aves_all_observations.Rdata'))
  
  if(! file.exists(paste0(i, '/Aves_all_observations.Rdata')) ){ next}
  tmp <- nrow(gbif_holc_int[!gbif_holc_int$holc_grade == 'E',])
  y <- rbind(y, tmp)
}


city_sampled = data.frame(cbind( basename(cities_i_want[-159]), gsub('tmp','',y)))
names(city_sampled) <- c('City', 'n_bird_obs')
city_sampled$n_bird_obs <- as.numeric(city_sampled$n_bird_obs)
sum(city_sampled$n_bird_obs)
city_sampled[city_sampled$n_bird_obs == max(city_sampled$n_bird_obs),] # Most sampled of our cities
city_sampled[city_sampled$n_bird_obs == min(city_sampled$n_bird_obs),] # Least sampled city
