# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# Ecological consequences of systemic racism across and within 157 US cities reveal and uneven sampling of biodiversity across residential housing segregation
#
# Diego Ellis Soto, Millie Chapman, Dexter Locker
## 
# Corresponding author: diego.ellissoto@yale.edu
#
# Aim: The aim of this script is to download biodiversity data from GBIF (birds, mammals, reptiles, amphibians and invertebrates, separately).

# Using rgbif and the polygons from the mapping inequality projects as a shapefile, biodiversity data for each redlined area for each city is downlaoded as a .csv.
#
# Query gbif based on a polygon:
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

require(tidyverse)
library(curl)
# library(geojsonio)
require(rgdal)
library(rgeos)
library(sp)

library(rgbif)
library(rmapshaper)
# library(wellknown)
# require(leaflet)
require(tmaptools)
library(rgbif)
require(plyr)
require(dplyr)
require(sf)
# require(widgetframe) 
# require(bit64)
require(gridExtra)

GBIF_USER = 'diego_ellis_soto' # Use account: i.e. diego_ellis_soto
GBIF_PWD = 'Atelopus1!' # Password
GBIF_EMAIL = 'diego.ellissoto@yale.edu' # User emaol i.e. diego.ellissoto@yale.edu

# Load in all Redlined areas for each city
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
          dplyr::select(id, state, city, holc_id, holc_grade, city_state, area_holc_km2) # < 3 seconds

# st_write(holc, dsn = "/Users/diegoellis/Desktop/holc_singlepoly.gbd",layer="holc_poly", driver="ESRI Shapefile")


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# Download gbif data from HOLC shapefile of choice:                                       
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 


# holc_city <- holc[holc$city == 'New Haven', ] 

# cities_i_downloaded <- list.files('/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/Data/Outputs/GBIF_all_HOLC/')

# This city removes cities that were already downloaded previously as running this code through all cities may take longer than a full day
# holc_no_birmingham <- holc[!holc$city %in%  cities_i_downloaded,]

# unique(holc_no_birmingham$city)


# Run a loop through each unique city and download gbif data
# i = unique(holc$id)[1]

for(i in unique(holc$id)[4956:5000]){
  
  # We keep cities that have at least one record per HOLC grade.
  # message('Keep only multipolygon that have at least one record per HOLC GRADE  ')
  
  holc_singlepolygon <- holc %>% dplyr::filter(id == i)
  
  message(paste0(which(holc$id == holc_singlepolygon$id), ' out of ', nrow(holc)))
  
  # message(paste0('Choose city ', unique(holc_singlepolygon$city), ' multipolygon ', holc_singlepolygon$id))
  
  # Simplify the spatial structure of the shapefiles
  # sf::st_bbox()
  
  
  wkt_siplified_holc_city <- rmapshaper::ms_simplify( as(tmaptools::bb_poly(holc_singlepolygon), 'Spatial') )
  
  message(paste0('Starting GBIF download ', unique(holc_singlepolygon$city), ' multipolygon ', holc_singlepolygon$id ))
  
  
  
  # Specific query: Inspired by Downloading occurrences from a long list of species in R and python
  
  # This is the key part for downloading biodiversity data within a city shapefile. 
  gbif_download_key <- occ_download(
    pred_within( # Put the polygon you want inside of here translated as a WKT format
      rgeos::writeWKT(wkt_siplified_holc_city)
    ),
    pred("hasCoordinate", TRUE),
    pred("hasGeospatialIssue", FALSE),
    pred_gte('year', 1932), # HOLC was in 1933
    pred_in('basisOfRecord', c('HUMAN_OBSERVATION', # Remove unknown records etc. 
                               'LIVING_SPECIMEN',
                               'OBSERVATION',
                               'PRESERVED_SPECIMEN',
                               'MACHINE_OBSERVATION')
    ), user = 'diego_ellis_soto', pwd = 'Atelopus1!', email = 'diego.ellissoto@yale.edu'
  )
  
  # Add has coordinate here ! -> and more ! 
  stat <- "PREPARING"
  while(stat %in% c('PREPARING', 'RUNNING')) {
    met <- occ_download_meta(gbif_download_key)
    stat <- met$status
    Sys.sleep(3)
  }
  
  # Once ready, download and import
  out <- occ_download_get(gbif_download_key) %>% occ_download_import()
  
  message(paste0('GBIF download ', unique(holc_singlepolygon$id), ' succesful !'))
  
  out <- out %>% dplyr::filter(
    !kingdom %in%
      c('Bacteria', 'Chromista',
        'Protozoa', 'incertae sedis',
        'Fungi', 'Viruses', 'Plantae')) %>% 
    drop_na(decimalLongitude, decimalLatitude)  %>% mutate(class = as.factor(class))
  
  
  if( nrow(out) == 0){
    message('Not a single GBIF record in this multipolygon, writting as a log file')
    write.table(out, file = paste0("/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/Data/Outputs/log_file_single_polygon_zero_values/",
                                   i, '_no_gbif_occ.txt'))
  }
  
  
  message('Subsetting by taxa: Aves, Mammaloa, Reptilia, Amphibia, Insecta')
  
  # Get birds
  taxa_I_want <- 'Aves'
  out_bird <- out %>% dplyr::filter(class %in% paste0(taxa_I_want))
  #  %>% subset(year >= year_I_want_cutoff)  %>% st_intersection(HOLC_nh_sf)
  # Get mammals
  taxa_I_want <- 'Mammalia'
  out_mammal <- out %>%  dplyr::filter(class %in% paste0(taxa_I_want))
  # Get insects
  taxa_I_want <- 'Insecta'
  out_insect <- out %>% dplyr::filter(class %in% paste0(taxa_I_want))
  # Get reptiles
  taxa_I_want <- 'Reptilia'  
  out_reptile <- out %>% dplyr::filter(class %in% paste0(taxa_I_want))
  nrow(out_reptile)
  # Get amphibians
  taxa_I_want <- 'Amphibia'
  out_amphibia <- out %>% dplyr::filter(class %in% paste0(taxa_I_want))
  
  outdir <- '/Users/diegoellis/projects/Proposals_funding/Yale_internal_grants/Redlining/Data/Outputs/GBIF_all_HOLC_2022/'
  
  # holc_city_name <- unique(holc_city$city)
  
  # Intersect GBIF with holc:
  # gbif_df <- out_amphibia
  # kingdom <- 'Amphibia'
  
  
  # We run this function separately for birds, mammals, reptiles, amphibians and insects.
  inters_gbif_holc <- function(gbif_df, holc_singlepolygon, outdir, kingdom){
    
# holc_singlepolygon is a single polygon
    
    # kingdom is just used to name the .Rdata object
    
    gbif_df_sp <- SpatialPointsDataFrame(
      gbif_df,
      coords = gbif_df[,c('decimalLongitude', 'decimalLatitude')],
      proj4string =CRS("+proj=longlat +datum=WGS84")
    )
    
    gbif_df_sp_sf <- st_as_sf(gbif_df_sp)
    
    # myprj <- paste0("+proj=laea +lat_0=", round(mean(gbif_df_sp$decimalLatitude)),' +lon_0=', round(mean(gbif_df_sp$decimalLongitude)),' +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')
    # gbif_df_sp <- spTransform(gbif_df_sp, myprj)
    
    # Using a zero-width buffer cleans up many topology problems in R.
    # gbif_df_sp <- gBuffer(gbif_df_sp, byid=TRUE, width=0)
    
    # gbif_df_sp <- gSimplify(gbif_df_sp, tol = 0.00001)
    
    # holc_city_to_work_with_sf <- st_transform(holc_city_to_work_with_sf, st_crs(gbif_df_sp_sf))
    gbif_holc_int <- gbif_df_sp_sf %>% st_intersection((holc_singlepolygon))
    gbif_holc_int$holc_id <- as.factor(gbif_holc_int$holc_id)
    gbif_holc_int$holc_grade <- as.factor(gbif_holc_int$holc_grade)
    
    
    if(nrow(gbif_holc_int) > 0){
      
      # Save raw gbif data intersecting the city with holc polygons of choice
      
      save(gbif_holc_int, file = paste0(outdir, unique(holc_singlepolygon$id),'_', kingdom, '_all_observations.Rdata')) # Raw data # Has species names.
      
      # Calculate number of species and species richness:
      # This is outdated as we calculate this again in a separate script
      n_obs_sp <- ddply(gbif_holc_int, 'holc_id', function(x){
        data.frame(
          holc_grade = unique(x$holc_grade),
          n_species = length(unique(x$species)),
          n_obs = nrow(x)
        )  
      })
      
      # Just intersect if there is more than one species: 
      
      holc_singlepolygon_sf_n_obs_sp <- left_join(holc_singlepolygon, n_obs_sp)
      
      save(holc_singlepolygon_sf_n_obs_sp, file = paste0(outdir, unique(holc_singlepolygon$id),'_', kingdom, '_HOLC_n_obs_sp.Rdata'))
      return(holc_singlepolygon_sf_n_obs_sp)
      
      message(paste0('Saved GBIF intersected with HOLC from ', unique(holc_singlepolygon$id),' : ' ,kingdom))
      
    } # End of loop that checks whether there is more than one observation:
    
    
    # ggplot(holc_city_sf_n_obs_sp,aes(x = holc_id, y = n_obs)) + geom_point()
    # ggplot(holc_city_sf_n_obs_sp,aes(x = holc_id, y = n_obs, color = holc_grade)) + geom_point()
  }
  
  # If we want all taxa:
  
  if(! nrow(out_bird) == 0 ){
  bird  <- inters_gbif_holc(out_bird, holc_singlepolygon, outdir, 'Aves')
  if (! is.null(bird) ){
    bird <- bird %>% dplyr::rename(n_bird_sp = n_species, n_bird_obs = n_obs)
    bird_df <- data_frame(bird) %>% dplyr::select(id, state, city, holc_id, holc_grade, area_holc_km2, n_bird_sp, n_bird_obs)
  }
  }
  
  if(! nrow(out_mammal) == 0 ){
  mam <- inters_gbif_holc(out_mammal, holc_singlepolygon, outdir, 'Mamalia')
  
  if (! is.null(mam) ){
    mam <- mam %>% dplyr::rename(n_mam_sp = n_species, n_mam_obs = n_obs)
    mam_df <- data_frame(mam) %>% dplyr::select(id, state, city, holc_id, holc_grade, area_holc_km2, n_mam_sp, n_mam_obs)
  }
  }
  
  if(! nrow(out_reptile) == 0 ){
  rep <- inters_gbif_holc(out_reptile, holc_singlepolygon, outdir, 'Reptilia')
  if (! is.null(rep) ){
    rep <- rep %>% dplyr::rename(n_rep_sp = n_species, n_rep_obs = n_obs)
    rep_df <- data_frame(rep) %>% dplyr::select(id, state, city, holc_id, holc_grade, area_holc_km2, n_rep_sp, n_rep_obs)
  }
  }
  
  if(! nrow(out_amphibia) == 0 ){
  amph <- inters_gbif_holc(out_amphibia, holc_singlepolygon, outdir, 'Amphibia') 
  
  if (! is.null(amph) ){
    amph <- amph %>% dplyr::rename(n_amph_sp = n_species, n_amph_obs = n_obs)
    amph_df <- data_frame(amph) %>% dplyr::select(id, state, city, holc_id, holc_grade, area_holc_km2, n_amph_sp, n_amph_obs)
  }
}
  
  if(! nrow(out_insect) == 0 ){
  ins <- inters_gbif_holc(out_insect, holc_singlepolygon, outdir, 'Insecta')
  
  if (! is.null(ins) ){
    ins <- ins %>% dplyr::rename(n_ins_sp = n_species, n_ins_obs = n_obs)
    ins_df <- data_frame(ins) %>% dplyr::select(id, state, city, holc_id, holc_grade, area_holc_km2, n_ins_sp, n_ins_obs)
  }
  }
  # Link all richness together and make a leaflet widget:
  
  holc_singlepolygon_across_groups_n_obs_n_sp <- holc_singlepolygon
  
  # Allways make sure file exists: Sometimes for example there are no amphibians recorded
  if( exists('bird_df') ){
    holc_singlepolygon_across_groups_n_obs_n_sp <- holc_singlepolygon_across_groups_n_obs_n_sp %>% left_join(bird_df)
  }
  
  if( exists('mam_df') ){
    holc_singlepolygon_across_groups_n_obs_n_sp <- holc_singlepolygon_across_groups_n_obs_n_sp %>% left_join(mam_df)
  }
  
  if( exists('rep_df') ){
    holc_singlepolygon_across_groups_n_obs_n_sp <- holc_singlepolygon_across_groups_n_obs_n_sp %>% left_join(rep_df)
  }
  
  if( exists('amph_df') ){
    holc_singlepolygon_across_groups_n_obs_n_sp <- holc_singlepolygon_across_groups_n_obs_n_sp %>% left_join(amph_df)
  }
  
  if( exists('ins_df') ){
    holc_singlepolygon_across_groups_n_obs_n_sp <- holc_singlepolygon_across_groups_n_obs_n_sp %>% left_join(ins_df)
  }
  
  # Remove 
  if( exists('bird_df') ){rm(bird_df)}
  if( exists('mam_df') ){rm(mam_df)}
  if( exists('rep_df') ){rm(rep_df)}
  if( exists('amph_df') ){rm(amph_df)}
  if( exists('ins_df') ){rm(ins_df)}
  
  if( exists('out_bird') ){rm(out_bird)}
  if( exists('out_mammal') ){rm(out_mammal)}
  if( exists('out_reptile') ){rm(out_reptile)}
  if( exists('out_amphibia') ){rm(out_amphibia)}
  if( exists('out_insect') ){rm(out_insect)}
  
  message(print(paste0('Calculated N.obs and N.species for birds, mammals, reptiles, amphibians and insects by HOLC for ', unique(holc_singlepolygon$id))))
  
  
  save(holc_singlepolygon_across_groups_n_obs_n_sp, file = paste0(outdir, unique(holc_singlepolygon$id),'_HOLC_all_taxa_groups_annotated.Rdata'))
  
  rm(holc_singlepolygon_across_groups_n_obs_n_sp)
  
  
  
  message(paste0('Finished ', unique(holc_singlepolygon$id)))
  
} # Polygon 1404 1404 out of 9848 # Error in rep(x1, ny) : invalid 'times' argument # 9852

# GBIF download API: using the route /occurrence/download/, and the functions that are prefixed with rgbif::occ_download*

# These functions have a slightly different interface than rgbif::occ_search and rgbif::occ_data, so there’s the downside of learning a new query interface. However, plus side is that the query interface is more flexible, and you can get as much data as you like. Another down side is that you don’t get the data immediately, but rather wait for the file to be prepared. But don’t worry! We make it easy to get the file and import without leaving R.