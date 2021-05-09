## Data curation

### Imported the processed data for devices, inspections and device inspections
devices <- read_processed_csv(file.path(processed_dir, "devices_revised.csv"))
inspections <- read_processed_csv(file.path(processed_dir, "inspections_revised.csv"))
device_inspections <- read_processed_csv(file.path(processed_dir, "device_inspections.csv"))

### Stores (raw)

#### Data dictionary

stores_data_dict <- data_dict[data_dict$Table=='Stores',]
stores_data_dict
stores_path<-file.path(raw_dir, "bac0a05c-71b5-4634-96b9-10cc8a20102d.csv")
stores<-read.csv(stores_path,encoding = "UTF-8", stringsAsFactors = TRUE) 

#### Import

stores_path<-file.path(raw_dir, "bac0a05c-71b5-4634-96b9-10cc8a20102d.csv")
stores<-read.csv(stores_path,encoding = "UTF-8", stringsAsFactors = TRUE) 
head(stores)
summary(stores,maxsum=15)
municipalities_path<-file.path(raw_dir, "pennsylvania_municipalities.csv")
pa_municipalities<-read.csv(municipalities_path,encoding = "UTF-8", stringsAsFactors = TRUE) 
pa_municipalities$COUNTY<-trimws(pa_municipalities$COUNTY)
pa_municipalities$MUNICIPALITY<-trimws(pa_municipalities$MUNICIPALITY)
pa_municipalities$CLASS<-trimws(pa_municipalities$CLASS)

allegheny_municipalities<-pa_municipalities[pa_municipalities$COUNTY=='Allegheny',]
allegheny_municipalities$municipality<-toupper(allegheny_municipalities$MUNICIPALITY)
allegheny_municipalities$MUNICIPALITY<-NULL
allegheny_municipalities$municipality<-str_replace(allegheny_municipalities$municipality,"CITY|BOROUGH|TOWNSHIP","")
allegheny_municipalities$municipality<-trimws(allegheny_municipalities$municipality)


#### Missing data
##### Municipality 


stores[stores$municipality=='',]

#### Data cleaning


stores$municipality<-trimws(stores$municipality)
stores<-uppercased(stores,"new_municipality")
stores<-uppercased(stores,"store_name")
stores<-uppercased(stores,"neighborhood")
stores$shopping_center_name = stores$shopping_center
stores$shopping_center<-NULL
stores<-uppercased(stores,"shopping_center_name")
#### Store names

stores$store_name<-str_replace(stores$store_name,"\\(CITGO\\)","\\@ CITGO")
stores$store_name<-str_replace(stores$store_name,"COGOS \\(VALERO\\)","COGOS @ VALERO")
stores$store_name<-str_replace(stores$store_name,"COGOS \\(EXXON\\)","COGOS @ EXXON")
stores$store_name<-str_replace(stores$store_name,"COGOS EXXON","COGOS @ EXXON")
stores$store_name<-str_replace(stores$store_name,"COGOS \\(SUNOCO\\)","COGOS @ SUNOCO")
stores$store_name<-str_replace(stores$store_name,"BP \\(COGOS\\)","COGOS @ BP")
stores$store_name<-str_replace(stores$store_name,"SUNOCO \\(COGOS\\)","COGOS @ SUNOCO")
stores$store_name<-str_replace(stores$store_name,"AMERICAN PRODUCT\\@BP","AMERICAN PRODUCT \\@ BP")
#stores$store_name<-str_replace(stores$store_name,"BASKIN ROBBIN","BASKIN ROBBINS")

stores$store_name<-str_replace(stores$store_name,"GET GO STORE","GET GO")
stores$store_name<-str_replace(stores$store_name,"GIANT EAGLE EXPRESS","GIANT EAGLE")
stores$store_name<-str_replace(stores$store_name,"GULF \\(7 ELEVEN\\)","7 ELEVEN \\@ GULF")
stores$store_name<-str_replace(stores$store_name,"KUHNS MARKET","KUHNS")
stores$store_name<-str_replace(stores$store_name,"KUHNS","KUHNS MARKET")

stores$store_name<-str_replace(stores$store_name,"MARATHON OIL","MARATHON")
stores$store_name<-str_replace(stores$store_name,"MARATHON TROCKI SERVICE","TROCKI SERVICE \\@ MARATHON")
stores$store_name<-str_replace(stores$store_name,"SUNOCO \\(TURNPIKE\\)","SUNOCO \\@ TURNPIKE")
stores$store_name<-str_replace(stores$store_name,"WALMART SUPERCENTER","WALMART")
stores$municipality<-str_replace(stores$municipality,"TWP","")

stores$municipality<-str_replace(stores$municipality,"E\\. MCKEESPORT","EAST MCKEESPORT")	
stores$municipality<-str_replace(stores$municipality,"GREENTREE","GREEN TREE")
stores$municipality<-str_replace(stores$municipality,"EAST LIBERTY","LIBERTY")	
stores$municipality<-str_replace(stores$municipality,"LIBERTY BORO","LIBERTY")	
stores$municipality<-str_replace(stores$municipality,"LINCOLN PLACE","LINCOLN")
stores$municipality<-str_replace(stores$municipality,"NEVILLE ISLAND","NEVILLE")
stores$municipality<-str_replace(stores$municipality,"PLUM BORO","PLUM")
stores$municipality<-str_replace(stores$municipality,"N\\. VERSAILLES","NORTH VERSAILLES")
stores$municipality<-trimws(stores$municipality)


store_municipalities_sel<-distinct(select(stores, municipality))
#stores1$municipality<-as.factor(stores1$municipality)

municipalities_with_stores<-inner_join(allegheny_municipalities, store_municipalities_sel)
municipalities_wo_stores<-anti_join(allegheny_municipalities, municipalities_with_stores)

stores$raw_store_name<-stores$store_name
stores$flagship_store_name<-NA


at_indices<-grep("@",stores$raw_store_name)
matching_records<-str_split_fixed(stores[at_indices,]$raw_store_name,' @ ',n=2)
stores[at_indices,]$flagship_store_name<-matching_records[,2]
stores[at_indices,]$store_name<-matching_records[,1]

stores$flagship_store_name<-as.factor(stores$flagship_store_name)
#unique(sort(as.character(stores$store_name)))


### Data classification (within shopping center)

stores$is_shopping_center<-ifelse(str_length(stores$shopping_center_name)==0,0,1)

store_locations<-select(stores,store_id,store_name,municipality,longitude,latitude)
head(store_locations)

#https://pmarchand1.github.io/atelier_rgeo/rgeo_workshop.html
head(store_locations)
stores_coords_known<-store_locations %>%
  filter(!is.na(latitude)) %>%
  filter(!is.na(longitude))

store_plots <- st_as_sf(stores_coords_known, coords = c("longitude", "latitude"), crs = st_crs('NAD83'))

stores_coords_missing<-anti_join(store_locations,stores_coords_known)

stores_coords_manual <- read_delim("Data/processed/store_coordinates_found.csv", 
    ";", escape_double = FALSE, trim_ws = TRUE)
stores_coords_to_fill<-select(stores_coords_manual,store_id,latitude,longitude)

# https://stackoverflow.com/questions/14563531/combine-column-to-remove-nas
stores_coords_fix<-left_join(stores, stores_coords_to_fill, "store_id") %>%
                 mutate(latitude = coalesce(latitude.x,latitude.y)) %>% 
                 mutate(longitude = coalesce(longitude.x,longitude.y)) %>% 
                 select(-c(latitude.x, latitude.y, longitude.x, longitude.y))

stores_coords_fix_known<-stores_coords_fix %>%
  filter(!is.na(latitude)) %>%
  filter(!is.na(longitude))

stores_coords_still_missing<-anti_join(stores,stores_coords_fix_known,by="store_id")

write.csv(stores_coords_still_missing,file.path(export_dir,"stores_coords_still_missing.csv"))