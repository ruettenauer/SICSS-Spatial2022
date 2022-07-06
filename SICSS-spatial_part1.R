pkgs <- c("sf", "gstat", "mapview", "nngeo", "rnaturalearth", "dplyr",
          "nomisr", "osmdata", "tidyr", "texreg") 
lapply(pkgs, require, character.only = TRUE)


sessionInfo()


# Coordinate pairs of two locations
coords1 <- c(51.752595, -1.262801)
coords2 <- c(51.753237, -1.253904)
coords <- rbind(coords1, coords2)

# Conventional data frame
nuffield.df <- data.frame(name = c("Nuffield College", "Radcliffe Camera"),
                          address = c("New Road", "Radcliffe Sq"),
                          lat = coords[,1], lon = coords[,2])

head(nuffield.df)

# Combine to spatial data frame
nuffield.spdf <- st_as_sf(nuffield.df, 
                          coords = c("lon", "lat"), # Order is important
                          crs = 4326) # EPSG number of CRS

# Map
mapview(nuffield.spdf, zcol = "name")


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
st_crs(world)

# Extract a country and plot in current CRS (WGS84)
ger.spdf <- world[world$name == "Germany", ]
plot(st_geometry(ger.spdf))

# Now, lets transform Germany into a CRS optimized for Iceland
ger_rep.spdf <- st_transform(ger.spdf, crs = 5325)
plot(st_geometry(ger_rep.spdf))


# Create subdir 
dn <- "_data"
ifelse(dir.exists(dn), "Exists", dir.create(dn))

# Download zip file and unzip
tmpf <- tempfile()
boundary.link <- "https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip"
download.file(boundary.link, tmpf)
unzip(zipfile = tmpf, exdir = paste0(dn))
unlink(tmpf)

# This is a shapefile
# We only need the MSOA layer for now
msoa.spdf <- st_read(dsn = paste0(dn, "/statistical-gis-boundaries-london/ESRI"),
                     layer = "MSOA_2011_London_gen_MHW" # Note: no file ending
                     )
head(msoa.spdf)

# Similarly you can read geo.json (usually more efficient, but less common)
# Download file
ulez.link <- "https://data.london.gov.uk/download/ultra_low_emissions_zone/3d980a29-c340-4892-8230-ed40d8c7f32d/Ultra_Low_Emissions_Zone.json"
download.file(ulez.link, paste0(dn, "/ulez.json"))

# Read geo.json
st_layers(paste0(dn, "/ulez.json"))
ulez.spdf <- st_read(dsn = paste0(dn, "/ulez.json")) # here dsn is simply the file
head(ulez.spdf)




mapview(msoa.spdf[, "POPDEN"])


### For larger request, register and set key
# Sys.setenv(NOMIS_API_KEY = "XXX")
# nomis_api_key(check_env = TRUE)

x <- nomis_data_info()

# Get London ids
london_ids <- msoa.spdf$MSOA11CD

### Get key statistics ids
# select requires tables (https://www.nomisweb.co.uk/sources/census_2011_ks)
# Let's get KS201EW (ethnic group) and KS402EW (housing tenure)

# Get internal ids
stats <- c("KS201EW", "KS402EW")
oo <- which(grepl(paste(stats, collapse = "|"), x$name.value))
ksids <- x$id[oo]
ksids # This are the internal ids


### look at meta information
q <- nomis_overview(ksids[1])
head(q)
a <- nomis_get_metadata(id = ksids[1], concept = "GEOGRAPHY", type = "type")
a # TYPE297 is MSOA level

b <- nomis_get_metadata(id = ksids[1], concept = "MEASURES", type = "TYPE297")
b # 20100 is the measure of absolute numbers


### Query data in loop over the required statistics
for(i in ksids){

  # Determin if data is divided by sex or urban-rural
  nd <- nomis_get_metadata(id = i)
  if("RURAL_URBAN" %in% nd$conceptref){
    UR <- TRUE
  }else{
    UR <- FALSE
  }
  if("C_SEX" %in% nd$conceptref){
    SEX <- TRUE
  }else{
    SEX <- FALSE
  }

  # make data request
  if(UR == TRUE){
    if(SEX == TRUE){
      tmp_en <- nomis_get_data(id = i, time = "2011", 
                               geography = london_ids, # replace with "TYPE297" for all MSOAs
                               measures = 20100, RURAL_URBAN = 0, C_SEX = 0)
    }else{
      tmp_en <- nomis_get_data(id = i, time = "2011", 
                               geography = london_ids, # replace with "TYPE297" for all MSOAs
                               measures = 20100, RURAL_URBAN = 0)
    }
  }else{
    if(SEX == TRUE){
      tmp_en <- nomis_get_data(id = i, time = "2011", 
                               geography = london_ids, # replace with "TYPE297" for all MSOAs
                               measures = 20100, C_SEX = 0)
    }else{
      tmp_en <- nomis_get_data(id = i, time = "2011", 
                               geography = london_ids, # replace with "TYPE297" for all MSOAs
                               measures = 20100)
    }

  }

  # Append (in case of different regions)
  ks_tmp <- tmp_en

  # Make lower case names
  names(ks_tmp) <- tolower(names(ks_tmp))
  names(ks_tmp)[names(ks_tmp) == "geography_code"] <- "msoa11"
  names(ks_tmp)[names(ks_tmp) == "geography_name"] <- "name"

  # replace weird cell codes
  onlynum <- which(grepl("^[[:digit:]]+$", ks_tmp$cell_code))
  if(length(onlynum) != 0){
    code <- substr(ks_tmp$cell_code[-onlynum][1], 1, 7)
    if(is.na(code)){
      code <- i
    }
    ks_tmp$cell_code[onlynum] <- paste0(code, "_", ks_tmp$cell_code[onlynum])
  }

  # save codebook
  ks_cb <- unique(ks_tmp[, c("date", "cell_type", "cell", "cell_code", "cell_name")])

  ### Reshape
  ks_res <- tidyr::pivot_wider(ks_tmp, id_cols = c("msoa11", "name"),
                               names_from = "cell_code",
                               values_from = "obs_value")

  ### Merge
  if(i == ksids[1]){
    census_keystat.df <- ks_res
    census_keystat_cb.df <- ks_cb
  }else{
    census_keystat.df <- merge(census_keystat.df, ks_res, by = c("msoa11", "name"), all = TRUE)
    census_keystat_cb.df <- rbind(census_keystat_cb.df, ks_cb)
  }

}


# Descirption are saved in the codebook
head(census_keystat_cb.df)

### Merge with MSOA geometries above
msoa.spdf <- merge(msoa.spdf, census_keystat.df, 
                   by.x = "MSOA11CD", by.y = "msoa11", all.x = TRUE)



msoa.spdf$per_white <- msoa.spdf$KS201EW_100 / msoa.spdf$KS201EW0001 * 100

mapview(msoa.spdf[, "per_white"])


# Download
pol.link <- "https://uk-air.defra.gov.uk/datastore/pcm/mapno22011.csv"
download.file(pol.link, paste0(dn, "/mapno22011.csv"))
pol.df <- read.csv(paste0(dn, "/mapno22011.csv"), skip = 5, header = T, sep = ",",
                      stringsAsFactors = F, na.strings = "MISSING")

head(pol.df)

# The data comes as point data with x and y as coordinates. 
# We have to transform this into spatial data first.
pol.spdf <- st_as_sf(pol.df, coords = c("x", "y"),
                    crs = 27700)

# Subsequently, we transform the point coordinates into a regular grid with "diameter" 500m
pol.spdf <- st_buffer(pol.spdf, dist = 500, nQuadSegs  = 1, 
                      endCapStyle = 'SQUARE')

# Plot NO2
plot(pol.spdf[, "no22011"], border = NA)

# First we create a bounding box of where we want to query data
# st_bbox() can be used to get bounding boxes of an existing spatial object (needs CRS = 4326)
# q <- opq(bbox = 'greater london uk')
q <- opq(bbox = st_bbox(st_transform(msoa.spdf, 4326)))

# Lets get the location of pubs in London
osmq <- add_osm_feature(q, key = "amenity", value = c("pub", "bar"))

# Query
pubs.osm <- osmdata_sf(osmq)

# Right now there are some results in polygons, some in points, and they overlap
# Make unique points / polygons 
pubs.osm <- unique_osmdata(pubs.osm)

# Get points and polygons (there are barly any pubs as polygons, so we ignore for now)
pubs.points <- pubs.osm$osm_points
pubs.polys <- pubs.osm$osm_multipolygons

mapview(pubs.points)

# Drop OSM file
rm(pubs.osm); gc()

# Reduce to point object only 
pubs.spdf <- pubs.points



st_crs(msoa.spdf) == st_crs(pol.spdf)
st_crs(msoa.spdf) == st_crs(pubs.spdf)
st_crs(msoa.spdf) == st_crs(ulez.spdf)


# MSOA in different crs --> transform
pol.spdf <- st_transform(pol.spdf, crs = st_crs(msoa.spdf))
pubs.spdf <- st_transform(pubs.spdf, crs = st_crs(msoa.spdf))
ulez.spdf <- st_transform(ulez.spdf, crs = st_crs(msoa.spdf))


# Check if all geometries are valid, and make valid if needed
msoa.spdf <- st_make_valid(msoa.spdf)



# Subset to pollution estimates in London
pol_sub.spdf <- pol.spdf[msoa.spdf, ] # or:
pol_sub.spdf <- st_filter(pol.spdf, msoa.spdf)
mapview(pol_sub.spdf)


# Subset pubs to pubs not in the ulez area
sub2.spdf <- pubs.spdf[ulez.spdf, , op = st_disjoint] # or:
sub2.spdf <- st_filter(pubs.spdf, ulez.spdf, .predicate = st_disjoint)
mapview(sub2.spdf)

# We can easily create indicators of whether an MSOA is within ulez or not
msoa.spdf$ulez <- 0
within <- msoa.spdf[ulez.spdf,]
msoa.spdf$ulez[which(msoa.spdf$MSOA11CD %in% within$MSOA11CD)] <- 1
table(msoa.spdf$ulez)


# Assign MSOA to each point
pubs_msoa.join <- st_join(pubs.spdf, msoa.spdf, join = st_within)

# Count N by MSOA code (drop geometry to speed up)
pubs_msoa.join <- dplyr::count(st_drop_geometry(pubs_msoa.join),
                               MSOA11CD = pubs_msoa.join$MSOA11CD,
                               name = "pubs_count")
sum(pubs_msoa.join$pubs_count)

# Merge and replace NAs with zero (no matches, no pubs)
msoa.spdf <- merge(msoa.spdf, pubs_msoa.join,
                   by = "MSOA11CD", all.x = TRUE)
msoa.spdf$pubs_count[is.na(msoa.spdf$pubs_count)] <- 0


# Use geometric centroid of each MSOA
cent.sp <- st_centroid(msoa.spdf[, "MSOA11CD"])

# Get K nearest neighbour with distance
knb.dist <- st_nn(cent.sp, pubs.spdf,
                  k = 1, returnDist = TRUE,
                  progress = FALSE)
msoa.spdf$dist_pubs <- unlist(knb.dist$dist)
summary(msoa.spdf$dist_pubs)


# Create buffer (1km radius)
cent.buf <- st_buffer(cent.sp, dist = 1000)
mapview(cent.buf)

# Add area of each buffer (in this constant) 
cent.buf$area <- as.numeric(st_area(cent.buf))

# Calculate intersection of pollution grid and buffer
int.df <- st_intersection(cent.buf, pol.spdf)
int.df$int_area <- as.numeric(st_area(int.df)) # area of intersection

# Area of intersection as share of buffer
int.df$area_per <- int.df$int_area / int.df$area

# Aggregate as weighted mean
int.df <- st_drop_geometry(int.df)
int.df$no2_weighted <- int.df$no22011 * int.df$area_per
int.df <- aggregate(list(no2 = int.df[, "no2_weighted"]), 
                    by = list(MSOA11CD = int.df$MSOA11CD),
                    sum)

# Merge back to spatial data.frame
msoa.spdf <- merge(msoa.spdf, int.df, by = "MSOA11CD", all.x = TRUE)

mapview(msoa.spdf[, "no2"])


# Define ethnic group shares
msoa.spdf$per_mixed <- msoa.spdf$KS201EW_200 / msoa.spdf$KS201EW0001 * 100
msoa.spdf$per_asian <- msoa.spdf$KS201EW_300 / msoa.spdf$KS201EW0001 * 100
msoa.spdf$per_black <- msoa.spdf$KS201EW_400 / msoa.spdf$KS201EW0001 * 100
msoa.spdf$per_other <- msoa.spdf$KS201EW_500 / msoa.spdf$KS201EW0001 * 100

# Define tenure
msoa.spdf$per_owner <- msoa.spdf$KS402EW_100 / msoa.spdf$KS402EW0001 * 100
msoa.spdf$per_social <- msoa.spdf$KS402EW_200 / msoa.spdf$KS402EW0001 * 100


# Run regression
mod1.lm <- lm(no2 ~ per_mixed + per_asian + per_black + per_other +
                per_owner + per_social + pubs_count + POPDEN + ulez,
              data = msoa.spdf)

# summary
screenreg(list(mod1.lm), digits = 3)


# Save
save(msoa.spdf, file = "_data/msoa_spatial.RData")
save(ulez.spdf, file = "_data/ulez_spatial.RData")



# Download traffic
traffic.link <- "https://storage.googleapis.com/dft-statistics/road-traffic/downloads/aadf/region_id/dft_aadf_region_id_6.csv"
download.file(traffic.link, paste0(dn, "/traffic.csv"))

# Read and select major roads only
traffic.df <- read.csv(paste0(dn, "/traffic.csv"))
traffic.df <- traffic.df[which(traffic.df$year == 2011 &
                       traffic.df$road_type == "Major"), ]
names(traffic.df)

# Transform to spatial data
traffic.spdf <- st_as_sf(traffic.df,
                         coords = c("longitude", "latitude"),
                         crs = 4326)

# Transform into common crs
traffic.spdf <- st_transform(traffic.spdf,
                             crs = st_crs(msoa.spdf))

# Map
mapview(traffic.spdf[, "all_motor_vehicles"])

# Save (for later exercise)
save(traffic.spdf, file = "_data/traffic.RData")


# Set up regular grid over London with 1km cell size
london.sp <- st_union(st_geometry(msoa.spdf))
grid <- st_make_grid(london.sp, cellsize = 1000)
mapview(grid)

# Reduce to London bounds
grid <- grid[london.sp]
mapview(grid)



# IDW interpolation
all_motor.idw <- idw(all_motor_vehicles ~ 1,
                     locations = traffic.spdf,
                     newdata = grid,
                     idp = 2) # power of distance decay
mapview(all_motor.idw[, "var1.pred"])

# Variogram
all_motor.var <- variogram(all_motor_vehicles ~ 1,
                          traffic.spdf)
plot(all_motor.var)

# Fit variogram
all_motor.varfit <- fit.variogram(all_motor.var,
                                  vgm(c("Nug", "Exp")), # use vgm() to see options
                                  fit.kappa = TRUE, fit.sills = TRUE, fit.ranges = TRUE)

plot(all_motor.var, all_motor.varfit)

# Parameters
all_motor.varfit


### Ordinary Kriging 
#(ATTENTION: This can take some time (~3min with current setting))

all_motor.kg <- krige(all_motor_vehicles ~ 1,
                      locations = traffic.spdf,
                      newdata = grid,
                      model = all_motor.varfit)

# Look at results
mapview(all_motor.kg[, "var1.pred"])


# Calculate intersection
smoa_grid.int <- st_intersection(msoa.spdf, all_motor.kg)

# average per MSOA
smoa_grid.int <- aggregate(list(traffic = smoa_grid.int$var1.pred),
                         by = list(MSOA11CD = smoa_grid.int$MSOA11CD),
                         mean)

# Merge back
msoa.spdf <- merge(msoa.spdf, smoa_grid.int, by = "MSOA11CD", all.x = TRUE)

