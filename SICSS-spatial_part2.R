pkgs <- c("sf", "mapview", "nngeo", "tmap", "tmaptools", "viridisLite", 
          "ggplot2", "ggthemes", "rmapshaper") 
lapply(pkgs, require, character.only = TRUE)


sessionInfo()


load(file = "_data/msoa_spatial.RData")
load(file = "_data/ulez_spatial.RData")


# Define colours
cols <- viridis(n = 7, direction = 1, option = "C")

mp1 <-  tm_shape(msoa.spdf) + 
  tm_fill(col = "no2", 
          style = "fisher", # algorithm to def cut points
          n = 7, # Number of requested cut points
          palette = cols, # colours
          alpha = 1, # transparency 
          title = "NO2", 
          legend.hist = FALSE # histogram next to map?
          ) +
  tm_borders(col = "white", lwd = 0.5, alpha = 0.5) 

mp1



# Define colours
cols <- viridis(n = 7, direction = 1, option = "C")

mp1 <-  tm_shape(msoa.spdf) + 
  tm_fill(col = "no2", 
          style = "fisher", # algorithm to def cut points
          n = 7, # Number of requested cut points
          palette = cols, # colours
          alpha = 1, # transparency 
          title = "NO2", 
          legend.hist = FALSE # histogram next to map?
          ) +
  tm_borders(col = "white", lwd = 0.5, alpha = 0.5) +
  tm_shape(ulez.spdf) +
  tm_borders(col = "red", lwd = 1, alpha = 1) 

mp1



# Define colours
cols <- viridis(n = 7, direction = 1, option = "C")

mp1 <-  tm_shape(msoa.spdf) + 
  tm_fill(col = "no2", 
          style = "fisher", # algorithm to def cut points
          n = 7, # Number of requested cut points
          palette = cols, # colours
          alpha = 1, # transparency 
          title = expression('in'~mu*'g'/m^{3}), 
          legend.hist = FALSE # histogram next to map?
          ) +
  tm_borders(col = "white", lwd = 0.5, alpha = 0.5) +
  tm_shape(ulez.spdf) +
  tm_borders(col = "red", lwd = 1, alpha = 1) +
  tm_layout(frame = FALSE,
            legend.frame = TRUE, legend.bg.color = TRUE,
            legend.position = c("right", "bottom"),
            legend.outside = FALSE,
            main.title = "NO2", 
            main.title.position = "center",
            main.title.size = 1.6,
            legend.title.size = 0.8,
            legend.text.size = 0.8)

mp1


# Save old projection
crs_orig <- st_crs(msoa.spdf)

# Change projection
ulez.spdf <- st_transform(ulez.spdf, 4326)
msoa.spdf <- st_transform(msoa.spdf, 4326)

# Get OSM data for background
osm_tmp <- read_osm(st_bbox(msoa.spdf), ext = 1, 
                    type = "stamen-toner", zoom = NULL) 

# Define colours
cols <- viridis(n = 7, direction = 1, option = "C")

mp1 <-  tm_shape(osm_tmp) + tm_rgb() +
  tm_shape(msoa.spdf) + 
  tm_fill(col = "no2", 
          style = "fisher", # algorithm to def cut points
          n = 7, # Number of requested cut points
          palette = cols, # colours
          alpha = 0.8, # transparency 
          title = expression('in'~mu*'g'/m^{3}), 
          legend.hist = FALSE # histogram next to map?
          ) +
  #tm_borders(col = "white", lwd = 0.5, alpha = 0.5) +
  tm_shape(ulez.spdf) +
  tm_borders(col = "red", lwd = 1, alpha = 1) +
  tm_layout(frame = FALSE,
            legend.frame = TRUE, legend.bg.color = TRUE,
            legend.position = c("right", "bottom"),
            legend.outside = FALSE,
            main.title = "NO2", 
            main.title.position = "center",
            main.title.size = 1.6,
            legend.title.size = 0.8,
            legend.text.size = 0.8)

mp1


# Define colours
cols1 <- viridis(n = 7, direction = 1, option = "C")

# Define colours
cols2 <- viridis(n = 7, direction = 1, option = "D")

mp1 <-  tm_shape(osm_tmp) + tm_rgb() +
  tm_shape(msoa.spdf) + 
  tm_fill(col = "no2", 
          style = "fisher", # algorithm to def cut points
          n = 7, # Number of requested cut points
          palette = cols1, # colours
          alpha = 0.8, # transparency 
          title = expression('in'~mu*'g'/m^{3}), 
          legend.hist = FALSE # histogram next to map?
          ) +
  #tm_borders(col = "white", lwd = 0.5, alpha = 0.5) +
  tm_shape(ulez.spdf) +
  tm_borders(col = "red", lwd = 1, alpha = 1) +
  tm_layout(frame = FALSE,
            legend.frame = TRUE, legend.bg.color = TRUE,
            legend.position = c("right", "bottom"),
            legend.outside = FALSE,
            main.title = "NO2", 
            main.title.position = "center",
            main.title.size = 1.4,
            legend.title.size = 0.8,
            legend.text.size = 0.8)

mp2 <-  tm_shape(osm_tmp) + tm_rgb() +
  tm_shape(msoa.spdf) + 
  tm_fill(col = "per_black", 
          style = "fisher", # algorithm to def cut points
          n = 7, # Number of requested cut points
          palette = cols2, # colours
          alpha = 0.8, # transparency 
          title = "% white", 
          legend.hist = FALSE # histogram next to map?
          ) +
  #tm_borders(col = "white", lwd = 0.5, alpha = 0.5) +
  tm_shape(ulez.spdf) +
  tm_borders(col = "red", lwd = 1, alpha = 1) +
  tm_layout(frame = FALSE,
            legend.frame = TRUE, legend.bg.color = TRUE,
            legend.position = c("right", "bottom"),
            legend.outside = FALSE,
            main.title = "Ethnic Black inhabitants", 
            main.title.position = "center",
            main.title.size = 1.4,
            legend.title.size = 0.8,
            legend.text.size = 0.8)

tmap_arrange(mp1, mp2, ncol = 2, nrow = 1)

png(file = paste("London.png", sep = ""), width = 14, height = 7, units = "in", 
    res = 100, bg = "white", family = "Times New Roman")
par(mar=c(0,0,3,0))
par(mfrow=c(1,1),oma=c(0,0,0,0))
tmap_arrange(mp1, mp2, ncol = 2, nrow = 1)
dev.off()

gp <- ggplot(msoa.spdf)+
    geom_sf(aes(fill = no2))+
    scale_fill_viridis_c(option = "B")+
    coord_sf(datum = NA)+
    theme_map()+
    theme(legend.position = c(.9, .6))
gp

# Get some larger scale boundaries
borough.spdf <- st_read(dsn = paste0("_data", "/statistical-gis-boundaries-london/ESRI"),
                     layer = "London_Borough_Excluding_MHW" # Note: no file ending
                     )

# transform to only inner lines
borough_inner <- ms_innerlines(borough.spdf)

# Plot with inner lines
gp <- ggplot(msoa.spdf)+
    geom_sf(aes(fill = no2), color = NA)+
    scale_fill_viridis_c(option = "A")+
    geom_sf(data = borough_inner, color = "gray92")+
    geom_sf(data = ulez.spdf, color = "red", fill = NA)+
    coord_sf(datum = NA)+
    theme_map()+
    labs(fill = "NO2")+
    theme(legend.position = c(.9, .6))
gp
