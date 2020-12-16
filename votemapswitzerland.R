# Load packages ------------------------------------------------------------------------
source("get_geodata.R")
library(swissdd)
library(dplyr)
library(ggplot2)
library(sf)
library(tibble)
library(purrr)
library(transformr)
library(tweenr)
library(tidyr)
library(magick)

# Load data ----------------------------------------------------------------------------

# Geodata
gd <- get_geodata() %>% 
  mutate(id = as.numeric(mun_id)) %>% 
  arrange(id) %>% 
  select(-id)

# Vote data
vd <- get_nationalvotes() %>% 
  mutate(id2 = as.numeric(mun_id)) %>% 
  arrange(id2) %>% 
  select(-id2) %>% 
  filter(id == 6360) %>% 
  # filter(canton_id == "2")

# State at start
start <- gd %>% 
  filter(mun_id %in% vd$mun_id) %>% 
  rename(id = mun_id)

start %>% 
  ggplot() +
  geom_sf()

# Prep function inputs (specify radius factor)
radii <- vd %>% 
  filter(mun_id %in% start$id) %>% 
  select(mun_id, gueltigeStimmen) %>% 
  mutate(radius = sqrt(3000*gueltigeStimmen / pi)) %>% 
  arrange(as.numeric(mun_id)) %>% 
  pull(radius) 

yes_share <- vd %>% 
  filter(mun_id %in% start$id) %>% 
  select(mun_id, jaStimmenInProzent) %>% 
  arrange(as.numeric(mun_id))

ids <- yes_share$mun_id

# Transformation Pt. 1: Polygons to circles --------------------------------------------

# Function to draw circles
draw_circle <- function(id, centre_x = 0, centre_y = 0, radius = 1000, detail = 360, st = TRUE) {
  
  i <- seq(0, 2 * pi, length.out = detail + 1)[-detail - 1]
  x <- centre_x + (radius * sin(i))
  y <- centre_y + (radius * cos(i))
  
  if (st) {
    
    cir <- st_polygon(list(cbind(x, y)[c(seq_len(detail), 1), , drop = FALSE]))
    d <- st_sf(data.frame(id = id, geom = st_sfc(cir)))

  } else {
    
    d <- tibble(id = id, x = x, y = y)
    
  }
  
  return(d)
  
}

# Draw circles
centroids <- as_tibble(st_coordinates(st_centroid(start$geometry)))
end <- pmap_dfr(list(ids, centroids$X, centroids$Y, radii), draw_circle)

end %>% 
  ggplot() +
  geom_sf()

# Transformation data
td <- tween_sf(start, end, ease = "cubic-in-out", nframes = 40, id = id)

# Avoid overlaps -----------------------------------------------------------------------

# Functions
disperse_around_municipality <- function(data, mun_name, intensity = 40) {
  
  #Preparation
  centroids <- suppressWarnings(st_centroid(data))
  distances <- as_tibble(st_distance(centroids))
  names(distances) <- data$id
  
  ## Disperse from Zurich: Distance
  id <- vd$mun_id[vd$mun_name == mun_name]
  distance <- distances[[id]] / 1000
  
  ## Disperse from Zurich: Directions
  centroidsXY <- centroids %>% 
    st_coordinates() %>% 
    as_tibble() %>% 
    mutate(id = end$id)
  
  direction_x <- (centroidsXY$X - centroidsXY$X[centroidsXY$id == id]) / distance
  direction_y <- (centroidsXY$Y - centroidsXY$Y[centroidsXY$id == id]) / distance
  
  ## Disperse from Zurich
  for (i in c(1:nrow(data))[-which(distance == 0)]) {
    data$geometry[i] <- data$geometry[i] + c(
      direction_x[i] * (intensity / distance[i]), 
      direction_y[i] * (intensity / distance[i])
    )
  }
  
  # Return
  return(data)
  
  
  
}
get_collisions <- function(data) {
  
  collisions <- data %>% 
    st_intersects(sparse = F) %>% 
    as_tibble() %>% 
    mutate(id = ids)
  
  names(collisions) <- c(ids, "id")
  
  collisions <- collisions %>% 
    pivot_longer(cols = -id, names_to = "id2") %>% 
    filter(!id == id2) %>% 
    filter(value) %>% 
    select(id, id2)
  
  return(collisions)
  
}
solve_collisions <- function(id1, id2, data, stabilize = 5000, stabilize_factor = 0.1) {
  
  # Base
  polygons <- bind_rows(data %>% filter(id == id1), data %>% filter(id == id2))
  radii <- ceiling(sqrt(st_area(polygons) / pi))
  centroids <- suppressWarnings(st_centroid(polygons))
  centroidsXY <- st_coordinates(centroids)
  distance <- max(st_distance(centroids))
  
  # Displacement Params
  disp_mag <- sum(radii) - distance
  disp_dir_x <- (centroidsXY[2,1] - centroidsXY[1,1]) / distance
  disp_dir_y <- (centroidsXY[2,2] - centroidsXY[1,2]) / distance
  
  # Polygons
  polygon1 <- polygons %>% 
    filter(id == id1) %>% 
    st_geometry()
  
  polygon2 <- polygons %>% 
    filter(id == id2) %>% 
    st_geometry()
  
  # Update Polygons
  if (max(radii) >= stabilize) {
    
    if (radii[1] >= stabilize & radii[2] >= stabilize) {
      
      if (radii[1] >= radii[2]) {
        
        polygon1_t <- polygon1 + c(-disp_dir_x * disp_mag * stabilize_factor, -disp_dir_y * (disp_mag * stabilize_factor))
        polygon2_t <- polygon2 + c(disp_dir_x * disp_mag * (1 - stabilize_factor), disp_dir_y * disp_mag * (1 - stabilize_factor))
        
      } else {
        
        polygon1_t <- polygon1 + c(-disp_dir_x * disp_mag * (1 - stabilize_factor), -disp_dir_y * disp_mag * (1 - stabilize_factor))
        polygon2_t <- polygon2 + c(disp_dir_x * disp_mag * stabilize_factor, disp_dir_y * disp_mag * stabilize_factor)
        
      }
      
    } else {
      
      if (radii[1] >= stabilize) {
        
        polygon1_t <- polygon1
        polygon2_t <- polygon2 + c(disp_dir_x * disp_mag, disp_dir_y * disp_mag)
        
      } else {
        
        polygon1_t <- polygon1 + c(-disp_dir_x * disp_mag, -disp_dir_y * disp_mag / 2)
        polygon2_t <- polygon2
        
      }
      
    }
    
  } else {
    
    polygon1_t <- polygon1 + c(-disp_dir_x * (disp_mag / 2), -disp_dir_y * (disp_mag / 2))
    polygon2_t <- polygon2 + c(disp_dir_x * (disp_mag / 2), disp_dir_y * (disp_mag / 2))
    
  }
  
  # Return
  output <- st_sf(data.frame(id = c(id1, id2), geom = st_sfc(c(polygon1_t, polygon2_t))))
  
  # Return
  return(output)
  
}

# Disperse Zurich area to achieve faster convergence
if ("261" %in% end$id & "261" %in% end$id) {
  
  end <- disperse_around_municipality(end, "Zürich", 40)
  end <- disperse_around_municipality(end, "Winterthur", 10)
  end %>% ggplot() + geom_sf()
  
}

# Initialize overlap avoidance
end2 <- end
num_overlaps <- nrow(get_collisions(end2)) / 2
counter <- 0

# Solve overlaps (terribly slow and inefficient)
while (num_overlaps > 0) {
  
  # Check for collisions
  collisions <- get_collisions(end2)
  
  # Update & Feedback
  num_overlaps <- nrow(collisions) / 2  
  cat("Improvement attempts:", counter, "|| Collisions found:", num_overlaps, "\n")
  
  # Select a single collision per polygon at a time
  collisions <- collisions %>% 
    group_by(id2) %>% 
    slice(1) %>% 
    group_by(id) %>% 
    slice(1) %>% 
    mutate(joint_id = ifelse(id < id2, paste0(id, id2), paste0(id2, id))) %>% 
    group_by(joint_id) %>% 
    slice(1) %>% 
    ungroup()
  
  # Solve collisions
  solved <- map2_dfr(collisions$id, collisions$id2, solve_collisions, end2)
  counter <- counter + 1
  
  # Update data
  for (i in 1:nrow(solved)) st_geometry(end2)[end2$id == solved[["id"]][i]] = solved[["geometry"]][i]
  
}
end2 %>% ggplot() + geom_sf()

# Transformation Pt. 2: Update circle positions ----------------------------------------
td2 <- tween_sf(td, end2, ease = "cubic-in-out", nframes = 20, id = id) %>% 
  keep_state(nframes = 40)

# Enrich transformation data
td2 <- left_join(td2, yes_share, by = c("id" = "mun_id"))

# Plot transformation -----------------------------------------------------------------

# Define plot function
plot_data <- function(data, pos, xlim, ylim) {
  
  # Recoding yes shares
  data <- data %>% 
    mutate(
      stimmen = factor(case_when(
        jaStimmenInProzent < 35 ~ "",
        jaStimmenInProzent >= 35 & jaStimmenInProzent < 40 ~ "35", 
        jaStimmenInProzent >= 40 & jaStimmenInProzent < 45 ~ "40",
        jaStimmenInProzent >= 45 & jaStimmenInProzent < 50 ~ "45",
        jaStimmenInProzent >= 50 & jaStimmenInProzent < 55 ~ "50",
        jaStimmenInProzent >= 55 & jaStimmenInProzent < 60 ~ "55",
        jaStimmenInProzent >= 60 & jaStimmenInProzent < 65 ~ "60",
        jaStimmenInProzent >= 65 ~ "65"
        ), levels = c("", "35", "40", "45", "50", "55", "60", "65")
      )
    )
  
  # Plot
  p <- ggplot(data$geometry) +
    geom_sf(aes(fill = data$stimmen), color = NA) +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_manual(
    values = c(
      "#8d0613", "#c91022", "#f1434a", "#ff9193",
      "#91cdff", "#42a2f1", "#1a7bc5", "#105182"
      )
    ) +
    theme_void() +
    theme(legend.position = "none")
  
  # Updates
  if (!pos %% 10 == 0) cat(".")
  if (pos %% 10 == 0) cat(pos, "frames\n")
  
  # Print
  print(p)
  
  }

# Define limits
xlim <- c(0.99 * min(st_coordinates(end2$geometry)[,1]), 1.01 * max(st_coordinates(end2$geometry)[,1]))
ylim <- c(0.99 * min(st_coordinates(end2$geometry)[,2]), 1.01 * max(st_coordinates(end2$geometry)[,2]))

# Plots
img <- image_graph(res = 96)
datalist <- split(td2, td2$.frame)
out <- map2(datalist, 1:length(datalist), plot_data, xlim, ylim)
dev.off()

# Make gif
animation <- image_animate(img, fps = 10)
image_write(animation, "animation2.gif")