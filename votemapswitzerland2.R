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
library(particles)
library(igraph)
library(tidygraph)
library(stringr)
library(hrbrthemes)

# Load data ----------------------------------------------------------------------------

# Geodata
gd_mun <- get_geodata("municipality", latest = F)
gd_nat <- get_geodata("national", latest = F)

# Vote data
vd <- get_nationalvotes(votedates = "2020-09-27") %>% filter(id == 6320)

# Set length of different animation phases ---------------------------------------------

p1 <- 10
t1 <- 30
p2 <- 20
t2 <- 30
p3 <- 20
t3 <- 30
p4 <- 40

tot_p <- p1 + p2 + p3 + p4
tot_t <- t1 + t2 + t3
tot <- tot_p + tot_t

# Build grid from area size of municipalities with yes majority -----------------------

# Ranking of the municipalities according to area size
ranking_area <- tibble(
  mun_id = gd_mun$mun_id,
  area = st_area(gd_mun) / 1000 ^ 2
  ) %>% 
  left_join(vd %>% select(mun_id, jaStimmenInProzent), by = "mun_id") %>% 
  mutate(yes = ifelse(jaStimmenInProzent >= 50, 1, 0)) %>% 
  group_by(yes) %>% 
  mutate(rank_area = rank(1/area, ties.method = "first")) %>% 
  ungroup() %>% 
  filter(!is.na(jaStimmenInProzent))

# Limits Swiss map
grid_lim <- st_coordinates(gd_nat) %>% 
  as_tibble() %>% 
  summarise(across(c("X", "Y"), list(min = min, max = max)))

# Define grid limits 
grid_X_lim_yes <- c(grid_lim$X_min - 250000, grid_lim$X_min + (grid_lim$X_max - grid_lim$X_min)/2 - 20000)
grid_Y_lim_yes <- c(0.6 * grid_lim$Y_min, 1.4 * grid_lim$Y_max)
X_total_yes <- grid_X_lim_yes[2] - grid_X_lim_yes[1]
Y_total_yes <- grid_Y_lim_yes[2] - grid_Y_lim_yes[1]

# Geometries ranked by area 
gd_mun_area_yes <- gd_mun %>% 
  left_join(ranking_area, by = "mun_id") %>% 
  filter(yes == 1) %>% 
  arrange(rank_area)

# Extension (lon/lat) of municipalities
mun_ext_yes <- gd_mun_area_yes %>%
  st_coordinates() %>%
  as_tibble() %>%
  group_by(L3) %>% 
  summarise(across(c("X", "Y"), list(min = min, max = max))) %>% 
  mutate(
    X_ext = X_max - X_min,
    Y_ext = Y_max - Y_min
  ) %>% 
  mutate(mun_id = gd_mun_area_yes$mun_id) %>%
  select(-L3) 
  
# Function to add a constant (padding) to a cumulative sum
sum_padding <- function(..., padding = 4000) sum(...) + padding 

# Cumulative extensions, allocation to row and definition of row heigth
mun_ext_rows_yes <- mun_ext_yes %>% 
  mutate(X_cum = accumulate(X_ext, sum_padding)) %>% 
  mutate(row = ceiling(X_cum / X_total_yes)) %>% 
  group_by(row) %>% 
  mutate(Y_row = max(Y_ext) + 3000)

# Join with geodata
gd_mun_ext_rows_yes <- left_join(gd_mun_area_yes, mun_ext_rows_yes, by = "mun_id")

# Function to define exact postions of municipalities after transformation
points2dfr <- function(id, x, y) st_sf(tibble(mun_id = id, geom = st_sfc(st_point(c(x, y)))))
get_new_positions <- function(row_id, data, grid_X_lim, grid_Y_lim, padding_edge = 5000) {
  
  # State at start
  mun_start <- data %>% 
    filter(row == row_id) %>% 
    mutate(
      X_center = (X_min + X_max) / 2,
      Y_center = (Y_min + Y_max) / 2
    )
  
  # Calculate padding between polygons 
  padding_between <- ((grid_X_lim[2] - grid_X_lim[1]) - sum(mun_start$X_ext) - 2 * padding_edge) / nrow(mun_start)
    
  # Calculate coordinates of new center
  center_new_X <- grid_X_lim[1] + padding_edge + accumulate(mun_start$X_ext, sum_padding, padding = padding_between) - mun_start$X_ext/2
  
  row_heigths <- data %>% 
    st_drop_geometry() %>% 
    filter(row <= row_id) %>%
    group_by(row) %>% 
    slice(1) %>% 
    pull(Y_row)
  
  center_new_Y <- grid_Y_lim[2] - padding_edge - sum_padding(row_heigths, padding = padding_edge) + row_heigths[length(row_heigths)]/2
  
  # Build dfr from center points
  centers_new <- pmap_dfr(list(mun_start$mun_id, center_new_X, center_new_Y), points2dfr)
  centers_old <- pmap_dfr(list(mun_start$mun_id, mun_start$X_center, mun_start$Y_center), points2dfr)
  
  # Define distance and direction
  distance <- st_distance(centers_old, centers_new, by_element = T)
  direction_X <- (st_coordinates(centers_new)[,1] - st_coordinates(centers_old)[,1]) / distance
  direction_Y <- (st_coordinates(centers_new)[,2] - st_coordinates(centers_old)[,2]) / distance
  
  # Translation of polygons
  mun_end <- mun_start
  for (i in 1:nrow(mun_end)) {
    mun_end$geometry[i] <- mun_end$geometry[i] + c(
      direction_X[i] * distance[i], 
      direction_Y[i] * distance[i]
    )
  }
  
  # Updates
  if (!row_id %% 10 == 0) cat(".")
  if (row_id %% 10 == 0) cat(row_id, "rows\n")
  
  # Return
  mun_end <- mun_end %>% select(mun_id, area, rank_area)
  return(mun_end)
}

# Get new positions for all rows
step2_yes <- map_dfr(
  unique(gd_mun_ext_rows_yes$row), 
  get_new_positions, 
  gd_mun_ext_rows_yes,
  grid_X_lim_yes, 
  grid_Y_lim_yes, 
  padding_edge = 2000
  )

# Build grid from area size of municipalities with no majority ------------------------

# Define grid limits 
grid_X_lim_no <- c(grid_X_lim_yes[2] + 40000, grid_lim$X_max + 250000)
grid_Y_lim_no <- grid_Y_lim_yes
X_total_no <- grid_X_lim_no[2] - grid_X_lim_no[1]
Y_total_no <- grid_Y_lim_no[2] - grid_Y_lim_no[1]

# Geometries ranked by area 
gd_mun_area_no <- gd_mun %>% 
  left_join(ranking_area, by = "mun_id") %>% 
  filter(yes == 0) %>% 
  arrange(rank_area)

# Extension (lon/lat) of municipalities
mun_ext_no <- gd_mun_area_no %>%
  st_coordinates() %>%
  as_tibble() %>%
  group_by(L3) %>% 
  summarise(across(c("X", "Y"), list(min = min, max = max))) %>% 
  mutate(
    X_ext = X_max - X_min,
    Y_ext = Y_max - Y_min
  ) %>% 
  mutate(mun_id = gd_mun_area_no$mun_id) %>%
  select(-L3) 

# Cumulative extensions, allocation to row and definition of row heigth
mun_ext_rows_no <- mun_ext_no %>% 
  mutate(X_cum = accumulate(X_ext, sum_padding)) %>% 
  mutate(row = ceiling(X_cum / X_total_no)) %>% 
  group_by(row) %>% 
  mutate(Y_row = max(Y_ext) + 3000)

# Join with geodata
gd_mun_ext_rows_no <- left_join(gd_mun_area_no, mun_ext_rows_no, by = "mun_id")

# Get new positions for all rows
step2_no <- map_dfr(
  unique(gd_mun_ext_rows_no$row), 
  get_new_positions, 
  gd_mun_ext_rows_no,
  grid_X_lim_no, 
  grid_Y_lim_no, 
  padding_edge = 2000
  )

# Transformation 1: Order municipalities by area size ----------------------------------

# Scale map for better start picture
step1 <- bind_rows(gd_mun_area_yes, gd_mun_area_no) %>% 
  select(mun_id) %>% 
  mutate(geometry = geometry * 2 - (st_centroid(gd_nat$geometry * 2) - st_centroid(gd_nat$geometry)))

step2 <- bind_rows(step2_yes, step2_no) %>% select(mun_id)

# Transformation data
td1 <- tween_sf(step1, step1, ease = "linear", nframes = p1, id = mun_id) %>% 
  tween_sf(step2, ease = "cubic-in-out", nframes = t1, id = mun_id) %>% 
  keep_state(nframes = p2)

# Get circles proportional to the number of votes cast ---------------------------------

# Get center coordinates
step3_centers <- step2 %>% 
  st_coordinates() %>%
  as_tibble() %>%
  group_by(L3) %>% 
  summarise(across(c("X", "Y"), list(min = min, max = max))) %>% 
  mutate(
    X_center = (X_min + X_max) / 2,
    Y_center = (Y_min + Y_max) / 2
  ) %>% 
  mutate(mun_id = step2$mun_id) %>% 
  select(mun_id, X_center, Y_center) %>% 
  left_join(vd %>% select(mun_id, gueltigeStimmen), by = "mun_id")

# Define conversion factor votes to area
votes_total <- sum(vd$gueltigeStimmen[vd$mun_id %in% step3_centers$mun_id])
area_total <- st_area(gd_nat)
conv_fac <- 2 * area_total / votes_total

# Define circle radius from vote
step3_circle_data <- step3_centers %>% 
  mutate(radius = sqrt(conv_fac * gueltigeStimmen / pi))

# Function to draw circles
draw_circle <- function(id, centre_x = 0, centre_y = 0, radius = 1000, detail = 360) {
  
  i <- seq(0, 2 * pi, length.out = detail + 1)[-detail - 1]
  x <- centre_x + (radius * sin(i))
  y <- centre_y + (radius * cos(i))
  
  cir <- st_polygon(list(cbind(x, y)[c(seq_len(detail), 1), , drop = FALSE]))
  d <- st_sf(data.frame(mun_id = id, geom = st_sfc(cir)))
  
  return(d)
  
}

# Draw circles
step3 <- pmap_dfr(
  list(
    step3_circle_data$mun_id, 
    step3_circle_data$X_center, 
    step3_circle_data$Y_center, 
    step3_circle_data$radius
  ), draw_circle
)

# Arrange circles as dense bubble plots ------------------------------------------------

# Get bubble positions with {particles} (an R implementation of the d3-force algorithm)
get_bubble_positions <- function(data, bubble_center_x, bubble_center_y, spacing = 1000) {
  
  # Arrange by radius (otherwise no nice bubble is formed)
  data <- arrange(data, desc(radius)) 
  
  # Build pseudo graph based on data
  graph <- erdos.renyi.game(nrow(data), 0) %>%
    as_tbl_graph() %>% 
    mutate(
      x = data$X_center,
      y = data$Y_center,
      radius = data$radius
    )
  
  # Simulate forces to get new positions
  sim <- graph %>%
    simulate(setup = predefined_genesis(x, y)) %>%
    wield(collision_force, radius = (radius + spacing), n_iter = 5) %>%
    wield(x_force, x = bubble_center_x) %>%
    wield(y_force, y = bubble_center_y) %>%
    evolve()
  
  # Get positions
  positions <- as_tibble(sim$position)
  
  # Draw circles
  a <- pmap_dfr(
    list(data$mun_id, positions$x, positions$y, data$radius),
    draw_circle
  )
  
}

# Yes circles
step3_circles_yes <- step3_circle_data %>%
  left_join(vd %>% select(mun_id, jaStimmenInProzent), by = "mun_id") %>% 
  mutate(yes = ifelse(jaStimmenInProzent >= 50, 1, 0)) %>% 
  filter(yes == 1)

x_center_yes <- mean(grid_X_lim_yes)
y_center_yes <- mean((st_coordinates(gd_nat))[,2])

step4_yes <- get_bubble_positions(step3_circles_yes, x_center_yes, y_center_yes)

# No circles
step3_circles_no <- step3_circle_data %>%
  left_join(vd %>% select(mun_id, jaStimmenInProzent), by = "mun_id") %>% 
  mutate(yes = ifelse(jaStimmenInProzent >= 50, 1, 0)) %>% 
  filter(yes == 0)

x_center_no <- mean(grid_X_lim_no)
y_center_no <- y_center_yes

step4_no <- get_bubble_positions(step3_circles_no, x_center_no, y_center_no)

# Bind yes and no
step4 <- bind_rows(step4_yes, step4_no) 

# Transformation 2: From grided circles to bubble plots --------------------------------

td2 <- tween_sf(td1, step4, ease = "cubic-in-out", nframes = t2, id = mun_id) %>% 
  keep_state(nframes = p3)

# Split circles into yes and no parts and arrange as bubble plots ----------------------

# Yes votes from municipalities with yes majority
yy_radius <- step4_yes %>% 
  left_join(vd %>% select(mun_id, jaStimmenAbsolut), by = "mun_id") %>% 
  mutate(radius = sqrt(conv_fac * jaStimmenAbsolut / pi))

yy_centroid <- as_tibble(st_coordinates(st_centroid(yy_radius)))

step5_yy <- pmap_dfr(list(yy_radius$mun_id, yy_centroid$X, yy_centroid$Y, yy_radius$radius), draw_circle)

# No votes from municipalities with yes majority
yn_radius <- step4_yes %>% 
  left_join(vd %>% select(mun_id, neinStimmenAbsolut), by = "mun_id") %>% 
  mutate(radius = sqrt(conv_fac * neinStimmenAbsolut / pi)) %>% 
  mutate(mun_id = paste0(mun_id, "_cp"))

yn_centroid <- as_tibble(st_coordinates(st_centroid(yn_radius)))

step5_yn <- pmap_dfr(list(yn_radius$mun_id, yn_centroid$X, yn_centroid$Y, yn_radius$radius), draw_circle)

# No votes from municipalities with no majority
nn_radius <- step4_no %>% 
  left_join(vd %>% select(mun_id, neinStimmenAbsolut), by = "mun_id") %>% 
  mutate(radius = sqrt(conv_fac * neinStimmenAbsolut / pi))

nn_centroid <- as_tibble(st_coordinates(st_centroid(nn_radius)))

step5_nn <- pmap_dfr(list(nn_radius$mun_id, nn_centroid$X, nn_centroid$Y, nn_radius$radius), draw_circle)

# Yes votes from municipalities with no majority
ny_radius <- step4_no %>% 
  left_join(vd %>% select(mun_id, jaStimmenAbsolut), by = "mun_id") %>% 
  mutate(radius = sqrt(conv_fac * jaStimmenAbsolut / pi)) %>% 
  mutate(mun_id = paste0(mun_id, "_cp"))

ny_centroid <- as_tibble(st_coordinates(st_centroid(ny_radius)))

step5_ny <- pmap_dfr(list(ny_radius$mun_id, ny_centroid$X, ny_centroid$Y, ny_radius$radius), draw_circle)

# All yes circles
step4_circles_yes <- bind_cols(
  bind_rows(yy_radius, ny_radius) %>%
    st_drop_geometry() %>%
    select(mun_id, radius),
  bind_rows(yy_centroid, ny_centroid)
  ) %>% 
  rename(X_center = X, Y_center = Y)

# Arrange in bubble plot
step5_yes <- get_bubble_positions(step4_circles_yes, x_center_yes, y_center_yes)

# All no circles
step4_circles_no <- bind_cols(
  bind_rows(nn_radius, yn_radius) %>%
    st_drop_geometry() %>%
    select(mun_id, radius),
  bind_rows(nn_centroid, yn_centroid)
  ) %>% 
  rename(X_center = X, Y_center = Y)

# Arrange in bubble plot
step5_no <- get_bubble_positions(step4_circles_no, x_center_no, y_center_no)
  
# Transformation 3: Split circles and rearrange ----------------------------------------

# Function to enter new polygons (workaround since I can't get the enter argument of tween_sf to run)
enter_new_polygons <- function(td, nd) {
  
  td_last <- td[td$.frame == max(td$.frame),]
  entering <- nd %>% filter(!mun_id %in% td_last$mun_id)
  
  if (nrow(entering) > 0) {
    
    ids <- entering$mun_id
    centroids <- suppressWarnings(as_tibble(st_coordinates(st_centroid(entering))))
    radius <- rep(0, length(ids))
    new_rows <- pmap_dfr(list(ids, centroids$X, centroids$Y, radius), draw_circle)
    new_rows$.id <- max(td_last$.id) + c(1:nrow(new_rows))
    new_rows$.phase <- unique(td_last$.phase)
    new_rows$.frame <- unique(td_last$.frame)
    td_new <- bind_rows(td, new_rows)
    return(td_new)
    
  }
  
}

# States of last step
step5_start <- bind_rows(step5_yy, step5_yn, step5_nn, step5_ny)
step5_end <- bind_rows(step5_yes, step5_no)

# Modifiy start state (add counterparts)
td2 <- enter_new_polygons(td2, step5_start)
  
# Transformation data
td3 <- tween_sf(td2, step5_end, ease = "cubic-in-out", nframes = t3, id = mun_id) %>% 
  keep_state(nframes = p4)

# Animation -----------------------------------------------------------------------------

# Add vote data
td_def <- td3 %>% 
  mutate(mun_id2 = str_remove_all(mun_id, "_cp")) %>% 
  left_join(vd %>% select(mun_id, jaStimmenInProzent), by = c("mun_id2" = "mun_id")) %>% 
  select(-mun_id2) %>% 
  mutate(jaStimmenInProzent = ifelse(str_detect(mun_id, "_cp"), 100 - jaStimmenInProzent, jaStimmenInProzent)) %>%
  
  # To equalise colours of all yes/no bubbles -> Visually, I prefer the less accurate bubble chart with gradations. 
  # mutate(jaStimmenInProzent = case_when(
  # .frame <= tot - t3 - p4 ~ jaStimmenInProzent,
  # .frame > tot - t3 - p4 & !str_detect(mun_id, "_cp") & jaStimmenInProzent < 50 ~ 19,
  # .frame > tot - t3 - p4 & !str_detect(mun_id, "_cp") & jaStimmenInProzent >= 50 ~ 80,
  # .frame > tot - t3 - p4 & str_detect(mun_id, "_cp") & jaStimmenInProzent <= 50 ~ 19,
  # .frame > tot - t3 - p4 & str_detect(mun_id, "_cp") & jaStimmenInProzent >= 50 ~ 80
  # )) %>% 
  
  # Prevent blue bubbles in no bubble plot (== 50%) 
  mutate(jaStimmenInProzent = ifelse(
    .frame > tot - t3 - p4 & 
      str_detect(mun_id, "_cp") & 
      jaStimmenInProzent == 50, 49, jaStimmenInProzent
    )) %>% 
  mutate(
  stimmen = factor(case_when(
    jaStimmenInProzent < 20 ~ "",
    jaStimmenInProzent >= 20 & jaStimmenInProzent < 30 ~ "20", 
    jaStimmenInProzent >= 30 & jaStimmenInProzent < 40 ~ "30",
    jaStimmenInProzent >= 40 & jaStimmenInProzent < 50 ~ "40",
    jaStimmenInProzent >= 50 & jaStimmenInProzent < 60 ~ "50",
    jaStimmenInProzent >= 60 & jaStimmenInProzent < 70 ~ "60",
    jaStimmenInProzent >= 70 & jaStimmenInProzent < 80 ~ "70",
    jaStimmenInProzent >= 80 ~ "80"
    ), levels = c("", "20", "30", "40", "50", "60", "70", "80")
    )
  )

# Define plot function
plot_data <- function(data, pos, xlim, ylim) {

  # Title
  subtitle <- case_when(
    pos <= p1 + t1 + p2 + t2/2 ~ "What looks like a clear victory in the choropleth map...",
    pos > p1 + t1 + p2  + t2/2 & pos <= p1 + t1 + p2 + t2 + p3 + t3/2 ~ "...turns out to be a defeat in the end.",
    pos > p1 + t1 + p2 + t2 + p3 + t3/2 ~ "...at least a narrow one."
    )
  
  # Caption
  caption <- case_when(
    pos <= p1 + t1/2 ~ "Voting results at the municipal level.",
    pos > p1 + t1/2 & pos <= p1 + t1 + p2 + t2/2 ~ "Municipalities sorted by voting majority and area size.",
    pos > p1 + t1 + p2 + t2/2 & pos <= p1 + t1 + p2 + t2 + p3 + t3/2  ~ "Municipalities sorted by voting majority and weighted according to number of votes cast.",
    pos > p1 + t1 + p2 + t2 + p3 + t3/2  ~ "Yes and No shares of all 2192 Swiss municipalities."
    )
  
  # Annoation
  if (pos >= tot - p4) {
    
    annotation_yes <- "Yes share: 48.1 %"
    annotation_no <- "No share: 51.9 %"
    
    } else {
    
    annotation_yes <- ""
    annotation_no <- ""
    
  }
  
  # Plot
  p <- ggplot(data$geometry) +
    geom_sf(aes(fill = data$stimmen), color = NA) +
    coord_sf(xlim = xlim, ylim = ylim) +
    annotate("text", x = x_center_yes, y = -10000, label = annotation_yes) +
    annotate("text", x = x_center_no, y = -10000, label = annotation_no) +
    scale_fill_manual(
      values = c(
        "#8d0613", "#c91022", "#f1434a", "#ff9193",
        "#91cdff", "#42a2f1", "#1a7bc5", "#105182"
      ),
      drop = F,
      name = "Percentage of yes votes",
      guide = guide_legend(
        direction = "horizontal",
        keyheight = unit(2, units = "mm"),
        keywidth = unit(c(16, rep(8, 6), 16), units = "mm"),
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = 1,
        nrow = 1,
        byrow = T,
        reverse = T,
        label.position = "bottom"
      )
    ) +
    labs(
      subtitle = subtitle,
      caption = paste0(
        "Proposal: Amendment of the Hunting Act, 2020-09-27 | Data: opendata.swiss\n",
        "What it shows: ", caption)
    ) +
    theme_ipsum_rc() +
    theme(
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text = element_text(color = "white"),
      axis.ticks = element_line(color = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0)
    )
  
  # Updates
  if (!pos %% 10 == 0) cat(".")
  if (pos %% 10 == 0) cat(pos, "frames\n")
  
  # Print
  if (!dir.exists("output")) dir.create("output")
  ggsave(paste0("output/", sprintf("%03d", pos), ".png"), p, width = 11.1/1.5, height = 8.33/1.5)
  
}

# Plots
datalist <- split(td_def, td_def$.frame)
xlim <- c(grid_X_lim_yes[1], grid_X_lim_no[2])
ylim <- c(min(st_coordinates(step3)[,2]), max(st_coordinates(step3)[,2]))
walk2(datalist, 1:length(datalist), plot_data, xlim, ylim)

# Gif (make sure ffmpeg is installed -> https://ffmpeg.org/download.html)
system("ffmpeg -framerate 12 -i output/%03d.png animation.gif")


