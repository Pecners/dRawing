library(tidyverse)
library(tigris)
library(sf)
library(glue)
library(foreach)
library(doParallel)
library(MetBrewer)
library(colorspace)
library(rnaturalearth)

map <- "denver"
this_dir <- "temp/denver"

tx_places <- tigris::places(state = "co")
this_place <- tx_places |> 
  filter(NAME == "Denver")

this_place |> 
  ggplot() +
  geom_sf()

this_poly <- this_place |> 
  st_cast(to = "POLYGON") |> 
  st_cast(to = "LINESTRING") |> 
  mutate(ind = row_number()) |> 
  select(ind, geometry)

this_point <- this_poly |> 
  st_cast(to = "POINT") |> 
  group_by(ind) |> 
  mutate(n = row_number())

# Set up color palette

c_pal <- met.brewer("Demuth", n = 12)
swatchplot(c_pal)
bg <- c_pal[9]
line <- c_pal[4]
p_stroke <- c_pal[1]
p_fill <- c_pal[6]


# first frame

first_p <- this_place |> 
  st_cast("MULTILINESTRING") |> 
  ggplot() +
  # base mke map
  geom_sf(fill = NA, color = NA) +
  theme_void()

ggsave(filename = glue("temp/{map}/0000.png"), 
       plot = first_p,  bg = "#092E59", 
       w = 1650, h = 2000, units = "px")



do_it <- function(p) {
  ind <- str_pad(as.character(p), width = 5, side = "left", pad = "0")
  tf <- glue("temp/{map}/{ind}.png") 
  
  if (!file.exists(tf)) {
    
    cum_pts <- this_point[c(1:(p+1)),]
    
    t <- this_point[p,]
    
    tail <- 250
    if (p > tail) {
      l <- tail - 1
    } else {
      l <- p - 1
    }
    these_pts <- this_point[c((p-l):(p+1)),]
    
    this_line <- these_pts |> 
      group_by(ind) |> 
      summarise(do_union = FALSE) |> 
      st_cast("LINESTRING")
    
    if (!(t$ind == 1 & t$n == 12)) {
      cum_line <- cum_pts |> 
        group_by(ind) |> 
        summarise(do_union = FALSE) |> 
        st_cast("LINESTRING")
      
      big <- st_buffer(these_pts[1,], 10000)
      
      this_p <- this_line |> 
        ggplot() +
        # base mke map
        geom_sf(data = this_poly, fill = NA, color = NA) +
        # cumulative line
        geom_sf(data = cum_line, color = "#E4C601",
                linewidth = .25) +
        # point shadow
        geom_sf(data = these_pts[nrow(these_pts),], fill = "#E4C601",
                shape = 21, color = "#E4C601",
                alpha = .25, size = 4) +
    
        # segment of line
        geom_sf(color = "white",
                linewidth = .25) +
        geom_sf(color = "#E4C601", alpha = .25,
                linewidth = 1) +
        geom_sf(data = these_pts[nrow(these_pts),], fill = "white",
                color = "white",
                size = 1, shape = 21) +
        theme_void()
      
      # this_p
      
      
      ggsave(filename = tf, plot = this_p, bg = "#092E59", 
             w = 1650, h = 2000, units = "px")
    } 
  }
  
}

# Set up cores for parallel rendering of plots
registerDoParallel(10)

foreach(i = c(1:(nrow(this_point) - 1))) %dopar% do_it(p = i)

p <- nrow(this_point)
ind <- str_pad(as.character(p), width = 5, side = "left", pad = "0")
tf <- glue("temp/{map}/{ind}.png") 

this_p <- this_place |> 
  ggplot() +
  geom_sf(fill = alpha("white", .5), 
          color = "#E4C601") +
  theme_void()
# this_p


ggsave(filename = tf, plot = this_p, bg = "#092E59", 
       w = 1650, h = 2000, units = "px")



