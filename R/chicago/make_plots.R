library(tidyverse)
library(tigris)
library(sf)
library(glue)
library(foreach)
library(doParallel)
library(MetBrewer)
library(colorspace)

map <- "chicago"
this_dir <- "temp/chicago"

if (!dir.exists(this_dir)) {
  dir.create(this_dir)
}

# download data

state_places <- places("IL")

this_place <- state_places |> 
  filter(NAME == "Chicago")

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

c_pal <- met.brewer("Hiroshige", n = 14)
cp <- c_pal[7:1]
swatchplot(c_pal)
swatchplot(cp)

# first frame

first_p <- this_place |> 
  st_cast("MULTILINESTRING") |> 
  ggplot() +
  # base mke map
  geom_sf(fill = NA, color = NA) +
  theme_void()

ggsave(filename = glue("temp/{map}/0000.png"), 
       plot = first_p, bg = c_pal[12], 
       w = 1650, h = 2000, units = "px")



do_it <- function(p) {
  ind <- str_pad(as.character(p), width = 5, side = "left", pad = "0")
  tf <- glue("temp/{map}/{ind}.png") 
  
  if (!file.exists(tf)) {
    these_pts <- this_point[c(p:(p+1)),]
    
    cum_pts <- this_point[c(1:(p+1)),]
    
    t <- this_point[p,]
    
    
    
    this_line <- these_pts |> 
      summarise() |> 
      st_cast("LINESTRING")
    
    if (!(t$ind == 1 & t$n == 12)) {
      cum_line <- cum_pts |> 
        group_by(ind) |> 
        summarise(do_union = FALSE) |> 
        st_cast("LINESTRING")
      
      this_p <- this_line |> 
        ggplot() +
        # base mke map
        geom_sf(data = this_poly, fill = NA, color = NA) +
        geom_sf(data = cum_line, color = c_pal[6]) +
        # segment of border
        geom_sf(color = c_pal[1], linewidth = 1.5) +
        geom_sf(data = these_pts[1,], fill = c_pal[2],
                color = c_pal[14],
                size = 1, shape = 21) +
        theme_void()
      this_p
      
      
      ggsave(filename = tf, plot = this_p, bg = c_pal[12], 
             w = 1650, h = 2000, units = "px")
    } 
  }
  
}
# do_it(6446)

# Set up cores for parallel rendering of plots
registerDoParallel(10)

foreach(i = c(1:(nrow(this_point) - 1))) %dopar% do_it(p = i)




