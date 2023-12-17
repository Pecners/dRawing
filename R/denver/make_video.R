library(tidyverse)
library(magick)
library(glue)
library(av)

dir <- "temp/denver"
this_vec <- list.files(dir)
max <- this_vec |> 
  str_remove("\\.png") |> 
  as.numeric() |> 
  max(na.rm = TRUE) |> 
  str_pad(width = 5, side = "left", pad = "0") 

max_f <- glue("{dir}/{max}.png")

# first frame

first <- glue("{dir}/first.png")
img <- image_read(glue("{dir}/0000.png"))
cap <- "dRawing"
img |> 
  image_annotate(text = cap,
                 gravity = "center",
                 location = "+0-500",
                 font = "El Messiri", 
                 boxcolor = alpha("white", .75),
                 weight = 900,
                 size = 100) |> 
  image_write(first)

# second frame
# second <- glue("{dir}/second.png")
# cap <- "Drawing city bounds\npoint by point..."
# img |> 
#   image_annotate(text = cap,
#                  gravity = "center",
#                  location = "+0-500",
#                  font = "El Messiri", 
#                  boxcolor = alpha("white", .75),
#                  weight = 900,
#                  size = 100) |> 
#   image_write(second)

# final frame
final <- glue("{dir}/final.png")
img <- image_read(max_f)
cap <- "Corpus Christi, Texas"
img |> 
  image_annotate(text = cap,
                 gravity = "center",
                 location = "+0-500",
                 font = "El Messiri", 
                 boxcolor = alpha("white", .75),
                 weight = 900,
                 size = 100) |> 
  image_write(final)


# make the video

these <- paste(dir, this_vec, sep = "/")
these <- these[which(!str_detect(these, "first|second|final|0000"))]


av_encode_video(input = rep(first, 200), output = glue("videos/{map}/intro.mp4"), 
                vfilter = "setpts=0.1*PTS")

av_encode_video(input = these, output = glue("videos/{map}/main.mp4"),
                vfilter = "setpts=0.1*PTS")

av_encode_video(input = these[575:length(these)], output = glue("videos/{map}/main1.mp4"),
                vfilter = "setpts=0.1*PTS")


av_encode_video(input = rep(final, 400), output = glue("videos/{map}/final.mp4"),
                vfilter = "setpts=0.1*PTS")

file.create("this_text.txt")

glue("file 'videos/{map}/intro.mp4'\n",
     "file 'videos/{map}/main.mp4'\n",
     # "file 'videos/{map}/main1.mp4'\n",
     "file 'videos/{map}/final.mp4'") |> 
  write_lines(file = "this_text.txt")

system(
  glue("ffmpeg -f concat -safe 0 -i this_text.txt -c copy 'tracked_videos/{map}.mp4'")
)

