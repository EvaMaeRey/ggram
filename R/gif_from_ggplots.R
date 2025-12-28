gif_from_ggplots <- function(plots, gif_output_path = "temp.gif"){
  
  plot_nums <- 1:length(plots) 
  plot_files <- paste("temp", plot_nums |> 
                        stringr::str_pad(pad = "0", width = 2), ".png")
  
  for(i in plot_nums){
    
    ggplot2::ggsave(plot_files[i], plot = plots[[i]])
    
  }
  
  image_list <- lapply(plot_files, magick::image_read)
  frames <- magick::image_join(image_list)
  gif <- magick::image_animate(frames, fps = 2, optimize = TRUE) # 1 frame per second
  magick::image_write(gif, path = gif_output_path)
  cat(paste("GIF saved to:", gif_output_path, "\n"))

}
