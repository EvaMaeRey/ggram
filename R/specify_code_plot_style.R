specify_code_plot_style <- function(
                            highlight_colors = c(ggplot2::alpha("grey90",.4), alpha("yellow", .4)),
                                 family = "mono",
                                 size = 1,
                                 vline_color = "darkred", 
                                 hline_color = "blue", 
                                 paper_color = ggplot2::alpha("whitesmoke", .1),
                                 width = 35,
                                 height = 20, 
                                 accent = stamp_punched_holes()){
  
  list(highlight_colors = highlight_colors, 
       family = family,size = size, 
       vline_color = vline_color,      
       hline_color = hline_color, 
       paper_color = paper_color, 
       width = width, 
       height = height, 
       accent = accent)

}


