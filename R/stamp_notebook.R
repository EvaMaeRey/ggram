stamp_punched_holes <- function(){
  
   list(
    annotate("point", x = -1.5, y = I(c(1,9,17)/20) , color = "white", size = 5) ,
    annotate("point", x = -1.5, y = I(c(1,9,17)/20), shape = 21, 
             alpha = .3, size = 5, fill = "grey92")
   )
  
}


stamp_notebook <- function(vline_color = "darkred", hline_color = "blue", paper_color = alpha("whitesmoke", .1),
                           width = 35,
                           height = 20, punch_holes = T){
  
  punch_base <- annotate("point", x = -1.5, y = I(c(1,9,17)/20) , color = "white", size = 5)
  punch_edge <- annotate("point", x = -1.5, y = I(c(1,9,17)/20), shape = 21, 
             alpha = .3, size = 5, fill = "grey92")
  
  list(
    
    theme_void(),
    theme(plot.background = element_rect(fill = paper_color)),
    scale_y_reverse(limits = c(-1, height)),
    # coord_equal(),
    scale_x_continuous(limits = c(-3, width)),
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, 
             fill = alpha("grey90", .1)),
    geom_vline(xintercept = 0, color = vline_color) ,
    geom_hline(yintercept = 1:29 + .5, color = hline_color, linewidth = .2, alpha = .5),
    if(punch_holes){punch_base}else{NULL},
    if(punch_holes){punch_edge}else{NULL}
    
  )
  
}
