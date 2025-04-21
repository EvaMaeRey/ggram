
stamp_notebook <- function(){
  
  list(
    
    theme_void(),
    theme(plot.background = element_rect(fill = alpha("whitesmoke", .1))),
    scale_y_reverse(limits = c(-1, 20)),
    # coord_equal(),
    scale_x_continuous(limits = c(-3, 35)),
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, 
             fill = alpha("grey90", .1)),
    geom_vline(xintercept = 0, color = "darkred") ,
    geom_hline(yintercept = 1:29 + .5, color = "blue", linewidth = .2, alpha = .5),
    NULL
  )
  
}

stamp_punched_holes <- function(){
  
   list(
    annotate("point", x = -1.5, y = c(1,9,17) + .25, color = "white", size = 5) ,
    annotate("point", x = -1.5, y = c(1,9,17) + .25, shape = 21, 
             alpha = .3, size = 5, fill = "grey92")
   )
  
}


stamp_graph_paper <- function(){
  
  list(
    
    theme_void(),
    theme(plot.background = element_rect(fill = alpha("whitesmoke", .1))),
    scale_y_reverse(limits = c(-1, 20)),
    # coord_equal(),
    scale_x_continuous(limits = c(-3, 35)),
    # annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, 
             # fill = alpha("grey90", .1)),
    geom_vline(xintercept = -3:35, color = "blue", alpha = .5, linewidth = .2) ,
    geom_hline(yintercept = -1:29 + .5, color = "blue", linewidth = .2, alpha = .5),
    NULL
  )
  
}




