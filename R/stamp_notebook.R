
stamp_notebook <- function(vline_color = "darkred", hline_color = "blue", paper_color = alpha("whitesmoke", .1),
                           width = 35,
                           height = 20){
  
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
    NULL
  )
  
}


stamp_notebook_college_rule <- function(){
  
stamp_notebook(width = 40,
               height = 25)
  
}


stamp_typed_page <- function(){
  
stamp_notebook(vline_color = "darkolivegreen", 
               hline_color = alpha("lightgrey", .1), 
               paper_color = alpha("whitesmoke", .1),
               width = 50,
               height = 30)
  
}


stamp_legal_pad <- function(){
  
  list(
stamp_notebook(vline_color = "darkred", 
               hline_color = "blue", 
               paper_color = alpha("yellow", .2),
               width = 40,
               height = 25
               ),
 geom_vline(xintercept = -.2, color = "darkred"))
  
  
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
    
    theme_classic(),
    theme(plot.background = element_rect(fill = alpha("whitesmoke", .1))),
    annotate(geom = "segment", x = I(-3:23/20), xend = I(-3:23/20), y = I(-.2), yend = I(1.1), color = "blue", alpha = .15, linewidth = .2) ,
    annotate(geom = "segment", y = I(-3:23/20), yend = I(-3:23/20), x = I(-.2), xend = I(1.1),  color = "blue", alpha = .15,  linewidth = .2),
    NULL,  
    coord_cartesian(clip = "off")
  )
  
}




