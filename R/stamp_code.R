stamp_code <- function(title = "code style",
                             theme = "light",
                             width = 35,
                             height = 20) {

  # 0. Error handling // available themes

  supported_themes <- c("dark", "light", "warm")

  if (!theme %in% supported_themes) {
    cli::cli_abort(
      c(
        "x" = "{.val {theme}} is not a supported theme.",
        "i" = "Please select one of: {.or {.val {supported_themes}}}."
      )
    )
  }

  # 1. :root CSS-Style theme definitions

  palette <- switch(theme,
                    "dark" = list(
                      body_color   = "#1e1e1e",
                      header_stop  = "#2d2d30",
                      header_start = "#454955",
                      title_color   = "white",
                      line_color   = alpha("white", 0.1)
                    ),
                    "light" = list(
                      body_color   = "#FFFFFF",
                      header_stop  = "#E1E4E8",
                      header_start = "#FFFFFF",
                      title_color   = "#24292F",
                      line_color   = alpha("black", 0.1)
                    ),
                    "warm" = list(
                      body_color   = "#FAF5E4",
                      header_stop  = "#E6DCCA",
                      header_start = "#FFF8DC",
                      title_color   = "#4A403A",
                      line_color   = alpha("#8C7B75", 0.15)
                      )
                    )

  dot_colors <- c("#ff5f56", "#ffbd2e", "#27c93f")
  dot_positions <- seq(-2.2,-0.2,length = 3)

  # 2. Gradients

  # some funky grid:: stuff here to create a gradient on the header
  gradient_fill <- grid::linearGradient(
    colours = c(palette$header_start, palette$header_stop),
    x1 = 0.5, y1 = 1, x2 = 0.5, y2 = 0
  )
  # 3. Lets assemble this together

  list(
    theme_void(),
    theme(
      plot.background  = element_rect(fill = palette$body_color, color = NA),
      panel.background = element_rect(fill = palette$body_color, color = NA),
      plot.margin      = margin(0, 0, 0, 0)
    ),

    scale_y_reverse(limits = c(-1, height), expand = c(0,0)),
    scale_x_continuous(limits = c(-3, width), expand = c(0,0)),

    # for some reason the header is not position:sticky so this is a small hack
    annotation_custom(
      grob = grid::rectGrob(
        gp = grid::gpar(fill = palette$header_start, col = NA)
      ),
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = -0.9
    ),

    # this is the actual gradient
    annotation_custom(
      grob = grid::rectGrob(
        gp = grid::gpar(fill = gradient_fill, col = NA)
      ),
      xmin = -Inf, xmax = Inf,
      ymin = -1, ymax = 0
    ),


    annotate("point", x = dot_positions[1], y = -0.5, color = dot_colors[1], size = 3),
    annotate("point", x = dot_positions[2], y = -0.5, color = dot_colors[2], size = 3),
    annotate("point", x = dot_positions[3], y = -0.5, color = dot_colors[3], size = 3),

    annotate("text",
             x = 0.7, y = -0.5,
             label = title,
             color = palette$title_color,
             size = 4,
             hjust = 0,
             family = "mono"),

    # small line that makes a big difference
    annotate("segment",
             x = -Inf, xend = Inf,
             y = 0.05, yend = 0.05,
             color = palette$line_color,
             linewidth = 0.6)
  )
}
