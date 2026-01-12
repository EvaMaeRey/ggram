get_code <- function(code = NULL){

  # TODO future feature: allow user to insert custom code

  # 1. get code from history if not provided
  if(is.null(code)){

    # 1.a Get code from history
    temp <- tempfile()
    savehistory(file = temp)
    lines <- readLines(temp)

    # WHY this complexity? to allow dark themes, we want to adjust the text-background
    # which is currently set in code_df_to_code_plot.R
    # as highlight_colors = c(alpha("grey90",.4), alpha("yellow", .4))
    # Previously, modifying this argument within ggram() broke due to parsing error.

    ggram_start <- max(which(stringr::str_detect(lines, "ggram\\(")))

    if(length(ggram_start) > 0 && ggram_start > 0){
      # Remove from ggram line to the end (MUST: ggram is the last call)
      lines <- lines[1:(ggram_start - 1)]
    }

    # Collapse remaining lines
    lines |> paste(collapse = "\n")

  }else{
    code |> paste(collapse = "\n")
  }
}
specify_code_plot <- function(code, style = stamp_notebook(),...){

  code |>
    code_file_to_code_df() |>
    code_df_to_code_plot(style = style,...)

}

patch_code_and_output <- function(code_plot, output_plot, widths, title, ...){

  print(
  patchwork::free(code_plot) + output_plot +
  patchwork::plot_layout(widths = widths) +
  patchwork::plot_annotation(title = title, ...) &
  theme(plot.background = element_rect(colour = "black", linewidth = .05))
  )

}

specify_textoutput_plot <- function(output){

  output |>
    data.frame(code = _) |>
      dplyr::mutate(row_number = dplyr::row_number()) |>
      ggplot() +
      aes(x = 1,
          y = row_number,
          label = code) +
      geom_text(hjust = 0, family = "mono") +
    stamp_notebook() +
    stamp_punched_holes()

}
