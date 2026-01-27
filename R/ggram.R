#' @export
ggram <- function(title = NULL, widths = c(1,1), code = NULL, 
                  code_style_args = specify_code_plot_style(),
                   output_plot = NULL, ...){
  
  code_plot <- specify_code_plot(code, code_style_args = code_style_args)

  code_vector <- code |> code_to_vector()
  code_vector <- code %||% clipr::read_clip()  # clip not clear history, get_code(code = code) #
  output <- eval(parse(text = code_vector))
  
  output_plot <- output
  # # 
  patch_code_and_output(code_plot, output_plot, widths, title, ...)

}



