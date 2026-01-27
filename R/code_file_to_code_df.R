code_file_to_code_df <- function(code = NULL, filepath = ".Rhistory"){
  
  code_vector <- code |> code_to_vector()
  code_vector <- code_vector %||% clipr::read_clip()
  
  code_vector |> 
    # styler::style_text() |> 
    as.character() |>
    data.frame(code = _) #|>
    # filter(!stringr::str_detect(code, "^ggram.+"))
  
}
