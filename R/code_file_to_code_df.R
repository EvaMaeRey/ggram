code_file_to_code_df <- function(filepath = ".Rhistory"){
  
  readLines(filepath) |> 
    paste(collapse = "\n") |> 
    styler::style_text() |> 
    as.character() |>
    data.frame(code = _) |>
    filter(!stringr::str_detect(code, "^ggram.+"))
  
}
