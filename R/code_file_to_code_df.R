code_file_to_code_df <- function(filepath = ".Rhistory"){
  
  readLines(filepath) |> 
    paste(collapse = "\n") |> 
    stringr::str_remove("ggram\\(.+\\)|ggram\\(\\)") |>
    styler::style_text() |> 
    as.character() |>
    data.frame(code = _) 
  
}
