#' @export
clearhistory <- function() {
  
  temp <- tempfile()
  write("", file=temp)
  loadhistory(temp)
  unlink(".blank")
}
