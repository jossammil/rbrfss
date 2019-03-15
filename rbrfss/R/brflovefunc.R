#' A brfss Function
#'
#' This function allows you to express your love of brfss
#' @param love Do you love rbrfss? Defaults to TRUE.
#' @keywords rbrffs
#' @export
#' @examples
#' rbrfss()

rbrffs <- function(love = TRUE) {
  if (love == TRUE) {
    print("I love brfss")
  } else {
    print("I don't like brfss")
  }
}
