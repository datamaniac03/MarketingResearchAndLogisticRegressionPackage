#' Clear Console
#'
#' Clears console for 100 lines to make output easier to read
#' @param x Clear automatically
#' @keywords clear console
#' @export
#' @examples
#' cls()

cls <- function() {
	cat(rep("\n",100))
}

