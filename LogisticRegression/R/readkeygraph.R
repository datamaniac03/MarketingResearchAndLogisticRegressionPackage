#' Readkey Graph 
#'
#' For graph key press interactivity
#' @param prompt Presstheanykey
#' @keywords presstheanykey
#' @export
#' @examples
#' readkeygraph()

readkeygraph <- function(prompt) {
	getGraphicsEvent(prompt = prompt, 
			   onMouseDown = NULL, 
			   onMouseMove = NULL,
			   onMouseUp = NULL, 
			   onKeybd = onKeybd,
			   consolePrompt = "[click on the graphice device and press any key to advance k]")
 	Sys.sleep(0.01)
	return(keyPressed)
}

