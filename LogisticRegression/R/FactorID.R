#' Detect Factors
#'
#' This function determines which columns in a data frame x are nominal
#' @param x A data frame
#' @keywords factors
#' @export
#' @examples
#' FactorID()

FactorID <-  function(x) { 
	w <- sapply(x, function(x)all(class(x)=="factor"))
	fact_cols <- names(which(w))
	return(fact_cols)
}