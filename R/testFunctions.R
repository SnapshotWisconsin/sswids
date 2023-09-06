#' Bin a vector of dates into occasions of defined length
#'
#' @param x The vector of dates to be binned
#' @param len desired occasion length
#'
#' @return vector of numeric occasion assignments. If total interval is not equally divisible by len,
#' then remainder days are either assigned into the final complete occasion (if remainder/len <0 .5),
#' in which case the final occasion is longer than specified len or else
#' assigned into an additional occasion shorter then len, if remainder/len >= 0.5.
#'
#' @export
#'
#' @examples
#' @examples
#' \dontrun{
#' a <- seq(as.Date("2020-10-01"),as.Date("2020-12-31"),1)
#' tile(a,7)
#' }

tile <- function(x, len){
  mod <- as.integer(max(x) - min(x) +1) %% len
  cutpoints <- seq(min(x), max(x)+1, len)
  labs <- 1:(length(cutpoints)-1)
  CUT <- cut(x, breaks=cutpoints, labels=labs, right=FALSE)
  NAs <- which(is.na(CUT))
  CUT <- as.integer(CUT)
  CUT[NAs] <- ifelse(mod/len < 0.5, max(labs), max(labs)+1)
  return(CUT)
}
