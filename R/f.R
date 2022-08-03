#' Helper function
#'
#' @param a numeric
#' @param s numeric
#' @param e numeric thr
#'
#' @export
fv <- function(a, s = 2, e = 1e-4) {

  vec <- vector(mode = "numeric", len = length(a))

  for (i in seq_along(a)) {
    p <- .Machine$integer.max
    x <- a[i]
    while (p >= e) {
      xi <- x * (s - 1) / s + a[i] / (s * x^(s - 1))
      p <- abs(xi - x)
      x <- xi
    }
    vec[i] <- x
  }
  return(vec)
}









#' Helper function with parameter check
#'
#' @param a numeric
#' @param s numeric
#' @param e numeric thr
#'
#' @export
fv_strict <- function(a, s = 2, e = 1e-4) {

  if (!is.numeric(a)) stop("parameter a should be numeric vector")
  res <- fv(a = a, s = s, e = e)
  return(res)
}
