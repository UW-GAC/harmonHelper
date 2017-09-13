.zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  all(abs(max(x) - min(x)) < tol)
}
