f <- function(x) { 
  w(x)
  g(h(x)) 
  w(x)
} 
g <- function(x) {
  a <- 10
  x
} 
h <- function(x) {
  w(x) 
  w(x) 
}
w <- function(x) { 
  if (sample(10, 1) == 1) stop("This is an error!")
}
############################

larger <- function(x, y) {
  if (length(y) < length(x)) y <- rep(y, length=length(x))
  
  y.is.bigger <- y > x 
  x[y.is.bigger] <- y[y.is.bigger] 
  x
} 

larger(c(1, 5, 10), c(2, 4, 11))

larger(c(1, 5, 10), 6)
larger(c(1, 5, 10), c(15,1))
debug(larger)

col_means <- function(df) {
  numeric <- sapply(df, is.numeric)
  numeric_cols <- df[, numeric]
  data.frame(lapply(numeric_cols, mean))
}


col_means(mtcars)
col_means(mtcars[, 0])
col_means(mtcars[0, ])
col_means(mtcars[, "mpg", drop = F])
col_means(1:10)
col_means(as.matrix(mtcars))
col_means(as.list(mtcars))
