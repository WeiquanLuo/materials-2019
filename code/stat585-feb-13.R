rep(16+9*0:11, each=4) + c(1,7,8,9)
rep(16+9*0:11, each=4) + c(6,7,8,9)

paste0(rep(c("Value","DMflag", "QCflag", "DSflag"), 12),
       rep(1:12, each=4))

x <- c(1, 3, 2, 3, 0, 1, 0, 1, 3, 3)
mean(x)

loglikpois <- function(lambda, x) {
  if(lambda == 0) # if we are smarter about this if condition, we can make the function work on vectors
    stop("lambda must be strictly positive")
  n <- length(x)
  
  L = -n*lambda + sum(x)*log(lambda)
  L
}

loglikpois(lambda = 1.6, x)
loglikpois(lambda = 1.7, x)
loglikpois(lambda = 0, x)

lambdas <- seq(from=0.0, to=10, by=0.1)
lambdas[1] <- 0.01

dframe <- data.frame(lambda = lambdas,
    L = sapply(lambdas, FUN = loglikpois, x = x))

head(dframe)

library(tidyverse)
dframe %>% ggplot(aes(x = lambda, y = L)) + geom_point()
dframe %>% ggplot(aes(x = lambda, y = L)) + geom_line() +
  geom_vline(xintercept=mean(x), color="red")


loglikpois(c(1.2,1.7, 1.9), x = x)
dframe %>% filter(lambda == 1.20)


somefunction2 <- function (...) 
{
  browser()
  k <- length(ll <- list(...))
  if (k == 0L) 
    return(invisible())
  mc <- match.call()
  for (i in 1L:k) if (!(is.logical(r <- ll[[i]]) && !any(is.na(r)) && 
                        all(r))) {
    ch <- deparse(mc[[i + 1]], width.cutoff = 60L)
    if (length(ch) > 1L) 
      ch <- paste(ch[1L], "....")
    stop(paste(ch, " is not ", if (length(r) > 1L) 
      "all ", "TRUE", sep = ""), call. = FALSE)
  }
  invisible()
}

x <- 1
somefunction2(x==x, 1+1==2, c(7+5==11.99999, 1+1==2))
