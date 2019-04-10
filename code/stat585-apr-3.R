
library(nycflights13)
delays <- flights %>% group_by(flight) %>% 
  summarize(
    mean_delay = mean(arr_delay, na.rm=FALSE)
  )

library(randomForest)
library(future)

ptm0 <- proc.time()
x %<-% {
  color_models <- diamonds %>%
    group_by(clarity) %>%
    do(
      mymodel = randomForest(color ~ carat+cut+depth+table+price+x+y+z, 
                             data = ., ntree = 1000)
    )
}

ptm1 <- proc.time()


parallel::detectCores()
plan(multiprocess)

ptm2 <- proc.time()
x %<-% {
  color_models <- diamonds %>%
    group_by(clarity) %>%
    do(
      mymodel = randomForest(color ~ carat+cut+depth+table+price+x+y+z, 
                             data = ., ntree = 1000)
    )
}

ptm3 <- proc.time()


ptm0_purrr <- proc.time()
color_purrr <- 
  diamonds %>% nest(-clarity) %>% 
  mutate (
    mymodel = data %>% purrr::map(.f = function(d) 
      randomForest(color ~ carat+cut+depth+table+price+x+y+z, 
                   data = d, ntree = 1000)
    )
  )
ptm1_purrr <- proc.time()

ptm1_purrr- ptm0_purrr

library(furrr)
library(randomForest)
library(tidyverse)
plan(multiprocess)
ptm2_purrr <- proc.time()
color_purrr <- 
  diamonds %>% nest(-clarity) %>% 
  mutate (
    mymodel = data %>% furrr::future_map(.f = function(d) 
      randomForest(color ~ carat+cut+depth+table+price+x+y+z, 
                   data = d, ntree = 1000)
    )
  )
ptm3_purrr <- proc.time()
ptm3_purrr-ptm2_purrr


