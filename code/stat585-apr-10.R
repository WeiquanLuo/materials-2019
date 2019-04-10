library(tidyverse)
accident <- read_csv("https://stat585-at-isu.github.io/materials-2019/data/fars2017/accident.csv")
person <- read_csv("https://stat585-at-isu.github.io/materials-2019/data/fars2017/person.csv")
vehicle <- read_csv("https://stat585-at-isu.github.io/materials-2019/data/fars2017/vehicle.csv")

library(DBI)
mydb <- dbConnect(RSQLite::SQLite(), "my-db.sqlite")
dbDisconnect(mydb)
unlink("my-db.sqlite")

mydb <- dbConnect(RSQLite::SQLite(), "fars2017.sqlite")

dbWriteTable(mydb, "accident", accident)
dbWriteTable(mydb, "person", person)
dbWriteTable(mydb, "vehicle", vehicle)
dbDisconnect(mydb)

fars2017 <- dplyr::src_sqlite("fars2017.sqlite")



# ---- Regular Expressions -----

library(stringr)
passwords <- readLines("http://bit.ly/585-passwords")

# How many of the passwords have at least one space?
sum(str_count(passwords, " ") > 0)
str_detect(passwords, " ") %>% sum

# What is the most common character in a password?
str_split(passwords, '') %>% 
  unlist() %>% 
  table() %>% 
  sort(decreasing = T)

# What proportion of the passwords have ., ?, 
# and ! characters?
(str_detect(passwords, '\\.') | 
  str_detect(passwords, '\\?') |
  str_detect(passwords, '!')) %>% mean()

test_str <- "Hello World!"
str_extract_all(test_str, "[A-z]{1,}")
str_extract_all(test_str, "[^A-z]{1,}")
str_extract_all(test_str, ".")

# How many of the passwords have at least one 
# space, -, or _?

str_detect(passwords, "[ -_]") %>% sum() # This is wrong
str_detect(passwords, "[ _-]") %>% sum()

# What proportion of the passwords have 
# ., ?, and ! characters?

str_detect(passwords, "[\\.\\?!]") %>% mean()

# What proportion of the passwords have only 
# lowercase letters?

1 - (str_detect(passwords, "[^a-z]") %>% mean())
1 - (str_detect(passwords, "[^0-9]") %>% mean())
