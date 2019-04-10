library(sqldf)

head(iris)
sqldf("select * from iris limit 5")

sqldf("select Species, count(Species) from iris group by Species")

# backticks or double quotes for variable names with dots
sqldf('select Species, count(Species) from iris WHERE "Petal.Length"<4 group by Species')

library(dplyr)
fars <- src_sqlite("U:\\Desktop\\fars2014")
fars

accidents <- tbl(fars, "accidents")


dayweek <- accidents %>% 
  group_by(DAY_WEEK) %>% tally() 
dayweek %>% collect() %>% 
  ggplot(aes(x = DAY_WEEK, weight=n)) + geom_bar() 







hours <- accidents %>% group_by(HOUR) %>% tally() 
hours <- hours %>% collect() 

hours %>% dplyr::filter(HOUR < 25) %>%
  ggplot(aes(x = HOUR, weight=n)) + geom_bar()


drunk <- accidents %>% group_by(DRUNK_DR) %>% tally() 
drunk %>% collect() %>% 
  ggplot(aes(x = DRUNK_DR, weight=n)) + geom_bar()


allinone <- accidents %>% 
  group_by(DAY_WEEK, HOUR, DRUNK_DR>0) %>% tally()
allinone %>% dplyr::filter(HOUR < 25) %>%
  collect() %>%
  ggplot(aes( x= HOUR, y = n, colour=factor(`DRUNK_DR > 0`))) +
  geom_point() +
  facet_wrap(~DAY_WEEK)
