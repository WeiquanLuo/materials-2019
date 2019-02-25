library(tidyverse)
library(gapminder)
head(gapminder)

gapminder %>% filter(country=="Germany") %>%
  ggplot(aes(x = year, y = lifeExp)) +
  geom_point() +
  geom_smooth(method="lm")

lm(data = gapminder %>% filter(country=="Germany"),
   lifeExp~I(year-1950))

gapminderplus <- gapminder %>% 
  group_by(country) %>%
  mutate(year = year - 1950) %>% 
  summarize(
    model = list(lm(lifeExp ~ year))
  )


countryList <- gapminder %>% mutate(year = year-1950) %>% 
  nest(-country, -continent) 

countryList$data[[countryList$country=="Canada"]]

which(countryList$country=="Canada")

# list with one element
countryList$data[countryList$country=="Canada"]

countryList$data[countryList$country=="Canada"][[1]]

###################

?ChickWeight

ChickWeight %>% group_by() %>% nest(-Diet) 

ChickWeight %>% group_by(Diet) %>% nest()

###################

countryList <- gapminder %>% 
  group_by(continent, country) %>% nest()


countryList <- countryList %>% mutate(
  model = data %>% purrr::map(.f = function(d) {
    lm(lifeExp ~ I(year-1950), data = d)
  })
)

?map
map_dbl

