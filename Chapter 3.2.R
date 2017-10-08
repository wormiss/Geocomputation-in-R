# This tutorial requires the packages sf, raster and tidyverse
library(sf)
library(raster)
library(tidyverse)

# It also relies on spData, which loads the datasets world, worldbank_df and us_states
library(spData)

methods(class = "sf") # methods for sf objects

dim(world) # it is a 2 dimensional object, with rows and columns
nrow(world) # how many rows?
ncol(world) # how many columns?

world_df = st_set_geometry(world, NULL) # extract the attribute data from 'world' and create new object 'world_df'
class(world_df)

world[1:6, ] # subset rows by position
world[ ,1:3] # subset columns by position
world[, c("name_long", "lifeExp")] # subset columns by name

sel_area = world$area_km2 < 10000 # find out how many countries have an area less than 10000km2
summary(sel_area) # summarise number FALSE or TRUE
small_countries = world[sel_area, ] # create new dataset based on selection

small_countries = world[world$area_km2 < 10000,] # a more concise method of doing same thing
small_countries = subset(world, area_km2 < 10000) # this base R function achieves same result

world$name_long # select a specific variable, as a vector

world1 = dplyr::select(world, name_long, pop) # prefix with dplyr:: because select() is in more than 1 package
names(world1) # list field names

# all columns between name_long and pop (inclusive)
world2 = dplyr::select(world, name_long:pop)

# all columns except subregion and area_km2 (inclusive)
world3 = dplyr::select(world, -subregion, -area_km2)

# subset and rename at same time
world4 = dplyr::select(world, name_long, population = pop)
names(world4)

# the base R function equivalent of the above is...
world5 = world[, c("name_long", "pop")] # subset columns by name
names(world5)[2] = "population" # rename column manually

# slice is the row equivalent of select
slice(world, 3:5) # select rows 3 to 5

# filter() is dplyr's equivalent of base R's subset()
# countries with a life expectancy more than 82 years
world6 = filter(world, lifeExp > 82)

# the %>% pipe operator is used to chain commands
world7 = world %>%
  dplyr::select(name_long, continent) %>% # take 2 columns
  slice(1:5) # and first 5 rows

# the same as above but nested function calls i.e. more confusing
world8 = dplyr::select(slice(world, 1:5), name_long, continent)

# get R to check whether both are identical
identical(world7, world8)

# base R function 'aggregate' used to aggregate pop by continent (no geometry)
aggregate(pop ~ continent, FUN = sum, data = world, na.rm = TRUE)

# summarize() is the dplyr equivalent or aggregate(), and keep the geometry
group_by(world, continent) %>%
  summarize(pop = sum(pop, na.rm = TRUE))

# this method is more flexible allowing all things to be aggregated and rename columns
world %>%
  summarize(pop = sum(pop, na.rm = TRUE), n_countries = n())

# chain dplyr command to find world's 3 most populous continents
world %>%
  dplyr::select(pop, continent) %>% # select only pop and continent columns
  group_by(continent) %>% # group by continent
  # aggregate pop, remove missing values, & add country count column
  summarize(pop = sum(pop, na.rm = TRUE), n_countries = n()) %>%
  top_n(n = 3, wt = pop) %>% # pick top 3 by pop column
  st_set_geometry(value = NULL) # remove geometry

# create north_america dataset from 'world' file
north_america = world %>%
  filter(subregion == "Northern America") %>%
  dplyr::select(iso_a2, name_long)
north_america$name_long

# create wb_north_america dataset from world bank dataset
wb_north_america = worldbank_df %>%
  filter(name %in% c("Canada", "Mexico", "United States")) %>%
  dplyr::select(name, iso_a2, urban_pop, unemploy = "unemployment")

# join using left_join() function and iso_a2 column
left_join1 = north_america %>%
  left_join(wb_north_america, by = "iso_a2")

# left_join using field names that are different
left_join2 = north_america %>%
  left_join(wb_north_america, by = c("name_long" = "name"))
names(left_join2)

# this removes the duplicate iso_a2 field from left_join2
left_join3 = north_america %>%
  left_join(wb_north_america, by = c("iso_a2", "name_long" = "name"))

# data frame first to drop sf class (keep geom column)
left_join4 = wb_north_america %>%
  left_join(north_america, by = c("iso_a2"))
class(left_join4)

# convert data frame with a geometry column to sf object
st_as_sf(left_join4)
class(left_join4)

# inner_joins only keep observations from left object if matching object in right object
inner_join1 = north_america %>%
  inner_join(wb_north_america, by = c("iso_a2", "name_long" = "name"))
inner_join1$name_long # only US and Canada
