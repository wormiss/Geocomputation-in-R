library(sf)
library(tidyverse)
library(raster)
library(spData)

# Spatial subsetting

# create Africa dataset
africa_wgs = world[world$continent == "Africa",]
africa = st_transform(africa_wgs, crs = 32630)

# create circle centred on point where GMT crosses equator
center = st_sf(geometry = st_sfc(st_point(c(0, 0)), crs = 4326))
buff = st_buffer(x = center, dist = 20)
buff= st_transform(buff, 32630)

# intersect query
africa_buff = africa[buff, ]
plot(africa_buff["pop"])
plot(buff, add = TRUE)

# Another method of intersecting
sel_buff = st_intersects(x = africa, y = buff, sparse = FALSE)
africa_buff2 = africa[sel_buff,]
plot(africa_buff2["lifeExp"])
plot(buff, add = TRUE)

# a third way of intersecting
africa_buff3 = filter(africa, sel_buff)
plot(africa_buff3["area_km2"])
plot(buff, add = TRUE)

# test whether 3 results are identical
identical(x = africa_buff, y = africa_buff2)
identical(x = africa_buff, y = africa_buff3) # FALSE because dplyr changes row names

# create some topological data for understanding theire relationships
a1 = st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
a = st_sfc(a1)
l1 = st_linestring(x = matrix(c(-1, -1, -0.5, 1), , 2))
l = st_sfc(l1)
p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
p_multi = st_multipoint(x = p_matrix)
p = st_sf(st_cast(st_sfc(p_multi), "POINT"))

# draw shapes
plot(a)
plot(l, add = TRUE)
plot(p, add = TRUE)

# find which objects intersect: 1 means yes, 0 means no
st_intersects(p, a)
st_intersects(p, a, sparse = FALSE) # results in a logical vector that is more useful
p[st_intersects(p, a, sparse = FALSE),]

# other topological operations
st_within(p, a, sparse = FALSE)
st_touches(p, a, sparse = FALSE) # only returns objects touching border
st_is_within_distance(p,a, dist = 0.9) # can only return sparse matrix

# spatial joining and aggregation
urb = urban_agglomerations %>%
  filter(year == 2020) %>% # 2020 population
  top_n(n = 3, wt = population_millions) # top 3
asia = world %>%
  filter(continent == "Asia")

joined = st_join(x = asia, y = urb) # default left join and intersect
joined[!is.na(joined$population_millions),]
plot(joined["population_millions"])

joined = st_join(x = asia, y = urb, left = FALSE) # inner join
joined[!is.na(joined$population_millions),]
plot(joined["population_millions"])

# Aggregating/dissolving

regions = aggregate(x = us_states[, "total_pop_15"], by = list(us_states$REGION), FUN = sum, na.rm = TRUE)
par(mfrow = c(1, 2))
plot(us_states[, "total_pop_15"], main = "US states") # original data
plot(regions[, "total_pop_15"], main = "US regions") # aggregated data

# Tidyverse version
group_by(us_states, REGION) %>%
  summarize(sum(pop = total_pop_15, na.rrm = TRUE))

# aggregate buffer using Africa data. (many issues, better to do proportion sum)
buff_agg = aggregate(x = africa[, "pop"], by = buff, FUN = sum)
plot(buff_agg)
plot(africa["pop"], add = TRUE)

# Area weighted interpolation i.e. proportion sum
buff_agg_aw = st_interpolate_aw(x = africa["pop"], to = buff, extensive = TRUE)

b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
l = c("x", "y")
plot(b)
text(x = c(-0.5, 1.5), y = 1, labels = l) # add text

# select various areas of overlap
x = b[1]
y = b[2]
x_and_y = st_intersection(x, y)
x_and_y = st_difference(y, x) # y that doesn't overlap x
x_and_y = st_difference(x, y) # x that doesn't overlap y
x_and_y = st_union(x, y) # all of x and y i.e union
x_and_y = st_sym_difference(x, y) # everything but overlap
x_and_y = st_intersection(x, y)
plot(b)
plot(x_and_y, col = "lightgrey", add = TRUE) # colour intersecting area

bb = st_bbox(st_union(x, y))
pmat = matrix(c(bb[c(1, 2, 3, 2, 3, 4, 1, 4, 1, 2)]), ncol = 2, byrow = TRUE)
box = st_polygon(list(pmat))
set.seed(2017)
p = st_sample(x = box, size = 10)
plot(box)
plot(x, add = TRUE)
plot(y, add = TRUE)
plot(p, add = TRUE)
text(x = c(-0.5, 1.5), y = 1, labels = l)
