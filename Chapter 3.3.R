library(raster)

# create 6 by 6 raster, coords -1.5 to 1.5
elev = raster(nrow = 6, ncol = 6, res = 0.5, xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5, vals = 1:36)

grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
# rasters can't contain text so factr used to created ordered factor
grain_fact = factor(grain_char, levels = grain_order)
grain = raster(nrow = 6, ncol = 6, res = 0.5, xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5, vals = grain_fact)

# ratify function can be used to retrieve raster attribute table (look up)
ratify(grain)

# rasters store categorical variables as numbers, so use 'levels' to retrieve and add extra values
levels(grain)[[1]] = cbind(levels(grain)[[1]], wetness = c("wet", "moist", "dry"))
levels(grain)

# look up attribute values
factorValues(grain, grain[c(1, 12, 36)])
factorValues(grain, grain[c(1:36)])

# find top left pixel value in 2 ways
elev[1,1] # row 1, column 1
elev[1] # cell ID 1

# stack mutliple rasters
stack(elev, grain)[1]

# extract all values in a raster
elev[]
values(elev)

# print descriptive stats for raster
summary(elev)
cellStats(elev, median) # specify a specific stat e.g. sd, var, mean, median
summary(brick(elev, grain)) # summarize 2 rasters simultaneously

# various visualisations
boxplot(elev)
density(elev)
hist(elev)
pairs()
