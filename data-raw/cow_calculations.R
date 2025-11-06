library(units)
GWP_CH4_CO2_100yr <- 25  # from IPCC AR4
y <- set_units(100, kg / year)  # ch4 emitted by one cow per year
y <- y * GWP_CH4_CO2_100yr  # co2 equivalent emitted by one cow per year

x <- set_units(367, tonnes / ha)  # co2 sequestered by woodland averaged over rotation
x <- x / 44 * 12  # convert from co2 to C
x <- set_units(x, kg / m^2)  # co2 sequestered by woodland averaged over rotation
x <- x / 12 * 44  # convert back from C to co2
x # 36.7 kg CO2/m2

required_affn <- y / x  # m2 of woodland needed to offset one cow's emissions per year

stocking_density <- set_units(0.25, 1 / ha)  # livestock units per hectare
area_per_cow <- set_units(1 / stocking_density, m^2)  # area in m2 per cow
# The required 68 m$^2$/yr of woodland would constitute 0.625%/yr or 1/160th/yr 
# of the land area used for grazing that cow 
required_affn / area_per_cow * 100
# 587 years to afforest the whole area at this rate
area_per_cow / required_affn

# one cow at 0.25 LU/ha for 60 years requires 
time_period <- set_units(60, year)
required_affn * time_period 
required_affn * time_period / area_per_cow * 100

v_years <- set_units(seq(0, 588, by = 1), yr)
afforested_area <- required_affn * v_years
total_area <- area_per_cow
remaining_grassland <- total_area - afforested_area
v_stocking_density <- set_units(1, 1) / (set_units(remaining_grassland, ha))

# when stocking density exceeds 1 LU/ha
findInterval(1, as.numeric(v_stocking_density), checkSorted = FALSE, checkNA = FALSE)  
