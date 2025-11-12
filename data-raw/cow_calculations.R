library(units)
GWP_CH4_CO2_100yr <- 25  # from IPCC AR4
e_cow <- set_units(100, kg / year)  # ch4 emitted by one cow per year
e_cow <- e_cow * GWP_CH4_CO2_100yr  # co2 equivalent emitted by one cow per year

S_woods <- set_units(367, tonnes / ha)  # carbon stock in woodland (as co2) averaged over rotation
S_woods <- S_woods / 44 * 12  # convert from co2 to C
S_woods <- set_units(S_woods, kg / m^2)  # carbon stock in woodland averaged over rotation
S_woods <- S_woods / 12 * 44  # convert back from C to co2
S_woods # 36.7 kg CO2/m2

required_affn <- e_cow / S_woods  # m2 of woodland needed to offset one cow's emissions per year

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

df <- read.csv(url("https://raw.githubusercontent.com/NERC-CEH/beem_data/main/trees.csv"))
names(df)
setDT(df)
df[, tree_mass_c := tree_mass * 0.5]
df[, tree_mass_co2 := tree_mass_c / 12 * 44]
hist(df$tree_mass_co2)
p <- ggplot(df[species == "SS"], aes(x = dbh, y = tree_mass_co2)) +
  geom_point() +
  geom_smooth(method = "loess") +
  xlab("Age (years)") +
  ylab("Tree mass (kg CO2)") +
  theme_minimal()
p
plot(df$age, df$tree_mass_co2)
table(df[species == "SS", age])
df[, f_stem := stem_mass / (stem_mass + root_mass)]
f_stem <- mean(df[species == "SS", f_stem], na.rm = TRUE)

spacing <- 4  # m
trees_per_m2 <- 1 / (spacing ^ 2)
tree_mass_per_m2 <- df[species == "SS" & dbh > 40, mean(tree_mass_co2, na.rm = TRUE)] * trees_per_m2
tree_mass_per_m2 <- 2500 * trees_per_m2 * f_stem
