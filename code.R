library(rgdal)
library(sf)
library(viridis)

# Load dataset
orch_perc <- read_csv("orch_perc.csv")

# Load map by county
USAmapCO <- read_sf("C:/Users/diego.rincon/Desktop/R projects/WTFmaps/map/coUSAKHI_LAEA_2022.shp")

# Obtain percentages of crop land from percentages of orchard land
orch_perc$apples <- (orch_perc$perc_area * orch_perc$apples) / 100
orch_perc$cherries <- (orch_perc$perc_area * orch_perc$cherries) / 100
orch_perc$tart_cherries <- (orch_perc$perc_area * orch_perc$tart_cherries) / 100
orch_perc$Peaches <- (orch_perc$perc_area * orch_perc$Peaches) / 100
orch_perc$Pears <- (orch_perc$perc_area * orch_perc$Pears) / 100
orch_perc$Plums <- (orch_perc$perc_area * orch_perc$Plums) / 100
orch_perc$Almonds <- (orch_perc$perc_area * orch_perc$Almonds) / 100
orch_perc$Pecans <- (orch_perc$perc_area * orch_perc$Pecans) / 100
orch_perc$Walnuts <- (orch_perc$perc_area * orch_perc$Walnuts) / 100

# Sum all crops into a single variable
orch_perc$WFT <- rowSums(orch_perc[, 3:11])

# Bind the data to the shp file
USAmapCO <- cbind(USAmapCO, orch_perc)

# Remove AL and HI
USAmapCO <- USAmapCO[-seq(1, 10)][-grep("02|15", substr(USAmapCO$atlas_stco, 1, 2)), ]

# Plot
par(oma = c(3, 2, 3, 2), xpd = NA)

plot(USAmapCO[1], pal = viridis, main = "Acres of land in orchards as % of cropland acreage 2022\n\n", cex = 2.5)
plot(USAmapCO[11], pal = viridis, main = "Acres of land in orchards as % of cropland acreage 2022\n(excluding citrus, grapes and avocados)\n")
