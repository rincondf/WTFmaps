library(rgdal)
library(sf)
library(viridis)
library(readr)

# Load datasets
orch_perc <- read_csv("orch_perc.csv")
pistachios <- read_csv("pistachios.csv")
tot_acres <- read_csv("tot_acres.csv")

# function to build state and county codes
joinCod <- function(a, b) {
  
  resp <- rep(NA, length(a))
  for(i in 1: length(a)) {
    if(nchar(as.character(pistachios$coutny_n[i])) == 1)
      resp[i] <- paste0("0", as.character(pistachios$state_n[i]), "0", "0", pistachios$coutny_n[i])
    if(nchar(as.character(pistachios$coutny_n[i])) == 2)
      resp[i] <- paste0("0", as.character(pistachios$state_n[i]), "0", pistachios$coutny_n[i])
    if(nchar(as.character(pistachios$coutny_n[i])) == 3)
      resp[i] <- paste0("0", as.character(pistachios$state_n[i]), pistachios$coutny_n[i])
  }
  
  resp[which(nchar(resp) > 5)] <- substr(resp[which(nchar(resp) > 5)], 2, 6)
  
  resp
}

# obtaining pistachios acreage as percentage of havested cropland
pistachios <- data.frame(FIPS = as.numeric(joinCod(pistachios$state_n, pistachios$coutny_n)), pist_acres = pistachios$pist)
pistachios <- merge(pistachios, tot_acres, by = "FIPS")
pistachios$Pistachios <- (pistachios$pist_acres / pistachios$tot_acres) * 100

# Merge pistachios into the dataset
orch_perc <- merge(orch_perc, pistachios[, c(1, 4)], by = "FIPS", all = TRUE)
orch_perc[which(is.na(orch_perc$Pistachios)), "Pistachios"] <- 0

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

# Sum required crops into a single variable
orch_perc$WFT <- rowSums(orch_perc[, 3:12])
orch_perc$TNuts <- rowSums(orch_perc[, 9:12])
orch_perc$TNuts1 <- rowSums(orch_perc[, c(9, 11, 12)])

# Bind the data to the shp file
USAmapCO <- cbind(USAmapCO, orch_perc)

# Remove AK and HI
USAmapCO <- USAmapCO[-seq(1, 10)][-grep("02|15", substr(USAmapCO$atlas_stco, 1, 2)), ]

# Plot

pld <- c(viridis(14), "white")

par(oma = c(3, 2, 3, 2), xpd = NA)

plot(USAmapCO[1], pal = pld[15: 7], nbreaks = 10, main = "Acres of land in orchards as % of cropland acreage 2022\n\n", cex = 2.5)
plot(USAmapCO[12], pal = pld[15: 8], nbreaks = 8,
     main = "Acres of land in orchards as % of cropland acreage 2022\n(excluding citrus, grapes and avocados)\n")
plot(USAmapCO[13], pal = pld[15: 3], nbreaks = 13,
     main = "Acres of land in tree nuts as % of cropland acreage 2022\n(includes almonds, pecans, walnuts and pistachios)\n")
plot(USAmapCO[14], pal = pld[15: 3], nbreaks = 13,
     main = "Acres of land in tree nuts as % of cropland acreage 2022\n(includes almonds, walnuts and pistachios)\n")


