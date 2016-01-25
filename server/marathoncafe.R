# Barvagter 
rm(list = ls()) # Clear workspace
print = 0

install_packages = 0
load_packages = 1
reso <- 91
udkomt <- 0 # 0 = udkommenteret

files <- dir("functions", full.names = TRUE)
for (i in 1:length(files)) source(files[i])

Initialize(install_packages, install_packages);

library(data.table) 
library(plotrix)
library(RSQLite)
library(grid)
library(gridExtra)
library(lattice)
library(zoo) 
library(httpRequest)

data <- Load_Data();

## Define variable "starttime"
starttime = data$utime[1]

Antal_Streger_Paa_Hvert_Koekken <- Beregn_Antal_Streger(data)
### SKAL SKRIVES TIL HTTP 

data <- Beregn_Kumuleret_Antal_Streger(data)
### SKAL SKRIVES TIL HTTP data$min,data$cumst

cheats <- Antal_Streger_Paa_Koekken_Med_Barvagt(data)
### SKAL SKRIVES TIL HTTP cheats$vagt # De har r??kkef??lgen: G,M,N
data$vagt <- 0


datac <- Beregn_Kumuleret_Antal_Streger_For_Foerende_5(data)

C <- 100000

Hamringsmomentum_Koekkener <- Beregn_Hamringsmomentum_Fordelt_Paa_Koekkenerne(datac,Antal_Streger_Paa_Hvert_Koekken,C,print,reso)


Hamrings_Momentum <- Beregn_Hamringsmomentum(data,C)

