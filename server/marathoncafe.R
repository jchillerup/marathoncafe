# Barvagter 
rm(list = ls()) # Clear workspace
print = 1

install_packages = 0
load_packages = 1
reso <- 91
udkomt <- 0 # 0 = udkommenteret

files <- dir("functions", full.names = TRUE)
for (i in 1:length(files)) source(files[i])

library(data.table) 
library(plotrix)
library(RSQLite)
library(grid)
library(gridExtra)
library(lattice)
library(zoo) 
library(httpRequest)
library(jsonlite)

data <- Load_Data();

jsonToServer <- "{"

## Define variable "starttime"
starttime = data$utime[1]

Antal_Streger_Paa_Hvert_Koekken <- Beregn_Antal_Streger(data)
### SKAL SKRIVES TIL HTTP 

## jsonToServer <- paste(jsonToServer, "\"antal_streger_paa_hvert_koekken\": ")
## jsonToServer <- paste(jsonToServer, toJSON(Antal_Streger_Paa_Hvert_Koekken))
## jsonToServer <- paste(jsonToServer, ",")

data <- Beregn_Kumuleret_Antal_Streger(data)
### SKAL SKRIVES TIL HTTP data$min,data$cumst
jsonToServer <- paste(jsonToServer, "\"kumulerede_streger_min\": ")
jsonToServer <- paste(jsonToServer, toJSON(data$min))
jsonToServer <- paste(jsonToServer, ",")
jsonToServer <- paste(jsonToServer, "\"kumulerede_streger_cumst\": ")
jsonToServer <- paste(jsonToServer, toJSON(data$cumst))
jsonToServer <- paste(jsonToServer, ",")



cheats <- Antal_Streger_Paa_Koekken_Med_Barvagt(data)
### SKAL SKRIVES TIL HTTP cheats$vagt # De har r??kkef??lgen: G,M,N

jsonToServer <- paste(jsonToServer, "\"cheats\": ")
jsonToServer <- paste(jsonToServer, toJSON(cheats))
jsonToServer <- paste(jsonToServer, ",")

data$vagt <- 0


datac <- Beregn_Kumuleret_Antal_Streger_For_Foerende_5(data)

C <- 5000

Hamringsmomentum_Koekkener <- Beregn_Hamringsmomentum_Fordelt_Paa_Koekkenerne(datac,Antal_Streger_Paa_Hvert_Koekken,C,print,reso)
jsonToServer <- paste(jsonToServer, "\"hamringsmomentum_koekkener\": ")
jsonToServer <- paste(jsonToServer, toJSON(Hamringsmomentum_Koekkener))
jsonToServer <- paste(jsonToServer, ",")


Hamrings_Momentum <- Beregn_Hamringsmomentum(data,C)
jsonToServer <- paste(jsonToServer, "\"hamringsmomentum_total\": ")
jsonToServer <- paste(jsonToServer, toJSON(Hamrings_Momentum$momm))


# Luk JSON objekt og send afsted
jsonToServer <- paste(jsonToServer, "}")
simplePostToHost("127.0.0.1", "/", jsonToServer, "", "application/json", 8081)

