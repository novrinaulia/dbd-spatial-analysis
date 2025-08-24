#utils.R

# Pastikan Anda telah menginstal paket-paket ini sebelumnya:
# install.packages(c("shiny", "shinydashboard", "leaflet", "sf", "dplyr", "DBI", "RPostgres", "DT",
#                    "GWmodel", "sp", "spdep", "tmap", "raster", "spatialreg",
#                    "lmtest", "readr", "readxl", "car", "ggplot2", "plotly", "htmltools", "tidyr",
#                    "shinyWidgets")) # Tambahkan shinyWidgets di sini jika belum terinstal

library(shiny)
library(shinydashboard)
library(leaflet)       # Untuk peta interaktif
library(sf)            # Untuk data spasial
library(dplyr)         # Untuk manipulasi data
library(DBI)           # Antarmuka umum database
library(RPostgres)     # Driver untuk PostgreSQL
library(DT)            # Untuk tabel interaktif
library(sp)            # Diperlukan oleh beberapa fungsi spdep/GWmodel
library(spdep)         # Untuk Moran's I dan bobot spasial
library(spatialreg)    # Untuk model regresi spasial (lagsarlm, errorsarlm)
library(lmtest)        # Untuk bptest, dwtest
library(car)           # Untuk vif
library(ggplot2)       # Untuk visualisasi (meskipun peta utama pakai leaflet)
library(plotly)        # Untuk plot interaktif
library(htmltools)     # Untuk HTML di label leaflet
library(tidyr)         # Untuk membersihkan dan merapikan data, seperti pivot_longer
library(shinyWidgets)  # Pastikan ini ada!
library(GWmodel)