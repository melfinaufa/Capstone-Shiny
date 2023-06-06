# Import Library Dashboard
library(shiny)
library(shinydashboard)

# Import Library Visualization
library(readxl)
library(dplyr) # untuk transformasi data
library(plotly) # untuk membuat plot menjadi interaktif
library(glue) # untuk custom informasi saat plot interaktif
library(scales) # untuk custom keterangan axis atau lainnya
library(tidyr) # untuk custom keterangan axis atau lainnya
library(stringr)# untuk melakuan kustom teks pada tooltip
library(lubridate) # utntuk mengolah data tanggal
library(leaflet)
library(geojsonio)

# Import Library Read Table
library(DT) # untuk menampilkan dataset

# Setting Agar tidak muncul numeric value
options(scipen = 9999)


# Data Preparation


disaster <- read_excel("Data Bencana.xlsx")
disaster$Tanggal_Kejadian <- ymd(disaster$Tanggal_Kejadian)



# Data Cleansing
# Hilangkan kolom yang tidak dipakai
# Mengubah Tipe Data


disaster_clean <-  disaster %>% 
  select(-c("No.", "Kode_Identitas_Bencana", "Kronologi & Dokumentasi")) %>% 
  mutate(ID_Kabupaten = as.factor(ID_Kabupaten),
         Kejadian = as.factor(Kejadian),
         Kabupaten = as.factor(Kabupaten),
         Provinsi = as.factor(Provinsi),
         Meninggal = as.integer(Meninggal),
         Hilang = as.integer(Hilang),
         Terluka = as.integer(Terluka),
         Rumah_Rusak = as.integer(Rumah_Rusak), 
         Rumah_Terendam = as.integer(Rumah_Terendam), 
         Fasum_Rusak = as.integer(Fasum_Rusak)) %>% 
  mutate(Meninggal = coalesce(Meninggal,0)) %>% 
  mutate(Hilang = coalesce(Hilang,0)) %>% 
  mutate(Terluka = coalesce(Terluka,0)) %>% 
  mutate(Rumah_Rusak = coalesce(Rumah_Rusak,0)) %>% 
  mutate(Rumah_Terendam = coalesce(Rumah_Terendam,0)) %>% 
  na.omit(Lokasi, Penyebab) %>% 
  mutate(Bulan = month(Tanggal_Kejadian, label = TRUE, abbr = FALSE)) %>% 
  mutate(Tanggal = day(Tanggal_Kejadian)) 
  droplevels(disaster_clean$Bulan)




# Map Leaflet


perdismap <- as.data.frame(read_excel("Data Agg Per Kejadian.xlsx")) # untuk input data bencana
indo_sf <- readRDS("gadm36_IDN_1_sp.rds") # untuk data map polygon



# Data Preparation Map

# Mengubah huruf provinsi menjadi huruf besar semua

indo_sf@data <- indo_sf@data %>% 
  mutate(Provinsi = toupper(NAME_1)) 

# menyamakan nama provinsi antara ke dua data

indo_sf@data <- indo_sf@data %>% 
  mutate(Provinsi = case_when(Provinsi == "BANGKA BELITUNG" ~ "KEPULAUAN BANGKA BELITUNG",
                              Provinsi == "JAKARTA RAYA" ~ "DKI JAKARTA",
                              Provinsi == "YOGYAKARTA" ~ "DI YOGYAKARTA",
                              TRUE~Provinsi)) %>% 
  left_join(perdismap, by = c("Provinsi" = "Provinsi"))


  

