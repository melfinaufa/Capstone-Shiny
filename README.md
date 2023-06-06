# Disaster R Shiny Dashboard

Capstone Project Data Visualisation with R

Pada capstone kali ini, saya membuat dashboard bencana alam. Data diambil dari website pemerintah Geoportal dan Bencana Indonesia. Selang waktu yang ditarik adalah dari tanggal 1 Januari - 28 May 2023. Dengan mengaplikasikan R Shiny dapat lebih mudah untuk memonitor kejadian bencana alam di Indonesia ke dalam bentuk Dashboard sehingga lebih insightful.

Dashboard R Shiny Bencana Alam dapat dilihat melalui link berikut: https://melfinaufa.shinyapps.io/Capstone_Final/

# Library

library(shiny) : untuk membuat shiny
library(shinydashboard) : untuk membuat dashboard
library(readxl) : membaca data dalam format excel
library(dplyr) : untuk transformasi data
library(plotly) : untuk membuat plot menjadi interaktif
library(glue) : untuk custom informasi saat plot interaktif
library(scales) : untuk custom keterangan axis atau lainnya
library(tidyr) : untuk custom keterangan axis atau lainnya
library(stringr) : untuk melakuan kustom teks pada tooltip
library(lubridate) : untuk mengolah data tanggal
library(leaflet) : untuk membuaat peta ilustrasi persebaran provinsi Indonesia
library(DT) : untuk menampilkan dataset


# Features

Neat code for header-sidebar-body structure
Paragraph inside the body

Boxes :
- tabBox
- valueBox
- fluidRow

Dynamic Content:
- selectInput
- radioButtons
- dateRangeInput
- Interactive plot (plotly) output
- Leaflet output
- Data table output

Other:
- href link
- Hover tooltip

# UI

## Overview

## MenuItem 1 Plot 

![image.png](https://github.com/melfinaufa/Capstone-Shiny/blob/main/Screenshot%202023-06-06%20153348.png)


## menuItem 2 Graph

![image.png](https://github.com/melfinaufa/Capstone-Shiny/blob/main/Screenshot%202023-06-06%20153437.png)


## menuItem 3 Maph

![image.png](https://github.com/melfinaufa/Capstone-Shiny/blob/main/Screenshot%202023-06-06%20153510.png)


## menuItem 4 DataTable

![image.png](https://github.com/melfinaufa/Capstone-Shiny/blob/main/Screenshot%202023-06-06%20153630.png)





