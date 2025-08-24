# Analisis Spasial Penyebaran DBD dengan R Shiny

Proyek ini merupakan aplikasi **R Shiny** untuk analisis spasial penyebaran **Demam Berdarah Dengue (DBD)** di Kabupaten Jember.  
Aplikasi terbagi menjadi **dua bagian**:  
1. **User App** → menampilkan analisis, tren kasus, peta spasial, dan regresi spasial.  
2. **Admin Panel** → dashboard untuk admin dalam mengelola data, validasi input, dan monitoring dataset.  

Keduanya terhubung dengan **PostgreSQL + PostGIS** untuk pengelolaan data spasial dan atribut.  

## 🚀 Fitur Utama
### User App
- Dashboard interaktif dengan `shinydashboard`
- Peta interaktif menggunakan `leaflet`
- Analisis autokorelasi spasial (Moran’s I)
- Analisis regresi spasial (SAR, SEM)
- Tren temporal kasus DBD (kabupaten & kecamatan)
- Tabel interaktif menggunakan `DT`

### Admin Panel
- Upload & update data kasu
- Ringkasan dataset per periode
- Validasi input sebelum masuk ke database

## 📂 Struktur Proyek
dbd-spatial-analysis/
├── user_app/
│ ├── app.R # File utama User App
│ ├── utils.R # Library & konfigurasi umum
│ ├── conn.R # Koneksi ke database PostgreSQL
│ ├── model.R # Logika pengambilan & transformasi data
│ ├── ui.R # User Interface
│ ├── output.R # Definisi output (grafik, peta, tabel)
│ └── footer.R # Informasi teks footer
│
├── admin_panel/
│ ├── app.R # File utama Admin Panel
│ ├── ui.R # Tampilan admin dashboard
│ ├── server.R # Logika server
│ └── utils.R # Fungsi pendukung (upload, validasi data)
│
└── README.md # Dokumentasi proyek

---

## ⚙️ Persiapan
### 1. Instalasi R Package
```R
install.packages(c(
  "shiny", "shinydashboard", "leaflet", "sf", "dplyr",
  "DBI", "RPostgres", "DT", "GWmodel", "sp", "spdep", "spatialreg",
  "lmtest", "car", "ggplot2", "plotly", "htmltools", "tidyr",
  "shinyWidgets"
))
```
### 2. Konfigurasi Database
```R
db_host <- "localhost"
db_port <- 5432
db_name <- "spatial_db"
db_user <- "geoid"
db_password <- "user123"
```

### 3. Jalankan Aplikasi
```R
User_App <- shiny::runApp()
Admin-Panel <- shiny::runApp()
```
## Skripsi: IDENTIFIKASI POLA SPASIAL DAN PEMODELAN FAKTOR RISIKO DEMAM BERDARAH DENGUE DI KABUPATEN JEMBER BERBASIS GI
###Dibuat oleh: [Aulia Novrin Harleyanto]
###Tahun: 2025

## Tampilan Dashboard User App <img width="1344" height="722" alt="image" src="https://github.com/user-attachments/assets/3ca55b67-a3b2-465e-9dda-5f5893bcab54" />


