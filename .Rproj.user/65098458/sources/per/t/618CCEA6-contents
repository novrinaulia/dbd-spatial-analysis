# ui.R

# ------------------------------------------------------------------
# DEFINISI USER INTERFACE (UI) KESELURUHAN
# ------------------------------------------------------------------
# Hitung triwulan saat ini berdasarkan bulan
current_month <- as.numeric(format(Sys.Date(), "%m"))
current_triwulan <- ceiling(current_month / 3)
# Header Dashboard
ui_header <- dashboardHeader(title = "Peta Kerawanan DBD Jember")

# Sidebar Dashboard (Menu Navigasi)
ui_sidebar <- dashboardSidebar(
  sidebarMenu(
    # Menu utama Dashboard
    menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
    
    # Sub-menu untuk Data Tren Kasus (DIKEMBALIKAN KE SUB-MENU TERPISAH)
    menuItem("Data Tren Kasus", tabName = "data_tren_menu", icon = icon("chart-line"), # Kembali ke menu_utama
             menuSubItem("Tren Kasus DBD Jember", tabName = "trenKab", icon = icon("chart-simple")), # Sub-menu pertama
             menuSubItem("Tren Kasus DBD Kecamatan", tabName = "trenKec", icon = icon("chart-simple"))), # Sub-menu kedua
    
    # Sub-menu untuk Analisis Spasial
    menuItem("Analisis Spasial", tabName = "spatial_analysis_menu", icon = icon("chart-area"),
             menuSubItem("Moran's Global", tabName = "morans_global"),
             menuSubItem("LISA Bivariat", tabName = "lisa_bivariat"),
             menuSubItem("Regresi Spasial", tabName = "spatial_regression"))
  )
)

# Body Dashboard (Konten Utama)
ui_body <- dashboardBody(
  tabItems(
    # Tab Dashboard (Peta Utama)
    tabItem(tabName = "dashboard",
            fluidRow(
              box(
                title = textOutput("dbd_map_title"), # Judul peta yang dinamis
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                fluidRow(
                  column(width = 6,
                         airYearpickerInput(
                           inputId = "lisa_map_year",
                           label = "Pilih Tahun Kerawanan:",
                           value = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")), # Nilai default tahun saat ini
                           minDate = "1990-01-01",
                           maxDate = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
                           dateFormat = "yyyy",
                           autoClose = TRUE,
                           placeholder = "Pilih Tahun"
                         )
                  ),
                  column(width = 6,
                         selectInput("lisa_map_triwulan", "Pilih Triwulan Kerawanan:",
                                     choices = c("Pilih Triwulan"="", "Triwulan 1" = 1, "Triwulan 2" = 2, "Triwulan 3" = 3, "Triwulan 4" = 4),
                                     selected = current_triwulan) # Nilai default triwulan 1
                  ),
                  column(width = 12, # Kolom kecil untuk teks instruksi di bawah input
                           tags$small(class = "text-muted", style = "color: red; font-size: 14px;","Silakan pilih rentang tahun untuk menampilkan Peta Kerawanan Kasus DBD Kab. Jember.")
                  )
                ),
                br(), # Spasi
                leafletOutput("dbd_map", height = 500), # Output peta Leaflet
                
                # START: Memindahkan informasi peta kerawanan ke dalam box ini
                hr(), # Garis pemisah opsional antara peta dan informasi
                h4("Informasi Peta Kerawanan DBD:", style = "text-align: left; margin-bottom: 5px;"),
                htmlOutput("infoPetaKerawanan") # Informasi tambahan tentang peta
                # END: Memindahkan informasi peta kerawanan
              )
            )),
    
    # Tab Tren Kasus DBD Kabupaten (DIKEMBALIKAN SEBAGAI TAB TERPISAH)
    tabItem(tabName = "trenKab", # tabName asli
            fluidRow( # Satu fluidRow besar untuk menampung satu box utama
              box(
                title = "Tren Kasus DBD Kabupaten Jember", # Judul tunggal untuk seluruh box
                solidHeader = TRUE,
                status = "primary", # Status untuk box utama
                width = 12, # Box utama mengambil lebar penuh
                
                # 1. Inputan tahun samping-sampingan
                fluidRow( 
                  column(width = 6, # Kolom untuk input tahun awal
                         airYearpickerInput(
                           inputId = "start_year_tren_kab",
                           label = "Pilih Tahun Awal Tren Kasus:",
                           value = NULL,
                           minDate = "1990-01-01", # Sesuaikan dengan tahun minimum data Anda
                           maxDate = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
                           dateFormat = "yyyy",
                           autoClose = TRUE,
                           placeholder = "Pilih Tahun Awal"
                         )
                  ),
                  column(width = 6, # Kolom untuk input tahun akhir
                         airYearpickerInput(
                           inputId = "end_year_tren_kab",
                           label = "Pilih Tahun Akhir Tren Kasus:",
                           value = NULL,
                           minDate = "1990-01-01", # Sesuaikan dengan tahun minimum data Anda
                           maxDate = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
                           dateFormat = "yyyy",
                           autoClose = TRUE,
                           placeholder = "Pilih Tahun Akhir"
                         )
                  ),
                  column(width = 12, # Kolom kecil untuk teks instruksi di bawah input
                         tags$small(class = "text-muted", style = "color: red; font-size: 14px;","Silakan pilih rentang tahun untuk menampilkan data tren kasus DBD Kab. Jember.")
                  )
                ), # END inner fluidRow untuk input
                
                hr(), # Garis pemisah horizontal antara input dan plot
                
                # 2. Di bawahnya merupakan output (plot)
                plotlyOutput("time_series_plot_kab", height = 400),
                br() # Baris baru untuk spasi
                
                # 3. Informasi dari tren
                #h4("Informasi Tren Kasus & Kematian DBD Tingkat Kabupaten:", style = "text-align: left; margin-bottom: 5px;"), # Sub-judul untuk informasi
                #htmlOutput("infoTrenKasusKab")
              ) # END box utama
            )),
    
    # Tab Tren Kasus DBD Kecamatan (DIKEMBALIKAN SEBAGAI TAB TERPISAH)
    tabItem(tabName = "trenKec", # tabName asli
            fluidRow( # Satu fluidRow besar untuk menampung satu box utama
              box(
                title = "Tren Kasus DBD Kecamatan Jember", # Judul tunggal untuk seluruh box
                solidHeader = TRUE,
                status = "primary", # Status untuk box utama
                width = 12, # Box utama mengambil lebar penuh
                
                # 1. Inputan tahun dan triwulan samping-sampingan
                fluidRow(
                  column(width = 6, # Kolom untuk input tahun
                         airYearpickerInput(
                           inputId = "selected_year_kec_single",
                           label = "Pilih Tahun Tren Kasus:",
                           value = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                           minDate = "1990-01-01", # Sesuaikan dengan tahun minimum data Anda
                           maxDate = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
                           dateFormat = "yyyy",
                           autoClose = TRUE,
                           placeholder = "Pilih Tahun"
                         )
                  ),
                  column(width = 6, # Kolom untuk input triwulan
                         selectInput("selected_triwulan_kec", "Pilih Triwulan Tren Kasus:",
                                     choices = c("Pilih Triwulan"="", "Triwulan 1" = 1, "Triwulan 2" = 2, "Triwulan 3" = 3, "Triwulan 4" = 4), # Opsi triwulan
                                     selected = current_triwulan)
                  ),
                  column(width = 12, # Kolom kecil untuk teks instruksi di bawah input
                         tags$small(class = "text-muted", style = "color: red; font-size: 14px;", "Silakan pilih tahun dan triwulan untuk menampilkan data tren kasus DBD kecamatan.")
                  )
                ), # END inner fluidRow untuk input
                
                hr(), # Garis pemisah horizontal antara input dan plot
                
                # 2. Di bawahnya merupakan output (plot)
                h4(textOutput("trend_title_kec_single_year"), style = "text-align: center; margin-bottom: 10px;"), # Judul dinamis plot di dalam box
                plotlyOutput("time_series_plot_kec_single_year", height = 400),
                br(), # Baris baru untuk spasi
                
                # 3. Informasi dari tren
                h4("Informasi Tren Kasus DBD Tingkat Kecamatan:", style = "text-align: left; margin-bottom: 5px;"), # Sub-judul untuk informasi
                htmlOutput("infoTrenKasusKec")
              ) # END box utama
            )),
    
    # Tab Moran's Global (placeholder)
    tabItem(tabName = "morans_global",
            fluidRow(
              box(
                title = textOutput("moran_global_title"), # Judul dinamis di bagian atas box
                solidHeader = TRUE,
                status = "primary",
                width = 12, # Box utama yang mencakup semua konten
                fluidRow( # Inner fluidRow untuk input tahun/triwulan
                  column(width = 6, # Untuk input tahun
                         airYearpickerInput(
                           inputId = "moran_year",
                           label = "Pilih Tahun Indeks Moran:",
                           value = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                           minDate = "1990-01-01",
                           maxDate = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
                           dateFormat = "yyyy",
                           autoClose = TRUE,
                           placeholder = "Pilih Tahun"
                         )
                  ),
                  column(width = 6, # Untuk input triwulan
                         selectInput("moran_triwulan", "Pilih Triwulan Indeks Moran:",
                                     choices = c("Pilih Triwulan"="", "Triwulan 1" = 1, "Triwulan 2" = 2, "Triwulan 3" = 3, "Triwulan 4" = 4),
                                     selected = current_triwulan)
                  ),
                  column(width = 12,
                         tags$small(class = "text-muted", style = "color: red; font-size: 14px;", "Pilih tahun dan triwulan untuk melihat hasil Indeks Moran Global.")
                  )
                ),
                hr(), # Garis pemisah
                plotlyOutput("moran_scatter_plot", height = 400), # Scatter plot Moran
                br(), # Spasi
                h4("Hasil Indeks Moran Global:", style = "text-align: center;"), # Sub-judul untuk hasil Moran
                verbatimTextOutput("morans_output") # Output Moran's I summary
              )
            )
    ),
    # Tab LISA Bivariat
    tabItem(tabName = "lisa_bivariat",
            fluidRow(
              box(
                title = textOutput("lisa_bivariat_title"), # Judul dinamis
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                fluidRow(
                  column(width = 4,
                         airYearpickerInput(
                           inputId = "lisa_bivar_year",
                           label = "Pilih Tahun Lisa Bivariat:",
                           value = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")), # Default tahun saat ini
                           minDate = "1990-01-01",
                           maxDate = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
                           dateFormat = "yyyy",
                           autoClose = TRUE,
                           placeholder = "Pilih Tahun"
                         )
                  ),
                  column(width = 4,
                         selectInput("lisa_bivar_triwulan", "Pilih Triwulan Lisa Bivariat:",
                                     choices = c("Pilih Triwulan"="", "Triwulan 1" = 1, "Triwulan 2" = 2, "Triwulan 3" = 3, "Triwulan 4" = 4),
                                     selected = current_triwulan) # Default Triwulan 1
                  ),
                  column(width = 4,
                         selectInput("lisa_bivar_variable", "Pilih Variabel Independen Lisa Bivariat:",
                                     choices = c("Pilih Variabel"="",
                                                 "Curah Hujan" = "total_curah_hujan",
                                                 "Angka Bebas Jentik (ABJ)" = "total_abj",
                                                 "Suhu Rata-rata" = "avg_suhu",
                                                 "Kelembaban Rata-rata" = "avg_kelembaban",
                                                 "Kepadatan Penduduk" = "total_penduduk"),
                                     selected = "") # Biarkan kosong agar pengguna memilih
                  ),
                  column(width = 12,
                         tags$small(class = "text-muted", style = "color: red; font-size: 14px;", "Pilih tahun, triwulan, dan variabel Independen (X) untuk analisis LISA Bivariat Kasus DBD.")
                  )
                ), # END inner fluidRow untuk input
                
                hr(), # Garis pemisah
                leafletOutput("lisa_bivariat_map", height = 500), # Output peta LISA Bivariat
                br(), # Spasi
                h4("Interpretasi Klaster LISA Bivariat:", style = "text-align: left; margin-bottom: 5px;"),
                htmlOutput("infoLisaBivariat") # Informasi dan interpretasi dinamis
              )
            )),
    
    # Tab Regresi Spasial (placeholder)
    tabItem(tabName = "spatial_regression",
            fluidRow(
              box(title = textOutput("regression_title"), solidHeader = TRUE, status = "primary", width = 12,
                  fluidRow(
                    column(width = 4,
                           airYearpickerInput(
                             inputId = "regression_year",
                             label = "Pilih Tahun Regresi Spasial:",
                             value = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                             minDate = "1990-01-01",
                             maxDate = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
                             dateFormat = "yyyy",
                             autoClose = TRUE,
                             placeholder = "Pilih Tahun"
                           )
                    ),
                    column(width = 4,
                           selectInput("regression_triwulan", "Pilih Triwulan Regresi Spasial:",
                                       choices = c("Pilih Triwulan"="", "Triwulan 1" = 1, "Triwulan 2" = 2, "Triwulan 3" = 3, "Triwulan 4" = 4),
                                       selected = current_triwulan)
                    ),
                    column(width = 4,
                           selectInput("regression_var_dependen", "Variabel Dependen (Y):",
                                       choices = c("Pilih Variabel" = "",
                                                   "Kasus DBD" = "log_kasus"),
                                       selected = "log_kasus") # Default Kasus DBD
                    )
                  ),
                  fluidRow(
                    column(width = 12,
                           # Menggunakan checkboxGroupInput untuk memilih banyak variabel independen
                           checkboxGroupInput("regression_vars_independen", "Pilih Variabel Independen (X) Regresi Spasial:",
                                              choices = c("Curah Hujan" = "log_curah_hujan",
                                                          "Angka Bebas Jentik (ABJ)" = "total_abj",
                                                          "Suhu Rata-rata" = "avg_suhu",
                                                          "Kelembaban Rata-rata" = "avg_kelembaban",
                                                          "Kepadatan Penduduk" = "total_penduduk"), # Tambahkan Total Penduduk
                                              selected = c("log_curah_hujan", "total_abj", "avg_suhu", "avg_kelembaban", "total_penduduk")) # Default terpilih
                    ),
                    column(width = 12, # Kolom kecil untuk teks instruksi di bawah input
                           tags$small(class = "text-muted", style = "color: red; font-size: 14px;", "Silakan pilih tahun, triwulan, dan variabel independen (X) untuk menampilkan Hasil Analisis Regresi Spasial SLM dan SEM.")
                    )
                  ),
                  hr(),
                  h4("Hasil Regresi Spasial:", style = "text-align: left; margin-bottom: 5px;"),
                  verbatimTextOutput("regression_output"), # Ringkasan model SLM/SEM
                  br()
                  #h4("Evaluasi Model & Interpretasi:", style = "text-align: left; margin-bottom: 5px;"),
                  #htmlOutput("infoRegression") # Interpretasi dinamis
                  # plotlyOutput("regression_plot") # Opsional: Plot diagnostik
              )
            ))
  )
)

# Gabungkan semua komponen UI menjadi satu objek dashboardPage
ui_dashboard_page <- dashboardPage(
  ui_header,
  ui_sidebar,
  ui_body
)