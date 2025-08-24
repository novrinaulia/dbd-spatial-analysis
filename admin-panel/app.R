# app.R

# ------------------------------------------------------------------
# 1. PENGATURAN AWAL DAN MUAT LIBRARY
# ------------------------------------------------------------------
# Pastikan Anda telah menginstal paket-paket ini sebelumnya:
# install.packages(c("shiny", "shinydashboard", "leaflet", "sf", "dplyr", "DBI", "RPostgres", "DT",
#                    "htmltools", "shinyjs", "shinyWidgets")) 

library(shiny)
library(shinydashboard)
library(leaflet)       # Untuk peta interaktif
library(sf)            # Untuk data spasial
library(dplyr)         # Untuk manipulasi data
library(tidyr)
library(ggplot2)
library(plotly)
library(DBI)           # Antarmuka umum database
library(RPostgres)     # Driver untuk PostgreSQL
library(DT)            # Untuk tabel interaktif
library(htmltools)     # Untuk HTML di label leaflet
library(shinyjs)       # Memuat library shinyjs
library(shinyWidgets)  # Memuat library shinyWidgets
# ------------------------------------------------------------------
# 2. DEFINISI USER INTERFACE (UI)
# ------------------------------------------------------------------
# Hitung triwulan saat ini berdasarkan bulan
current_month <- as.numeric(format(Sys.Date(), "%m"))
current_triwulan <- ceiling(current_month / 3)
ui <- dashboardPage(
  # Header Dashboard
  dashboardHeader(title = "Admin-Panel"),
  
  # Sidebar Dashboard (Menu Navigasi)
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Info", tabName = "dashboard_info", icon = icon("info-circle")),
      
      menuItem("Data Master", tabName = "data_master_menu", icon = icon("database"),
               menuSubItem("Data Kasus", tabName = "input_data_kasus", icon = icon("dna")),
               menuSubItem("Data Klimatologi", tabName = "input_data_klimatologi", icon = icon("cloud-sun")),
               menuSubItem("Data Demografi", tabName = "input_data_demografi", icon = icon("users"))
      )
    )
  ),
  
  # Body Dashboard (Konten Utama)
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "dashboard_info",
              fluidRow(
                box(
                  title = "Tren Kasus DBD Kabupaten Jember",
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12,
                  fluidRow(
                    column(12,
                           h4("Tren Kasus DBD Tahunan"),
                           shinyWidgets::airDatepickerInput( # === BARU: Input Tahun untuk Tren Tahunan ===
                             inputId = "select_trend_year_annual",
                             label = "Pilih Tahun untuk Tren Tahunan:",
                             value = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                             minDate = "1990-01-01",
                             maxDate = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
                             view = "years",
                             minView = "years",
                             dateFormat = "yyyy",
                             autoClose = TRUE,
                             width = '250px' # Mengatur lebar input
                           ),
                           plotOutput("plot_trend_annual_dbd", height = 350)
                    )
                  ),
                  tags$hr(), # Garis pemisah
                  fluidRow(
                    column(12,
                           h4("Tren Kasus DBD per Kecamatan"),
                           fluidRow( # Row untuk filter kecamatan, tahun, triwulan
                             column(6,
                                    shinyWidgets::airDatepickerInput( # === BARU: Input Tahun untuk Tren Kecamatan ===
                                      inputId = "select_trend_year_kecamatan",
                                      label = "Pilih Tahun untuk Tren Kecamatan:",
                                      value = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                                      minDate = "1990-01-01",
                                      maxDate = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
                                      view = "years",
                                      minView = "years",
                                      dateFormat = "yyyy",
                                      autoClose = TRUE,
                                      width = '100%'
                                    )
                             ),
                             column(6,
                                    selectInput("select_trend_triwulan", "Pilih Triwulan:",
                                                choices = c("Triwulan 1" = 1, "Triwulan 2" = 2,
                                                            "Triwulan 3" = 3, "Triwulan 4" = 4),
                                                selected = current_triwulan, width = '100%')
                             )
                           ),
                           plotOutput("plot_trend_kecamatan_dbd", height = 350)
                    )
                  )
                )
              )),
      
      # Tab Item untuk Input Data Kasus (Tampilan Default: Tabel)
      tabItem(tabName = "input_data_kasus",
              fluidRow(
                box(
                  title = textOutput("cases_table_title"), # Judul dinamis untuk tabel
                  solidHeader = TRUE,
                  status = "info",
                  width = 12, # Box ini sekarang mencakup seluruh lebar
                  fluidRow( # Gunakan fluidRow untuk menata elemen di dalam box
                    column(6, # Kolom kiri untuk filter tahun
                           shinyWidgets::airDatepickerInput(
                             inputId = "select_cases_year",
                             label = "Pilih Tahun:",
                             value = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")), # === UBAH: Inisialisasi langsung ke 1 Januari 2025 ===
                             minDate = "1990-01-01", # Sesuaikan dengan tahun minimum data Anda
                             maxDate = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")), # Hingga tahun saat ini (real system year)
                             view = "years",         # Mulai di tampilan tahun
                             minView = "years",      # Hanya izinkan memilih tahun
                             dateFormat = "yyyy",    # Format tampilan hanya tahun
                             autoClose = TRUE,       # Tutup setelah pemilihan
                           )
                    ),
                    column(6, align = "right", # Kolom kanan untuk tombol Input Data Baru
                           actionButton("show_add_kasus_modal", "Input Data Baru", icon = icon("plus"), class = "btn-success",
                                        style = "margin-top: 25px;") 
                    )
                  ),
                  tags$hr(), # Garis pemisah
                  DTOutput("cases_data_table_full") # Tabel data kasus di bawahnya
                )
              )),
      
      # Tab Item untuk Input Data Klimatologi
      tabItem(tabName = "input_data_klimatologi",
              fluidRow(
                box(
                  title = textOutput("klimatologi_table_title"),
                  solidHeader = TRUE,
                  status = "info",
                  width = 12,
                  fluidRow( # Tambahkan fluidRow untuk tata letak
                    column(6,
                           shinyWidgets::airDatepickerInput(
                             inputId = "select_klimatologi_year",
                             label = "Pilih Tahun:",
                             value = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                             minDate = "1990-01-01",
                             maxDate = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
                             view = "years",
                             minView = "years",
                             dateFormat = "yyyy",
                             autoClose = TRUE
                           )
                    ),
                    column(6, align = "right",
                           actionButton("show_add_klimatologi_modal", "Input Data Baru", icon = icon("plus"), class = "btn-success",
                                        style = "margin-top: 25px;")
                    )
                  ),
                  tags$hr(),
                  DTOutput("klimatologi_data_table_full") # Pastikan ID output DT ini ada
                )
              )),
      
      # Tab Item untuk Input Data Demografi
      tabItem(tabName = "input_data_demografi",
              fluidRow(
                box(
                  title = textOutput("demografi_table_title"),
                  solidHeader = TRUE,
                  status = "info",
                  width = 12,
                  fluidRow( # Tambahkan fluidRow untuk tata letak
                    column(6,
                           shinyWidgets::airDatepickerInput(
                             inputId = "select_demografi_year",
                             label = "Pilih Tahun:",
                             value = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
                             minDate = "1990-01-01",
                             maxDate = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
                             view = "years",
                             minView = "years",
                             dateFormat = "yyyy",
                             autoClose = TRUE
                           )
                    ),
                    column(6, align = "right",
                           actionButton("show_add_penduduk_modal", "Input Data Baru", icon = icon("plus"), class = "btn-success",
                                        style = "margin-top: 25px;")
                    )
                  ),
                  tags$hr(),
                  DTOutput("penduduk_data_table_full") # Pastikan ID output DT ini ada
                )
              ))
    )
  )
)

# ------------------------------------------------------------------
# 3. DEFINISI SERVER LOGIC
# ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive value untuk menyimpan tahun sistem saat ini (bukan lagi hardcode)
  current_system_year_actual <- reactiveVal(format(Sys.Date(), "%Y"))
  selected_filter_year <- reactiveVal(format(Sys.Date(), "%Y")) # Inisialisasi dengan tahun saat ini
  selected_filter_year_klimatologi <- reactiveVal(format(Sys.Date(), "%Y")) # Inisialisasi dengan tahun saat ini
  selected_filter_year_demografi <- reactiveVal(format(Sys.Date(), "%Y")) # Inisialisasi dengan tahun saat ini
  
  # Reactive values untuk plot tren
  selected_trend_year_annual <- reactiveVal(format(Sys.Date(), "%Y"))
  selected_trend_year_kecamatan <- reactiveVal(format(Sys.Date(), "%Y")) 
  
  # Reactive values untuk menyimpan data yang sedang ditampilkan di tabel
  cases_data_rv <- reactiveVal(NULL)
  klimatologi_data_rv <- reactiveVal(NULL)
  penduduk_data_rv <- reactiveVal(NULL)
  all_cases_years <- reactiveVal(NULL) # Akan menyimpan semua tahun unik dari tb_kasus
  
  # ----------------------------------------------------------------
  #  PENGATURAN KONEKSI DATABASE POSTGRESQL
  # ----------------------------------------------------------------
  db_host <- "localhost"
  db_port <- 5432
  db_name <- "spatial_db"
  db_user <- "geoid"
  db_password <- "user123"
  # Fungsi untuk membuat koneksi ke database
  connect_to_db <- function() {
    tryCatch({
      DBI::dbConnect(
        RPostgres::Postgres(),
        host = db_host,
        port = db_port,
        dbname = db_name,
        user = db_user,
        password = db_password
      )
    }, error = function(e) {
      stop(paste("Gagal terhubung ke database:", e$message,
                 "Pastikan detail koneksi (host, port, dbname, user, password) sudah benar."))
    })
  }
  
  # --- PROSES AMBIL DATA DARI DATABASE (hanya yang relevan untuk admin) ---
  
  # Ambil Daftar Lengkap Nama Kecamatan untuk Dropdown Admin Panel (id_region & kecamatan)
  all_kecamatan_data <- reactiveVal(NULL) # Menyimpan id_region dan kecamatan
  
  observeEvent(TRUE, { # ini akan berjalan SEKALI saat aplikasi dimulai
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      
      # Ambil data kecamatan (tetap sama)
      query_all_kecamatan <- "SELECT DISTINCT kecamatan, id_region FROM tb_jember ORDER BY kecamatan;"
      kecamatan_data <- DBI::dbGetQuery(con, query_all_kecamatan)
      all_kecamatan_data(kecamatan_data)
      
      # Buat named vector untuk choices (nilai=id_region, nama=kecamatan)
      kec_choices <- c("Pilih Kecamatan" = "", setNames(kecamatan_data$id_region, kecamatan_data$kecamatan))
      
      # Update semua dropdown kecamatan di semua sub-menu input
      updateSelectInput(session, "input_kasus_kecamatan", choices = kec_choices)
      updateSelectInput(session, "input_klimatologi_kecamatan", choices = kec_choices)
      updateSelectInput(session, "input_penduduk_kecamatan", choices = kec_choices)
      
    }, error = function(e) {
      showNotification(
        paste("Error mengambil daftar kecamatan atau data awal:", e$message,
              "Pastikan detail koneksi (host, port, dbname, user, password) sudah benar."),
        type = "error",
        duration = NULL
      )
      all_kecamatan_data(NULL)
      all_cases_years(NULL)
    }, finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
    })
  }, once = TRUE) 
  
  #pantau select_year
  observeEvent(input$select_cases_year, {
    req(input$select_cases_year) # Pastikan ada nilai yang dipilih
    selected_filter_year(format(input$select_cases_year, "%Y"))
  }, ignoreNULL = FALSE) #
  # pantau select_year (Klimatologi)
  observeEvent(input$select_klimatologi_year, {
    req(input$select_klimatologi_year)
    selected_filter_year_klimatologi(format(input$select_klimatologi_year, "%Y"))
  }, ignoreNULL = FALSE) #
  # pantau select_year (Demografi)
  observeEvent(input$select_demografi_year, {
    req(input$select_demografi_year)
    selected_filter_year_demografi(format(input$select_demografi_year, "%Y"))
  }, ignoreNULL = FALSE) #
  # pantau select_year (Tren Tahunan)
  observeEvent(input$select_trend_year_annual, {
    req(input$select_trend_year_annual)
    selected_trend_year_annual(format(input$select_trend_year_annual, "%Y"))
  }, ignoreNULL = FALSE)
  # pantau select_year (Tren Kecamatan) 
  observeEvent(input$select_trend_year_kecamatan, {
    req(input$select_trend_year_kecamatan)
    selected_trend_year_kecamatan(format(input$select_trend_year_kecamatan, "%Y"))
  }, ignoreNULL = FALSE)
  
  # ----------------------------------------------------------------
  # --- LOGIKA UNTUK DATA MASTER (CRUD DATA) ---
  # ----------------------------------------------------------------
  # Helper function to get kecamatan name from id_region
  get_kecamatan_name <- function(id_region_lookup) {
    req(all_kecamatan_data())
    kec_map <- all_kecamatan_data()
    name <- filter(kec_map, id_region == id_region_lookup)$kecamatan[1]
    if(is.na(name) || is.null(name)) paste("ID_Region:", id_region_lookup) else name
  }
  
  # Helper function to fetch ALL data from DB for refresh of full tables
  fetch_all_cases_data <- reactive({
    # Ambil nilai tahun dari airDatepickerInput
    selected_year_str <- selected_filter_year() # Gunakan reactiveVal yang stabil
    
    query_condition <- paste("WHERE tahun =", as.integer(selected_year_str))
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      data <- DBI::dbGetQuery(con, sprintf("SELECT id_kasus, id_region, triwulan, tahun, kasus, mati, abj, created_at, updated_at FROM tb_kasus %s ORDER BY tahun DESC, triwulan DESC;", query_condition))
      
      data %>%
        mutate(Kecamatan = sapply(id_region, get_kecamatan_name)) %>%
        select(Kecamatan, Tahun = tahun, Triwulan = triwulan, 
               'Jumlah Kasus' = kasus, 'Jumlah Kematian' = mati, ABJ = abj,
               ID_Region = id_region # Tetap sertakan ID_Region untuk aksi Edit/Delete
        )
    }, error = function(e) {
      showNotification(paste("Error mengambil seluruh data kasus:", e$message), type = "error", duration = NULL)
      return(NULL)
    }, finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
    })
  })
  
  fetch_all_klimatologi_data <- reactive({
    selected_year_str <- selected_filter_year_klimatologi() # Gunakan reactiveVal yang stabil
    
    query_condition <- paste("WHERE tahun =", as.integer(selected_year_str))
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      data <- DBI::dbGetQuery(con, sprintf("SELECT id_iklim, id_region, triwulan, tahun, suhu, curah_hujan, kelembaban, created_at, updated_at FROM tb_klimatologi %s ORDER BY tahun DESC, triwulan DESC;", query_condition))
      data %>%
        mutate(Kecamatan = sapply(id_region, get_kecamatan_name)) %>% # Tambahkan kolom Kecamatan
        select(Kecamatan, Triwulan = triwulan, Suhu = suhu, 'Curah Hujan' = curah_hujan, Kelembaban = kelembaban, # Kolom yang ingin ditampilkan
               ID_Region = id_region, Tahun = tahun, # Tetap sertakan ID_Region dan Tahun untuk keperluan Edit/Delete
               ID_Iklim = id_iklim # Tetap sertakan ID_Iklim jika diperlukan untuk internal, tapi akan disembunyikan di DT
        )
    }, error = function(e) {
      showNotification(paste("Error mengambil seluruh data klimatologi:", e$message), type = "error", duration = NULL)
      return(NULL)
    }, finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
    })
  })
  
  fetch_all_penduduk_data <- reactive({
    selected_year_str <- selected_filter_year_demografi() # Gunakan reactiveVal yang stabil
    
    query_condition <- paste("WHERE tahun =", as.integer(selected_year_str))
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      data <- DBI::dbGetQuery(con, sprintf("SELECT id_penduduk, id_region, tahun, penduduk, created_at, updated_at FROM tb_penduduk %s ORDER BY tahun DESC;", query_condition))
      data %>%
        mutate(Kecamatan = sapply(id_region, get_kecamatan_name)) %>%
        select(Kecamatan, 'Kepadatan Penduduk' = penduduk, # Kolom yang ingin ditampilkan
               ID_Region = id_region, Tahun = tahun, # Tetap sertakan ID_Region dan Tahun untuk keperluan Edit/Delete
               ID_Penduduk = id_penduduk # Tetap sertakan ID_Iklim jika diperlukan untuk internal, tapi akan disembunyikan di DT
        )
    }, error = function(e) {
      showNotification(paste("Error mengambil seluruh data penduduk:", e$message), type = "error", duration = NULL)
      return(NULL)
    }, finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
    })
  })
  
  # --- Pemicu Refresh Tabel Utama ---
  # Pemicu refresh saat nilai filter tahun berubah untuk masing-masing tabel
  observeEvent(input$select_cases_year, {
    cases_data_rv(fetch_all_cases_data())
  }, ignoreNULL = FALSE) # === PENTING: ensure this runs even if input is initially NULL ===
  observeEvent(input$select_klimatologi_year, {
    klimatologi_data_rv(fetch_all_klimatologi_data())
  }, ignoreNULL = FALSE)
  observeEvent(input$select_demografi_year, {
    penduduk_data_rv(fetch_all_penduduk_data())
  }, ignoreNULL = FALSE)
  
  # Refresh data tables when corresponding tab is selected
  observeEvent(input$sidebarMenu, {
    req(input$sidebarMenu)
    if (input$sidebarMenu == "input_data_kasus") {
      # Trigger refresh of data based on current dateInput value
      cases_data_rv(fetch_all_cases_data())  
      
      # Reset formulir saat masuk tab ini
      updateSelectInput(session, "input_kasus_kecamatan", selected = "")
      updateNumericInput(session, "input_kasus_tahun", value = as.numeric(format(Sys.Date(), "%Y")))
      updateSelectInput(session, "input_kasus_triwulan", selected = 1)
      updateNumericInput(session, "input_kasus_jumlah", value = 0)
      updateNumericInput(session, "input_kasus_mati", value = 0)
      updateNumericInput(session, "input_kasus_abj", value = 0, min = 0, max = 100) 
      updateTextInput(session, "kasus_edit_id", value = "")
      output$kasus_form_status <- renderUI(NULL)
    } else if (input$sidebarMenu == "input_data_klimatologi") {
      klimatologi_data_rv(fetch_all_klimatologi_data())
      updateSelectInput(session, "input_klimatologi_kecamatan", selected = "")
      updateNumericInput(session, "input_klimatologi_tahun", value = as.numeric(format(Sys.Date(), "%Y")))
      updateSelectInput(session, "input_kasus_triwulan", selected = 1)
      updateNumericInput(session, "input_klimatologi_curah_hujan", value = 0)
      updateNumericInput(session, "input_klimatologi_suhu", value = 0)
      updateNumericInput(session, "input_klimatologi_kelembaban", value = 0)
      updateTextInput(session, "klimatologi_edit_id", value = "")
      output$klimatologi_form_status <- renderUI(NULL)
    } else if (input$sidebarMenu == "input_data_demografi") {
      penduduk_data_rv(fetch_all_penduduk_data())
      updateSelectInput(session, "input_penduduk_kecamatan", selected = "")
      updateNumericInput(session, "input_penduduk_tahun", value = as.numeric(format(Sys.Date(), "%Y")))
      updateNumericInput(session, "input_penduduk_jumlah", value = 0)
      updateTextInput(session, "penduduk_edit_id", value = "")
      output$penduduk_form_status <- renderUI(NULL)
    }
  }, ignoreNULL = FALSE) 
  
  # Trigger refresh for tables after a successful CRUD operation and year filter change
  observeEvent({
    input$add_new_kasus_data
    input$update_existing_kasus_data
    input$confirm_delete_kasus
    input$select_cases_year # === UBAH: select_cases_year (airDatepickerInput) sebagai pemicu refresh ===
  }, {
    cases_data_rv(fetch_all_cases_data())
  })
  
  # Logika refresh untuk data klimatologi
  observeEvent({
    input$add_new_klimatologi_data
    input$update_existing_klimatologi_data
    input$confirm_delete_klimatologi
    input$select_klimatologi_year # Ditambahkan pemicu dari filter tahun
  }, {
    klimatologi_data_rv(fetch_all_klimatologi_data())
  })
  
  # Logika refresh untuk data penduduk
  observeEvent({
    input$add_new_penduduk_data
    input$update_existing_penduduk_data
    input$confirm_delete_penduduk
    input$select_demografi_year # Ditambahkan pemicu dari filter tahun
  }, {
    penduduk_data_rv(fetch_all_penduduk_data())
  })
  
  observeEvent(input$add_new_klimatologi_data, klimatologi_data_rv(fetch_all_klimatologi_data()))
  observeEvent(input$update_existing_klimatologi_data, klimatologi_data_rv(fetch_all_klimatologi_data()))
  observeEvent(input$confirm_delete_klimatologi, klimatologi_data_rv(fetch_all_klimatologi_data()))
  
  observeEvent(input$add_new_penduduk_data, penduduk_data_rv(fetch_all_penduduk_data()))
  observeEvent(input$update_existing_penduduk_data, penduduk_data_rv(fetch_all_penduduk_data()))
  observeEvent(input$confirm_delete_penduduk, penduduk_data_rv(fetch_all_penduduk_data()))
  
  # --- Logika untuk Data Kasus (Input/Edit/Delete) ---
  
  # Judul dinamis untuk tabel data kasus
  output$cases_table_title <- renderText({
    # Dapatkan tahun yang dipilih dari input filter
    selected_year_for_title <- selected_filter_year()
    paste("Manajemen Data Kasus DBD Tahun", selected_year_for_title)
  })
  
  rv_delete_id_kasus <- reactiveVal(NULL)
  
  output$cases_data_table_full <- renderDT({
    data_display <- cases_data_rv()
    # Dapatkan tahun yang sedang difilter (dari airDatepickerInput) untuk pesan kosong ===
    #selected_year_filter_for_message <- if (is.null(input$select_cases_year)) {
    #  current_system_year_actual() # Jika belum ada nilai (misal saat startup), gunakan tahun berjalan contoh
    #} else {
    #  format(input$select_cases_year, "%Y") # Ambil tahun dari airDatepickerInput
    #}
    selected_year_filter_for_message <- selected_filter_year()
    
    # Logika pesan "data tidak ditemukan" yang lebih spesifik dan tanpa nomor baris ===
    if (is.null(data_display) || nrow(data_display) == 0) {
      return(datatable(data.frame(Status = paste0("Mohon Maaf Data Kasus DBD tahun ", selected_year_filter_for_message, " Kosong")), 
                       options = list(
                         dom = 't', # Hanya menampilkan tabel tanpa fitur pencarian/paginasi
                         ordering = FALSE, # Matikan sorting
                         paging = FALSE, # Matikan paginasi
                         info = FALSE, # Matikan "Showing X to Y of Z entries"
                         processing = FALSE, # Matikan "Processing..."
                         searching = FALSE, # Matikan search box
                         columnDefs = list(list(className = 'dt-center', targets = '_all')) # Pusatkan pesan
                       ),
                       rownames = FALSE # Hapus nomor baris
      ))
    }
    
    # Tambahkan kolom "Aksi" dengan tombol Edit dan Hapus
    data_display$Aksi <- paste0(
      '<div class="btn-group" role="group" aria-label="Aksi">',
      '<button type="button" class="btn btn-info btn-sm edit_btn" data-id="',
      data_display$ID_Region, '|', data_display$Tahun, '|', data_display$Triwulan,
      '">Edit</button>',
      '<button type="button" class="btn btn-danger btn-sm delete_btn" data-id="',
      data_display$ID_Region, '|', data_display$Tahun, '|', data_display$Triwulan,
      '">Hapus</button>',
      '</div>'
    )
    
    datatable(data_display, escape = FALSE, selection = 'none', rownames = FALSE, options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 25, 50),
      scrollX = TRUE,
      columnDefs = list(
        list(targets = c("ID_Region", "Tahun"), visible = FALSE), 
        list(width = '150px', targets = ncol(data_display) - 1), 
        list(className = 'dt-center', targets = '_all') 
      ),
      language = list(
        zeroRecords = "Mohon maaf, data tidak ditemukan." # Pesan default, tapi logika di atas lebih spesifik
      )
    ),
    callback = JS(
      "table.on('click', '.edit_btn', function() {",
      "  var id = $(this).data('id');",
      "  Shiny.onInputChange('dt_action_edit_kasus_id', id, {priority: 'event'});",
      "}),",
      "table.on('click', '.delete_btn', function() {",
      "  var id = $(this).data('id');",
      "  Shiny.onInputChange('dt_action_delete_kasus_id', id, {priority: 'event'});",
      "});"
    )
    )
  })
  
  # Observer untuk menampilkan Modal Form Kasus (Tombol "Input Data Baru")
  observeEvent(input$show_add_kasus_modal, {
    # Reset formulir saat modal dibuka untuk input data baru
    updateSelectInput(session, "input_kasus_kecamatan", selected = "")
    updateNumericInput(session, "input_kasus_tahun", value = as.numeric(format(Sys.Date(), "%Y")))
    updateSelectInput(session, "input_kasus_triwulan", selected = 1)
    updateNumericInput(session, "input_kasus_jumlah", value = 0)
    updateNumericInput(session, "input_kasus_mati", value = 0)
    updateNumericInput(session, "input_kasus_abj", value = 0, min = 0, max = 100) 
    updateTextInput(session, "kasus_edit_id", value = "")
    output$kasus_form_status <- renderUI(NULL)
    
    shinyjs::enable("add_new_kasus_data")
    shinyjs::disable("update_existing_kasus_data")
    
    showModal(modalDialog(
      title = "Input Data Kasus DBD",
      size = "l",
      fluidRow(
        column(12,
               tags$div(style = "display: none;", textInput("kasus_edit_id", label = NULL, value = "")),
               selectInput("input_kasus_kecamatan", "Kecamatan:", choices = c("Pilih Kecamatan" = "")),
               numericInput("input_kasus_tahun", "Tahun:", value = as.numeric(format(Sys.Date(), "%Y")), min = 2000, max = 2100),
               selectInput(
                 "input_kasus_triwulan",
                 "Triwulan:",
                 choices = c("Triwulan 1" = 1, "Triwulan 2" = 2, "Triwulan 3" = 3, "Triwulan 4" = 4),
                 selected = 1 # Set default ke Triwulan 1
               ),
               numericInput("input_kasus_jumlah", "Jumlah Kasus:", value = 0, min = 0),
               numericInput("input_kasus_mati", "Jumlah Kematian:", value = 0, min = 0),
               numericInput("input_kasus_abj", "Persentase ABJ (%):", value = 0, min = 0, max = 100), 
               hr(),
               actionButton("add_new_kasus_data", "Tambah Data Baru", icon = icon("plus"), class = "btn-success"),
               #actionButton("update_existing_kasus_data", "Update Data Terpilih", icon = icon("edit"), class = "btn-primary"),
               actionButton("clear_kasus_form", "Reset Formulir", icon = icon("sync"), class = "btn-default"),
               br(),br(),
               htmlOutput("kasus_form_status")
        )
      ),
      footer = tagList(
        modalButton("Tutup")
      )
    ))
    current_choices <- all_kecamatan_data()
    if(!is.null(current_choices) && nrow(current_choices) > 0) {
      kec_choices <- c("Pilih Kecamatan" = "", setNames(current_choices$id_region, current_choices$kecamatan))
      updateSelectInput(session, "input_kasus_kecamatan", choices = kec_choices, selected = "")
    }
    
    # Tambahkan kode JavaScript ini di dalam modal
    shinyjs::runjs(
      "
    $(document).ready(function() {
      $('#input_kasus_jumlah, #input_kasus_mati, #input_kasus_abj').on('focus', function() {
        $(this).select();
      });
    });
    "
    )
  })
  
  # Logika saat tombol Edit diklik di tabel (mengisi formulir dalam modal)
  observeEvent(input$dt_action_edit_kasus_id, {
    req(input$dt_action_edit_kasus_id)
    
    split_id <- strsplit(input$dt_action_edit_kasus_id, "|", fixed = TRUE)[[1]]
    id_region_to_edit <- as.integer(split_id[1])
    tahun_to_edit <- as.integer(split_id[2])
    triwulan_to_edit <- as.integer(split_id[3])
    
    selected_data <- cases_data_rv() %>%
      filter(ID_Region == id_region_to_edit, Tahun == tahun_to_edit, Triwulan == triwulan_to_edit)
    
    req(nrow(selected_data) > 0)
    
    shinyjs::enable("update_existing_kasus_data")
    
    showModal(modalDialog(
      title = "Edit Data Kasus DBD",
      size = "l",
      fluidRow(
        column(12,
               tags$div(style = "display: none;", textInput("kasus_edit_id", label = NULL, value = input$dt_action_edit_kasus_id)),
               selectInput("input_kasus_kecamatan", "Kecamatan:", choices = c("Memuat..." = ""), selected = selected_data$ID_Region),
               numericInput("input_kasus_tahun", "Tahun:", value = selected_data$Tahun, min = 2000, max = 2100),
               selectInput(
                 "input_kasus_triwulan",
                 "Triwulan:",
                 choices = c("Triwulan 1" = 1, "Triwulan 2" = 2, "Triwulan 3" = 3, "Triwulan 4" = 4),
                 selected = selected_data$Triwulan # Tetap menggunakan nilai dari data yang diedit
               ),
               numericInput("input_kasus_jumlah", "Jumlah Kasus:", value = selected_data$`Jumlah Kasus`, min = 0),
               numericInput("input_kasus_mati", "Jumlah Kematian:", value = selected_data$`Jumlah Kematian`, min = 0),
               numericInput("input_kasus_abj", "Persentase ABJ (%):", value = selected_data$ABJ, min = 0, max = 100), 
               hr(),
               actionButton("update_existing_kasus_data", "Update Data Terpilih", icon = icon("edit"), class = "btn-primary"),
               br(),br(),
               htmlOutput("kasus_form_status")
        )
      ),
      footer = tagList(
        modalButton("Tutup")
      )
    ))
    current_choices <- all_kecamatan_data()
    if(!is.null(current_choices) && nrow(current_choices) > 0) {
      kec_choices <- c("Pilih Kecamatan" = "", setNames(current_choices$id_region, current_choices$kecamatan))
      updateSelectInput(session, "input_kasus_kecamatan", choices = kec_choices, selected = selected_data$ID_Region)
    }
    # Tambahkan kode JavaScript ini di dalam modal
    shinyjs::runjs(
      "
    $(document).ready(function() {
      $('#input_kasus_jumlah, #input_kasus_mati, #input_kasus_abj').on('focus', function() {
        $(this).select();
      });
    });
    "
    )
    shinyjs::disable("input_kasus_kecamatan")
    shinyjs::disable("input_kasus_tahun")
    shinyjs::disable("input_kasus_triwulan")
  })
  
  # Logika saat tombol Hapus diklik di tabel (memunculkan konfirmasi)
  observeEvent(input$dt_action_delete_kasus_id, {
    req(input$dt_action_delete_kasus_id)
    
    rv_delete_id_kasus(input$dt_action_delete_kasus_id)
    
    split_id <- strsplit(rv_delete_id_kasus(), "|", fixed = TRUE)[[1]]
    id_region_display <- as.integer(split_id[1])
    tahun_display <- as.integer(split_id[2])
    triwulan_display <- as.integer(split_id[3])
    
    showModal(modalDialog(
      title = "Konfirmasi Penghapusan",
      paste0("Anda yakin ingin menghapus data kasus untuk Kecamatan: ", get_kecamatan_name(id_region_display),
             ", Tahun: ", tahun_display, ", Triwulan: ", triwulan_display, "?"),
      footer = tagList(
        modalButton("Batal"),
        actionButton("confirm_delete_kasus", "Hapus", class = "btn-danger")
      )
    ))
  })
  
  # Observer saat konfirmasi hapus ditekan
  observeEvent(input$confirm_delete_kasus, {
    req(rv_delete_id_kasus())
    
    split_id <- strsplit(rv_delete_id_kasus(), "|", fixed = TRUE)[[1]]
    id_region_to_delete <- as.integer(split_id[1])
    tahun_to_delete <- as.integer(split_id[2])
    triwulan_to_delete <- as.integer(split_id[3])
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      query_delete <- sprintf("
        DELETE FROM tb_kasus
        WHERE id_region = %s AND tahun = %s AND triwulan = %s;
      ", id_region_to_delete, tahun_to_delete, triwulan_to_delete)
      DBI::dbExecute(con, query_delete)
      
      status_msg <- paste0("Data kasus untuk Kecamatan: ", get_kecamatan_name(id_region_to_delete),
                           ", Tahun: ", tahun_to_delete, ", Triwulan: ", triwulan_to_delete,
                           " berhasil **dihapus**.")
      output$kasus_form_status <- renderUI({ HTML(paste0("<p style='color:green;'>", status_msg, "</p>")) })
      showNotification(status_msg, type = "message", duration = 3)
      
      cases_data_rv(fetch_all_cases_data()) # Refresh tabel untuk menampilkan data sesuai filter tahun sistem saat ini
      rv_delete_id_kasus(NULL)
    }, error = function(e) {
      error_msg <- paste("Error menghapus data kasus:", e$message)
      output$kasus_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal menghapus:</b> ", error_msg, "</p>")) })
      showNotification(error_msg, type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con)) { DBI::dbDisconnect(con) }
      removeModal()
    })
  })
  
  # Logika untuk tombol "Tambah Data Baru" (di dalam modal)
  observeEvent(input$add_new_kasus_data, {
    # Validasi input: pastikan semua input ada nilainya sebelum diproses
    req(input$input_kasus_kecamatan,
        input$input_kasus_tahun,
        input$input_kasus_triwulan,
        input$input_kasus_jumlah,
        input$input_kasus_mati,
        input$input_kasus_abj) 
    
    if (!is.null(input$kasus_edit_id) && input$kasus_edit_id != "") {
      showNotification("Error: Formulir sedang dalam mode edit. Silakan 'Reset Formulir' untuk menambah data baru.", type = "error", duration = 5)
      output$kasus_form_status <- renderUI({ HTML("<p style='color:red;'><b>Gagal menambah:</b> Formulir tidak kosong. Reset dulu.</p>") })
      return()
    }
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      
      id_region_val <- as.integer(input$input_kasus_kecamatan)
      tahun_val <- as.integer(input$input_kasus_tahun)
      triwulan_val <- as.integer(input$input_kasus_triwulan)
      kasus_val <- as.integer(input$input_kasus_jumlah)
      mati_val <- as.integer(input$input_kasus_mati)
      abj_val <- as.numeric(input$input_kasus_abj) 
      
      if (is.na(id_region_val) || is.na(tahun_val) || is.na(triwulan_val) || is.na(kasus_val) || is.na(mati_val) || is.na(abj_val)) { 
        showNotification("Error: Semua input harus diisi dengan nilai yang valid.", type = "error")
        output$kasus_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal menambah:</b> Ada input yang kosong atau tidak valid.</p>")) })
        return()
      }
      
      # Cek duplikasi sebelum INSERT
      query_check <- sprintf("
        SELECT COUNT(*) FROM tb_kasus
        WHERE id_region = %s AND tahun = %s AND triwulan = %s;
      ", id_region_val, tahun_val, triwulan_val)
      exists <- DBI::dbGetQuery(con, query_check)$count > 0
      
      if (exists) {
        showNotification("Peringatan: Data untuk kombinasi Kecamatan, Tahun, Triwulan ini sudah ada. Gunakan 'Update Data Terpilih' jika ingin mengubahnya.", type = "warning", duration = 8)
        output$kasus_form_status <- renderUI({ HTML("<p style='color:orange;'><b>Peringatan:</b> Data sudah ada. Gunakan Update.</p>") })
        return()
      }
      
      # INSERT data baru
      query_insert <- sprintf("
        INSERT INTO tb_kasus (id_region, tahun, triwulan, kasus, mati, abj)
        VALUES (%s, %s, %s, %s, %s, %s);
      ", id_region_val, tahun_val, triwulan_val, kasus_val, mati_val, abj_val)
      DBI::dbExecute(con, query_insert)
      
      status_msg <- paste0("Data kasus baru untuk Kecamatan: ", get_kecamatan_name(id_region_val),
                           ", Tahun: ", tahun_val, ", Triwulan: ", triwulan_val,
                           " berhasil **ditambahkan**!")
      output$kasus_form_status <- renderUI({ HTML(paste0("<p style='color:green;'>", status_msg, "</p>")) })
      showNotification(status_msg, type = "message", duration = 3)
      cases_data_rv(fetch_all_cases_data()) 
      updateTextInput(session, "kasus_edit_id", value = "") 
      }, error = function(e) {
      error_msg <- paste("Error menambah data kasus:", e$message)
      output$kasus_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal menambah:</b> ", error_msg, "</p>")) })
      showNotification(error_msg, type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con)) { DBI::dbDisconnect(con) }
      removeModal()
    })
  })
  
  # Logika untuk tombol "Update Data Terpilih" (di dalam modal)
  observeEvent(input$update_existing_kasus_data, {
    if (is.null(input$kasus_edit_id) || input$kasus_edit_id == "") {
      showNotification("Peringatan: Tidak ada data yang dipilih untuk diupdate. Pilih baris di tabel dulu.", type = "warning", duration = 5)
      output$kasus_form_status <- renderUI({ HTML("<p style='color:orange;'><b>Peringatan:</b> Tidak ada data terpilih untuk diupdate.</p>") })
      return()
    }
    
    # Validasi input formulir
    req(input$input_kasus_kecamatan,
        input$input_kasus_tahun,
        input$input_kasus_triwulan,
        input$input_kasus_jumlah,
        input$input_kasus_mati,
        input$input_kasus_abj) 
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      
      split_id_original <- strsplit(input$kasus_edit_id, "|", fixed = TRUE)[[1]]
      original_tahun <- as.integer(split_id_original[2]) 
      original_triwulan <- as.integer(split_id_original[3]) 
      original_id_region <- as.integer(split_id_original[1]) 
      
      new_id_region <- as.integer(input$input_kasus_kecamatan)
      new_tahun <- as.integer(input$input_kasus_tahun)
      new_triwulan <- as.integer(input$input_kasus_triwulan)
      new_kasus <- as.integer(input$input_kasus_jumlah)
      new_mati <- as.integer(input$input_kasus_mati)
      new_abj <- as.numeric(input$input_kasus_abj) 
      
      if (is.na(new_id_region) || is.na(new_tahun) || is.na(new_triwulan) || is.na(new_kasus) || is.na(new_mati) || is.na(new_abj)) { 
        showNotification("Error: Semua input harus diisi dengan nilai yang valid.", type = "error")
        output$kasus_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal update:</b> Ada input yang kosong atau tidak valid.</p>")) })
        return()
      }
      
      # Cek apakah kunci unik (id_region, tahun, triwulan) berubah dan apakah sudah ada di DB
      if (original_id_region != new_id_region || original_tahun != new_tahun || original_triwulan != new_triwulan) {
        query_check_new_key <- sprintf("
          SELECT COUNT(*) FROM tb_kasus
          WHERE id_region = %s AND tahun = %s AND triwulan = %s;
        ", new_id_region, new_tahun, new_triwulan)
        new_key_exists <- DBI::dbGetQuery(con, query_check_new_key)$count > 0
        
        if (new_key_exists) {
          showNotification("Error: Kombinasi Kecamatan, Tahun, Triwulan yang baru sudah ada di database. Tidak bisa update.", type = "error", duration = NULL)
          output$kasus_form_status <- renderUI({ HTML("<p style='color:red;'><b>Gagal update:</b> Kombinasi kunci baru sudah ada.</p>") })
          return()
        }
      }
      
      # Lakukan UPDATE
      query_update <- sprintf("
        UPDATE tb_kasus
        SET id_region = %s, tahun = %s, triwulan = %s, kasus = %s, mati = %s, abj = %s, updated_at = NOW()
        WHERE id_region = %s AND tahun = %s AND triwulan = %s;
      ", new_id_region, new_tahun, new_triwulan, new_kasus, new_mati, new_abj,
                              original_id_region, original_tahun, original_triwulan)
      DBI::dbExecute(con, query_update)
      
      status_msg <- paste0("Data kasus untuk Kecamatan: ", get_kecamatan_name(original_id_region),
                           " (Tahun: ", original_tahun, ", Triwulan: ", original_triwulan, ")",
                           " berhasil **diupdate**!")
      output$kasus_form_status <- renderUI({ HTML(paste0("<p style='color:green;'>", status_msg, "</p>")) })
      showNotification(status_msg, type = "message", duration = 3)
      cases_data_rv(fetch_all_cases_data()) 
      }, error = function(e) {
      error_msg <- paste("Error update data kasus:", e$message)
      output$kasus_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal update:</b> ", error_msg, "</p>")) })
      showNotification(error_msg, type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con)) { DBI::dbDisconnect(con) }
      removeModal()
    })
  })
  
  # Logika untuk tombol "Reset Formulir" (di dalam modal)
  observeEvent(input$clear_kasus_form, {
    updateSelectInput(session, "input_kasus_kecamatan", selected = "")
    updateNumericInput(session, "input_kasus_tahun", value = as.numeric(format(Sys.Date(), "%Y")))
    updateSelectInput(session, "input_kasus_triwulan", selected = 1)
    updateNumericInput(session, "input_kasus_jumlah", value = 0)
    updateNumericInput(session, "input_kasus_mati", value = 0)
    updateNumericInput(session, "input_kasus_abj", value = 0) # === BARU: Reset ABJ ===
    updateTextInput(session, "kasus_edit_id", value = "")
    output$kasus_form_status <- renderUI(NULL)
    showNotification("Formulir kasus telah direset.", type = "message", duration = 3)
    
    shinyjs::enable("add_new_kasus_data")
    shinyjs::disable("update_existing_kasus_data")
  })
  
  # --- Logika untuk Data Klimatologi (Input/Edit/Delete) ---
  
  # reactiveVal untuk menyimpan ID yang akan dihapus sementara untuk Klimatologi
  output$klimatologi_table_title <- renderText({
    # Dapatkan tahun yang dipilih dari input filter
    selected_year_for_title <- selected_filter_year_klimatologi()
    paste("Manajemen Data Klimatologi Tahun", selected_year_for_title)
  })
  
  rv_delete_id_klimatologi <- reactiveVal(NULL)
  
  output$klimatologi_data_table_full <- renderDT({
    data_display <- klimatologi_data_rv() # Menggunakan reactiveVal
    
    selected_year_filter_for_message <- selected_filter_year_klimatologi() # Ambil tahun filter
    
    if (is.null(data_display) || nrow(data_display) == 0) {
      return(datatable(data.frame(Status = paste0("Mohon Maaf Data Klimatologi tahun ", selected_year_filter_for_message, " Kosong")),
                       options = list(
                         dom = 't', # Hanya menampilkan tabel tanpa fitur pencarian/paginasi
                         ordering = FALSE, # Matikan sorting
                         paging = FALSE, # Matikan paginasi
                         info = FALSE, # Matikan "Showing X to Y of Z entries"
                         processing = FALSE, # Matikan "Processing..."
                         searching = FALSE, # Matikan search box
                         columnDefs = list(list(className = 'dt-center', targets = '_all')) # Pusatkan pesan
                       ),
                       rownames = FALSE # Hapus nomor baris
      ))
    }
    
    data_display$Aksi <- paste0(
      '<div class="btn-group" role="group" aria-label="Aksi">',
      '<button type="button" class="btn btn-info btn-sm edit_btn" data-id="',
      data_display$ID_Region, '|', data_display$Tahun, '|', data_display$Triwulan,
      '">Edit</button>',
      '<button type="button" class="btn btn-danger btn-sm delete_btn" data-id="',
      data_display$ID_Region, '|', data_display$Tahun, '|', data_display$Triwulan,
      '">Hapus</button>',
      '</div>'
    )
    
    datatable(data_display, escape = FALSE, selection = 'none', rownames = FALSE, options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 25, 50),
      scrollX = TRUE,
      columnDefs = list(
        list(targets = c("ID_Iklim", "ID_Region", "Tahun"), visible = FALSE), # Sembunyikan ID_Iklim dan ID_Region
        list(width = '150px', targets = ncol(data_display) - 1),
        list(className = 'dt-center', targets = '_all')
      )
    ),
    callback = JS(
      "table.on('click', '.edit_btn', function() {",
      "  var id = $(this).data('id');",
      "  Shiny.onInputChange('dt_action_edit_klimatologi_id', id, {priority: 'event'});",
      "});",
      "table.on('click', '.delete_btn', function() {",
      "  var id = $(this).data('id');",
      "  Shiny.onInputChange('dt_action_delete_klimatologi_id', id, {priority: 'event'});",
      "});"
    )
    )
  })
  
  observeEvent(input$show_add_klimatologi_modal, {
    # Reset formulir saat modal dibuka untuk input data baru
    updateSelectInput(session, "input_klimatologi_kecamatan", selected = "")
    updateNumericInput(session, "input_klimatologi_tahun", value = as.numeric(format(Sys.Date(), "%Y")))
    updateSelectInput(session, "input_klimatologi_triwulan", selected = 1)
    updateNumericInput(session, "input_klimatologi_curah_hujan", value = 0)
    updateNumericInput(session, "input_klimatologi_suhu", value = 0)
    updateNumericInput(session, "input_klimatologi_kelembaban", value = 0)
    updateTextInput(session, "klimatologi_edit_id", value = "")
    output$klimatologi_form_status <- renderUI(NULL)
    
    shinyjs::enable("add_new_klimatologi_data")
    shinyjs::disable("update_existing_klimatologi_data")
    
    showModal(modalDialog(
      title = "Input Data Klimatologi",
      size = "l",
      fluidRow(
        column(12,
               tags$div(style = "display: none;", textInput("klimatologi_edit_id", label = NULL, value = "")),
               selectInput("input_klimatologi_kecamatan", "Kecamatan:", choices = c("Pilih Kecamatan" = "")),
               numericInput("input_klimatologi_tahun", "Tahun:", value = as.numeric(format(Sys.Date(), "%Y")), min = 2000, max = 2100),
               selectInput(
                 "input_klimatologi_triwulan",
                 "Triwulan:",
                 choices = c("Triwulan 1" = 1, "Triwulan 2" = 2, "Triwulan 3" = 3, "Triwulan 4" = 4),
                 selected = 1
               ),
               numericInput("input_klimatologi_curah_hujan", "Curah Hujan (mm):", value = 0, min = 0),
               numericInput("input_klimatologi_suhu", "Suhu (°C):", value = 0, min = -50, max = 50),
               numericInput("input_klimatologi_kelembaban", "Kelembaban (%):", value = 0, min = 0, max = 100),
               hr(),
               actionButton("add_new_klimatologi_data", "Tambah Data Baru", icon = icon("plus"), class = "btn-success"),
               actionButton("clear_klimatologi_form", "Reset Formulir", icon = icon("sync"), class = "btn-default"),
               br(),br(),
               htmlOutput("klimatologi_form_status")
        )
      ),
      footer = tagList(
        modalButton("Tutup")
      )
    ))
    current_choices <- all_kecamatan_data()
    if(!is.null(current_choices) && nrow(current_choices) > 0) {
      kec_choices <- c("Pilih Kecamatan" = "", setNames(current_choices$id_region, current_choices$kecamatan))
      updateSelectInput(session, "input_klimatologi_kecamatan", choices = kec_choices, selected = "")
    }
    # Tambahkan kode JavaScript ini di dalam modal
    shinyjs::runjs(
      "
    $(document).ready(function() {
      $('#input_klimatologi_curah_hujan, #input_klimatologi_suhu, #input_klimatologi_kelembaban').on('focus', function() {
        $(this).select();
      });
    });
    "
    )
  })
  
  observeEvent(input$dt_action_edit_klimatologi_id, {
    req(input$dt_action_edit_klimatologi_id)
    split_id <- strsplit(input$dt_action_edit_klimatologi_id, "|", fixed = TRUE)[[1]]
    id_region_to_edit <- as.integer(split_id[1])
    tahun_to_edit <- as.integer(split_id[2])
    triwulan_to_edit <- as.integer(split_id[3])
    
    selected_data <- klimatologi_data_rv() %>%
      filter(ID_Region == id_region_to_edit, Tahun == tahun_to_edit, Triwulan == triwulan_to_edit)
    req(nrow(selected_data) > 0)
    
    # Atur status tombol dengan shinyjs
    shinyjs::disable("add_new_klimatologi_data")
    shinyjs::enable("update_existing_klimatologi_data")
    
    # Menampilkan modal dengan form yang terisi
    showModal(modalDialog(
      title = "Edit Data Klimatologi",
      size = "l",
      fluidRow(
        column(12,
               tags$div(style = "display: none;", textInput("klimatologi_edit_id", label = NULL, value = input$dt_action_edit_klimatologi_id)),
               selectInput("input_klimatologi_kecamatan", "Kecamatan:", choices = c("Memuat..." = ""), selected = selected_data$ID_Region),
               numericInput("input_klimatologi_tahun", "Tahun:", value = selected_data$Tahun, min = 2000, max = 2100),
               selectInput("input_klimatologi_triwulan", "Triwulan:", choices = c(1, 2, 3, 4), selected = selected_data$Triwulan),
               numericInput("input_klimatologi_curah_hujan", "Curah Hujan (mm):", value = selected_data$`Curah Hujan`, min = 0), # Gunakan Curah_Hujan dari selected_data
               numericInput("input_klimatologi_suhu", "Suhu (°C):", value = selected_data$Suhu, min = -50, max = 50), # Gunakan Suhu dari selected_data
               numericInput("input_klimatologi_kelembaban", "Kelembaban (%):", value = selected_data$Kelembaban, min = 0, max = 100), # Gunakan Kelembaban dari selected_data
               hr(),
               #actionButton("add_new_klimatologi_data", "Tambah Data Baru", icon = icon("plus"), class = "btn-success"), # Sembunyikan saat edit
               actionButton("update_existing_klimatologi_data", "Update Data Terpilih", icon = icon("edit"), class = "btn-primary"),
               br(),br(),
               htmlOutput("klimatologi_form_status")
        )
      ),
      footer = tagList(
        modalButton("Tutup")
      )
    ))
    # Update choices for kecamatan dropdown in the modal
    current_choices <- all_kecamatan_data()
    if(!is.null(current_choices) && nrow(current_choices) > 0) {
      kec_choices <- c("Pilih Kecamatan" = "", setNames(current_choices$id_region, current_choices$kecamatan))
      updateSelectInput(session, "input_klimatologi_kecamatan", choices = kec_choices, selected = selected_data$ID_Region)
    }
    # Tambahkan kode JavaScript ini di dalam modal
    shinyjs::runjs(
      "
    $(document).ready(function() {
      $('#input_klimatologi_curah_hujan, #input_klimatologi_suhu, #input_klimatologi_kelembaban').on('focus', function() {
        $(this).select();
      });
    });
    "
    )
    shinyjs::disable("input_klimatologi_kecamatan")
    shinyjs::disable("input_klimatologi_tahun")
    shinyjs::disable("input_klimatologi_triwulan")
  })
  
  observeEvent(input$dt_action_delete_klimatologi_id, {
    req(input$dt_action_delete_klimatologi_id)
    rv_delete_id_klimatologi(input$dt_action_delete_klimatologi_id)
    
    split_id <- strsplit(rv_delete_id_klimatologi(), "|", fixed = TRUE)[[1]]
    id_region_display <- as.integer(split_id[1])
    tahun_display <- as.integer(split_id[2])
    triwulan_display <- as.integer(split_id[3])
    
    showModal(modalDialog(
      title = "Konfirmasi Penghapusan",
      paste0("Anda yakin ingin menghapus data Klimatologi untuk Kecamatan: ", get_kecamatan_name(id_region_display),
             ", Tahun: ", tahun_display, ", Triwulan: ", triwulan_display, "?"),
      footer = tagList(
        modalButton("Batal"),
        actionButton("confirm_delete_klimatologi", "Hapus", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_klimatologi, {
    req(rv_delete_id_klimatologi())
    
    split_id <- strsplit(rv_delete_id_klimatologi(), "|", fixed = TRUE)[[1]]
    id_region_to_delete <- as.integer(split_id[1])
    tahun_to_delete <- as.integer(split_id[2])
    triwulan_to_delete <- as.integer(split_id[3])
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      query_delete <- sprintf("
          DELETE FROM tb_klimatologi
          WHERE id_region = %s AND tahun = %s AND triwulan = %s;
        ", id_region_to_delete, tahun_to_delete, triwulan_to_delete)
      DBI::dbExecute(con, query_delete)
      
      status_msg <- paste0("Data Klimatologi untuk Kecamatan: ", get_kecamatan_name(id_region_to_delete),
                           ", Tahun: ", tahun_to_delete, ", Triwulan: ", triwulan_to_delete,
                           " berhasil **dihapus**.")
      output$klimatologi_form_status <- renderUI({ HTML(paste0("<p style='color:green;'>", status_msg, "</p>")) })
      showNotification(status_msg, type = "message", duration = 5)
      klimatologi_data_rv(fetch_all_klimatologi_data())
      rv_delete_id_klimatologi(NULL)
    }, error = function(e) {
      error_msg <- paste("Error menghapus data Klimatologi:", e$message)
      output$klimatologi_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal menghapus:</b> ", error_msg, "</p>")) })
      showNotification(error_msg, type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con)) { DBI::dbDisconnect(con) }
      removeModal()
    })
  })
  
  observeEvent(input$add_new_klimatologi_data, {
    req(input$input_klimatologi_kecamatan,
        input$input_klimatologi_tahun,
        input$input_klimatologi_triwulan,
        input$input_klimatologi_curah_hujan,
        input$input_klimatologi_suhu,
        input$input_klimatologi_kelembaban)
    if (!is.null(input$klimatologi_edit_id) && input$klimatologi_edit_id != "") {
      showNotification("Error: Formulir sedang dalam mode edit. Silakan 'Reset Formulir' untuk menambah data baru.", type = "error", duration = 5)
      output$klimatologi_form_status <- renderUI({ HTML("<p style='color:red;'><b>Gagal menambah:</b> Formulir tidak kosong. Reset dulu.</p>") })
      return()
    }
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      
      id_region_val <- as.integer(input$input_klimatologi_kecamatan)
      tahun_val <- as.integer(input$input_klimatologi_tahun)
      triwulan_val <- as.integer(input$input_klimatologi_triwulan)
      curah_hujan_val <- as.numeric(input$input_klimatologi_curah_hujan)
      suhu_val <- as.numeric(input$input_klimatologi_suhu)
      kelembaban_val <- as.numeric(input$input_klimatologi_kelembaban)
      
      if (is.na(id_region_val) || is.na(tahun_val) || is.na(triwulan_val) || is.na(curah_hujan_val) || is.na(suhu_val) || is.na(kelembaban_val)) {
        showNotification("Error: Semua input harus diisi dengan nilai yang valid.", type = "error")
        output$klimatologi_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal menambah:</b> Ada input yang kosong atau tidak valid.</p>")) })
        return()
      }
      
      query_check <- sprintf("
          SELECT COUNT(*) FROM tb_klimatologi
          WHERE id_region = %s AND tahun = %s AND triwulan = %s;
        ", id_region_val, tahun_val, triwulan_val)
      exists <- DBI::dbGetQuery(con, query_check)$count > 0
      
      if (exists) {
        showNotification("Peringatan: Data untuk kombinasi Kecamatan, Tahun, Triwulan ini sudah ada. Gunakan 'Update Data Terpilih' jika ingin mengubahnya.", type = "warning", duration = 8)
        output$klimatologi_form_status <- renderUI({ HTML("<p style='color:orange;'><b>Peringatan:</b> Data sudah ada. Gunakan Update.</p>") })
        return()
      }
      
      query_insert <- sprintf("
          INSERT INTO tb_klimatologi (id_region, tahun, triwulan, suhu, curah_hujan, kelembaban)
          VALUES (%s, %s, %s, %s, %s, %s);
        ", id_region_val, tahun_val, triwulan_val, suhu_val, curah_hujan_val, kelembaban_val) # Kolom sesuai skema DB
      DBI::dbExecute(con, query_insert)
      
      status_msg <- paste0("Data Klimatologi baru untuk Kecamatan: ", get_kecamatan_name(id_region_val),
                           ", Tahun: ", tahun_val, ", Triwulan: ", triwulan_val,
                           " berhasil **ditambahkan**!")
      output$klimatologi_form_status <- renderUI({ HTML(paste0("<p style='color:green;'>", status_msg, "</p>")) })
      showNotification(status_msg, type = "message", duration = 5)
      klimatologi_data_rv(fetch_all_klimatologi_data())
    }, error = function(e) {
      error_msg <- paste("Error menambah data Klimatologi:", e$message)
      output$klimatologi_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal menambah:</b> ", error_msg, "</p>")) })
      showNotification(error_msg, type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con)) { DBI::dbDisconnect(con) }
      removeModal()
    })
  })
  
  observeEvent(input$update_existing_klimatologi_data, {
    if (!is.null(input$klimatologi_edit_id) && input$klimatologi_edit_id == "") {
      showNotification("Peringatan: Tidak ada data yang dipilih untuk diupdate. Pilih baris di tabel dulu.", type = "warning", duration = 5)
      output$klimatologi_form_status <- renderUI({ HTML("<p style='color:orange;'><b>Peringatan:</b> Tidak ada data terpilih untuk diupdate.</p>") })
      return()
    }
    req(input$input_klimatologi_kecamatan,
        input$input_klimatologi_tahun,
        input$input_klimatologi_triwulan,
        input$input_klimatologi_curah_hujan,
        input$input_klimatologi_suhu,
        input$input_klimatologi_kelembaban)
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      
      split_id_original <- strsplit(input$klimatologi_edit_id, "|", fixed = TRUE)[[1]]
      original_id_region <- as.integer(split_id_original[1])
      original_tahun <- as.integer(split_id_original[2])
      original_triwulan <- as.integer(split_id_original[3])
      
      new_id_region <- as.integer(input$input_klimatologi_kecamatan)
      new_tahun <- as.integer(input$input_klimatologi_tahun)
      new_triwulan <- as.integer(input$input_klimatologi_triwulan)
      new_curah_hujan <- as.numeric(input$input_klimatologi_curah_hujan)
      new_suhu <- as.numeric(input$input_klimatologi_suhu)
      new_kelembaban <- as.numeric(input$input_klimatologi_kelembaban)
      
      if (is.na(new_id_region) || is.na(new_tahun) || is.na(new_triwulan) || is.na(new_curah_hujan) || is.na(new_suhu) || is.na(kelembaban_val)) {
        showNotification("Error: Semua input harus diisi dengan nilai yang valid.", type = "error")
        output$klimatologi_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal update:</b> Ada input yang kosong atau tidak valid.</p>")) })
        return()
      }
      
      if (original_id_region != new_id_region || original_tahun != new_tahun || original_triwulan != new_triwulan) {
        query_check_new_key <- sprintf("
          SELECT COUNT(*) FROM tb_klimatologi
          WHERE id_region = %s AND tahun = %s AND triwulan = %s;
        ", new_id_region, new_tahun, new_triwulan)
        new_key_exists <- DBI::dbGetQuery(con, query_check_new_key)$count > 0
        
        if (new_key_exists) {
          showNotification("Error: Kombinasi Kecamatan, Tahun, Triwulan yang baru sudah ada di database. Tidak bisa update.", type = "error", duration = NULL)
          output$klimatologi_form_status <- renderUI({ HTML("<p style='color:red;'><b>Gagal update:</b> Kombinasi kunci baru sudah ada.</p>") })
          return()
        }
      }
      
      query_update <- sprintf("
          UPDATE tb_klimatologi
          SET id_region = %s, tahun = %s, triwulan = %s, suhu = %s, curah_hujan = %s, kelembaban = %s, updated_at = NOW()
          WHERE id_region = %s AND tahun = %s AND triwulan = %s;
        ", new_id_region, new_tahun, new_triwulan, new_suhu, new_curah_hujan, new_kelembaban, # Update kolom sesuai skema DB
                              original_id_region, original_tahun, original_triwulan)
      DBI::dbExecute(con, query_update)
      
      status_msg <- paste0("Data Klimatologi untuk Kecamatan: ", get_kecamatan_name(original_id_region),
                           ", Tahun: ", original_tahun, ", Triwulan: ", original_triwulan, ")",
                           " berhasil **diupdate**!")
      output$klimatologi_form_status <- renderUI({ HTML(paste0("<p style='color:green;'>", status_msg, "</p>")) })
      showNotification(status_msg, type = "message", duration = 5)
      klimatologi_data_rv(fetch_all_klimatologi_data())
    }, error = function(e) {
      error_msg <- paste("Error update data Klimatologi:", e$message)
      output$klimatologi_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal update:</b> ", error_msg, "</p>")) })
      showNotification(error_msg, type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con)) { DBI::dbDisconnect(con) }
      removeModal()
    })
  })
  
  observeEvent(input$clear_klimatologi_form, {
    updateSelectInput(session, "input_klimatologi_kecamatan", selected = "")
    updateNumericInput(session, "input_klimatologi_tahun", value = as.numeric(format(Sys.Date(), "%Y")))
    updateSelectInput(session, "input_klimatologi_triwulan", selected = 1) # Perbaikan: set ke 1
    updateNumericInput(session, "input_klimatologi_curah_hujan", value = 0)
    updateNumericInput(session, "input_klimatologi_suhu", value = 0)
    updateNumericInput(session, "input_klimatologi_kelembaban", value = 0)
    updateTextInput(session, "klimatologi_edit_id", value = "") # Pastikan hidden ID kosong
    output$klimatologi_form_status <- renderUI(NULL)
    showNotification("Formulir Klimatologi telah direset.", type = "message", duration = 3)
    
    shinyjs::enable("add_new_klimatologi_data")
    shinyjs::disable("update_existing_klimatologi_data")
  })
  
  # --- Logika untuk Data Penduduk (Input/Edit/Delete) ---
  
  # reactiveVal untuk menyimpan ID yang akan dihapus sementara untuk Penduduk
  output$demografi_table_title <- renderText({
    # Dapatkan tahun yang dipilih dari input filter
    selected_year_for_title <- selected_filter_year_demografi()
    paste("Manajemen Data Demografi Tahun", selected_year_for_title)
  })
  
  rv_delete_id_penduduk <- reactiveVal(NULL)
  
  output$penduduk_data_table_full <- renderDT({
    data_display <- penduduk_data_rv() # Menggunakan reactiveVal
    
    selected_year_filter_for_message <- selected_filter_year_demografi() # Ambil tahun filter
    
    if (is.null(data_display) || nrow(data_display) == 0) {
      return(datatable(data.frame(Status = paste0("Mohon Maaf Data Penduduk tahun ", selected_year_filter_for_message, " Kosong")),
                       options = list(
                         dom = 't', # Hanya menampilkan tabel tanpa fitur pencarian/paginasi
                         ordering = FALSE, # Matikan sorting
                         paging = FALSE, # Matikan paginasi
                         info = FALSE, # Matikan "Showing X to Y of Z entries"
                         processing = FALSE, # Matikan "Processing..."
                         searching = FALSE, # Matikan search box
                         columnDefs = list(list(className = 'dt-center', targets = '_all')) # Pusatkan pesan
                       ),
                       rownames = FALSE # Hapus nomor baris
      ))
    }
    
    data_display$Aksi <- paste0(
      '<div class="btn-group" role="group" aria-label="Aksi">',
      '<button type="button" class="btn btn-info btn-sm edit_btn" data-id="',
      data_display$ID_Region, '|', data_display$Tahun,
      '">Edit</button>',
      '<button type="button" class="btn btn-danger btn-sm delete_btn" data-id="',
      data_display$ID_Region, '|', data_display$Tahun,
      '">Hapus</button>',
      '</div>'
    )
    
    datatable(data_display, escape = FALSE, selection = 'none', rownames = FALSE, options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 25, 50),
      scrollX = TRUE,
      columnDefs = list(
        list(targets = c("ID_Penduduk", "ID_Region", "Tahun"), visible = FALSE), # Sembunyikan ID_Penduduk dan ID_Region
        list(width = '150px', targets = ncol(data_display) - 1),
        list(className = 'dt-center', targets = '_all')
      )
    ),
    callback = JS(
      "table.on('click', '.edit_btn', function() {",
      "  var id = $(this).data('id');",
      "  Shiny.onInputChange('dt_action_edit_penduduk_id', id, {priority: 'event'});",
      "}),",
      "table.on('click', '.delete_btn', function() {",
      "  var id = $(this).data('id');",
      "  Shiny.onInputChange('dt_action_delete_penduduk_id', id, {priority: 'event'});",
      "});"
    )
    )
  })
  
  # Observer untuk menampilkan Modal Form Penduduk (Tombol "Input Data Baru")
  observeEvent(input$show_add_penduduk_modal, {
    # Reset formulir saat modal dibuka untuk input data baru
    updateSelectInput(session, "input_penduduk_kecamatan", selected = "")
    updateNumericInput(session, "input_penduduk_tahun", value = as.numeric(format(Sys.Date(), "%Y")))
    updateNumericInput(session, "input_penduduk_jumlah", value = 0)
    updateTextInput(session, "penduduk_edit_id", value = "")
    output$penduduk_form_status <- renderUI(NULL)
    
    shinyjs::enable("add_new_penduduk_data")
    shinyjs::disable("update_existing_penduduk_data")
    
    showModal(modalDialog(
      title = "Input Data Penduduk",
      size = "l",
      fluidRow(
        column(12,
               tags$div(style = "display: none;", textInput("penduduk_edit_id", label = NULL, value = "")),
               selectInput("input_penduduk_kecamatan", "Kecamatan:", choices = c("Pilih Kecamatan" = "")),
               numericInput("input_penduduk_tahun", "Tahun:", value = as.numeric(format(Sys.Date(), "%Y")), min = 2000, max = 2100),
               numericInput("input_penduduk_jumlah", "Kepadatan Penduduk:", value = 0, min = 0),
               hr(),
               actionButton("add_new_penduduk_data", "Tambah Data Baru", icon = icon("plus"), class = "btn-success"),
               actionButton("clear_penduduk_form", "Reset Formulir", icon = icon("sync"), class = "btn-default"),
               br(),br(),
               htmlOutput("penduduk_form_status")
        )
      ),
      footer = tagList(
        modalButton("Tutup")
      )
    ))
    current_choices <- all_kecamatan_data()
    if(!is.null(current_choices) && nrow(current_choices) > 0) {
      kec_choices <- c("Pilih Kecamatan" = "", setNames(current_choices$id_region, current_choices$kecamatan))
      updateSelectInput(session, "input_penduduk_kecamatan", choices = kec_choices, selected = "")
    }
    # Tambahkan kode JavaScript ini di dalam modal
    shinyjs::runjs(
      "
    $(document).ready(function() {
      $('#input_penduduk_jumlah').on('focus', function() {
        $(this).select();
      });
    });
    "
    )
  })
  
  observeEvent(input$dt_action_edit_penduduk_id, {
    req(input$dt_action_edit_penduduk_id)
    split_id <- strsplit(input$dt_action_edit_penduduk_id, "|", fixed = TRUE)[[1]]
    id_region_to_edit <- as.integer(split_id[1])
    tahun_to_edit <- as.integer(split_id[2])
    
    selected_data <- penduduk_data_rv() %>%
      filter(ID_Region == id_region_to_edit, Tahun == tahun_to_edit)
    req(nrow(selected_data) > 0)
    
    # Atur status tombol dengan shinyjs
    shinyjs::disable("add_new_penduduk_data")
    shinyjs::enable("update_existing_penduduk_data")
    
    # Menampilkan modal dengan form yang terisi
    showModal(modalDialog(
      title = "Edit Data Penduduk",
      size = "l",
      fluidRow(
        column(12,
               tags$div(style = "display: none;", textInput("penduduk_edit_id", label = NULL, value = input$dt_action_edit_penduduk_id)),
               selectInput("input_penduduk_kecamatan", "Kecamatan:", choices = c("Memuat..." = ""), selected = selected_data$ID_Region),
               numericInput("input_penduduk_tahun", "Tahun:", value = selected_data$Tahun, min = 2000, max = 2100),
               numericInput("input_penduduk_jumlah", "Kepadatan Penduduk:", value = selected_data$`Kepadatan Penduduk`, min = 0), # Gunakan Jumlah_Penduduk dari selected_data
               hr(),
               actionButton("update_existing_penduduk_data", "Update Data Terpilih", icon = icon("edit"), class = "btn-primary"),
               br(),br(),
               htmlOutput("penduduk_form_status")
        )
      ),
      footer = tagList(
        modalButton("Tutup")
      )
    ))
    # Update choices for kecamatan dropdown in the modal
    current_choices <- all_kecamatan_data()
    if(!is.null(current_choices) && nrow(current_choices) > 0) {
      kec_choices <- c("Pilih Kecamatan" = "", setNames(current_choices$id_region, current_choices$kecamatan))
      updateSelectInput(session, "input_penduduk_kecamatan", choices = kec_choices, selected = selected_data$ID_Region)
    }
    shinyjs::disable("input_penduduk_kecamatan")
    shinyjs::disable("input_penduduk_tahun")
  })
  
  observeEvent(input$dt_action_delete_penduduk_id, {
    req(input$dt_action_delete_penduduk_id)
    rv_delete_id_penduduk(input$dt_action_delete_penduduk_id)
    
    split_id <- strsplit(rv_delete_id_penduduk(), "|", fixed = TRUE)[[1]]
    id_region_display <- as.integer(split_id[1])
    tahun_display <- as.integer(split_id[2])
    
    showModal(modalDialog(
      title = "Konfirmasi Penghapusan",
      paste0("Anda yakin ingin menghapus data Penduduk untuk Kecamatan: ", get_kecamatan_name(id_region_display),
             ", Tahun: ", tahun_display, "?"),
      footer = tagList(
        modalButton("Batal"),
        actionButton("confirm_delete_penduduk", "Hapus", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_penduduk, {
    req(rv_delete_id_penduduk())
    
    split_id <- strsplit(rv_delete_id_penduduk(), "|", fixed = TRUE)[[1]]
    id_region_to_delete <- as.integer(split_id[1])
    tahun_to_delete <- as.integer(split_id[2])
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      query_delete <- sprintf("
          DELETE FROM tb_penduduk
          WHERE id_region = %s AND tahun = %s;
        ", id_region_to_delete, tahun_to_delete)
      DBI::dbExecute(con, query_delete)
      
      status_msg <- paste0("Data Penduduk untuk Kecamatan: ", get_kecamatan_name(id_region_to_delete),
                           ", Tahun: ", tahun_to_delete,
                           " berhasil **dihapus**.")
      output$penduduk_form_status <- renderUI({ HTML(paste0("<p style='color:green;'>", status_msg, "</p>")) })
      showNotification(status_msg, type = "message", duration = 5)
      penduduk_data_rv(fetch_all_penduduk_data())
    }, error = function(e) {
      error_msg <- paste("Error menghapus data Penduduk:", e$message)
      output$penduduk_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal menghapus:</b> ", error_msg, "</p>")) })
      showNotification(error_msg, type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con)) { DBI::dbDisconnect(con) }
      removeModal()
    })
  })
  
  observeEvent(input$add_new_penduduk_data, {
    req(input$input_penduduk_kecamatan,
        input$input_penduduk_tahun,
        input$input_penduduk_jumlah)
    if (!is.null(input$penduduk_edit_id) && input$penduduk_edit_id != "") {
      showNotification("Error: Formulir sedang dalam mode edit. Silakan 'Reset Formulir' untuk menambah data baru.", type = "error", duration = 5)
      output$penduduk_form_status <- renderUI({ HTML("<p style='color:red;'><b>Gagal menambah:</b> Formulir tidak kosong. Reset dulu.</p>") })
      return()
    }
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      
      id_region_val <- as.integer(input$input_penduduk_kecamatan)
      tahun_val <- as.integer(input$input_penduduk_tahun)
      jumlah_penduduk_val <- as.integer(input$input_penduduk_jumlah)
      
      if (is.na(id_region_val) || is.na(tahun_val) || is.na(jumlah_penduduk_val)) {
        showNotification("Error: Semua input harus diisi dengan nilai yang valid.", type = "error")
        output$penduduk_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal menambah:</b> Ada input yang kosong atau tidak valid.</p>")) })
        return()
      }
      
      query_check <- sprintf("
          SELECT COUNT(*) FROM tb_penduduk
          WHERE id_region = %s AND tahun = %s;
        ", id_region_val, tahun_val)
      exists <- DBI::dbGetQuery(con, query_check)$count > 0
      
      if (exists) {
        showNotification("Peringatan: Data untuk kombinasi Kecamatan, Tahun ini sudah ada. Gunakan 'Update Data Terpilih' jika ingin mengubahnya.", type = "warning", duration = 8)
        output$penduduk_form_status <- renderUI({ HTML("<p style='color:orange;'><b>Peringatan:</b> Data sudah ada. Gunakan Update.</p>") })
        return()
      }
      
      query_insert <- sprintf("
          INSERT INTO tb_penduduk (id_region, tahun, penduduk)
          VALUES (%s, %s, %s);
        ", id_region_val, tahun_val, jumlah_penduduk_val) # Insert ke kolom 'penduduk'
      DBI::dbExecute(con, query_insert)
      
      status_msg <- paste0("Data Penduduk baru untuk Kecamatan: ", get_kecamatan_name(id_region_val),
                           ", Tahun: ", tahun_val,
                           " berhasil **ditambahkan**!")
      output$penduduk_form_status <- renderUI({ HTML(paste0("<p style='color:green;'>", status_msg, "</p>")) })
      showNotification(status_msg, type = "message", duration = 5)
      penduduk_data_rv(fetch_all_penduduk_data())
    }, error = function(e) {
      error_msg <- paste("Error menambah data Penduduk:", e$message)
      output$penduduk_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal menambah:</b> ", error_msg, "</p>")) })
      showNotification(error_msg, type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con)) { DBI::dbDisconnect(con) }
      removeModal()
    })
  })
  
  observeEvent(input$update_existing_penduduk_data, {
    if (!is.null(input$penduduk_edit_id) && input$penduduk_edit_id == "") {
      showNotification("Peringatan: Tidak ada data yang dipilih untuk diupdate. Pilih baris di tabel dulu.", type = "warning", duration = 5)
      output$penduduk_form_status <- renderUI({ HTML("<p style='color:orange;'><b>Peringatan:</b> Tidak ada data terpilih untuk diupdate.</p>") })
      return()
    }
    req(input$input_penduduk_kecamatan,
        input$input_penduduk_tahun,
        input$input_penduduk_jumlah)
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      
      split_id_original <- strsplit(input$penduduk_edit_id, "|", fixed = TRUE)[[1]]
      original_id_region <- as.integer(split_id_original[1])
      original_tahun <- as.integer(split_id_original[2])
      
      new_id_region <- as.integer(input$input_penduduk_kecamatan)
      new_tahun <- as.integer(input$input_penduduk_tahun)
      new_jumlah_penduduk <- as.integer(input$input_penduduk_jumlah)
      
      if (is.na(new_id_region) || is.na(new_tahun) || is.na(new_jumlah_penduduk)) {
        showNotification("Error: Semua input harus diisi dengan nilai yang valid.", type = "error")
        output$penduduk_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal update:</b> Ada input yang kosong atau tidak valid.</p>")) })
        return()
      }
      
      if (original_id_region != new_id_region || original_tahun != new_tahun) {
        query_check_new_key <- sprintf("
          SELECT COUNT(*) FROM tb_penduduk
          WHERE id_region = %s AND tahun = %s;
        ", new_id_region, new_tahun)
        new_key_exists <- DBI::dbGetQuery(con, query_check_new_key)$count > 0
        
        if (new_key_exists) {
          showNotification("Error: Kombinasi Kecamatan, Tahun yang baru sudah ada di database. Tidak bisa update.", type = "error", duration = NULL)
          output$penduduk_form_status <- renderUI({ HTML("<p style='color:red;'><b>Gagal update:</b> Kombinasi kunci baru sudah ada.</p>") })
          return()
        }
      }
      
      query_update <- sprintf("
          UPDATE tb_penduduk
          SET id_region = %s, tahun = %s, penduduk = %s, updated_at = NOW()
          WHERE id_region = %s AND tahun = %s;
        ", new_id_region, new_tahun, new_jumlah_penduduk, # Update kolom 'penduduk'
                              original_id_region, original_tahun)
      DBI::dbExecute(con, query_update)
      
      status_msg <- paste0("Data Penduduk untuk Kecamatan: ", get_kecamatan_name(original_id_region),
                           ", Tahun: ", original_tahun,
                           " berhasil **diupdate**!")
      output$penduduk_form_status <- renderUI({ HTML(paste0("<p style='color:green;'>", status_msg, "</p>")) })
      showNotification(status_msg, type = "message", duration = 5)
      penduduk_data_rv(fetch_all_penduduk_data())
    }, error = function(e) {
      error_msg <- paste("Error update data Penduduk:", e$message)
      output$penduduk_form_status <- renderUI({ HTML(paste0("<p style='color:red;'><b>Gagal update:</b> ", error_msg, "</p>")) })
      showNotification(error_msg, type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con)) { DBI::dbDisconnect(con) }
      removeModal()
    })
  })
  
  observeEvent(input$clear_penduduk_form, {
    updateSelectInput(session, "input_penduduk_kecamatan", selected = "")
    updateNumericInput(session, "input_penduduk_tahun", value = as.numeric(format(Sys.Date(), "%Y")))
    # updateSelectInput(session, "input_penduduk_triwulan", selected = "") # Ini tidak ada di form penduduk. Komentar ini menunjukkan bahwa baris ini harus dihapus atau diabaikan.
    updateNumericInput(session, "input_penduduk_jumlah", value = 0)
    updateTextInput(session, "penduduk_edit_id", value = "") # Pastikan hidden ID kosong
    output$penduduk_form_status <- renderUI(NULL)
    showNotification("Formulir Penduduk telah direset.", type = "message", duration = 3)
    
    shinyjs::enable("add_new_penduduk_data")
    shinyjs::disable("update_existing_penduduk_data")
  })
  
  # Logika untuk Plot Tren Tahunan per Triwulan
  fetch_annual_dbd_trend_data <- reactive({
    req(input$select_trend_year_annual) # Pastikan input tahun ada
    target_year <- as.integer(format(input$select_trend_year_annual, "%Y")) # Ambil tahun dari input
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      query <- sprintf("
        SELECT triwulan, SUM(kasus) AS total_kasus
        FROM tb_kasus
        WHERE tahun = %s
        GROUP BY triwulan
        ORDER BY triwulan;
      ", target_year)
      data <- DBI::dbGetQuery(con, query)
      full_triwulan <- data.frame(triwulan = 1:4)
      data <- left_join(full_triwulan, data, by = "triwulan") %>%
        replace_na(list(total_kasus = 0)) %>%
        arrange(triwulan)
      return(data)
    }, error = function(e) {
      showNotification(paste("Error mengambil data tren DBD tahunan:", e$message), type = "error", duration = NULL)
      return(NULL)
    }, finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
    })
  })
  
  output$plot_trend_annual_dbd <- renderPlot({
    plot_data <- fetch_annual_dbd_trend_data()
    req(plot_data)
    
    selected_year_for_title <- format(input$select_trend_year_annual, "%Y")
    
    if (nrow(plot_data) == 0 || all(plot_data$total_kasus == 0)) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0("Tidak ada data kasus DBD untuk tahun ", selected_year_for_title, "."),
                 size = 6, color = "red") +
        theme_void()
      return(p)
    } else {
      p <- ggplot(plot_data, aes(x = factor(triwulan), y = total_kasus, group = 1)) +
        geom_line(aes(color = "Jumlah Kasus"), size = 1) + # === UBAH: Petakan warna garis untuk legenda ===
        geom_point(color = "red", size = 2) + # === UBAH: Set warna titik secara statis (di luar aes) ===
        geom_text(aes(label = total_kasus), vjust = -0.5, hjust = 0.5, color = "black", size = 4) +
        labs(
          title = paste("Jumlah Kasus DBD per Triwulan Tahun", selected_year_for_title),
          x = "Triwulan",
          y = "Jumlah Kasus DBD"
        ) +
        theme_minimal() +
        scale_color_manual(name = NULL,
                           values = c("Jumlah Kasus" = "blue")) + # === UBAH: Hanya definisikan warna untuk 'Jumlah Kasus' (garis) ===
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12),
              axis.text.y = element_text(size = 11),
              axis.title.x = element_text(size = 14, face = "bold"),
              axis.title.y = element_text(size = 14, face = "bold"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = 12)) +
        scale_x_discrete(labels = paste("Triwulan", 1:4))
      
      return(p)
    }
  })
  
  
  # Logika untuk Plot Tren Antar Kecamatan
  fetch_all_kecamatan_dbd_data <- reactive({
    req(input$select_trend_year_kecamatan) # Pastikan tahun dipilih
    target_year <- as.integer(format(input$select_trend_year_kecamatan, "%Y")) # Ambil tahun dari input
    triwulan_filter <- input$select_trend_triwulan # Ambil triwulan dari input
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      
      query <- sprintf("
        SELECT tk.id_region, tj.kecamatan, tk.triwulan, tk.kasus
        FROM tb_kasus tk
        JOIN tb_jember tj ON tk.id_region = tj.id_region
        WHERE tk.tahun = %s
      ", target_year)
      
      if (triwulan_filter != "all") {
        query <- paste(query, "AND tk.triwulan =", as.integer(triwulan_filter))
      }
      
      query <- paste(query, "ORDER BY tj.kecamatan, tk.triwulan;")
      data <- DBI::dbGetQuery(con, query)
      
      if (triwulan_filter == "all") {
        data <- data %>%
          group_by(id_region, kecamatan) %>%
          summarise(total_kasus = sum(kasus, na.rm = TRUE), .groups = 'drop') %>% # Tambah .groups = 'drop' untuk dplyr terbaru
          ungroup() %>%
          arrange(kecamatan)
      } else {
        # Untuk triwulan spesifik, pastikan semua kecamatan ditampilkan (termasuk yang 0 kasus)
        all_kec_map <- all_kecamatan_data() %>% select(id_region, kecamatan)
        data <- left_join(all_kec_map, data, by = c("id_region", "kecamatan")) %>%
          # Jika tidak ada data kasus untuk kecamatan dan triwulan/tahun tertentu, set kasus ke 0
          mutate(kasus = replace_na(kasus, 0)) %>%
          # total_kasus akan sama dengan kasus untuk triwulan spesifik
          mutate(total_kasus = kasus) %>%
          select(id_region, kecamatan, total_kasus) %>%
          arrange(kecamatan)
      }
      
      return(data)
    }, error = function(e) {
      showNotification(paste("Error mengambil data tren DBD antar kecamatan:", e$message), type = "error", duration = NULL)
      return(NULL)
    }, finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
    })
  })
  
  output$plot_trend_kecamatan_dbd <- renderPlot({
    plot_data <- fetch_all_kecamatan_dbd_data()
    req(plot_data)
    
    # Ambil tahun dan triwulan yang dipilih untuk judul
    current_year_for_plot <- format(input$select_trend_year_kecamatan, "%Y")
    triwulan_text <- if(input$select_trend_triwulan == "all") "" else paste0(" Triwulan ", input$select_trend_triwulan)
    
    if (is.null(plot_data) || nrow(plot_data) == 0 || all(plot_data$total_kasus == 0)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0("Tidak ada data kasus DBD untuk semua kecamatan pada tahun ", current_year_for_plot, triwulan_text, "."), # Judul dinamis
                 size = 6, color = "red") +
        theme_void()
    } else {
      plot_data$kecamatan <- factor(plot_data$kecamatan, levels = unique(plot_data$kecamatan[order(plot_data$kecamatan)]))
      
      ggplot(plot_data, aes(x = kecamatan, y = total_kasus, fill = kecamatan)) +
        geom_bar(stat = "identity", color = "black") +
        geom_text(aes(label = total_kasus), vjust = -0.5, color = "black", size = 5) +
        labs(
          title = paste("Jumlah Kasus DBD per Kecamatan Tahun", current_year_for_plot, triwulan_text), # Judul dinamis
          x = "Kecamatan",
          y = "Jumlah Kasus DBD"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
              axis.text.y = element_text(size = 11),
              axis.title.x = element_text(size = 12, face = "bold"),
              axis.title.y = element_text(size = 12, face = "bold"),
              legend.position = "none",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank())
    }
  })
  
}

# ------------------------------------------------------------------
# 4. JALANKAN APLIKASI SHINY
# ------------------------------------------------------------------
shinyApp(ui, server)