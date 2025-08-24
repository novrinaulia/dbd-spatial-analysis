# app.R
# Tambahkan baris ini di awal untuk debugging traceback penuh
#options(error = recover)
#options(shiny.fullstacktrace = TRUE)
# ------------------------------------------------------------------
# 1. MUAT SEMUA FILE YANG DIPISAHKAN
# ------------------------------------------------------------------
source("utils.R", local = TRUE)   # Memuat library  
source("conn.R", local = TRUE)    # Memuat konfigurasi database dan fungsi koneksi
source("model.R", local = TRUE)   # Memuat logika pengambilan data (reactive values)
source("ui.R", local = TRUE)      # Memuat seluruh definisi UI
source("output.R", local = TRUE)  # Memuat semua output visualisasi dan judul dinamis
source("footer.R", local = TRUE)  # Memuat semua output informasi teks

# ------------------------------------------------------------------
# 2. DEFINISI USER INTERFACE (UI)
#    UI didefinisikan sepenuhnya di ui.R
# ------------------------------------------------------------------
# ui_dashboard_page sudah dimuat dari ui.R

# ------------------------------------------------------------------
# 3. DEFINISI SERVER LOGIC
#    Server logic akan memanggil semua bagian output dari file output.R dan footer.R
# ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Panggil semua logika server yang didefinisikan di output.R
  # Fungsi-fungsi ini akan menggunakan reactive values yang didefinisikan di model.R
  # dan input dari UI.R
  call_all_outputs(input, output, session,
                   data_kecamatan_jember, all_kecamatan_names,
                   data_tren_dbd_kab, data_tren_dbd_kec_single_year,
                   get_data_kasus_moran_func,
                   get_regression_data_func)
  
  # Panggil semua logika server untuk footer/info teks yang didefinisikan di footer.R
  call_all_info_text_outputs(input, output, session)
  
}

# ------------------------------------------------------------------
# 4. JALANKAN APLIKASI SHINY
# ------------------------------------------------------------------
shinyApp(ui = ui_dashboard_page, server)