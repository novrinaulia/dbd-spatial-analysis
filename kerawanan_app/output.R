# output.R

# ------------------------------------------------------------------
# LOGIKA OUTPUT UNTUK VISUALISASI DAN JUDUL DINAMIS
# ------------------------------------------------------------------

# Fungsi bantu untuk membungkus teks (wrap text) dengan tag HTML <br>
wrap_text_html <- function(text, line_length = 55) { # Menyesuaikan panjang baris
  words <- unlist(strsplit(text, " "))
  wrapped_text <- ""
  current_line_length <- 0
  for (word in words) {
    # Tambahkan 1 untuk spasi setelah kata
    if (current_line_length + nchar(word) + 1 > line_length) {
      wrapped_text <- paste0(wrapped_text, "<br>", word)
      current_line_length <- nchar(word)
    } else {
      if (wrapped_text == "") {
        wrapped_text <- word
      } else {
        wrapped_text <- paste0(wrapped_text, " ", word)
      }
      current_line_length <- current_line_length + nchar(word) + 1
    }
  }
  return(wrapped_text)
}

# Fungsi bantu untuk membungkus teks untuk output konsol/teks biasa
wrap_text_plain <- function(text, width = 80) { # Lebar standar konsol
  return(paste(strwrap(text, width = width), collapse = "\n"))
}

# Fungsi bantu untuk membuat plot kosong dengan pesan di tengah
create_blank_message_plot <- function(message_text_html) {
  plotly_empty(type = "scatter", mode = "lines") %>%
    layout(
      title = list(
        text = message_text_html,
        font = list(size = 18),
        xref = "paper", yref = "paper", x = 0.5, y = 0.5, xanchor = "center", yanchor = "middle", align = "center"
      ),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    ) %>%
    config(displayModeBar = FALSE)
}

# Fungsi bantu untuk ekstraksi nilai numerik yang aman (Pastikan fungsi ini ada di paling atas file output.R)
get_safe_numeric <- function(obj, path, default_val = NA) {
  val <- tryCatch({
    eval(parse(text = paste0("obj$", path)))
  }, error = function(e) {
    NULL
  })
  if (is.null(val) || !is.numeric(val) || length(val) == 0 || !is.finite(val)) {
    return(default_val)
  }
  return(val)
}

# Fungsi ini akan dipanggil di server utama app.R
call_all_outputs <- function(input, output, session,
                             data_kecamatan_jember_reactive, all_kecamatan_names_reactive,
                             get_data_tren_dbd_kab_func_wrapper,
                             get_data_tren_dbd_kec_func_wrapper,
                             get_data_kasus_moran_func_wrapper,
                             get_regression_data_func_wrapper) {
  
  # NEW: ReactiveVal untuk menyimpan data peta kerawanan yang sudah dihitung
  lisa_data_for_info_rv <- reactiveVal(NULL)
  # NEW: ReactiveVal untuk menyimpan data LISA Bivariat yang sudah dihitung
  lisa_bivar_data_for_info_rv <- reactiveVal(NULL)
  # NEW: ReactiveVal untuk menyimpan hasil regresi
  regression_results_rv <- reactiveVal(NULL)
  
  # Output Peta Kerawanan DBD Jember
  output$dbd_map <- renderLeaflet({
    # Ambil input tahun dan triwulan dari UI yang sudah dimodifikasi
    selected_year_lisa <- if (!is.null(input$lisa_map_year)) as.numeric(format(input$lisa_map_year, "%Y")) else NULL
    selected_triwulan_lisa <- input$lisa_map_triwulan
    
    # Inisialisasi peta dasar
    peta_dasar <- leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng = 113.6, lat = -8.17, zoom = 10)
    
    # Tampilkan pesan jika input tahun atau triwulan belum lengkap
    if (is.null(selected_year_lisa) || is.null(selected_triwulan_lisa) || selected_triwulan_lisa == "") {
      return(peta_dasar %>%
               addPopups(113.6, -8.17, "Silakan pilih Tahun dan Triwulan untuk menampilkan Peta Kerawanan DBD."))
    }
    
    # Panggil fungsi untuk mendapatkan data Moran, yang juga akan digunakan untuk LISA
    get_moran_data_func <- get_data_kasus_moran_func_wrapper()
    moran_data_list <- get_moran_data_func(selected_year_lisa, selected_triwulan_lisa)
    
    # Penanganan status error dari model.R
    if (!is.null(moran_data_list) && !is.null(moran_data_list$status)) {
      status_message <- switch(moran_data_list$status,
                               "input_not_selected" = "Silakan pilih Tahun dan Triwulan.",
                               "no_case_data_for_moran" = paste0("Tidak ada data kasus DBD yang tercatat untuk Tahun ", moran_data_list$year, " Triwulan ", moran_data_list$triwulan),
                               "low_variability" = paste0("Variabilitas data kasus DBD sangat rendah atau nol untuk Tahun ", moran_data_list$year, " Triwulan ", moran_data_list$triwulan, ". Peta Kerawanan DBD Gagal Dibuat karena semua nilai kasus sama."),
                               "no_geom_data" = "Data geometri kecamatan tidak tersedia dari database (tb_jember).",
                               "no_valid_spatial_data" = "Tidak ada data spasial yang valid setelah penggabungan.",
                               "not_enough_locations" = "Jumlah kecamatan terlalu sedikit (< 2) untuk analisis LISA.",
                               "not_enough_centroids" = "Tidak cukup centroid valid untuk matriks bobot (< 2 titik).",
                               "invalid_geometry_type" = "Tipe geometri tidak valid setelah centroid.",
                               "undefined_crs" = "Sistem Koordinat Referensi (CRS) tidak terdefinisi.",
                               "reprojection_failed" = "Gagal reproyeksi koordinat centroid.",
                               "empty_projected_coords" = "Koordinat terproyeksi kosong.",
                               "sf_to_sp_conversion_failed" = "Gagal konversi objek sf ke sp.",
                               "sp_object_null" = "Objek Spatial (sp) null.",
                               "sp_coords_na" = "Koordinat Spatial (sp) mengandung NA.",
                               "invalid_k_val" = "Nilai K (jumlah tetangga) tidak valid.",
                               "manual_nb_failed" = "Gagal membuat objek tetangga KNN secara manual.",
                               "manual_nb_failed_moran" = paste0("Gagal membuat objek tetangga KNN (dalam Moran): ", moran_data_list$message),
                               "nb_invalid_after_construction" = "Objek tetangga (nb) tidak valid setelah konstruksi.",
                               "nb_empty" = "Objek tetangga (nb) kosong.",
                               "no_neighbors_found" = "Tidak ada tetangga yang ditemukan untuk titik data.",
                               "listw_creation_failed" = "Gagal membuat matriks bobot spasial (listw).",
                               "listw_invalid" = "Matriks bobot spasial (listw) tidak valid.",
                               "invalid_weights" = "Beberapa observasi memiliki bobot NA atau tidak ada tetangga dalam matriks bobot spasial.",
                               "general_error" = paste0("Terjadi kesalahan umum: ", moran_data_list$message),
                               "Data tidak tersedia atau terjadi kesalahan saat mengambil data."
      )
      return(peta_dasar %>%
               addPopups(113.6, -8.17, paste0("Peta Kerawanan DBD Gagal Dibuat:<br/>", wrap_text_html(status_message, line_length = 40))))
    }
    
    # Pastikan data dan matriks bobot tersedia setelah penanganan error
    req(moran_data_list$data, moran_data_list$lw)
    data_for_lisa <- moran_data_list$data
    lw <- moran_data_list$lw
    
    # Pastikan data geometri tidak kosong
    if (is.null(data_for_lisa) || nrow(data_for_lisa) == 0) {
      return(peta_dasar %>%
               addPopups(113.6, -8.17, "Data geometri kecamatan tidak tersedia atau gagal dimuat.<br/>Pastikan koneksi DB dan query tabel 'tb_jember' sudah benar."))
    }
    
    # Lakukan perhitungan LISA
    local_moran <- spdep::localmoran(data_for_lisa$log_kasus, lw, zero.policy = TRUE)
    
    # Tambahkan hasil LISA ke data spasial
    data_for_lisa$Ii <- local_moran[, 1]
    data_for_lisa$Pvalue <- local_moran[, 5]
    
    # Identifikasi kuadran LISA
    # Standarisasi variabel
    z_log_kasus <- scale(data_for_lisa$log_kasus)
    spatial_lag_log_kasus <- lag.listw(lw, z_log_kasus, zero.policy = TRUE)
    
    data_for_lisa <- data_for_lisa %>%
      mutate(
        kuadran = case_when(
          Pvalue < 0.05 & z_log_kasus > 0 & spatial_lag_log_kasus > 0 ~ "HH (Tinggi-Tinggi)",
          Pvalue < 0.05 & z_log_kasus < 0 & spatial_lag_log_kasus < 0 ~ "LL (Rendah-Rendah)",
          Pvalue < 0.05 & z_log_kasus > 0 & spatial_lag_log_kasus < 0 ~ "HL (Tinggi-Rendah)",
          Pvalue < 0.05 & z_log_kasus < 0 & spatial_lag_log_kasus > 0 ~ "LH (Rendah-Tinggi)",
          TRUE ~ "Tidak Signifikan"
        )
      )
    # NEW: Update reactiveVal dengan data yang sudah dihitung dan diklasifikasikan
    lisa_data_for_info_rv(data_for_lisa)
    
    # Definisikan palet warna untuk kuadran LISA
    pal_lisa <- colorFactor(
      palette = c("red", "blue", "orange", "green", "lightgray"),
      domain = c("HH (Tinggi-Tinggi)", "LL (Rendah-Rendah)", "HL (Tinggi-Rendah)", "LH (Rendah-Tinggi)", "Tidak Signifikan")
    )
    
    # Buat label pop-up untuk peta LISA
    labels_lisa <- paste0(
      "<strong>Kecamatan: </strong>", data_for_lisa$kecamatan, "<br/>",
      "<strong>Total Kasus: </strong>", data_for_lisa$total_kasus, "<br/>",
      "<strong>LISA Ii: </strong>", round(data_for_lisa$Ii, 4), "<br/>",
      "<strong>P-value: </strong>", format.pval(data_for_lisa$Pvalue, digits = 3), "<br/>",
      "<strong>Kuadran: </strong>", data_for_lisa$kuadran
    ) %>% lapply(htmltools::HTML)
    
    # Tambahkan poligon dengan pewarnaan LISA
    peta_lisa <- peta_dasar %>%
      addPolygons(
        data = data_for_lisa,
        fillColor = ~pal_lisa(kuadran),
        weight = 1,
        opacity = 1,
        color = "darkblue",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE),
        label = labels_lisa,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addLegend(
        pal = pal_lisa,
        values = c("HH (Tinggi-Tinggi)", "LL (Rendah-Rendah)", "HL (Tinggi-Rendah)", "LH (Rendah-Tinggi)", "Tidak Signifikan"),
        opacity = 0.7,
        title = "Klaster Kasus DBD",
        position = "bottomright"
      )
    
    return(peta_lisa)
  })
  
  # Judul untuk peta dashboard (sekarang selalu LISA)
  output$dbd_map_title <- renderText({
    selected_year_lisa <- if (!is.null(input$lisa_map_year)) format(input$lisa_map_year, "%Y") else ""
    selected_triwulan_lisa <- input$lisa_map_triwulan
    
    if (selected_year_lisa == "" || selected_triwulan_lisa == "") {
      return("Peta Kerawanan Kasus DBD (Pilih Tahun & Triwulan)")
    } else {
      return(paste0("Peta Kerawanan Kasus DBD Jember Tahun ", selected_year_lisa, " Triwulan ", selected_triwulan_lisa))
    }
  })
  
  # Output untuk Informasi Peta Kerawanan DBD secara Dinamis
  output$infoPetaKerawanan <- renderUI({
    current_map_data <- lisa_data_for_info_rv()
    selected_year <- if (!is.null(input$lisa_map_year)) format(input$lisa_map_year, "%Y") else "..."
    selected_triwulan <- if (!is.null(input$lisa_map_triwulan) && input$lisa_map_triwulan != "") input$lisa_map_triwulan else "..."
    
    if (is.null(current_map_data) || nrow(current_map_data) == 0) {
      if (selected_year == "..." || selected_triwulan == "...") {
        return(HTML("<p>Pilih <b>Tahun</b> dan <b>Triwulan</b> di atas untuk menampilkan informasi detail dan peta kerawanan DBD.</p>"))
      } else {
        return(HTML(paste0("<p>Tidak ada data kasus DBD yang tersedia atau peta tidak dapat dibuat untuk Tahun <b>", selected_year, "</b> Triwulan <b>", selected_triwulan, "</b></p>")))
      }
    }
    
    # Hitung jumlah dan kumpulkan nama kecamatan per kuadran
    kuadran_info <- current_map_data %>%
      group_by(kuadran) %>%
      summarise(
        count = n(),
        kecamatan_list = paste0(sort(kecamatan), collapse = ", ")
      ) %>%
      ungroup() %>%
      arrange(match(kuadran, c("HH (Tinggi-Tinggi)", "LL (Rendah-Rendah)", "HL (Tinggi-Rendah)", "LH (Rendah-Tinggi)", "Tidak Signifikan")))
    
    # Fungsi bantu untuk mendapatkan info kuadran
    get_kuadran_detail <- function(kuadran_name) {
      info_row <- kuadran_info %>% filter(kuadran == kuadran_name)
      if (nrow(info_row) > 0) {
        return(list(count = info_row$count, list = info_row$kecamatan_list))
      } else {
        return(list(count = 0, list = "Tidak ada kecamatan."))
      }
    }
    
    hh_detail <- get_kuadran_detail("HH (Tinggi-Tinggi)")
    ll_detail <- get_kuadran_detail("LL (Rendah-Rendah)")
    hl_detail <- get_kuadran_detail("HL (Tinggi-Rendah)")
    lh_detail <- get_kuadran_detail("LH (Rendah-Tinggi)")
    tidak_signifikan_detail <- get_kuadran_detail("Tidak Signifikan")
    
    total_kecamatan <- nrow(current_map_data)
    
    info_html <- paste0(
      "<p>Peta di atas menampilkan klaster kerawanan DBD di Kabupaten Jember untuk <b>Tahun ", selected_year, "</b> dan <b>Triwulan ", selected_triwulan, "</b>. Klaster ini mengidentifikasi pola pengelompokan kasus DBD yang signifikan secara statistik di tingkat kecamatan.</p>",
      "<ul>",
      "<li><b>Area Merah (HH - Tinggi-Tinggi)</b>: Terdapat <b>", hh_detail$count, "</b> kecamatan dalam klaster ini. ",
      "Kecamatan: <i>", hh_detail$list, "</i>. ",
      "Ini adalah 'hotspot' DBD, di mana kecamatan dengan jumlah kasus DBD tinggi dikelilingi oleh kecamatan lain dengan jumlah kasus DBD tinggi. Area ini menunjukkan tingkat kerawanan sangat tinggi dan perlu perhatian prioritas.</li>",
      "<li><b>Area Biru (LL - Rendah-Rendah)</b>: Terdapat <b>", ll_detail$count, "</b> kecamatan dalam klaster ini. ",
      "Kecamatan: <i>", ll_detail$list, "</i>. ",
      "Ini adalah 'coldspot' DBD, di mana kecamatan dengan jumlah kasus DBD rendah dikelilingi oleh kecamatan lain dengan jumlah kasus DBD rendah. Area ini relatif aman atau program pencegahan berjalan efektif.</li>",
      "<li><b>Area Oranye (HL - Tinggi-Rendah)</b>: Terdapat <b>", hl_detail$count, "</b> kecamatan dalam klaster ini. ",
      "Kecamatan: <i>", hl_detail$list, "</i>. ",
      "Ini adalah 'outlier', di mana kecamatan dengan jumlah kasus DBD tinggi dikelilingi oleh kecamatan dengan jumlah kasus DBD rendah. Perlu investigasi khusus mengapa kecamatan ini menonjol di lingkungan yang lebih aman.</li>",
      "<li><b>Area Hijau (LH - Rendah-Tinggi)</b>: Terdapat <b>", lh_detail$count, "</b> kecamatan dalam klaster ini. ",
      "Kecamatan: <i>", lh_detail$list, "</i>. ",
      "Ini juga 'outlier', di mana kecamatan dengan jumlah kasus DBD rendah dikelilingi oleh kecamatan dengan jumlah kasus DBD tinggi. Kecamatan ini mungkin memiliki faktor pelindung atau metode pencegahan yang unik.</li>",
      "<li><b>Area Abu-abu Muda (Tidak Signifikan)</b>: Terdapat <b>", tidak_signifikan_detail$count, "</b> kecamatan dalam kategori ini. ",
      "Kecamatan: <i>", tidak_signifikan_detail$list, "</i>. ",
      "Pada area ini, pola penyebaran kasus DBD bersifat acak atau tidak menunjukkan pengelompokan yang signifikan secara statistik.</li>",
      "</ul>",
      "<p>Informasi ini sangat vital untuk mengarahkan upaya pencegahan, pengendalian, dan alokasi sumber daya di wilayah yang paling membutuhkan.</p>"
    )
    
    return(HTML(info_html))
  })
  
  # Judul untuk plot tren kabupaten
  output$trend_title_kab <- renderText({
    # Ekstrak tahun dari objek Date yang dikembalikan oleh airYearpickerInput
    start_year_val <- if (!is.null(input$start_year_tren_kab)) format(input$start_year_tren_kab, "%Y") else ""
    end_year_val <- if (!is.null(input$end_year_tren_kab)) format(input$end_year_tren_kab, "%Y") else ""
    
    base_title_kab <- "Tren Kasus DBD Kabupaten Jember"
    
    if (start_year_val == "" || end_year_val == "") {
      return(paste(base_title_kab, "(Pilih Rentang Tahun)"))
    }
    
    start_year_num <- as.numeric(start_year_val)
    end_year_num <- as.numeric(end_year_val)
    
    # Use req() here to prevent calculation if years are invalid
    req(start_year_num, end_year_num)
    
    if (start_year_num > end_year_num) {
      return(paste(base_title_kab, "(Rentang Tahun Tidak Valid)"))
    }
    
    if (start_year_num == end_year_num) {
      return(paste(base_title_kab, "Tahun", start_year_num))
    } else {
      return(paste(base_title_kab, "(", start_year_num, "-", end_year_num, ")"))
    }
  })
  
  # Plot Tren Kasus dan Kematian DBD Tingkat Kabupaten
  output$time_series_plot_kab <- renderPlotly({
    # Perbaikan: Tampilkan pesan instruksi jika input belum dipilih
    if (is.null(input$start_year_tren_kab) || is.null(input$end_year_tren_kab)) {
      msg <- "Silakan pilih 'Tahun Awal' dan 'Tahun Akhir' untuk menampilkan tren kasus DBD Kabupaten Jember."
      return(create_blank_message_plot(wrap_text_html(msg, line_length = 50)))
    }
    
    # Extract and validate year values from date inputs
    start_year_val <- if (!is.null(input$start_year_tren_kab)) format(input$start_year_tren_kab, "%Y") else ""
    end_year_val <- if (!is.null(input$end_year_tren_kab)) format(input$end_year_tren_kab, "%Y") else ""
    
    # Convert to numeric
    start_year_val_num <- if (start_year_val != "") as.numeric(start_year_val) else NULL
    end_year_val_num <- if (end_year_val != "") as.numeric(end_year_val) else NULL
    
    # Use req() to ensure inputs are not NULL and are valid before proceeding
    req(start_year_val_num, end_year_val_num)
    if (start_year_val_num > end_year_val_num) {
      return(
        plotly_empty(type = "scatter", mode = "lines") %>%
          layout(
            title = list(
              text = "Rentang Tahun Tidak Valid.",
              font = list(size = 18),
              xref = "paper",
              yref = "paper",
              x = 0.5,
              y = 0.5,
              xanchor = "center",
              yanchor = "middle",
              align = "center"
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    # Correct way to call the function: first get the inner function, then call it
    get_data_tren_kab <- get_data_tren_dbd_kab_func_wrapper()
    data_tren <- get_data_tren_kab(start_year_val_num, end_year_val_num)
    
    if (is.null(data_tren) || nrow(data_tren) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "lines") %>%
          layout(
            title = list(
              text = if (start_year_val_num == end_year_val_num) {
                wrap_text_html(paste0("Data Tren Tingkat Kabupaten tidak tersedia untuk Tahun ", start_year_val_num, "."), line_length = 50)
              } else {
                wrap_text_html(paste0("Data Tren Tingkat Kabupaten tidak tersedia untuk rentang Tahun ", start_year_val_num, " - ", end_year_val_num, "."), line_length = 50)
              },
              font = list(size = 18),
              xref = "paper",
              yref = "paper",
              x = 0.5,
              y = 0.5,
              xanchor = "center",
              yanchor = "middle",
              align = "center"
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    if (start_year_val_num == end_year_val_num) {
      # Jika hanya ada satu tahun, gunakan bar chart
      p <- plot_ly(data_tren) %>%
        add_trace(x = ~tahun, y = ~total_kasus, name = 'Total Kasus',
                  type = 'bar', marker = list(color = 'blue')) %>%
        layout(
          title = "",
          xaxis = list(title = "Tahun", type = "category"),
          yaxis = list(title = "Jumlah"),
          barmode = 'group',
          bargap = 0.8,
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.2)
        ) %>%
        config(displayModeBar = FALSE)
      
      # Tambahkan label angka di atas bar
      p <- p %>%
        add_text(
          x = ~tahun,
          y = ~total_kasus,
          text = ~total_kasus,
          textposition = "top center",
          showlegend = FALSE,
          inherit = FALSE,
          textfont = list(size = 10, color = 'black'),
          cliponaxis = FALSE
        )
      return(p)
      
    } else {
      # Jika ada rentang tahun, gunakan line plot seperti sebelumnya
      plot_ly(data_tren, x = ~tahun) %>%
        add_trace(y = ~total_kasus, name = 'Total Kasus',
                  type = 'scatter', mode = 'lines+markers+text',
                  text = ~total_kasus,
                  textposition = 'top center',
                  textfont = list(size = 8),
                  line = list(color = 'blue', width = 3),
                  marker = list(color = 'blue', size = 6)) %>%
        layout(
          title = "",
          xaxis = list(title = "Tahun", type = "category", tickvals = data_tren$tahun),
          yaxis = list(title = "Jumlah"),
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.2)
        ) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  # Judul untuk plot tren kecamatan
  output$trend_title_kec_single_year <- renderText({
    # Ekstrak tahun dari objek Date
    selected_year <- if (!is.null(input$selected_year_kec_single)) format(input$selected_year_kec_single, "%Y") else ""
    selected_triwulan <- input$selected_triwulan_kec
    
    base_title <- "Tren Kasus DBD per Kecamatan"
    
    if (!is.null(selected_year) && selected_year != "") {
      base_title <- paste(base_title, "Tahun", selected_year)
    }
    
    if (!is.null(selected_triwulan) && selected_triwulan != "") {
      if (!is.null(selected_year) && selected_year != "") {
        base_title <- paste(base_title, "Triwulan", selected_triwulan)
      } else {
        base_title <- paste(base_title, "Triwulan", selected_triwulan)
      }
    }
    
    return(base_title)
  })
  
  # Plot Tren Kasus dan Kematian DBD Tingkat Kecamatan (Tahun Tunggal)
  output$time_series_plot_kec_single_year <- renderPlotly({
    # Perbaikan: Tampilkan pesan instruksi jika input belum dipilih
    if (is.null(input$selected_year_kec_single) || is.null(input$selected_triwulan_kec) || input$selected_triwulan_kec == "") {
      msg <- "Silakan pilih 'Tahun' dan 'Triwulan' untuk menampilkan tren kasus DBD per Kecamatan."
      return(create_blank_message_plot(wrap_text_html(msg, line_length = 50)))
    }
    
    # Ekstrak tahun dari objek Date
    selected_year_val_num <- if (!is.null(input$selected_year_kec_single)) as.numeric(format(input$selected_year_kec_single, "%Y")) else NULL
    
    req(selected_year_val_num, input$selected_triwulan_kec)
    
    get_data_tren_kec <- get_data_tren_dbd_kec_func_wrapper()
    data_tren_result <- get_data_tren_kec(selected_year_val_num, input$selected_triwulan_kec)
    all_kec_names <- all_kecamatan_names_reactive()
    
    # Handle status from model.R
    if (!is.null(data_tren_result) && !is.null(data_tren_result$status)) {
      status_message <- switch(data_tren_result$status,
                               "input_not_selected" = "Silakan pilih Tahun dan Triwulan.",
                               "no_data" = paste0("Tidak ada data kasus DBD yang tercatat untuk Tahun ", data_tren_result$year, " Triwulan ", data_tren_result$triwulan, "."),
                               "db_error" = paste0("Kesalahan database saat mengambil data: ", data_tren_result$message),
                               "Data tidak tersedia."
      )
      return(create_blank_message_plot(wrap_text_html(status_message, line_length = 50)))
    }
    
    # If not a status object, assume it's data
    data_tren <- data_tren_result
    
    # Perbaikan: Sesuaikan pesan agar mirip dengan tren kabupaten
    if (is.null(data_tren) || nrow(data_tren) == 0 || is.null(all_kec_names)) {
      # Updated message to include specific year and triwulan
      current_year_msg <- selected_year_val_num
      current_triwulan_msg <- input$selected_triwulan_kec
      
      msg <- paste0("Data Tren Tingkat Kecamatan tidak tersedia untuk Tahun ", current_year_msg, " Triwulan ", current_triwulan_msg, ".")
      
      return(
        plotly_empty(type = "scatter", mode = "lines") %>%
          layout(
            title = list(
              text = wrap_text_html(msg, line_length = 50), # Use wrap_text_html here
              font = list(size = 18),
              xref = "paper",
              yref = "paper",
              x = 0.5,
              y = 0.5,
              xanchor = "center",
              yanchor = "middle",
              align = "center"
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    data_tren$kecamatan <- factor(data_tren$kecamatan, levels = all_kec_names)
    
    data_long <- data_tren %>%
      tidyr::pivot_longer(
        cols = c(total_kasus),
        names_to = "keterangan",
        values_to = "jumlah"
      ) %>%
      mutate(
        keterangan = factor(keterangan, levels = c("total_kasus"),
                            labels = c("Kasus")),
        tooltip_display_text = paste0("Kecamatan: ", kecamatan,
                                      "<br>",
                                      ifelse(keterangan == "Kasus", "Jumlah Kasus: ", ""),
                                      jumlah)
      )
    
    p <- ggplot(data_long, aes(x = kecamatan, y = jumlah, fill = keterangan,
                               text = tooltip_display_text)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = jumlah),
                position = position_dodge(width = 0.9),
                vjust = -1.2,
                size = 3,
                color = "black",
                check_overlap = TRUE) +
      labs(
        title = NULL,
        x = "Kecamatan",
        y = "Jumlah Kasus",
        fill = "Keterangan"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      ) +
      scale_fill_manual(values = c("Kasus" = "steelblue")) +
      scale_x_discrete(limits = all_kec_names)
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  #informasiTrenKec
  output$infoTrenKasusKec <- renderUI({
    selected_year_val_num <- if (!is.null(input$selected_year_kec_single)) as.numeric(format(input$selected_year_kec_single, "%Y")) else NULL
    selected_triwulan <- input$selected_triwulan_kec
    
    # Tampilkan pesan jika input belum lengkap
    if (is.null(selected_year_val_num) || is.null(selected_triwulan) || selected_triwulan == "") {
      return(HTML("<p>Silakan pilih <b>Tahun</b> dan <b>Triwulan</b> untuk melihat informasi detail tren kasus DBD per kecamatan.</p>"))
    }
    
    # Ambil data tren kecamatan
    get_data_tren_kec <- get_data_tren_dbd_kec_func_wrapper()
    data_tren <- get_data_tren_kec(selected_year_val_num, selected_triwulan)
    
    # Tangani kasus data tidak tersedia
    if (is.null(data_tren) || nrow(data_tren) == 0) {
      return(HTML(paste0("<p>Tidak ada data kasus DBD yang tersedia untuk Tahun <b>", selected_year_val_num, "</b> Triwulan <b>", selected_triwulan, "</b>.</p>")))
    }
    
    # Hitung statistik yang diperlukan
    total_kasus_provinsi <- sum(data_tren$total_kasus, na.rm = TRUE)
    rata_rata_kasus_kecamatan <- mean(data_tren$total_kasus, na.rm = TRUE)
    
    # Sortir untuk menemukan kecamatan dengan kasus tertinggi dan terendah
    data_sorted_highest <- data_tren %>%
      arrange(desc(total_kasus)) %>%
      head(3)
    
    data_sorted_lowest <- data_tren %>%
      arrange(total_kasus) %>%
      head(3)
    
    # Bangun pesan HTML
    info_html <- paste0(
      "<p>Untuk <b>Tahun ", selected_year_val_num, "</b> dan <b>Triwulan ", selected_triwulan, "</b>:</p>",
      "<ul>",
      "<li>Total Kasus DBD di seluruh kecamatan Jember: <b>", total_kasus_provinsi, "</b> kasus.</li>",
      "<li>Rata-rata kasus DBD per kecamatan: <b>", round(rata_rata_kasus_kecamatan, 2), "</b> kasus.</li>",
      "</ul>",
      "<p><b>Kecamatan dengan Kasus DBD TERTINGGI:</b></p>",
      "<ul>"
    )
    
    for (i in 1:nrow(data_sorted_highest)) {
      info_html <- paste0(info_html, "<li>", data_sorted_highest$kecamatan[i], ": <b>", data_sorted_highest$total_kasus[i], "</b> kasus</li>")
    }
    info_html <- paste0(info_html, "</ul>")
    
    info_html <- paste0(info_html, "<p><b>Kecamatan dengan Kasus DBD TERENDAH:</b></p><ul>")
    for (i in 1:nrow(data_sorted_lowest)) {
      info_html <- paste0(info_html, "<li>", data_sorted_lowest$kecamatan[i], ": <b>", data_sorted_lowest$total_kasus[i], "</b> kasus</li>")
    }
    info_html <- paste0(info_html, "</ul>")
    
    # Tambahkan interpretasi
    info_html <- paste0(info_html, "<p>Informasi ini dapat membantu mengidentifikasi wilayah yang memerlukan perhatian lebih (kasus tinggi) atau sebagai target keberhasilan program pencegahan (kasus rendah) pada triwulan dan tahun yang dipilih.</p>")
    
    return(HTML(info_html))
  })
  
  # Judul untuk Analisis Moran's Global
  output$moran_global_title <- renderText({
    # Ekstrak tahun dari objek Date yang dikembalikan oleh airYearpickerInput
    selected_year <- if (!is.null(input$moran_year)) format(input$moran_year, "%Y") else ""
    selected_triwulan <- input$moran_triwulan
    
    base_title <- "Analisis Indeks Moran Global Kasus DBD"
    
    if (selected_year == "" || selected_triwulan == "") {
      return(paste(base_title, "(Pilih Tahun dan Triwulan)"))
    }
    
    paste(base_title, "Tahun", selected_year, "Triwulan", selected_triwulan)
  })
  
  # Output untuk Analisis Moran's Global (Plot)
  output$moran_scatter_plot <- renderPlotly({
    selected_year <- if (!is.null(input$moran_year)) as.numeric(format(input$moran_year, "%Y")) else NULL
    selected_triwulan <- input$moran_triwulan
    
    # Perbaikan: Tampilkan pesan instruksi jika input belum dipilih
    if (is.null(selected_year) || is.null(selected_triwulan) || selected_triwulan == "") {
      msg <- "Silakan pilih 'Tahun' dan 'Triwulan' untuk menjalankan Analisis Indeks Moran Global."
      return(create_blank_message_plot(wrap_text_html(msg, line_length = 50)))
    }
    
    req(selected_year, selected_triwulan)
    
    get_moran_data_func <- get_data_kasus_moran_func_wrapper()
    moran_data_list <- get_moran_data_func(selected_year, selected_triwulan)
    
    # LOGIKA BARU: Penanganan Status Khusus
    if (!is.null(moran_data_list) && !is.null(moran_data_list$status)) {
      status_message <- switch(moran_data_list$status,
                               "input_not_selected" = "Silakan pilih Tahun dan Triwulan.",
                               "no_case_data_for_moran" = paste0("Tidak ada data kasus DBD yang tercatat untuk Tahun ", moran_data_list$year, " Triwulan ", moran_data_list$triwulan, " untuk analisis Moran."),
                               "low_variability" = paste0("Variabilitas data kasus DBD sangat rendah atau nol untuk Tahun ", moran_data_list$year, " Triwulan ", moran_data_list$triwulan, ". Analisis Moran tidak dapat dilakukan."),
                               "no_geom_data" = "Data geometri kecamatan tidak tersedia.",
                               "no_valid_spatial_data" = "Tidak ada data spasial yang valid setelah penggabungan.",
                               "not_enough_locations" = "Jumlah kecamatan terlalu sedikit (< 2) untuk analisis Moran.",
                               "not_enough_centroids" = "Tidak cukup centroid valid untuk matriks bobot (< 2 titik).",
                               "invalid_geometry_type" = "Tipe geometri tidak valid setelah centroid.",
                               "undefined_crs" = "Sistem Koordinat Referensi (CRS) tidak terdefinisi.",
                               "reprojection_failed" = "Gagal reproyeksi koordinat centroid.",
                               "empty_projected_coords" = "Koordinat terproyeksi kosong.",
                               "sf_to_sp_conversion_failed" = "Gagal konversi objek sf ke sp.",
                               "sp_object_null" = "Objek Spatial (sp) null.",
                               "sp_coords_na" = "Koordinat Spatial (sp) mengandung NA.",
                               "invalid_k_val" = "Nilai K (jumlah tetangga) tidak valid.",
                               "manual_nb_failed" = "Gagal membuat objek tetangga KNN secara manual.",
                               "manual_nb_failed_moran" = paste0("Gagal membuat objek tetangga KNN (dalam Moran): ", moran_data_list$message),
                               "nb_invalid_after_construction" = "Objek tetangga (nb) tidak valid setelah konstruksi.",
                               "nb_empty" = "Objek tetangga (nb) kosong.",
                               "no_neighbors_found" = "Tidak ada tetangga yang ditemukan untuk titik data.",
                               "listw_creation_failed" = "Gagal membuat matriks bobot spasial (listw).",
                               "listw_invalid" = "Matriks bobot spasial (listw) tidak valid.",
                               "invalid_weights" = "Beberapa observasi memiliki bobot NA atau tidak ada tetangga dalam matriks bobot spasial.",
                               "general_error" = paste0("Terjadi kesalahan umum: ", moran_data_list$message),
                               "Data tidak tersedia atau analisis gagal."
      )
      
      return(plotly_empty(type = "scatter", mode = "lines") %>%
               layout(
                 title = list(
                   text = wrap_text_html(status_message, line_length = 55),
                   font = list(size = 18),
                   xref = "paper", yref = "paper", x = 0.5, y = 0.5, xanchor = "center", yanchor = "middle", align = "center"
                 ),
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE)
               ) %>%
               config(displayModeBar = FALSE)
      )
    }
    
    # Logika yang ada jika data valid
    data_for_moran <- moran_data_list$data
    lw <- moran_data_list$lw
    
    # Pengecekan variabilitas data lagi untuk keamanan (ini seharusnya sudah ditangani oleh status "low_variability")
    if (sd(data_for_moran$log_kasus, na.rm = TRUE) == 0) {
      return(plotly_empty() %>%
               layout(title = "Variabilitas data kasus DBD (setelah transformasi log) sangat rendah atau nol.\nMoran Scatterplot tidak dapat dibuat."))
    }
    
    if (nrow(data_for_moran) == 0 || is.null(lw)) {
      return(plotly_empty() %>%
               layout(title = "Tidak ada data atau matriks bobot spasial yang valid untuk Moran Scatterplot."))
    }
    
    # Hitung lag spasial dari log_kasus
    z_log_kasus <- scale(data_for_moran$log_kasus)
    spatial_lag_log_kasus <- lag.listw(lw, z_log_kasus, zero.policy = TRUE)
    
    # Buat dataframe untuk plotting
    moran_df <- data.frame(
      log_kasus_std = as.vector(z_log_kasus),
      spatial_lag = as.vector(spatial_lag_log_kasus),
      kecamatan = data_for_moran$kecamatan
    )
    
    # Hitung kuadran
    moran_df <- moran_df %>%
      mutate(
        kuadran = case_when(
          log_kasus_std > 0 & spatial_lag > 0 ~ "HH (Tinggi-Tinggi)",
          log_kasus_std < 0 & spatial_lag < 0 ~ "LL (Rendah-Rendah)",
          log_kasus_std > 0 & spatial_lag < 0 ~ "HL (Tinggi-Rendah)",
          log_kasus_std < 0 & spatial_lag > 0 ~ "LH (Rendah-Tinggi)",
          TRUE ~ "Lainnya"
        )
      )
    
    # Buat scatter plot
    p <- ggplot(moran_df, aes(x = log_kasus_std, y = spatial_lag, color = kuadran,
                              text = paste0("Kecamatan: ", kecamatan,
                                            "<br>Kuadran: ", kuadran))) +
      geom_point(alpha = 0.7) +
      geom_text(aes(label = kecamatan),
                vjust = -0.8,
                size = 3.3,
                color = "black",
                check_overlap = TRUE) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      labs(
        title = "",
        x = "Kasus DBD",
        y = "Lag Spasial Kasus DBD",
        color = "Kuadran Moran"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.2)) +
      scale_color_manual(values = c("HH (Tinggi-Tinggi)" = "red",
                                    "LL (Rendah-Rendah)" = "blue",
                                    "HL (Tinggi-Rendah)" = "orange",
                                    "LH (Rendah-Tinggi)" = "green",
                                    "Lainnya" = "gray"))
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # Output untuk Analisis Moran's Global (Teks)
  output$morans_output <- renderPrint({
    selected_year <- if (!is.null(input$moran_year)) as.numeric(format(input$moran_year, "%Y")) else NULL
    selected_triwulan <- input$moran_triwulan
    
    req(selected_year, selected_triwulan)
    
    get_moran_data_func <- get_data_kasus_moran_func_wrapper()
    moran_data_list <- get_moran_data_func(selected_year, selected_triwulan)
    
    # LOGIKA BARU: Penanganan Status Khusus
    if (!is.null(moran_data_list) && !is.null(moran_data_list$status)) {
      status_message <- switch(moran_data_list$status,
                               "input_not_selected" = "Silakan pilih Tahun dan Triwulan untuk menjalankan analisis.",
                               "no_case_data_for_moran" = paste0("Tidak ada data kasus DBD yang tercatat untuk Tahun ", moran_data_list$year, " Triwulan ", moran_data_list$triwulan, " untuk analisis Moran."),
                               "low_variability" = paste0("Variabilitas data kasus DBD sangat rendah atau nol untuk Tahun ", moran_data_list$year, " Triwulan ", moran_data_list$triwulan, ". Analisis Moran tidak dapat dilakukan karena semua nilai kasus sama."),
                               "no_geom_data" = "Data geometri kecamatan tidak tersedia.",
                               "no_valid_spatial_data" = "Tidak ada data spasial yang valid setelah penggabungan data.",
                               "not_enough_locations" = "Jumlah kecamatan terlalu sedikit (< 2) untuk analisis Moran.",
                               "not_enough_centroids" = "Tidak cukup centroid yang valid untuk membuat matriks bobot spasial (< 2 titik).",
                               "invalid_geometry_type" = "Geometri bukan tipe POINT setelah centroid.",
                               "undefined_crs" = "Sistem Koordinat Referensi (CRS) tidak terdefinisi pada data geometri.",
                               "reprojection_failed" = "Gagal reproyeksi koordinat centroid.",
                               "empty_projected_coords" = "Koordinat terproyeksi kosong.",
                               "sf_to_sp_conversion_failed" = "Gagal mengonversi objek sf ke objek Spatial.",
                               "sp_object_null" = "Objek Spatial (sp) adalah null.",
                               "sp_coords_na" = "Koordinat spasial mengandung nilai NA.",
                               "invalid_k_val" = "Nilai k (jumlah tetangga) tidak valid (< 1).",
                               "manual_nb_failed" = "Gagal membuat objek tetangga KNN secara manual. Periksa detail error di konsol.",
                               "manual_nb_failed_moran" = paste0("Gagal membuat objek tetangga KNN (dalam Moran): ", moran_data_list$message),
                               "nb_invalid_after_construction" = "Objek tetangga (nb) tidak valid setelah konstruksi.",
                               "nb_empty" = "Objek tetangga (nb) kosong.",
                               "no_neighbors_found" = "Tidak ada tetangga yang ditemukan untuk titik data. Periksa sebaran spasial data Anda atau metode pembobot spasial.",
                               "listw_creation_failed" = "Gagal membuat matriks bobot spasial (listw).",
                               "listw_invalid" = "Matriks bobot spasial (listw) tidak valid.",
                               "invalid_weights" = "Beberapa observasi memiliki bobot NA atau tidak ada tetangga dalam matriks bobot spasial. Analisis mungkin tidak akurat atau gagal.",
                               "general_error" = paste0("Terjadi kesalahan umum: ", moran_data_list$message),
                               "Data tidak tersedia atau analisis gagal. Silakan periksa pilihan tahun/triwulan Anda."
      )
      cat(wrap_text_plain(status_message, width = 80))
      return(NULL)
    }
    
    # Tambahkan pengecekan NULL dan variabilitas data di sini juga
    if (is.null(moran_data_list) || is.null(moran_data_list$data) || is.null(moran_data_list$lw)) {
      cat("Data atau matriks bobot spasial tidak tersedia untuk perhitungan Moran's I. Silakan periksa notifikasi di atas atau pilihan tahun/triwulan Anda.\n")
      return(NULL)
    }
    
    data_for_moran <- moran_data_list$data
    lw <- moran_data_list$lw
    
    # Pengecekan variabilitas data lagi untuk keamanan
    if (sd(data_for_moran$log_kasus, na.rm = TRUE) == 0) {
      cat("Variabilitas data kasus DBD (setelah transformasi log) sangat rendah atau nol.\n")
      cat("Indeks Moran tidak dapat dihitung karena semua nilai kasus sama.\n")
      return(NULL)
    }
    
    # Pastikan ada data dan bobot spasial yang valid
    if (nrow(data_for_moran) == 0 || is.null(lw)) {
      cat("Tidak ada data atau matriks bobot spasial yang valid untuk perhitungan Moran's I.")
      return(NULL)
    }
    
    # Hitung Moran's I Global untuk log_kasus
    moran_test_result <- tryCatch({
      spdep::moran.test(data_for_moran$log_kasus, lw, zero.policy = TRUE)
    }, error = function(e) {
      showNotification(
        paste("Gagal menghitung Moran's I Global:", e$message),
        type = "error",
        duration = NULL
      )
      cat("Error saat menghitung Moran's I Global. Mohon periksa konsol untuk detailnya.")
      return(NULL)
    })
    
    if (!is.null(moran_test_result)) {
      cat("Indeks Moran Global untuk Kasus DBD (log-transformed):\n")
      print(moran_test_result)
      cat("\nInterpretasi:\n")
      if (moran_test_result$p.value < 0.05) {
        if (moran_test_result$estimate[1] > 0) {
          cat("Nilai Indeks Moran (I) positif dan signifikan (p-value < 0.05).\n")
          cat("Ini menunjukkan adanya *autokorelasi spasial positif* pada kasus DBD (setelah transformasi log).\n")
          cat("Kecamatan dengan kasus DBD tinggi cenderung bertetangga dengan kecamatan berkasus DBD tinggi, dan sebaliknya (klasterisasi).\n")
        } else {
          cat("Nilai Indeks Moran (I) negatif dan signifikan (p-value < 0.05).\n")
          cat("Ini menunjukkan adanya *autokorelasi spasial negatif* pada kasus DBD (setelah transformasi log).\n")
          cat("Kecamatan dengan kasus DBD tinggi cenderung bertetangga dengan kecamatan berkasus DBD rendah, dan sebaliknya (pola menyebar).\n")
        }
      } else {
        cat("Nilai Indeks Moran (I) tidak signifikan (p-value >= 0.05).\n")
        cat("Ini menunjukkan *tidak ada autokorelasi spasial yang signifikan* pada kasus DBD (setelah transformasi log).\n")
        cat("Penyebaran kasus DBD bersifat acak atau tidak menunjukkan pola spasial yang jelas.\n")
      }
    }
  })
  
  # Judul untuk LISA Bivariat
  output$lisa_bivariat_title <- renderText({
    selected_year <- if (!is.null(input$lisa_bivar_year)) format(input$lisa_bivar_year, "%Y") else ""
    selected_triwulan <- input$lisa_bivar_triwulan
    var_mapping_bivar <- c("Pilih Variabel"="",
                           "Curah Hujan" = "total_curah_hujan",
                           "Angka Bebas Jentik (ABJ)" = "total_abj",
                           "Suhu Rata-rata" = "avg_suhu",
                           "Kelembaban Rata-rata" = "avg_kelembaban",
                           "Kepadatan Penduduk" = "total_penduduk")
    selected_variable_label <- names(var_mapping_bivar[var_mapping_bivar == input$lisa_bivar_variable])
    if (length(selected_variable_label) == 0) selected_variable_label <- "Variabel Lain"
    
    if (selected_year == "" || selected_triwulan == "" || input$lisa_bivar_variable == "") {
      return("Analisis LISA Bivariat Kasus DBD & Variabel Lain (Pilih Tahun, Triwulan, Variabel)")
    } else {
      return(paste0("Analisis LISA Bivariat Kasus DBD & ", selected_variable_label, " Tahun ", selected_year, " Triwulan ", selected_triwulan))
    }
  })
  
  # LISA Bivariat Map
  output$lisa_bivariat_map <- renderLeaflet({
    selected_year <- if (!is.null(input$lisa_bivar_year)) as.numeric(format(input$lisa_bivar_year, "%Y")) else NULL
    selected_triwulan <- input$lisa_bivar_triwulan
    selected_variable <- input$lisa_bivar_variable
    
    peta_dasar <- leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng = 113.6, lat = -8.17, zoom = 10)
    
    if (is.null(selected_year) || is.null(selected_triwulan) || selected_triwulan == "" || is.null(selected_variable) || selected_variable == "") {
      lisa_bivar_data_for_info_rv(NULL)
      return(peta_dasar %>%
               addPopups(113.6, -8.17, "Silakan pilih Tahun, Triwulan, dan Variabel untuk menampilkan Peta LISA Bivariat."))
    }
    
    get_moran_data_func <- get_data_kasus_moran_func_wrapper()
    bivar_data_list <- get_moran_data_func(selected_year, selected_triwulan)
    
    # Handle status/errors from data retrieval
    if (!is.null(bivar_data_list) && !is.null(bivar_data_list$status)) {
      lisa_bivar_data_for_info_rv(NULL)
      status_message <- switch(bivar_data_list$status,
                               "input_not_selected" = "Silakan pilih Tahun, Triwulan, dan Variabel.",
                               "no_case_data_for_moran" = paste0("Tidak ada data kasus DBD yang tercatat untuk Tahun ", bivar_data_list$year, " Triwulan ", bivar_data_list$triwulan, "."),
                               "low_variability" = paste0("Variabilitas data sangat rendah atau nol untuk Tahun ", bivar_data_list$year, " Triwulan ", bivar_data_list$triwulan, ". Analisis LISA Bivariat tidak dapat dilakukan."),
                               "no_geom_data" = "Data geometri kecamatan tidak tersedia.",
                               "no_valid_spatial_data" = "Tidak ada data spasial yang valid setelah penggabungan.",
                               "not_enough_locations" = "Jumlah kecamatan terlalu sedikit (< 2) untuk analisis.",
                               "not_enough_centroids" = "Tidak cukup centroid valid untuk matriks bobot.",
                               "invalid_geometry_type" = "Tipe geometri tidak valid setelah centroid.",
                               "undefined_crs" = "Sistem Koordinat Referensi (CRS) tidak terdefinisi.",
                               "reprojection_failed" = "Gagal reproyeksi koordinat centroid.",
                               "empty_projected_coords" = "Koordinat terproyeksi kosong.",
                               "sf_to_sp_conversion_failed" = "Gagal konversi objek sf ke sp.",
                               "sp_object_null" = "Objek Spatial (sp) null.",
                               "sp_coords_na" = "Koordinat Spatial (sp) mengandung NA.",
                               "invalid_k_val" = "Nilai K (jumlah tetangga) tidak valid.",
                               "manual_nb_failed" = "Gagal membuat objek tetangga KNN secara manual.",
                               "manual_nb_failed_moran" = paste0("Gagal membuat objek tetangga KNN (dalam Moran): ", bivar_data_list$message),
                               "nb_invalid_after_construction" = "Objek tetangga (nb) tidak valid setelah konstruksi.",
                               "nb_empty" = "Objek tetangga (nb) kosong.",
                               "no_neighbors_found" = "Tidak ada tetangga yang ditemukan untuk titik data.",
                               "listw_creation_failed" = "Gagal membuat matriks bobot spasial.",
                               "listw_invalid" = "Matriks bobot spasial tidak valid.",
                               "invalid_weights" = "Beberapa observasi memiliki bobot NA atau tidak ada tetangga dalam matriks bobot spasial.",
                               "general_error" = paste0("Terjadi kesalahan umum: ", bivar_data_list$message),
                               "Data tidak tersedia atau analisis gagal."
      )
      return(peta_dasar %>%
               addPopups(113.6, -8.17, paste0("LISA Bivariat Gagal Dibuat:<br/>", wrap_text_html(status_message, line_length = 40))))
    }
    
    # Extract data and spatial weights
    data_bivar <- bivar_data_list$data
    lw <- bivar_data_list$lw
    
    # Check for selected variable validity
    if (!(selected_variable %in% names(data_bivar))) {
      lisa_bivar_data_for_info_rv(NULL)
      return(peta_dasar %>% addPopups(113.6, -8.17, paste0("Variabel '", selected_variable, "' tidak ditemukan dalam data.<br/>Periksa nama kolom di database atau logika model.")))
    }
    
    # Periksa variabilitas variabel utama dan variabel kedua
    if (sd(data_bivar$log_kasus, na.rm = TRUE) == 0 || sd(data_bivar[[selected_variable]], na.rm = TRUE) == 0) {
      lisa_bivar_data_for_info_rv(NULL)
      return(peta_dasar %>%
               addPopups(113.6, -8.17, paste0("Variabilitas data Kasus DBD atau '", selected_variable, "' sangat rendah atau nol.\nAnalisis LISA Bivariat tidak dapat dilakukan.")))
    }
    
    # Perform Bivariate LISA calculation
    # Standarisasi variabel X (Kasus DBD)
    z_var1 <- scale(data_bivar$log_kasus) %>% as.vector()
    # Standarisasi variabel Y (Variabel kedua terpilih)
    if (selected_variable == "total_penduduk") {
      z_var2_raw <- data_bivar[[selected_variable]]
      z_var2 <- scale(z_var2_raw) %>% as.vector()
    } else if (selected_variable == "total_curah_hujan") {
      z_var2 <- scale(data_bivar$log_curah_hujan) %>% as.vector()
    } else {
      z_var2 <- scale(data_bivar[[selected_variable]]) %>% as.vector()
    }
    
    # Calculate spatial lag for var2 (standardized Y variable)
    spatial_lag_var2 <- lag.listw(lw, z_var2, zero.policy = TRUE)
    
    # Classification into quadrants (based on signs, not necessarily significance for each location)
    data_bivar <- data_bivar %>%
      mutate(
        bivar_kuadran = case_when(
          z_var1 > 0 & spatial_lag_var2 > 0 ~ "HH (Kasus Tinggi - Variabel Lain Tinggi)",
          z_var1 < 0 & spatial_lag_var2 < 0 ~ "LL (Kasus Rendah - Variabel Lain Rendah)",
          z_var1 > 0 & spatial_lag_var2 < 0 ~ "HL (Kasus Tinggi - Variabel Lain Rendah)",
          z_var1 < 0 & spatial_lag_var2 > 0 ~ "LH (Kasus Rendah - Variabel Lain Tinggi)",
          TRUE ~ "Tidak Terklasifikasi"
        )
      )
    
    # Update reactiveVal with data for info
    lisa_bivar_data_for_info_rv(data_bivar)
    
    # Define color palette for Bivariate LISA quadrants
    pal_bivar <- colorFactor(
      palette = c("red", "blue", "orange", "green", "gray"),
      domain = c("HH (Kasus Tinggi - Variabel Lain Tinggi)",
                 "LL (Kasus Rendah - Variabel Lain Rendah)",
                 "HL (Kasus Tinggi - Variabel Lain Rendah)",
                 "LH (Kasus Rendah - Variabel Lain Tinggi)",
                 "Tidak Terklasifikasi")
    )
    
    # Get variable label for popup
    selected_variable_label <- names(which(c("Pilih Variabel"="",
                                             "Curah Hujan" = "total_curah_hujan",
                                             "Angka Bebas Jentik (ABJ)" = "total_abj",
                                             "Suhu Rata-rata" = "avg_suhu",
                                             "Kelembaban Rata-rata" = "avg_kelembaban",
                                             "Total Penduduk" = "total_penduduk") == input$lisa_bivar_variable))
    
    # Create pop-up labels for Bivariate LISA map
    labels_bivar <- paste0(
      "<strong>Kecamatan: </strong>", data_bivar$kecamatan, "<br/>",
      "<strong>Kasus DBD: </strong>", data_bivar$total_kasus, "<br/>",
      "<strong>", selected_variable_label, ": </strong>", round(data_bivar[[selected_variable]], 2), "<br/>",
      "<strong>Kuadran: </strong>", data_bivar$bivar_kuadran
    ) %>% lapply(htmltools::HTML)
    
    # Add polygons with Bivariate LISA coloring
    peta_bivar <- peta_dasar %>%
      addPolygons(
        data = data_bivar,
        fillColor = ~pal_bivar(bivar_kuadran),
        weight = 1,
        opacity = 1,
        color = "darkblue",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE),
        label = labels_bivar,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addLegend(
        pal = pal_bivar,
        values = c("HH (Kasus Tinggi - Variabel Lain Tinggi)",
                   "LL (Kasus Rendah - Variabel Lain Rendah)",
                   "HL (Kasus Tinggi - Variabel Lain Rendah)",
                   "LH (Kasus Rendah - Variabel Lain Tinggi)",
                   "Tidak Terklasifikasi"),
        opacity = 0.7,
        title = paste0("Klaster Kasus DBD &<br/>", selected_variable_label),
        position = "bottomright"
      )
    
    return(peta_bivar)
  })
  
  # Output untuk Informasi LISA Bivariat secara Dinamis
  output$infoLisaBivariat <- renderUI({
    current_bivar_data <- lisa_bivar_data_for_info_rv()
    selected_year <- if (!is.null(input$lisa_bivar_year)) format(input$lisa_bivar_year, "%Y") else "..."
    selected_triwulan <- if (!is.null(input$lisa_bivar_triwulan) && input$lisa_bivar_triwulan != "") input$lisa_bivar_triwulan else "..."
    selected_variable_label <- names(which(c("Pilih Variabel"="",
                                             "Curah Hujan" = "total_curah_hujan",
                                             "Angka Bebas Jentik (ABJ)" = "total_abj",
                                             "Suhu Rata-rata" = "avg_suhu",
                                             "Kelembaban Rata-rata" = "avg_kelembaban",
                                             "Total Penduduk" = "total_penduduk") == input$lisa_bivar_variable))
    
    if (is.null(current_bivar_data) || nrow(current_bivar_data) == 0 || selected_variable_label == "") {
      if (selected_year == "..." || selected_triwulan == "..." || selected_variable_label == "") {
        return(HTML("<p>Pilih <b>Tahun</b>, <b>Triwulan</b>, dan <b>Variabel Lain</b> di atas untuk menampilkan informasi detail dan peta LISA Bivariat.</p>"))
      } else {
        return(HTML(paste0("<p>Tidak ada data kasus DBD atau '", selected_variable_label, "' yang tersedia, atau peta tidak dapat dibuat untuk Tahun <b>", selected_year, "</b> Triwulan <b>", selected_triwulan, "</b>. Periksa notifikasi di peta atau koneksi database.</p>")))
      }
    }
    
    # Hitung jumlah dan kumpulkan nama kecamatan per kuadran bivariat
    kuadran_info_bivar <- current_bivar_data %>%
      group_by(bivar_kuadran) %>%
      summarise(
        count = n(),
        kecamatan_list = paste0(sort(kecamatan), collapse = ", ")
      ) %>%
      ungroup() %>%
      arrange(match(bivar_kuadran, c("HH (Kasus Tinggi - Variabel Lain Tinggi)",
                                     "LL (Kasus Rendah - Variabel Lain Rendah)",
                                     "HL (Kasus Tinggi - Variabel Lain Rendah)",
                                     "LH (Kasus Rendah - Variabel Lain Tinggi)",
                                     "Tidak Terklasifikasi")))
    
    # Fungsi bantu untuk mendapatkan info kuadran bivariat
    get_bivar_kuadran_detail <- function(kuadran_name) {
      info_row <- kuadran_info_bivar %>% filter(bivar_kuadran == kuadran_name)
      if (nrow(info_row) > 0) {
        return(list(count = info_row$count, list = info_row$kecamatan_list))
      } else {
        return(list(count = 0, list = "Tidak ada kecamatan."))
      }
    }
    
    hh_bivar_detail <- get_bivar_kuadran_detail("HH (Kasus Tinggi - Variabel Lain Tinggi)")
    ll_bivar_detail <- get_bivar_kuadran_detail("LL (Kasus Rendah - Variabel Lain Rendah)")
    hl_bivar_detail <- get_bivar_kuadran_detail("HL (Kasus Tinggi - Variabel Lain Rendah)")
    lh_bivar_detail <- get_bivar_kuadran_detail("LH (Kasus Rendah - Variabel Lain Tinggi)")
    tidak_terklasifikasi_bivar_detail <- get_bivar_kuadran_detail("Tidak Terklasifikasi")
    
    info_html <- paste0(
      "<p>Peta di atas menampilkan analisis LISA Bivariat antara <b>Kasus DBD</b> (variabel X) dan <b>", selected_variable_label, "</b> (variabel Y) di Kabupaten Jember untuk <b>Tahun ", selected_year, "</b> dan <b>Triwulan ", selected_triwulan, "</b>. Analisis ini menunjukkan pola pengelompokan spasial simultan dari kedua variabel tersebut.</p>",
      "<ul>",
      "<li><b>Area Merah (HH - Kasus Tinggi - ", selected_variable_label, " Tinggi)</b>: Terdapat <b>", hh_bivar_detail$count, "</b> kecamatan dalam klaster ini. ",
      "Kecamatan: <i>", hh_bivar_detail$list, "</i>. ",
      "Ini menunjukkan wilayah di mana kasus DBD dan ", selected_variable_label, " sama-sama tinggi. Area ini mungkin menunjukkan hubungan positif yang kuat antara kedua variabel.</li>",
      "<li><b>Area Biru (LL - Kasus Rendah - ", selected_variable_label, " Rendah)</b>: Terdapat <b>", ll_bivar_detail$count, "</b> kecamatan dalam klaster ini. ",
      "Kecamatan: <i>", ll_bivar_detail$list, "</i>. ",
      "Ini menunjukkan wilayah di mana kasus DBD dan ", selected_variable_label, " sama-sama rendah. Area ini mungkin menunjukkan hubungan positif yang kuat antara kedua variabel.</li>",
      "<li><b>Area Oranye (HL - Kasus Tinggi - ", selected_variable_label, " Rendah)</b>: Terdapat <b>", hl_bivar_detail$count, "</b> kecamatan dalam klaster ini. ",
      "Kecamatan: <i>", hl_bivar_detail$list, "</i>. ",
      "Ini adalah 'outlier', di mana kecamatan dengan jumlah kasus DBD tinggi dikelilingi oleh tetangga dengan ", selected_variable_label, " yang rendah. Hal ini mungkin menunjukkan faktor lain yang mempengaruhi tingginya kasus DBD di luar variabel yang dianalisis.</li>",
      "<li><b>Area Hijau (LH - Kasus Rendah - ", selected_variable_label, " Tinggi)</b>: Terdapat <b>", lh_bivar_detail$count, "</b> kecamatan dalam klaster ini. ",
      "Kecamatan: <i>", lh_bivar_detail$list, "</i>. ",
      "Ini juga 'outlier', di mana kecamatan dengan kasus DBD rendah dikelilingi oleh tetangga dengan ", selected_variable_label, " yang tinggi. Perlu investigasi mengapa kasus tetap rendah meskipun variabel pendorong potensial tinggi.</li>",
      "<li><b>Area Abu-abu (Tidak Terklasifikasi)</b>: Terdapat <b>", tidak_terklasifikasi_bivar_detail$count, "</b> kecamatan dalam kategori ini. ",
      "Kecamatan: <i>", tidak_terklasifikasi_bivar_detail$list, "</i>. ",
      "Pada area ini, pola pengelompokan kedua variabel bersifat acak atau tidak menunjukkan hubungan spasial yang jelas.</li>",
      "</ul>",
      "<p>Analisis bivariat ini membantu mengidentifikasi wilayah di mana hubungan antara kasus DBD dan faktor lingkungan/demografi tertentu terkonsentrasi secara spasial, memberikan wawasan yang lebih dalam untuk strategi intervensi.</p>"
    )
    
    return(HTML(info_html))
  })
  
  # NEW: Judul untuk Regresi Spasial
  output$regression_title <- renderText({
    selected_year <- if (!is.null(input$regression_year)) format(input$regression_year, "%Y") else ""
    selected_triwulan <- input$regression_triwulan
    selected_dep_var_label <- names(which(c("Pilih Variabel" = "", "Kasus DBD" = "log_kasus") == input$regression_var_dependen))
    if (length(selected_dep_var_label) == 0) selected_dep_var_label <- "Variabel Dependen"
    
    selected_indep_vars_labels <- sapply(input$regression_vars_independen, function(x) {
      names(c("Curah Hujan" = "log_curah_hujan",
              "Angka Bebas Jentik (ABJ)" = "total_abj",
              "Suhu Rata-rata" = "avg_suhu",
              "Kelembaban Rata-rata" = "avg_kelembaban",
              "Kepadatan Penduduk" = "total_penduduk")[c("log_curah_hujan", "total_abj", "avg_suhu", "avg_kelembaban", "total_penduduk") == x])
    })
    
    if (selected_year == "" || selected_triwulan == "" || input$regression_var_dependen == "" || is.null(input$regression_vars_independen) || length(input$regression_vars_independen) == 0) {
      return("Analisis Regresi Spasial (Pilih Tahun, Triwulan, Variabel)")
    } else {
      indep_vars_text <- if (length(selected_indep_vars_labels) > 0) paste(selected_indep_vars_labels, collapse = ", ") else "Tidak Ada"
      return(paste0("Analisis Regresi Spasial Kasus DBD (", selected_dep_var_label, ") vs. (", indep_vars_text, ") Tahun ", selected_year, " Triwulan ", selected_triwulan))
    }
  })
  
  # Output untuk Analisis Regresi Spasial (Teks Ringkasan Model)
  output$regression_output <- renderPrint({
    selected_year <- if (!is.null(input$regression_year)) as.numeric(format(input$regression_year, "%Y")) else NULL
    selected_triwulan <- input$regression_triwulan
    dependen_var <- input$regression_var_dependen
    independen_vars <- input$regression_vars_independen
    
    # Reset reactiveVal
    regression_results_rv(NULL)
    
    # Perbaikan: Tampilkan pesan instruksi jika input belum dipilih
    if (is.null(selected_year) || is.null(selected_triwulan) || selected_triwulan == "" ||
        is.null(dependen_var) || dependen_var == "" || is.null(independen_vars) || length(independen_vars) == 0) {
      cat(wrap_text_plain("Silakan pilih Tahun, Triwulan, Variabel Dependen, dan setidaknya satu Variabel Independen untuk menjalankan Analisis Regresi Spasial.", width = 80))
      return(NULL)
    }
    
    req(selected_year, selected_triwulan)
    
    get_reg_data_func <- get_regression_data_func_wrapper()
    reg_data_list <- get_reg_data_func(selected_year, selected_triwulan, dependen_var, independen_vars)
    
    # NEW: Tambahkan logging untuk melihat isi reg_data_list
    #cat("--- DEBUG: Hasil get_regression_data_func ---\n")
    #print(str(reg_data_list)) # Print struktur data untuk debugging
    #cat("--------------------------------------------\n")
    
    # Penanganan Status Khusus dari model.R
    if (!is.null(reg_data_list) && !is.null(reg_data_list$status)) {
      status_message <- switch(reg_data_list$status,
                               "input_not_selected" = "Silakan pilih Tahun, Triwulan, Variabel Dependen, dan Variabel Independen.",
                               "no_geom_data" = "Data geometri kecamatan tidak tersedia dari database (tb_jember).",
                               "no_valid_spatial_data" = "Tidak ada data spasial yang valid setelah penggabungan data.",
                               "no_data_for_regression" = paste0("Tidak ada data kasus DBD atau data lainnya yang tersedia untuk Tahun ", reg_data_list$year, " Triwulan ", reg_data_list$triwulan, " untuk analisis regresi. Silakan pilih kombinasi tahun/triwulan lain."),
                               "low_variability" = paste0("Variabilitas variabel '", reg_data_list$variable, "' sangat rendah atau nol untuk Tahun ", reg_data_list$year, " Triwulan ", reg_data_list$triwulan, ". Regresi tidak dapat dilakukan karena semua nilai sama atau tidak valid."),
                               "not_enough_locations_for_regression" = "Jumlah observasi terlalu sedikit (< 5) untuk analisis regresi spasial. Diperlukan minimal 5 lokasi.",
                               "split_failed" = paste0("Pembagian data latih/uji gagal: ", reg_data_list$message),
                               "nb_train_failed" = paste0("Gagal membuat objek tetangga KNN (data latih): ", reg_data_list$message),
                               "nb_train_invalid" = paste0("Objek tetangga (nb) data latih tidak valid: ", reg_data_list$message),
                               "no_neighbors_train" = "Tidak ada tetangga yang ditemukan untuk data latih. Analisis regresi spasial tidak dapat dilakukan.",
                               "listw_train_failed" = paste0("Gagal membuat matriks bobot spasial (listw) data latih: ", reg_data_list$message),
                               "listw_train_invalid" = "Matriks bobot spasial (listw) data latih tidak valid.",
                               "nb_test_failed" = paste0("Gagal membuat objek tetangga KNN (data uji): ", reg_data_list$message),
                               "nb_test_invalid" = paste0("Objek tetangga (nb) data uji tidak valid: ", reg_data_list$message),
                               "listw_test_failed" = paste0("Gagal membuat matriks bobot spasial (listw) data uji: ", reg_data_list$message),
                               "listw_test_invalid" = "Matriks bobot spasial (listw) data uji tidak valid.",
                               "missing_vars" = paste0("Variabel yang dipilih tidak ditemukan dalam data: ", reg_data_list$message),
                               "missing_vars_in_train" = "Variabel yang dipilih tidak ditemukan di data latih.",
                               "general_error" = paste0("Terjadi kesalahan umum: ", reg_data_list$message),
                               "Data tidak tersedia atau analisis gagal. Silakan periksa pilihan Anda."
      )
      cat(wrap_text_plain(status_message, width = 80))
      return(NULL)
    }
    
    # NEW: Jika reg_data_list adalah NULL tanpa status, mungkin ada kegagalan tak terduga
    if (is.null(reg_data_list)) {
      cat(wrap_text_plain("Gagal mendapatkan data regresi. Ada kesalahan tak terduga dalam proses pengambilan atau persiapan data.", width = 80))
      return(NULL)
    }
    
    # Pastikan data dan bobot tersedia
    if (is.null(reg_data_list$data_train) || !inherits(reg_data_list$data_train, "sf") ||
        is.null(reg_data_list$lw_train) || !inherits(reg_data_list$lw_train, "listw") ||
        is.null(reg_data_list$data_test) || !inherits(reg_data_list$data_test, "sf") ||
        is.null(reg_data_list$lw_test) || !inherits(reg_data_list$lw_test, "listw")) {
      cat(wrap_text_plain("Data latih/uji atau matriks bobot spasial tidak valid setelah persiapan data. Analisis regresi tidak dapat dilanjutkan.", width = 80))
      return(NULL)
    }
    
    
    data_train <- reg_data_list$data_train
    lw_train <- reg_data_list$lw_train
    data_test <- reg_data_list$data_test
    lw_test <- reg_data_list$lw_test
    
    # Formulir Regresi
    formula_str <- paste(dependen_var, "~", paste(independen_vars, collapse = " + "))
    # formula_obj <- as.formula(formula_str) # formula_obj tidak lagi digunakan langsung dalam model call
    
    results <- list()
    
    # Model OLS (sebagai dasar perbandingan)
    cat("--------------------------------------------")
    cat("\n--- Model Regresi Linier Berganda (OLS) ---\n")
    cat("--------------------------------------------\n")
    model_ols <- tryCatch({
      lm(as.formula(formula_str), data = data_train) # FIX: Gunakan as.formula(formula_str) langsung
    }, error = function(e) {
      cat("Error saat menjalankan model OLS:", e$message, "\n")
      # NEW: Tambahkan detail error spesifik jika model tidak dapat diestimasi
      if (grepl("singularities", e$message) || grepl("rank-deficient", e$message)) {
        cat("Ini seringkali disebabkan oleh variabel independen yang sangat berkorelasi (multikolinearitas tinggi),\n")
        cat("atau salah satu variabel independen tidak bervariasi (misalnya, semua nilainya sama atau nol),\n")
        cat("atau jumlah observasi terlalu sedikit dibandingkan jumlah prediktor.\n")
      }
      NULL
    })
    
    if (!is.null(model_ols)) {
      print(summary(model_ols))
      cat("\nAIC (Akaike Information Criterion): ", AIC(model_ols), "\n") 
      results$ols <- model_ols
      
      # Uji Lagrange Multiplier untuk autokorelasi spasial
      cat("--------------------------------------------")
      cat("\n--- Uji Lagrange Multiplier untuk Autokorelasi Spasial ---\n")
      cat("--------------------------------------------\n")
      lm_test <- tryCatch({
        # Periksa apakah residual OLS memiliki variabilitas
        if (sd(residuals(model_ols), na.rm = TRUE) == 0) {
          cat("Residual model OLS tidak bervariasi. Uji Lagrange Multiplier tidak dapat dilakukan.\n")
          NULL
        } else {
          spdep::lm.morantest(model_ols, lw_train, zero.policy = TRUE)
        }
      }, error = function(e) {
        cat("Error saat menjalankan Moran's I on residuals (LM Test):", e$message, "\n")
        NULL
      })
      
      if (!is.null(lm_test)) {
        print(lm_test)
        results$lm_test <- lm_test
        cat("\nInterpretasi LM Test:\n")
        cat("Jika p-value untuk 'Moran I statistic' signifikan (< 0.05),\n")
        cat("maka ada bukti autokorelasi spasial pada residual.\n")
        cat("Ini mengindikasikan bahwa model OLS mungkin tidak memadai dan model spasial (SLM/SEM) lebih cocok.\n")
      }
    } else {
      cat("Model OLS gagal dijalankan atau tidak dapat diestimasi. Silakan periksa kembali pilihan variabel dan data Anda.\n")
    }
    
    # Spatial Lag Model (SLM)
    cat("--------------------------------------------")
    cat("\n--- Spatial Lag Model (SLM) ---\n")
    cat("--------------------------------------------\n")
    model_slm <- tryCatch({
      spatialreg::lagsarlm(as.formula(formula_str), data = data_train, listw = lw_train, zero.policy = TRUE, tol.solve = 1e-15) # FIX: Gunakan as.formula(formula_str)
    }, error = function(e) {
      cat("Error saat menjalankan Spatial Lag Model (SLM):", e$message, "\n")
      if (grepl("singularity", e$message) || grepl("rank-deficient", e$message)) {
        cat("Masalah singularitas/rank-deficient juga terjadi pada SLM.\n")
      }
      NULL
    })
    
    if (!is.null(model_slm)) {
      print(summary(model_slm))
      results$slm <- model_slm
    } else {
      cat("Model SLM gagal dijalankan atau tidak dapat diestimasi.\n")
    }
    
    # Spatial Error Model (SEM)
    cat("--------------------------------------------")
    cat("\n--- Spatial Error Model (SEM) ---\n")
    cat("--------------------------------------------\n")
    model_sem <- tryCatch({
      spatialreg::errorsarlm(as.formula(formula_str), data = data_train, listw = lw_train, zero.policy = TRUE, tol.solve = 1e-15) # FIX: Gunakan as.formula(formula_str)
    }, error = function(e) {
      cat("Error saat menjalankan Spatial Error Model (SEM):", e$message, "\n")
      if (grepl("singularity", e$message) || grepl("rank-deficient", e$message)) {
        cat("Masalah singularitas/rank-deficient juga terjadi pada SEM.\n")
      }
      NULL
    })
    
    if (!is.null(model_sem)) {
      print(summary(model_sem))
      results$sem <- model_sem
    } else {
      cat("Model SEM gagal dijalankan atau tidak dapat diestimasi.\n")
    }
    
    # Store results for dynamic info output
    # Ensure this is called with the full list of results
    regression_results_rv(list(
      models = results,
      data_train = data_train, # Store data_train as well for consistency
      data_test = data_test,
      lw_train = lw_train,
      lw_test = lw_test,
      dependen_var = dependen_var,
      independen_vars = independen_vars
    ))
  }) # END output$regression_output
}