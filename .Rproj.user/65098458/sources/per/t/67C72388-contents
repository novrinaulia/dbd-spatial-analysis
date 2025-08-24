# model.R

# ------------------------------------------------------------------
# LOGIKA PENGAMBILAN DATA (REACTIVE VALUES)
# ------------------------------------------------------------------
# Reactive value untuk menyimpan tahun sistem saat ini (bukan lagi hardcode)
system_current_year_rv <- reactiveVal(NULL)
observeEvent(TRUE, { # Hanya dijalankan sekali saat aplikasi dimulai
  system_current_year_rv(as.numeric(format(Sys.Date(), "%Y")))
}, once = TRUE)

# NEW: Fungsi bantu untuk membuat matriks bobot KNN secara manual
# Tempatkan fungsi ini di sini, sebelum fungsi reactive lainnya memanggilnya.
create_manual_knn_nb <- function(sf_data, k_neighbors = 2, id_col = "id_region") {
  if (nrow(sf_data) < 2) {
    stop("Diperlukan setidaknya 2 lokasi untuk membuat objek tetangga spasial.")
  }
  
  # Pastikan geometri adalah tipe POINT untuk perhitungan jarak centroid-to-centroid
  # Jika bukan, ubah ke centroid
  if (!all(sf::st_geometry_type(sf_data) %in% c("POINT", "MULTIPOINT"))) {
    sf_data_centroids <- tryCatch({
      sf::st_centroid(sf_data)
    }, error = function(e) {
      return(list(status = "invalid_geometry_type", message = paste("Gagal menghitung centroid untuk data spasial:", e$message)))
    })
    if (inherits(sf_data_centroids, "list")) return(sf_data_centroids)
    sf_data <- sf_data_centroids
  }
  
  # Periksa CRS dan proyeksikan jika belum dalam CRS proyeksian
  if (is.na(sf::st_crs(sf_data)$epsg)) {
    warning("CRS tidak terdefinisi. Mencoba mengasumsikan WGS84 dan memproyeksikan ke EPSG:3857.")
    sf_data <- tryCatch({
      sf::st_set_crs(sf_data, 4326) %>% sf::st_transform(3857) # WGS84 ke Web Mercator
    }, error = function(e) {
      return(list(status = "reprojection_failed", message = paste("Gagal memproyeksikan data spasial:", e$message)))
    })
  } else if (sf::st_crs(sf_data)$units_gdal != "metre") {
    warning("CRS bukan dalam meter, memproyeksikan ke EPSG:3857.")
    sf_data <- tryCatch({
      sf::st_transform(sf_data, 3857) # Proyeksikan ke Web Mercator (meter)
    }, error = function(e) {
      return(list(status = "reprojection_failed", message = paste("Gagal memproyeksikan data spasial:", e$message)))
    })
  }
  
  # Ekstrak koordinat
  coords <- sf::st_coordinates(sf_data)
  if (is.null(coords) || nrow(coords) == 0 || any(is.na(coords))) {
    return(list(status = "empty_projected_coords", message = "Koordinat terproyeksi kosong atau mengandung NA setelah pemrosesan."))
  }
  
  # Konversi sf ke sp untuk fungsi spdep
  sp_obj <- tryCatch({
    as(sf_data, "Spatial")
  }, error = function(e) {
    return(list(status = "sf_to_sp_conversion_failed", message = paste("Gagal mengonversi objek sf ke sp:", e$message)))
  })
  
  if (is.null(sp_obj)) {
    return(list(status = "sp_object_null", message = "Objek Spatial (sp) adalah null setelah konversi."))
  }
  if (any(is.na(sp_obj@coords))) {
    return(list(status = "sp_coords_na", message = "Koordinat Spatial (sp) mengandung nilai NA setelah konversi."))
  }
  
  # Pastikan k tidak lebih besar dari jumlah observasi-1
  k_actual <- min(k_neighbors, nrow(sp_obj) - 1)
  if (k_actual < 1) {
    return(list(status = "invalid_k_val", message = paste("Nilai k (jumlah tetangga) tidak valid:", k_actual)))
  }
  
  nb_obj <- tryCatch({
    knearneigh(sp_obj, k = k_actual)$nn
  }, error = function(e) {
    return(list(status = "manual_nb_failed", message = paste("Gagal membuat objek tetangga KNN secara manual:", e$message)))
  })
  
  if (is.null(nb_obj)) {
    return(list(status = "manual_nb_failed", message = "Objek tetangga KNN null setelah knearneigh."))
  }
  
  # Konversi matriks indeks tetangga menjadi list of neighbours
  nb_list <- lapply(1:nrow(nb_obj), function(i) {
    as.integer(nb_obj[i, ]) # Pastikan indeks adalah integer
  })
  class(nb_list) <- "nb"
  attr(nb_list, "region.id") <- as.character(sf_data[[id_col]])
  attr(nb_list, "call") <- match.call()
  attr(nb_list, "type") <- "k-nearest neighbours"
  attr(nb_list, "nb_type") <- "manual"
  
  # Periksa apakah ada tetangga yang kosong setelah pembuatan
  if (any(sapply(nb_list, length) == 0)) {
    warning("Beberapa lokasi tidak memiliki tetangga setelah pembuatan KNN. Menambahkan penanganan.")
  }
  
  return(nb_list)
}

# Ambil Daftar Lengkap Nama Kecamatan untuk Default X-axis (Plot Tren Kecamatan)
all_kecamatan_names <- reactiveVal(NULL)
observeEvent(TRUE, {
  con <- NULL
  tryCatch({
    con <- connect_to_db()
    query_all_kecamatan <- "SELECT DISTINCT kecamatan FROM tb_jember ORDER BY kecamatan;"
    kecamatan_list <- DBI::dbGetQuery(con, query_all_kecamatan)$kecamatan
    all_kecamatan_names(kecamatan_list)
  }, error = function(e) {
    showNotification(
      paste("Error mengambil daftar semua kecamatan dari database:", e$message),
      type = "error",
      duration = NULL
    )
    all_kecamatan_names(NULL)
  }, finally = {
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })
}, once = TRUE)

# Ambil Data Geometri Kecamatan dari tabel tb_jember
data_kecamatan_jember <- reactive({
  con <- NULL
  tryCatch({
    con <- connect_to_db()
    query_geom <- "
      SELECT
        id_region,
        geom,
        kecamatan
      FROM
        tb_jember;
    "
    data <- sf::st_read(con, query = query_geom)
    return(data)
  }, error = function(e) {
    showNotification(
      paste("Error mengambil data geometri dari database:", e$message,
            "Pastikan nama tabel 'tb_jember' dan kolom 'id', 'kecamatan', 'geometry' sudah benar di database, serta kolom 'geometry' bertipe geometri/geografi PostGIS."),
      type = "error",
      duration = NULL
    )
    return(NULL)
  }, finally = {
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })
})

# Ambil Data Tren Kasus dan Kematian DBD Kabupaten
data_tren_dbd_kab <- reactive({
  get_data_tren_dbd_kab_func <- function(start_year, end_year) {
    if (is.null(start_year) || is.null(end_year) || start_year == "" || end_year == "") {
      return(NULL)
    }
    
    min_year <- as.numeric(start_year)
    max_year <- as.numeric(end_year)
    
    if (min_year > max_year) {
      return(NULL)
    }
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      
      query_tren <- sprintf("
        SELECT
          tahun,
          SUM(kasus) as total_kasus,
          SUM(mati) as total_kematian
        FROM
          tb_kasus
        WHERE
          tahun BETWEEN %s AND %s
        GROUP BY
          tahun
        ORDER BY
          tahun;
      ", min_year, max_year)
      
      data <- DBI::dbGetQuery(con, query_tren)
      
      data$total_kasus <- as.numeric(data$total_kasus)
      data$total_kematian <- as.numeric(data$total_kematian)
      
      return(data)
    }, error = function(e) {
      showNotification(
        paste("Error mengambil data tren kabupaten dari database:", e$message,
              "Pastikan tabel 'tb_kasus' dan kolom 'tahun', 'kasus', 'mati' sudah benar."),
        type = "error",
        duration = NULL
      )
      return(NULL)
    }, finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
    })
  }
  
  return(get_data_tren_dbd_kab_func)
})

# Ambil Data Tren Kasus dan Kematian DBD Kecamatan
data_tren_dbd_kec_single_year <- reactive({
  get_data_tren_dbd_kec_func <- function(selected_year, selected_triwulan) {
    if (is.null(selected_year) || is.null(selected_triwulan) || selected_year == "" || selected_triwulan == "") {
      return(NULL)
    }
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      
      where_clause <- paste0("t.tahun = ", selected_year)
      if (selected_triwulan != "") {
        where_clause <- paste0(where_clause, " AND t.triwulan = ", selected_triwulan)
      }
      
      query_tren_kec <- sprintf("
        SELECT
          t.tahun,
          t.triwulan,
          j.kecamatan,
          SUM(t.kasus) as total_kasus,
          SUM(t.mati) as total_kematian
        FROM
          tb_kasus t
        JOIN
          tb_jember j ON t.id_region = j.id_region
        WHERE
          %s
        GROUP BY
          t.tahun, t.triwulan, j.kecamatan
        ORDER BY
          j.kecamatan, t.triwulan;
      ", where_clause)
      
      data <- DBI::dbGetQuery(con, query_tren_kec)
      
      data$total_kasus <- as.numeric(data$total_kasus)
      data$total_kematian <- as.numeric(data$total_kematian)
      
      data <- data %>%
        mutate(
          triwulan_label = paste0("Triwulan ", triwulan),
          triwulan_label = factor(triwulan_label, levels = c("Triwulan 1", "Triwulan 2", "Triwulan 3", "Triwulan 4"))
        )
      
      return(data)
    }, error = function(e) {
      showNotification(
        paste("Error mengambil data tren kecamatan dari database:", e$message,
              "Pastikan tabel 'tb_kasus' dan 'tb_jember' sudah benar, serta kolom 'id_region', 'tahun', 'triwulan', 'kasus', 'mati', 'kecamatan' ada."),
        type = "error",
        duration = NULL
      )
      return(NULL)
    }, finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
    })
  }
  
  return(get_data_tren_dbd_kec_func)
})

# Ambil Data Kasus dan Curah Hujan untuk Analisis Moran
get_data_kasus_moran_func <- reactive({
  get_data_moran <- function(selected_year, selected_triwulan) {
    if (is.null(selected_year) || is.null(selected_triwulan) || selected_year == "" || selected_triwulan == "") {
      return(list(status = "input_not_selected"))
    }
    
    con <- NULL
    tryCatch({
      con <- connect_to_db()
      
      # 1. Query Data Sumber Mentah dari Database
      query_kasus <- sprintf("
        SELECT
          t.id_region,
          SUM(t.kasus) as total_kasus,
          SUM(t.abj) as total_abj
        FROM
          tb_kasus t
        WHERE
          t.tahun = %s AND t.triwulan = %s
        GROUP BY
          t.id_region;
      ", selected_year, selected_triwulan)
      data_kasus_abj <- DBI::dbGetQuery(con, query_kasus)
      
      query_klimatologi <- sprintf("
        SELECT
          t.id_region,
          SUM(t.curah_hujan) as total_curah_hujan,
          AVG(t.suhu) as avg_suhu,
          AVG(t.kelembaban) as avg_kelembaban
        FROM
          tb_klimatologi t
        WHERE
          t.tahun = %s AND t.triwulan = %s
        GROUP BY
          t.id_region;
      ", selected_year, selected_triwulan)
      data_klimatologi <- DBI::dbGetQuery(con, query_klimatologi)
      
      query_penduduk <- sprintf("
        SELECT
          id_region,
          SUM(penduduk) as total_penduduk
        FROM
          tb_penduduk
        WHERE
          tahun = %s
        GROUP BY
          id_region;
      ", selected_year)
      data_penduduk <- DBI::dbGetQuery(con, query_penduduk)
      
      query_geom <- "
        SELECT
          id_region,
          kecamatan,
          geom
        FROM
          tb_jember;
      "
      data_geom_sf <- sf::st_read(con, query = query_geom)
      
      if (is.null(data_geom_sf) || nrow(data_geom_sf) == 0) {
        showNotification("Tidak ada data geometri kecamatan dari database (tb_jember). Pastikan tabel 'tb_jember' memiliki data.", type = "error", duration = NULL)
        return(list(status = "no_geom_data"))
      }
      
      # 2. Gabungkan Semua Data
      data_final <- data_geom_sf %>%
        left_join(data_kasus_abj, by = "id_region") %>%
        left_join(data_klimatologi, by = "id_region") %>%
        left_join(data_penduduk, by = "id_region")
      
      # 3. Tangani NA dan Konversi Tipe Data
      data_final <- data_final %>%
        mutate(
          total_kasus = as.numeric(total_kasus),
          total_abj = as.numeric(total_abj),
          total_penduduk = as.numeric(total_penduduk)
        ) %>%
        replace_na(list(
          total_kasus = 0,
          total_abj = 0,
          total_curah_hujan = 0,
          avg_suhu = 0,
          avg_kelembaban = 0,
          total_penduduk = 0
        )) %>%
        mutate(
          total_kasus = as.integer(total_kasus),
          total_abj = as.integer(total_abj),
          total_penduduk = as.integer(total_penduduk)
        )
      
      # 4. Transformasi Log(x+1) untuk kasus dan curah hujan
      data_final <- data_final %>%
        mutate(
          log_kasus = log(total_kasus + 1),
          log_curah_hujan = log(total_curah_hujan + 1)
        )
      
      if (!inherits(data_final, "sf") || nrow(data_final) == 0) {
        showNotification("Tidak ada data spasial yang valid setelah penggabungan data.", type = "error", duration = NULL)
        return(list(status = "no_valid_spatial_data"))
      }
      
      initial_rows <- nrow(data_final)
      data_final <- data_final %>% dplyr::filter(!sf::st_is_empty(geom))
      
      if (nrow(data_final) < initial_rows) {
        warning(paste0("Terdapat ", initial_rows - nrow(data_final), " kecamatan dengan geometri kosong yang dihapus."))
      }
      
      if (nrow(data_final) < 2) {
        showNotification("Jumlah kecamatan terlalu sedikit (< 2) setelah pembersihan data untuk analisis Moran's I. Diperlukan minimal 2 kecamatan dengan geometri valid dan data.", type = "warning", duration = NULL)
        return(list(status = "not_enough_locations"))
      }
      
      # Perbaikan: Tambahkan pemeriksaan untuk kasus DBD nol total sebelum pemeriksaan SD
      if (all(data_final$total_kasus == 0, na.rm = TRUE) && nrow(data_final) > 0) {
        return(list(status = "no_case_data_for_moran", year = selected_year, triwulan = selected_triwulan))
      }
      
      sd_log_kasus <- sd(data_final$log_kasus, na.rm = TRUE)
      if (is.na(sd_log_kasus) || is.nan(sd_log_kasus) || sd_log_kasus == 0) {
        return(list(status = "low_variability", year = selected_year, triwulan = selected_triwulan))
      }
      
      # Panggil fungsi create_manual_knn_nb untuk membuat objek tetangga
      nb <- tryCatch({
        create_manual_knn_nb(data_final, min(5, nrow(data_final) - 1), "id_region")
      }, error = function(e) {
        showNotification(paste("Gagal membuat objek tetangga KNN (dalam Moran):", e$message), type = "error", duration = NULL)
        return(list(status = "manual_nb_failed_moran", message = e$message))
      })
      
      if (is.null(nb) || (is.list(nb) && !is.null(nb$status))) {
        return(nb)
      }
      
      
      if (is.null(nb) || !inherits(nb, "nb")) {
        showNotification("Pembuatan objek tetangga (nb) gagal atau menghasilkan objek tidak valid.",
                         type = "error", duration = NULL)
        return(list(status = "nb_invalid_after_construction"))
      }
      
      # Pemeriksaan lanjutan untuk objek nb yang valid
      if (length(nb) == 0) {
        showNotification("Objek tetangga (nb) kosong. Tidak ada tetangga yang terdeteksi untuk analisis spasial.",
                         type = "warning", duration = NULL)
        return(list(status = "nb_empty"))
      }
      if (all(sapply(nb, length) == 0)) {
        showNotification("Tidak ada tetangga yang ditemukan untuk titik data. Periksa sebaran spasial data Anda atau metode pembobot spasial. Pastikan setiap titik memiliki setidaknya satu tetangga.",
                         type = "warning", duration = NULL)
        return(list(status = "no_neighbors_found"))
      }
      
      lw <- tryCatch({
        spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
      }, error = function(e) {
        showNotification(paste("Gagal membuat matriks bobot spasial (listw) dari objek tetangga:",
                               e$message), type = "error", duration = NULL)
        return(list(status = "listw_creation_failed"))
      })
      
      if (is.null(lw) || !inherits(lw, "listw")) {
        showNotification("Pembuatan matriks bobot spasial (listw) gagal atau menghasilkan objek tidak valid.",
                         type = "error", duration = NULL)
        return(list(status = "listw_invalid"))
      }
      
      # --- START PERBAIKAN: Pemeriksaan bobot NA/kosong yang lebih robust ---
      has_na_weights <- FALSE
      has_empty_weights <- FALSE
      
      if (!is.null(lw$weights) && length(lw$weights) > 0) {
        na_check_results <- sapply(lw$weights, function(w) {
          if (is.null(w)) return(TRUE)
          any(is.na(w))
        })
        if (any(na_check_results, na.rm = TRUE)) {
          has_na_weights <- TRUE
        }
        
        empty_check_results <- sapply(lw$weights, function(w) {
          if (is.null(w)) return(TRUE)
          length(w) == 0
        })
        if (any(empty_check_results, na.rm = TRUE)) {
          has_empty_weights <- TRUE
        }
      } else {
        has_na_weights <- TRUE
        has_empty_weights <- TRUE
      }
      # --- END PERBAIKAN ---
      
      if (has_na_weights || has_empty_weights) {
        showNotification("Beberapa observasi memiliki bobot NA atau tidak ada tetangga dalam matriks bobot spasial. Analisis mungkin tidak akurat atau gagal.",
                         type = "warning", duration = NULL)
        return(list(status = "invalid_weights"))
      }
      
      return(list(data = data_final, lw = lw))
      
    }, error = function(e) {
      showNotification(
        paste("Error umum saat menyiapkan data untuk analisis Moran:", e$message,
              "Ini mungkin disebabkan oleh masalah koneksi database, data yang tidak lengkap, atau kesalahan pada struktur data spasial."),
        type = "error",
        duration = NULL
      )
      return(list(status = "general_error", message = e$message))
    }, finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
    })
  }
  
  return(get_data_moran)
})

#Ambil Data untuk Regresi Spaial
get_regression_data_func <- reactive({
  get_regression_data <- function(selected_year, selected_triwulan,
                                  dependen_var, independen_vars) {

    if (is.null(selected_year) || is.null(selected_triwulan) || selected_year == "" || selected_triwulan == "" ||
        is.null(dependen_var) || dependen_var == "" || is.null(independen_vars) || length(independen_vars) == 0) {
      return(list(status = "input_not_selected"))
    }

    con <- NULL
    tryCatch({
      con <- connect_to_db()

      # 1. Query Data Sumber Mentah dari Database (Sama seperti Moran)
      query_kasus <- sprintf("
        SELECT
          t.id_region,
          SUM(t.kasus) as total_kasus,
          SUM(t.abj) as total_abj
        FROM
          tb_kasus t
        WHERE
          t.tahun = %s AND t.triwulan = %s
        GROUP BY
          t.id_region;
      ", selected_year, selected_triwulan)
      data_kasus_abj <- DBI::dbGetQuery(con, query_kasus)

      query_klimatologi <- sprintf("
        SELECT
          t.id_region,
          SUM(t.curah_hujan) as total_curah_hujan,
          AVG(t.suhu) as avg_suhu,
          AVG(t.kelembaban) as avg_kelembaban
        FROM
          tb_klimatologi t
        WHERE
          t.tahun = %s AND t.triwulan = %s
        GROUP BY
          t.id_region;
      ", selected_year, selected_triwulan)
      data_klimatologi <- DBI::dbGetQuery(con, query_klimatologi)

      query_penduduk <- sprintf("
        SELECT
          id_region,
          SUM(penduduk) as total_penduduk
        FROM
          tb_penduduk
        WHERE
          tahun = %s
        GROUP BY
          id_region;
      ", selected_year)
      data_penduduk <- DBI::dbGetQuery(con, query_penduduk)

      query_geom <- "
        SELECT
          id_region,
          kecamatan,
          geom
        FROM
          tb_jember;
      "
      data_geom_sf <- sf::st_read(con, query = query_geom)

      if (is.null(data_geom_sf) || nrow(data_geom_sf) == 0) {
        showNotification("Tidak ada data geometri kecamatan dari database (tb_jember). Pastikan tabel 'tb_jember' memiliki data.", type = "error", duration = NULL)
        return(list(status = "no_geom_data"))
      }

      # 2. Gabungkan Semua Data
      data_final <- data_geom_sf %>%
        left_join(data_kasus_abj, by = "id_region") %>%
        left_join(data_klimatologi, by = "id_region") %>%
        left_join(data_penduduk, by = "id_region")

      # 3. Tangani NA dan Konversi Tipe Data
      data_final <- data_final %>%
        mutate(
          total_kasus = as.numeric(total_kasus),
          total_abj = as.numeric(total_abj),
          total_penduduk = as.numeric(total_penduduk)
        ) %>%
        replace_na(list(
          total_kasus = 0,
          total_abj = 0,
          total_curah_hujan = 0,
          avg_suhu = 0,
          avg_kelembaban = 0,
          total_penduduk = 0
        )) %>%
        mutate(
          total_kasus = as.integer(total_kasus),
          total_abj = as.integer(total_abj),
          total_penduduk = as.integer(total_penduduk)
        )

      # 4. Transformasi Log(x+1) untuk kasus dan curah hujan
      data_final <- data_final %>%
        mutate(
          log_kasus = log(total_kasus + 1),
          log_curah_hujan = log(total_curah_hujan + 1)
        )

      if (!inherits(data_final, "sf") || nrow(data_final) == 0) {
        showNotification("Tidak ada data spasial yang valid setelah penggabungan data.", type = "error", duration = NULL)
        return(list(status = "no_valid_spatial_data"))
      }

      initial_rows <- nrow(data_final)
      data_final <- data_final %>% dplyr::filter(!sf::st_is_empty(geom))

      if (nrow(data_final) < initial_rows) {
        warning(paste0("Terdapat ", initial_rows - nrow(data_final), " kecamatan dengan geometri kosong yang dihapus."))
      }

      # NEW: Pemeriksaan data yang lebih ketat untuk regresi
      if (nrow(data_final) == 0) {
        return(list(status = "no_data_for_regression", year = selected_year, triwulan = selected_triwulan, message = "Tidak ada data yang tersedia untuk tahun dan triwulan yang dipilih."))
      }

      # Periksa apakah variabel dependen dan independen yang dipilih memiliki variabilitas yang cukup
      # atau apakah semua nilainya NA/0
      all_selected_vars <- c(dependen_var, independen_vars)
      for (v in all_selected_vars) {
        if (!v %in% names(data_final)) {
          return(list(status = "missing_vars", message = paste0("Variabel '", v, "' tidak ditemukan dalam data.")))
        }
        
        # Periksa semua nilai NA/Inf
        if (all(is.na(data_final[[v]]) | !is.finite(data_final[[v]]))) {
          return(list(status = "low_variability", year = selected_year, triwulan = selected_triwulan, variable = v, message = paste0("Variabel '", v, "' hanya mengandung nilai NA atau Inf untuk tahun dan triwulan ini.")))
        }

        # Periksa variabilitas (setelah NA diabaikan)
        if (sd(data_final[[v]], na.rm = TRUE) == 0) {
          return(list(status = "low_variability", year = selected_year, triwulan = selected_triwulan, variable = v, message = paste0("Variabilitas variabel '", v, "' sangat rendah (semua nilai sama) untuk tahun dan triwulan ini. Regresi tidak dapat dilakukan.")))
        }
      }

      if (nrow(data_final) < 5) { # Minimal data untuk split dan regresi
        showNotification("Jumlah observasi terlalu sedikit (< 5) setelah pembersihan data untuk analisis regresi spasial.", type = "warning", duration = NULL)
        return(list(status = "not_enough_locations_for_regression"))
      }

      # NEW: Pembagian data latih dan uji (80:20)
      set.seed(123) # Untuk reproduktifitas
      train_indices <- sample(1:nrow(data_final), size = 0.8 * nrow(data_final))

      data_train_sf <- data_final[train_indices, ]
      data_test_sf <- data_final[-train_indices, ]

      # Tambahan pengecekan setelah split
      if (nrow(data_train_sf) < 2) {
        return(list(status = "split_failed", message = "Dataset pelatihan terlalu kecil (< 2 observasi) setelah pembagian data."))
      }
      if (nrow(data_test_sf) < 1) {
        return(list(status = "split_failed", message = "Dataset pengujian terlalu kecil (< 1 observasi) setelah pembagian data."))
      }

      # NEW: Buat matriks bobot spasial untuk data latih dan data uji secara terpisah
      # Menggunakan fungsi create_manual_knn_nb
      nb_train <- tryCatch({
        create_manual_knn_nb(data_train_sf, k_neighbors = 2, id_col = "id_region")
      }, error = function(e) {
        return(list(status = "nb_train_failed", message = paste("Gagal membuat objek tetangga KNN (data latih):", e$message)))
      })

      if (is.list(nb_train) && !is.null(nb_train$status)) {
        return(nb_train) # Propagate error status
      }
      if (is.null(nb_train) || !inherits(nb_train, "nb")) {
        showNotification("Pembuatan objek tetangga (nb) data latih gagal atau menghasilkan objek tidak valid.", type = "error", duration = NULL)
        return(list(status = "nb_train_invalid"))
      }
      if (all(sapply(nb_train, length) == 0)) {
        showNotification("Tidak ada tetangga yang ditemukan untuk data latih. Analisis regresi spasial tidak dapat dilakukan.", type = "warning", duration = NULL)
        return(list(status = "no_neighbors_train"))
      }
      # Periksa jika ada elemen NA di nb_train setelah pembuatan
      if (any(sapply(nb_train, is.na))) {
        return(list(status = "nb_train_invalid", message = "Objek tetangga data latih mengandung NA."))
      }


      lw_train <- tryCatch({
        spdep::nb2listw(nb_train, style = "W", zero.policy = TRUE)
      }, error = function(e) {
        showNotification(paste("Gagal membuat matriks bobot spasial (listw) dari objek tetangga data latih:", e$message), type = "error", duration = NULL)
        return(list(status = "listw_train_failed"))
      })

      if (is.null(lw_train) || !inherits(lw_train, "listw")) {
        showNotification("Pembuatan matriks bobot spasial (listw) data latih gagal atau menghasilkan objek tidak valid.", type = "error", duration = NULL)
        return(list(status = "listw_train_invalid"))
      }

      # Untuk data uji, kita juga perlu membuat matriks bobot.
      nb_test <- tryCatch({
        create_manual_knn_nb(data_test_sf, k_neighbors = 2, id_col = "id_region")
      }, error = function(e) {
        return(list(status = "nb_test_failed", message = paste("Gagal membuat objek tetangga KNN (data uji):", e$message)))
      })

      if (is.list(nb_test) && !is.null(nb_test$status)) {
        return(nb_test) # Propagate error status
      }
      if (is.null(nb_test) || !inherits(nb_test, "nb")) {
        showNotification("Pembuatan objek tetangga (nb) data uji gagal atau menghasilkan objek tidak valid.", type = "error", duration = NULL)
        return(list(status = "nb_test_invalid"))
      }
      if (all(sapply(nb_test, length) == 0)) {
        showNotification("Tidak ada tetangga yang ditemukan untuk data uji. Evaluasi model mungkin terbatas.", type = "warning", duration = NULL)
        # Jangan langsung return error, coba lanjutkan jika memungkinkan untuk training
      }
      # Periksa jika ada elemen NA di nb_test setelah pembuatan
      if (any(sapply(nb_test, is.na))) {
        return(list(status = "nb_test_invalid", message = "Objek tetangga data uji mengandung NA."))
      }

      lw_test <- tryCatch({
        spdep::nb2listw(nb_test, style = "W", zero.policy = TRUE)
      }, error = function(e) {
        showNotification(paste("Gagal membuat matriks bobot spasial (listw) dari objek tetangga data uji:", e$message), type = "error", duration = NULL)
        return(list(status = "listw_test_failed"))
      })

      if (is.null(lw_test) || !inherits(lw_test, "listw")) {
        showNotification("Pembuatan matriks bobot spasial (listw) data uji gagal atau menghasilkan objek tidak valid.", type = "error", duration = NULL)
        return(list(status = "listw_test_invalid"))
      }

      # Pastikan variabel yang dipilih ada di dataset
      if (!all(c(dependen_var, independen_vars) %in% names(data_train_sf))) {
        missing_vars <- setdiff(c(dependen_var, independen_vars), names(data_train_sf))
        showNotification(paste("Variabel tidak ditemukan di data latih:", paste(missing_vars, collapse = ", ")), type = "error", duration = NULL)
        return(list(status = "missing_vars_in_train"))
      }
      if (!all(c(dependen_var, independen_vars) %in% names(data_test_sf))) {
        missing_vars <- setdiff(c(dependen_var, independen_vars), names(data_test_sf))
        showNotification(paste("Variabel tidak ditemukan di data uji:", paste(missing_vars, collapse = ", ")), type = "warning", duration = NULL)
        # Lanjutkan, tapi ada warning
      }

      return(list(data_train = data_train_sf, lw_train = lw_train,
                  data_test = data_test_sf, lw_test = lw_test,
                  dependen_var = dependen_var, independen_vars = independen_vars))

    }, error = function(e) {
      showNotification(
        paste("Error umum saat menyiapkan data untuk regresi spasial:", e$message),
        type = "error",
        duration = NULL
      )
      return(list(status = "general_error", message = e$message))
    }, finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
    })
  }

  return(get_regression_data)
})


