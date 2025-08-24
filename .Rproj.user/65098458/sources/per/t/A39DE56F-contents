# conn.R

# ------------------------------------------------------------------
# PENGATURAN KONEKSI DATABASE
# ------------------------------------------------------------------

# Pengaturan Koneksi Database PostgreSQL
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