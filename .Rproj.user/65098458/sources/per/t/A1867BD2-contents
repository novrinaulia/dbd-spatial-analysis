# footer.R

# ------------------------------------------------------------------
# LOGIKA OUTPUT UNTUK INFORMASI TEKS (FOOTER)
# ------------------------------------------------------------------

# Fungsi ini akan dipanggil di server utama app.R
call_all_info_text_outputs <- function(input, output, session) {
  
  output$infoTrenKasusKab <- renderUI({
    HTML("
      <p>Grafik ini menampilkan tren jumlah kasus DBD dan jumlah kematian akibat DBD di Kabupaten Jember secara keseluruhan dalam rentang tahun yang Anda pilih.</p>
      <ul>
        <li>Gunakan <b>dropdown 'Tahun Awal'</b> dan <b>'Tahun Akhir'</b> di atas untuk menyesuaikan rentang tahun yang ingin Anda analisis.</li>
        <li>Garis biru menunjukkan tren jumlah kasus DBD.</li>
        <li>Garis merah menunjukkan tren jumlah kematian DBD.</li>
      </ul>
      <p>Analisis tren ini dapat membantu mengidentifikasi pola musiman atau peningkatan/penurunan kasus DBD dari waktu ke waktu di tingkat kabupaten.</p>
    ")
  })
}