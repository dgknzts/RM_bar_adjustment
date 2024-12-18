combine_html_files <- function(input_folder, output_file) {
  # Birleştirilmiş dosyanın tam yolu
  combined_html <- file.path(input_folder, output_file)
  
  # Daha önce var olan birleştirilmiş dosyayı sil
  if (file.exists(combined_html)) {
    file.remove(combined_html)
  }
  
  # HTML dosyalarını listele
  html_files <- list.files(input_folder, pattern = "\\.html$", full.names = TRUE)
  
  # Ana HTML dosyasını başlat
  cat("<html><head><title>Combined HTML Files</title></head><body>\n", 
      file = combined_html)
  
  # HTML dosyalarını sırayla birleştir
  for (file in html_files) {
    cat("<h2 style='margin-top: 30px; font-family: Arial, sans-serif;'>", 
        gsub(".html", "", basename(file)), 
        "</h2>\n", file = combined_html, append = TRUE)  # Başlık ekle
    cat("<div style='margin-left: 20px;'>\n", file = combined_html, append = TRUE)
    cat(readLines(file), sep = "\n", file = combined_html, append = TRUE)       # İçeriği ekle
    cat("</div>\n", file = combined_html, append = TRUE)  # İçeriği kapat
  }
  
  # Ana HTML dosyasını kapat
  cat("</body></html>", file = combined_html, append = TRUE)
  
  # Tekil dosyaları sil (birleştirilmiş dosyayı hariç tut)
  file.remove(html_files[html_files != combined_html])
  
  # Bilgi mesajı
  cat("Combined HTML file created successfully: ", combined_html, "\n")
}
