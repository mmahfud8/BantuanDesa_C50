library(shiny)
library(bslib)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("paper"),
  titlePanel("Bantuan Desa C50"),
  sidebarLayout(
    sidebarPanel(
      tags$h5("Predict Single Data"),
      # Select variable for Jenis Kelamin
      selectInput(inputId = "Jenis_Kelamin",
                  label = "Jenis Kelamin",
                  choices = c('Laki-Laki', 'Perempuan'),
                  selected = 'Laki-Laki'),
      
      # Select variable for Usia
      selectInput(inputId = "Usia",
                  label = "Usia",
                  choices = c('0-24', '25-49', '50-74', '75-99'),
                  selected = '0-24'),
      
      # Select variable for Anggota Keluarga
      selectInput(inputId = "Anggota_Keluarga",
                  label = "Anggota Keluarga",
                  choices = c('1', '2', '3', '4', '5', '6+'),
                  selected = '1'),
      
      # Select variable for Pendidikan Terakhir
      selectInput(inputId = "Pendidikan_Terakhir",
                  label = "Pendidikan Terakhir",
                  choices = c('Tidak Sekolah', 'SD', 'SMP', 'SMA', 'D3', 'S1'),
                  selected = 'Tidak Sekolah'),
      
      # Select variable for Pekerjaan
      selectInput(inputId = "Pekerjaan",
                  label = "Pekerjaan",
                  choices = c('Tidak Bekerja', 'Buruh Harian Lepas','Petani', 'Karyawan Swasta', 'Satpam', 'Sopir', 'Mekanik/Teknisi', 'Penjahit', 'Guru', 'TNI', 'Polisi','Perawat', 'PNS','Pensiunan', 'TKI', 'Tukang Bangunan', 'Wirausaha', 'Pedagang', 'Pedagang Kecil'),
                  selected = 'Tidak Bekerja'),
      
      # Select variable for Penghasilan
      selectInput(inputId = "Penghasilan",
                  label = "Penghasilan",
                  choices = c('0-499.999', '500.000-999.999', '1.000.000-1.499.999', '1.500.000-1.999.999', '2.000.000-2.499.999', '2.500.000-2.999.999', '3.000.000-3.499.999', '3.500.000-3.999.999', '4.000.000-4.499.999', '4.500.000-4.999.999', '5.000.000+'),
                  selected = '0-499.999'),
      
      # Select variable for Kepemilikan Rumah
      selectInput(inputId = "Kepemilikan_Rumah",
                  label = "Kepemilikan Rumah",
                  choices = c('Milik Sendiri', 'Milik Keluarga', 'Milik Orang tua', 'Sewa/Kontrak', 'Pinjam Pakai'),
                  selected = 'Milik Sendiri'),
      
      # Select variable for Dinding
      selectInput(inputId = "Dinding",
                  label = "Dinding",
                  choices = c('Kayu', 'Tembok'),
                  selected = "Kayu"),
      
      # Select variable for Lantai
      selectInput(inputId = "Lantai",
                  label = "Lantai",
                  choices = c('Tanah', 'Semen', 'Keramik'),
                  selected = 'Tanah'),
      
      # Select variable for Aset Tanah
      selectInput(inputId = "Aset_Tanah",
                  label = "Aset Tanah (ha)",
                  choices = c('0-0.1', '0.1-0.2','0.2-0.3', '0.3-0.4', '0.4-0.5', '0.5-0.6', '0.6-0.7', '0.7-0.8', '0.8-0.9', '0.9-1.0', '1.0-5.0'),
                  selected = '0-0.1'),
      
      # Select variable for Aset Lainnya
      selectInput(inputId = "Aset_Lainnya",
                  label = "Aset Lainnya",
                  choices = c('Tidak Ada', 'Elektronik', 'Sepeda Motor', 'Mobil', 'Sepeda Motor dan Mobil'),
                  selected = 'Tidak Ada'),
      
      # Select variable for Perilaku Hidup Bersih
      selectInput(inputId = "Perilaku_Hidup_Bersih",
                  label = "Perilaku Hidup Bersih",
                  choices = c( 'Memiliki WC', 'Memiliki WC Darurat', 'Mengunakan MCK Umum'),
                  selected = 'Memiliki WC'),
      tags$br(),
      tags$h5("Predict Multiple Data"),
      fileInput("file1", "Pilih File CSV atau Excel", accept = c(".csv", ".xlsx"))
    ),
    mainPanel(
      # Add other UI components or data visualization here
      card(
        tags$h4("Predict Single Data"),
        tableOutput("user_data_table"),
        tags$h5(textOutput("prediction_result"))),
      
      card(
        tags$br(),
        uiOutput("contents"))
    )
  )
)


server <- function(input, output) {
  # Load libraries and data outside of the reactive context
  library(C50)
  library(openxlsx)
  
  # Ensure the dataset is read correctly and is available
  df <- read.xlsx("dataset_penelitian.xlsx")
  
  # Define the model building function
  build_model <- function(df) {
    input_columns <- c("Jenis_Kelamin", "Usia", "Anggota_Keluarga", 
                       "Pendidikan_Terakhir", "Pekerjaan", "Penghasilan", 
                       "Kepemilikan_Rumah", "Dinding", "Lantai", "Aset_Tanah", "Aset_Lainnya", "Perilaku_Hidup_Bersih")
    data_training <- df[, input_columns]
    df$Bantuan_Desa <- as.factor(df$Bantuan_Desa)
    
    control_ops <- C5.0Control(subset = TRUE,
                               noGlobalPruning = TRUE,
                               CF = 0.5,
                               minCases = 1,
                               label = "Bantuan_Desa")
    
    # Build the C5.0 model
    model_bantuan_desa <- C5.0(
      x = data_training, 
      y = df$Bantuan_Desa,
      control = control_ops,
      trials = 1
    )
    return(model_bantuan_desa)
  }
  
  # Call the model building function and pass the dataset
  model_bantuan_desa <- build_model(df)
  
  # Reactive expression to create a data frame based on user inputs
  user_data <- reactive({
    data.frame(
      Jenis_Kelamin = input$Jenis_Kelamin,
      Usia = input$Usia,
      Anggota_Keluarga = input$Anggota_Keluarga,
      Pendidikan_Terakhir = input$Pendidikan_Terakhir,
      Pekerjaan = input$Pekerjaan,
      Penghasilan = input$Penghasilan,
      Kepemilikan_Rumah = input$Kepemilikan_Rumah,
      Dinding = input$Dinding,
      Lantai = input$Lantai,
      Aset_Tanah = input$Aset_Tanah,
      Aset_Lainnya = input$Aset_Lainnya,
      Perilaku_Hidup_Bersih = input$Perilaku_Hidup_Bersih
    )
  })
  
  # Display the data frame in a table format
  output$user_data_table <- renderTable({
    user_data()
  })
  
  # Use a reactive expression to handle predictions
  predictions <- reactive({
    predict(model_bantuan_desa, newdata = user_data())
  })
  
  # Display prediction result
  output$prediction_result <- renderText({
    paste("Hasil Prediksi: ", predictions())
  })
  
  # read file 
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if (grepl(".csv$", inFile$name)) {
      read.csv(inFile$datapath, sep = ",")
    } else if (grepl(".xlsx$", inFile$name)) {
      read.xlsx(inFile$datapath, sheet = 1)
    }
  })
  
  predictionstable <- reactive({
    if (is.null(data())) {
      return(NULL)  # Return NULL if data is NULL to avoid prediction errors
    }
    predict(model_bantuan_desa, newdata = data())
  })
  
  output$contents <- renderUI({
    if (is.null(predictionstable())) {
      return(NULL)  # Return NULL if predictions are NULL to avoid rendering errors
    }
    
    # Menggunakan tagList untuk menggabungkan header dan tabel
    tagList(
      tags$h4("Predict Multiple Data"),  # Menambahkan header
      tableOutput("table"),              # Menampilkan output tabel
      tags$h5("Simpan Hasil Prediksi"),
      downloadButton("downloadData", "Simpan"),
    )
  })
  
  # Membuat output tabel terpisah untuk data
  output$table <- renderTable({
    data.frame(data(), hasil_prediksi = predictionstable())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("predictions-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Ambil data untuk diunduh
      predictions <- data.frame(data(), hasil_prediksi = predictionstable())
      
      # Pastikan ada data untuk diunduh
      if (!is.null(predictions)) {
        # Simpan sebagai CSV
        write.csv(predictions, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)