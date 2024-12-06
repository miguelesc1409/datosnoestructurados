# Instalar los paquetes necesarios si no están instalados
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")

library(shiny)
library(ggplot2)
library("siebanxicor")
library(plotly)
library(stringr)
library(rvest)
library(dplyr)
library(tidyr)
library(DT)
library(readxl)
library(mongolite)



# Conexión a MongoDB
mongo_conexionLC <- mongo(collection = "Gasto", 
                          db = "Gastos", 
                          url = "mongodb+srv://user:contrasena@cluster0.w6xph8m.mongodb.net/gastos?retryWrites=true&w=majority")


# Conexión a MongoDB
mongo_connectionPR <- mongo(
  collection = "Gasto",  # Colección principal
  db = "Gastos", 
  url = "mongodb+srv://user:contrasena@cluster0.w6xph8m.mongodb.net/gastos?retryWrites=true&w=majority"
)


# Conexión a MongoDB
mongo_connectionMZ <- mongo(
  collection = "Gasto", 
  db = "Gastos", 
  url = "mongodb+srv://user:contrasena@cluster0.w6xph8m.mongodb.net/gastos?retryWrites=true&w=majority"
)



csv_file_imc <- "mayolo/conjunto_de_datos_imcpmi_cp2023_05.csv"
# Ruta del archivo Excel
file_path <- "project1/Productos.xlsx"


# Función para cargar y procesar los datos
load_data <- function(file_path) {
  data <- read.csv(file_path, sep = ",", header = TRUE)
  
  data_long <- data %>%
    pivot_longer(
      cols = -Descriptores,  # Todas las columnas excepto 'Descriptores'
      names_to = "Periodo",
      values_to = "Valor"
    ) %>%
    separate(Periodo, into = c("Año", "Mes"), sep = "\\.", extra = "merge", fill = "right") %>%
    mutate(
      Año = as.numeric(gsub("X", "", Año)),  # Quitar el prefijo 'X' de los años
      Mes = factor(Mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")),
      Valor = as.numeric(Valor)
    ) %>%
    drop_na(Valor)  # Eliminar filas con valores faltantes
  
  return(data_long)
}
# Cargar datos iniciales
data_long <- load_data(csv_file_imc)
last_modified_time <- file.info(csv_file_imc)$mtime  # Obtener la fecha de modificación inicial



get_mexico_data <- function() {
  url <- "https://datosmacro.expansion.com/ipc"
  page <- read_html(url)
  
  # Extraer los nombres de los países
  country_names <- page %>%
    html_nodes("a[href^='/ipc-paises/']") %>%
    html_text() %>%
    str_extract("^[^-]+")
  
  
  # Extraer los valores numéricos
  values <- page %>%
    html_nodes("td[data-value].numero") %>%  # Selecciona el primer elemento con la clase "dm-wg-value"
    html_text() %>%                  # Extrae el texto del nodo
    
    
    
    print(str(country_names))
  print(str(values))
    
    
    
    # Crear el dataframe
    country_names <- head(country_names, 9)
  values <- head(values, 9)
  data <- data.frame(country = country_names, value = values)
  return(data)
}


# Definir la interfaz de usuario
ui <- fluidPage(
  titlePanel("Dashboard análisis financieros"),
  navbarPage("Dashboard",
             tabPanel(
                      "Inicio",
                      h2("Bienvenid@"),
                      p("Este dashboard presenta análisis de la economía interna y externa de México enfocada en los precios de los productos y divisas."),
                      p("realizado por: Bárcenas Castillo Alan Emmanuel, Escamilla Elias Miguel Salomón y Gibran Fernandez")
             ),
             
             tabPanel("Gastos",
                      titlePanel("Subida de Archivos"),
                      
                      # Primera fila: Archivo para Lugares de compra y su gráfica
                      fluidRow(
                        column(5, 
                               fileInput("file_lugar_comp", "Archivo para Lugares de compra:",
                                         accept = c(".txt", ".csv", ".xlsx", ".docx"))
                        ),
                        column(7, 
                               plotlyOutput("graficoBarrasLC")  # Gráfica correspondiente
                        )
                      ),
                      
                      # Segunda fila: Archivo para Mes y su gráfica
                      fluidRow(
                        column(5, 
                               fileInput("file_tipo_gasto", "Archivo para Mes:",
                                         accept = c(".txt", ".csv", ".xlsx", ".docx"))
                        ),
                        column(7, 
                               plotlyOutput("graficoBarrasPR")  # Gráfica correspondiente
                        )
                      ),
                      
                      # Tercera fila: Archivo para Producto y su gráfica
                      fluidRow(
                        column(5, 
                               fileInput("file_folio_hog", "Archivo para Producto:",
                                         accept = c(".txt", ".csv", ".xlsx", ".docx"))
                        ),
                        column(7, 
                               plotlyOutput("graficoBarrasMZ")  # Gráfica correspondiente
                        )
                      )
             ),
             
             
             tabPanel("IMCPMI",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("descriptor", "Selecciona un Descriptor:", choices = unique(data_long$Descriptores)),
                          selectInput("anio", "Selecciona un Año:", 
                                      choices = c("Todos" = "all", unique(data_long$Año))),
                          selectInput("tipo_grafica", "Selecciona el Tipo de Gráfica:",
                                      choices = c("Línea", "Barras", "Histograma"))
                        ),
                        mainPanel(
                          plotlyOutput("grafica"),
                          verbatimTextOutput("status")  # Mostrar estado de los datos
                        )
                      )
             ),
             
             tabPanel("INPC por producto",
                      titlePanel("Dashboard de Índice Nacional de Precios al Consumidor"),
                      sidebarLayout(
                        sidebarPanel(
                          # Selección de categoría para el eje Y
                          selectInput("y_var", "Selecciona la columna para el eje Y:",
                                      choices = c("Mostrar todas las categorías")),
                          br(),
                          br(),
                          helpText("Las gráficas también se actualizarán automáticamente si detectan cambios en el archivo.")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Gráfico", plotlyOutput("plotp")),
                            tabPanel("Tabla de Datos", DTOutput("dataTable"))
                          )
                        )
                      )
             ),
             
             tabPanel("INPC por pais",
                      plotOutput("plotweb")
             ),
             tabPanel("Tipos de cambio",
                        plotlyOutput("tipoCambioPlot")
             )
  )
)

# Definir la lógica del servidor
server <- function(input, output,session) {
  
  
  
  
  output$file_info <- renderText({
    req(input$file_lugar_comp)
    paste("Has subido el archivo:", input$file_lugar_comp$name)
  })
  
  output$contents_lugar_comp <- renderPrint({
    # Realiza una consulta a MongoDB
    
    br()
    
    req(input$file_lugar_comp)
    
    ext <- tools::file_ext(input$file_lugar_comp$name)
    
    content_lugar_comp <- NULL
    
    if (ext == "txt") {
      content_lugar_comp <- readLines(input$file_lugar_comp$datapath)
      data <- strsplit(content_lugar_comp, ",")
      content_lugar_comp <- data.frame(
        lugar_comp = sapply(data, function(x) x[1]),
        stringsAsFactors = FALSE
      )
      
    } else if (ext == "csv") {
      content_lugar_comp <- read.csv(input$file_lugar_comp$datapath, stringsAsFactors = FALSE, colClasses = c("lugar_comp" = "character"))
      
    } else if (ext == "xlsx") {
      library(readxl)
      content_lugar_comp <- read_excel(input$file_lugar_comp$datapath, col_types = "text")
      
    } else if (ext == "docx") {
      library(readtext)
      content_lugar_comp <- readtext(input$file_lugar_comp$datapath)
      text_parts <- strsplit(content_lugar_comp$text, ",|\n")[[1]]
      
      content_lugar_comp <- data.frame(
        lugar_comp = text_parts,
        stringsAsFactors = FALSE
      )
      
      
    } else {
      return(data.frame(Error = "Tipo de archivo no soportado."))
    }
    
    
    
    print(typeof(content_lugar_comp))
    
    print(content_lugar_comp)
    
    tryCatch({
      mongo_connection$insert(content_lugar_comp)
    }, error = function(e) {
      cat("Error al insertar datos en MongoDB:", e$message, "\n")
    })
  })
  
  
  
  
  
  
  
  output$contents_tipo_gasto <- renderPrint({
    req(input$file_tipo_gasto)
    
    ext <- tools::file_ext(input$file_tipo_gasto$name)
    
    content_lugar_comp <- NULL
    
    if (ext == "txt") {
      content_tipo_gasto <- readLines(input$file_tipo_gasto$datapath)
      data <- strsplit(content_tipo_gasto, ",")
      content_tipo_gasto <- data.frame(
        tipo_gasto = sapply(data, function(x) x[1]),
        stringsAsFactors = FALSE
      )
      
    } else if (ext == "csv") {
      content_tipo_gasto <- read.csv(input$file_tipo_gasto$datapath, stringsAsFactors = FALSE, colClasses = c("lugar_comp" = "character"))
      
    } else if (ext == "xlsx") {
      library(readxl)
      content_tipo_gasto <- read_excel(input$file_tipo_gasto$datapath, col_types = "text")
      
    } else if (ext == "docx") {
      library(readtext)
      content_tipo_gasto <- readtext(input$file_tipo_gasto$datapath)
      text_parts <- strsplit(content_tipo_gasto$text, ",|\n")[[1]]
      
      content_tipo_gasto <- data.frame(
        tipo_gasto = text_parts,
        stringsAsFactors = FALSE
      )
      
      
    } else {
      return(data.frame(Error = "Tipo de archivo no soportado."))
    }
    
    
    
    print(typeof(content_tipo_gasto))
    
    print(content_tipo_gasto)
    
    tryCatch({
      mongo_connection$insert(content_tipo_gasto)
    }, error = function(e) {
      cat("Error al insertar datos en MongoDB:", e$message, "\n")
    })
  })
  
  
  
  
  
  
  
  
  output$content_folio_hog <- renderPrint({
    req(input$file_folio_hog)
    
    ext <- tools::file_ext(input$file_folio_hog$name)
    
    content_lugar_comp <- NULL
    
    if (ext == "txt") {
      content_folio_hog <- readLines(input$file_folio_hog$datapath)
      data <- strsplit(content_folio_hog, ",")
      content_folio_hog <- data.frame(
        foliohog = sapply(data, function(x) x[1]),
        stringsAsFactors = FALSE
      )
      
    } else if (ext == "csv") {
      content_folio_hog <- read.csv(input$file_folio_hog$datapath, stringsAsFactors = FALSE, colClasses = c("lugar_comp" = "character"))
      
    } else if (ext == "xlsx") {
      library(readxl)
      content_folio_hog <- read_excel(input$file_folio_hog$datapath, col_types = "text")
      
    } else if (ext == "docx") {
      library(readtext)
      content_folio_hog <- readtext(input$file_folio_hog$datapath)
      text_parts <- strsplit(content_folio_hog$text, ",|\n")[[1]]
      
      content_folio_hog <- data.frame(
        foliohog = text_parts,
        stringsAsFactors = FALSE
      )
      
      
    } else {
      return(data.frame(Error = "Tipo de archivo no soportado."))
    }
    
    
    
    print(typeof(content_folio_hog))
    
    print(content_folio_hog)
    
    tryCatch({
      mongo_connection$insert(content_folio_hog)
    }, error = function(e) {
      cat("Error al insertar datos en MongoDB:", e$message, "\n")
    })
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Función para obtener el conteo de documentos
  obtenerConteoDocumentosLC <- function() {
    return(mongo_conexionLC$count('{}'))
  }
  
  # Función para obtener y procesar los datos
  obtenerDatosLC <- function() {
    consulta <- '[ 
      { 
        "$lookup": { 
          "from": "Lugar_compra", 
          "localField": "lugar_comp", 
          "foreignField": "lugar_comp", 
          "as": "lugar_compra_info" 
        } 
      },
      { 
        "$unwind": "$lugar_compra_info" 
      },
      { 
        "$group": { 
          "_id": "$lugar_compra_info.nombre", 
          "conteo": { "$count": {} } 
        } 
      }
    ]'
    
    datos <- mongo_conexionLC$aggregate(consulta)
    colnames(datos) <- c("NombreLC", "ConteoLC")
    return(datos)
  }
  
  # Reactivo que actualiza solo cuando hay cambios
  datosReactiveLC <- reactivePoll(
    intervalMillis = 1000,  # Verifica cada segundo
    session = session,
    checkFunc = obtenerConteoDocumentosLC,  # Verifica el conteo de documentos
    valueFunc = obtenerDatosLC  # Obtiene los datos si hay cambios
  )
  
  output$graficoBarrasLC <- renderPlotly({
    dfLC <- datosReactiveLC()  # Llama al objeto reactivo
    
    if (nrow(dfLC) > 0) {
      grafico <- plot_ly(dfLC, 
                         x = ~NombreLC, 
                         y = ~ConteoLC, 
                         type = 'bar', 
                         marker = list(color = 'steelblue')) %>%
        layout(
          title = "Distribución por lugar de compra",
          xaxis = list(title = "Lugar de Compra"),
          yaxis = list(title = "Cantidad"),
          margin = list(b = 150),
          barmode = 'group'
        )
      
      return(grafico)
    } else {
      return(NULL)
    }
  })
  
  
  
  
  
  
  
  
  # Función para obtener el conteo de documentos
  obtenerConteoDocumentosPR <- function() {
    return(mongo_connectionPR$count('{}'))  # Verifica el total de documentos
  }
  
  # Función para obtener y procesar los datos
  obtenerDatosPR <- function() {
    consulta <- '[ 
      { 
        "$lookup": { 
          "from": "Fecha_pago", 
          "localField": "tipo_gasto", 
          "foreignField": "mesdia", 
          "as": "fecha_pago_info" 
        } 
      },
      { 
        "$unwind": "$fecha_pago_info" 
      },
      { 
        "$lookup": { 
          "from": "Mes", 
          "localField": "fecha_pago_info.mes", 
          "foreignField": "mes", 
          "as": "mes_info" 
        } 
      },
      { 
        "$unwind": "$mes_info" 
      },
      { 
        "$group": { 
          "_id": "$mes_info.nombre", 
          "conteo": { "$sum": 1 } 
        } 
      }
    ]'
    
    # Ejecutar la consulta en MongoDB
    datosPR <- mongo_connectionPR$aggregate(consulta)
    
    # Renombrar columnas para un mejor entendimiento
    colnames(datosPR) <- c("MesPR", "ConteoPR")
    return(datosPR)
  }
  
  # Reactivo que se actualiza solo cuando hay cambios
  datosReactivePR <- reactivePoll(
    intervalMillis = 1000,  # Verifica cada segundo
    session = session,
    checkFunc = obtenerConteoDocumentosPR,  # Verifica el conteo de documentos
    valueFunc = obtenerDatosPR  # Obtiene los datos si hay cambios
  )
  
  # Renderizar la gráfica
  output$graficoBarrasPR <- renderPlotly({
    dfPR <- datosReactivePR()  # Llama al objeto reactivo
    
    if (nrow(dfPR) > 0) {
      graficoPR <- plot_ly(
        dfPR, 
        x = ~MesPR, 
        y = ~ConteoPR, 
        type = 'bar', 
        marker = list(color = 'steelblue')
      ) %>%
        layout(
          title = "Distribución por Mes",
          xaxis = list(title = "Mes"),
          yaxis = list(title = "Cantidad"),
          margin = list(b = 150),
          barmode = 'group'
        )
      
      return(graficoPR)
    } else {
      return(NULL)
    }
  })
  
  
  
  
  
  
  
  
  
  # Función para obtener el conteo de documentos
  obtenerConteoDocumentosMZ <- function() {
    return(mongo_connectionMZ$count('{}'))  # Verifica el total de documentos
  }
  
  # Función para obtener y procesar los datos
  obtenerDatosMZ <- function() {
    consulta <- '[ 
      { 
        "$lookup": { 
          "from": "Producto", 
          "localField": "foliohog", 
          "foreignField": "clave", 
          "as": "producto_info" 
        } 
      },
      { 
        "$unwind": "$producto_info" 
      },
      { 
        "$lookup": { 
          "from": "Tipo_producto", 
          "localField": "producto_info.tipo", 
          "foreignField": "tipo", 
          "as": "tipo_producto_info" 
        } 
      },
      { 
        "$unwind": "$tipo_producto_info" 
      },
      { 
        "$group": { 
          "_id": "$tipo_producto_info.nombre", 
          "conteo": { "$sum": 1 } 
        } 
      }
    ]'
    
    # Ejecutar la consulta en MongoDB
    datosMZ <- mongo_connectionMZ$aggregate(consulta)
    
    # Renombrar columnas para un mejor entendimiento
    colnames(datosMZ) <- c("NombreMZ", "ConteoMZ")
    return(datosMZ)
  }
  
  # Reactivo que se actualiza solo cuando hay cambios
  datosReactiveMZ <- reactivePoll(
    intervalMillis = 1000,  # Verifica cada segundo
    session = session,
    checkFunc = obtenerConteoDocumentosMZ,  # Verifica el conteo de documentos
    valueFunc = obtenerDatosMZ  # Obtiene los datos si hay cambios
  )
  
  # Renderizar la gráfica
  output$graficoBarrasMZ <- renderPlotly({
    dfMZ <- datosReactiveMZ()  # Llama al objeto reactivo
    
    if (nrow(dfMZ) > 0) {
      graficoMZ <- plot_ly(
        dfMZ, 
        x = ~NombreMZ, 
        y = ~ConteoMZ, 
        type = 'bar', 
        marker = list(color = 'steelblue')
      ) %>%
        layout(
          title = "Distribución por Tipo de Producto",
          xaxis = list(title = "Tipo de Producto"),
          yaxis = list(title = "Cantidad"),
          margin = list(b = 150),
          barmode = 'group'
        )
      
      return(graficoMZ)
    } else {
      return(NULL)
    }
  })
  
  #imcpmi
  # Variable reactiva para los datos y la fecha de modificación
  datos_reactivos <- reactiveVal(data_long)
  last_modified_imc <- reactiveVal(last_modified_time)
  
  # Monitorear cambios en el archivo
  observe({
    invalidateLater(5000, session)  # Revisar cada 5 segundos
    current_time_imc <- file.info(csv_file_imc)$mtime
    
    if (!is.null(current_time_imc) && current_time_imc != last_modified_imc()) {
      datos_reactivos(load_data(csv_file_imc))
      last_modified_imc(current_time_imc)
      showNotification("El archivo CSV ha cambiado. Los datos han sido recargados.", type = "message")
    }
  })
  
  # Filtrar datos según selección
  datos_filtrados <- reactive({
    data <- datos_reactivos()
    if (input$anio == "all") {
      data %>% filter(Descriptores == input$descriptor)
    } else {
      data %>% filter(Descriptores == input$descriptor, Año == as.numeric(input$anio))
    }
  })
  
  # Renderizar gráfica
  output$grafica <- renderPlotly({
    datos <- datos_filtrados()
    
    if (input$tipo_grafica == "Línea") {
      p <- plot_ly(datos, x = ~Mes, y = ~Valor, color = ~as.factor(Año), type = 'scatter', mode = 'lines+markers') %>%
        layout(title = ifelse(input$anio == "all", 
                              "Tendencia Mensual (Todos los Años)", 
                              paste("Tendencia Mensual en", input$anio)),
               xaxis = list(title = "Mes"),
               yaxis = list(title = "Valor"),
               legend = list(title = list(text = "Año")))
      p
      
    } else if (input$tipo_grafica == "Barras") {
      # Crear un dataframe auxiliar para la gráfica de barras
      bar_data <- datos %>%
        mutate(Mes_Año = paste(Mes, Año, sep = " - ")) %>%
        select(Mes_Año, Valor)
      
      p <- plot_ly(bar_data, x = ~Mes_Año, y = ~Valor, type = 'bar') %>%
        layout(title = ifelse(input$anio == "all", 
                              "Valores Mensuales (Todos los Años)", 
                              paste("Valores Mensuales en", input$anio)),
               xaxis = list(title = "Mes - Año"),
               yaxis = list(title = "Valor"))
      p
    } else if (input$tipo_grafica == "Histograma") {
      p <- plot_ly(datos, x = ~Valor, color = ~as.factor(Año), type = 'histogram', 
                   opacity = 0.7) %>%
        layout(title = ifelse(input$anio == "all", 
                              "Distribución de Valores (Todos los Años)", 
                              paste("Distribución de Valores en", input$anio)),
               xaxis = list(title = "Valor"),
               yaxis = list(title = "Frecuencia"),
               legend = list(title = list(text = "Año")))
      p
    }
  })
  
  # Mostrar el estado de los datos
  output$status <- renderPrint({
    cat("Última actualización de datos:", last_modified_imc(), "\n")
    cat("Número de filas en los datos actuales:", nrow(datos_reactivos()), "\n")
  })
  
  
  
  
  
  
  
  
  
  
  #inpc por producto
  
  # Lectura reactiva del archivo Excel
  data <- reactiveFileReader(
    intervalMillis = 5000,  # Verifica cambios cada 5 segundos
    session = session,
    filePath = file_path,
    readFunc = function(file) {
      # Leer el archivo Excel
      df <- read_excel(file, sheet = 1)
      # Convertir columnas numéricas
      for (col in 2:ncol(df)) {
        df[[col]] <- as.numeric(df[[col]])
      }
      return(df)
    }
  )
  
  # Actualizar las opciones del eje Y automáticamente
  observe({
    updateSelectInput(session, "y_var",
                      choices = c("Mostrar todas las categorías", names(data())[2:ncol(data())]))
  })
  
  # Gráfico interactivo con Plotly
  output$plotp <- renderPlotly({
    if (input$y_var == "Mostrar todas las categorías") {
      # Graficar todas las columnas de categorías
      plotp <- plot_ly()
      for (col in names(data())[2:ncol(data())]) {
        plotp <- plotp %>% add_lines(x = data()$`Fecha (por mes)`, y = data()[[col]], name = col)
      }
      plotp %>% layout(title = "Gráfico de Líneas: Todas las Categorías",
                      xaxis = list(title = "Fecha"),
                      yaxis = list(title = "Valores"),
                      legend = list(title = list(text = "Categorías")))
    } else {
      # Graficar solo una categoría seleccionada
      plot_ly(data(), x = ~`Fecha (por mes)`, y = ~get(input$y_var),
              type = "scatter", mode = "lines+markers", name = input$y_var) %>%
        layout(title = paste("Gráfico de Líneas:", input$y_var),
               xaxis = list(title = "Fecha (por mes)"),
               yaxis = list(title = input$y_var))
    }
  })
  
  # Tabla interactiva con datos reactivos
  output$dataTable <- renderDT({
    datatable(data())
  })
  
  # Actualización manual
  observeEvent(input$update, {
    session$reload()
  })
  
  
  
  
  
  
  
  #inpc por pais
  
  #autoInvalidate <- reactiveTimer(50000) # Intervalo de 5 segundos
  # Datos reactivos que se actualizan automáticamente
  dataweb <- reactive({
    #autoInvalidate() # Invoca el timer
    get_mexico_data()
  })
  
  
  
output$plotweb <- renderPlot({
  ggplot(dataweb(), aes(x = country, y = value, fill = country)) +  # Mapeo de fill
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(x = "", y = input$indicator) +
    theme_minimal() +
    scale_fill_discrete()  # Colores diferentes para cada barra
})
  
  
  
  
  
  
  
  #datos
  setToken("5da47631db1d33c1eb19e799489d2adbaa60ce14a1b5eb8478bba93dfcf15570")
  
  idSeries <- c("SF43718", "SF46410", "SF46407","SF290383","SF46406","SF57847")
  series <- getSeriesData(idSeries, '2020-01-01', '2024-12-04')
  
  output$tipoCambioPlot <- renderPlotly({
    # Crear un data frame para cada serie
    data_sf43718 <- getSerieDataFrame(series, "SF43718")
    data_sf46410 <- getSerieDataFrame(series, "SF46410")
    data_sf46407 <- getSerieDataFrame(series, "SF46407")
    data_SF290383 <- getSerieDataFrame(series, "SF290383")
    data_SF46406 <- getSerieDataFrame(series, "SF46406")
    data_SF57847 <- getSerieDataFrame(series, "SF57847")
    
    
    # Combina los datos en un solo gráfico interactivo
    p <- plot_ly() %>%
      add_lines(data = data_sf43718, x = ~date, y = ~value, name = "Dolar estadounidense", line = list(color = "#ff6602")) %>%
      add_lines(data = data_sf46410, x = ~date, y = ~value, name = "Libra esterlina", line = list(color = "#ff0202")) %>%
      add_lines(data = data_sf46407, x = ~date, y = ~value, name = "Euro", line = list(color = "#0013ff")) %>%
      add_lines(data = data_SF290383, x = ~date, y = ~value, name = "Yuan chino", line = list(color = "#fff700")) %>%
      add_lines(data = data_SF46406, x = ~date, y = ~value, name = "Yen japonés", line = list(color = "#000000")) %>%
      add_lines(data = data_SF57847, x = ~date, y = ~value, name = "Dinar kuwaití", line = list(color = "#30ff02")) %>%        
      layout(title = "Tipos de Cambio de divisas respecto al peso mexicano",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Valor"),
             hovermode = "closest")
    
    p
  })
  
  
  
  
  
  
  
  
  
  
  
  
  #default
  
  output$plot2 <- renderPlot({
    ggplot(mtcars, aes(x = hp, y = mpg)) +
      geom_point() +
      ggtitle("Gráfico 2: Relación entre caballos de fuerza y MPG")
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)