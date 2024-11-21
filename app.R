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

csv_file_imc <- "C:/datos/mayolo/conjunto_de_datos_imcpmi_cp2023_05.csv"
# Ruta del archivo Excel
file_path <- "C:/datos/project1/Productos.xlsx"


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
  url <- "https://datosmacro.expansion.com/paises/mexico"
  page <- read_html(url)
  
  # Extraer los nombres de los países
  country_names <- page %>%
    html_nodes("a[href^='/ipc-paises/']") %>%
    html_attr("title") %>%
    str_extract("^[^-]+")
  
  
  # Extraer los valores numéricos
  values <- page %>%
    html_nodes(".dm-wg-value") %>%  # Selecciona el primer elemento con la clase "dm-wg-value"
    html_text() %>%                  # Extrae el texto del nodo
    
    # Verificar la longitud de los vectores
    print(length(country_names))
  print(length(values))
  
  
  # Crear el dataframe
  country_names <- head(country_names, 7)
  values <- head(values, 7)
  data <- data.frame(country = country_names, value = values)
  return(data)
}


# Definir la interfaz de usuario
ui <- fluidPage(
  titlePanel("Dashboard análisis financieros"),
  navbarPage("Inicio",
             tabPanel("Inicio",
                      h2("Bienvenid@"),
                      p("Este dashboard presenta análisis de la economía interna y externa de México enfocada en los precios de los productos y divisas.")
                      p("realizado por: Bárcenas Castillo Alan Emmanuel, Escamilla Elias Miguel Salomón y Gibran Fernandez")
             ),
             
             tabPanel("Gastos",
                      plotOutput("plot2")
             ),
             
             
             tabPanel("IMCPMI",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("descriptor", "Selecciona un Descriptor:", choices = unique(data_long$Descriptores)),
                          selectInput("anio", "Selecciona un Año:", 
                                      choices = c("Todos" = "all", unique(data_long$Año))),
                          selectInput("tipo_grafica", "Selecciona el Tipo de Gráfica:",
                                      choices = c("Línea", "Barras", "Histograma")),
                          actionButton("reload", "Actualizar Datos Manualmente")
                        ),
                        mainPanel(
                          plotlyOutput("grafica"),
                          verbatimTextOutput("status")  # Mostrar estado de los datos
                        )
                      )
             ),
             
             tabPanel("INPC por producto",
                      titlePanel("Dashboard de Índice Nacional de Precios al Consumidor (Excel)"),
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
  
  autoInvalidate <- reactiveTimer(5000) # Intervalo de 5 segundos
  # Datos reactivos que se actualizan automáticamente
  dataweb <- reactive({
    autoInvalidate() # Invoca el timer
    get_mexico_data()
  })
  
  
  
  output$plotweb <- renderPlot({
    ggplot(dataweb(), aes(x = country, y = value)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "", y = input$indicator) +
      theme_minimal()
  })
  
  #datos
  setToken("5da47631db1d33c1eb19e799489d2adbaa60ce14a1b5eb8478bba93dfcf15570")
  
  idSeries <- c("SF43718", "SF46410", "SF46407","SF290383","SF46406","SF57847")
  series <- getSeriesData(idSeries, '2020-01-01', '2024-11-20')
  
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