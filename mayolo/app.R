library(shiny)
library(tidyverse)
library(plotly)

# Ruta del archivo CSV
csv_file <- "C:/datos/mayolo/conjunto_de_datos_imcpmi_cp2023_05.csv"

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
data_long <- load_data(csv_file)
last_modified_time <- file.info(csv_file)$mtime  # Obtener la fecha de modificación inicial

# UI
ui <- fluidPage(
  titlePanel("Visualización de Datos del Archivo CSV con Detección de Cambios"),
  
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
)

# Server
server <- function(input, output, session) {
  
  # Variable reactiva para los datos y la fecha de modificación
  datos_reactivos <- reactiveVal(data_long)
  last_modified_imc <- reactiveVal(last_modified_time)
  
  # Monitorear cambios en el archivo
  observe({
    invalidateLater(5000, session)  # Revisar cada 5 segundos
    current_time_imc <- file.info(csv_file)$mtime
    
    if (!is.null(current_time_imc) && current_time_imc != last_modified_imc()) {
      datos_reactivos(load_data(csv_file))
      last_modified_imc(current_time_imc)
      showNotification("El archivo CSV ha cambiado. Los datos han sido recargados.", type = "message")
    }
  })
  
  # Botón para recargar manualmente
  observeEvent(input$reload, {
    datos_reactivos(load_data(csv_file))
    last_modified(file.info(csv_file)$mtime)
    showNotification("Los datos han sido recargados manualmente.", type = "message")
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
    cat("Última actualización de datos:", last_modified(), "\n")
    cat("Número de filas en los datos actuales:", nrow(datos_reactivos()), "\n")
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
