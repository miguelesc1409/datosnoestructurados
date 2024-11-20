library(shiny)
library("siebanxicor")
library(plotly)

# Define UI
ui <- fluidPage(
    titlePanel("Gráfica de Tipos de Cambio"),
    mainPanel(
        plotlyOutput("tipoCambioPlot")
    )
)

# Define server logic
server <- function(input, output) {
    setToken("5da47631db1d33c1eb19e799489d2adbaa60ce14a1b5eb8478bba93dfcf15570")

    idSeries <- c("SF290383","SF46406")
    series <- getSeriesData(idSeries, '2020-01-01', '2023-01-31')

    output$tipoCambioPlot <- renderPlotly({
        # Crear un data frame para cada serie
        data_SF290383 <- getSerieDataFrame(series, "SF290383")
	  data_SF46406 <- getSerieDataFrame(series, "SF46406")
        
        # Combina los datos en un solo gráfico interactivo
        p <- plot_ly() %>%
            add_lines(data = data_SF290383, x = ~date, y = ~value, name = "Yuan chino", line = list(color = "#00b300")) %>%
		add_lines(data = data_SF46406, x = ~date, y = ~value, name = "Yen japonés", line = list(color = "#000000")) %>%
            layout(title = "Tipos de Cambio",
                   xaxis = list(title = "Fecha"),
                   yaxis = list(title = "Valor"),
                   hovermode = "closest")
        
        p
    })
}


# Run the application 
shinyApp(ui = ui, server = server)