library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(plotly)

# Cargar datos
file_path <- "salud_mental_ecuador_con_edad.csv" # Cambiar por la ruta de tu archivo
data <- read.csv(file_path)

# Crear estadísticas clave
stats <- data %>%
  summarise(
    Provincias = n_distinct(Province),
    Ciudades = n_distinct(City),
    Hospitales = n_distinct(HealthCenter), 
    Pacientes = n_distinct(PatientID)
  )

# Generar coordenadas únicas para cada combinación de Provincia y Ciudad
set.seed(123) # Para reproducibilidad
unique_locations <- data %>%
  distinct(Province, City) %>%
  mutate(
    Latitude = runif(n(), -5, 1),  # Coordenadas aproximadas para Ecuador
    Longitude = runif(n(), -80, -75)
  )

# Asignar coordenadas al dataset original
data <- data %>%
  left_join(unique_locations, by = c("Province", "City"))

# Interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard de Salud Mental en Ecuador"),
  dashboardSidebar(
    selectInput("province", "Selecciona una Provincia:", choices = unique(data$Province), selected = "Azuay")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("provinces_box"),
      valueBoxOutput("cities_box"),
      valueBoxOutput("hospitals_box"),
      valueBoxOutput("patients_box")
    ),
    fluidRow(
      box(width = 12, leafletOutput("map", height = 400))
    ),
    fluidRow(
      box(width = 6, plotlyOutput("cases_by_city")),
      box(width = 6, plotlyOutput("diagnosis_dist"))
    )
  )
)

# Lógica del servidor
server <- function(input, output) {
  # Estadísticas clave
  output$provinces_box <- renderValueBox({
    valueBox(stats$Provincias, "Provincias", icon = icon("globe"), color = "green")
  })
  output$cities_box <- renderValueBox({
    valueBox(stats$Ciudades, "Ciudades", icon = icon("city"), color = "yellow")
  })
  output$hospitals_box <- renderValueBox({
    valueBox(stats$Hospitales, "Hospitales", icon = icon("hospital"), color = "orange")
  })
  output$patients_box <- renderValueBox({
    valueBox(stats$Pacientes, "Pacientes", icon = icon("user"), color = "blue")
  })
  
  # Mapa interactivo
  output$map <- renderLeaflet({
    filtered_data <- data %>% filter(Province == input$province)
    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        color = "blue",
        popup = ~paste("<strong>Centro:</strong>", HealthCenter, "<br><strong>Ciudad:</strong>", City)
      )
  })
  
  # Casos por Ciudad
  output$cases_by_city <- renderPlotly({
    city_data <- data %>% 
      filter(Province == input$province) %>% 
      group_by(City) %>% 
      summarise(Casos = n())
    
    p <- ggplot(city_data, aes(x = reorder(City, -Casos), y = Casos, fill = City)) +
      geom_bar(stat = "identity") +
      labs(title = "Casos por Ciudad", x = "", y = "Número de Casos") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Distribución de Diagnósticos
  output$diagnosis_dist <- renderPlotly({
    diagnosis_data <- data %>% 
      filter(Province == input$province) %>% 
      group_by(Diagnosis) %>% 
      summarise(Casos = n())
    
    p <- ggplot(diagnosis_data, aes(x = Diagnosis, y = Casos, fill = Diagnosis)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribución de Diagnósticos", x = "", y = "Número de Casos") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
}

# Ejecutar aplicación
shinyApp(ui = ui, server = server)
