# Librerías ---------------------------------------------------------------

library(shinydashboard)
library(dashboardthemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly) 
library(leaflet)
library(DT)


# Datos y manipulación ----------------------------------------------------

# dataframe
df_p <- read.delim("/cloud/project/datos/pC.txt", encoding="UTF-8")

# Transformación con dplyr, nueva columna con mutate asignando tipo de profundidad
df_rp <- df_p %>%
  select(-X) %>%
  replace_na(list(profundidad = 0, kWh_M = 0, extraccion_max = 0)) %>% 
  mutate(
    Nivel = case_when(
      profundidad < 100 ~ "A",
      profundidad > 100 & profundidad <= 200 ~ "B",
      profundidad > 200 & profundidad <= 300 ~ "C",
      profundidad > 300 & profundidad <= 400 ~ "D",
      profundidad > 400 & profundidad <= 500 ~ "E",
      profundidad > 500 & profundidad <= 600 ~ "F"
    )
  )

# Establecer niveles de profundidad
df_rp$Nivel <- factor(df_rp$Nivel,
                      levels = c("A","B","C","D","E","F"),
                      labels = c("A","B","C","D","E","F"))

# Tabla resumen: conteo de pozos dentro del nivel

df_rpdf_rp_tr <- df_rp %>%
  group_by(Nivel) %>%
  summarise(profundidad = n()) %>%
  ungroup() %>%
  datatable(
    colnames = c(
      "Rango de perforación",
      "Número de pozos"))

# Quitar NA's de las columnas lat y long para el mapa
# (También se eliminan los renglones donde se encuentran)
df_rp_m <- df_rp %>%
  drop_na()

# Función para identificar los pozos por nivel en el mapa
coloR_df_rp_m <- function(df_rp_m)
{sapply(df_rp_m$Nivel, function(Nivel)
{
  if(Nivel == "A")
  {"purple"}
  else if(Nivel == "B")
  {"green"}
  else if(Nivel == "C")
  {"orange"}
  else if(Nivel == "D")
  {"yellow"}
  else if(Nivel == "E")
  {"olive"}
  else{"red"}})}

iconos <- awesomeIcons(
  icon = 'ios-close',
  iconColor = "black",
  library = 'ion',
  markerColor = coloR_df_rp_m(df_rp_m)
)

# Dashboard ---------------------------------------------------------------

ui <- dashboardPage( skin = "black",
                     dashboardHeader(disable = FALSE,
                                     title = "Situación actual de los pozos en Celaya",
                                     titleWidth = 600),
                     dashboardSidebar(disable = TRUE),
                     dashboardBody(
                       shinyDashboardThemes(theme = "grey_dark"),
                       # 1er renglon para indicadores
                       fluidRow(
                         # casillas estáticas
                         valueBox(109, "Colonias beneficiadas",
                                  icon = icon("fas fa-house-user"),
                                  color = "olive",
                                  width = 2 ),
                         valueBox(97, "Pozos en operación",
                                  icon = icon("fas fa-water"),
                                  color = "navy",
                                  width = 2),
                         valueBox(5, "Pozos en estado crítico",
                                  icon = icon("fas fa-exclamation-triangle"),
                                  color = "orange",
                                  width = 2),
                         valueBox(3, "Pozos en rehabilitación ",
                                  icon = icon("fas fa-tools"),
                                  color = "purple",
                                  width = 2),
                         valueBox(615208, "Usuarios beneficiados",
                                  icon = icon("fas fa-users"),
                                  color = "green",
                                  width = 2),
                         valueBox(6, "Obras de mantenimiento a drenajes",
                                  icon = icon("fas fa-clipboard-list"),
                                  color = "lime",
                                  width = 2)
                       ),
                       
                       # 2do renglon para el mapa
                       fluidRow(
                         box(title = "Mapa",
                             solidHeader = T,
                             background = NULL,
                             width = "150px",
                             collapsible = T,
                             collapsed = F,
                             color = "info",
                             leafletOutput("Mapa")
                         ),
                         
                         # 3er renglon para la tabla
                         fluidRow(
                           box(title = "Tabla de información",
                               solidHeader = T,
                               background = NULL,
                               width  = 12,
                               collapsible = T,
                               collapsed = F,
                               color = "info",
                               DTOutput("Tabla")
                               # primary,success,info,warning,danger = colores
                           )
                         ),
                         # 4to renglon para el gráfico
                         fluidRow(
                           box(title = "Gráfico interactivo",
                               solidHeader = T,
                               background = NULL,
                               width  = 12,
                               collapsible = T,
                               collapsed = F,
                               color = "info",
                               plotlyOutput("g1")
                               # primary,success,info,warning,danger = colores
                           )
                         )
                         
                       ),
                       # tags$head(
                       #     tags$style(
                       #         HTML('
                       #             /* body */
                       #             .content-wrapper,
                       #             .right-side {
                       #             background-color: white; }'
                       #         )))
                     )
)

server <- function(input, output) {
  
  output$Mapa <- renderLeaflet({
    Mapa <- df_rp %>%
      leaflet() %>%
      addTiles() %>%
      # ~ para identificar un atributo y no un objeto
      addAwesomeMarkers(lat = ~lat,lng = ~long, icon = iconos,
                        clusterOptions = markerClusterOptions(), popup = paste(
                          "<strong> Ubicación: </strong>",df_rp_m$id,
                          "<br><strong>Consumo eléctrico mensual: </strong>",
                          df_rp_m$kWh_M,
                          "<br><strong>Volumen extracción (l/s): </strong>",
                          df_rp_m$extraccion_actual)) %>% 
      setView(lng = -100.820623, lat = 20.525321, zoom = 12)
  })
  
  output$Tabla <- renderDT({
    Tabla <- datatable(df_rp,
                       colnames = c('Número de pozo',
                                    'Ubicación',
                                    'Profundidad',
                                    'Volumen de extracción actual',
                                    'Volumen de extracción máximo',
                                    'Latitud',
                                    'Longitud',
                                    'Consumo eléctrico (KWh/mes)',
                                    'Nivel'),
                       caption = htmltools::tags$caption(
                         style = 'caption-side: bottom;text-align: center;',
                         'Tabla 1:', 
                         htmltools::em('Informacion proporcionada por JUMAPA')))
    
  })
  
  output$g1 <- renderPlotly({
    g1  <- df_rp %>%
      plot_ly(
        x = ~profundidad,
        y = ~extraccion_actual,
        color = ~Nivel,
        #variables
        type = "scatter", mode = "markers",
        # tipo de grafico
        text = ~paste("Ubicación: ", id, 
                      '<br> Pozo: ', n_pozo,
                      '<br> Consumo eléctrico: ', kWh_M)) %>% 
      # texto para cada punto
      layout(
        # capa para elementos
        title = "Relación entre profundidad y extracción", 
        # titulo principal
        # plot_bgcolor = '#D0D3D4', paper_bgcolor = '#D0D3D4', # color fondos
        xaxis = list( title = "Profundidad del pozo en metros", # eje X
                      tickangle = 0,
                      showline = FALSE, linewidth = 1,
                      zeroline = FALSE
                      #, linecolor = "#3a5199"
        ),
        yaxis = list( title = "Extracción en litros por segundo", # eje Y
                      linewidth = 1,
                      zeroline = FALSE, showline = FALSE,
                      showgrid = T, showticklabels = F
        ),
        annotations =
          list( # leyenda esquina inferior derecha
            text = "Elaborado por: github.com/gavmtz",
            showarrow = F,
            font = list( size = 9, color = "#212F3C"),
            xref = 'paper', x = 1.1,
            yref = 'paper', y = -0.09), autosize = F
      )
    
  })
}

shinyApp(ui = ui, server = server)