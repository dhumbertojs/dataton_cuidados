# Cargar paquetes

library(sf)
library(dplyr)
library(shiny)
library(bslib)
library(stringr)
library(leaflet)

# Datos -------------------------------------------------------------------

lunas <- read.csv("data/lunas-cdmx.csv") %>% 
    st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>% 
    mutate(nombre_luna = str_to_title(nombre_luna))

pasos <- st_read("data/pasos_seguros_cdmx/pasos_seguros_cdmx.shp")

srv_atn <- read.csv("data/servicios_atencion_mujeres.csv") %>% 
    st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>% 
    mutate(
        dir_com = paste0(calle, " #", numero_exterior, " col. ", colonia, 
                         " C.P. ", codigo_postal_servicio_postal_mexicano, " ", 
                         str_to_title(alcaldia), "."),
        horario = paste0("De ", substr(horario_ini, 1, 5), " a ", substr(horario_fin, 1, 5)),
        horario = ifelse(horario == "De 00:00 a 23:59", "Todo el día", horario)
    )

metro <- st_read("data/stcmetro_shp/STC_Metro_lineas_utm14n.shp") %>% 
    st_zm(drop = TRUE, what = "ZM") %>% 
    janitor::clean_names() %>% 
    st_transform(crs = 4326)

mb <- st_read("data/mb_shp/Metrobus_lineas.shp") %>% 
    st_zm(drop = TRUE, what = "ZM") %>% 
    st_transform(crs = 4326)

cablebus <- st_read("data/ste_shp/ste_cablebus_shp/STE_Cablebus_lineas_utm14n.shp") %>% 
    st_zm(drop = TRUE, what = "ZM") %>% 
    st_transform(crs = 4326)

tren_ligero <- st_read("data/ste_shp/ste_tren_ligero_shp/STE_TrenLigero_linea_utm14n.shp") %>% 
    st_zm(drop = TRUE, what = "ZM") %>% 
    st_transform(crs = 4326)

trole <- st_read("data/ste_shp/ste_trolebus_shp/STE_Trolebus_Paradas.shp") %>% 
    st_zm(drop = TRUE, what = "ZM") %>% 
    st_transform(crs = 4326)

ecobici <- st_read("data/cicloestaciones_ecobici/cicloestaciones_ecobici.shp") %>% 
    st_transform(crs = 4326)

c_atn <- st_read("data/Centros_atención_violencia_mujer/Centros_atención_violencia_mujer.shp") %>% 
    st_transform(crs = 4326) %>% 
    mutate(colonia = str_to_title(colonia), alcaldia = str_to_title(alcaldia))

urb_soc <- st_read("data/urbanismo_social_sintesis/urbanismo_social_sintesis.shp") %>% 
    st_transform(crs = 4326)

# UI ----------------------------------------------------------------------

# ui <- fluidPage(
#     titlePanel("Mapa interactivo sobre la disponibilidad de servicios para cuidados en la CDMX"),
#     leafletOutput("mapa")
# )

ui <- page_navbar(
    title = "Disponibilidad de infraestructura para cuidados en la CDMX",
    theme = bs_theme(bootswatch = "sandstone", base_font = font_google("Montserrat")),
    fluid = T,
    fillable = T,
    fillable_mobile = T, 
    nav_panel(title = "Mapa interactivo", leafletOutput("mapa"))
    
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
    
    # Crear el mapa
    output$mapa <- renderLeaflet({
        
        pal <- colorFactor(
            palette = c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#1a9850"),  # Rojo a verde
            levels = c(5, 4, 3, 2, 1)
        )
        
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            
            # Agregar 'Centros de atención a la violencia contra la mujer'
            addPolygons(data = c_atn,
                        fillColor = ~pal(C_CA_VIOLM),
                        color = "black",
                        weight = 1,
                        fillOpacity = 0.5,  
                        group = "Número de centros de atención",
                        popup = ~paste("<strong>Alcaldía:</strong>", alcaldia, "<br>",
                                       "<strong>Colonia:</strong>", colonia, "<br>",
                                       "<strong>Centros de atención en esta colonia:</strong>", COUNT_orie)) %>%
            
            # Agregar 'LUNAS' servicios de asesoría
            addCircleMarkers(data = lunas,
                             radius = 5,
                             color = "blue",
                             fillOpacity = 0.7,
                             group = "LUNAS",
                             popup = ~paste("<strong>Nombre:</strong>", nombre_luna, "<br>",
                                            "<strong>Dirección:</strong>", direccion, "<br>",
                                            "Teléfono:", telefono_1, "<br>",
                                            "Correo:", correo, "<br>",
                                            "Horario:", horarios, "<br>",
                                            "<strong>Servicios:</strong>", servicios)) %>%
            
            # Agregar 'Servicios de atención a violencia contra las mujeres'
            addCircleMarkers(data = srv_atn,
                             radius = 5,
                             color = "green",
                             fillOpacity = 0.7,
                             group = "Servicios de Atención",
                             popup = ~paste("<strong>Nombre:</strong>", sede, "<br>",
                                            "Dirección:", dir_com, "<br>",
                                            "Horario:", horario, "<br>",
                                            "Teléfono:", telefono_uno, "<br>",
                                            "<strong>Referencia:</strong>", referencia_de_ubicacion)) %>%
            
            # Agregar 'Pasos Seguros' como puntos
            addCircleMarkers(data = pasos,
                             color = "purple",
                             radius = 5,
                             weight = 2,
                             group = "Pasos Seguros",
                             popup = ~paste("<strong>Paso seguro:</strong>", name)) %>%
            
            # Agregar 'Urbanismo Social'
            addPolygons(data = urb_soc,
                        fillColor = ~pal(C_US),
                        color = "black",
                        weight = 1,
                        fillOpacity = 0.5,  
                        group = "Grado de marginación",
                        popup = ~paste("<strong>Proyecto:</strong>", colonia)) %>%
            
            # Agregar líneas de transporte
            addPolylines(data = metro,
                         color = ~case_when(
                             linea == "1" ~ "#F04E98",
                             linea == "2" ~ "#005EB8",
                             linea == "3" ~ "#AF9800",
                             linea == "4" ~ "#6BBBAE",
                             linea == "5" ~ "#FFD100",
                             linea == "6" ~ "#DA291C",
                             linea == "7" ~ "#E87722",
                             linea == "8" ~ "#009A44",
                             linea == "9" ~ "#512F2E",
                             linea == "A" ~ "#981D97", 
                             linea == "B" ~ "#b1b3b3", 
                             linea == "12" ~ "#B9975B",     
                             TRUE ~ "black"), 
                         weight = 3,
                         group = "Metro",
                         popup = ~paste("<strong>Línea del Metro: </strong>", linea)) %>% 
            
            # Agregar las líneas del Metrobús
            addPolylines(data = mb, color = "#C10020", weight = 3, group = "Metrobús",
                         popup = ~paste("<strong>Línea del Metrobús</strong>")) %>%
            
            # Agregar las líneas del Cablebús
            addPolylines(data = cablebus, color = "#4EC3E0", weight = 3, group = "Cablebús",
                         popup = ~paste("<strong>Línea del Cablebús</strong>")) %>%
            
            # Agregar la línea del Tren Ligero
            addPolylines(data = tren_ligero, color = "#0057B8", weight = 3, group = "Tren Ligero",
                         popup = ~paste("<strong>Línea del Tren Ligero</strong>")) %>%
            
            # Agregar las paradas del Trolebús
            addCircleMarkers(data = trole, color = "#0057B8", radius = 4, group = "Trolebús",
                             popup = ~paste("<strong>Parada del Trolebús</strong>")) %>%
            
            # Agregar las cicloestaciones de Ecobici
            addCircleMarkers(data = ecobici, color = "#009A44", radius = 4, group = "Ecobici",
                             popup = ~paste("<strong>Estación de Ecobici</strong>")) %>%
            
            # Agregar controles de capas
            addLayersControl(
                overlayGroups = c("Grado de marginación", "Número de centros de atención",
                                  "LUNAS", "Servicios de Atención", "Pasos Seguros", 
                                  "Metro", "Metrobús", "Cablebús", "Tren Ligero", 
                                  "Trolebús", "Ecobici"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            
            # Ocultar capas por defecto
            hideGroup(c("Número de centros de atención", "LUNAS", "Servicios de Atención",
                        "Pasos Seguros", "Metro", "Metrobús", "Cablebús", "Tren Ligero", 
                        "Trolebús", "Ecobici")) %>%
            
            # Ajustar la vista inicial
            setView(lng = -99.1332, lat = 19.4326, zoom = 10)
    })
}

# Ejecutar la aplicación
shinyApp(ui, server)
