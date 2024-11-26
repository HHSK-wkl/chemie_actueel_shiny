library(shiny)
# library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(HHSKwkl)
library(leaflet)
library(glue)
library(sf)
library(bslib)

# Load data
ws_grens <- sf::st_read("data/ws_grens.gpkg") %>% sf::st_transform(crs = 4326)

# blauw    <- colorspace::hex(colorspace::colorspace::HLS(202.5, 0.38, 1))
# blauw_m  <- colorspace::hex(colorspace::HLS(202.5, 0.60, 1))
# blauw_l  <- colorspace::hex(colorspace::HLS(202.5, 0.80, 1))
# oranje   <- colorspace::hex(colorspace::HLS(25   , 0.38, 1))
# oranje_m <- colorspace::hex(colorspace::HLS(25   , 0.60, 1))
# oranje_l <- colorspace::hex(colorspace::HLS(25   , 0.80, 1))
# grijs    <- "#616161"
# grijs_m  <- "#999999"
# grijs_l  <- "#cccccc"

# Helper functions
url_csv <- function(mp) paste0('<a href="https://www.schielandendekrimpenerwaard.nl/kaart/waterkwaliteit/wkl_gegevens_op_kaart/meetgegevens/', mp, '.csv">Meetgegevens</a>')
url_pdf <- function(mp) paste0('<a href="https://www.schielandendekrimpenerwaard.nl/kaart/waterkwaliteit/wkl_gegevens_op_kaart/grafieken/', mp, '.pdf">Grafieken</a>')

# Define UI
ui <- page_sidebar(
  title = "HHSK - Actuele waterkwaliteit",
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  sidebar = sidebar(
    width = 350,
    dateRangeInput("datum_sel", "Periode", language = "nl", separator = "t/m", start = Sys.Date() - 31, format = "dd-mm-yyyy"),
    selectInput("param_sel", "Parameter", choices = c("Chloride" = 1)),
    selectInput("param_group", "Parametergroep (optioneel)", multiple = TRUE,
                choices = list("Algemeen", "Bacteriologie", "Bestrijdingsmiddelen", "Blauwalgen", 
                               "Metalen opgelost", "Metalen totaal", "Organisch", "Zintuiglijk")),
    selectInput("agg_fun", "Waardebewerkingsmethode", 
                choices = c("Gemiddelde" = "mean",
                            "Laatste" = "first",
                            "Mediaan" = "median",
                            "Maximum" = "max",
                            "Minimum" = "min")),
    hr(),
    # Adding logo to the sidebar
    img(src = "logo_website.png", width = "100%")
  ),
  layout_column_wrap(
    width = 1/2,
    card(
      full_screen = TRUE,
      card_header("Kaart"),
      leafletOutput("kaart", height = "700px")
    ),
    layout_column_wrap(
      width = 1,
      card(
        full_screen = TRUE,
        card_header("Grafiek per locatie"),
        plotOutput("grafiek_loc", height = "350px")
      ),
      card(
        full_screen = TRUE,
        card_header("Toelichting"),
        p("De kaart laat de meetpunten zien waar in de gekozen periode metingen van de gekozen parameter beschikbaar zijn.
          De kleuren geven de waarde aan volgens de gekozen bewerkingsmethode. De kleuren zijn niet lineair verdeeld,
          maar geven de volgende intervallen: 0% - 1% - 5% - 10% - 30% - 70% - 90% - 95% - 99% - 100%."),
        p("De grafiek toont de metingen (onbewerkt) vanaf 2010 van het meetpunt dat op de kaart wordt aangeklikt.")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  meetpunten <- data_online("meetpunten.rds")
  parameters <- data_online("parameters.rds")
  fys_chem <- data_online("fys_chem.rds") %>% semi_join(filter(meetpunten, meetpunttypering %in% c(1, 2, 3, 5, 12)))
  
  # shiny::updateDateRangeInput(inputId = "datum_sel", start = Sys.Date() - 31)
  
  meetpunten_leaflet <- meetpunten %>% 
    select(mp, x, y) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
    sf::st_transform(crs = 4326)
  
  f_parnaam <- maak_opzoeker(parameters, parnr, parnaamlang)
  f_eenheid <- maak_opzoeker(parameters, parnr, eenheid)
  f_mpomsch <- maak_opzoeker(meetpunten, mp, mpomsch)
  
  f_bins <- function(domain){ 
    bins <- 
      c(min(domain),
        domain %>% 
          quantile(c(0.01,0.05,0.1,0.3,0.7,0.9,0.95,0.99)) %>% 
          signif(digits = 2),
        max(domain)
      ) %>% 
      unique()
    
    if (length(bins)  <= 1) bins <- c(bins, bins + 0.0001)
    
    bins
  }
  
  fys_chem_per <- reactive({fys_chem %>% filter(datum >= input$datum_sel[1], datum <= input$datum_sel[2])})
  fys_chem_sel <- reactive({fys_chem_per() %>% filter(parnr == input$param_sel)})
  mp_sel <- reactive({
    if (!is.null(input$kaart_marker_click)){
      
      punt <- 
        st_sfc(
          st_point(c(input$kaart_marker_click[[4]], 
                     input$kaart_marker_click[[3]])), 
          crs = 4326)
      
      mp_op_kaart <- 
        meetpunten_leaflet %>% 
        inner_join(fys_chem_sel(), by = "mp")
      
      mp_sel <- 
        mp_op_kaart %>% 
        .[[st_nearest_feature(punt, mp_op_kaart), 1]]
      
    } else {
      mp_sel <- "XXXX"
    }
    mp_sel
  })
  
  # Update parameterselectie
  observe({
    parnrs <- fys_chem_per() %>% pull(parnr) %>% unique() %>% sort()
    
    parnamen <- parameters %>% filter(parnr %in% parnrs) 
    if (!is.null(input$param_group)) parnamen <- parnamen %>% filter(cluster %in% input$param_group)
    
    par_selected <- ifelse(input$param_sel %in% parnamen$parnr, input$param_sel, parnamen$parnr[1])
    
    parnamen_choices <-  parnamen %>% select(parnaamlang, parnr) %>% deframe()
    
    updateSelectInput(inputId = "param_sel", choices = parnamen_choices, selected = par_selected)
  })
  
  output$kaart <- renderLeaflet({
    basiskaart() %>% 
      addPolylines(data = ws_grens, color = "grey", opacity = 1, weight = 2) %>% 
      leaflet.extras::addFullscreenControl()
  })
  
  observe({
    agg_fun <- eval(sym(input$agg_fun))
    
    data_leaflet <- meetpunten_leaflet %>% 
      inner_join(fys_chem_sel()) %>% 
      group_by(mp) %>% 
      arrange(desc(datum)) %>% 
      summarise(detectiegrens = ifelse(any(is.na(detectiegrens)), "", paste0(first(detectiegrens), " ")),
                popup_tekst = glue("<b>Meetpunt:</b> {first(mp)}<br><b>Parameter:</b> {f_parnaam(input$param_sel)}<br><br>{url_pdf(first(mp))}<br><br>{url_csv(first(mp))}"),
                waarde = agg_fun(waarde)) %>% 
      ungroup() %>% 
      arrange(waarde) %>% 
      mutate(label_tekst = glue("{detectiegrens} {format(signif(waarde, 3), decimal.mark = ',', nsmall = 0)} {f_eenheid(input$param_sel)}"))
    
    rev_switch <- !input$param_sel %in% c(10, 11, 12, 14)
    
    pal <- colorBin("RdBu", domain = NULL, bins = f_bins(data_leaflet$waarde), reverse = rev_switch)
    
    if(nrow(data_leaflet) > 0){
      leafletProxy("kaart", data = data_leaflet) %>% 
        leaflet::removeControl("legenda") %>% 
        leaflet::clearMarkers() %>% 
        addCircleMarkers(label = ~label_tekst, popup = ~popup_tekst,
                         fillColor = ~pal(waarde), color = "#555", fillOpacity = 1, opacity = 1, 
                         weight = 1, radius = 8, clusterId = "meetpunten") %>% 
        addLegend(pal = pal, values = ~waarde, opacity = 1, 
                  labFormat = labelFormat(suffix = glue(" {f_eenheid(input$param_sel)}")),
                  layerId = "legenda")
    } else {
      leafletProxy("kaart") %>% 
        leaflet::removeControl("legenda") %>% 
        leaflet::clearMarkers()
    }
  })
  
  output$grafiek_loc <- renderPlot({
    fys_chem %>%
      filter(mp == mp_sel(), parnr == input$param_sel, datum >= lubridate::ymd(20100101) ) %>%
      grafiek_basis(mp = glue("{mp_sel()}"),
                    mpomsch = f_mpomsch(mp_sel()),
                    parnaam = f_parnaam(input$param_sel),
                    eenheid = f_eenheid(input$param_sel))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
