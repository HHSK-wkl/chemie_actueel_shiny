# functie afkomstig uit HHSKwkl

data_online <- function(file, url = "https://www.schielandendekrimpenerwaard.nl/kaart/waterkwaliteit/alle_wkl_metingen/data"){
  url <- paste0(url, "/", file)
  
  temp <- tempfile() 
  download.file(url, temp)
  res <- readRDS(temp)
  unlink(temp)
  res
}

maak_opzoeker <- function(df, key = 1, value = 2){
  key <- dplyr::enquo(key)
  value <- dplyr::enquo(value)
  opzoektabel <- dplyr::select(df, !!key, !!value) %>% dplyr::distinct() %>% tibble::deframe()
  #rm(df)
  function(key){
    unname(opzoektabel[as.character(key)])
  }
  
}

basiskaart <- function(data = NULL, type = c("osm", "cartolight")) {
  type <- rlang::arg_match(type, c("osm", "cartolight"))
  
  leaflet::leaflet(data) %>% 
    {if (type == "osm") {leaflet::addProviderTiles(. ,"OpenStreetMap", group = "Kaart") } else {.}} %>%
    {if (type == "cartolight") {leaflet::addProviderTiles(., "CartoDB.Positron", group = "Kaart") } else {.}} %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Luchtfoto") %>% 
    leaflet::addLayersControl( baseGroups = c("Kaart", "Luchtfoto"), 
                               options = leaflet::layersControlOptions(collapsed = FALSE),
                               position = "topleft")
}

grafiek_basis <- function(data, mp = NULL, mpomsch = NULL, parnaam = NULL, eenheid = NULL, plot_loess = TRUE){
  
  blauw    <- "#0079C2"
  
  #limieten
  range_y <- range(data$waarde, na.rm = TRUE)
  ylimieten <- range_y * c(0, 1.1)
  
  if (range_y[1] * 2 > range_y[2] & range_y[1] != range_y[2]) {ylimieten <- range_y * c(0.95, 1.05)}
  
  #grafiek
  grafiek <- ggplot2::ggplot(data, ggplot2::aes(x = datum, y = waarde))
  
  if (plot_loess) {
    grafiek <- grafiek + 
      ggplot2::geom_smooth(se = TRUE, col = "grey80", linetype = "dashed", 
                           fill = "grey40", alpha = 0.08, fullrange = TRUE)
  }
  
  grafiek <- grafiek +
    ggplot2::geom_line(col = blauw) +
    ggplot2::geom_point(col = blauw) +
    ggplot2::geom_point(data = dplyr::filter(data, detectiegrens == "<"), 
                        pch = 21, col = blauw, fill = "white") + # detectiegrenswaarden
    ggplot2::labs(title = paste0("Meetpunt: ", mp), 
                  subtitle = paste0("Parameter: ", parnaam),
                  x = "datum", 
                  y = eenheid,
                  caption = mpomsch) +
    ggplot2::scale_y_continuous(limits = ylimieten, expand = c(0,0), oob = scales::rescale_none ) +
    ggplot2::scale_x_date(date_breaks = "years", labels = lubridate::year) + 
    hhskthema()
  
  grafiek
  
}

hhskthema <- function(){
  #require(ggplot2)
  hhskthema <- ggplot2::theme_light() + ggplot2::theme(
    
    plot.title =    ggplot2::element_text(color = "grey50", face = "bold", hjust = 0),
    plot.subtitle = ggplot2::element_text(color = "grey50", face = "bold", hjust = 0, size = ggplot2::rel(1.1)),
    plot.caption =  ggplot2::element_text(color = "grey40", face = "italic"),
    
    axis.title =  ggplot2::element_text(color = "grey40"),
    axis.text =   ggplot2::element_text(color = "grey40"),
    axis.ticks =  ggplot2::element_line(color = "grey40"),
    axis.line = ggplot2::element_line(color = "grey40", linewidth = 0.5),
    
    panel.border =     ggplot2::element_blank(),#ggplot2::element_rect(color = hhskblauw, size = 1),
    panel.grid.major = ggplot2::element_line(color = "grey80", linetype = "dotted", linewidth = 0.5),
    panel.grid.minor = ggplot2::element_blank(),#ggplot2::element_line(color = "grey60", linetype = "dotted", size = 0.5),
    
    legend.title = ggplot2::element_text(color = "grey50", face = "bold", hjust = 0),
    legend.text =  ggplot2::element_text(color = "grey40"),
    
    strip.background = ggplot2::element_rect(fill = NA, colour = "grey50"),
    strip.text =       ggplot2::element_text(face = "bold", color = "grey50")
  )
  hhskthema
}

