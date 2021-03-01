library(shiny)
library(raster)
library(sf)
library(tidyverse)
library(fields)
library(akima)
library(viridisLite)
library(plotKML)
library(leaflet)

ui <- fluidPage(
  
  titlePanel("LakeInterpolator"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("Lake XYZ data"),
      tags$p("Load spatial vector file containing x- and y-coordinates and depth.", 
             tags$br(), 
             "This file can be created from a normal textfile using the ",
             strong("TextToSpatial"), " app."),

      # Input: Select a file ----
      fileInput("xyz", "Choose spatial point vector file",
                multiple = FALSE,
                accept = c(".sqlite", ".kml", ".kmz", ".shp")),
      
      h3("Lake polygon"),
      tags$p("Load spatial vector file containing the lake polygon.", 
             tags$br(), "This file can be obtained from OpenStreetMap (https://www.openstreetmap.org/)",
             tags$br(), "or, it can be created by drawing a polygon in Google Earth"),

      fileInput("poly", "Choose spatial polygon vector file",
                multiple = FALSE,
                accept = c(".sqlite", ".kml", ".kmz", ".shp")),
      
      tags$hr(),
      
      h3("Interpolation settings"),
      tags$p("Choose method for interpolation.",
             tags$br(), "Linear method is simpler and faster",
             tags$br(), "Thin-plate spline method is slower but produces smoother outputs"),
      
      radioButtons("method", "Interpolation method",
                   choices = c(Linear = "lin",
                               TPS = "tps"),
                   selected = "lin"),
      
      numericInput("res", "Grid resolution", 5),
      
      tags$hr(),
      
      h3("Export files to KML"),
      p("Export files for viewing in Google Earth"),
      
      downloadButton("rasterfile", "Download raster"),
      
      downloadButton("contourfile", "Download contour-lines")
    ),
    
    mainPanel(
      
      h2("Map"),
      
      leafletOutput("plot")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  spat_preproc <- reactive({
    
    req(input$xyz)
    req(input$poly)
    
    lake_xyz <- st_read(input$xyz$datapath)
    
    lake_poly <- st_read(input$poly$datapath) %>% 
      st_zm() %>% 
      st_transform(st_crs(lake_xyz)) 
      
    lake_poly_points <- lake_poly %>%
      st_cast("MULTILINESTRING") %>%
      st_cast("LINESTRING") %>%
      st_line_sample(density = 0.05) %>%
      st_cast("POINT") %>%
      st_as_sf() %>%
      mutate(z = 0) %>%
      dplyr::select(z) %>%
      rename(GEOMETRY = x)

    lake_xyz_all_sf <- rbind(lake_poly_points, st_intersection(dplyr::select(lake_xyz, z), st_geometry(lake_poly)))
    
    tmp_raster <- raster(lake_xyz_all_sf, res = c(input$res, input$res), crs = st_crs(lake_xyz_all_sf)$proj4string)
    
    lake_xyz_all_raster <- rasterize(as(lake_xyz_all_sf, "Spatial"), tmp_raster, field = "z", fun = mean)
    names(lake_xyz_all_raster) <- "z"

    lake_xyz_all <- as.data.frame(lake_xyz_all_raster, xy = TRUE, na.rm = TRUE) %>% 
      st_as_sf(coords = c("x", "y"), crs = st_crs(tmp_raster))
    
    return(list("poly" = lake_poly, 
                "xyz" = lake_xyz, 
                "zmax" = max(lake_xyz_all$z, na.rm = TRUE), 
                "lake_xyz_all" =  lake_xyz_all,
                "tmp_raster" = tmp_raster))
  })
  
  spat_interp <- reactive({
    
    req(input$xyz)
    req(input$poly)
    req(input$method)
    
    lake_xyz_all <- spat_preproc()$lake_xyz_all
    tmp_raster <- spat_preproc()$tmp_raster
    lake_poly <- spat_preproc()$poly
    
    if(input$method == "lin"){
      tmp_raster_coords <- coordinates(tmp_raster)
      tmp_raster_bbox <- as.numeric(st_bbox(tmp_raster))
      
      n_xaxis <- (tmp_raster_bbox[3] - tmp_raster_bbox[1])/input$res
      n_yaxis <- (tmp_raster_bbox[4] - tmp_raster_bbox[2])/input$res
      
      interpolator <- interp(as(lake_xyz_all, "Spatial"), z = "z", nx=n_xaxis, ny=n_yaxis, duplicate = "mean")
      
      raster_interp <- raster(interpolator)
      
      raster_interp_mask <- mask(raster_interp, as(lake_poly, "Spatial"))
      
      crs(raster_interp_mask) <- st_crs(lake_xyz_all)$proj4string
      
    }else{
      
      tps <- Tps(st_coordinates(lake_xyz_all), lake_xyz_all$z)
      
      raster_interp <- interpolate(tmp_raster, tps)
      
      raster_interp_mask <- mask(raster_interp, as(lake_poly, "Spatial"))
      
    }
    
    return(list("rast" = raster_interp_mask))
    
  })

  output$plot <- renderLeaflet(
    {
      req(input$xyz)
      req(input$poly)
      req(input$method)
      
      rast_reproj <- projectRaster(spat_interp()$rast, crs="+proj=longlat +datum=WGS84", method='ngb')
      poly_reproj <- st_transform(spat_preproc()$poly, crs="+proj=longlat +datum=WGS84")
      
      pal <- colorNumeric(palette = "viridis", reverse = TRUE, values(rast_reproj), na.color = "transparent")
      
      leaflet() %>% 
        addTiles() %>% 
        addRasterImage(rast_reproj, color = pal, opacity = 0.6) %>% 
        addPolygons(data=poly_reproj, fill = FALSE) %>% 
        addLegend(pal = pal, values = values(rast_reproj), title = "Lake depth (m)", opacity = 1)
    }
  )
  
  output$rasterfile <- downloadHandler(
    filename = function() {
      "interpolated_raster.kmz"
    },
    
    content = function(file) {
      KML(projectRaster(spat_interp()$rast, crs="+proj=longlat +datum=WGS84", method='ngb'), 
          file, col = rev(viridis(10)))
    }
  )
  
  output$contourfile <- downloadHandler(
    filename = function() {
      "contour_lines.kml"
    },
    
    content = function(file) {
      dybde_contour <- rasterToContour(spat_interp()$rast, levels = seq(0, spat_preproc()$zmax, 0.5))
      st_write(st_as_sf(dybde_contour), file)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)