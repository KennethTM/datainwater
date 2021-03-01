library(shiny)
library(sf)
library(tidyverse)
library(leaflet)

ui <- fluidPage(
    
    titlePanel("TextToSpatial"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            h3("Import text file"),
            tags$p("Choose a normal text file containing x- and y-coordinates and depths."),
            
            fileInput("file1", "Choose text File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            tags$p("Does the text file have a header row?"),
            checkboxInput("header", "Header", TRUE),
            
            tags$p("Choose the separator used in the text file."),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = "\t"),
            
            tags$p("Does the text file contain quoting?"),
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            tags$hr(),
            
            h3("Define geography"),
            tags$p("Fill in the column names that contains x- and y-coordinates and depths."),
            
            textInput("x", "Name of x column"),
            textInput("y", "Name of y column"),
            textInput("z", "Name of z column"),
            
            tags$p("Choose the source and target coordinate reference system",
                   tags$br(), "Default source system is that used by Google Earth"),
            
            numericInput("s_srs", "Source EPSG code", 4326),
            numericInput("t_srs", "Target EPSG code", 25832),
            
            tags$hr(),
            
            h3("Filter z-values"),
            tags$p("Removed out-of-range data."),
            
            numericInput("z_min", "Minimum depth", 0),
            numericInput("z_max", "Maximum depth", 38),
            
            tags$hr(),
            h3("Export spatial file"),
            tags$p("Export spatial vector file",
                   tags$br(), "This file can be used subsequently with the ",
                   strong("LakeInterpolator"), " app."),
            
            downloadButton("downloadData", "Download")

        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Data", dataTableOutput("contents")),
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("Map", leafletOutput("plot")))
            
        )
        
    )
)

server <- function(input, output, session) {
    
    df <- reactive({
        
        req(input$file1)

        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
    }
    )
    
    df_to_sf <- reactive({
        
        req(input$x)
        req(input$y)
        req(input$z)
        
        sf <- df() %>%
          select(input$x, input$y, z = input$z) %>% 
          filter(between(z, input$z_min, input$z_max)) %>% 
          st_as_sf(coords = c(input$x, input$y), crs = input$s_srs) %>% 
          st_transform(input$t_srs)
        
        return(sf)
    }
    )
    
    output$contents <- renderDataTable({
        df()
    })
    
    
    output$plot <- renderLeaflet(
        {
            lake_xyz <- df_to_sf()
            
            pal <- colorNumeric(palette = "magma", lake_xyz$z)
            
            lake_xyz %>% 
                st_transform(4326) %>% 
                as("Spatial") %>% 
                leaflet() %>% 
                addTiles() %>% 
                addCircles(color = ~pal(z), radius = 2) %>% 
                addLegend(pal = pal, values = ~z, title = "Lake depth (m)", opacity = 1)
        }
    )
    
    output$summary <- renderPrint(
        df() %>%
            select(x = input$x, y = input$y, z = input$z) %>%
            filter(between(z, input$z_min, input$z_max)) %>% 
            summary()
    )
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(substr(input$file1, 1, nchar(input$file1)-4), ".sqlite", sep = "")
        },
        content = function(file) {
            st_write(df_to_sf(), file)
        }
    )
}

shinyApp(ui, server)