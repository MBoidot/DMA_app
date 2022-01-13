library(shiny)
library(xlsx)
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)
library(zoo)
library(lubridate)
library(shinyFiles)
library(htmltools)
library(colourpicker)


theme_set(theme_bw(10))
theme_update(
  panel.grid.major = element_line(colour = "#b2b2b2", size = 0.5),
  panel.grid.minor = element_line(colour = "#c5c5c5", size = 0.5),
  legend.title = element_text(size = 18),
  axis.title.x = element_text(size = 20),
  axis.title.y = element_text(size = 20, angle = 90, vjust = 1.5),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 16),
  legend.text = element_text(size = 16),
  plot.title = element_text(size = 25, face = "bold", vjust =0.5),
  strip.text.x = element_text(size = 14, face = "bold"),
  strip.text.y = element_text(size = 14, face = "bold")
)


ui <- fluidPage(navbarPage(
  "DMA-App",
  tabPanel(
    "Single file treatment",
    pageWithSidebar(
      headerPanel("I'm the header panel"),
      sidebarPanel(
        width = 2,
        fileInput("file_in", "Data file", multiple = FALSE),
        uiOutput("columnsX"),
        uiOutput("columnsY"),
      ),
      
      mainPanel(tabsetPanel(
        tabPanel(
          "Plot",
          
          fluidRow(column(
            12,
            h3("Display properties"),
            
            column(
              2,
              sliderInput(
                "point_size",
                "Point size",
                min = 0,
                max = 6,
                value = 0.25,
                step = 0.05
              )
            ),
            
            #column(4,selectInput("col_theme", "Color palette:", choices =  list(Brewer = c(`Set1` = 'Br_S1', `Set2` = 'Br_S2', `Set3` = 'Br_S3', `Spectral` = 'Br_Spectral'),
            #                                                                    Viridis = c(`Viridis` = 'Vir_vir',`Plasma` = 'Vir_plas',`Magma` = 'Vir_mag')))),
            
            
            
            column(
              2,
              sliderInput(
                "axlabel_size",
                "Axis label font size",
                min = 6,
                max = 24,
                value = 12
              )
            ),

            column(2, sliderInput("graph_width", "Width", min=0.1, max=1, value=1)),
            column(2, sliderInput("graph_height", "Height", min=0.1, max=1, value=1)),
            column(2, colourInput("col", "Select colour", "purple")),
            ),
            
          ),
          hr(),
          fluidRow(column(
            12,
            h3("Plot"),
            
          
          plotOutput("reduced_plot_origin", width="100%"),
          hr()))
        ),
        
        tabPanel("Data",
                 fluidRow(column(
                   12,
                   h3("Display properties"),
                   hr(),
                   dataTableOutput("dataset_table")
                 ))),
        tabPanel(
          "Test",
          fluidRow(
            column(12,
                   h2("Test render UI variables"),
                   hr()),
            h3("textOutput test_colnames"),
            tableOutput("test_colnames"),
            h3("dataset_class"),
            textOutput("dataset_class"),
            h3("Testing space"),
            h3("Testing space")
            
          )
        )
      ))
    )
  ),
  
  tabPanel("Help")
))

Sys.setlocale('LC_ALL', 'C')
server = function(input, output, session) {
  #lecture des donnees
  DataSet <- reactive({
    inFile <- input$file_in
    if (is.null(inFile)) {
      return(NULL)
    } else {
      column_names <-
        c(
          "T",
          "Freq",
          "Cycles",
          "temp",
          "D",
          "Strain",
          "Dmin",
          "Dmax",
          "dynD",
          "dynS",
          "StatD",
          "Fpp",
          "Strepp",
          "Fmin",
          "Fmax",
          "dynF",
          "Stredyn",
          "StatF",
          "K1",
          "delta",
          "K2",
          "tdelta",
          "K3",
          "E1",
          "dE",
          "E2",
          "tandE",
          "E3",
          "J1",
          "J2",
          "J3"
        )
      lines <- readLines(inFile$datapath, warn = FALSE)
      data <-
        as.data.frame(matrix(unlist(strsplit(lines, '\t')), ncol = 59, byrow =
                               T))
      data <- data[, c(12:42)]
      colnames(data) <- column_names
      for (i in 1:length(data)) {
        data[, i] <- as.numeric(data[, i])
      }
      return(data)
    }
  })
  
  output$reduced_plot_origin <- renderPlot({
    req(input$file_in)
    ddm <- DataSet()
    #col_pal <- color_palette()
    g <-
      ggplot(ddm, aes_string(x = as.name(input$X_Axis), y = as.name(input$Y_Axis))) +
      geom_point(size = input$point_size, color = input$col)
    
    #shaping
    g <-  g + theme_bw()
    #g <- g + col_pal
    g <- g + theme(
        axis.text.x = element_text(size = input$axlabel_size,angle = 0,hjust = 0,vjust = 0.5),
        axis.text.y = element_text(size = input$axlabel_size,hjust = 0.5,vjust = 0.5),
        axis.title.x = element_text(size = input$axlabel_size +2),
        axis.title.y = element_text(size = input$axlabel_size +2),
        plot.title = element_text(size = 20,face = "bold",vjust = 1),
        strip.text.x = element_text(size = input$strip_size,face = "bold",color = "white"),
        strip.text.y = element_text(size = input$strip_size,face = "bold",color = "white"),
        strip.background = element_rect(fill = "#2d2d2d")
      )
    width = 50
    height = 100
    
    #legend.position="none"),
    #g <- g + xlab(as.name(input$X_Axis))
    #g <- g + ylab(as.name(input$Y_Axis))
    #ggsave("plot.pdf", g,width =input$Down_width/300,height = input$Down_height/300)
    
    print(g)
  })
  
  
  
  output$dataset_class <- renderText({
    df <- DataSet()
    return(class(df))
  })
  
  output$dataset_table <- renderDataTable({
    DataSet()
  })
  
  output$columnsX <- renderUI({
    df <- DataSet()
    varSelectInput("X_Axis",
                   label = "Select X Variable",
                   data = df,
                   selected = "Cycles")
  })
  
  
  output$columnsY <- renderUI({
    df <- DataSet()
    varSelectInput("Y_Axis",
                   label = "Select Y Variable",
                   data = df,
                   selected = "Strain")
  })
  
  
  
  # color_palette2 <- reactive({
  #   plottype <- input$pl_type
  #   if (input$col_theme2 == "Vir_vir"){
  #     str_col_palette <- scale_color_viridis(discrete = TRUE,option="viridis")
  #   } else if (input$col_theme2 == "Vir_mag") {
  #     str_col_palette <- scale_color_viridis(discrete = TRUE,option="magma")
  #   } else if (input$col_theme2 == "Vir_plas") {
  #     str_col_palette <- scale_color_viridis(discrete = TRUE,option="plasma")
  #   } else if (input$col_theme2 == "Br_S1") {
  #     str_col_palette <- scale_color_brewer(palette="Set1")
  #   } else if (input$col_theme2 == "Br_S2") {
  #     str_col_palette <- scale_color_brewer(palette="Set2")
  #   } else if (input$col_theme2 == "Br_S3") {
  #     str_col_palette <- scale_color_brewer(palette="Set3")
  #   } else if (input$col_theme2 == "Br_Spectral") {
  #     str_col_palette <- scale_color_brewer(palette="Spectral")
  #   }
  #
  #   return(str_col_palette)
  # })
  # color_palette <- reactive({
  #   plottype <- input$pl_type
  #   if (input$col_theme == "Vir_vir"){
  #     str_col_palette <- scale_color_viridis(discrete = TRUE,option="viridis")
  #   } else if (input$col_theme == "Vir_mag") {
  #     str_col_palette <- scale_color_viridis(discrete = TRUE,option="magma")
  #   } else if (input$col_theme == "Vir_plas") {
  #     str_col_palette <- scale_color_viridis(discrete = TRUE,option="plasma")
  #   } else if (input$col_theme == "Br_S1") {
  #     str_col_palette <- scale_color_brewer(palette="Set1")
  #   } else if (input$col_theme == "Br_S2") {
  #     str_col_palette <- scale_color_brewer(palette="Set2")
  #   } else if (input$col_theme == "Br_S3") {
  #     str_col_palette <- scale_color_brewer(palette="Set3")
  #   } else if (input$col_theme == "Br_Spectral") {
  #     str_col_palette <- scale_color_brewer(palette="Spectral")
  #   }
  #
  #   return(str_col_palette)
  # })
  
}

shinyApp(
  ui = ui,
  server = server,
  options = list(launch.browser = TRUE)
)
