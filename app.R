
library(shiny)
library(shinyjs)
library(shinyvalidate)
library(DT) 
library(bslib)
library(shinyFiles)
library(plotly)
library(tidyr)

# Define UI for data upload app ----
ui <- fluidPage(
  title = "Cold Lake water balance model",
   navset_tab( 
    nav_panel("Set run settings", 
    tags$h1("Cold Lake model settings"),
    textAreaInput("runname", "Model run name", rows = 1),

    
    textAreaInput("description", "Model run description", rows = 1),
   
    dateInput("startdate", "Run start date:", value = "2015-01-01"),
    dateInput("enddate", "Run end date:", value = "2100-12-31"),
     
    fileInput("parameterfile", "Choose model parameter CSV File", accept = ".csv"),
    
    tags$h2("Temporary file location"),
    textInput("rundir","Model output folder", value = "/tmp"),
    
    tags$h2("Model forcing data files"),
            
    fileInput("withdrawalsfile", "Choose Cold Lake withdrawals CSV File", accept = ".csv"),
    
    fileInput("primroseforcingsfile", "Choose Primrose Lake CRHM forcings CSV File", accept = ".csv"),
    fileInput("interlakeforcingsfile", "Choose Interlake CRHM forcings CSV File", accept = ".csv"),
    fileInput("coldforcingsfile", "Choose Cold Lake CRHM forcings CSV File", accept = ".csv"),
    
    actionButton("saverun", "Save run information to server"),
    textOutput("saved"),
    
    downloadButton("downloadrun", "Download run info to your computer")
    

    ),
    nav_panel("Primrose Lake", 
      tags$h1("Run Primrose Lake model"),

      actionButton("primrose", "Run Primrose Lake model"),
      DT::dataTableOutput("primrosetable"),
    
      actionButton("plotprimrose", "Plot Primrose Lake"),
      plotlyOutput("primroseplot"), 
      downloadButton("downloadPrimroseLake", "Download Primrose Lake output")
    
    ),
    nav_panel("Interlake",
      tags$h1("Interlake model"),
      actionButton("interlake", "Run Interlake model"),
      DT::dataTableOutput("interlaketable"),
    
      actionButton("plotinterlake", "Plot Interlake"),
      plotlyOutput("interlakeplot"),
      downloadButton("downloadInterlake", "Download Interlake output")
    ),    
    nav_panel("Cold Lake", 
      tags$h1("Run Cold Lake Lake model"),
      actionButton("coldlake", "Run Cold Lake model"),
      DT::dataTableOutput("coldlaketable"),
      
      actionButton("plotcoldlake", "Plot Cold Lake"),
      plotlyOutput("coldlakeplot"),
      downloadButton("downloadColdLake", "Download Cold Lake output")

    ),
      
    nav_panel("Edit parameters",
      tags$h1("Edit model parameters"),
      fileInput("editparams", "Choose parameter CSV File", accept = ".csv"),
      DT::dataTableOutput("modelparams"))
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  source("global.R")
    
    output$runname <- renderText({ input$runname })
    output$description <- renderText({ input$description })
    
    output$rundir <- renderText({
      file1 <- input$parameterfile
      
    })
    
    output$saved <- renderText({
        file1 <- input$parameterfile
        file2 <- input$withdrawalsfile
        file3 <- input$primroseforcingsfile
        file4 <- input$interlakeforcingsfile
        file5 <- input$coldforcingsfile
        parameter_file <- file1$name
        withdrawal_file <- file2$name
        primrose_forcings_file <- file3$name
        interlake_forcings_file <- file4$name
        cold_forcings_file <- file5$name        
        startdate <- as.character(input$startdate)
        enddate <- as.character(input$enddate)
        rundir <- input$rundir
        runname <- input$runname
        description <- input$description
        save_time <- as.character(Sys.time())
        settings_file <-  paste0(rundir, "/", runname, "_settings.csv")
        
     setting_name <- c("Save time", "Run name", "Description", "Start date", 
                     "End date", "Parameter file", "Withdrawals file", 
                     "Primrose Lake forcings file", "Interlake forcings file", 
                     "Cold Lake forcings file")
     setting_value <- c(save_time, runname , description, startdate, enddate, 
                        parameter_file, withdrawal_file, primrose_forcings_file,
                        interlake_forcings_file, cold_forcings_file)
     df <- data.frame(setting_name, setting_value)
     write.csv(df, file = settings_file, row.names = FALSE)
     cat("Run settings information saved")
    }) |>
        bindEvent(input$saverun)

    
    output$downloadrun <- downloadHandler(
      filename = function() { paste(input$runname,"_settings.csv",sep="") },
      content = function(file) {
        file1 <- input$parameterfile
        file2 <- input$withdrawalsfile
        file3 <- input$primroseforcingsfile
        file4 <- input$interlakeforcingsfile
        file5 <- input$coldforcingsfile
        parameter_file <- file1$name
        withdrawal_file <- file2$name
        primrose_forcings_file <- file3$name
        interlake_forcings_file <- file4$name
        cold_forcings_file <- file5$name        
        startdate <- as.character(input$startdate)
        enddate <- as.character(input$enddate)
        rundir <- input$rundir
        runname <- input$runname
        description <- input$description
        save_time <- as.character(Sys.time())
        settings_file <-  paste0(rundir, "/", runname, "_settings.csv")
        
        setting_name <- c("Save time", "Run name", "Description", "Start date", 
                          "End date", "Parameter file", "Withdrawals file", 
                          "Primrose Lake forcings file", "Interlake forcings file", 
                          "Cold Lake forcings file")
        setting_value <- c(save_time, runname , description, startdate, enddate, 
                           parameter_file, withdrawal_file, primrose_forcings_file,
                           interlake_forcings_file, cold_forcings_file)
        df <- data.frame(setting_name, setting_value)
        write.csv(df, file, row.names=F)
      }
    ) 
    
    
    output$primrosetable <-  DT::renderDataTable({ 
        startdate <- input$startdate
        enddate <- input$enddate
        
        file1 <- input$parameterfile
        file2 <- input$primroseforcingsfile
        file3 <- input$withdrawalsfile
        parameters <- read.csv(file1$datapath, header = TRUE)
        primroseforcings <- read.csv(file2$datapath, header = TRUE) 

        runname <- input$runname
        rundir <- input$rundir
   
        
        vals <- run_primrose(startdate, enddate, parameters, primroseforcings, runname, rundir)
        return(vals)
    }) |>
        bindEvent(input$primrose)
    
    output$primroseplot <-  renderPlotly({ 
        runname <- input$runname
        rundir <- input$rundir
        infile <- paste0(rundir, "/", runname, "_Primrose_Lake.csv")
        primrose <- read.csv(file = infile, header = TRUE)
        primrose$date <- as.Date(primrose$date)
        file1 <- input$parameterfile
        parameters <- read.csv(file1$datapath, header = TRUE)
        primrose_outlet_el <- parameters$Value[5]
        
        p <- primrose |> 
            ggplot(aes(date, elevation)) + 
            geom_line() + 
            geom_abline(slope = 0, 
                        intercept = primrose_outlet_el, 
                        colour = "red" ) +
            ggtitle("Primrose Lake simulation") +
            ylab("Surface elevation (m)") +
            xlab("Date")
        
        graph_file <- paste0(rundir, "/", runname, "_Primrose_Lake_elevation.png")
        ggsave(file = graph_file, width = 8, height = 5)
        ggplotly(p) 

    }) |>
        bindEvent(input$plotprimrose)
    
    output$downloadPrimroseLake <- downloadHandler(
      filename = function() { paste(input$runname,"_PrimroseLake.csv",sep="") },
      content = function(file) {
        runname <- input$runname
        rundir <- input$rundir
        infile <- paste0(rundir, "/", runname, "_Primrose_Lake.csv")
        primrose <- read.csv(file = infile, header = TRUE)
        write.csv(primrose, file, row.names=F)
      }
    ) 
    
    
    output$interlaketable <-  DT::renderDataTable({ 
      startdate <- input$startdate
      enddate <- input$enddate
      
      file1 <- input$parameterfile
      file2 <- input$interlakeforcingsfile
      parameters <- read.csv(file1$datapath, header = TRUE)
      interlakeforcings <- read.csv(file2$datapath, header = TRUE) 
      
      runname <- input$runname
      rundir <- input$rundir
      
      
      vals <- run_interlake(startdate, enddate, parameters, interlakeforcings, runname, rundir)
      return(vals)
    }) |>
      bindEvent(input$interlake)
    
    
    output$interlakeplot <-  renderPlotly({ 
      runname <- input$runname
      rundir <- input$rundir
      infile <- paste0(rundir, "/", runname, "_Interlake.csv")
      interlake <- read.csv(file = infile, header = TRUE)
      interlake$date <- as.Date(interlake$date)
      interlake_tall <- interlake %>% pivot_longer(cols = -date, names_to = "flow", values_to = "flowrate")
      p <- 
        interlake_tall |> 
        ggplot(aes(date, flowrate, colour = flow)) + 
        geom_line() +
        ggtitle("Interlake simulation") +
        ylab("Daily discharge (m³/s)") +
        xlab("Date")
      
      graph_file <- paste0(rundir, "/", runname, "_Interlake_flows.png")
      ggsave(file = graph_file, width = 8, height = 5)
      ggplotly(p) 
      
    }) |>
      bindEvent(input$plotinterlake)
    
    output$downloadInterlake <- downloadHandler(
      filename = function() { paste(input$runname,"_Interlake.csv",sep="") },
      content = function(file) {
        runname <- input$runname
        rundir <- input$rundir
        infile <- paste0(rundir, "/", runname, "_Interlake.csv")
        inter <- read.csv(file = infile, header = TRUE)
        write.csv(inter, file, row.names=F)
      }
    )  
    
    
    output$coldlaketable <-  DT::renderDataTable({ 
      startdate <- input$startdate
      enddate <- input$enddate
      
      
      file1 <- input$parameterfile
      file2 <- input$coldforcingsfile
      file3 <- input$withdrawalsfile
      
      parameters <- read.csv(file1$datapath, header = TRUE)
      coldlakeforcings <- read.csv(file2$datapath, header = TRUE) 
      monthlywithdrawals <- read.csv(file3$datapath, header = TRUE) 
      
      runname <- input$runname
      rundir <- input$rundir
      
      
      vals <- run_coldlake(startdate, enddate, parameters, coldlakeforcings, monthlywithdrawals, runname, rundir)
      return(vals)
    }) |>
      bindEvent(input$coldlake)
    
    
    output$coldlakeplot <-  renderPlotly({ 
      runname <- input$runname
      rundir <- input$rundir
      infile <- paste0(rundir, "/", runname, "_Cold_Lake.csv")
      cold <- read.csv(file = infile, header = TRUE)
      cold$date <- as.Date(cold$date)
      p <- 
        cold |> 
        ggplot(aes(date, elevation)) + 
        geom_line() +
        ggtitle("Cold Lake simulation") +
        ylab("Surface elevation (m)") +
        xlab("Date")
      
      graph_file <- paste0(rundir, "/", runname, "_Cold_Lake_elevation.png")
      ggsave(file = graph_file, width = 8, height = 5)
      ggplotly(p) 
      
    }) |>
      bindEvent(input$plotcoldlake)
    
    output$downloadColdLake <- downloadHandler(
      filename = function() { paste(input$runname,"_ColdLake.csv",sep="") },
      content = function(file) {
        runname <- input$runname
        rundir <- input$rundir
        infile <- paste0(rundir, "/", runname, "_Cold_Lake.csv")
        cold <- read.csv(file = infile, header = TRUE)
        write.csv(cold,file,row.names=F)
      }
    )  
    
    output$modelparams <- DT::renderDT({
        req(input$editparams)
    
        df <- read.csv(input$editparams$datapath,
                       header = TRUE)
      return(df)
    }, 
    editable = "cell", 
    extensions = 'Buttons',
    options = list(scrollX = TRUE
                             , pageLength = 15
                             , dom = 'Blfrtip'
                             ,buttons = c('copy', 'csv', 'print')
    ),
    server = FALSE, rownames = FALSE) 
}

# Run the application 
shinyApp(ui = ui, server = server)
