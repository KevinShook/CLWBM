server <- function(input, output) {
  source("global.R")
  
  output$runname <- renderText({ input$runname })
  output$description <- renderText({ input$description })
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "directory", roots = volumes, 
                 restrictions = system.file(package = "base"), 
                 allowDirCreate = TRUE)
  
  output$rundir <- renderText({
    if (is.integer(input$directory)) {
      # cat("No directory has been selected")
    } else {
      parseDirPath(volumes, input$directory)
    }
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
    rundir <- parseDirPath(volumes, input$directory)
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
  
  
  output$primrosetable <-  DT::renderDataTable({ 
    startdate <- input$startdate
    enddate <- input$enddate
    
    file1 <- input$parameterfile
    file2 <- input$primroseforcingsfile
    file3 <- input$withdrawalsfile
    parameters <- read.csv(file1$datapath, header = TRUE)
    primroseforcings <- read.csv(file2$datapath, header = TRUE) 
    
    runname <- input$runname
    rundir <- parseDirPath(volumes, input$directory)
    
    
    vals <- run_primrose(startdate, enddate, parameters, primroseforcings, runname, rundir)
    return(vals)
  }) |>
    bindEvent(input$primrose)
  
  output$primroseplot <-  renderPlotly({ 
    runname <- input$runname
    rundir <- parseDirPath(volumes, input$directory)
    infile <- paste0(rundir, "/", runname, "_Primrose_Lake.csv")
    primrose <- read.csv(file = infile, header = TRUE)
    primrose$date <- as.Date(primrose$date)
    p <- 
      primrose |> 
      ggplot(aes(date, elevation)) + 
      geom_line() +
      ggtitle("Primrose Lake simulation") +
      ylab("Surface elevation (m)") +
      xlab("Date")
    
    graph_file <- paste0(rundir, "/", runname, "_Primrose_Lake_elevation.png")
    ggsave(file = graph_file, width = 8, height = 5)
    ggplotly(p) 
    
  }) |>
    bindEvent(input$plotprimrose)
  
  
  output$interlaketable <-  DT::renderDataTable({ 
    startdate <- input$startdate
    enddate <- input$enddate
    
    file1 <- input$parameterfile
    file2 <- input$interlakeforcingsfile
    parameters <- read.csv(file1$datapath, header = TRUE)
    interlakeforcings <- read.csv(file2$datapath, header = TRUE) 
    
    runname <- input$runname
    rundir <- parseDirPath(volumes, input$directory)
    
    
    vals <- run_interlake(startdate, enddate, parameters, interlakeforcings, runname, rundir)
    return(vals)
  }) |>
    bindEvent(input$interlake)
  
  
  output$interlakeplot <-  renderPlotly({ 
    runname <- input$runname
    rundir <- parseDirPath(volumes, input$directory)
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
  
  
  output$coldlaketable <-  DT::renderDataTable({ 
    startdate <- input$startdate
    enddate <- input$enddate
    
    file1 <- input$parameterfile
    file2 <- input$coldlakeforcingsfile
    parameters <- read.csv(file1$datapath, header = TRUE)
    interlakeforcings <- read.csv(file2$datapath, header = TRUE) 
    
    runname <- input$runname
    rundir <- parseDirPath(volumes, input$directory)
    
    
    vals <- run_interlake(startdate, enddate, parameters, interlakeforcings, runname, rundir)
    return(vals)
  }) |>
    bindEvent(input$coldlake)
  
  
  output$coldlakeplot <-  renderPlotly({ 
    runname <- input$runname
    rundir <- parseDirPath(volumes, input$directory)
    infile <- paste0(rundir, "/", runname, "_Colde_Lake.csv")
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

