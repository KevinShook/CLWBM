ui <- fluidPage(
  title = "Cold Lake water balance model",
  navset_tab( 
    nav_panel("Set run settings", 
              tags$h1("Cold Lake model settings"),
              textAreaInput("runname", "Model run name", rows = 1),
              shinyDirButton("directory", "Folder select", "Model output folder"),
              textOutput("rundir"),
              
              textAreaInput("description", "Model run description", rows = 1),
              
              dateInput("startdate", "Run start date:", value = "2015-01-01"),
              dateInput("enddate", "Run end date:", value = "2100-12-31"),
              
              fileInput("parameterfile", "Choose model parameter CSV File", accept = ".csv"),
              
              fileInput("withdrawalsfile", "Choose Cold Lake withdrawals CSV File", accept = ".csv"),
              
              fileInput("primroseforcingsfile", "Choose Primrose Lake CRHM forcings CSV File", accept = ".csv"),
              fileInput("interlakeforcingsfile", "Choose Interlake CRHM forcings CSV File", accept = ".csv"),
              fileInput("coldforcingsfile", "Choose Cold Lake CRHM forcings CSV File", accept = ".csv"),
              
              actionButton("saverun", "Save run information"),
              textOutput("saved"),
              
    ),
    nav_panel("Primrose Lake", 
              tags$h1("Run Primrose Lake model"),
              
              actionButton("primrose", "Run Primrose Lake model"),
              DT::dataTableOutput("primrosetable"),
              
              actionButton("plotprimrose", "Plot Primrose Lake"),
              plotlyOutput("primroseplot") 
              
    ),
    nav_panel("Interlake",
              tags$h1("Interlake model"),
              actionButton("interlake", "Run Interlake model"),
              DT::dataTableOutput("interlaketable"),
              
              actionButton("plotinterlake", "Plot Interlake"),
              plotlyOutput("interlakeplot") 
    ),    
    nav_panel("Cold Lake", 
              tags$h1("Run Cold Lake Lake model"),
              actionButton("coldlake", "Run Cold Lake model"),
              DT::dataTableOutput("coldlaketable"),
              
              actionButton("plotcoldlake", "Plot Cold Lake"),
              plotlyOutput("coldlakeplot") 
    ),
    
    nav_panel("Edit parameters",
              tags$h1("Edit model parameters"),
              fileInput("editparams", "Choose parameter CSV File", accept = ".csv"),
              DT::dataTableOutput("modelparams"))
  )
)
