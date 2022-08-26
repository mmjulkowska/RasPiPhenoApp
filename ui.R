ui <- fluidPage(
  theme = shinytheme("superhero"),
  navbarPage( title = "RasPiPheno App", 
              
  tabPanel("Data Upload", icon=icon("upload"),
    sidebarPanel(
      
      selectizeInput("expType", label="What system did you use to collect data?",
                     choices = c("PhenoRig", "PhenoCage"), multiple = F),
      
      fileInput("csv.raspi",
                label="Upload Raspberry Pi derived CSV files here",
                multiple = TRUE),
      
      uiOutput("timeSTART"),
      
      fileInput("csv.meta",
                label="Upload the decoding  Meta-data file here",
                accept = c('text / csv', '.csv', 'text/comma-separated-values')),
      
      checkboxInput("FWCheck", label = "My Meta-data contains collumn with Fresh Weight informatio", value = F),
      uiOutput("FWColumn"),
      
      actionButton("MergeData", label = "Merge Data")
      # end of sidebar panel
    ),
  
    
    
    mainPanel(navbarPage(
      ">> Data <<",
      tabPanel("Raspberry Pi data", icon = icon("flask"),
               verbatimTextOutput("uploaded_RasPi_data_report"),
               dataTableOutput("Data_tabl1")),
      tabPanel("Meta data", icon = icon("ruler"),
               verbatimTextOutput("uploaded_metadata_report"),
               dataTableOutput("Data_tabl2")),
      tabPanel("Decoded data", icon = icon("wand-magic-sparkles"),
               verbatimTextOutput("merged_data_report"),
               uiOutput("mergedtable_button"),
               dataTableOutput("Data_tabl3"))
    ))
    # end of Tab1
  ),
  
  tabPanel("Vizual Curation", icon=icon("eye"),
    sidebarPanel(
      uiOutput("Color_per")
             
    # end of sidebar panel
    ), 
    mainPanel(navbarPage("Visualization",
      tabPanel("Timeseries Graph", icon=icon("chart-line"),
               uiOutput("graph_over_time"))
    ))
    # end of Tab2
)
# end of App - do not move!
))
