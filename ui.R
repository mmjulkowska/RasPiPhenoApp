ui <- fluidPage(
  theme = shinytheme("superhero"),
  navbarPage( title = "RasPiPheno App", 
              
  tabPanel("Data Upload", icon=icon("upload"),
    sidebarPanel(
      fileInput("csv.raspi",
                label="Upload Raspberry Pi derives CSV files here",
                multiple = TRUE),
      
      fileInput("csv.meta",
                label="Upload the decoding  Meta-data file here",
                accept = c('text / csv', '.csv', 'text/comma-separated-values')),
      checkboxInput("FWCheck", label = "My Meta-data contains collumn with Fresh Weight informatio", value = F),
      uiOutput("FWColumn"),
      
      checkboxInput("SVCheck", label = "My RasPi data contains multiple side-view of one plant", value = F),
      uiOutput("SVNumber"),
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
      tabPanel("Decoded data", icon = icon("magic"),
               verbatimTextOutput("merged_data_report"),
               dataTableOutput("merged_data"))
    ))
    # end of Tab1
  ),
  
  tabPanel("Vizual Curation", icon=icon("eye"),
    sidebarPanel(
             
    # end of sidebar panel
    ), 
    mainPanel(navbarPage("Visualization",
      tabPanel("Timeseries Graph", icon=icon("chart-line"))
    ))
    # end of Tab2
)
# end of App - do not move!
))