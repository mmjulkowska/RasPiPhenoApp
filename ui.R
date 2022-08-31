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
               dataTableOutput("Data_tabl3")),
      tabPanel("Vizual Curation", icon=icon("eye"),
               uiOutput("color_original"),
               plotlyOutput("graph_over_time"))
    ))
    # end of Tab1
  ),

  tabPanel("Data Smoothing", icon=icon("blender"),
           sidebarPanel(
             selectizeInput("smoothType", label = "What kind of smoothing would you like to use?",
                            choices = c("SmoothSpline", "SomethingElse"), multiple=F),
             sliderInput("degFree", label="How many degrees of freedom would you like to use?", min = 1, max=20, step = 1, value = 2),
             actionButton("SmoothGo", label = "Smooth all samples")
           ),
           mainPanel(navbarPage(
             ">> Smooooth <<",
             tabPanel("smooth design", icon = icon("snowplow"),
               uiOutput("Choose_smooth_sample"),
               plotOutput("Smoothed_graph_one_sample")),
             tabPanel("smooth data", icon=icon("bug-slash"),
                      uiOutput("Smooth_table_button"),
                      dataTableOutput("Smooth_table")),
             tabPanel("smooth graph", icon=icon("cloud-sun"),
                      uiOutput("color_smooth"),
                      plotlyOutput("all_smooth_graph"))
             
           ))
           # end of Tab3
  )
# end of App - do not move!
))