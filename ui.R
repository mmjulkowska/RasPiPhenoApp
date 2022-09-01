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
  
  # # # # # # # # # # # # # # # # TAB 2 # # # # # # # # # # # # # # # # # # # # # # # # 

  tabPanel("Data Smoothing", icon=icon("blender"),
           sidebarPanel(
             selectizeInput("smoothType", label = "What kind of smoothing would you like to use?",
                            choices = c("SmoothSpline", "LowessFit", "Polynomial"), multiple=F),
             uiOutput("degFreeUI"),
             uiOutput("KnotesUI"),
             actionButton("SmoothGo", label = "Smooth all samples")
           ),
           mainPanel(navbarPage(
             "Smooooth",
             tabPanel("smooth design", icon = icon("snowplow"),
               uiOutput("Choose_smooth_sample"),
               plotOutput("Smoothed_graph_one_sample")),
             tabPanel("smooth data", icon=icon("bug-slash"),
                      uiOutput("Smooth_table_button"),
                      dataTableOutput("Smooth_table")),
             tabPanel("smooth graph", icon=icon("cloud-sun"),
                      uiOutput("color_smooth"),
                      plotlyOutput("all_smooth_graph"),
                      uiOutput("Smooth_graph_button"))
             
           ))
           # end of Tab2
  ),
  
  # # # # # # # # # # # # # # # # TAB 3 # # # # # # # # # # # # # # # # # # # # # # # # 
  
  tabPanel("Growth Rate", icon=icon("seedling"),
           sidebarPanel(
             selectizeInput("dataGrowth", label = "What data to use for growth rate calculations?", 
                            choices = c("Original data", "Smooth data"), multiple = F),
             selectizeInput("GrowthType", "Growth rate type to be calculated:",
                            choices=c("Over whole experiment", "Step-wise")),
             uiOutput("interval"),
             uiOutput("step"),
             actionButton("GoGrowth", label = "Calculate Growth Rate")),
           
           mainPanel(navbarPage("Grrrrrow",
                                tabPanel("Growth Table",
                                         uiOutput("Growth_table_button"),
                                         dataTableOutput("Growth_table")),
                                tabPanel("Growth Graph",
                                         fluidRow(
                                          column(4,uiOutput("Growth_Color_button")),
                                          column(4, uiOutput("Growth_Xaxis")),
                                          column(4, checkboxInput("Rtoolow", "Exclude samples with low R2"),
                                                 uiOutput("Rhowlowui"))),
                                          hr(),
                                         plotOutput("Growth_Graph"),
                                         uiOutput("Growth_graph_button"))
           ))
           # end of Tab3
  )
  
# end of App - do not move!
))