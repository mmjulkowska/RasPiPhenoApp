server <- function(input, output) {
    
    Raspi <- reactive({
        rbindlist(lapply(input$csv.raspi$datapath, fread),
                  use.names = TRUE, fill = TRUE)})
    
    Raspi_clean <-  reactive(if(is.null(input$csv.raspi)) {
        return(NULL)}
        else{
            if(input$expType == "PhenoCage"){
                Raspi <- Raspi() %>% separate(roi, c("Year", "Month", "Day","Hour","Min", "Sec","Raspi","Side","Camera"))
                Raspi$timestamp <- paste0(Raspi$Year, ".", Raspi$Month,".", Raspi$Day, "-", Raspi$Hour, ".", Raspi$Min, ".", Raspi$Sec)
                Raspi$TS2 <- paste0(Raspi$Year, "-", Raspi$Month,"-", Raspi$Day)
                Raspi_clean <- Raspi[,c("area","Year", "Month", "Day","Hour","Min", "Sec","timestamp", "Raspi", "TS2")]
                }
            if(input$expType == "PhenoRig"){
                Raspi <- Raspi() %>% separate(plantID, c("RasPi","Camera", "Year", "Month", "Day","Hour","Min", "Sec", "position"))
                Raspi$timestamp <- paste0(Raspi$Year, ".", Raspi$Month,".", Raspi$Day, "-", Raspi$Hour, ".", Raspi$Min, ".", Raspi$Sec)
                Raspi$TS2 <- paste0(Raspi$Year, "-", Raspi$Month,"-", Raspi$Day, " ", Raspi$Hour, ":", Raspi$Min) 
                Raspi_clean <- Raspi[,c("area","Year", "Month", "Day","Hour","Min", "Sec","timestamp", "RasPi", "position", "TS2")]
                Raspi_clean$position <- as.numeric(as.character(Raspi_clean$position))
            }
            return(Raspi_clean)
     })
    
    
    # Start calculating data table 
    output$Data_tabl1 <- renderDataTable({
            Raspi_clean()
    })
    
    ItemList = reactive(if (is.null(input$csv.meta)) {
        return()
    } else {
        d2 = read.csv(input$csv.meta$datapath)
        return(colnames(d2))
    })
    
    output$FWColumn <- renderUI({
        if ((is.null(ItemList())) | (input$FWCheck == FALSE)) {
            return ()
        } else
            tagList(
                selectizeInput(
                    inputId = "FWCol",
                    label = "Select column containing Fresh Weight",
                    choices = ItemList(),
                    multiple = F
                )
            )
    })
    
    timepointList = reactive(if(is.null(Raspi_clean())){
        return()} else {
            temp <- Raspi_clean()
            return(str_sort(unique(temp$TS2)))
            
    })
    
    output$timeSTART <- renderUI({
        if(is.null(input$csv.raspi)){return() | is.null(timepointList())} else {
            tagList(
                selectizeInput(
                    inputId = "StartTimeExp",
                    label = "Select experiment start timepoint",
                    choices = timepointList(),
                    multiple = F
                )
            )
        }
    })
    
    decoding <- reactive(if (input$MergeData == F) {
        return()
    } else {
        decoding <- read.csv(input$csv.meta$datapath)
        if(input$FWCheck == TRUE){
            FW <- input$FWCol
            decoding <- decoding[ , -which(names(decoding) %in% c(FW))]
        }
        decoding
    })

    output$uploaded_RasPi_data_report <- renderText({
        if(is.null(input$csv.raspi)) {
            return(NULL)}
        else{
            data <- Raspi_clean()
            no_PIs <- length(unique(data$RasPi))
            no_Month <- length(unique(data$Month))
            no_Day <- length(unique(data$Day))
            no_H <- length(unique(data$Hour))
            sentence <- paste("Your Raspberry Pi uploaded data contains images collected over ",no_PIs, " Raspberry Pi devices, 
                              collected over the period of ", no_Month, " months, ", no_Day, " days, and ", no_H, " hours.")
            return(sentence)
        }
    })
    
    output$uploaded_metadata_report <- renderText({
        if(is.null(input$csv.meta)) {
            return(NULL)}
        else{
            meta <- decoding()
            no_Pot <- length(unique(meta$POT))
            no_Geno <- length(unique(meta$Genotype))
            no_Cond <- length(unique(meta$Condition))
            meta_sentence <- paste("Your uploaded Meta-data contains ", no_Pot, " unique Pots, ", no_Geno, "unique genotypes, and ", no_Cond, " unique conditions.")
        }
        
    })
    
    
    # quick glance of the decoding file
    output$Data_tabl2 <- renderDataTable({decoding()})
    
    
    ### merge seven images each day 
    Raspi_unique <-  reactive(if(input$MergeData == F){
        return()} else{
            if(input$expType == "PhenoCage"){
                Raspi_clean <- Raspi_clean()
                Raspi_clean <- Raspi_clean %>% group_by(timestamp) %>% mutate(side.counts = n())
                Raspi_clean <- Raspi_clean %>% group_by(timestamp) %>% mutate(area.total = sum(area))
                meta <- decoding()
                pots <- meta$POT
                times <- length(unique(Raspi_clean$timestamp)) / length(pots)
                decoded_list <- rep(pots, times)
                # calculate time_of_experiment based on # Yr.Month.Day format of input$StartTimeExp
                Raspi_clean$start <- input$StartTimeExp
                Raspi_clean$time.days <- difftime(Raspi_clean$TS2, Raspi_clean$start, units = "days")
                
                Raspi_unique <- unique(Raspi_clean[,c("Raspi", "Month", "Day", "timestamp", "time.days", "side.counts","area.total")])
                Raspi_unique$POT <- decoded_list
                Raspi_decoded <- merge(Raspi_unique, meta, by="POT", all = TRUE) 
                }
            if(input$expType == "PhenoRig"){
                Raspi_clean <- Raspi_clean()
                Raspi_clean$Plant.ID <- paste(Raspi_clean$RasPi, Raspi_clean$position, sep="_")
                # calculate time_of_experiment based on # Yr.Month.Day-Hr.Min format of input$StartTimeExp
                Raspi_clean$start <- input$StartTimeExp
                Raspi_clean$time.min <- difftime(Raspi_clean$TS2, Raspi_clean$start, units = "mins")
                
                Raspi_unique <- unique(Raspi_clean[,c("RasPi", "Plant.ID", "position", "Month", "Day", "Hour","Min", "Sec", "timestamp", "time.min", "area")])
                meta <- decoding()
                meta$Plant.ID <- paste(meta$RasPi, meta$position, sep="_")
                Raspi_decoded <- merge(Raspi_unique, meta, by=c("Plant.ID", "position", "RasPi"), all = TRUE, allow.cartesian = TRUE)
            }
            Raspi_decoded <- na.omit(Raspi_decoded)
            Raspi_decoded
    }) 
    
    output$merged_data_report <- renderText({
        if(is.null(input$csv.meta)) {
            return(NULL)}
        else{
            if(input$expType == "PhenoCage"){
                data <- Raspi_unique()
                index <- length(unique(data$timestamp))
                sides <- unique(data$side.counts)
                meta <- decoding()
                pot_no <- length(unique(meta$POT))
                timepoint <- index / pot_no
                
                meta_sentence <- paste("Your data contains ", index, " unique observations, that were derived from  summarizing ", sides, " side views. \n The observations were decoded using the order of timestamp for each day for ", pot_no, " plant identifiers uploaded in your metadata. \n The number of timepoints in your data, derived by timestamp / plant identifiers is ", timepoint, " timepoints")    
            }
            if(input$expType == "PhenoRig"){
                data <- Raspi_unique()
                timepoints <- length(unique(data$timestamp))
                plant.id <- length(unique(data$Plant.ID))
                meta_sentence <- paste("Your data was collected over ", timepoints, " unique timepoints, for ", plant.id, " unique plants.")
            }
            return(meta_sentence)
            
        }
        
    })
    
    output$Data_tabl3 <- renderDataTable({
            Raspi_unique()
    })
    
    output$mergedtable_button <- renderUI({
        if (is.null(ItemList())) {
            return()
        }
        else{
            downloadButton("mergedtable_download_button", label = "Download table")
        }
    })
    
    
    ########### download merged file ####################################  
    
    
    output$mergedtable_download_button <- downloadHandler(
        filename = paste("Merged_data.RasPiPhenoApp.csv"),
        content <- function(file) {
            result <- Raspi_unique()
            write.csv(result, file)
            
        }
    )
    
    ########### Data Vis tab ####################################  
    
    metaList = reactive(if(is.null(Raspi_unique())){
        return(NULL)} else {
            temp <- decoding()
            return(colnames(temp))
            
        })
    
    
    output$Color_per <- renderUI({
        if(is.null(Raspi_unique())){return()} else {
            tagList(
                selectizeInput(
                    inputId = "ColorAreaGG",
                    label = "Color individual lines per:",
                    choices = metaList(),
                    multiple = F
                )
            )
        }
    })
    
    output$graph_over_time <- renderUI({
        if(is.null(Raspi_unique())){
            return(NULL)
        }
        else
            plotlyOutput("time_graph")
    })
    
    output$time_graph <- renderPlotly({
        TimeG()
    })
    
    TimeG <- reactive(if(is.null(Raspi_unique())){return(NULL)
        }else{  
        temp <- Raspi_unique()
        col.I.want <- input$ColorAreaGG
        colnames(temp)
        temp$col.sorting <- as.factor(temp[,input$Color_Graph])
        if(input$expType == "PhenoRig"){
            temp$time.min <- as.numeric(temp$time.min)
            Area_graph <- ggplot(data=temp, aes(x= time.min, y=area, group = Plant.ID, color = col.sorting)) 
            Area_graph <- Area_graph + geom_line(alpha = 0.1) 
            Area_graph <- Area_graph + ylab("Rosette Area (pixels)") + xlab("Time (minutes)")
        }
        if(input$expType == "PhenoCage"){
            temp$time.days <- as.numeric(temp$time.days)
            Area_graph <- ggplot(data=temp, aes(x= time.days, y=area.total, group = POT, color = col.sorting)) 
            Area_graph <- Area_graph + geom_line(alpha = 0.1) 
            Area_graph <- Area_graph + ylab("Cummulative Shoot Area (pixels)") + xlab("Time (days)")
        }
        Area_graph
    })
        
     
    
}
