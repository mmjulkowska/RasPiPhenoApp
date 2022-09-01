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
                Raspi_clean <- Raspi[,c("area","Year", "Month", "Day","Hour","Min", "Sec","timestamp", "RasPi", "Camera", "position", "TS2")]
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
        } else{
            tagList(
                selectizeInput(
                    inputId = "FWCol",
                    label = "Select column containing Fresh Weight",
                    choices = ItemList(),
                    multiple = F
                )
            )}
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
                Raspi_clean$time.days <- as.numeric(difftime(Raspi_clean$TS2, Raspi_clean$start, units = "days"))
                
                Raspi_unique <- unique(Raspi_clean[,c("Raspi", "Month", "Day", "timestamp", "time.days", "side.counts","area.total")])
                Raspi_unique$POT <- decoded_list
                Raspi_decoded <- merge(Raspi_unique, meta, by="POT", all = TRUE) 
            }
            if(input$expType == "PhenoRig"){
                Raspi_clean <- Raspi_clean()
                Raspi_clean$Plant.ID <- paste(Raspi_clean$RasPi, Raspi_clean$Camera, Raspi_clean$position, sep="_")
                # calculate time_of_experiment based on # Yr.Month.Day-Hr.Min format of input$StartTimeExp
                Raspi_clean$start <- input$StartTimeExp
                Raspi_clean$time.min <- as.numeric(difftime(Raspi_clean$TS2, Raspi_clean$start, units = "mins"))
                
                Raspi_unique <- unique(Raspi_clean[,c("RasPi", "Camera","Plant.ID", "position", "Month", "Day", "Hour","Min", "Sec", "timestamp", "time.min", "area")])
                meta <- decoding()
                meta$Plant.ID <- paste(meta$RasPi, meta$Camera, meta$position, sep="_")
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
            write.csv(result, file, row.names = FALSE)
            
        }
    )
    
    ########### Data Vis tab ####################################  
    
    metaList = reactive(if(is.null(Raspi_unique())){
        return(NULL)} else {
            temp <- decoding()
            return(colnames(temp))
            
        })
    
    
    output$color_original <- renderUI({
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
    
    TimeGraph <- reactive(if(is.null(Raspi_unique())){return(NULL)}else{  
        my_data <- Raspi_unique()
        my_data$col.sorting <- as.factor(my_data[,input$ColorAreaGG])
        if(input$expType == "PhenoRig"){
            my_data$time.min <- as.numeric(my_data$time.min)
            my_data$area <- as.numeric(my_data$area)
            Area_graph <- ggplot(data=my_data, aes(x= time.min, y=area, group = Plant.ID, color = col.sorting)) 
            Area_graph <- Area_graph + geom_line(alpha = 0.1) 
            Area_graph <- Area_graph + ylab("Rosette Area (pixels)") + xlab("Time (minutes)")
        }
        if(input$expType == "PhenoCage"){
            my_data$time.days <- as.numeric(my_data$time.days)
            my_data$area.total <- as.numeric(my_data$area.total)
            Area_graph <- ggplot(data=my_data, aes(x= time.days, y=area.total, group = POT, color = col.sorting)) 
            Area_graph <- Area_graph + geom_line(alpha = 0.1) 
            Area_graph <- Area_graph + ylab("Cummulative Shoot Area (pixels)") + xlab("Time (days)")
        }
        Area_graph
    })
    
    output$graph_over_time <- renderPlotly({
        ggplotly(TimeGraph())
    })
    
   
    
    smooth_sample <- reactive(if(is.null(Raspi_unique())){return(NULL)
    }else{  
        temp <- Raspi_unique()
        
        if(input$expType == "PhenoRig"){
            sample.id <- unique(temp$Plant.ID)
        }
        if(input$expType == "PhenoCage"){
            sample.id <- unique(temp$POT)
        }
        return(sample.id)
        
    })
    
    output$Choose_smooth_sample <- renderUI({
        if(is.null(Raspi_unique())){return(NULL)
        }else{tagList(
            selectizeInput(
                inputId = "sample_smooth",
                label = "Select sample to display smoothing",
                choices = smooth_sample(),
                multiple=F))}
    })
    
    Smooth_plot_one <- reactive(
        if(is.null(smooth_sample())){return(NULL)
        }
        else{  
            temp <- Raspi_unique()
            if(input$expType == "PhenoRig"){
                temp2 <- subset(temp, temp$Plant.ID == input$sample_smooth)
                temp2$time.min <- as.numeric(temp2$time.min)
                time.min <- unique(temp2$time.min)
                plot.spl <- with(temp2, smooth.spline(time.min, area, df = as.numeric(input$degFree)))
                plot(area ~ time.min, data = temp2)
                lines(plot.spl, col = "blue")
                #lines(predict(plot.spl, time.min), col="red")
            }
            if(input$expType == "PhenoCage"){
                temp2 <- subset(temp, temp$POT == input$sample_smooth)
                temp2$time.days <- as.numeric(temp2$time.days)
                time.days <- unique(temp2$time.days)
                plot.spl <- with(temp2, smooth.spline(time.days, area.total, df = as.numeric(input$degFree)))
                plot(area.total ~ time.days, data = temp2)
                lines(plot.spl, col = "blue")
                #lines(predict(plot.spl, time.days), col="red")
            }
        })
    
    output$Smoothed_graph_one_sample <- renderPlot({
        Smooth_plot_one()}
    )
    
    smooth_all <- reactive(if(input$SmoothGo == FALSE){return(NULL)}else{
        my_data <- Raspi_unique()
        if(input$expType == "PhenoRig"){
            names <- c(text="Plant.ID", "time.min", "area.spl")
            spline_data <- data.frame()
            for (k in names) spline_data[[k]] <- as.character()
            i=1
            temp <- subset(my_data, my_data$Plant.ID == unique(my_data$Plant.ID)[1])
            temp$time.min <- as.numeric(as.character(temp$time.min))
            day <- unique(temp$time.min)
            max_day <- length(day)
            plot.spl <- with(temp, smooth.spline(time.min, area, df = as.numeric(input$degFree)))
            pred_temp <- predict(plot.spl, day)
            spline_data[1:max_day,2] <- pred_temp$x
            spline_data[1:max_day,3] <- pred_temp$y
            spline_data[1:max_day,1] <- temp$Plant.ID[1]
            final_spline <- spline_data
            for(i in 1:length(unique(my_data$Plant.ID))){
                temp <- subset(my_data, my_data$Plant.ID == unique(my_data$Plant.ID)[i])
                temp$time.min <- as.numeric(as.character(temp$time.min))
                day <- unique(temp$time.min)
                max_day <- length(day)
                plot.spl <- with(temp, smooth.spline(time.min, area, df = as.numeric(input$degFree)))
                pred_temp <- predict(plot.spl, day)
                spline_data[1:max_day,2] <- pred_temp$x
                spline_data[1:max_day,3] <- pred_temp$y
                spline_data[1:max_day,1] <- temp$Plant.ID[1]
                final_spline <- rbind(final_spline, spline_data)
            }
            meta <- decoding()
            meta$Plant.ID <- paste(meta$RasPi, meta$Camera, meta$position, sep="_")
            Raspi_decoded <- merge(final_spline, meta, by="Plant.ID", all = TRUE) 
            Raspi_decoded2 <- na.omit(Raspi_decoded)
            }
        if(input$expType == "PhenoCage"){
            names <- c(text="POT", "time.days", "area.total.spl")
            spline_data <- data.frame()
            for (k in names) spline_data[[k]] <- as.character()
            i=1
            temp <- subset(my_data, my_data$POT == unique(my_data$POT)[1])
            temp$time.day <- as.numeric(as.character(temp$time.day))
            day <- unique(temp$time.day)
            max_day <- length(day)
            plot.spl <- with(temp, smooth.spline(time.day, area.total, df = as.numeric(input$degFree)))
            pred_temp <- predict(plot.spl, day)
            spline_data[1:max_day,2] <- pred_temp$x
            spline_data[1:max_day,3] <- pred_temp$y
            spline_data[1:max_day,1] <- temp$POT[1]
            final_spline <- spline_data
            for(i in 1:length(unique(my_data$POT))){
                temp <- subset(my_data, my_data$POT == unique(my_data$POT)[i])
                temp$time.day <- as.numeric(as.character(temp$time.day))
                day <- unique(temp$time.day)
                max_day <- length(day)
                plot.spl <- with(temp, smooth.spline(time.day, area.total, df = as.numeric(input$degFree)))
                pred_temp <- predict(plot.spl, day)
                spline_data[1:max_day,2] <- pred_temp$x
                spline_data[1:max_day,3] <- pred_temp$y
                spline_data[1:max_day,1] <- temp$POT[1]
                final_spline <- rbind(final_spline, spline_data)
            }
            meta <- decoding()
            Raspi_decoded <- merge(final_spline, meta, by="POT", all = TRUE) 
            Raspi_decoded2 <- na.omit(Raspi_decoded)
            
        }
        return(Raspi_decoded2)
    })
    
    output$Smooth_table <- renderDataTable({
        smooth_all()
    })    

    output$Smooth_table_button <- renderUI({
        if (is.null(smooth_all())) {
            return()
        }
        else{
            downloadButton("smooth_table_download_button", label = "Download table")
        }
    })
    
    
    ########### download smoothed file ####################################  
    
    
    output$smooth_table_download_button <- downloadHandler(
        filename = paste("Smoothed_data.RasPiPhenoApp.csv"),
        content <- function(file) {
            result <- smooth_all()
            write.csv(result, file, row.names = FALSE)
            
        }
    )
    
    # # # smooth graph # # # # # 
    
    output$color_smooth <- renderUI({
        if(is.null(smooth_all())){return()} else {
            tagList(
                selectizeInput(
                    inputId = "ColorSmoothGG",
                    label = "Color individual lines per:",
                    choices = metaList(),
                    multiple = F
                )
            )
        }
    })
    
    smooth_graph_all <- reactive(if(is.null(smooth_all())){return(NULL)}else{
        my_data <- smooth_all()
        my_data$col.sorting <- as.factor(my_data[,input$ColorSmoothGG])
        if(input$expType == "PhenoRig"){
            my_data$time.min <- as.numeric(my_data$time.min)
            my_data$area.spl <- as.numeric(my_data$area.spl)
            Area_graph <- ggplot(data=my_data, aes(x= time.min, y=area.spl, group = Plant.ID, color = col.sorting)) 
            Area_graph <- Area_graph + geom_line(alpha = 0.1) 
            Area_graph <- Area_graph + ylab("Rosette Area (pixels)") + xlab("Time (minutes)")
        }
        if(input$expType == "PhenoCage"){
            my_data$time.days <- as.numeric(my_data$time.days)
            my_data$area.total.spl <- as.numeric(my_data$area.total.spl)
            Area_graph <- ggplot(data=my_data, aes(x= time.days, y=area.total.spl, group = POT, color = col.sorting)) 
            Area_graph <- Area_graph + geom_line(alpha = 0.1) 
            Area_graph <- Area_graph + ylab("Cummulative Shoot Area (pixels)") + xlab("Time (days)")
        }
        Area_graph
    })
    
    output$all_smooth_graph <- renderPlotly(
        ggplotly(smooth_graph_all())
    )
    
    # Calculating growth rate inputs
    
    interval_choices <- reactive(if(is.null(Raspi_unique())){return(NULL)}else{
        if(input$expType == "PhenoRig"){
            interval_list <- c("3 hours", "6 hours", "day", "2 days")
        }
        if(input$expType == "PhenoCage"){
            interval_list <- c("6 days", "8 days", "10 days")
        }
        return(interval_list)
    })
    
    output$interval <- renderUI({
        if(is.null(Raspi_unique()) | input$GrowthType == "Over whole experiment"){return(NULL)}else{
            tagList(selectizeInput(
                inputId = "GrowthInterval",
                label = "Calculate growth rate for every:",
                choices = interval_choices(),
                multiple=F
            ))
            
        }
    })
    
    step_choices <- reactive(if(is.null(Raspi_unique())){return(NULL)}else{
        if(input$expType == "PhenoRig"){
            interval_list <- c("1 h", "6 h", "12 h", "24 h")
        }
        if(input$expType == "PhenoCage"){
            interval_list <- c("2 days", "4 days", "6 days")
        }
        return(interval_list)
    })
    
    
    output$step <- renderUI({
        if(is.null(Raspi_unique()) | input$GrowthType == "Over whole experiment"){return(NULL)}else{
            tagList(selectizeInput(
                inputId = "step_size",
                label = "Calculate growth rate with step size every:",
                choices = step_choices(),
                multiple=F
            ))
        }
    })
    
    
    
    Growth_rate_table <- reactive(if(input$GoGrowth == FALSE){return(NULL)}else{
        if(input$GrowthType == "Step-wise"){
            if(input$dataGrowth == "Original data"){
                my_data <- Raspi_unique()
                if(input$expType == "PhenoRig"){
                    # isolate 1st plant 
                    all_plants <- unique(my_data$Plant.ID)
                    my_data$time.min <- as.numeric(as.character(my_data$time.min))
                    temp <- subset(my_data, my_data$Plant.ID == all_plants[1])
                    # isolate first time interval
                    min_time <- min(my_data$time.min)
                    max_time <- max(my_data$time.min)
                    if(input$step_size == "1 h"){step_time <- 60}
                    if(input$step_size == "6 h"){step_time <- 60*6}
                    if(input$step_size == "24 h"){step_time <- 60*24}
                    growth_timeline <- seq(min_time, max_time, by=step_time)
                    growth_timeline <- growth_timeline[-length(growth_timeline)]
                    if(input$GrowthInterval == "3 hours"){
                        temp_now <- subset(temp, temp$time.min < 180)
                        temp_now$area.spl <- as.numeric(temp_now$area.spl)
                        temp_now$time.min <- as.numeric(temp_now$time.min)
                        model_now <- lm(temp_now$area ~ temp_now$time.min)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="Plant.ID", "min", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$Plant.ID[1]
                        growth_data[1,2] <- min(temp_now$time.min)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 180
                            temp_now <- subset(temp, temp$time.min < max)
                            temp_now <- subset(temp_now, temp_now$time.min >= min)
                            temp_now$area.spl <- as.numeric(temp_now$area.spl)
                            temp_now$time.min <- as.numeric(temp_now$time.min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area ~ temp_now$time.min)
                            growth_data[t,1] <- temp_now$Plant.ID[1]
                            growth_data[t,2] <- min(temp_now$time.min)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = t + 1
                        for(r in 2:length(all_plants)){
                            temp <- subset(my_data, my_data$Plant.ID == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 180
                                    temp_now <- subset(temp, temp$time.min < max)
                                    temp_now <- subset(temp_now, temp_now$time.min >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area ~ temp_now$time.min)
                                    growth_data[counter,1] <- temp_now$Plant.ID[1]
                                    growth_data[counter,2] <- min(temp_now$time.min)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                        }
                    if(input$GrowthInterval == "6 hours"){
                        temp_now <- subset(temp, temp$time.min < 360)
                        model_now <- lm(temp_now$area ~ temp_now$time.min)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="Plant.ID", "min", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$Plant.ID[1]
                        growth_data[1,2] <- min(temp_now$time.min)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 360
                            temp_now <- subset(temp, temp$time.min < max)
                            temp_now <- subset(temp_now, temp_now$time.min >= min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area ~ temp_now$time.min)
                            growth_data[t,1] <- temp_now$Plant.ID[1]
                            growth_data[t,2] <- min(temp_now$time.min)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$Plant.ID == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 360
                                    temp_now <- subset(temp, temp$time.min < max)
                                    temp_now <- subset(temp_now, temp_now$time.min >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area ~ temp_now$time.min)
                                    growth_data[counter,1] <- temp_now$Plant.ID[1]
                                    growth_data[counter,2] <- min(temp_now$time.min)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                    if(input$GrowthInterval == "day"){
                        temp_now <- subset(temp, temp$time.min < 1440)
                        model_now <- lm(temp_now$area ~ temp_now$time.min)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="Plant.ID", "min", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$Plant.ID[1]
                        growth_data[1,2] <- min(temp_now$time.min)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 1440
                            temp_now <- subset(temp, temp$time.min < max)
                            temp_now <- subset(temp_now, temp_now$time.min >= min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area ~ temp_now$time.min)
                            growth_data[t,1] <- temp_now$Plant.ID[1]
                            growth_data[t,2] <- min(temp_now$time.min)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$Plant.ID == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 1440
                                    temp_now <- subset(temp, temp$time.min < max)
                                    temp_now <- subset(temp_now, temp_now$time.min >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area ~ temp_now$time.min)
                                    growth_data[counter,1] <- temp_now$Plant.ID[1]
                                    growth_data[counter,2] <- min(temp_now$time.min)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                    if(input$GrowthInterval == "2 days"){
                        temp_now <- subset(temp, temp$time.min < 2880)
                        model_now <- lm(temp_now$area ~ temp_now$time.min)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="Plant.ID", "min", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$Plant.ID[1]
                        growth_data[1,2] <- min(temp_now$time.min)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 2880
                            temp_now <- subset(temp, temp$time.min < max)
                            temp_now <- subset(temp_now, temp_now$time.min >= min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area ~ temp_now$time.min)
                            growth_data[t,1] <- temp_now$Plant.ID[1]
                            growth_data[t,2] <- min(temp_now$time.min)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$Plant.ID == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 2880
                                    temp_now <- subset(temp, temp$time.min < max)
                                    temp_now <- subset(temp_now, temp_now$time.min >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area ~ temp_now$time.min)
                                    growth_data[counter,1] <- temp_now$Plant.ID[1]
                                    growth_data[counter,2] <- min(temp_now$time.min)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                }
                if(input$expType == "PhenoCage"){
                    # isolate 1st plant 
                    all_plants <- unique(my_data$POT)
                    my_data$time.days <- as.numeric(as.character(my_data$time.days))
                    temp <- subset(my_data, my_data$POT == all_plants[1])
                    # isolate first time interval
                    min_time <- min(my_data$time.days)
                    max_time <- max(my_data$time.days)
                    if(input$step_size == "2 days"){step_time <- 2}
                    if(input$step_size == "4 days"){step_time <- 4}
                    if(input$step_size == "6 days"){step_time <- 6}
                    growth_timeline <- seq(min_time, max_time, by=step_time)
                    growth_timeline <- growth_timeline[-length(growth_timeline)]
                    if(input$GrowthInterval == "6 days"){
                        temp_now <- subset(temp, temp$time.days < 6)
                        model_now <- lm(temp_now$area.total ~ temp_now$time.days)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="POT", "day", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$POT[1]
                        growth_data[1,2] <- min(temp_now$time.days)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 6
                            temp_now <- subset(temp, temp$time.days < max)
                            temp_now <- subset(temp_now, temp_now$time.days >= min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area.total ~ temp_now$time.days)
                            growth_data[t,1] <- temp_now$POT[1]
                            growth_data[t,2] <- min(temp_now$time.days)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$POT == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 6
                                    temp_now <- subset(temp, temp$time.days < max)
                                    temp_now <- subset(temp_now, temp_now$time.days >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area.total ~ temp_now$time.days)
                                    growth_data[counter,1] <- temp_now$POT[1]
                                    growth_data[counter,2] <- min(temp_now$time.days)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                    if(input$GrowthInterval == "8 days"){
                        temp_now <- subset(temp, temp$time.days < 8)
                        model_now <- lm(temp_now$area.total ~ temp_now$time.days)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="POT", "day", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$POT[1]
                        growth_data[1,2] <- min(temp_now$time.days)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 8
                            temp_now <- subset(temp, temp$time.days < max)
                            temp_now <- subset(temp_now, temp_now$time.days >= min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area.total ~ temp_now$time.days)
                            growth_data[t,1] <- temp_now$POT[1]
                            growth_data[t,2] <- min(temp_now$time.days)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$POT == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 8
                                    temp_now <- subset(temp, temp$time.days < max)
                                    temp_now <- subset(temp_now, temp_now$time.days >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area.total ~ temp_now$time.days)
                                    growth_data[counter,1] <- temp_now$POT[1]
                                    growth_data[counter,2] <- min(temp_now$time.days)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                    if(input$GrowthInterval == "10 days"){
                        temp_now <- subset(temp, temp$time.days < 10)
                        model_now <- lm(temp_now$area.total ~ temp_now$time.days)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="POT", "day", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$POT[1]
                        growth_data[1,2] <- min(temp_now$time.days)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 10
                            temp_now <- subset(temp, temp$time.days < max)
                            temp_now <- subset(temp_now, temp_now$time.days >= min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area.total ~ temp_now$time.days)
                            growth_data[t,1] <- temp_now$POT[1]
                            growth_data[t,2] <- min(temp_now$time.days)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$POT == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 10
                                    temp_now <- subset(temp, temp$time.days < max)
                                    temp_now <- subset(temp_now, temp_now$time.days >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area.total ~ temp_now$time.days)
                                    growth_data[counter,1] <- temp_now$POT[1]
                                    growth_data[counter,2] <- min(temp_now$time.days)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                }
            }
            if(input$dataGrowth == "Smooth data"){
                my_data <- smooth_all()   
                if(input$expType == "PhenoRig"){
                    # isolate 1st plant 
                    all_plants <- unique(my_data$Plant.ID)
                    my_data$time.min <- as.numeric(as.character(my_data$time.min))
                    temp <- subset(my_data, my_data$Plant.ID == all_plants[1])
                    # isolate first time interval
                    min_time <- min(my_data$time.min)
                    max_time <- max(my_data$time.min)
                    if(input$step_size == "1 h"){step_time <- 60}
                    if(input$step_size == "6 h"){step_time <- 60*6}
                    if(input$step_size == "24 h"){step_time <- 60*24}
                    growth_timeline <- seq(min_time, max_time, by=step_time)
                    growth_timeline <- growth_timeline[-length(growth_timeline)]
                    if(input$GrowthInterval == "3 hours"){
                        temp_now <- subset(temp, temp$time.min < 180)
                        temp_now$area.spl <- as.numeric(temp_now$area.spl)
                        temp_now$time.min <- as.numeric(temp_now$time.min)
                        model_now <- lm(temp_now$area.spl ~ temp_now$time.min)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="Plant.ID", "min", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$Plant.ID[1]
                        growth_data[1,2] <- min(temp_now$time.min)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 180
                            temp_now <- subset(temp, temp$time.min < max)
                            temp_now <- subset(temp_now, temp_now$time.min >= min)
                            if(dim(temp_now)[1]>3){
                            temp_now$area.spl <- as.numeric(temp_now$area.spl)
                            temp_now$time.min <- as.numeric(temp_now$time.min)
                            model_now <- lm(temp_now$area.spl ~ temp_now$time.min)
                            growth_data[t,1] <- temp_now$Plant.ID[1]
                            growth_data[t,2] <- min(temp_now$time.min)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = t + 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$Plant.ID == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 180
                                    temp_now <- subset(temp, temp$time.min < max)
                                    temp_now <- subset(temp_now, temp_now$time.min >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area.spl ~ temp_now$time.min)
                                    growth_data[counter,1] <- temp_now$Plant.ID[1]
                                    growth_data[counter,2] <- min(temp_now$time.min)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                    if(input$GrowthInterval == "6 hours"){
                        temp_now <- subset(temp, temp$time.min < 360)
                        model_now <- lm(temp_now$area.spl ~ temp_now$time.min)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="Plant.ID", "min", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$Plant.ID[1]
                        growth_data[1,2] <- min(temp_now$time.min)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 360
                            temp_now <- subset(temp, temp$time.min < max)
                            temp_now <- subset(temp_now, temp_now$time.min >= min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area.spl ~ temp_now$time.min)
                            growth_data[t,1] <- temp_now$Plant.ID[1]
                            growth_data[t,2] <- min(temp_now$time.min)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$Plant.ID == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 360
                                    temp_now <- subset(temp, temp$time.min < max)
                                    temp_now <- subset(temp_now, temp_now$time.min >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area.spl ~ temp_now$time.min)
                                    growth_data[counter,1] <- temp_now$Plant.ID[1]
                                    growth_data[counter,2] <- min(temp_now$time.min)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                    if(input$GrowthInterval == "day"){
                        temp_now <- subset(temp, temp$time.min < 1440)
                        model_now <- lm(temp_now$area.spl ~ temp_now$time.min)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="Plant.ID", "min", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$Plant.ID[1]
                        growth_data[1,2] <- min(temp_now$time.min)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 1440
                            temp_now <- subset(temp, temp$time.min < max)
                            temp_now <- subset(temp_now, temp_now$time.min >= min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area.spl ~ temp_now$time.min)
                            growth_data[t,1] <- temp_now$Plant.ID[1]
                            growth_data[t,2] <- min(temp_now$time.min)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$Plant.ID == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 1440
                                    temp_now <- subset(temp, temp$time.min < max)
                                    temp_now <- subset(temp_now, temp_now$time.min >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area.spl ~ temp_now$time.min)
                                    growth_data[counter,1] <- temp_now$Plant.ID[1]
                                    growth_data[counter,2] <- min(temp_now$time.min)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                    if(input$GrowthInterval == "2 days"){
                        temp_now <- subset(temp, temp$time.min < 2880)
                        model_now <- lm(temp_now$area.spl ~ temp_now$time.min)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="Plant.ID", "min", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$Plant.ID[1]
                        growth_data[1,2] <- min(temp_now$time.min)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 2880
                            temp_now <- subset(temp, temp$time.min < max)
                            temp_now <- subset(temp_now, temp_now$time.min >= min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area.spl ~ temp_now$time.min)
                            growth_data[t,1] <- temp_now$Plant.ID[1]
                            growth_data[t,2] <- min(temp_now$time.min)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$Plant.ID == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 2880
                                    temp_now <- subset(temp, temp$time.min < max)
                                    temp_now <- subset(temp_now, temp_now$time.min >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area.spl ~ temp_now$time.min)
                                    growth_data[counter,1] <- temp_now$Plant.ID[1]
                                    growth_data[counter,2] <- min(temp_now$time.min)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                }
                if(input$expType == "PhenoCage"){
                    # isolate 1st plant 
                    all_plants <- unique(my_data$POT)
                    my_data$time.days <- as.numeric(as.character(my_data$time.days))
                    temp <- subset(my_data, my_data$POT == all_plants[1])
                    # isolate first time interval
                    min_time <- min(my_data$time.days)
                    max_time <- max(my_data$time.days)
                    if(input$step_size == "2 days"){step_time <- 2}
                    if(input$step_size == "4 days"){step_time <- 4}
                    if(input$step_size == "6 days"){step_time <- 6}
                    growth_timeline <- seq(min_time, max_time, by=step_time)
                    growth_timeline <- growth_timeline[-length(growth_timeline)]
                    if(input$GrowthInterval == "6 days"){
                        temp_now <- subset(temp, temp$time.days < 6)
                        model_now <- lm(temp_now$area.total.spl ~ temp_now$time.days)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="POT", "day", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$POT[1]
                        growth_data[1,2] <- min(temp_now$time.days)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 6
                            temp_now <- subset(temp, temp$time.days < max)
                            temp_now <- subset(temp_now, temp_now$time.days >= min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area.total.spl ~ temp_now$time.days)
                            growth_data[t,1] <- temp_now$POT[1]
                            growth_data[t,2] <- min(temp_now$time.days)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$POT == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 6
                                    temp_now <- subset(temp, temp$time.days < max)
                                    temp_now <- subset(temp_now, temp_now$time.days >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area.total.spl ~ temp_now$time.days)
                                    growth_data[counter,1] <- temp_now$POT[1]
                                    growth_data[counter,2] <- min(temp_now$time.days)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                    if(input$GrowthInterval == "8 days"){
                        temp_now <- subset(temp, temp$time.days < 8)
                        model_now <- lm(temp_now$area.total.spl ~ temp_now$time.days)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="POT", "day", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$POT[1]
                        growth_data[1,2] <- min(temp_now$time.days)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 8
                            temp_now <- subset(temp, temp$time.days < max)
                            temp_now <- subset(temp_now, temp_now$time.days >= min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area.total.spl ~ temp_now$time.days)
                            growth_data[t,1] <- temp_now$POT[1]
                            growth_data[t,2] <- min(temp_now$time.days)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$POT == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 8
                                    temp_now <- subset(temp, temp$time.days < max)
                                    temp_now <- subset(temp_now, temp_now$time.days >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area.total.spl ~ temp_now$time.days)
                                    growth_data[counter,1] <- temp_now$POT[1]
                                    growth_data[counter,2] <- min(temp_now$time.days)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                    if(input$GrowthInterval == "10 days"){
                        temp_now <- subset(temp, temp$time.days < 10)
                        model_now <- lm(temp_now$area.total.spl ~ temp_now$time.days)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        names <- c(text="POT", "day", "GR", "R2")
                        growth_data <- data.frame()
                        for (k in names) growth_data[[k]] <- as.character()
                        growth_data
                        growth_data[1,1] <- temp_now$POT[1]
                        growth_data[1,2] <- min(temp_now$time.days)
                        growth_data[1,3] <- GR
                        growth_data[1,4] <- R2
                        for(t in 2:length(growth_timeline)){
                            min = growth_timeline[t]
                            max = min + 10
                            temp_now <- subset(temp, temp$time.days < max)
                            temp_now <- subset(temp_now, temp_now$time.days >= min)
                            if(dim(temp_now)[1]>3){
                            model_now <- lm(temp_now$area.total.spl ~ temp_now$time.days)
                            growth_data[t,1] <- temp_now$POT[1]
                            growth_data[t,2] <- min(temp_now$time.days)
                            growth_data[t,3] <- GR
                            growth_data[t,4] <- R2}}
                        counter = 1
                        for(r in 1:length(all_plants)){
                            temp <- subset(my_data, my_data$POT == all_plants[r])
                            if(dim(temp)[1]>0){
                                for(t in 1:length(growth_timeline)){
                                    min = growth_timeline[t]
                                    max = min + 10
                                    temp_now <- subset(temp, temp$time.days < max)
                                    temp_now <- subset(temp_now, temp_now$time.days >= min)
                                    if(dim(temp_now)[1]>3){
                                    model_now <- lm(temp_now$area.total.spl ~ temp_now$time.days)
                                    growth_data[counter,1] <- temp_now$POT[1]
                                    growth_data[counter,2] <- min(temp_now$time.days)
                                    growth_data[counter,3] <- model_now$coefficients[2]
                                    growth_data[counter,4] <- summary(model_now)$r.squared
                                    counter <- counter + 1}
                                }}}
                    }
                }
            }}
        if(input$GrowthType == "Over whole experiment"){
            if(input$dataGrowth == "Original data"){
                my_data <- Raspi_unique()
                if(input$expType == "PhenoRig"){
                    all_plants <- unique(my_data$Plant.ID)
                    my_data$time.min <- as.numeric(as.character(my_data$time.min))
                    temp <- subset(my_data, my_data$Plant.ID == all_plants[1])
                    names <- c(text="Plant.ID", "GR", "R2")
                    growth_data <- data.frame()
                    for (k in names) growth_data[[k]] <- as.character()
                    growth_data
                    model_now <- lm(temp$area ~ temp$time.min)
                    GR <- model_now$coefficients[2]
                    R2 <- summary(model_now)$r.squared
                    growth_data[1,1] <- temp$Plant.ID[1]
                    growth_data[1,2] <- GR
                    growth_data[1,3] <- R2
                    for(i in 2:length(all_plants)){
                        temp <- subset(my_data, my_data$Plant.ID == all_plants[i])
                        names <- c(text="Plant.ID", "GR", "R2")
                        model_now <- lm(temp$area ~ temp$time.min)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        growth_data[i,1] <- temp$Plant.ID[1]
                        growth_data[i,2] <- GR
                        growth_data[i,3] <- R2
                    }}
                if(input$expType == "PhenoCage"){
                    all_plants <- unique(my_data$POT)
                    my_data$time.days <- as.numeric(as.character(my_data$time.days))
                    temp <- subset(my_data, my_data$POT == all_plants[1])
                    names <- c(text="POT", "GR", "R2")
                    growth_data <- data.frame()
                    for (k in names) growth_data[[k]] <- as.character()
                    growth_data
                    model_now <- lm(temp$area.total ~ temp$time.days)
                    GR <- model_now$coefficients[2]
                    R2 <- summary(model_now)$r.squared
                    growth_data[1,1] <- temp_now$POT[1]
                    growth_data[1,2] <- GR
                    growth_data[1,3] <- R2
                    for(i in 2:length(all_plants)){
                        temp <- subset(my_data, my_data$POT == all_plants[i])
                        names <- c(text="POT", "GR", "R2")
                        model_now <- lm(temp$area.total ~ temp$time.days)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        growth_data[i,1] <- temp$POT[1]
                        growth_data[i,2] <- GR
                        growth_data[i,3] <- R2
                    }}}
            if(input$dataGrowth == "Smooth data"){
                my_data <- smooth_all()   
                if(input$expType == "PhenoRig"){
                    all_plants <- unique(my_data$Plant.ID)
                    my_data$time.min <- as.numeric(as.character(my_data$time.min))
                    temp <- subset(my_data, my_data$Plant.ID == all_plants[1])
                    names <- c(text="Plant.ID", "GR", "R2")
                    growth_data <- data.frame()
                    for (k in names) growth_data[[k]] <- as.character()
                    growth_data
                    model_now <- lm(temp$area.spl ~ temp$time.min)
                    GR <- model_now$coefficients[2]
                    R2 <- summary(model_now)$r.squared
                    growth_data[1,1] <- temp$Plant.ID[1]
                    growth_data[1,2] <- GR
                    growth_data[1,3] <- R2
                    for(i in 2:length(all_plants)){
                        temp <- subset(my_data, my_data$Plant.ID == all_plants[i])
                        names <- c(text="Plant.ID", "GR", "R2")
                        model_now <- lm(temp$area.spl ~ temp$time.min)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        growth_data[i,1] <- temp$Plant.ID[1]
                        growth_data[i,2] <- GR
                        growth_data[i,3] <- R2
                    }}
                if(input$expType == "PhenoCage"){
                    all_plants <- unique(my_data$POT)
                    my_data$time.days <- as.numeric(as.character(my_data$time.days))
                    temp <- subset(my_data, my_data$POT == all_plants[1])
                    names <- c(text="POT", "GR", "R2")
                    growth_data <- data.frame()
                    for (k in names) growth_data[[k]] <- as.character()
                    growth_data
                    model_now <- lm(temp$area.total.spl ~ temp$time.days)
                    GR <- model_now$coefficients[2]
                    R2 <- summary(model_now)$r.squared
                    growth_data[1,1] <- temp$POT[1]
                    growth_data[1,2] <- GR
                    growth_data[1,3] <- R2
                    for(i in 2:length(all_plants)){
                        temp <- subset(my_data, my_data$POT == all_plants[i])
                        names <- c(text="POT", "GR", "R2")
                        model_now <- lm(temp$area.total.spl ~ temp$time.days)
                        GR <- model_now$coefficients[2]
                        R2 <- summary(model_now)$r.squared
                        growth_data[i,1] <- temp$POT[1]
                        growth_data[i,2] <- GR
                        growth_data[i,3] <- R2
                    }}}
        }
        meta <- decoding()
        if(input$expType == "PhenoCage"){
            growth_data <- merge(growth_data, meta, by="POT", all = TRUE, allow.cartesian = TRUE)
            }
        if(input$expType == "PhenoRig"){
            meta$Plant.ID <- paste(meta$RasPi, meta$Camera, meta$position, sep="_")
            growth_data <- merge(growth_data, meta, by=c("Plant.ID"), all = TRUE, allow.cartesian = TRUE)
        }
        growth_data <- na.omit(growth_data)
        return(growth_data)
    })
    
    
    
    
    output$Growth_table <- renderDataTable(
        Growth_rate_table()
    )
    
    # Growth rate table button
    
    output$Growth_table_button <- renderUI({
        if (is.null(Growth_rate_table())) {
            return()
        }
        else{
            downloadButton("growthtable_download_button", label = "Download table")
        }
    })
    
    
    ########### download growth rate file ####################################  
    
    
    output$growthtable_download_button <- downloadHandler(
        filename = paste("GrowthRate_data.RasPiPhenoApp.csv"),
        content <- function(file) {
            result <- Growth_rate_table()
            write.csv(result, file, row.names = FALSE)
            
        }
    )
    
    # # # # # # # # Growth rate graph inputs # # # # # # # # # # # # # # 
    
    output$Growth_Color_button <- renderUI({
        if(is.null(Growth_rate_table())){return()} else {
            tagList(
                selectizeInput(
                    inputId = "ColorGrowth",
                    label = "Color graph per:",
                    choices = metaList(),
                    multiple = F
                ))}
    })
    
    output$Growth_Xaxis <- renderUI({
        if(input$GrowthType == "Step-wise"){return()} else {
            tagList(
                selectizeInput(
                    inputId = "XGrowth",
                    label = "X-axis:",
                    choices = metaList(),
                    multiple = F
                ))}
    })
    
    output$Growth_facet_check <- renderUI({
        if(is.null(Growth_rate_table())){return()} else {
            tagList(
                checkboxInput(
                    inputId = "GrowthFacet",
                    label = "Split the graph"
                ))}
    })
    
    output$Growth_facet <- renderUI({
        if(input$GrowthFacet == FALSE){return()} else {
            tagList(
                selectizeInput(
                    inputId = "FacetGrowth",
                    label = "Facet graph per:",
                    choices = metaList(),
                    multiple = F
                ))}
    })
    
    # # # # # # # # Growth rate graph  # # # # # # # # # # # # # # 
    
    Growth_rate_graph <- reactive(if(input$GoGrowth==FALSE){return(NULL)}else{
        my_data <- Growth_rate_table()
        my_data$GR <- as.numeric(as.character(my_data$GR))
        my_data$fill <- my_data[,input$ColorGrowth]
        if(input$GrowthType == "Over whole experiment"){
            my_data$Xaxis <- my_data[,input$XGrowth]
            if(input$expType == "PhenoRig"){
                my_plot <- ggerrorplot(my_data, y="GR", x="Xaxis", fill="fill", color="fill",
                                       desc_stat = "mean_sd", add="jitter", add.params = list(color = "darkgray"),
                                       ylab="Growth Rate (pix / min)", xlab=input$XGrowth)
            }
            if(input$expType == "PhenoCage"){
                my_plot <- ggerrorplot(my_data, y="GR", x="Xaxis", fill="fill", color="fill",
                                       desc_stat = "mean_sd", add="jitter", add.params = list(color = "darkgray"),
                                       ylab="Growth Rate (pix / day)", xlab=input$XGrowth)
            }}
        if(input$GrowthType == "Step-wise"){
            if(input$expType == "PhenoRig"){
                my_data$min <- as.numeric(as.character(my_data$min))
                my_plot <- ggplot(data=my_data, aes(x= min, y=GR, group = Plant.ID, color = fill)) 
                my_plot <- my_plot + geom_line(alpha = 0.1) 
                my_plot <- my_plot + stat_summary(fun.data = mean_se, geom="ribbon", linetype=0, aes(group= fill), alpha=0.3)
                my_plot <- my_plot + stat_summary(fun=mean, aes(group= fill),  size=0.7, geom="line", linetype = "dashed")
                my_plot <- my_plot + ylab("Growth Rate (pix / min)") + xlab("time (min)")
            }
            if(input$expType == "PhenoCage"){
                my_data$day <- as.numeric(as.character(my_data$day))
                my_plot <- ggplot(data=my_data, aes(x= day, y=GR, group = POT, color = fill)) 
                my_plot <- my_plot + geom_line(alpha = 0.1) 
                my_plot <- my_plot + stat_summary(fun.data = mean_se, geom="ribbon", linetype=0, aes(group= fill), alpha=0.3)
                my_plot <- my_plot + stat_summary(fun=mean, aes(group= fill),  size=0.7, geom="line", linetype = "dashed")
                my_plot <- my_plot + ylab("Growth Rate (pix / day)") + xlab("time (days)")
            }
        }
        #my_plot <- my_plot + scale_color_jco()
        return(my_plot)
    })
    
    output$Growth_Graph <- renderPlot({
        Growth_rate_graph()
    })
    
    
    ########### download growth rate graph ####################################  
    
    output$Growth_graph_button <- renderUI({
        if (is.null(Growth_rate_graph())) {
            return()
        }
        else{
            downloadButton("growthgraph_download_button", label = "Download plot")
        }
    })
    
    
    output$growthgraph_download_button <- downloadHandler(
        filename = paste("GrowthRate_graph.RasPiPhenoApp.pdf"),
        content <- function(file) {
            pdf(file)
            Growth_rate_graph()
            dev.off()
            
        }
    )
    
    
    # Cant touch this! 
}
