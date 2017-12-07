library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(DT)
library(tidyr)
library(RColorBrewer)

# for local use
survey <- read.csv("~/USAREC/ARSOF_all_survey_results.csv", stringsAsFactors = FALSE)
# for AWS use
#survey <- read.csv("ARSOF_all_survey_results.csv", stringsAsFactors = FALSE, check.names = FALSE)


# survey_dem <- survey[, 1:11]
# colnames(survey_dem) <- c("rank","rank_coded","gender","age_range","installation","ethnicity",
#                           "CMF","SORB_installation","profile","marital_status","num_children")
# survey_dem$rank <- factor(survey_dem$rank, levels = c("PV2", "PFC","SPC","CPL","SGT","SSG","SFC"))
# survey_dem$CMF <- as.factor(survey_dem$CMF)
# survey_dem$ethnicity <- factor(survey_dem$ethnicity)
# levels(survey_dem$ethnicity)[1]<-"AMERICAN INDIAN\n or ALASKAN NATIVE"

survey1 <- survey
colnames(survey1)[1:11] <- c("rank","rank_coded","gender","age_range","installation","ethnicity",
                             "CMF","SORB_installation","profile","marital_status","num_children")
colnames(survey1)[16:42] <- c("we_sf","md_sf","cp_sf","we_ca","md_ca","cp_ca",
                           "we_po","md_po","cp_po", "we_rr","md_rr","cp_rr",
                           "we_160","md_160","cp_160", "sf_gb","ca","po","soar","rr",
                           "consider","reclass","choice_sf","choice_ca",
                           "choice_po","choice_soar","choice_rr")
survey1$rank <- factor(survey1$rank, levels = c("PV2", "PFC","SPC","CPL","SGT","SSG","SFC"))
survey1$CMF <- as.factor(survey1$CMF)
survey1$ethnicity <- factor(survey1$ethnicity)
levels(survey1$ethnicity)[1]<-"AMERICAN INDIAN\n or ALASKAN NATIVE"
survey1$num_children <- factor(survey1$num_children)
survey1 <- filter(survey1, marital_status != "")
survey1 <- filter(survey1, marital_status != "UNKNOWN")
palette_yn <- c("red3","steelblue")
palette_4 <- c("steelblue", "green3","orange", "red3")
palette_5 <- c("steelblue", "green3","yellow", "orange", "red3")






shinyServer(function(input,output) {
  ###### read in CSV
  #datasetInput <- reactive({
    #infile <- input$file1
    #if (is.null(infile))
      #return(NULL)
    #read.csv(infile$datapath)
  #})
  # output$file <- renderTable({
  #   infile <- input$file1
  #   survey_df <- read.csv(infile$datapath)
  #})
  # output$survey_df_head <- renderTable({
  #   if(is.null(datasetInput)){
  #     #File not uploaded yet
  #     return(NULL)
  #   }
  #   survey_df <- datasetInput()
  #   head(survey_df)
  # })
  
  ###### Demographics Plot #########
  output$dem_plot <- renderPlot({
    data1 <- switch(input$var,
                   "Rank"= survey1$rank,
                   "Gender" = survey1$gender,
                   "Age Range" = survey1$age_range,
                   "Ethnicity" = survey1$ethnicity,
                   "CMF" = survey1$CMF,
                   "SORB Installation" = survey1$SORB_installation,
                   "Marital Status" = survey1$marital_status,
                   "Number of Children" = survey1$num_children)
    # data1 <- switch(input$var,
    #                 "Rank"= survey_dem$rank,
    #                 "Gender" = survey_dem$gender,
    #                 "Age Range" = survey_dem$age_range,
    #                 "Ethnicity" = survey_dem$ethnicity,
    #                 "CMF" = survey_dem$CMF,
    #                 "SORB Installation" = survey_dem$SORB_installation,
    #                 "Marital Status" = survey_dem$marital_status,
    #                 "Number of Children" = survey_dem$num_children)
    
    #calculate counts and percentages
    data11 <- switch(input$var,
                     "Rank"= "rank",
                     "Gender" = "gender",
                     "Age Range" = "age_range",
                     "Ethnicity" = "ethnicity",
                     "CMF" = "CMF",
                     "SORB Installation" = "SORB_installation",
                     "Marital Status" = "marital_status",
                     "Number of Children" = "num_children")
    # Use Non-standard evaluation version of group_by (group_by_)
    #data22 <- as.data.frame(dplyr::summarize(group_by_(survey_dem, data11), n = n()))
    data22 <- as.data.frame(dplyr::summarize(group_by_(survey1, data11), n = n()))
    data22$percent <- round(data22$n/sum(data22$n)*100,1)
    
    ## Variables for adjusting the angle/hjust/vjust of x-axis labels based on data selected
    if(data11 %in% c("rank","gender","age_range","num_children","CMF") == TRUE){
      ang <- 0
      vert <- 0
      horz <- .5
    }else {
      ang <- 45
      vert <- 1
      horz <- 1
    }
    
    if(input$plot_type == "Count"){
      ggplot(survey1, aes(x = data1)) +
        geom_bar(fill ="steelblue") +
        theme(axis.title = element_blank()) +
        ggtitle("Number of Respondents") +
        theme(plot.title = element_text(size = 18, hjust = .5)) +
        theme(axis.text.x = element_text(size = 12,angle = ang, vjust = vert, hjust = horz))
      
    } else if (input$plot_type=="Percent"){
      ggplot(data22, aes(x = data22[,1], y = data22$percent)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme(axis.title = element_blank()) +
        ggtitle("Percentage of Respondents") +
        theme(plot.title = element_text(size = 18, hjust = .5)) +
        theme(axis.text.x = element_text(size = 12, angle = ang, vjust = vert, hjust = horz)) +
        geom_text(aes(label = paste(data22$percent,"%")), vjust = -.4)
        
    }
     
  }
  )
  ####### Demographics Table ###############
  output$mytable <- renderTable({
    data11 <- switch(input$var,
                     "Rank"= "rank",
                     "Gender" = "gender",
                     "Age Range" = "age_range",
                     "Ethnicity" = "ethnicity",
                     "CMF" = "CMF",
                     "SORB Installation" = "SORB_installation",
                     "Marital Status" = "marital_status",
                     "Number of Children" = "num_children")
    data22 <- as.data.frame(dplyr::summarize(group_by_(survey1, data11), n = n()))
    data22$percent <- round(data22$n/sum(data22$n)*100,1)
    colnames(data22) <- c(input$var, "Number","Percent")
    data22
    })
  
  ######## Knowledge Plot #########
  output$know_plot <- renderPlot({
    
    # survey_know <- survey[,16:30]
    # colnames(survey_know) <- c("we_sf","md_sf","cp_sf","we_ca","md_ca","cp_ca",
    #                            "we_po","md_po","cp_po", "we_rr","md_rr","cp_rr",
    #                            "we_160","md_160","cp_160")
    survey_know <- survey1[,16:30]
    names <- names(survey_know)
    my_df <- data.frame()
    for(i in names){
      df1 <- dplyr::summarize(group_by_(survey_know,i), count = n())
      df1$quest <- i
      names(df1)[1]<- "scale"
      df2 <- tidyr::spread(df1, scale , count)
      colnames(df2)[7] <- "unknown"
      my_df <- rbind(my_df,df2)
    }
    my_df$unknown <- NULL
    names(my_df)<- c("quest","none","some1","some2","some3","very")
    my_df2 <- gather(my_df, key = response, value = count, none, some1, some2, some3, very)
    my_df3 <- plyr::ddply(my_df2, "quest", transform, percent_resp = count/sum(count))
    limit <- c("we_sf","md_sf","cp_sf","we_ca","md_ca","cp_ca","we_po","md_po",
               "cp_po", "we_rr","md_rr","cp_rr","we_160","md_160","cp_160")

    label <- c("SF Work Environment", "SF Mission/Deployment","SF Career Progression",
               "CA Work Environment", "CA Mission/Deployment","CA Career Progression",
               "PsyOp Work Environment", "PsyOp Mission/Deployment","PsyOp Career Progression",
               "75th RR Work Environment", "75th RR Mission/Deployment","75th RR Career Progression",
               "160th Work Environment", "160th Mission/Deployment","160th Career Progression")
    ggplot(data = my_df3, aes(x = quest, y = percent_resp, fill = response)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      #scale_fill_discrete(labels = c("1 - Not At All","2 - Minimally","3 - Somewhat",
      #                               "4 - Quite", "5 - Very"))+
      ggtitle("How knowledgeable are you about...") +
      theme(plot.title = element_text(size = 20)) +
      theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5))+
      scale_x_discrete(limits = limit, labels = label) +
      labs(fill = "Response") +
      theme(axis.title.x = element_blank()) +
      ylab("Percentage of Respondants") +
      theme(panel.grid.major = element_blank())+
      theme(panel.grid.minor=element_blank()) +
      geom_vline(xintercept = 3.5, size = 1.25) +  geom_vline(xintercept = 6.5,size = 1.25) + 
      geom_vline(xintercept = 9.5,size = 1.25) + geom_vline(xintercept = 12.5,size = 1.25) +
      scale_fill_brewer(palette = "Spectral",labels = c("1 - Not At All","2 - Minimally","3 - Somewhat",
                                                        "4 - Quite", "5 - Very"))

  })
  ###### Know anyone plot#####
  output$any_plot <- renderPlot({
    anyone <- survey[,31:35]
    colnames(anyone) <- c("sf_gb","ca","po","soar","rr")
    any2 <- gather(anyone, "org","response",1:5)
    any3<-dplyr::summarize(group_by(any2, org, response), count = n())
    any4 <- filter(any3, response != "")
    any44 <- mutate(any4, freq = count/sum(count))
    any44 <- mutate(group_by_(any44, "org"), label_y_2 = cumsum(freq)-.5*freq)
    
    palette_yn = c("red3","steelblue")
    ggplot(data = any44, aes(x = org, y = freq, fill = response))+
      geom_bar(stat = "identity", width = .5) +
      theme_bw() +
      ggtitle("Do you know anyone currently serving in ARSOF?")+
      theme(plot.title = element_text(size = 20)) +
      labs(fill = "Response") +
      #theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5))+
      theme(axis.title = element_blank()) +
      scale_x_discrete(labels = c("Special Forces/\n Green Berets","Civil Affairs", 
                                  "Psychological\n Operations","75th Ranger Regiment",
                                  "160th SOAR"),
                       limits = c("sf_gb","ca","po","rr","soar")) +
      geom_text(aes(y = (1-label_y_2), label = paste(round(freq,2)*100,"%")), 
                color = "white")+
      scale_fill_manual(values = palette_yn)
  })
  ######### Considering plot ########
  output$newjob_plot <- renderPlot({
    considering <- survey1[, c(1:11,36)]
    
    # colnames(considering) <- c("rank","rank_coded","gender","age_range","installation","ethnicity",
    #                            "CMF","SORB_installation","profile","marital_status",
    #                            "num_children","quest")
    # considering$rank <- factor(considering$rank, levels = c("PV2", "PFC","SPC","CPL",
    #                                                        "SGT","SSG","SFC"))
    # considering$CMF <- as.factor(considering$CMF)
    # considering$ethnicity <- factor(considering$ethnicity)
    # levels(considering$ethnicity)[1]<-"AMERICAN INDIAN\n or ALASKAN NATIVE"
    # considering$num_children <- factor(considering$num_children)
    # palette_yn = c("red3","steelblue")
    
    data_con2 <- switch(input$con_var,
                       "All" = "all",
                       "Rank"= "rank",
                       "Gender" = "gender",
                       "Age Range" = "age_range",
                       "Ethnicity" = "ethnicity",
                       "CMF" = "CMF",
                       "SORB Installation" = "SORB_installation",
                       "Marital Status" = "marital_status",
                       "Number of Children" = "num_children")
    
    if(data_con2 %in% c("all","rank","gender","age_range","num_children","CMF") == TRUE){
      ang <- 0
      vert <- 0
      horz <- .5
      fsize <- 12
    }else{
      ang <- 45
      vert <- 1
      horz <- 1
      fsize <- 12
    }
    if(data_con2 == "CMF"){
      labelsize <- 4
    }else{
      labelsize <- 6
    }
    
    if(data_con2 == "all"){
      con1 <- dplyr::summarize(group_by(considering, consider), n = n())
      con1 <- filter(con1, consider != "")
      con1$percent <- round(con1$n/sum(con1$n)*100,1)
      
      ggplot(con1, aes(x = consider, y = percent, fill = consider)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste(percent,"%")),vjust = 1.5, color = "white")+
        theme_bw() +
        scale_fill_manual(values = palette_yn, guide = FALSE) +
        theme(axis.title.x = element_blank())+
        ylab("Percentage")+
        theme(axis.text.x = element_text(size = fsize,angle = ang, vjust = vert, hjust = horz))+
        #ggtitle("Considering a new job/career/MOS?") +
        theme(plot.title = element_text(size = 20))
    }else {
      con2 <- as.data.frame(dplyr::summarize(group_by_(considering, data_con2, "consider"), count = n()))
      con2 <- filter(con2, consider != "")
      con22 <- mutate(group_by_(con2,data_con2), freq = count/sum(count))
      con22 <- mutate(group_by_(con22, data_con2), label_y_2 = cumsum(freq)-.5*freq)
      #con22 <- mutate(group_by_(con22, data_con2), label_y = cumsum(freq))  #for label placement
      data_con <- switch(input$con_var,
                         "All" = "all",
                         "Rank"= con22$rank,
                         "Gender" = con22$gender,
                         "Age Range" = con22$age_range,
                         "Ethnicity" = con22$ethnicity,
                         "CMF" = con22$CMF,
                         "SORB Installation" = con22$SORB_installation,
                         "Marital Status" = con22$marital_status,
                         "Number of Children" = con22$num_children)
      
      ggplot(con22, aes(x = data_con, y = freq, fill = consider))+
        geom_bar(stat = "identity")+
        theme_bw()+
        scale_fill_manual(values = palette_yn) +
        theme(axis.title.x = element_blank())+
        ylab("Percentage")+
        theme(axis.text.x = element_text(size = fsize,angle = ang, vjust = vert, hjust = horz))+
        labs(fill = "Response") +
        theme(plot.title = element_text(size = 20)) +
        geom_text(aes(y = (1-label_y_2), label = paste(round(freq,2)*100,"%")), 
                  color = "white", size = labelsize)
      
      #ggplot(considering, aes(x = quest))+
        #geom_bar()
    }
    
  })
  ########### Propensity Plot #################
  output$reclass_plot <- renderPlot({
    propense <- survey1[, c(1:11,31:42)]
    prop1 <- dplyr::summarize(group_by(propense,reclass), n = n())
    prop1 <- filter(prop1, reclass != "")
    prop1$percent <- round(prop1$n/sum(prop1$n)*100,1)
    prop1$reclass <- factor(prop1$reclass, levels = c("Definitely will reclassify",
                                                      "Probably will reclassify",
                                                      "Probably will not reclassify",
                                                      "Definitely will not reclassify"))
    data_prop <- switch(input$prop_var,
                        "All" = "all",
                        "Rank"= "rank",
                        "Gender" = "gender",
                        "Age Range" = "age_range",
                        "Ethnicity" = "ethnicity",
                        "CMF" = "CMF",
                        "SORB Installation" = "SORB_installation",
                        "Marital Status" = "marital_status",
                        "Number of Children" = "num_children")
    
    if(data_prop == "all"){
      ggplot(prop1, aes(x = reclass, y = percent)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        #geom_text(aes(label = paste(percent,"%")),vjust = 1.5, color = "white")+
        theme_bw() +
        theme(axis.title.x = element_blank())+
        ylab("Percentage")+
        theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1))+
        scale_x_discrete(labels = c("Definitely will","Probably will",
                                    "Probably will not", "Definitely will not"))
      
    }else{
      prop2 <- as.data.frame(dplyr::summarize(group_by_(propense, data_prop, "reclass"), count = n()))
      prop2 <- filter(prop2, reclass != "")
      prop2$reclass <- factor(prop2$reclass, levels = c("Definitely will reclassify",
                                                        "Probably will reclassify",
                                                        "Probably will not reclassify",
                                                        "Definitely will not reclassify"))
      prop2 <- arrange_(prop2,data_prop,"reclass")
      prop22 <- mutate(group_by_(prop2,data_prop), freq = count/sum(count))
      prop22 <- mutate(group_by_(prop22, data_prop), label_y_2 = 1-(cumsum(freq)-.5*freq))
      data_prop2 <- switch(input$prop_var,
                           "All" = "all",
                           "Rank"= prop22$rank,
                           "Gender" = prop22$gender,
                           "Age Range" = prop22$age_range,
                           "Ethnicity" = prop22$ethnicity,
                           "CMF" = prop22$CMF,
                           "SORB Installation" = prop22$SORB_installation,
                           "Marital Status" = prop22$marital_status,
                           "Number of Children" = prop22$num_children)
      
      if(data_prop %in% c("all","rank","gender","age_range","num_children","CMF") == TRUE){
        ang <- 0
        vert <- 0
        horz <- .5
        fsize <- 12
      }else{
        ang <- 45
        vert <- 1
        horz <- 1
        fsize <- 12
      }
      if(data_prop == "CMF"){
        labelsize <- 4
      }else{
        labelsize <- 6
      }
      
      ggplot(prop22, aes(x = data_prop2, y = freq, fill = reclass))+
        geom_bar(stat = "identity")+
        theme_bw()+
        scale_fill_manual(values = palette_4) +
        theme(axis.title.x = element_blank())+
        ylab("Percentage")+
        theme(axis.text.x = element_text(size = fsize,angle = ang, vjust = vert, hjust = horz))+
        labs(fill = "Response") +
        #geom_text(aes(y = (1-label_y_2), label = paste(round(freq,2)*100,"%")), 
        #          color = "white", size = labelsize) +
        geom_text(aes(y = label_y_2, label = paste(round(freq,2)*100,"%")), 
                  color = "white") +
        ggtitle("Propensity")
      
    }
    
  })
  output$reclass_table <- renderTable({
    if(input$reclass_checkbox==TRUE){
      propense <- survey1[, c(1:11,31:42)]
      data_prop <- switch(input$prop_var,
                          "All" = "all",
                          "Rank"= "rank",
                          "Gender" = "gender",
                          "Age Range" = "age_range",
                          "Ethnicity" = "ethnicity",
                          "CMF" = "CMF",
                          "SORB Installation" = "SORB_installation",
                          "Marital Status" = "marital_status",
                          "Number of Children" = "num_children")
      if(data_prop == "all"){
        prop2 <- as.data.frame(dplyr::summarize(group_by_(propense, "reclass"), count = n()))
        prop2 <- filter(prop2, reclass != "")
        prop2$reclass <- factor(prop2$reclass, levels = c("Definitely will reclassify",
                                                          "Probably will reclassify",
                                                          "Probably will not reclassify",
                                                          "Definitely will not reclassify"))
        prop2 <- arrange_(prop2,"reclass")
        prop22 <- mutate(prop2, freq = count/sum(count))
        colnames(prop22) <- c(input$prop_var, "Number","Percent")
        prop22
        
      }else{
        prop2 <- as.data.frame(dplyr::summarize(group_by_(propense, data_prop, "reclass"), count = n()))
        prop2 <- filter(prop2, reclass != "")
        prop2$reclass <- factor(prop2$reclass, levels = c("Definitely will reclassify",
                                                          "Probably will reclassify",
                                                          "Probably will not reclassify",
                                                          "Definitely will not reclassify"))
        prop2 <- arrange_(prop2,data_prop,"reclass")
        prop22 <- mutate(group_by_(prop2,data_prop), freq = count/sum(count))
        colnames(prop22) <- c(input$prop_var,"Response", "Number","Percent")
        prop22 
      }
      
    }
   
  })
  

  output$byknow_plot <- renderPlot({
    prop_know <- survey1[,c(31:35,37)]
    pk1 <- gather(prop_know, "org","response",1:5)
    pk2 <- dplyr::summarize(group_by(pk1, org, response,reclass), count = n())
    pk2[pk2==""] <- NA
    pk2 <- na.omit(pk2)
    # pk2 <- filter(pk2, reclass != "")
    # pk2 <- filter(pk2, response != "")
    pk2$reclass <- factor(pk2$reclass, levels = c("Definitely will reclassify",
                                                  "Probably will reclassify",
                                                  "Probably will not reclassify",
                                                  "Definitely will not reclassify"))
    
    pk3 <- arrange(pk2,org, response, reclass)
    pk4 <- mutate(pk3, freq = count/sum(count))
    pk4 <- mutate(group_by(pk4, org, response), label_y_2 = cumsum(freq)-.6*freq)
    pk4$org_resp <- paste(pk4$org, pk4$response, sep = "_")
    
    ggplot(data = pk4, aes(x = org_resp, y = freq, fill = reclass))+
      geom_bar(stat = "identity") +
      theme_bw()+
      scale_fill_manual(values = palette_4) +
      geom_text(aes(y = 1-label_y_2, label = paste(round(freq,2)*100,"%")), 
                color = "white") +
      labs(fill = "Response") +
      theme(axis.title.x = element_blank())+
      ylab("Percentage")+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
      scale_x_discrete(labels = c("CA-No","CA-Yes","PO-No", "PO-Yes","RR-No","RR-Yes",
                                  "SF-No", "SF-Yes","SOAR-No","SOAR-Yes"))
  })
  output$bycon_plot <- renderPlot({
    prop_con <- survey1[,36:37]
    prop_con1 <- dplyr::summarize(group_by(prop_con, consider, reclass), count=n())
    prop_con1[prop_con1==""] <- NA
    prop_con1 <- na.omit(prop_con1)
    prop_con1$reclass <- factor(prop_con1$reclass, levels = c("Definitely will reclassify",
                                                              "Probably will reclassify",
                                                              "Probably will not reclassify",
                                                              "Definitely will not reclassify"))
    prop_con2 <- arrange(prop_con1,consider, reclass)
    prop_con2 <- mutate(prop_con2, freq = count/sum(count))
    prop_con2 <- mutate(group_by(prop_con2, consider), label_y_2 = cumsum(freq)-.5*freq)
    if(input$facet_check==FALSE){
      ggplot(data = prop_con2, aes(x = consider, y = freq, fill = reclass))+
        geom_bar(stat="identity")+
        theme_bw()+
        scale_fill_manual(values = palette_4) +
        geom_text(aes(y = 1-label_y_2, label = paste(round(freq,2)*100,"%")), 
                  color = "white") +
        labs(fill = "Response") +
        theme(axis.title.x = element_blank())+
        ylab("Percentage")
    }else{
      ggplot(data = prop_con2, aes(x = consider, y = freq))+
        geom_bar(stat="identity", fill = "steelblue")+
        facet_wrap(~reclass)+
        theme_bw()+
        # scale_fill_manual(values = palette_4) +
        # geom_text(aes(y = 1-label_y_2, label = paste(round(freq,2)*100,"%")), 
        #           color = "white") +
        #labs(fill = "Response") +
        theme(axis.title.x = element_blank())+
        ylab("Percentage")
    }
    
  })
  
  output$choice_plot <- renderPlot({
    choice <- survey1[,38:42]
    choice1 <- gather(choice, "org","response",1:5)
    choice1 <- dplyr::summarize(group_by(choice1, org, response), count = n())
    choice1 <- filter(choice1, response != "")
    choice1 <- mutate(choice1, freq = count/sum(count))
    choice1 <- mutate(group_by(choice1, org), label_y_2 = cumsum(freq)-.5*freq)
    choice1$response <- factor(choice1$response)
    choice1$response <- plyr::revalue(choice1$response, c("1"="First Choice", "2" = "Second Choice","3" = "Third Choice",
                                                          "4"="Fourth Choice","5" = "Fifth Choice"))
    
    ggplot(data = choice1, aes(x = org, y = freq, fill = response))+
      geom_bar(stat = "identity", width = .5) +
      theme_bw()+
      labs(fill = "") +
      theme(axis.title = element_blank())+
      scale_x_discrete(labels = c("Special Forces/\n Green Berets","Civil Affairs", 
                                  "Psychological\n Operations","75th Ranger Regiment",
                                  "160th SOAR"),
                       limits = c("choice_sf","choice_ca","choice_po","choice_rr","choice_soar"))+
      geom_text(aes(y = 1-label_y_2, label = paste(round(freq,2)*100,"%")), 
                color = "black")+
      scale_fill_manual(values = palette_5)
  })
  
  


}   ##end of shinyserver function
)