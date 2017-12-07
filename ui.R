# Shiny app for USAREC survey analysis charts

shinyUI(
  fluidPage(
    titlePanel("Survey Analysis"),
  tabsetPanel(
    tabPanel(title = "Demographics",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("var",
                             label = "Choose what to chart.",
                             choices = list("Rank","Gender","Age Range","Ethnicity", "CMF",
                                            "SORB Installation", "Marital Status", "Number of Children"),
                             selected = "Rank"),
                 selectInput("plot_type",
                             label = "Count or Percent",
                             choices = list("Count","Percent"),
                             selected = "Count")
                  ),
                
                # show the plot and the summary data table
                mainPanel(
                  #h4("plot"),
                  plotOutput("dem_plot"),
                  tableOutput("mytable"),
                  tableOutput("survey_df_head")
                )
             )
    ),
    tabPanel(title = "Knowledge",
             mainPanel(h3("Knowledge Questions"),
                       plotOutput("know_plot"), 
                       tags$br(),  #add a break between the plots
                       tags$br(),
                       plotOutput("any_plot"))
               
             ),
    tabPanel(title = "Considering",
             sidebarLayout(
               sidebarPanel(
                 selectInput("con_var",
                             label = "Choose what to chart by:",
                             choices = list("All","Rank","Gender","Age Range","Ethnicity", "CMF",
                                            "SORB Installation", "Marital Status", "Number of Children"),
                             selected = "All")
               ),
               mainPanel(h3("Are you actively looking for or considering a different \n 
                            Army job/career/MOS now or in the next year?"),
                         plotOutput("newjob_plot"))
             )
             
    ),
    tabPanel(title = "Propensity",
             sidebarLayout(
               sidebarPanel(
                 selectInput("prop_var",
                             label = "Choose what to chart by:",
                             choices = list("All","Rank","Gender","Age Range","Ethnicity", "CMF",
                                            "SORB Installation", "Marital Status", "Number of Children"),
                             selected = "All"),
                 checkboxInput("reclass_checkbox","See the data table.", value = FALSE),
                 checkboxInput("facet_check", "Display facet chart.", value = FALSE)
               ),
               mainPanel(h3("Propensity to Reclassify"),
                         plotOutput("reclass_plot"),
                         tableOutput("reclass_table"),
                         h3("Propensity by knowing someone in ARSOF response"),
                         plotOutput("byknow_plot", height = 600, width = 600),
                         h3("Propensity by considering a new job response"),
                         plotOutput("bycon_plot", width = 600))
             )),
    tabPanel(title = "Choice",
             mainPanel(h3("Which Branch/Unit Would You Choose?"),
                       plotOutput("choice_plot")))
    
  
)))
  
  
