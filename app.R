


#  Shiny app - version 1  (.R language)

#Software tool for policymakers to evaluate the impacts of climate change on agricultural economies and health outcome specifically “Diseases of Despair” in farming communities.

#The software tool runs at this address https://cornar.shinyapps.io/thirdapp/

#The software tool requires as input the file Dataset.xlsx (DOI 10.5281/zenodo.15001294)

#The software tool requires also as input files the .shp files (available in 'Country map.7z' file) as described in "Technical Document.docx"
#Github resource: https://github.com/acc25uom/climate_change_study

#  Shiny app has a DOI.


library(shiny)
library(leaflet)
#library(rgdal)
library(terra)
library(sf)
library(here)
library(tmap)
library(DiagrammeR)
library(ggplot2)
library(DiagrammeRsvg)
library(readxl)

library(rmapshaper)

# Set the seed
set.seed(123)

# Expand the limit of the memory
options(shiny.maxRequestSize = 1000 * 1024^2)


library(rsconnect)
#Command to deploy to online server


prob_dd_gt_2 =  c(NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA)





# UI
ui <- fluidPage(
  titlePanel("Causal Inference Toolkit for Climate, Economy and Health"),

  sidebarLayout(
    sidebarPanel(


      fileInput("in_file", "Data Input file:",accept=c("csv/xls/xlsx", "text/comma-separated-values,text/plain", ".xlsx")),

      fileInput('inputdata', 'Country map shape file',accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),

      hr(),
      actionButton("calculate", " Calculate Direct and Indirect Effects "),
      width = 5


    ),

    mainPanel(
      tabsetPanel(

        tabPanel("Project Framework (DAG)",
                 hr(),

                 radioButtons("dist", "Model selection:",
                              c("Model 1" = "model1",
                                "Model 2" = "model2")),
                              #selected = "model1"),
                 #plotOutput("distPlot"),

                 htmlOutput("testHTML1"),
                 grVizOutput("dag_plot"),  # DAG plot
                 textOutput("text1"),
                 textOutput("text2"),
                 hr(),
                 textOutput("text31"),
                 #textOutput("text3"),
                 textOutput("text4"),
                 hr(),
                 hr(),
                 #plotOutput("plot1"),  # Box plot
                 htmlOutput("testHTML2"),
                 #hr(),
                 grVizOutput("dag_plot2"),
                 textOutput("text20"),
                 textOutput("text21"),
                 hr(),
                 textOutput("text22"),
                 textOutput("text23"),

                 hr(),
                 textOutput("text24"),
                 textOutput("text25"),
                 hr()
        ),

        tabPanel("Health Maps",
                 fluidRow(
                   column(width = 6, h4("Indirect effect"), leafletOutput("map1")),  # Map 1
                   column(width = 6, h4("Direct effect"), leafletOutput("map2"))  # Map 2
                 ),
                 hr(),
                 fluidRow(
                    column(width = 12, h4("Direct Effects (States names)"), leafletOutput("map3"))  # Map 3
                 )
        ),

        tabPanel("Health Numerical Results",

                 tableOutput("table1")

        ),
        tabPanel("Climate",

                 #tableOutput("table1")
                 fluidRow(
                   column(width = 6, h4("Maximum Temperature for period 2010 - 2021"), leafletOutput("map4")),  # Map 4
                   column(width = 6, h4("Maximum Precipitation for period 2010 - 2021"), leafletOutput("map5"))  # Map 5
                 ),
                 hr(),
                 fluidRow(
                   column(width = 6, h4("SPEI index (minimum value)"), leafletOutput("map6"))  # Map 6
                 )
        ),
        tabPanel("Economy",
                 fluidRow(
                   column(width = 6, h4("GDP from Agriculture for period 2010 - 2021 (minimum value)"), leafletOutput("map7")),  # Map 7
                   column(width = 6, h4("GDP from Agriculture for period 2010 - 2021 (maximum value)"), leafletOutput("map8")),  # Map 8

                 ),
                 hr()
                 #tableOutput("table1")

        ),
        tabPanel("Workers background",
                 fluidRow(
                   column(width = 6, h4("Credits to farmers"), leafletOutput("map9")),  # Map 7
                   column(width = 6, h4("Irrigation areas"), leafletOutput("map10")),  # Map 8

                 ),
                 hr()
                 #tableOutput("table1")

        ),
        tabPanel("App Info",
                 hr(),
                 textOutput("text5"),
                 hr(),
                 textOutput("text6"),
                 hr(),
                 textOutput("text7"),
                 hr(),

                 htmlOutput("testHTML"),
                 textOutput("text9"),

                 hr(),
                 textOutput("text10"),
                 hr(),
                 tableOutput("table2"),
                 hr(),
                 textOutput("text11"),
                 hr(),
                 tableOutput("table3"),
                 hr()

        ),
        tabPanel("Contact",
                 hr(),
                 textOutput("contactinfo"),
                 hr()

        )

      )
    )
  )
)

# Server
server <- function(input, output,session) {



 # Create a node data frame (ndf) for DAG plot
  ndf <- create_node_df(
    n         = 3,
    label     = c( "Temperature", "GDP Agriculture", "Diseases of Despair \n Outcome"),
    shape     = c(rep("rectangle", 3)),
    style     = "empty",
    fontsize  = 6,
    fixedsize = TRUE,
    height    = .5,
    width     = .8,
    color     = "gray80",
    x         = c(1, 2, 3),
    y         = c(1, 2, 1)
  )

  # Create an edge data frame (edf) for DAG plot
  edf <- create_edge_df(
    from     = c(1, 1, 2),
    to       = c(2, 3, 3),
    label    = c("a", "c", "b"),
    fontsize = 6,
    minlen   = 1,
    color    = c("blue", "red", "blue")
  )

  ndf2 <- create_node_df(
    n         = 6,
    label     = c( "Temperature", "GDP Agriculture", "Diseases of Despair \n Outcome", "Credit to \n farmers","Irrigation area","\ua0"),
    shape     = c(rep("rectangle", 5)),
    style     = "empty",
    fontsize  = 6,
    fixedsize = TRUE,
    height    = c(.5,.5,.5,.5,.5,0.01),
    width     = c(.8,.8,.8,.8,.8,0.01),
    color     = "gray80",
    x         = c(1, 2, 3,3.5,0.7,1.6),
    y         = c(1, 2, 1,2.5,2.1,1.6)
  )

  # Create an edge data frame (edf) for DAG plot
  edf2 <- create_edge_df(
    from     = c(1, 1, 2,4,4,5),
    to       = c(2, 3, 3,2,3,6),
    label    = c("a", "c", "b","d","e","f"),
    fontsize = 6,
    minlen   = 1,
    color    = c("blue", "red", "blue", "gray80", "gray80")
  )

  # Create a graph with the ndf and edf for DAG plot
  dag <- create_graph(
    nodes_df = ndf,
    edges_df = edf
  )

  dag2 <- create_graph(
    nodes_df = ndf2,
    edges_df = edf2
  )

  # Render DAG plot
  output$dag_plot <- renderGrViz({
    render_graph(dag, as_svg = TRUE)
  })

  # Render DAG plot
  output$dag_plot2 <- renderGrViz({
    render_graph(dag2, as_svg = TRUE)
  })


  output$testHTML1 <- renderText({
    paste("<u><b>Model 1: </b></u>")
  })

  output$testHTML2 <- renderText({
    paste("<u><b>Model 2: </b></u>")
  })

  output$contactinfo <- renderText({ "For feedback please contact us at email: tr35@tutanota.com" })

  output$text1 <- renderText({ "Direct Effect = c" })



  output$text2 <- renderText({ "Indirect Effect = a x b" })


  output$text31 <- renderText( { "Equations: \ua0 Outcome = Temperature x c + GDP_Agriculture x b " } )


  output$text4 <- renderText( { "\ua0 \ua0 \ua0 \ua0 \ua0 \ua0 \ua0 \ua0 \ua0 \ua0 GDP_Agriculture = Temperature x a" } )


  output$text5 <- renderText( { "This causal inference software toolkit requires two inputs : 1) The data input file and 2) The country map shape file(s):" } )

  output$text6 <- renderText( { "1) The data input file is mandatory and it must be in Excel format and to have at least 3 different pages corresponding to the 'Outcome', 'Maximum Temperature' and 'GDP from Agriculture' and in the format shown below." } )

  output$text7 <- renderText( { "2) The country map shape file is not obligatory, but if it is used, then it must consist of 6 different files with the file extensions \ua0.shp, \ua0.dbf, \ua0.sbn, \ua0.sbx, \ua0.shx, \ua0.prj. \ua0 For example, for the country of India, the files could be 'India.shp', 'India.dbf', 'India.sbn', 'India.sbx', 'India.shx' and 'India.prj'. \ua0 To select all 6 files you must press the keys SHIFT + Left Button mouse." } )

  output$text9 <- renderText( { "Observation: The order of country states (or country counties) from the Excel file must be the same as the order of states/counties available in the .shp (shape) map input file. " } )

  output$text10 <- renderText( { "Example of 'Outcome' excel page for country of India in the excel data input file:" } )

  output$text11 <- renderText( { "Example of 'Maximum Temperature' excel page for country of India in the excel data input file:" } )


  output$testHTML <- renderText({
    paste("<u>The Excel format of data input file: </u>")
  })

  output$text20 <- renderText({ "Direct Effect = c" })

  output$text21 <- renderText({ "Indirect Effect = a x b" })

  output$text22 <- renderText( { "Equations: \ua0 Outcome = Temperature x c + GDP_Agriculture x b + Credit_to_farmer x e" } )

  output$text23 <- renderText( { "\ua0 \ua0 \ua0 \ua0 \ua0 \ua0 \ua0 \ua0 \ua0 \ua0 GDP_Agriculture = Temperature x a + Credit_to_farmer x d + (Temperature x a) : (Irrigation_area x f)" } )



  output$text24 <- renderText( { "Legend:\ua0  x \ua0 is multiplication sign" } )

  output$text25 <- renderText( { "\ua0 \ua0 \ua0 \ua0 \ua0 \ua0 \ua0 \ua0 : \ua0 is interaction effect between two variables (i.e. multiplication of two variables)" } )


  Plotinputdata <-matrix(rpois(444,37),ncol=12)

  Designinputdata2 <-matrix(rpois(444,37),ncol=12)

   Plotinputdata  <- cbind(c( "Andaman & Nicobar",
                                 "Chandigarh",
                                 "Daman and Diu and Dadra and Nagar Haveli",
                                 "Delhi",
                                 "Haryana",
                                 "Jharkhand",
                                 "Karnataka",
                                 "Kerala",
                                 "Lakshadweep",
                                 "Madhya Pradesh",
                                 "Maharashtra",
                                 "Odisha",
                                 "Puducherry",
                                 "Tamilnadu",
                                 "Chhattishgarh",
                                 "Telengana",
                                 "Andhra Pradesh",
                                 "Puducherry",
                                 "Goa",
                                 "Himachal Pradesh",
                                 "Punjab",
                                 "Rajasthan",
                                 "Gujarat",
                                 "Uttarakhand",
                                 "Uttar Pradesh",
                                 "Sikkim",
                                 "Assam",
                                 "Arunachal Pradesh",
                                 "Nagaland",
                                 "Manipur",
                                 "Mizoram",
                                 "Tripura",
                                 "Meghalaya",
                                 "West Bengal",
                                 "Bihar",
                                 "Ladakh",
                                 "Jammu and Kashmir"), Plotinputdata )


       Designinputdata2  <- cbind(c( "\ua0 \ua0 ",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0",
                                     "\ua0 \ua0"), Designinputdata2 )


       Designinputdata2  <- cbind(c( "Andaman & Nicobar",
                                  "Chandigarh",
                                  "Daman and Diu and Dadra and Nagar Haveli",
                                  "Delhi",
                                  "Haryana",
                                  "Jharkhand",
                                  "Karnataka",
                                  "Kerala",
                                  "Lakshadweep",
                                  "Madhya Pradesh",
                                  "Maharashtra",
                                  "Odisha",
                                  "Puducherry",
                                  "Tamilnadu",
                                  "Chhattishgarh",
                                  "Telengana",
                                  "Andhra Pradesh",
                                  "Puducherry",
                                  "Goa",
                                  "Himachal Pradesh",
                                  "Punjab",
                                  "Rajasthan",
                                  "Gujarat",
                                  "Uttarakhand",
                                  "Uttar Pradesh",
                                  "Sikkim",
                                  "Assam",
                                  "Arunachal Pradesh",
                                  "Nagaland",
                                  "Manipur",
                                  "Mizoram",
                                  "Tripura",
                                  "Meghalaya",
                                  "West Bengal",
                                  "Bihar",
                                  "Ladakh",
                                  "Jammu and Kashmir"), Designinputdata2 )

       colnames(Plotinputdata) <- c("State name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")

       colnames(Designinputdata2) <- c("State name","\ua0 ","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")



  output$table2 <- renderTable(

    #colnames(Directeffect_plot) <- c("Direct Effects"),
    Plotinputdata,
    #Directeffect_plot,
    bordered = TRUE,
    align = 'c',
    digits = 10,
    rownames = TRUE
    #colnames = TRUE

  )

  output$table3 <- renderTable(

    #colnames(Directeffect_plot) <- c("Direct Effects"),
    Designinputdata2,
    #Directeffect_plot,
    bordered = TRUE,
    align = 'c',
    digits = 10,
    rownames = FALSE
    #colnames = TRUE

  )




observeEvent(input$inputdata, {
  myshape<- input$inputdata
  if (is.null(myshape))
    return(NULL)

  dir<-dirname(myshape[1,4])
  print(dir)

  print(myshape[1,4])

  for ( i in 1:nrow(myshape))
  {
    print(myshape[i,4])
    file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))
  }


  getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
  #print(getshp)
  datamyimage <<- st_read(getshp)

  # datamyimage <<- st_simplify(datamyimage, preserveTopology = TRUE,
  #                           dTolerance = 20000)

   datamyimage <<- ms_simplify(datamyimage, keep = 0.001, keep_shapes = TRUE)
  #plot(shape)

  })

  observeEvent(input$in_file,{

          output$map4 <- renderLeaflet({

              temperature_train_unlist = vector(,37);
              precipitation_train_unlist = vector(,37);
              spei_train_unlist = vector(,37);

              gdp_minimum_unlist = vector(,37);
              gdp_maximum_unlist = vector(,37);

              credit_farmer_unlist = vector(,37);
              irrigation_areas_unlist = vector(,37);


                 #  stop("Error message")
              if (!is.null(input$in_file) & !is.null(input$inputdata))
               {


                #for (i in 1:34)
                #{
                #  if (i != 7  & i != 16 & i != 9 & i != 23 & i != 32)
                    #Lack of maximum temperature : Gujarat (9), Lakshadweep (16 - islands in Indian Ocean) , Odisha (23), West Bengal (32)
                    # Lack of GDP : Dadra and Nagar Haveli (7 - small teritory )
                #  {
                #    print(i)
                    b1 = as.character(1)
                    b2 = as.character(35)

                    train_data <- sprintf("C%s:N%s",b1,b2)

                    test_data <- sprintf("N%s:N%s",b1,b2)

                    farm_size <- sprintf("B%s:B%s",b1,b2)

                    #Training for 2010-2020 and predicting for 2021
                    temperature_train <- read_excel(input$in_file$datapath, sheet = "Maximum Temperature", range = train_data)#S38
                    #temperature_test<- read_excel(input$in_file$datapath, sheet = "Maximum Temperature", range = test_data)#S38
                    #print(nrow(temperature_train))
                    #print(ncol(temperature_train))

                    precipitation_train <- read_excel(input$in_file$datapath, sheet = "Maximum Precipitation", range = train_data)#S38
                    #precipitation_test<- read_excel(input$in_file$datapath, sheet = "Maximum Precipitation", range = test_data)#S38

                    spei_train <- read_excel(input$in_file$datapath, sheet = "SPEI", range = train_data)#S38
                    #spei_test<- read_excel(input$in_file$datapath, sheet = "SPEI", range = test_data)#S38

                    gdp_train <- read_excel(input$in_file$datapath, sheet = "GDP Agriculture", range = train_data)#S38
                    #gdp_test<- read_excel(input$in_file$datapath, sheet = "GDP Agriculture", range = test_data)#S38

                    credit_farmer_train <- read_excel(input$in_file$datapath, sheet = "Credit to farmers", range = train_data)#S38

                    irrigation_areas_train <- read_excel(input$in_file$datapath, sheet = "Irrigation area", range = train_data)#S38

                    #train data
                    #temperature_train_unlist[i]  <- max(unlist(temperature_train))

                    #precipitation_train_unlist[i] <- max(unlist(precipitation_train))

                    #spei_train_unlist[i] <- min(unlist(spei_train))

                    #gdp_minimum_unlist[i] = min(unlist(gdp_train))
                    #gdp_maximum_unlist[i] = max(unlist(gdp_train))


                    #test data
                    #precipitation_test_unlist <- unlist(precipitation_test)

                    #temperature_test_unlist <- unlist(temperature_test)


                    #   }
              #}

                    #stop("Error message")

                 for (i in 1:34)
                 {
                   if (i != 7  & i != 16 & i != 9 & i != 23 & i != 32)
                #     #Lack of maximum temperature : Gujarat (9), Lakshadweep (16 - islands in Indian Ocean) , Odisha (23), West Bengal (32)
                #     # Lack of GDP : Dadra and Nagar Haveli (7 - small teritory )
                   {
                #     print(i)
                #     b1 = as.character(i)
                #     b2 = as.character(i+1)
                #
                #     train_data <- sprintf("C%s:N%s",b1,b2)
                #
                #     test_data <- sprintf("N%s:N%s",b1,b2)
                #
                #     farm_size <- sprintf("B%s:B%s",b1,b2)
                #
                #     #Training for 2010-2020 and predicting for 2021
                #     temperature_train <- read_excel(input$in_file$datapath, sheet = "Maximum Temperature", range = train_data)#S38
                #     temperature_test<- read_excel(input$in_file$datapath, sheet = "Maximum Temperature", range = test_data)#S38
                #
                #     precipitation_train <- read_excel(input$in_file$datapath, sheet = "Maximum Precipitation", range = train_data)#S38
                #     precipitation_test<- read_excel(input$in_file$datapath, sheet = "Maximum Precipitation", range = test_data)#S38
                #
                #     spei_train <- read_excel(input$in_file$datapath, sheet = "SPEI", range = train_data)#S38
                #     spei_test<- read_excel(input$in_file$datapath, sheet = "SPEI", range = test_data)#S38
                #
                #     gdp_train <- read_excel(input$in_file$datapath, sheet = "GDP Agriculture", range = train_data)#S38
                #     gdp_test<- read_excel(input$in_file$datapath, sheet = "GDP Agriculture", range = test_data)#S38
                #
                #     #train data
                     temperature_train_unlist[i]  <- max(as.numeric(unlist(temperature_train[i,])),na.rm = TRUE)
                #
                     precipitation_train_unlist[i] <- max(as.numeric(unlist(precipitation_train[i,])),na.rm = TRUE)
                #
                     spei_train_unlist[i] <- min(as.numeric(unlist(spei_train[i,])),na.rm=TRUE)
                #
                     gdp_minimum_unlist[i] = min(as.numeric(unlist(gdp_train[i,])),na.rm=TRUE)
                     gdp_maximum_unlist[i] = max(as.numeric(unlist(gdp_train[i,])),na.rm=TRUE)

                     credit_farmer_unlist[i] = max(as.numeric(unlist(credit_farmer_train[i,])),na.rm=TRUE)
                     irrigation_areas_unlist[i] = max(as.numeric(unlist(irrigation_areas_train[i,])),na.rm=TRUE)

                #
                #
                #     #test data
                #     #precipitation_test_unlist <- unlist(precipitation_test)
                #
                #     temperature_test_unlist <- unlist(temperature_test)
                #
                #
                      }
                    }


                    #print(as.numeric(temperature_train_unlist))
                    #print(class(temperature_train_unlist))
                    #print(mode(temperature_train_unlist))

                  temperature_train_plot = vector(,37);
                  precipitation_train_plot = vector(,37);
                  spei_train_plot = vector(,37);
                  gdp_maximum_plot = vector(,37);
                  gdp_minimum_plot =  vector(,37);

                  credit_farmer = vector(,37);
                  irrigation_areas =  vector(,37);




                  temperature_train_plot[1] =  NA   #Andaman & Nicobar

                  temperature_train_plot[2] =  temperature_train_unlist[5]  #Chandigarh

                  temperature_train_plot[3] =  temperature_train_unlist[7]  #Daman and Diu and Dadra and Nagar Haveli

                  temperature_train_plot[4] =  temperature_train_unlist[33]  #Delhi

                  temperature_train_plot[5] = temperature_train_unlist[10]   #Haryana

                  temperature_train_plot[6] =   temperature_train_unlist[13] #Jharkhand

                  temperature_train_plot[7] =   temperature_train_unlist[14] #Karnataka

                  temperature_train_plot[8] =  temperature_train_unlist[15]  #Kerala

                  temperature_train_plot[9] =  temperature_train_unlist[16]  #Lakshadweep

                  temperature_train_plot[10] = temperature_train_unlist[17]  #Madhya Pradesh

                  temperature_train_plot[11] = temperature_train_unlist[18]  #Maharashtra

                  temperature_train_plot[12] = temperature_train_unlist[23]  #Odisha

                  temperature_train_plot[13] = temperature_train_unlist[34]  #Puducherry

                  temperature_train_plot[14] = temperature_train_unlist[27]  #Tamilnadu

                  temperature_train_plot[15] = temperature_train_unlist[6]  #Chhattishgarh

                  temperature_train_plot[16] = temperature_train_unlist[28]  #Telengana

                  temperature_train_plot[17] = temperature_train_unlist[1]  #Andhra Pradesh

                  temperature_train_plot[18] = temperature_train_unlist[34]  #Puducherry

                  temperature_train_plot[19] = temperature_train_unlist[8]  #Goa

                  temperature_train_plot[20] = temperature_train_unlist[11]  #Himachal Pradesh

                  temperature_train_plot[21] = temperature_train_unlist[24]  #Punjab

                  temperature_train_plot[22] = temperature_train_unlist[25]  #Rajasthan

                  temperature_train_plot[23] = temperature_train_unlist[9]  #Gujarat

                  temperature_train_plot[24] = temperature_train_unlist[31]  #Uttarakhand

                  temperature_train_plot[25] =  temperature_train_unlist[30] #Uttar Pradesh

                  temperature_train_plot[26] =  temperature_train_unlist[26] #Sikkim

                  temperature_train_plot[27] =  temperature_train_unlist[3] #Assam

                  temperature_train_plot[28] =  temperature_train_unlist[2] #Arunachal Pradesh

                  temperature_train_plot[29] =  temperature_train_unlist[22] #Nagaland

                  temperature_train_plot[30] =  temperature_train_unlist[19] #Manipur

                  temperature_train_plot[31] =  temperature_train_unlist[21] #Mizoram

                  temperature_train_plot[32] = temperature_train_unlist[29]  #Tripura

                  temperature_train_plot[33] = temperature_train_unlist[20]  #Meghalaya

                  temperature_train_plot[34] = temperature_train_unlist[32]  #West Bengal

                  temperature_train_plot[35] = temperature_train_unlist[4]  #Bihar

                  temperature_train_plot[36] = NA #temperature_train[]  #Ladakh

                  temperature_train_plot[37] =  temperature_train_unlist[12] #Jammu and Kashmir


                  temperature_train_plot[temperature_train_plot == 0] <- NA



                  precipitation_train_plot[1] =  NA   #Andaman & Nicobar

                  precipitation_train_plot[2] =  precipitation_train_unlist[5]  #Chandigarh

                  precipitation_train_plot[3] =  precipitation_train_unlist[7]  #Daman and Diu and Dadra and Nagar Haveli

                  precipitation_train_plot[4] =  precipitation_train_unlist[33]  #Delhi

                  precipitation_train_plot[5] = precipitation_train_unlist[10]   #Haryana

                  precipitation_train_plot[6] =   precipitation_train_unlist[13] #Jharkhand

                  precipitation_train_plot[7] =   precipitation_train_unlist[14] #Karnataka

                  precipitation_train_plot[8] =  precipitation_train_unlist[15]  #Kerala

                  precipitation_train_plot[9] =  precipitation_train_unlist[16]  #Lakshadweep

                  precipitation_train_plot[10] = precipitation_train_unlist[17]  #Madhya Pradesh

                  precipitation_train_plot[11] = precipitation_train_unlist[18]  #Maharashtra

                  precipitation_train_plot[12] = precipitation_train_unlist[23]  #Odisha

                  precipitation_train_plot[13] = precipitation_train_unlist[34]  #Puducherry

                  precipitation_train_plot[14] = precipitation_train_unlist[27]  #Tamilnadu

                  precipitation_train_plot[15] = precipitation_train_unlist[6]  #Chhattishgarh

                  precipitation_train_plot[16] = precipitation_train_unlist[28]  #Telengana

                  precipitation_train_plot[17] = precipitation_train_unlist[1]  #Andhra Pradesh

                  precipitation_train_plot[18] = precipitation_train_unlist[34]  #Puducherry

                  precipitation_train_plot[19] = precipitation_train_unlist[8]  #Goa

                  precipitation_train_plot[20] = precipitation_train_unlist[11]  #Himachal Pradesh

                  precipitation_train_plot[21] = precipitation_train_unlist[24]  #Punjab

                  precipitation_train_plot[22] = precipitation_train_unlist[25]  #Rajasthan

                  precipitation_train_plot[23] = precipitation_train_unlist[9]  #Gujarat

                  precipitation_train_plot[24] = precipitation_train_unlist[31]  #Uttarakhand

                  precipitation_train_plot[25] =  precipitation_train_unlist[30] #Uttar Pradesh

                  precipitation_train_plot[26] =  precipitation_train_unlist[26] #Sikkim

                  precipitation_train_plot[27] =  precipitation_train_unlist[3] #Assam

                  precipitation_train_plot[28] =  precipitation_train_unlist[2] #Arunachal Pradesh

                  precipitation_train_plot[29] =  precipitation_train_unlist[22] #Nagaland

                  precipitation_train_plot[30] =  precipitation_train_unlist[19] #Manipur

                  precipitation_train_plot[31] =  precipitation_train_unlist[21] #Mizoram

                  precipitation_train_plot[32] = precipitation_train_unlist[29]  #Tripura

                  precipitation_train_plot[33] = precipitation_train_unlist[20]  #Meghalaya

                  precipitation_train_plot[34] = precipitation_train_unlist[32]  #West Bengal

                  precipitation_train_plot[35] = precipitation_train_unlist[4]  #Bihar

                  precipitation_train_plot[36] = NA #precipitation_train[]  #Ladakh

                  precipitation_train_plot[37] =  precipitation_train_unlist[12] #Jammu and Kashmir


                  precipitation_train_plot[precipitation_train_plot == 0] <- NA



                  spei_train_plot[1] =  NA   #Andaman & Nicobar

                  spei_train_plot[2] =  spei_train_unlist[5]  #Chandigarh

                  spei_train_plot[3] =  spei_train_unlist[7]  #Daman and Diu and Dadra and Nagar Haveli

                  spei_train_plot[4] =  spei_train_unlist[33]  #Delhi

                  spei_train_plot[5] = spei_train_unlist[10]   #Haryana

                  spei_train_plot[6] =   spei_train_unlist[13] #Jharkhand

                  spei_train_plot[7] =   spei_train_unlist[14] #Karnataka

                  spei_train_plot[8] =  spei_train_unlist[15]  #Kerala

                  spei_train_plot[9] =  spei_train_unlist[16]  #Lakshadweep

                  spei_train_plot[10] = spei_train_unlist[17]  #Madhya Pradesh

                  spei_train_plot[11] = spei_train_unlist[18]  #Maharashtra

                  spei_train_plot[12] = spei_train_unlist[23]  #Odisha

                  spei_train_plot[13] = spei_train_unlist[34]  #Puducherry

                  spei_train_plot[14] = spei_train_unlist[27]  #Tamilnadu

                  spei_train_plot[15] = spei_train_unlist[6]  #Chhattishgarh

                  spei_train_plot[16] = spei_train_unlist[28]  #Telengana

                  spei_train_plot[17] = spei_train_unlist[1]  #Andhra Pradesh

                  spei_train_plot[18] = spei_train_unlist[34]  #Puducherry

                  spei_train_plot[19] = spei_train_unlist[8]  #Goa

                  spei_train_plot[20] = spei_train_unlist[11]  #Himachal Pradesh

                  spei_train_plot[21] = spei_train_unlist[24]  #Punjab

                  spei_train_plot[22] = spei_train_unlist[25]  #Rajasthan

                  spei_train_plot[23] = spei_train_unlist[9]  #Gujarat

                  spei_train_plot[24] = spei_train_unlist[31]  #Uttarakhand

                  spei_train_plot[25] =  spei_train_unlist[30] #Uttar Pradesh

                  spei_train_plot[26] =  spei_train_unlist[26] #Sikkim

                  spei_train_plot[27] =  spei_train_unlist[3] #Assam

                  spei_train_plot[28] =  spei_train_unlist[2] #Arunachal Pradesh

                  spei_train_plot[29] =  spei_train_unlist[22] #Nagaland

                  spei_train_plot[30] =  spei_train_unlist[19] #Manipur

                  spei_train_plot[31] =  spei_train_unlist[21] #Mizoram

                  spei_train_plot[32] = spei_train_unlist[29]  #Tripura

                  spei_train_plot[33] = spei_train_unlist[20]  #Meghalaya

                  spei_train_plot[34] = spei_train_unlist[32]  #West Bengal

                  spei_train_plot[35] = spei_train_unlist[4]  #Bihar

                  spei_train_plot[36] = NA #spei_train[]  #Ladakh

                  spei_train_plot[37] =  spei_train_unlist[12] #Jammu and Kashmir


                  spei_train_plot[spei_train_plot == 0] <- NA




                  gdp_maximum_plot[1] =  NA   #Andaman & Nicobar

                  gdp_maximum_plot[2] =  gdp_maximum_unlist[5]  #Chandigarh

                  gdp_maximum_plot[3] =  gdp_maximum_unlist[7]  #Daman and Diu and Dadra and Nagar Haveli

                  gdp_maximum_plot[4] =  gdp_maximum_unlist[33]  #Delhi

                  gdp_maximum_plot[5] = gdp_maximum_unlist[10]   #Haryana

                  gdp_maximum_plot[6] =   gdp_maximum_unlist[13] #Jharkhand

                  gdp_maximum_plot[7] =   gdp_maximum_unlist[14] #Karnataka

                  gdp_maximum_plot[8] =  gdp_maximum_unlist[15]  #Kerala

                  gdp_maximum_plot[9] =  gdp_maximum_unlist[16]  #Lakshadweep

                  gdp_maximum_plot[10] =gdp_maximum_unlist[17]  #Madhya Pradesh

                  gdp_maximum_plot[11] = gdp_maximum_unlist[18]  #Maharashtra

                  gdp_maximum_plot[12] = gdp_maximum_unlist[23]  #Odisha

                  gdp_maximum_plot[13] = gdp_maximum_unlist[34]  #Puducherry

                  gdp_maximum_plot[14] = gdp_maximum_unlist[27]  #Tamilnadu

                  gdp_maximum_plot[15] = gdp_maximum_unlist[6]  #Chhattishgarh

                  gdp_maximum_plot[16] = gdp_maximum_unlist[28]  #Telengana

                  gdp_maximum_plot[17] = gdp_maximum_unlist[1]  #Andhra Pradesh

                  gdp_maximum_plot[18] = gdp_maximum_unlist[34]  #Puducherry

                  gdp_maximum_plot[19] = gdp_maximum_unlist[8]  #Goa

                  gdp_maximum_plot[20] = gdp_maximum_unlist[11]  #Himachal Pradesh

                  gdp_maximum_plot[21] = gdp_maximum_unlist[24]  #Punjab

                  gdp_maximum_plot[22] = gdp_maximum_unlist[25]  #Rajasthan

                  gdp_maximum_plot[23] = gdp_maximum_unlist[9]  #Gujarat

                  gdp_maximum_plot[24] = gdp_maximum_unlist[31]  #Uttarakhand

                  gdp_maximum_plot[25] =  gdp_maximum_unlist[30] #Uttar Pradesh

                  gdp_maximum_plot[26] =  gdp_maximum_unlist[26] #Sikkim

                  gdp_maximum_plot[27] = gdp_maximum_unlist[3] #Assam

                  gdp_maximum_plot[28] =  gdp_maximum_unlist[2] #Arunachal Pradesh

                  gdp_maximum_plot[29] =  gdp_maximum_unlist[22] #Nagaland

                  gdp_maximum_plot[30] =  gdp_maximum_unlist[19] #Manipur

                  gdp_maximum_plot[31] =  gdp_maximum_unlist[21] #Mizoram

                  gdp_maximum_plot[32] = gdp_maximum_unlist[29]  #Tripura

                  gdp_maximum_plot[33] = gdp_maximum_unlist[20]  #Meghalaya

                  gdp_maximum_plot[34] = gdp_maximum_unlist[32]  #West Bengal

                  gdp_maximum_plot[35] = gdp_maximum_unlist[4]  #Bihar

                  gdp_maximum_plot[36] = NA #spei_train[]  #Ladakh

                  gdp_maximum_plot[37] =  gdp_maximum_unlist[12] #Jammu and Kashmir


                  gdp_maximum_plot[gdp_maximum_plot == 0] <- NA





                  gdp_minimum_plot[1] =  NA   #Andaman & Nicobar

                  gdp_minimum_plot[2] =  gdp_minimum_unlist[5]  #Chandigarh

                  gdp_minimum_plot[3] =  gdp_minimum_unlist[7]  #Daman and Diu and Dadra and Nagar Haveli

                  gdp_minimum_plot[4] =  gdp_minimum_unlist[33]  #Delhi

                  gdp_minimum_plot[5] = gdp_minimum_unlist[10]   #Haryana

                  gdp_minimum_plot[6] =   gdp_minimum_unlist[13] #Jharkhand

                  gdp_minimum_plot[7] =   gdp_minimum_unlist[14] #Karnataka

                  gdp_minimum_plot[8] =  gdp_minimum_unlist[15]  #Kerala

                  gdp_minimum_plot[9] =  gdp_minimum_unlist[16]  #Lakshadweep

                  gdp_minimum_plot[10] =gdp_minimum_unlist[17]  #Madhya Pradesh

                  gdp_minimum_plot[11] = gdp_minimum_unlist[18]  #Maharashtra

                  gdp_minimum_plot[12] = gdp_minimum_unlist[23]  #Odisha

                  gdp_minimum_plot[13] = gdp_minimum_unlist[34]  #Puducherry

                  gdp_minimum_plot[14] = gdp_minimum_unlist[27]  #Tamilnadu

                  gdp_minimum_plot[15] = gdp_minimum_unlist[6]  #Chhattishgarh

                  gdp_minimum_plot[16] = gdp_minimum_unlist[28]  #Telengana

                  gdp_minimum_plot[17] = gdp_minimum_unlist[1]  #Andhra Pradesh

                  gdp_minimum_plot[18] = gdp_minimum_unlist[34]  #Puducherry

                  gdp_minimum_plot[19] = gdp_minimum_unlist[8]  #Goa

                  gdp_minimum_plot[20] = gdp_minimum_unlist[11]  #Himachal Pradesh

                  gdp_minimum_plot[21] = gdp_minimum_unlist[24]  #Punjab

                  gdp_minimum_plot[22] = gdp_minimum_unlist[25]  #Rajasthan

                  gdp_minimum_plot[23] = gdp_minimum_unlist[9]  #Gujarat

                  gdp_minimum_plot[24] = gdp_minimum_unlist[31]  #Uttarakhand

                  gdp_minimum_plot[25] =  gdp_minimum_unlist[30] #Uttar Pradesh

                  gdp_minimum_plot[26] =  gdp_minimum_unlist[26] #Sikkim

                  gdp_minimum_plot[27] = gdp_minimum_unlist[3] #Assam

                  gdp_minimum_plot[28] =  gdp_minimum_unlist[2] #Arunachal Pradesh

                  gdp_minimum_plot[29] =  gdp_minimum_unlist[22] #Nagaland

                  gdp_minimum_plot[30] =  gdp_minimum_unlist[19] #Manipur

                  gdp_minimum_plot[31] =  gdp_minimum_unlist[21] #Mizoram

                  gdp_minimum_plot[32] = gdp_minimum_unlist[29]  #Tripura

                  gdp_minimum_plot[33] = gdp_minimum_unlist[20]  #Meghalaya

                  gdp_minimum_plot[34] = gdp_minimum_unlist[32]  #West Bengal

                  gdp_minimum_plot[35] = gdp_minimum_unlist[4]  #Bihar

                  gdp_minimum_plot[36] = NA #spei_train[]  #Ladakh

                  gdp_minimum_plot[37] =  gdp_minimum_unlist[12] #Jammu and Kashmir


                  gdp_minimum_plot[gdp_minimum_plot == 0] <- NA




                  credit_farmer[1] =  NA   #Andaman & Nicobar

                  credit_farmer[2] =  credit_farmer_unlist[5]  #Chandigarh

                  credit_farmer[3] =  credit_farmer_unlist[7]  #Daman and Diu and Dadra and Nagar Haveli

                  credit_farmer[4] =  credit_farmer_unlist[33]  #Delhi

                  credit_farmer[5] = credit_farmer_unlist[10]   #Haryana

                  credit_farmer[6] =   credit_farmer_unlist[13] #Jharkhand

                  credit_farmer[7] =   credit_farmer_unlist[14] #Karnataka

                  credit_farmer[8] =  credit_farmer_unlist[15]  #Kerala

                  credit_farmer[9] =  credit_farmer_unlist[16]  #Lakshadweep

                  credit_farmer[10] =credit_farmer_unlist[17]  #Madhya Pradesh

                  credit_farmer[11] = credit_farmer_unlist[18]  #Maharashtra

                  credit_farmer[12] = credit_farmer_unlist[23]  #Odisha

                  credit_farmer[13] = credit_farmer_unlist[34]  #Puducherry

                  credit_farmer[14] = credit_farmer_unlist[27]  #Tamilnadu

                  credit_farmer[15] = credit_farmer_unlist[6]  #Chhattishgarh

                  credit_farmer[16] = credit_farmer_unlist[28]  #Telengana

                  credit_farmer[17] = credit_farmer_unlist[1]  #Andhra Pradesh

                  credit_farmer[18] = credit_farmer_unlist[34]  #Puducherry

                  credit_farmer[19] = credit_farmer_unlist[8]  #Goa

                  credit_farmer[20] = credit_farmer_unlist[11]  #Himachal Pradesh

                  credit_farmer[21] = credit_farmer_unlist[24]  #Punjab

                  credit_farmer[22] = credit_farmer_unlist[25]  #Rajasthan

                  credit_farmer[23] = credit_farmer_unlist[9]  #Gujarat

                  credit_farmer[24] = credit_farmer_unlist[31]  #Uttarakhand

                  credit_farmer[25] =  credit_farmer_unlist[30] #Uttar Pradesh

                  credit_farmer[26] =  credit_farmer_unlist[26] #Sikkim

                  credit_farmer[27] = credit_farmer_unlist[3] #Assam

                  credit_farmer[28] =  credit_farmer_unlist[2] #Arunachal Pradesh

                  credit_farmer[29] =  credit_farmer_unlist[22] #Nagaland

                  credit_farmer[30] =  credit_farmer_unlist[19] #Manipur

                  credit_farmer[31] =  credit_farmer_unlist[21] #Mizoram

                  credit_farmer[32] = credit_farmer_unlist[29]  #Tripura

                  credit_farmer[33] = credit_farmer_unlist[20]  #Meghalaya

                  credit_farmer[34] = credit_farmer_unlist[32]  #West Bengal

                  credit_farmer[35] = credit_farmer_unlist[4]  #Bihar

                  credit_farmer[36] = NA #spei_train[]  #Ladakh

                  credit_farmer[37] =  credit_farmer_unlist[12] #Jammu and Kashmir


                  credit_farmer[credit_farmer == 0] <- NA



                  irrigation_areas[1] =  NA   #Andaman & Nicobar

                  irrigation_areas[2] =  irrigation_areas_unlist[5]  #Chandigarh

                  irrigation_areas[3] =  irrigation_areas_unlist[7]  #Daman and Diu and Dadra and Nagar Haveli

                  irrigation_areas[4] =  irrigation_areas_unlist[33]  #Delhi

                  irrigation_areas[5] = irrigation_areas_unlist[10]   #Haryana

                  irrigation_areas[6] =   irrigation_areas_unlist[13] #Jharkhand

                  irrigation_areas[7] =   irrigation_areas_unlist[14] #Karnataka

                  irrigation_areas[8] =  irrigation_areas_unlist[15]  #Kerala

                  irrigation_areas[9] =  irrigation_areas_unlist[16]  #Lakshadweep

                  irrigation_areas[10] =irrigation_areas_unlist[17]  #Madhya Pradesh

                  irrigation_areas[11] = irrigation_areas_unlist[18]  #Maharashtra

                  irrigation_areas[12] = irrigation_areas_unlist[23]  #Odisha

                  irrigation_areas[13] = irrigation_areas_unlist[34]  #Puducherry

                  irrigation_areas[14] = irrigation_areas_unlist[27]  #Tamilnadu

                  irrigation_areas[15] = irrigation_areas_unlist[6]  #Chhattishgarh

                  irrigation_areas[16] = irrigation_areas_unlist[28]  #Telengana

                  irrigation_areas[17] = irrigation_areas_unlist[1]  #Andhra Pradesh

                  irrigation_areas[18] = irrigation_areas_unlist[34]  #Puducherry

                  irrigation_areas[19] = irrigation_areas_unlist[8]  #Goa

                  irrigation_areas[20] = irrigation_areas_unlist[11]  #Himachal Pradesh

                  irrigation_areas[21] = irrigation_areas_unlist[24]  #Punjab

                  irrigation_areas[22] = irrigation_areas_unlist[25]  #Rajasthan

                  irrigation_areas[23] = irrigation_areas_unlist[9]  #Gujarat

                  irrigation_areas[24] = irrigation_areas_unlist[31]  #Uttarakhand

                  irrigation_areas[25] =  irrigation_areas_unlist[30] #Uttar Pradesh

                  irrigation_areas[26] =  irrigation_areas_unlist[26] #Sikkim

                  irrigation_areas[27] = irrigation_areas_unlist[3] #Assam

                  irrigation_areas[28] =  irrigation_areas_unlist[2] #Arunachal Pradesh

                  irrigation_areas[29] =  irrigation_areas_unlist[22] #Nagaland

                  irrigation_areas[30] =  irrigation_areas_unlist[19] #Manipur

                  irrigation_areas[31] =  irrigation_areas_unlist[21] #Mizoram

                  irrigation_areas[32] = irrigation_areas_unlist[29]  #Tripura

                  irrigation_areas[33] = irrigation_areas_unlist[20]  #Meghalaya

                  irrigation_areas[34] = irrigation_areas_unlist[32]  #West Bengal

                  irrigation_areas[35] = irrigation_areas_unlist[4]  #Bihar

                  irrigation_areas[36] = NA #spei_train[]  #Ladakh

                  irrigation_areas[37] =  irrigation_areas_unlist[12] #Jammu and Kashmir


                  irrigation_areas[irrigation_areas == 0] <- NA





                  datamyimage$maximum_precip <<- precipitation_train_plot

                  datamyimage$maximum_temp = temperature_train_plot

                  datamyimage$spei <<- spei_train_plot

                  datamyimage$gdp_minimum <<- gdp_minimum_plot

                  datamyimage$gdp_maximum <<- gdp_maximum_plot


                  datamyimage$credit_farmer <<- credit_farmer

                  datamyimage$irrigation_areas <<- irrigation_areas

                  # print(temperature_train_plot)
                   # stop("Error message")


                   tm <- tm_shape(datamyimage) + tm_polygons("maximum_temp", legend.title = "Maximum Temperature",  title = "Maximum Temperature")
                   sf_use_s2(FALSE)
                   tmap_leaflet(tm,in.shiny = TRUE)
                 }
               })

         output$map5 <- renderLeaflet({
             tm <- tm_shape(datamyimage) + tm_polygons("maximum_precip", legend.title = "Maximum Precipitation", title = "Maximum Precipitation")
              sf_use_s2(FALSE)
              tmap_leaflet(tm,in.shiny = TRUE)


         })

         output$map6 <- renderLeaflet({
           tm <- tm_shape(datamyimage) + tm_polygons("spei", legend.title = "SPEI", title = "SPEI index")
           sf_use_s2(FALSE)
           tmap_leaflet(tm,in.shiny = TRUE)


         })

         output$map7 <- renderLeaflet({
           tm <- tm_shape(datamyimage) + tm_polygons("gdp_minimum", legend.title = "GDP", title = "GDP minimum value")
           sf_use_s2(FALSE)
           tmap_leaflet(tm,in.shiny = TRUE)


         })


         output$map8 <- renderLeaflet({
           tm <- tm_shape(datamyimage) + tm_polygons("gdp_maximum", legend.title = "GDP", title = "GDP maximum vakue")
           sf_use_s2(FALSE)
           tmap_leaflet(tm,in.shiny = TRUE)


         })

         output$map9 <- renderLeaflet({
           tm <- tm_shape(datamyimage) + tm_polygons("credit_farmer", legend.title = "Credits to Farmers (maximum)", title = "Credits to Farmers")
           sf_use_s2(FALSE)
           tmap_leaflet(tm,in.shiny = TRUE)


         })

         output$map10 <- renderLeaflet({
           tm <- tm_shape(datamyimage) + tm_polygons("irrigation_areas", legend.title = "Irrigation areas (maximum)", title = "Irrigation areas")
           sf_use_s2(FALSE)
           tmap_leaflet(tm,in.shiny = TRUE)


         })

  }
  )

  # Calculate direct and indirect effect
  observeEvent(input$calculate,

    # if (!is.null(input$in_file) & !is.null(input$file2) ) {
    #if (!is.null(input$in_file)  ) {
       if ( !is.null(input$in_file) & ( input$dist == 'model1' ) )  {



      outcome_slope1 = vector(,34);
      outcome_intercept = vector(,34);
      outcome_slope2 = vector(,34);


      outcome_slope1= matrix(, nrow = 34, ncol = 2);
      outcome_slope2 = matrix(, nrow = 34, ncol = 2);

      newpredictions = vector(,37);

      realcasesplot = vector(,34);
      realcases = vector(,34);

      modellfit =  matrix(, nrow = 34, ncol = 2);

      gdp_agriculture_intercept = vector(,34)
      gdp_agriculture_slope1  = matrix(, nrow = 34, ncol = 2);
      gdp_agriculture_slope2  = matrix(, nrow = 34, ncol = 2);


      #for (i in 1:34)
      #{
      #if (i != 7  & i != 16 & i != 9 & i != 23 & i != 32)
    #Lack of maximum temperature : Gujarat (9), Lakshadweep (16 - islands in Indian Ocean) , Odisha (23), West Bengal (32)
    # Lack of GDP : Dadra and Nagar Haveli (7 - small teritory )
      # {

    b1 = as.character(1)
    b2 = as.character(35)

    train_data <- sprintf("C%s:M%s",b1,b2)

    test_data <- sprintf("N%s:N%s",b1,b2)

    farm_size <- sprintf("B%s:B%s",b1,b2)

    #Training for 2010-2020 and predicting for 2021
    temperature_train <- read_excel(input$in_file$datapath, sheet = "Maximum Temperature", range = train_data)#S38
    temperature_test<- read_excel(input$in_file$datapath, sheet = "Maximum Temperature", range = test_data)#S38

    #outcome event of interest such as death, mental disease, etc
    outcome_train <- read_excel(input$in_file$datapath, sheet = "Outcome", range = train_data)#G38
    outcome_test <- read_excel(input$in_file$datapath, sheet = "Outcome", range = test_data)#G38


    gdpagriculture_train <- read_excel(input$in_file$datapath, sheet = "GDP Agriculture", range = train_data)#S38
    gdpagriculture_test <- read_excel(input$in_file$datapath, sheet = "GDP Agriculture", range = test_data)#S38


  for (i in 1:34)
   {
      if (i != 7  & i != 16 & i != 9 & i != 23 & i != 32)
        #Lack of maximum temperature : Gujarat (9), Lakshadweep (16 - islands in Indian Ocean) , Odisha (23), West Bengal (32)
        # Lack of GDP : Dadra and Nagar Haveli (7 - small teritory )
    {
    #train data


    temperature_train_unlist <- as.numeric(unlist(temperature_train[i,]))
    outcome_train_unlist <- as.numeric(unlist(outcome_train[i,]))
    #precipitation_train_unlist <- unlist(precipitation_train)
    gdpagriculture_train_unlist <- as.numeric(unlist(gdpagriculture_train[i,]))

    #farmer_population_unlist <- unlist(farmer_population)

    #test data
    #precipitation_test_unlist <- unlist(precipitation_test)
    #gdpagriculture_test_unlist <- unlist(gdpagriculture_test)
    #temperature_test_unlist <- unlist(temperature_test)
    #outcome_test_unlist <- unlist(outcome_test)



    #outcome event of interest such as death, mental disease, etc
    outcome <- glm(formula = outcome_train_unlist ~ temperature_train_unlist + gdpagriculture_train_unlist,family = "gaussian")
    #outcome <- glm(formula = outcome_train_unlist ~ temperature_train_unlist + gdpagriculture_train_unlist,family = "poisson")

    #GDP from agriculture
    #gdp_agriculture <- glm(formula = gdpagriculture_train_unlist ~ temperature_train_unlist,family = "poisson")
    gdp_agriculture <- glm(formula = gdpagriculture_train_unlist ~ temperature_train_unlist,family = "gaussian")


    outcome_intercept[i] = outcome$coefficients[1]
    outcome_slope1[i,] = outcome$coefficients[2] #temperature
    outcome_slope2[i,] = outcome$coefficients[3] #gdp_agriculture


    gdp_agriculture_intercept[i] = gdp_agriculture$coefficients[1]
    gdp_agriculture_slope1[i,] = gdp_agriculture$coefficients[2]  #temperature
    gdp_agriculture_slope2[i,] = gdp_agriculture$coefficients[3]  #



  }
}



      # Put Indian states names in order to be plotted/listed

      Directeffect  = outcome_slope1[,1]   #temperature

      Indirecteffect = outcome_slope2[,1]*gdp_agriculture_slope1[,1]     #gdp*temperature



      aDirecteffect_plot = vector(,37)
      # print(Directeffect)

      aDirecteffect_plot[1] =  0   #Andaman & Nicobar



      aDirecteffect_plot[2] =  Directeffect[5]  #Chandigarh

      aDirecteffect_plot[3] =  Directeffect[7]  #Daman and Diu and Dadra and Nagar Haveli

      aDirecteffect_plot[4] =  Directeffect[33]  #Delhi

     aDirecteffect_plot[5] =  Directeffect[10]   #Haryana

      aDirecteffect_plot[6] =   Directeffect[13] #Jharkhand


      aDirecteffect_plot[7] =   Directeffect[14] #Karnataka

      aDirecteffect_plot[8] =  Directeffect[15]  #Kerala

      aDirecteffect_plot[9] =  Directeffect[16]  #Lakshadweep

      aDirecteffect_plot[10] = Directeffect[17]  #Madhya Pradesh

      aDirecteffect_plot[11] = Directeffect[18]  #Maharashtra

      aDirecteffect_plot[12] = Directeffect[23]  #Odisha

      aDirecteffect_plot[13] = Directeffect[34]  #Puducherry

      aDirecteffect_plot[14] = Directeffect[27]  #Tamilnadu

       aDirecteffect_plot[15] = Directeffect[6]  #Chhattishgarh

      aDirecteffect_plot[16] = Directeffect[28]  #Telengana

      aDirecteffect_plot[17] = Directeffect[1]  #Andhra Pradesh

      aDirecteffect_plot[18] = Directeffect[34]  #Puducherry

      aDirecteffect_plot[19] = Directeffect[8]  #Goa

      aDirecteffect_plot[20] = Directeffect[11]  #Himachal Pradesh

      aDirecteffect_plot[21] = Directeffect[24]  #Punjab

      aDirecteffect_plot[22] = Directeffect[25]  #Rajasthan

      aDirecteffect_plot[23] = Directeffect[9]  #Gujarat

      aDirecteffect_plot[24] = Directeffect[31]  #Uttarakhand

      aDirecteffect_plot[25] =  Directeffect[30] #Uttar Pradesh

      aDirecteffect_plot[26] =  Directeffect[26] #Sikkim

      aDirecteffect_plot[27] =  Directeffect[3] #Assam

      aDirecteffect_plot[28] =  Directeffect[2] #Arunachal Pradesh

      aDirecteffect_plot[29] =  Directeffect[22] #Nagaland

      aDirecteffect_plot[30] =  Directeffect[19] #Manipur

      aDirecteffect_plot[31] =  Directeffect[21] #Mizoram

      aDirecteffect_plot[32] = Directeffect[29]  #Tripura

      aDirecteffect_plot[33] = Directeffect[20]  #Meghalaya

      aDirecteffect_plot[34] = Directeffect[32]  #West Bengal

      aDirecteffect_plot[35] = Directeffect[4]  #Bihar

      aDirecteffect_plot[36] = 0 #Directeffect[]  #Ladakh

      aDirecteffect_plot[37] =  Directeffect[12] #Jammu and Kashmir

      #Indirect effect


      Indirecteffect_plot = vector(,37);

      Indirecteffect_plot[1] =  0   #Andaman & Nicobar

      Indirecteffect_plot[2] =  Indirecteffect[5]  #Chandigarh

      Indirecteffect_plot[3] =  Indirecteffect[7]  #Daman and Diu and Dadra and Nagar Haveli

      Indirecteffect_plot[4] =  Indirecteffect[33]  #Delhi

      Indirecteffect_plot[5] = Indirecteffect[10]   #Haryana

      Indirecteffect_plot[6] =   Indirecteffect[13] #Jharkhand

      Indirecteffect_plot[7] =   Indirecteffect[14] #Karnataka

      Indirecteffect_plot[8] =  Indirecteffect[15]  #Kerala

      Indirecteffect_plot[9] =  Indirecteffect[16]  #Lakshadweep

      Indirecteffect_plot[10] = Indirecteffect[17]  #Madhya Pradesh

      Indirecteffect_plot[11] = Indirecteffect[18]  #Maharashtra

      Indirecteffect_plot[12] = Indirecteffect[23]  #Odisha

      Indirecteffect_plot[13] = Indirecteffect[34]  #Puducherry

      Indirecteffect_plot[14] = Indirecteffect[27]  #Tamilnadu

      Indirecteffect_plot[15] = Indirecteffect[6]  #Chhattishgarh

      Indirecteffect_plot[16] = Indirecteffect[28]  #Telengana

      Indirecteffect_plot[17] = Indirecteffect[1]  #Andhra Pradesh

      Indirecteffect_plot[18] = Indirecteffect[34]  #Puducherry

      Indirecteffect_plot[19] = Indirecteffect[8]  #Goa

      Indirecteffect_plot[20] = Indirecteffect[11]  #Himachal Pradesh

      Indirecteffect_plot[21] = Indirecteffect[24]  #Punjab

      Indirecteffect_plot[22] = Indirecteffect[25]  #Rajasthan

      Indirecteffect_plot[23] = Indirecteffect[9]  #Gujarat

      Indirecteffect_plot[24] = Indirecteffect[31]  #Uttarakhand

      Indirecteffect_plot[25] =  Indirecteffect[30] #Uttar Pradesh

      Indirecteffect_plot[26] =  Indirecteffect[26] #Sikkim

      Indirecteffect_plot[27] =  Indirecteffect[3] #Assam

      Indirecteffect_plot[28] =  Indirecteffect[2] #Arunachal Pradesh

      Indirecteffect_plot[29] =  Indirecteffect[22] #Nagaland

      Indirecteffect_plot[30] =  Indirecteffect[19] #Manipur

      Indirecteffect_plot[31] =  Indirecteffect[21] #Mizoram

      Indirecteffect_plot[32] = Indirecteffect[29]  #Tripura

      Indirecteffect_plot[33] = Indirecteffect[20]  #Meghalaya

      Indirecteffect_plot[34] = Indirecteffect[32]  #West Bengal

      Indirecteffect_plot[35] = Indirecteffect[4]  #Bihar

      Indirecteffect_plot[36] = 0 #Indirecteffectct[]  #Ladakh

      Indirecteffect_plot[37] =  Indirecteffect[12] #Jammu and Kashmir


      #data <- st_sf(data)
      datamyimage$log_dd = aDirecteffect_plot

      log_dd = aDirecteffect_plot

      prob_dd_gt_2 = aDirecteffect_plot

      datamyimage$indirect = Indirecteffect_plot

      indirectpl = Indirecteffect_plot


      Numericalresults <-cbind(aDirecteffect_plot,Indirecteffect_plot)
      colnames(Numericalresults) <- c("Direct Effects on Health Outcome","Indirect Effects on Health Outcome")

      rownames(Numericalresults)  <- c( "Andaman & Nicobar",
                                        "Chandigarh",
                                        "Daman and Diu and Dadra and Nagar Haveli",
                                        "Delhi",
                                        "Haryana",
                                        "Jharkhand",
                                        "Karnataka",
                                        "Kerala",
                                        "Lakshadweep",
                                        "Madhya Pradesh",
                                        "Maharashtra",
                                        "Odisha",
                                        "Puducherry",
                                        "Tamilnadu",
                                        "Chhattishgarh",
                                        "Telengana",
                                        "Andhra Pradesh",
                                        "Puducherry",
                                        "Goa",
                                        "Himachal Pradesh",
                                        "Punjab",
                                        "Rajasthan",
                                        "Gujarat",
                                        "Uttarakhand",
                                        "Uttar Pradesh",
                                        "Sikkim",
                                        "Assam",
                                        "Arunachal Pradesh",
                                        "Nagaland",
                                        "Manipur",
                                        "Mizoram",
                                        "Tripura",
                                        "Meghalaya",
                                        "West Bengal",
                                        "Bihar",
                                        "Ladakh",
                                        "Jammu and Kashmir")

      output$table1 <- renderTable(

        #colnames(Directeffect_plot) <- c("Direct Effects"),
        Numericalresults,
        #Directeffect_plot,
        bordered = TRUE,
        align = 'c',
        digits = 10,
        rownames = TRUE
        #colnames = TRUE

      )



      output$map1 <- renderLeaflet({




        if (!is.null(input$inputdata))
        {
          # stop("Error message")
          tm <- tm_shape(datamyimage) + tm_polygons("indirect", legend.title = "Indirect Effect", title = "Indirect Effect")
          sf_use_s2(FALSE)
          tmap_leaflet(tm,in.shiny = TRUE)
        }
      })




      # Render Map 2:
      output$map2 <- renderLeaflet({


        if (!is.null(input$inputdata))
        {
          sf_use_s2(FALSE)

          tm <- tm_shape(datamyimage) + tm_polygons("log_dd", title = "Direct effect", legend.title = "Direct effect")

          tmap_leaflet(tm,in.shiny = TRUE)
        }
      })

      output$map3 <- renderLeaflet({

        if (!is.null(input$inputdata))
        {
          datamyimage$exceed <- prob_dd_gt_2
          sf_use_s2(FALSE)

          tm <- tm_shape(datamyimage) + tm_polygons("exceed", title = "Prob that \nDirect Effect > 0", legend.title = "Prob that Direct Effect exceeds 0") +
             tm_fill(col = 'State_Name', alpha = 0.5, palette('Okabe-Ito'), legend.show = F) + tm_text('State_Name', col = 'steelblue', legend.size.show = F, legend.col.show = F)


          tmap_leaflet(tm,in.shiny = TRUE)
        }
      })

    }
    else if ( !is.null(input$in_file) & ( input$dist == 'model2' ) )  {


      outcome_slope1 = vector(,34);
      outcome_intercept = vector(,34);
      outcome_slope2 = vector(,34);


      outcome_slope1= matrix(, nrow = 34, ncol = 2);
      outcome_slope2 = matrix(, nrow = 34, ncol = 2);

      newpredictions = vector(,37);

      realcasesplot = vector(,34);
      realcases = vector(,34);

      modellfit =  matrix(, nrow = 34, ncol = 2);

      gdp_agriculture_intercept = vector(,34)
      gdp_agriculture_slope1  = matrix(, nrow = 34, ncol = 2);
      gdp_agriculture_slope2  = matrix(, nrow = 34, ncol = 2);



      #for (i in 1:34)
      #{
      #  if (i != 7  & i != 16 & i != 9 & i != 23 & i != 32)
          #Lack of maximum temperature : Gujarat (9), Lakshadweep (16 - islands in Indian Ocean) , Odisha (23), West Bengal (32)
          # Lack of GDP : Dadra and Nagar Haveli (7 - small teritory )
      # {
          #print(i)
          b1 = as.character(1)
          b2 = as.character(35)

          train_data <- sprintf("C%s:M%s",b1,b2)

          test_data <- sprintf("N%s:N%s",b1,b2)

          farm_size <- sprintf("B%s:B%s",b1,b2)

          #Training for 2010-2020 and predicting for 2021
          temperature_train <- read_excel(input$in_file$datapath, sheet = "Maximum Temperature", range = train_data)#S38
          #temperature_test<- read_excel(input$in_file$datapath, sheet = "Maximum Temperature", range = test_data)#S38

          #outcome event of interest such as death, mental disease, etc
          outcome_train <- read_excel(input$in_file$datapath, sheet = "Outcome", range = train_data)#G38
          #outcome_test <- read_excel(input$in_file$datapath, sheet = "Outcome", range = test_data)#G38


          gdpagriculture_train <- read_excel(input$in_file$datapath, sheet = "GDP Agriculture", range = train_data)#S38
          #gdpagriculture_test <- read_excel(input$in_file$datapath, sheet = "GDP Agriculture", range = test_data)#S38


          precipitation_train <- read_excel(input$in_file$datapath, sheet = "Maximum Precipitation", range = train_data)#S38
          #precipitation_test <- read_excel(input$in_file$datapath, sheet = "Maximum Precipitation", range = test_data)#S38


          irrigation_train <- read_excel(input$in_file$datapath, sheet = "Irrigation area", range = train_data)#S38
          #irrigation_test <- read_excel(input$in_file$datapath, sheet = "Irrigation area", range = test_data)#S38

          credit_train <- read_excel(input$in_file$datapath, sheet = "Credit to farmers", range = train_data)#S38
          #credit_test <- read_excel(input$in_file$datapath, sheet = "Credit to farmers", range = test_data)#S38


          for (i in 1:34)
          {
            if (i != 7  & i != 16 & i != 9 & i != 23 & i != 32)
          #Lack of maximum temperature : Gujarat (9), Lakshadweep (16 - islands in Indian Ocean) , Odisha (23), West Bengal (32)
          # Lack of GDP : Dadra and Nagar Haveli (7 - small teritory )
           {
          #train data
          temperature_train_unlist <- as.numeric(unlist(temperature_train[i,]))
          outcome_train_unlist <- as.numeric(unlist(outcome_train[i,]))
          precipitation_train_unlist <- as.numeric(unlist(precipitation_train[i,]))
          gdpagriculture_train_unlist <- as.numeric(unlist(gdpagriculture_train[i,]))


          irrigation_train_unlist <- as.numeric(unlist(irrigation_train[i,]))
          #irrigation_test_unlist <- unlist(irrigation_test)

          credit_train_unlist <- as.numeric(unlist(credit_train[i,]))
          #credit_test_unlist <- unlist(credit_test)

          #farmer_population_unlist <- unlist(farmer_population)

          #test data
          #precipitation_test_unlist <- unlist(precipitation_test)
          #gdpagriculture_test_unlist <- unlist(gdpagriculture_test)
          #temperature_test_unlist <- unlist(temperature_test)
          #outcome_test_unlist <- unlist(outcome_test)


          outcome <- lm(formula = outcome_train_unlist ~ temperature_train_unlist + gdpagriculture_train_unlist+ credit_train_unlist)


          #GDP from agriculture
          #gdp_agriculture <- glm(formula = gdpagriculture_train_unlist ~ temperature_train_unlist + credit_to_farmer_train_unlist + temperature_train_unlist:Irrigation_area_train_unlist,family = "poisson")
          gdp_agriculture <- lm(formula = gdpagriculture_train_unlist ~ temperature_train_unlist + credit_train_unlist + temperature_train_unlist:irrigation_train_unlist)

          #print(irrigation_train_unlist)
          #outcome event of interest such as death, mental disease, etc
          #outcome <- glm(formula = outcome_train_unlist ~ temperature_train_unlist + gdpagriculture_train_unlist,family = "poisson")
          #outcome <- glm(formula = outcome_train_unlist ~ temperature_train_unlist + gdpagriculture_train_unlist,family = "gaussian")

          #GDP from agriculture
          #gdp_agriculture <- glm(formula = gdpagriculture_train_unlist ~ temperature_train_unlist,family = "poisson")
          #gdp_agriculture <- glm(formula = gdpagriculture_train_unlist ~ temperature_train_unlist,family = "gaussian")




           outcome_intercept[i] = outcome$coefficients[1]
           outcome_slope1[i,] = outcome$coefficients[2] #temperature
           outcome_slope2[i,] = outcome$coefficients[3] #gdp_agriculture


           gdp_agriculture_intercept[i] = gdp_agriculture$coefficients[1]
           gdp_agriculture_slope1[i,] = gdp_agriculture$coefficients[2]  #temperature
           gdp_agriculture_slope2[i,] = gdp_agriculture$coefficients[3]  #




        }
      }



      # Put Indian states names in order to be plotted/listed

      Directeffect  = outcome_slope1[,1]   #temperature

      Indirecteffect = outcome_slope2[,1]*gdp_agriculture_slope1[,1]     #gdp*temperature



      Directeffect_plot = vector(,37);

      Directeffect_plot[1] =  0   #Andaman & Nicobar

      Directeffect_plot[2] =  Directeffect[5]  #Chandigarh

      Directeffect_plot[3] =  Directeffect[7]  #Daman and Diu and Dadra and Nagar Haveli

      Directeffect_plot[4] =  Directeffect[33]  #Delhi

      Directeffect_plot[5] = Directeffect[10]   #Haryana

      Directeffect_plot[6] =   Directeffect[13] #Jharkhand

      Directeffect_plot[7] =   Directeffect[14] #Karnataka

      Directeffect_plot[8] =  Directeffect[15]  #Kerala

      Directeffect_plot[9] =  Directeffect[16]  #Lakshadweep

      Directeffect_plot[10] = Directeffect[17]  #Madhya Pradesh

      Directeffect_plot[11] = Directeffect[18]  #Maharashtra

      Directeffect_plot[12] = Directeffect[23]  #Odisha

      Directeffect_plot[13] = Directeffect[34]  #Puducherry

      Directeffect_plot[14] = Directeffect[27]  #Tamilnadu

      Directeffect_plot[15] = Directeffect[6]  #Chhattishgarh

      Directeffect_plot[16] = Directeffect[28]  #Telengana

      Directeffect_plot[17] = Directeffect[1]  #Andhra Pradesh

      Directeffect_plot[18] = Directeffect[34]  #Puducherry

      Directeffect_plot[19] = Directeffect[8]  #Goa

      Directeffect_plot[20] = Directeffect[11]  #Himachal Pradesh

      Directeffect_plot[21] = Directeffect[24]  #Punjab

      Directeffect_plot[22] = Directeffect[25]  #Rajasthan

      Directeffect_plot[23] = Directeffect[9]  #Gujarat

      Directeffect_plot[24] = Directeffect[31]  #Uttarakhand

      Directeffect_plot[25] =  Directeffect[30] #Uttar Pradesh

      Directeffect_plot[26] =  Directeffect[26] #Sikkim

      Directeffect_plot[27] =  Directeffect[3] #Assam

      Directeffect_plot[28] =  Directeffect[2] #Arunachal Pradesh

      Directeffect_plot[29] =  Directeffect[22] #Nagaland

      Directeffect_plot[30] =  Directeffect[19] #Manipur

      Directeffect_plot[31] =  Directeffect[21] #Mizoram

      Directeffect_plot[32] = Directeffect[29]  #Tripura

      Directeffect_plot[33] = Directeffect[20]  #Meghalaya

      Directeffect_plot[34] = Directeffect[32]  #West Bengal

      Directeffect_plot[35] = Directeffect[4]  #Bihar

      Directeffect_plot[36] = 0 #Directeffect[]  #Ladakh

      Directeffect_plot[37] =  Directeffect[12] #Jammu and Kashmir

      #Indirect effect


      Indirecteffect_plot = vector(,37);

      Indirecteffect_plot[1] =  0   #Andaman & Nicobar

      Indirecteffect_plot[2] =  Indirecteffect[5]  #Chandigarh

      Indirecteffect_plot[3] =  Indirecteffect[7]  #Daman and Diu and Dadra and Nagar Haveli

      Indirecteffect_plot[4] =  Indirecteffect[33]  #Delhi

      Indirecteffect_plot[5] = Indirecteffect[10]   #Haryana

      Indirecteffect_plot[6] =   Indirecteffect[13] #Jharkhand

      Indirecteffect_plot[7] =   Indirecteffect[14] #Karnataka

      Indirecteffect_plot[8] =  Indirecteffect[15]  #Kerala

      Indirecteffect_plot[9] =  Indirecteffect[16]  #Lakshadweep

      Indirecteffect_plot[10] = Indirecteffect[17]  #Madhya Pradesh

      Indirecteffect_plot[11] = Indirecteffect[18]  #Maharashtra

      Indirecteffect_plot[12] = Indirecteffect[23]  #Odisha

      Indirecteffect_plot[13] = Indirecteffect[34]  #Puducherry

      Indirecteffect_plot[14] = Indirecteffect[27]  #Tamilnadu

      Indirecteffect_plot[15] = Indirecteffect[6]  #Chhattishgarh

      Indirecteffect_plot[16] = Indirecteffect[28]  #Telengana

      Indirecteffect_plot[17] = Indirecteffect[1]  #Andhra Pradesh

      Indirecteffect_plot[18] = Indirecteffect[34]  #Puducherry

      Indirecteffect_plot[19] = Indirecteffect[8]  #Goa

      Indirecteffect_plot[20] = Indirecteffect[11]  #Himachal Pradesh

      Indirecteffect_plot[21] = Indirecteffect[24]  #Punjab

      Indirecteffect_plot[22] = Indirecteffect[25]  #Rajasthan

      Indirecteffect_plot[23] = Indirecteffect[9]  #Gujarat

      Indirecteffect_plot[24] = Indirecteffect[31]  #Uttarakhand

      Indirecteffect_plot[25] =  Indirecteffect[30] #Uttar Pradesh

      Indirecteffect_plot[26] =  Indirecteffect[26] #Sikkim

      Indirecteffect_plot[27] =  Indirecteffect[3] #Assam

      Indirecteffect_plot[28] =  Indirecteffect[2] #Arunachal Pradesh

      Indirecteffect_plot[29] =  Indirecteffect[22] #Nagaland

      Indirecteffect_plot[30] =  Indirecteffect[19] #Manipur

      Indirecteffect_plot[31] =  Indirecteffect[21] #Mizoram

      Indirecteffect_plot[32] = Indirecteffect[29]  #Tripura

      Indirecteffect_plot[33] = Indirecteffect[20]  #Meghalaya

      Indirecteffect_plot[34] = Indirecteffect[32]  #West Bengal

      Indirecteffect_plot[35] = Indirecteffect[4]  #Bihar

      Indirecteffect_plot[36] = 0 #Indirecteffectct[]  #Ladakh

      Indirecteffect_plot[37] =  Indirecteffect[12] #Jammu and Kashmir

      #data <- st_sf(data)
      datamyimage$log_dd = Directeffect_plot

      log_dd = Directeffect_plot

      prob_dd_gt_2 = Directeffect_plot

      datamyimage$indirect = Indirecteffect_plot

      indirectpl = Indirecteffect_plot


      Numericalresults <-cbind(Directeffect_plot,Indirecteffect_plot)
      colnames(Numericalresults) <- c("Direct Effects on Health Outcome","Indirect Effects on Health Outcome")

      rownames(Numericalresults)  <- c( "Andaman & Nicobar",
                                        "Chandigarh",
                                        "Daman and Diu and Dadra and Nagar Haveli",
                                        "Delhi",
                                        "Haryana",
                                        "Jharkhand",
                                        "Karnataka",
                                        "Kerala",
                                        "Lakshadweep",
                                        "Madhya Pradesh",
                                        "Maharashtra",
                                        "Odisha",
                                        "Puducherry",
                                        "Tamilnadu",
                                        "Chhattishgarh",
                                        "Telengana",
                                        "Andhra Pradesh",
                                        "Puducherry",
                                        "Goa",
                                        "Himachal Pradesh",
                                        "Punjab",
                                        "Rajasthan",
                                        "Gujarat",
                                        "Uttarakhand",
                                        "Uttar Pradesh",
                                        "Sikkim",
                                        "Assam",
                                        "Arunachal Pradesh",
                                        "Nagaland",
                                        "Manipur",
                                        "Mizoram",
                                        "Tripura",
                                        "Meghalaya",
                                        "West Bengal",
                                        "Bihar",
                                        "Ladakh",
                                        "Jammu and Kashmir")

      output$table1 <- renderTable(

        #colnames(Directeffect_plot) <- c("Direct Effects"),
        Numericalresults,
        #Directeffect_plot,
        bordered = TRUE,
        align = 'c',
        digits = 10,
        rownames = TRUE
        #colnames = TRUE

      )



      output$map1 <- renderLeaflet({




        if (!is.null(input$inputdata))
        {
          # stop("Error message")
          tm <- tm_shape(datamyimage) + tm_polygons("indirect", legend.title = "Indirect Effect", title = "Indirect Effect")
          sf_use_s2(FALSE)
          tmap_leaflet(tm,in.shiny = TRUE)
        }
      })




      # Render Map 2
      output$map2 <- renderLeaflet({


        if (!is.null(input$inputdata))
        {
          sf_use_s2(FALSE)

          tm <- tm_shape(datamyimage) + tm_polygons("log_dd", title = "Direct effect", legend.title = "Direct effect")

          tmap_leaflet(tm,in.shiny = TRUE)
        }
      })

      output$map3 <- renderLeaflet({

        if (!is.null(input$inputdata))
        {
          datamyimage$exceed <- prob_dd_gt_2
          sf_use_s2(FALSE)

          tm <- tm_shape(datamyimage) + tm_polygons("exceed", title = "Prob that \nDirect Effect > 0", legend.title = "Prob that Direct Effect exceeds 0") +
            tm_fill(col = 'State_Name', alpha = 0.5, palette('Okabe-Ito'), legend.show = F) + tm_text('State_Name', col = 'steelblue', legend.size.show = F, legend.col.show = F)


          tmap_leaflet(tm,in.shiny = TRUE)
        }
      })



    }

    )






}



# Run the Shiny app
shinyApp(ui, server)

