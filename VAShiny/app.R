
# file.edit("VAShiny/dependenciesfinal.R")
# source("VAShiny/dependencies.R")

#Space - Choose where to zoom - country selection; regional selection, global view (no selection)
#Indicator
#TempRes
#Textinput - function TBA


library(shiny)
library(leaflet)
library(leafpop)
library(DT)



ui <- fluidPage(
    
    
    # App title ----
    titlePanel(
      fluidRow(
        column(5, PAGE_TITLE), 
        column(7, div(style="float:right;", img(width=200, src="Senckenberg_Logo.jpg"), img(width=100, src="IPBES_LOGO.jpg"))
        
               # 
               # style="text-align: center;", 
               #          div(style="display: inline-block; width: 67%", img(width=200, src="Senckenberg_Logo.jpg")), 
               #          div(style="display: inline-block; width: 33%", img(width=100, src="IPBES_LOGO.jpg")))
      ))
    ),
    
    
    
    
    # titlePanel(
    #   
    #   
    #   
    #   
    #   # fluidRow(
    # 
    #             # title=div('Valuation Atlas Viewer                            ', img(width=200, src="Senckenberg_Logo.jpg"), img(width=100, src="IPBES_LOGO.jpg"))
    #   # title=div('Valuation Atlas Viewer', img(width=200, src="Senckenberg_Logo.jpg", class="leftAlign"), img(width=100, src="IPBES_LOGO.jpg", class="leftAlign"))
    #   windowTitle = PAGE_TITLE,
    #   title=div(PAGE_TITLE, img(width=200, src="Senckenberg_Logo.jpg", class="rightAlign"), img(width=100, src="IPBES_LOGO.jpg", class="rightAlign"))
    #   
    # #   
    #   # fluidRow(
    #   #                     column(8, 'Valuation Atlas Viewer'),
    #   #                     
    #   #                     column(1, img(width=200, src="Senckenberg_Logo.jpg")),
    #   #                     column(1, offset=1, img(width=100, src="IPBES_LOGO.jpg"))
    #   # 
    #   #                     )
    #            ),
    
    # Sidebar layout with input and output definitions ----
    # sidebarLayout(
        # Sidebar panel for inputs ----
        
    column(width = 2,
           
           # Upper panel for choices ----    
           
    # Map
    conditionalPanel(
        condition = "input.inTabset == 'Map'",
        {fluidRow(style="background-color:#e8e8e8; padding:10px; border-radius:10px",
                        # tags$b("View Map"), br(), br(), 
                        
                        # Add Reset view button
                        # Add options to limit map in box
                        
                        # Choose space
                        selectInput(inputId = "Space", label="Select location", choices = c("Global", "Country level"), selected = "Global"), # Make zoom
        
                        #Make second choices pop up based on first choice in Space
                        #Name selection
                        conditionalPanel(condition='input.Space=="Country level"',
                                                                selectInput(inputId="Name",
                                                                            label="Select jurisdiction",
                                                                            choices=c("",poly$Name)) 
                                                            ),
                        
                        
                        # radioButtons(inputId = "Indicator", label="Choose the dataset", choices = c("Density of Studies", "Density of Organizations", "Ratio of Studies and Organizations"), selected = "Density of Studies"),
                        
                        radioButtons(inputId = "Indicator", label="Select Attribute", choiceNames = CNI, choiceValues = CVI),# 
                        
                        
                        
                        
                        radioButtons(inputId = "TempRes", label="Select time frame", choiceNames = CNT, choiceValues = CVT), #
                        # textInput(inputId = "Search", label="")
                        
                        
                        
                        )}   
    ),
    
    #Table
    conditionalPanel(
        condition = "input.inTabset == 'Table'",
        {fluidRow(style="background-color:#e8e8e8; padding:10px; border-radius:10px",
                  tags$b("Column name abbreviations:"), br(), br(), br(),
                  
                  #Name selection
                    # tags$b("Select a country to view data"), br(), br(),
                                   # selectInput(inputId="Name",
                                   #             label="Select jurisdiction",
                                   #             choices=c("", poly$Name))
                  
                  
                  # style = "font-size:10px;text-align:justify;position:fixed;0%;width:14%;",
                  tags$b("Indicator"), br(), 
                  "DoS   = Density of Studies", br(), 
                  "DoO   = Density of Organizations", br(), 
                  "Ratio = Ratio of Studies and Organizations", br(), br(), 
                  
                  tags$b("Time frame"), br(), 
                  "Total = All data available", br(), #including pre-1990 to 2020
                  "b1990 = Pre-1990 data", br(), 
                  "b2000 = 1990-1999 data", br(),
                  "b2010 = 2000-2009 data", br(),
                  "b2020 = 2010-2020 data", br(),
                  
                  
                  
                  
                  
         )}   
        
    ),
    
    
    #Compare
    # conditionalPanel(
    #     condition = "input.inTabset == 'Compare'",
    #     {fluidRow(style="background-color:#e8e8e8; padding:10px; border-radius:10px",
    #               tags$b("View comparison"), br(), br(),
    #               
    #               #Name selection
    #               
    #               selectInput(inputId="Name1",
    #                           label="1st country",
    #                           choices=c("",poly$Name)),
    #               
    #               selectInput(inputId="Name1",
    #                           label="2nd country",
    #                           choices=c("",poly$Name))
    #               
    #               
    #               
    #               
    #     )}   
    #     
    # ),
    
       
    
    tags$footer(style = "font-size:8px;text-align:justify;position:fixed;bottom:0%;width:14%;",
        
                HTML('
                        <i>    
                            The designations employed and the presentation of material 
                            on the maps shown here do not imply the expression of any 
                            opinion whatsoever on the part of the IPBES concerning the 
                            legal status of any country, territory, city or area or of 
                            its authorities, or concerning the delimitation of its frontiers or boundaries. 
                            <br><br>
                        </i>
                     '),
            
                                               
                ),    
    #   <br><br>
    #   <center><img src="IPBES_LOGO.jpg" width="40%"></center>
    #   <br><br>
    
    
    
            # Lower panel for information ----
            # Disclaimer
            # fluidRow(
            #     br(),br(), # add two line breaks
            #     em("The designations employed and the presentation of material on the maps shown here do not imply the expression of any opinion whatsoever on the part of the IPBES concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries."),
            #     br(),br(),br(),br(),
            # 
            # # IPBES logo
            #     # tags$img(src="IPBES_LOGO.jpg", align="center", height="60%", width="60%") # center doesnt work
            #     HTML('<center><img src="IPBES_LOGO.jpg" width="60%"></center>')
            #     
            #     
            #     )      
                     
                        
        ), # sidebar panel end
        
        # fluidRow(renderImage("Data/IPBES_LOGO.jpg") # add ipbes logo here 
        # 
        # ),
        
        # Main panel for map ----
        column(width = 10,
               
               tabsetPanel(id="inTabset",
                  tabPanel("Map", leafletOutput("map", height="80vh") ),
                  tabPanel("Table", dataTableOutput("table", width = "100%"), style = "height:80vh; overflow-y: scroll;overflow-x: scroll;"),
                  # tabPanel("Compare")
        )#end tabset panel
                  )
    # )
)

server <- function(input, output, session) {
    
    # Combined choices of Indicator and TempRes return full Indicator name
    cchoices <- reactive(paste0(input$Indicator, input$TempRes)) 
    
    # Filtered data for popup table
    fpdata <- reactive({ 
      pdata[pdata$Name == input$Name, c(1, 2, grep(input$TempRes, names(poly)))]
    })

    
    # Map   
    {
    output$map <- renderLeaflet({my_map})
    

    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        poly[cchoices()]
    })
    
    
    # Circlesizes
    circlesizes <- reactive({
      upcsize <- 5 + poly[[cchoices()]]
      upcsize[is.na(upcsize)] <- 0
      upcsize <- 5 + upcsize
      log(upcsize)*2
    })

    
    
    observe({
        palette <- colorBin(bins=10, pretty=T,
                            palette = pal[[input$Indicator]],
                            domain = poly[[cchoices()]]
                            )
        
        leafletProxy("map", data = filteredData()) %>%
            clearControls() %>% clearPopups() %>% clearShapes() %>%
            addCircleMarkers(lng= poly$Lon, lat= poly$Lat, stroke=F, radius = circlesizes(), #, clusterOptions = markerClusterOptions(disableClusteringAtZoom=6, maxClusterRadius=20)
                        color = ~palette(poly[[cchoices()]]), label=poly$Name, # data shown on hover
                        popup=popupTable(pdata[, c(1, 2, grep(input$TempRes, names(poly)))], row.numbers = F, feature.id = F), #map(1:length(names(poly)), ~poly[[.x]])
                        fillOpacity = 0.7)  %>%
            addScaleBar(position = "bottomleft", options = scaleBarOptions()) %>%
            addLegend("topright", title=tcAttributes$Names[tcAttributes$Values==cchoices()], pal=palette, values=poly[[cchoices()]], na.label = "Missing", opacity=1)
            labelFormat(suffix=" ")
            # choiceNames[grep(paste0(input$Indicator,"$"), choiceValues)]
    })
    
    
    #Global zoom
    observe({
        req(input$Space=="Global")
            leafletProxy("map") %>%
                clearPopups() %>%  # addMarkers(lng = ctryzoom$Lon, lat = ctryzoom$Lat, popup=popupTable(pdata, row.numbers = F, feature.id = F)) %>% 
                setView(lng = 0, lat = 20, zoom = 2) 
    })
    
    # Name selection zoom
    fpts <- reactive({ #filtered points
        coordpts[poly$Name == input$Name,]
    })


    observe({
        req(input$Name)
        ctryzoom <- fpts()
        leafletProxy("map") %>%
            clearPopups() %>%  # addMarkers(lng = ctryzoom$Lon, lat = ctryzoom$Lat, popup=popupTable(pdata, row.numbers = F, feature.id = F)) %>% 
            setView(lng = ctryzoom$Lon, lat = ctryzoom$Lat, zoom = 6) %>%
            addPopups(lng = ctryzoom$Lon, lat = ctryzoom$Lat, popup=popupTable(fpdata(), row.numbers = F, feature.id = F))
             
    })


    
    #Add polygons
    # observeEvent(input$Indicator, {
    #     leafletProxy("map", filteredData()) %>%
    #         addPolygons(fillColor = colorpal)
    # })
    
    # Add polygons
    # observe({
    #     # pal <- colorpal()
    #     
    #     leafletProxy("map", data = filteredData()) %>%
    #         clearShapes() %>%
    #         addPolygons(fillColor = colorpal)
    # })
    }
    
    
    # input$tableId_rows_selected <- reactive(input$Name)
    # https://stackoverflow.com/questions/21515800/subset-a-data-frame-based-on-user-input-shiny
    
    # Table
    output$table <- renderDT({datatable(polydt, options = list(paging = FALSE))})
    
    
    # if(exists("input$Name")){return(pdata)}else{pdata[poly$Name == input$Name,]}
    
}

shinyApp(ui = ui, server = server)


#helpful links (possibly)
# https://stackoverflow.com/questions/41940403/popup-on-a-shape-using-tmap
# https://community.rstudio.com/t/integrating-tmap-into-shiny/6036
# https://community.rstudio.com/t/how-to-plot-leaflet-map-in-shiny-mainpanel/107079
# https://geocompr.robinlovelace.net/adv-map.html#interactive-maps


# https://community.rstudio.com/t/select-polygon-by-clicking-on-map-changing-item-selected-via-dropdown/89211/2

# https://medium.com/ibm-data-ai/asynchronous-loading-of-leaflet-layer-groups-afc073999e77


# https://stackoverflow.com/questions/30091093/is-it-possible-to-update-polygon-fill-in-leaflet-for-shiny-without-recreating-th
    

# https://github.com/cenuno/shiny/blob/master/Interactive_UI/Dynamic_Legend/server.R#L25

# view selectinput values
# https://stackoverflow.com/questions/22423363/r-shiny-access-input-fields-in-ui


# https://rstudio.github.io/leaflet/shiny.html


#https://community.rstudio.com/t/make-shiny-leaflet-map-less-cumbersome-faster/210/8

# https://datascience.blog.wzb.eu/2021/04/16/interactive-visualization-of-geospatial-data-with-r-shiny/

# dateline issue
# https://github.com/rstudio/leaflet/issues/729
