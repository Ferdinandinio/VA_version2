
# file.edit("VAShiny/dependenciesfinal.R")
# source("VAShiny/dependencies_leaflet_circles.R")

#Space - Choose where to zoom - country selection; regional selection, global view (no selection)
#Indicator
#TempRes
#Textinput - function TBA





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
    
    
    # Sidebar layout with input and output definitions ----
    # sidebarLayout(
        # Sidebar panel for inputs ----
        
    column(width = 3,
           
           # Upper panel for choices ----    
           
    # Map
    conditionalPanel(
        condition = "input.inTabset == 'Map'",
        {fluidRow(style="background-color:#e8e8e8; padding:10px; border-radius:10px",
                        # tags$b("View Map"), br(), br(), 
                        
                        # Add Reset view button?
                        # Add options to limit map in box?
                        
                        # Choose space
                        selectInput(inputId = "Space", label="Select location", choices = c("Global", "Country level"), selected = "Global"), # Make zoom
        
                        #Make second choices pop up based on first choice in Space
                        #Name selection
                        conditionalPanel(condition='input.Space=="Country level"',
                                                                selectInput(inputId="Name",
                                                                            label="Select jurisdiction",
                                                                            choices=c("",poly$Name)) 
                                                            ),
                  
                        br(), br(),
                        
                        # Indicator selection
                        # Simple radio buttons
                        # radioButtons(inputId = "Indicator",  label="Select attribute", choiceNames = CNI, choiceValues = CVI),
                  
                        # Radio buttons with icon including information
                        radioButtons(inputId = "Indicator",  label="Select attribute", choiceValues = CVI,
                                     choiceNames = list(
                                                         tagList(
                                                           tags$span(CNI[1]), #DoS
                                                           tags$span(icon("info-circle"), id = "DoS_info", style = "color: gray;")
                                                         ), 
                                                         tagList(
                                                           tags$span(CNI[2]), #DoO
                                                           tags$span(icon("info-circle"), id = "DoO_info", style = "color: gray;")
                                                         ), 
                                                         tagList(
                                                           tags$span(CNI[3]), #Ratio
                                                           tags$span(icon("info-circle"), id = "Ratio_info", style = "color: gray;")
                                                         ))
                                      ),# 
                        
                        bsPopover(id="DoS_info", title=NULL, content=DoS_info, trigger="hover", placement="right", options=list(container="body")),
                        bsPopover(id="DoO_info", title=NULL, content=DoO_info, trigger="hover", placement="right", options=list(container="body")),
                        bsPopover(id="Ratio_info", title=NULL, content=Ratio_info, trigger="hover", placement="right", options=list(container="body")),
                        
                        br(),
                  
                        # Time selection
                        radioButtons(inputId = "TempRes", label="Select time frame", choiceNames = CNT, choiceValues = CVT), #
                        # textInput(inputId = "Search", label="")
                        
                        
                        
                        )}   
    ),
    
    #Table
    conditionalPanel(
        condition = "input.inTabset == 'Table'",
        {fluidRow(style="background-color:#e8e8e8; padding:10px; border-radius:10px",
                  HTML("<span style='font-size:15px'> <b>Column name abbreviations:</b></span><br><br><br> 
                        <b>Indicator</b> <br> 
                        DoS   = Density of Studies <br> 
                        DoO   = Density of Organizations <br>
                        Ratio = Ratio of Studies and Organizations <br><br>
                       
                        <b>Time frame</b> <br>  
                        Total = All data available <br> 
                        b1990 = Pre-1990 data <br> 
                        b2000 = 1990-1999 data <br> 
                        b2010 = 2000-2009 data <br> 
                        b2020 = 2010-2020 data <br> 
                       
                       
                       
                       ")
                  
                  
                  # tags$b("Column name abbreviations:"), br(), br(), br(),
                  
                  #Name selection
                    # tags$b("Select a country to view data"), br(), br(),
                                   # selectInput(inputId="Name",
                                   #             label="Select jurisdiction",
                                   #             choices=c("", poly$Name))
                  
                  
                  # style = "font-size:10px;text-align:justify;position:fixed;0%;width:14%;",
                  # tags$b("Indicator"), br(), 
                  # "DoS   = Density of Studies", br(), 
                  # "DoO   = Density of Organizations", br(), 
                  # "Ratio = Ratio of Studies and Organizations", br(), br(), 
                  # 
                  # tags$b("Time frame"), br(), 
                  # "Total = All data available", br(), #including pre-1990 to 2020
                  # "b1990 = Pre-1990 data", br(), 
                  # "b2000 = 1990-1999 data", br(),
                  # "b2010 = 2000-2009 data", br(),
                  # "b2020 = 2010-2020 data", br(),
                  
                  
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
    #     )}  
    # ),
    
    
    #Table
    conditionalPanel(
      condition = "input.inTabset == 'Further information'",
      {fluidRow(style="background-color:#e8e8e8; padding:10px; border-radius:10px",
                tags$b("Disclaimer regarding app and dataset information"), 
      
      )}   
      
    ),
    
    
    # Disclaimer at bottom of sidepanel
    conditionalPanel(condition = "input.inTabset != 'Further information'",
          tags$footer(#style = "font-size:8px;text-align:justify";position:fixed;bottom:0%;width:14%;", # 14% if column width is 2
                      style = "font-size:11px;text-align:justify;position:fixed;bottom:0%;width:22%;",
                      HTML("  <span style='font-size:13px'>
                              The data behind this visualization are derived from a corpus of ~79,000 publications, gathered and analyzed for the IPEBS Values Assessment (<a href='https://doi.org/10.5281/zenodo.6522522'>Link</a>). 
                              <br>
                              For more information about the corpus see the IPBES VA Chapter 3. Systematic review on Method Families (<a href='https://doi.org/10.5281/zenodo.4404436'>Link</a>) and IPBES VA Chapter 4. Systematic review on valuation uptake (<a href='https://doi.org/10.5281/zenodo.4391335'>Link</a>).
                              <br>
                              For more information on how the number of organizations and studies were derived, please review the documentation available on our github <a href='https://jkumagai96.github.io/VA_version2/Valuation_atlas.html'>here</a>. 
                              <br><br></span>
                              <i>    
                              The designations employed and the presentation of material on the maps used in the assessment do not imply the expression of any opinion whatsoever on the part of IPBES concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. These maps have been prepared or used for the sole purpose of facilitating the assessment of the broad biogeographical areas represented therein and for purposes of representing scientific data spatially.
                              <br><br>
                              </i>
                           "))) # make it collide with top box

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
            
                        
        ), # sidebar panel end
        
        # fluidRow(renderImage("Data/IPBES_LOGO.jpg") # add ipbes logo here 
        # 
        # ),
        
        # Main panel ----
        column(width = 9,
               
               tabsetPanel(id="inTabset",
                  tabPanel("Map", leafletOutput("map", height="80vh") ),
                  tabPanel("Table", dataTableOutput("table", width = "100%"), style = "height:80vh; overflow-y: scroll;overflow-x: scroll;"),
                  tabPanel("Further information", Further_information)
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

    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
      poly[cchoices()]
    })
    
    
    
    # Map   
    {
    output$map <- renderLeaflet({my_map})
    

    
    # Circlesizes
    circlesizes <- reactive({
      upcsize <- 5 + poly[[cchoices()]]
      upcsize[is.na(upcsize)] <- 0
      upcsize <- 5 + upcsize
      
      log(upcsize)*25000 # circles (something between 15000 and 35000)
      # log(upcsize)*2     # circlemarkers
      
    })

    # Mapzoom size limit
    # mapzoomsl <- reactive({
    #   return(input$map_zoom)
    # }) 
    # mapzoomsl <- observe(#eventExpr = input$map_zoom, 
    #                           {
    #                             as.numeric(
    #                             cat(case_when(input$map_zoom ==2 ~1, input$map_zoom ==3 ~2, input$map_zoom ==4 ~3, input$map_zoom >=5 ~4)))
    #                             })
    #                       
    # print(is.numeric(mapzoomsl))
    
    # Interactive map
    observe({ 
    # Colors
        palette <- colorBin(bins=10, pretty=T,
                            palette = pal[[input$Indicator]],
                            domain = poly[[cchoices()]]
                            )
    # Edit map according to input
        leafletProxy("map", data = filteredData()) %>%
            clearControls() %>% #clearPopups() %>% #clearMarkers() %>% #removeMarker(layerId = poly$Name) %>% #clearsShape(layerId = 10) %>%
            addCircles(lng= poly$Lon, lat= poly$Lat, stroke=T, weight=5, 
                       radius = circlesizes()*case_when(input$map_zoom ==2 ~1.8, input$map_zoom ==3 ~1.2, input$map_zoom ==4 ~1.1, input$map_zoom ==5 ~0.9, input$map_zoom ==6 ~0.8, input$map_zoom ==7 ~0.6), layerId = poly$Name,# clusterOptions = markerClusterOptions(disableClusteringAtZoom=8, maxClusterRadius=2),
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
        # req(input$Name)
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
