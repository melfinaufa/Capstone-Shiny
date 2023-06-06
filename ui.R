ui <- dashboardPage(title = "Bencana Alam",
                    
                    # Header
                    dashboardHeader(title = "Bencana Alam",titleWidth = 200,
                    tags$li(actionLink("LinkedIn", 
                                       label = "", 
                                       icon = icon("linkedin"),
                                       onclick = "window.open('https://www.linkedin.com/in/melfinaufa/')"),
                            class = "dropdown")),
                    
                    # Side bar of the Dashboard
                    dashboardSidebar(
                      
                      # Side menu of the Dashboard  
                      sidebarMenu(
                      
                        
                        
                        menuItem(text = "Plot",
                                 icon = icon("chart-line"),
                                 tabName = "page1",
                                 badgeLabel = "Numeric",
                                 badgeColor = "green"),
                        
                        menuItem(text = "Graph",
                                 icon = icon("magnifying-glass-chart"),
                                 tabName = "page3",
                                 badgeLabel = "Proporsi",
                                 badgeColor = "red"),
                        
                        # actionLink("remove", icon = icon("sync-alt"),"Remove detail tabs"),
                        
                        menuItem("Map", tabName = "page4", icon = icon("map-location")), 
                        
                        menuItem("Data Table", tabName = "page2", icon = icon("table-list")),
                        
                        menuItem("Source Code", icon = icon("github"), href = "https://github.com/melfinaufa/Capstone-Shiny"))
                        
                      ),
                      
          
                    
                    # The body of the dashboard
                    
                  dashboardBody(
                    tabItems(
                      # --- PAGE 1 (Dashboard) --- 
                      
                      
                      tabItem(tabName = "page1",
                              h2(tags$b("Report Kejadian Bencana Menurut Bulan dan Jenis Kejadian Bencana di Indonesia Tahun 2023")),
                              br(),
                              
                              
                              tabsetPanel(id = "tabs",
                                          tabPanel(title = "Dashboard",
                                                   value = "page1",
                                          fluidRow(
                                            
                                            box(width = 6,
                                                solidHeader = T,
                                                background = "light-blue",
                                                height = 100,
                                                radioButtons(inputId = "input_bulan",label = h4(tags$b("Bulan")),choiceNames = toupper(unique(disaster_clean$Bulan)), choiceValues = unique(disaster_clean$Bulan), selected = "May", inline = TRUE)),
                                            box(width = 6,
                                                solidHeader = T,
                                                background = "light-blue",
                                                height = 100,
                                                selectInput(inputId = "input_kejadian", label = h4(tags$b("Kejadian Bencana")), choices = levels(disaster_clean$Kejadian),selected = "Banjir",selectize = FALSE))
                                                   ),

                                           fluidRow(valueBoxOutput("sum_meninggal"),
                                                    valueBoxOutput("sum_hilang"),
                                                    valueBoxOutput("sum_terluka"),
                                                    valueBoxOutput("sum_rusak"),
                                                    valueBoxOutput("sum_terendam"),
                                                    valueBoxOutput("sum_fasum")),
                                            
                                           fluidRow(
                                             box(width = 6, height = 480, plotlyOutput("plot_trend", height = 460)),
                                             box(width = 6, height = 480, plotlyOutput("plot_rangking", height = 460)))
                                  ))),
                      
                      tabItem(tabName = "page3",
                              h2(tags$b("Report Proporsi Jumlah Kejadian Bencana Menurut Provinsi dan Bulan")),
                              br(),
                              
                              box(width = 12,
                                  solidHeader = T,
                                  background = "light-blue",
                                  height = 120,
                              
                              selectInput(inputId = "input_prov",
                                           label = h4(tags$b("Provinsi")),
                                           choices = levels(disaster_clean$Provinsi))),
                              
                                
                              fluidRow(
                                
                                box(width = 6, height = 500, plotlyOutput(outputId = "sum_dis", height = 480)),
                                box(width = 6, height = 500, plotlyOutput(outputId = "prop_dis", height = 480)),
                                box(width = 6, height = 500, plotlyOutput(outputId = "sum_kor", height = 480)),
                                box(width = 6, height = 500, plotlyOutput(outputId = "prop_kor", height = 480))
                              ),
       
                              
                              
                      ),
                      
                      
                      
                      # PAGE 4 ---------------------------------------------------------
                      
                      tabItem(tabName = "page4",
                              h2(tags$b("Ilustrasi Sebaran Jumlah Kejadian Bencana di Setiap Provinsi di Indonesia")),
                              br(),
                              
                              
                              
                              fluidRow(
                                
                                box(width = 12, height = 140,
                                    solidHeader = TRUE,
                                    status = "warning",title = "Kejadian Bencana", radioButtons(inputId = "pilih_kejadian", 
                                                                                                label = h4(tags$b("Kejadian Bencana")), 
                                                                                                inline = TRUE,
                                                                                                choiceNames =  c("BANJIR",
                                                                                                                 "GELOMBANG PASANG/ABRASI",
                                                                                                                 "GEMPA BUMI",
                                                                                                                 "KEBAKARAN BUMI",
                                                                                                                 "KEKERINGAN",
                                                                                                                 "LETUSAN GUNUNG",
                                                                                                                 "PUTING BELIUNG",
                                                                                                                 "TANAH LONGSOR") ,
                                                                                                choiceValues = c("Banjir",
                                                                                                                 "Gelombang_Pasang",
                                                                                                                 "Gempa_Bumi",
                                                                                                                 "Kebakaran_Hutan",
                                                                                                                 "Kekeringan",
                                                                                                                 "Letusan_Gunung",
                                                                                                                 "Puting_Beliung",
                                                                                                                 "Tanah_Longsor"
                                                                                                                 ))),
                                
                                
                                box(width = 12, height = 500, leafletOutput(outputId = "map_dis", height = 480))),
                                
                              
                              
                              
                              fluidRow(
                                box(width = 6, height = 140,
                                    solidHeader = TRUE,
                                    status = "warning",
                                    title = "Tanggal Kejadian", dateRangeInput(inputId = "input_kalender",label = h4(tags$b("Kalender")), start = "2023-01-01", end = "2023-05-28", min = "2023-01-01", max = "2023-05-28")),
                                box(width = 6, height = 140,
                                    solidHeader = TRUE,
                                    status = "warning",title = "Pilih Provinsi", selectInput(inputId = "input_provinsi", label = h4(tags$b("Provinsi")), choices = levels(disaster_clean$Provinsi),selectize = FALSE)),
                              
                                box(width = 12, height = 500, plotlyOutput(outputId = "series_dis", height = 480))
                                
                                ), 
                              
                              
                              ),
                            
                      
                      # --- PAGE 3 (Data Table) ---
                      tabItem(tabName = "page2",
                              fluidRow(
                                box(width = 12, dataTableOutput(outputId = "table_data"))  
                              )
                      
                     )
                      )
)
)