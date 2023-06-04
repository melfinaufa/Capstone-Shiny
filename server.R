function(input, output) { 
  
  tab_list <- NULL
  
  
  showNotification("Interative Plotting Project
                   created by Melfin", duration = NULL, type = "message")
  
  
  
  
  # ___________________________________________________________________________________________________________________
  # ____________________________________________________ VALUE BOX ____________________________________________________
  
  
  
  # Jumlah Orang Meninggal
  
  output$sum_meninggal <- renderValueBox({
    mng <- disaster_clean %>% 
      
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Meninggal = sum(Meninggal)) 
    
    valueBox(subtitle = "Jumlah Orang Meninggal", 
               value = sum(mng$Jumlah_Meninggal),
               icon = icon("bed-pulse"),
               color = "red")
  })
  
  
  # Jumlah Orang Hilang
  
  
  output$sum_hilang <- renderValueBox({
    hlg <- disaster_clean %>% 
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Hilang = sum(Hilang)) 
    
  
      valueBox(subtitle = "Jumlah Orang Hilang", 
               value = sum(hlg$Jumlah_Hilang),
               icon = icon("person-circle-exclamation"),
               color = "yellow")
  })
  
  

  # Jumlah Orang Terluka
  
  output$sum_terluka <- renderValueBox({
    trl <- disaster_clean %>% 
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Terluka = sum(Terluka)) 
      
      valueBox(subtitle = "Jumlah Orang Terluka", 
               value = sum(trl$Jumlah_Terluka),
               icon = icon("user-injured"),
               color = "green")
  })
  
  # Jumlah Rumah Rusak
  
  output$sum_rusak <- renderValueBox({
    rrsk <- disaster_clean %>% 
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Rusak = sum(Rumah_Rusak)) 
      
      valueBox(subtitle = "Jumlah Rumah Rusak", 
               value = sum(rrsk$Jumlah_Rusak),
               icon = icon("house-circle-xmark"),
               color = "blue")
  })
  
  
  # valueBoxOutput("sum_terendam")
  
  output$sum_terendam <- renderValueBox({
    rtrn <- disaster_clean %>% 
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Terendam = sum(Rumah_Terendam)) 
      
      
      valueBox(subtitle = "Jumlah Rumah Terendam", 
               value = sum(rtrn$Jumlah_Terendam),
               icon = icon("house-flood-water"),
               color = "teal")
  })
  
  
  # valueBoxOutput("sum_fasum"))
  
  output$sum_fasum <- renderValueBox({
    fsmr <- disaster_clean %>% 
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Fasum = sum(Fasum_Rusak)) 
      
      valueBox(subtitle = "Jumlah Fasum Rusak", 
               value = sum(fsmr$Jumlah_Fasum),
               icon = icon("building-shield"),
               color = "purple")
  })
  
  
  
  # -------------------------- PLOT 1 --------------------------------------
  
  
  # Plot 1: Rangking -> Top Provinsi 
  
  output$plot_rangking <- renderPlotly({
    # Data Prep Plot 1
    plot_agg_1 <- disaster_clean %>%
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Provinsi, Kejadian) %>% 
      summarise(Jumlah_Kejadian = n()) %>% 
      ungroup() %>% 
      arrange(desc(Jumlah_Kejadian)) %>% 
      top_n(10)
    
    plot_agg_1 <-  plot_agg_1 %>% 
      mutate(label=glue("Provinsi : {Provinsi}
      Jumlah Kejadian : {Jumlah_Kejadian} Kali"))
    
    # Plot Rangking Statis
    plot_rangking <- plot_agg_1 %>% 
      top_n(10) %>% 
      ggplot(mapping = aes(x= Jumlah_Kejadian , y= reorder(Provinsi, Jumlah_Kejadian), text = label)) +
      geom_col(aes(fill = Jumlah_Kejadian)) +
      
      scale_x_continuous(breaks = seq(0, {max(plot_agg_1$Jumlah_Kejadian)}, 1)) +
      scale_fill_gradient(high = "#ff0a54" , low = "#ff99ac") +
      
      labs(title = glue("TOP 10 Provinsi berdasarkan Jumlah Kejadian {input$input_kejadian}  Bulan {input$input_bulan}") ,
           x = NULL,
           y = NULL) +
      theme(legend.position = "none") +
      
      theme(plot.title = element_text(face = "bold", size = 10, hjust = 1, vjust = 3),
            plot.title.position = "center",
            axis.ticks.y = element_blank(),
            plot.background = element_rect(fill = "#e2eafc"),
            panel.background = element_rect(fill = "#e2eafc"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            axis.text = element_text(color="black"))
            
    
    # Plot Rangking Interaktif
    ggplotly(plot_rangking, tooltip = "text")
  })
  
  
  
  
  # -------------------------- PLOT 2 --------------------------------------
  
  output$plot_trend <- renderPlotly({
   
    plot_agg_2 <- disaster_clean %>% 
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Tanggal, Kejadian) %>% 
      summarise(Jumlah_Kejadian =n())
  
    plot2 <- plot_agg_2 %>% 
      ggplot(mapping = aes( x= Tanggal, 
                            y = Jumlah_Kejadian)) +
      geom_area(aes(group = Kejadian), fill= "#e0b1cb", color= "#bd4f6c", linetype = "dashed") +
      geom_point(aes(group = Kejadian,
                     text = glue("Tanggal :  {Tanggal}
                                    Jumlah Kejadian : {Jumlah_Kejadian} Kali")), color =  "#f48498") +
     
      
      scale_x_continuous(breaks = seq(1,31,1)) +
      scale_y_continuous(breaks = seq(0,{max(plot_agg_2$Jumlah_Kejadian)},1)) +
      
      labs(title = glue("Trend Jumlah Kejadian Bencana {input$input_kejadian} Bulan {input$input_bulan} di Indonesia"),
           x = "Tanggal",
           y = "Jumlah Kejadian") +
      
      theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.05, vjust = 0.05),
            plot.title.position = "center",
            axis.ticks.y = element_blank(),
            plot.background = element_rect(fill = "#e2eafc"),
            panel.background = element_rect(fill = "#e2eafc"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            axis.text = element_text(color="black"))
    
    
    ggplotly(plot2, tooltip = "text")

  
  })
  
  
  # ___________________________________________________________________________________________________________________
  # _____________________________________________________ PAGE 3 ______________________________________________________
  
  
  
  # --------------------------------------- PLOT 3.1 ------------------------------------
  
  
  output$sum_dis <- renderPlotly({
    
   


    plot_agg_3.1 <- disaster_clean %>%
      filter(Provinsi == input$input_prov) %>%
      group_by(Bulan) %>%
      summarise(Jumlah_Kejadian = n())
    
    plot_agg_3.1 <- plot_agg_3.1 %>% 
      mutate(label= glue("Bulan : {Bulan} 
                          Jumlah Kejadian : {Jumlah_Kejadian}"))
    
    plot3 <- plot_agg_3.1 %>% 
      ggplot(mapping = aes(x = Bulan, y = Jumlah_Kejadian, text = label)) +
      geom_col(fill= "#619b8a") +
      geom_text(aes(label = Jumlah_Kejadian), nudge_y = 1) +
    
      scale_y_continuous() +
      
      
      labs(title = glue("Jumlah Semua Kejadian Bencana di Provinsi {input$input_prov}"),
           x = NULL,
           y = "Jumlah Kejadian") +
      
      
      theme(legend.position = "none") +
      
      theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.05, vjust = 0.05),
            plot.title.position = "center",
            axis.ticks.y = element_blank(),
            plot.background = element_rect(fill = "#e2eafc"),
            panel.background = element_rect(fill = "#e2eafc"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            axis.text = element_text(color="black"))
      
      ggplotly(plot3, tooltip = "text")
    

  })
  

  
  # ---------------------------------------- PLOT 3.2 --------------------------------------
    
  
  
  output$prop_dis <- renderPlotly({
    
    
    plot_agg_3.2 <- disaster_clean %>%
      filter(Provinsi == input$input_prov) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Kejadian = n())
    
    plot_agg_3.2 <- plot_agg_3.2 %>% 
      mutate(label= glue("Bulan : {Bulan} 
                          Jenis Bencana : {Kejadian}
                          Jumlah Kejadian : {Jumlah_Kejadian}"))
    
    plot3.2 <- plot_agg_3.2 %>% 
      ggplot(mapping = aes(x = Bulan, y = Jumlah_Kejadian, text = label)) +
      geom_col(aes(fill = Kejadian), color = "#283618", position = "fill") +
      
      coord_flip() + 
      scale_y_continuous() +
      
      
      labs(title = glue("Proporsi Kejadian Bencana di Provinsi {input$input_prov}"),
           x = NULL,
           y = "Proporsi") +
      
      
      theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.05, vjust = 0.05),
            legend.background = element_rect(fill="#e2eafc"),
            plot.title.position = "center",
            axis.ticks.y = element_blank(),
            plot.background = element_rect(fill = "#e2eafc"),
            panel.background = element_rect(fill = "#e2eafc"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            axis.text = element_text(color="black"))
    
    ggplotly(plot3.2, tooltip = "text")
    
  })
  
  
  
  #---------------------------------------- PLOT 3.3 ------------------------------------------------------
  
  
  output$sum_kor <- renderPlotly({
    
    
    plot_agg_3.3 <- disaster_clean %>%
      filter(Provinsi == input$input_prov) %>% 
      select(Bulan, Meninggal, Terluka, Hilang, Rumah_Rusak, Rumah_Terendam, Fasum_Rusak) %>% 
      group_by(Bulan) %>% 
      summarise(Total_Korban = sum(Meninggal, Terluka,Hilang, Rumah_Rusak, Rumah_Terendam, Fasum_Rusak))
    
    plot_agg_3.3 <- plot_agg_3.3 %>% 
      mutate(label= glue("Bulan : {Bulan} 
                          Jumlah Korban dan Kerusakan : {Total_Korban}"))
    
    plot3.3 <- plot_agg_3.3 %>% 
      ggplot(mapping = aes(x = Bulan, y = Total_Korban, text = label)) +
      geom_col(fill= "#f95738") +
  
      geom_text(aes(label = Total_Korban), nudge_y = 0) +
      
      scale_y_continuous() +
      
      
      labs(title = glue("Jumlah Korban dan Kerusakan Kejadian Bencana di Provinsi {input$input_prov}"),
           x = NULL,
           y = "Jumlah Korban dan Kerusakan") +
      
      
      theme(legend.position = "none") +
      
      theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.05, vjust = 0.05),
            plot.title.position = "center",
            axis.ticks.y = element_blank(),
            plot.background = element_rect(fill = "#e2eafc"),
            panel.background = element_rect(fill = "#e2eafc"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            axis.text = element_text(color="black"))
    
    ggplotly(plot3.3, tooltip = "text")
    
    
  })
    
 
  
  
  #---------------------------------------- PLOT 3.4 ------------------------------------------------------
  
  
  
  output$prop_kor <- renderPlotly({
    
    data_agg_pivot <- disaster_clean %>% 
      pivot_longer(cols = c("Meninggal", "Terluka", "Hilang", "Rumah_Rusak", "Rumah_Terendam", "Fasum_Rusak"),
                   names_to = "Korban_Kerusakan",
                   values_to = "Jumlah_Korban")
    
    plot_agg_3.4 <- data_agg_pivot %>%
      filter(Provinsi == input$input_prov) %>% 
      group_by(Bulan, Korban_Kerusakan) %>% 
      summarise(Total_Korban = sum(Jumlah_Korban))
    
    plot_agg_3.4 <- plot_agg_3.4 %>% 
      mutate(label= glue("Bulan : {Bulan} 
                          Jumlah {Korban_Kerusakan}: {Total_Korban}"))
    
    plot3.4 <- plot_agg_3.4 %>% 
      ggplot(mapping = aes(x = Bulan, y = Total_Korban, text = label)) +
      geom_col(aes(fill = Korban_Kerusakan), color = "#283618", position = "fill") +
      coord_flip() +
      
      scale_fill_manual(values = c("#ffbe0b", "#fb5607", "#f72585", "#81b29a", "#d81159", "#e76f51")) +
      
      scale_y_continuous() +
      
      
      labs(title = glue("Proporsi Korban/Kerusakan Bencana di Provinsi {input$input_prov}"),
           x = NULL,
           y = "Proporsi",
           fill = "Korban/Kerusakan") +
    
      
      theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.05, vjust = 0.05),
            legend.background = element_rect(fill="#e2eafc"),
            plot.title.position = "center",
            axis.ticks.y = element_blank(),
            plot.background = element_rect(fill = "#e2eafc"),
            panel.background = element_rect(fill = "#e2eafc"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            axis.text = element_text(color="black"))
    
    ggplotly(plot3.4, tooltip = "text")
    

    
    
  })
  
  # ___________________________________________________________________________________________________________________
  # ____________________________________________________ PAGE 4 ____________________________________________________
  
  
  
  # -----------------------------------------------------MAP LEAFLET-----------------------------------------------------
  
  
  output$map_dis <- renderLeaflet({
    
    
    indo_sf_pergab <- sf::st_as_sf(indo_sf_pergab)
    
    
    if ("Banjir" %in% input$pilih_kejadian) {
      
      labels_map_Banjir <- glue("<b> {indo_sf_pergab$Banjir}</b> <br>
                     Jumlah Kejadian : {indo_sf_pergab$Banjir}") %>% 
        lapply(htmltools::HTML)
      
      # Membuat collor palet berdasarkan nilai
      
      pal_Banjir <- colorNumeric(palette = "Reds", 
                          domain = log(indo_sf_pergab$Banjir))
   
      
      indo_sf_pergab %>% 
        leaflet() %>% 
        addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addPolygons(fillColor = pal(log(indo_sf_pergab$Banjir)), 
                    fillOpacity = 0.8, 
                    weight = 2, 
                    label = labels_map_Banjir,  
                    color = "white", 
                    highlightOptions = highlightOptions(
                      color = "blue", 
                      weight = 5, 
                      bringToFront = TRUE, 
                      opacity = 0.8
                    ))  %>% 
        addLegend(pal = pal_Banjir,
                  values = log(indo_sf_pergab$Banjir),
                  labFormat = labelFormat(transform = function(x) round(exp(x))),
                  opacity = 1,
                  title = "Jumlah Kejadian Banjir", position = "bottomleft")
      
      
    }
    
    else if ("Gelombang_Pasang" %in% input$pilih_kejadian) {
      
      labels_map_Gelombang_Pasang <- glue("<b> {indo_sf_pergab$Gelombang_Pasang}</b> <br>
                     Jumlah Kejadian : {indo_sf_pergab$Gelombang_Pasang}") %>% 
        lapply(htmltools::HTML)
      
      # Membuat collor palet berdasarkan nilai
      
      pal_Gelombang_Pasang <- colorNumeric(palette = "Reds", 
                                 domain = indo_sf_pergab$Gelombang_Pasang)
      
  
      
      indo_sf_pergab %>% 
        leaflet() %>% 
        addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addPolygons(fillColor = pal(indo_sf_pergab$Gelombang_Pasang), 
                    fillOpacity = 0.8, 
                    weight = 2, 
                    label = labels_map_Gelombang_Pasang,  
                    color = "white", 
                    highlightOptions = highlightOptions(
                      color = "blue", 
                      weight = 5, 
                      bringToFront = TRUE, 
                      opacity = 0.8
                    ))  %>% 
        addLegend(pal = pal_Gelombang_Pasang,
                  values = indo_sf_pergab$Gelombang_Pasang,
                  opacity = 1,
                  title = "Jumlah Kejadian Gelombang Pasang", position = "bottomleft")
      
      
    }

    else if ("Gempa_Bumi" %in% input$pilih_kejadian) {
      
      labels_map_Gempa_Bumi <- glue("<b> {indo_sf_pergab$Gempa_Bumi}</b> <br>
                     Jumlah Kejadian : {indo_sf_pergab$Gempa_Bumi}") %>% 
        lapply(htmltools::HTML)
      
      # Membuat collor palet berdasarkan nilai
      
      pal_Gempa_Bumi <- colorNumeric(palette = "Reds", 
                                           domain = indo_sf_pergab$Gempa_Bumi)
      
      
      
      indo_sf_pergab %>% 
        leaflet() %>% 
        addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addPolygons(fillColor = pal(indo_sf_pergab$Gempa_Bumi), 
                    fillOpacity = 0.8, 
                    weight = 2, 
                    label = labels_map_Gempa_Bumi,  
                    color = "white", 
                    highlightOptions = highlightOptions(
                      color = "blue", 
                      weight = 5, 
                      bringToFront = TRUE, 
                      opacity = 0.8
                    ))  %>% 
        addLegend(pal = pal_Gempa_Bumi,
                  values = indo_sf_pergab$Gempa_Bumi,
                  opacity = 1,
                  title = "Jumlah Kejadian Gempa Bumi", position = "bottomleft")
      
      
    }
    
    
    
    else if ("Kebakaran_Hutan" %in% input$pilih_kejadian) {
      
      labels_map_Kebakaran_Hutan <- glue("<b> {indo_sf_pergab$Kebakaran_Hutan}</b> <br>
                     Jumlah Kejadian : {indo_sf_pergab$Kebakaran_Hutan}") %>% 
        lapply(htmltools::HTML)
      
      # Membuat collor palet berdasarkan nilai
      
      pal_Kebakaran_Hutan <- colorNumeric(palette = "Reds", 
                                     domain = indo_sf_pergab$Kebakaran_Hutan)
      
      
      
      indo_sf_pergab %>% 
        leaflet() %>% 
        addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addPolygons(fillColor = pal(indo_sf_pergab$Kebakaran_Hutan), 
                    fillOpacity = 0.8, 
                    weight = 2, 
                    label = labels_map_Kebakaran_Hutan,  
                    color = "white", 
                    highlightOptions = highlightOptions(
                      color = "blue", 
                      weight = 5, 
                      bringToFront = TRUE, 
                      opacity = 0.8
                    ))  %>% 
        addLegend(pal = pal_Kebakaran_Hutan,
                  values = indo_sf_pergab$Kebakaran_Hutan,
                  opacity = 1,
                  title = "Jumlah Kejadian Kebakaran Hutan", position = "bottomleft")
      
      
    }
    
    
    
    else if ("Kekeringan" %in% input$pilih_kejadian) {
      
      labels_map_Kekeringan <- glue("<b> {indo_sf_pergab$Kekeringan}</b> <br>
                     Jumlah Kejadian : {indo_sf_pergab$Kekeringan}") %>% 
        lapply(htmltools::HTML)
      
      # Membuat collor palet berdasarkan nilai
      
      pal_Kekeringan <- colorNumeric(palette = "Reds", 
                                          domain = indo_sf_pergab$Kekeringan)
      
      
      
      indo_sf_pergab %>% 
        leaflet() %>% 
        addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addPolygons(fillColor = pal(indo_sf_pergab$Kekeringan), 
                    fillOpacity = 0.8, 
                    weight = 2, 
                    label = labels_map_Kekeringan,  
                    color = "white", 
                    highlightOptions = highlightOptions(
                      color = "blue", 
                      weight = 5, 
                      bringToFront = TRUE, 
                      opacity = 0.8
                    ))  %>% 
        addLegend(pal = pal_Kekeringan,
                  values = indo_sf_pergab$Kekeringan,
                  opacity = 1,
                  title = "Jumlah Kejadian Kekeringan", position = "bottomleft")
      
      
    }
    
    
    else if ("Letusan_Gunung" %in% input$pilih_kejadian) {
      
      labels_map_Letusan_Gunung <- glue("<b> {indo_sf_pergab$Letusan_Gunung}</b> <br>
                     Jumlah Kejadian : {indo_sf_pergab$Letusan_Gunung}") %>% 
        lapply(htmltools::HTML)
      
      # Membuat collor palet berdasarkan nilai
      
      pal_Letusan_Gunung <- colorNumeric(palette = "Reds", 
                                     domain = indo_sf_pergab$Letusan_Gunung)
      
      
      
      indo_sf_pergab %>% 
        leaflet() %>% 
        addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addPolygons(fillColor = pal(indo_sf_pergab$Letusan_Gunung), 
                    fillOpacity = 0.8, 
                    weight = 2, 
                    label = labels_map_Letusan_Gunung,  
                    color = "white", 
                    highlightOptions = highlightOptions(
                      color = "blue", 
                      weight = 5, 
                      bringToFront = TRUE, 
                      opacity = 0.8
                    ))  %>% 
        addLegend(pal = pal_Letusan_Gunung,
                  values = indo_sf_pergab$Letusan_Gunung,
                  opacity = 1,
                  title = "Jumlah Kejadian Letusan Gunung", position = "bottomleft")
      
      
    }
    
    
    else if ("Puting_Beliung" %in% input$pilih_kejadian) {
      
      labels_map_Puting_Beliung <- glue("<b> {indo_sf_pergab$Puting_Beliung}</b> <br>
                     Jumlah Kejadian : {indo_sf_pergab$Puting_Beliung}") %>% 
        lapply(htmltools::HTML)
      
      # Membuat collor palet berdasarkan nilai
      
      pal_Puting_Beliung <- colorNumeric(palette = "Reds", 
                                         domain = indo_sf_pergab$Puting_Beliung)
      
      
      
      indo_sf_pergab %>% 
        leaflet() %>% 
        addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addPolygons(fillColor = pal(indo_sf_pergab$Puting_Beliung), 
                    fillOpacity = 0.8, 
                    weight = 2, 
                    label = labels_map_Puting_Beliung,  
                    color = "white", 
                    highlightOptions = highlightOptions(
                      color = "blue", 
                      weight = 5, 
                      bringToFront = TRUE, 
                      opacity = 0.8
                    ))  %>% 
        addLegend(pal = pal_Puting_Beliung,
                  values = indo_sf_pergab$Puting_Beliung,
                  opacity = 1,
                  title = "Jumlah Kejadian Puting Beliung", position = "bottomleft")
      
      
    }
    
    else if ("Tanah_Longsor" %in% input$pilih_kejadian) {
      
      labels_map_Tanah_Longsor <- glue("<b> {indo_sf_pergab$Tanah_Longsor}</b> <br>
                     Jumlah Kejadian : {indo_sf_pergab$Tanah_Longsor}") %>% 
        lapply(htmltools::HTML)
      
      # Membuat collor palet berdasarkan nilai
      
      pal_Tanah_Longsor <- colorNumeric(palette = "Reds", 
                                         domain = indo_sf_pergab$Tanah_Longsor)
      
      
      
      indo_sf_pergab %>% 
        leaflet() %>% 
        addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addPolygons(fillColor = pal(indo_sf_pergab$Tanah_Longsor), 
                    fillOpacity = 0.8, 
                    weight = 2, 
                    label = labels_map_Tanah_Longsor,  
                    color = "white", 
                    highlightOptions = highlightOptions(
                      color = "blue", 
                      weight = 5, 
                      bringToFront = TRUE, 
                      opacity = 0.8
                    ))  %>% 
        addLegend(pal = pal_Tanah_Longsor,
                  values = indo_sf_pergab$Tanah_Longsor,
                  opacity = 1,
                  title = "Jumlah Kejadian Tanah Longsor", position = "bottomleft")
      
      
    }
    
    
  })
  
   
  # -----------------------------------------------------PLOT 4.1-----------------------------------------------------
  
  
  output$series_dis <- renderPlotly({
    
    plot_agg_4.1 <- disaster_clean %>% 
      
      filter(Tanggal_Kejadian>=input$input_kalender[1] & Tanggal_Kejadian<=input$input_kalender[2]) %>% 
      filter(Provinsi == input$input_provinsi) %>% 
      
      group_by(Tanggal_Kejadian, Kejadian, Provinsi) %>% 
      summarise(Jumlah_Kejadian =n())
    
    plot_agg_4.1 <- plot_agg_4.1 %>% 
      mutate(label= glue("Tanggal : {Tanggal_Kejadian} 
                         Jenis Kejadian : {Kejadian}
                          Jumlah : {Jumlah_Kejadian}"))
    
    plot4.1 <- plot_agg_4.1 %>% 
      ggplot(mapping = aes( x= Tanggal_Kejadian, 
                       y = Jumlah_Kejadian, text = label)) +
      
      
       geom_col(aes(fill=Kejadian)) +
      
   
     
      
      labs(title = glue("Trend Jumlah Kejadian Bencana Bulan {input$input_bulan} di Provinsi {input$input_provinsi}"),
           x = "Tanggal",
           y = "Jumlah Kejadian") +
      
      theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.05, vjust = 0.05),
            legend.background = element_rect(fill="#e2eafc"),
            plot.title.position = "center",
            axis.ticks.y = element_blank(),
            plot.background = element_rect(fill = "#e2eafc"),
            panel.background = element_rect(fill = "#e2eafc"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            axis.text = element_text(color="black"))
    
    
    ggplotly(plot4.1, tooltip = "text")
    
    
  })
  
  
  
  
  #-------------------------------------------------- PLOT 4.2 --------------------------------------------------------
  
  
  # 
  # output$series_kor <- renderPlotly({
  #   
  #   plot_agg_4.2 <- data_agg_pivot %>% 
  #     
  #     filter(Tanggal_Kejadian>=input$input_kalender[1] & Tanggal_Kejadian<=input$input_kalender[2]) %>% 
  #     filter(Provinsi == input$input_provinsi) %>% 
  #     group_by(Korban_Kerusakan, Jumlah_Korban) %>% 
  #     summarise(Total_Korban = sum(Jumlah_Korban))
  # 
  #   
  #   plot_agg_4.2 <- plot_agg_4.2 %>% 
  #     mutate(label= glue("Tanggal : {Tanggal_Kejadian} 
  #                        Korban/Kejadian : {Korban_Kerusakan}
  #                         Jumlah : {Jumlah_Korban}"))
  #   
  #   plot4.2 <- plot_agg_4.2 %>% 
  #     ggplot(mapping = aes( x= Tanggal_Kejadian, 
  #                           y = Jumlah_Korban, text = label)) +
  #     
  #     
  #     geom_col() +
  #     
  #     
  #     
  #     
  #     labs(title = glue("Trend Jumlah Kejadian Bencana Bulan {input$input_kalender} di Provinsi {input$input_provinsi}"),
  #          x = "Tanggal",
  #          y = "Jumlah Korban/Kerusakan") +
  #     
  #     theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.05, vjust = 0.05),
  #           plot.title.position = "center",
  #           axis.ticks.y = element_blank(),
  #           plot.background = element_rect(fill = "#e2eafc"),
  #           panel.background = element_rect(fill = "#e2eafc"),
  #           panel.grid.major.x = element_blank(),
  #           panel.grid.major.y = element_blank(),
  #           panel.grid.minor.x = element_blank(),
  #           panel.grid.minor.y = element_blank(),
  #           axis.line.x = element_line(color = "black"),
  #           axis.line.y = element_line(color = "black"),
  #           axis.text = element_text(color="black"))
  #   
  #   
  #   ggplotly(plot4.2, tooltip = "text")
  #   
  #   
  # })
  
  
  # ___________________________________________________________________________________________________________________
  # ____________________________________________________ DATA TABLE ____________________________________________________
  
  
  
  output$table_data <- renderDataTable({
    disaster_table <- disaster_clean %>% 
      select(-Penyebab)
    
    datatable(data = disaster_table,
              options = list(scrollX = TRUE)
    )
  })
  
  
  
  
}