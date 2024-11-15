## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(gt)
library(broom)
library(magrittr)
library(ggdark)
library(DT)
library(htmltools)
library(fresh)

# Create a dark theme
dark_theme <- create_theme(
  adminlte_color(
    light_blue = "#111111"
  ),
  adminlte_sidebar(
    dark_bg = "#1e272e",
    dark_hover_bg = "#485460",
    dark_color = "#f5f6fa"
  ),
  adminlte_global(
    content_bg = "#111111",
    box_bg = "#000",
    info_box_bg = "#000"
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Linear Model"),
  dashboardSidebar(
    actionButton("rem_point", "remove 1"),
    actionButton("reset_all", "⟳")
  ),
  dashboardBody(
    use_theme(dark_theme),
    tags$head(
      tags$style(HTML("
      .sidebar {
      display: flex;
      flex-direction: column;
      align-items: center; /* Zentriert Buttons horizontal */
      justify-content: center; /* Zentriert Buttons vertikal */
      height: 100%; /* Deckt die volle Höhe der Sidebar ab */
    }
    .action-button {
      background-color: #34495e;
      color: white;
      border: none;
      border-radius: 0px;
      height: 110px;
      width: 110px;
      font-size: 14px;
      display: inline-flex;
      justify-content: center;
      align-items: center;
      margin: 10px 0; /* Abstand zwischen Buttons */
    }
    .action-button:hover {
      background-color: #485460;
    }
    .dataTable {
      color: white; /* Schriftfarbe auf Weiß setzen */
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      color: white !important; /* Seitenzahlen in Weiß */
    }
    .dataTables_wrapper .dataTables_filter input {
      background-color: #2c3e50; /* Hintergrundfarbe des Suchfeldes */
      color: white; /* Schriftfarbe im Suchfeld */
    }
    .dataTables_wrapper .dataTables_length select {
      background-color: #2c3e50; /* Hintergrundfarbe des Auswahlmenüs */
      color: white; /* Schriftfarbe im Auswahlmenü */
    }
    table.dataTable tr {
      background-color: #34495e; /* Hintergrund der Tabellenzeilen */
    }
    table.dataTable tr:nth-child(even) {
      background-color: #3b5361; /* Alternierende Zeilenfarbe */
    }
    table.dataTable tr:hover {
      background-color: #485460; /* Hover-Effekt */
    }
    .box {
      overflow: auto; /* Ermöglicht Scrollen */
    }
    .dataTables_wrapper {
      margin: 10px; /* Abstand einfügen */
    }
  "))
    ),
    fluidRow(
      box(
        title = "", status = "primary", solidHeader = TRUE,
        width = 9,
        h4("Click & add points", style = "color: white;"),
        plotOutput("plot1", click = "lmclick", height = "800px")
      ),
      box(
        title = "Raw Data Table", status = "primary", solidHeader = TRUE,
        width = 3,
        DT::dataTableOutput("table1", width = "90%", fill = FALSE),
        h4("Model Parameter", style = "color: white;"),
        uiOutput("linfit2")
      )
    )
  )
)

server <- function(input, output) {
  
  values <- reactiveValues()
  
  DT1 <- data.table(Nr = numeric(),
                    x = numeric(),
                    y = numeric(),
                    Pred = numeric(),
                    Resid = numeric(),
                    resid_num = numeric())
  
  values$DT <- DT1
  values$linear_model <- list()
  
  ## Plot erstellen ##
  output$plot1 <- renderPlot({
    
    if(nrow(values$DT) > 1){
      
      dt_act <- values$DT
      
      msd <- dt_act[, .(y_mean = mean(y), 
                        y_sd = sd(y), 
                        pred_mean = mean(Pred), 
                        pred_sd = sd(Pred), 
                        x_min = min(x) - 0.2/abs(max(x) - min(x)),
                        x_min2 = min(x) - 0.25/abs(max(x) - min(x)),
                        x_max = max(x) + 0.2/abs(max(x) - min(x)),
                        x_max2 = max(x) + 0.25/abs(max(x) - min(x)))]
      
      ggplot(dt_act, aes(x = x, y = y)) +
        geom_point(shape = 1, size = 3) + 
        geom_smooth(method = "lm", se = FALSE, linetype = 2) + 
        geom_pointrange(data = msd, aes(
          x = 99,
          y = y_mean, 
          ymin = y_mean + y_sd, 
          ymax = y_mean - y_sd), color = "red", shape = 1) +
        geom_pointrange(data = msd, aes(
          x = 98,
          y = pred_mean, 
          ymin = pred_mean + pred_sd, 
          ymax = pred_mean - pred_sd), color = "blue", shape = 1) +
        geom_segment(aes(yend = Pred, xend = x), color = "red", linetype = 1, size = 2) + 
        geom_text(aes(y = resid_num, label = round(Resid, 1)), hjust = -0.1, size = 5) +
        geom_rug(sides = "r", length = unit(2, "mm"), color = "red") + 
        geom_rug(aes(y = Pred), sides = "r", length = unit(2, "mm"), color = "blue") +
        lims(x = c(0, 100), y = c(0, 100)) +
        xlab("Unabhängige Variable") +
        ylab("Abhängige Variable") +
        ggdark::dark_theme_bw(base_size = 18)
    } else {
      ggplot(values$DT, aes(x = x, y = y)) +
        geom_point(shape = 1, size = 3)   + 
        lims(x = c(0, 100), y = c(0, 100)) + 
        ggdark::dark_theme_bw(base_size = 18)
    }
  })    
  
  ## Neuen Punkt beim Klicken hinzufügen ##
  observeEvent(input$lmclick, {
    add_row <- data.table(Nr = 1,
                          x = input$lmclick$x,
                          y = input$lmclick$y,
                          Pred = 0,
                          Resid = 0,
                          resid_num = 0)
    
    DT_new <- rbind(values$DT, add_row)
    
    if(nrow(DT_new) > 1){
      full_model <- lm(y ~ x, data = DT_new)
      DT_new[, Pred := predict(full_model)]
      DT_new[, Resid := (y - Pred)^2]
      DT_new[, Nr := 1:.N]
      DT_new[, resid_num := mean(c(y, Pred)), by = Nr]
    } else {
      full_model <- list()    
    }
    
    values$DT <- DT_new
    values$linear_model <- full_model
  })
  
  ## Letzten Punkt beim Klick auf Button entfernen ##
  observeEvent(input$rem_point, {
    if (nrow(values$DT) > 0) {
      values$DT <- values$DT[-nrow(values$DT), ]
    }
  })
  
  observeEvent(input$reset_all, {
    # Setze die Punkte und das lineare Modell zurück
    values$DT <- DT1  # Leerer Data Table
    values$linear_model <- list()  # Kein Modell mehr vorhanden
  })
  
  
  ## Tabelle rendern ##
  output$table1 <- DT::renderDataTable({
    printDT <- values$DT
    printDT <- printDT[, lapply(.SD, function(rr) round(rr,2))]
    setorder(printDT, -Nr)
    DT::datatable(printDT[, !c("resid_num")], 
                  options = list(lengthMenu = c(5, 30, 50), 
                                 pageLength = 5))
  })
  
  ## Zusammenfassung des linearen Modells anzeigen ##
  output$linfit <- renderPrint({
    if (length(values$linear_model) > 0) {
      summary(values$linear_model)
    }
  })
  
  
  output$linfit2 <- renderUI({
    if (length(values$linear_model) > 0) {
      # Extrahiere Modellinformationen
      model_summary <- tidy(values$linear_model)[, c("term", "estimate", "std.error", "p.value")]
      
      # Erstelle eine gt-Tabelle
      table <- model_summary %>%
        gt() %>%
        tab_header(title = "Lineares Modell - Zusammenfassung") %>%
        cols_label(
          term = "Parameter",
          estimate = "Schätzwert",
          std.error = "Std. Fehler",
          p.value = "p-Wert"
        ) %>%
        fmt_number(
          columns = c(estimate, std.error, p.value),
          decimals = 3
        ) %>%
        tab_style(
          style = cell_text(color = "white"),
          locations = cells_body()
        ) %>%
        tab_options(
          table.background.color = "#2c3e50",  # Dunkler Hintergrund
          heading.background.color = "#34495e",  # Kopfzeile
          column_labels.background.color = "#3b5361",  # Spaltenkopf
          column_labels.font.weight = "bold",
          table.font.size = 14
        )
      
      # HTML für Shiny rendern
      HTML(as_raw_html(table))
    } else {
      h4("------", style = "color: white;")
    }
  })
  
}

shinyApp(ui, server)
