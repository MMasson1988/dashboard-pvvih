library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)
library(lubridate)
library(leaflet)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(writexl)
library(gganimate)
library(gifski)
library(tidyr)

# --- CHARGEMENT DES DONNÉES ---
df <- readxl::read_excel("data_hiv.xlsx", sheet = "Sheet1")
# --- UI ---
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "CarisCareTrack"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Vue d'ensemble", tabName = "dashboard", icon = icon("dashboard")),
            selectInput("niveau_risque", "Filtrer par risque:", choices = c("Tous", unique(df$niveau_risque)), selected = "Tous"),
            selectInput("arv", "Filtrer par ARV:", choices = c("Tous", unique(df$type_arv)), selected = "Tous"),
            selectInput("proximite", "Proximité clinique:", choices = c("Tous", unique(df$acces_clinique_proximite)), selected = "Tous"),
            selectInput("agent", "Choisir un agent de terrain:", choices = c("Moise" = "+50940317880", "Masson" = "+50941743538", "Cassion" = "+50949163700"), selected = "+50940317880"),
            actionButton("verif_risque", "Verifier les risques PVVIH", class = "btn-danger"),
            actionButton("send_whatsapp", "Envoyer WhatsApp", class = "btn-success")
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        fluidRow(valueBoxOutput("totalBox"), valueBoxOutput("risqueBox"), valueBoxOutput("suiviBox")),
        
        fluidRow(
            box("Distribution des âges", status = "primary", solidHeader = TRUE, plotlyOutput("agePlot"), width = 6),
            box("Nombre de fois détectable", status = "danger", solidHeader = TRUE, plotlyOutput("detectablePlot"), width = 6)
        ),
        
        fluidRow(
            box("Type d'ARV", status = "success", solidHeader = TRUE, plotlyOutput("arvPlot"), width = 6),
            box("Charge virale vs Risque", status = "warning", solidHeader = TRUE, plotlyOutput("riskPlot"), width = 6)
        ),
        
        fluidRow(
            box(
                title = "Bar Chart Race - Sexe / Mois",
                status = "success",
                solidHeader = TRUE,
                width = 6,
                style = "height: 420px;",
                imageOutput("racePlot", height = "400px")
            ),
            box(
                title = "Carte des patients",
                status = "info",
                solidHeader = TRUE,
                width = 6,
                style = "height: 420px;",
                leafletOutput("mapPlot", height = "400px")
            )
        )
    )
)

# --- SERVER ---
server <- function(input, output, session) {
    
    data_filtered <- reactive({
        df_filtered <- df
        if (input$niveau_risque != "Tous") df_filtered <- df_filtered %>% filter(niveau_risque == input$niveau_risque)
        if (input$arv != "Tous") df_filtered <- df_filtered %>% filter(type_arv == input$arv)
        if (input$proximite != "Tous") df_filtered <- df_filtered %>% filter(acces_clinique_proximite == input$proximite)
        df_filtered
    })
    
    observeEvent(input$verif_risque, {
        showModal(modalDialog(
            title = "Confirmation",
            "Voulez-vous afficher la liste des PVVIH à risque élevé ?",
            easyClose = TRUE,
            footer = tagList(
                actionButton("afficher_risques", "Oui", class = "btn btn-danger"),
                modalButton("Non")
            )
        ))
    })
    
    observeEvent(input$afficher_risques, {
        removeModal()
        high_risk <- data_filtered() %>% filter(niveau_risque == "Élevé")
        
        if (nrow(high_risk) > 0) {
            showModal(modalDialog(
                title = div("PVVIH à Risque Elevé", style = "font-weight:bold;font-size:18px;color:#c0392b;"),
                HTML(paste0("<p style='font-size:16px;'>Nous avons détecté <strong>", nrow(high_risk), "</strong> patient(s) à <span style='color:red;font-weight:bold;'>risque élevé</span>.</p>")),
                DTOutput("table_popup"),
                br(),
                tags$div(style = "display:flex; gap:10px;",
                         downloadButton("download_csv", "CSV", class = "btn btn-info"),
                         downloadButton("download_excel", "Excel", class = "btn btn-success")
                ),
                easyClose = TRUE,
                size = "l",
                footer = modalButton("Fermer")
            ))
            
            output$table_popup <- renderDT({
                datatable(high_risk, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
            })
        } else {
            showModal(modalDialog(
                title = div("Aucun risque détecté", style = "font-weight: bold; color: green"),
                HTML("<p style='font-size:16px;'>Aucun patient à risque élevé selon les filtres sélectionnés.</p>"),
                easyClose = TRUE,
                footer = modalButton("Fermer")
            ))
        }
    })
    
    output$download_csv <- downloadHandler(
        filename = function() paste0("Liste_pvvih_a_risque_", Sys.Date(), ".csv"),
        content = function(file) {
            write.csv(data_filtered() %>% filter(niveau_risque == "Élevé"), file, row.names = FALSE)
        }
    )
    
    output$download_excel <- downloadHandler(
        filename = function() paste0("Liste_pvvih_a_risque_", Sys.Date(), ".xlsx"),
        content = function(file) {
            writexl::write_xlsx(data_filtered() %>% filter(niveau_risque == "Élevé"), path = file)
        }
    )
    
    output$totalBox <- renderValueBox({
        valueBox(nrow(data_filtered()), "PVVIH total", icon = icon("users"), color = "purple")
    })
    
    output$risqueBox <- renderValueBox({
        high_risk <- data_filtered() %>% filter(niveau_risque == "Élevé")
        valueBox(
            paste0(round(nrow(high_risk) / nrow(data_filtered()) * 100, 1), "%"),
            "à risque élevé", icon = icon("exclamation-triangle"), color = "red"
        )
    })
    
    output$suiviBox <- renderValueBox({
        faible_suivi <- data_filtered() %>% filter(nombre_visites_medicales <= 2)
        valueBox(nrow(faible_suivi), "Suivi médical faible", icon = icon("stethoscope"), color = "orange")
    })
    
    output$agePlot <- renderPlotly({
        plot_ly(data_filtered(), x = ~age, type = "histogram", marker = list(color = "#6a1b9a")) %>%
            layout(title = "Distribution des âges", xaxis = list(title = "Age"))
    })
    
    output$detectablePlot <- renderPlotly({
        plot_ly(data_filtered(), y = ~nb_fois_detectable, type = "box", color = ~niveau_risque, colors = "Set1") %>%
            layout(title = "Nombre de fois détectable par niveau de risque")
    })
    
    output$arvPlot <- renderPlotly({
        df_arv <- data_filtered() %>% group_by(type_arv) %>% summarise(N = n())
        plot_ly(df_arv, x = ~type_arv, y = ~N, type = "bar", marker = list(color = "#43a047")) %>%
            layout(title = "Nombre de patients par type d'ARV")
    })
    
    output$riskPlot <- renderPlotly({
        plot_ly(data_filtered(), x = ~niveau_risque, y = ~duree_detectable_mois, type = "box",
                color = ~niveau_risque, colors = "Set2") %>%
            layout(title = "Charge virale détectable vs niveau de risque")
    })
    
    output$mapPlot <- renderLeaflet({
        data_geo <- data_filtered() %>% filter(!is.na(latitude), !is.na(longitude))
        if (nrow(data_geo) == 0) {
            leaflet() %>% addTiles() %>%
                addPopups(0, 0, "Aucun point géographique disponible avec les filtres actuels.")
        } else {
            pal <- colorFactor(palette = c("green", "orange", "red"), domain = data_geo$niveau_risque)
            leaflet(data_geo) %>%
                addProviderTiles("CartoDB.Positron") %>%
                addCircleMarkers(
                    lng = ~longitude, lat = ~latitude,
                    color = ~pal(niveau_risque),
                    radius = 6, stroke = FALSE, fillOpacity = 0.8,
                    popup = ~htmltools::HTML(paste0(
                        "<strong>Sexe:</strong> ", sexe, "<br/>",
                        "<strong>Commune:</strong> ", commune, "<br/>",
                        "<strong>Région:</strong> ", region, "<br/>",
                        "<strong>Risque:</strong> ", niveau_risque
                    ))
                ) %>%
                addLegend("bottomright", pal = pal, values = ~niveau_risque,
                          title = "Niveau de risque", opacity = 1)
        }
    })
    
    observeEvent(input$send_whatsapp, {
        high_risk <- data_filtered() %>% filter(niveau_risque == "Élevé")
        nb_patients <- nrow(high_risk)
        if (input$agent == "" || is.null(input$agent)) {
            showNotification("Veuillez choisir un agent avant d'envoyer le message.", type = "error")
            return()
        }
        if (nb_patients == 0) {
            showNotification("Aucun patient à risque élevé à signaler.", type = "warning")
            return()
        }
        list_patients <- paste0("- ", high_risk$patient_id, " (", high_risk$sexe, ") (", high_risk$commune, ")", collapse = "\n")
        message_txt <- paste0("Bonjour, vous avez ", nb_patients,
                              " patients PVVIH à risque élevé à suivre :\n\n",
                              list_patients,
                              "\n\nMerci de vérifier le dashboard CarisCareTrack.")
        message_url <- URLencode(message_txt, reserved = TRUE)
        phone <- gsub("\\+", "", input$agent)
        wa_url <- paste0("https://wa.me/", phone, "?text=", message_url)
        shinyjs::runjs(sprintf("window.open('%s','_blank')", wa_url))
    })
    
    output$racePlot <- renderImage({
        data_race <- df %>%
            mutate(
                date = lubridate::floor_date(date_derniere_visite, "month"),
                date_label = format(date, "%B-%Y")
            ) %>%
            group_by(sexe, date_label) %>%
            summarise(n = n(), .groups = "drop") %>%
            complete(sexe, date_label, fill = list(n = 0)) %>%
            arrange(date_label)
        
        sexe_order <- data_race %>%
            group_by(sexe) %>%
            summarise(total = sum(n)) %>%
            arrange(desc(total)) %>%
            pull(sexe)
        
        data_race$sexe <- factor(data_race$sexe, levels = sexe_order)
        
        p <- ggplot(data_race, aes(x = reorder(sexe, n), y = n, fill = sexe)) +
            geom_col(show.legend = FALSE) +
            geom_text(aes(label = n), hjust = -0.2, color = "black", size = 5) +
            coord_flip(clip = "off") +
            labs(title = 'Mois: {closest_state}', x = NULL, y = "Nombre de visites") +
            transition_states(date_label, transition_length = 2, state_length = 10) +
            ease_aes('cubic-in-out') +
            theme_minimal() +
            theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
        
        anim <- animate(p, renderer = gifski_renderer(), width = 600, height = 400, res = 96)
        anim_save("www/bar_chart_race.gif", animation = anim)
        
        list(
            src = "www/bar_chart_race.gif",
            contentType = 'image/gif',
            width = 600,
            height = 400
        )
    }, deleteFile = FALSE)
    
    
}

# --- RUN APP ---
shinyApp(ui, server)

