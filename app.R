## CarisCareTrack personnalisée
##
## Cette version améliore l'interface utilisateur en la rendant plus moderne et personnelle.
## Les améliorations comprennent :
##   • Utilisation d'une police Google (Montserrat) et ajout d'icônes pour un rendu élégant.
##   • Introduction d'un panneau utilisateur dans la barre latérale pour accueillir les utilisateurs.
##   • Remplacement des sélecteurs par des `pickerInput` de shinyWidgets pour une meilleure ergonomie.
##   • Boutons stylisés via `actionBttn` de shinyWidgets.
##   • Personnalisation de la charte graphique avec des bordures arrondies et des ombres.
##
## Les fonctionnalités analytiques restent identiques à la version de base : filtres par risque, ARV et proximité, affichage des
## indicateurs, cartes et graphiques, téléchargement des listes, animation bar chart race et intégration WhatsApp.

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(readxl)
library(lubridate)
library(leaflet)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(writexl)
library(tidyr)
library(shinymanager)
library(gganimate)
library(gifski)

## --- CHARGEMENT DES DONNÉES ---
df <- readxl::read_excel("data_hiv.xlsx", sheet = "Sheet1")

## --- AJOUT : Définition des utilisateurs ---
credentials <- data.frame(
    user = c("admin", "caris1234"),
    password = c("adminpass", "carishaiti1234"),
    stringsAsFactors = FALSE
)

## --- INTERFACE UTILISATEUR ---
##
## Nous personnalisons également la page de connexion fournie par
## `secure_app()` pour y inclure un message d'accueil.  L'argument
## `tags_top` permet d'ajouter des balises HTML au-dessus du module
## d'authentification.  Nous l'utilisons pour afficher un titre et
## un sous-titre, centrés et stylisés avec la police Montserrat.
ui <- secure_app(
    # UI de l'application (dashboard) protégée
    ui = dashboardPage(
        skin = "purple",
        dashboardHeader(
            title = div(icon("heartbeat"), "CarisCareTrack",
                        style = "font-family: 'Montserrat', sans-serif; font-weight:700;"),
            titleWidth = 300
        ),
        dashboardSidebar(
            width = 250,
            ## Panneau utilisateur personnalisé
            tags$div(
                class = "user-panel",
                style = "padding:15px; background-color:#F5F5F5; border-radius:6px; margin:10px;",
                tags$div(class = "pull-left image",
                         tags$img(src = "https://cdn-icons-png.flaticon.com/512/1077/1077114.png",
                                  class = "img-circle", height = "40px")),
                tags$div(class = "pull-left info", style = "margin-left:10px;",
                         tags$p("Bienvenue !",
                                style = "margin:0; font-weight:bold; font-family:'Montserrat', sans-serif;"),
                         tags$small("Suivi PVVIH", style = "margin:0;"))
            ),
            sidebarMenu(
                menuItem("Vue d'ensemble", tabName = "dashboard", icon = icon("dashboard")),
                br(),
                ## Filtres avec pickerInput pour un rendu moderne
                pickerInput("niveau_risque", "Filtrer par risque:",
                            choices = c("Tous", unique(df$niveau_risque)), selected = "Tous"),
                pickerInput("arv", "Filtrer par ARV:",
                            choices = c("Tous", unique(df$type_arv)), selected = "Tous"),
                pickerInput("proximite", "Proximité clinique:",
                            choices = c("Tous", unique(df$acces_clinique_proximite)), selected = "Tous"),
                pickerInput("agent", "Agent de terrain:",
                            choices = c(Moise = "+50940317880", Masson = "+50941743538", Cassion = "+50949163700"),
                            selected = "+50940317880"),
                br(),
                actionBttn("verif_risque", "Vérifier risques PVVIH", color = "danger", style = "fill"),
                actionBttn("send_whatsapp", "Envoyer WhatsApp", color = "success", style = "fill")
            )
        ),
        dashboardBody(
            shinyjs::useShinyjs(),
            tags$head(
                tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Montserrat:400,700&display=swap"),
                tags$style(HTML(paste0(
                    ".content-wrapper, .right-side {background-color: #F7F7F7;}\n",
                    ".main-header .navbar {background-color: #6C5B7B;}\n",
                    ".main-header .logo {background-color: #6C5B7B; color: white !important; font-family: 'Montserrat', sans-serif; font-weight: bold;}\n",
                    ".box {border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);}\n",
                    ".value-box {border-radius: 8px;}\n"
                )))
            ),
            ## Utilisation de tabItems pour assurer l'association avec le menu
            tabItems(
                tabItem(tabName = "dashboard",
                        fluidRow(
                            valueBoxOutput("totalBox"),
                            valueBoxOutput("risqueBox"),
                            valueBoxOutput("suiviBox")
                        ),
                        fluidRow(
                            box(title = "Distribution des âges", status = "primary", solidHeader = TRUE,
                                plotlyOutput("agePlot"), width = 6),
                            box(title = "Nombre de fois détectable", status = "danger", solidHeader = TRUE,
                                plotlyOutput("detectablePlot"), width = 6)
                        ),
                        fluidRow(
                            box(title = "Type d'ARV", status = "success", solidHeader = TRUE,
                                plotlyOutput("arvPlot"), width = 6),
                            box(title = "Charge virale vs Risque", status = "warning", solidHeader = TRUE,
                                plotlyOutput("riskPlot"), width = 6)
                        ),
                        fluidRow(
                            box(title = "Bar Chart Race - Sexe / Mois", status = "success", solidHeader = TRUE, width = 6,
                                imageOutput("racePlot", height = "400px")),
                            box(title = "Carte des patients", status = "info", solidHeader = TRUE, width = 6,
                                leafletOutput("mapPlot", height = "400px"))
                        )
                )
            )
        )
    ),
    tags_top = tags$div(
        tags$h2(
            "Bienvenue sur le CarisCareTrack",
            style = "margin-top:0; font-family:'Montserrat', sans-serif; font-weight:700; color:#343a40;"
        ),
        tags$h5(
            "Un dashboard pour révolutionner le monitoring des projets en Haïti dans un style moderne",
            style = "margin-bottom:0; font-family:'Montserrat', sans-serif; font-weight:400; color:#6c757d;"
        ),
        style = "text-align:center; margin-bottom:20px;"
    )
)

## --- LOGIQUE SERVEUR ---
server <- function(input, output, session) {
    ## Authentification via shinymanager
    res_auth <- shinymanager::secure_server(
        check_credentials = shinymanager::check_credentials(credentials)
    )
    ## Filtrage réactif des données
    data_filtered <- reactive({
        df_filtered <- df
        if (input$niveau_risque != "Tous") df_filtered <- df_filtered %>% filter(niveau_risque == input$niveau_risque)
        if (input$arv != "Tous") df_filtered <- df_filtered %>% filter(type_arv == input$arv)
        if (input$proximite != "Tous") df_filtered <- df_filtered %>% filter(acces_clinique_proximite == input$proximite)
        df_filtered
    })
    ## Bouton de vérification des risques
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
                title = div("PVVIH à Risque Élevé", style = "font-weight:bold;font-size:18px;color:#c0392b;"),
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
    ## Téléchargements des listes à risque
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
    ## Boîtes d'indicateurs
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
    ## Graphiques
    output$agePlot <- renderPlotly({
        plot_ly(data_filtered(), x = ~age, type = "histogram") %>%
            layout(title = "Distribution des âges", xaxis = list(title = "Age"), yaxis = list(title = "Nombre"))
    })
    output$detectablePlot <- renderPlotly({
        plot_ly(data_filtered(), y = ~nb_fois_detectable, type = "box", color = ~niveau_risque) %>%
            layout(title = "Nombre de fois détectable par niveau de risque")
    })
    output$arvPlot <- renderPlotly({
        df_arv <- data_filtered() %>% group_by(type_arv) %>% summarise(N = n())
        plot_ly(df_arv, x = ~type_arv, y = ~N, type = "bar") %>%
            layout(title = "Nombre de patients par type d'ARV", xaxis = list(title = "Type d'ARV"), yaxis = list(title = "Nombre"))
    })
    output$riskPlot <- renderPlotly({
        plot_ly(data_filtered(), x = ~niveau_risque, y = ~duree_detectable_mois, type = "box", color = ~niveau_risque) %>%
            layout(title = "Charge virale détectable vs niveau de risque", xaxis = list(title = "Niveau de risque"), yaxis = list(title = "Durée détectable (mois)"))
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
    ## Envoi WhatsApp
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
    ## Animation bar chart race
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
        # enregistrer le gif dans un dossier temporaire interne
        if (!dir.exists("www")) dir.create("www")
        anim_save("www/bar_chart_race.gif", animation = anim)
        list(
            src = "www/bar_chart_race.gif",
            contentType = 'image/gif',
            width = 600,
            height = 400
        )
    }, deleteFile = FALSE)
}

## --- LANCEMENT DE L'APPLICATION ---
shinyApp(ui, server)