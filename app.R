## CarisCareTrack Extended
##
## This script defines a multi‑page Shiny dashboard for following up with persons
## living with HIV (PVVIH).  It extends the original application with
## additional programmatic features, including:
##   • A four‑page layout (overview, evaluation, form, chatbot) built
##     using shinydashboard.  Multiple pages let users navigate
##     between summary visualisations, detailed monitoring and evaluation,
##     data entry forms, and a conversational chatbot.
##   • A multi‑table data structure.  The underlying Excel file is
##     separated into a `beneficiaries` table (static patient information)
##     and a `visits` table (longitudinal follow‑up data).  These tables
##     can be joined on `patient_id` for analysis.
##   • A comprehensive data entry form with validation powered by the
##     shinyvalidate package【13839402855970†L30-L57】.  Separate tabs let
##     users create a new beneficiary record or record a new visit for an
##     existing patient.  Input validators ensure required fields are
##     filled in and provide immediate feedback.
##   • A simple chatbot interface based on shinychat【201093759941179†L19-L25】.  Users can
##     ask questions about the current data (e.g., “combien de patients
##     à risque élevé ?”) and receive summary statistics.  The chatbot
##     demonstrates how to build conversational interfaces without
##     external APIs.
##   • Notifications and alerts.  Users are notified when the number of
##     high‑risk patients exceeds a threshold or if required form
##     inputs are missing.  Alerts are delivered via showNotification
##     and modal dialogs.
##   • Secure authentication using the shinymanager package【795403320902766†L141-L170】.

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
library(shinyvalidate)
library(shinychat)
library(gganimate)
library(gifski)

## --- CHARGEMENT DES DONNÉES ---
## On lit les données de l'onglet « Sheet1 » du fichier Excel.  Le tableau
## complet est ensuite scindé en deux : un tableau des bénéficiaires
## (informations statiques) et un tableau des visites (informations
## longitudinales).
df <- readxl::read_excel("data_hiv.xlsx", sheet = "Sheet1")

## Construction du tableau des bénéficiaires : sélection des colonnes
## statiques et élimination des doublons.
beneficiaries_df <- df %>%
    select(patient_id, sexe, age, commune, region, type_arv,
           niveau_risque, acces_clinique_proximite, latitude, longitude) %>%
    distinct()

## Construction du tableau des visites : chaque ligne correspond à une
## visite médicale, avec la date, le nombre de visites, le nombre de
## fois détectable et la durée de détection virale.
visits_df <- df %>%
    select(patient_id, date_derniere_visite, nombre_visites_medicales,
           nb_fois_detectable, duree_detectable_mois)

## --- DÉFINITION DES UTILISATEURS ---
## La table `credentials` répertorie les utilisateurs autorisés et
## leurs mots de passe.  Le champ `admin` permet de distinguer un
## administrateur des utilisateurs classiques.  L'utilisation de
## `shinymanager` sécurise l'application comme recommandé dans la
## documentation【795403320902766†L141-L170】.
credentials <- data.frame(
    user = c("admin", "caris1234"),
    password = c("adminpass", "carishaiti1234"),
    admin = c(TRUE, FALSE),
    stringsAsFactors = FALSE
)

## --- INTERFACE UTILISATEUR ---
## On encapsule la page dans `secure_app()` afin de protéger l'accès.
ui <- secure_app(
    dashboardPage(
        skin = "purple",
        dashboardHeader(title = "CarisCareTrack"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Vue d'ensemble", tabName = "overview", icon = icon("dashboard")),
                menuItem("Suivi & Evaluation", tabName = "evaluation", icon = icon("chart-line")),
                menuItem("Formulaire", tabName = "form", icon = icon("edit")),
                menuItem("Chatbot", tabName = "chatbot", icon = icon("comments")),
                hr(),
                ## Filtres globaux s'appliquant à toutes les pages d'analyse
                selectInput("niveau_risque", "Filtrer par risque:",
                            choices = c("Tous", unique(df$niveau_risque)), selected = "Tous"),
                selectInput("arv", "Filtrer par ARV:",
                            choices = c("Tous", unique(df$type_arv)), selected = "Tous"),
                selectInput("proximite", "Proximité clinique:",
                            choices = c("Tous", unique(df$acces_clinique_proximite)), selected = "Tous"),
                selectInput("agent", "Choisir un agent de terrain:",
                            choices = c("Moise" = "+50940317880", "Masson" = "+50941743538",
                                        "Cassion" = "+50949163700"), selected = "+50940317880"),
                actionButton("verif_risque", "Verifier les risques PVVIH", class = "btn-danger"),
                actionButton("send_whatsapp", "Envoyer WhatsApp", class = "btn-success")
            )
        ),
        dashboardBody(
            shinyjs::useShinyjs(),
            tabItems(
                ## ---- PAGE : Vue d'ensemble ----
                tabItem(tabName = "overview",
                        fluidRow(
                            valueBoxOutput("totalBox"),
                            valueBoxOutput("risqueBox"),
                            valueBoxOutput("suiviBox")
                        ),
                        fluidRow(
                            box("Distribution des âges", status = "primary", solidHeader = TRUE,
                                plotlyOutput("agePlot"), width = 6),
                            box("Nombre de fois détectable", status = "danger", solidHeader = TRUE,
                                plotlyOutput("detectablePlot"), width = 6)
                        ),
                        fluidRow(
                            box("Type d'ARV", status = "success", solidHeader = TRUE,
                                plotlyOutput("arvPlot"), width = 6),
                            box("Charge virale vs Risque", status = "warning", solidHeader = TRUE,
                                plotlyOutput("riskPlot"), width = 6)
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
                ),
                ## ---- PAGE : Suivi & Evaluation ----
                tabItem(tabName = "evaluation",
                        fluidRow(
                            box(title = "Aperçu des indicateurs", width = 12, status = "primary",
                                DTOutput("evaluation_table"),
                                downloadButton("download_eval_excel", "Télécharger Excel", class = "btn-success")
                            )
                        ),
                        fluidRow(
                            box(title = "Tendance des visites détectables", width = 6, status = "info",
                                plotlyOutput("trendVisitsPlot")),
                            box(title = "Taux de risque par région", width = 6, status = "warning",
                                plotlyOutput("riskRegionPlot"))
                        )
                ),
                ## ---- PAGE : Formulaire ----
                tabItem(tabName = "form",
                        h2("Ajouter ou Mettre à jour des données"),
                        tabsetPanel(
                            tabPanel("Nouveau bénéficiaire",
                                     fluidRow(
                                         column(6,
                                                textInput("new_patient_id", "Identifiant du patient:"),
                                                selectInput("new_sexe", "Sexe:", choices = c("M", "F")),
                                                numericInput("new_age", "Age:", value = NA, min = 0, max = 120),
                                                textInput("new_commune", "Commune:"),
                                                textInput("new_region", "Région:"),
                                                selectInput("new_type_arv", "Type d'ARV:", choices = unique(df$type_arv)),
                                                selectInput("new_niveau_risque", "Niveau de risque:", choices = unique(df$niveau_risque)),
                                                selectInput("new_proximite", "Proximité clinique:", choices = unique(df$acces_clinique_proximite)),
                                                numericInput("new_latitude", "Latitude:", value = NA, step = 0.00001),
                                                numericInput("new_longitude", "Longitude:", value = NA, step = 0.00001),
                                                actionButton("submit_beneficiary", "Soumettre", class = "btn-primary")
                                         )
                                     ),
                                     verbatimTextOutput("beneficiary_message")
                            ),
                            tabPanel("Nouvelle visite",
                                     fluidRow(
                                         column(6,
                                                selectInput("visit_patient_id", "Sélectionner patient:", choices = beneficiaries_df$patient_id),
                                                dateInput("visit_date", "Date de la visite:", value = Sys.Date()),
                                                numericInput("visit_visites", "Nombre de visites médicales:", value = 1, min = 0),
                                                numericInput("visit_detectable", "Nombre de fois détectable:", value = 0, min = 0),
                                                numericInput("visit_duree", "Durée détectable (mois):", value = 0, min = 0),
                                                actionButton("submit_visit", "Enregistrer la visite", class = "btn-primary")
                                         )
                                     ),
                                     verbatimTextOutput("visit_message")
                            )
                        )
                ),
                ## ---- PAGE : Chatbot ----
                tabItem(tabName = "chatbot",
                        h2("Chatbot de suivi"),
                        chat_ui("pvchat", placeholder = "Posez une question..."),
                        p("Posez des questions sur les PVVIH, par exemple « Combien de patients à risque élevé ? » ou « Statistiques ». Le chatbot répondra avec les informations de votre sélection courante.")
                )
            )
        )
    )
)

## --- LOGIQUE SERVEUR ---
server <- function(input, output, session) {
    ## ---- Authentification ----
    ## L'appel à secure_server vérifie les identifiants stockés dans
    ## `credentials`.  Cette étape protège l'application complète tant que
    ## l'utilisateur n'est pas authentifié【795403320902766†L141-L170】.
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
    ## ---- Filtrage réactif ----
    ## On filtre le jeu de données en fonction des sélections effectuées dans la
    ## barre latérale (niveau de risque, type d'ARV, proximité).  Le filtrage
    ## est utilisé pour tous les graphiques et tableaux.
    data_filtered <- reactive({
        df_filtered <- df
        if (input$niveau_risque != "Tous") df_filtered <- df_filtered %>% filter(niveau_risque == input$niveau_risque)
        if (input$arv != "Tous") df_filtered <- df_filtered %>% filter(type_arv == input$arv)
        if (input$proximite != "Tous") df_filtered <- df_filtered %>% filter(acces_clinique_proximite == input$proximite)
        df_filtered
    })
    
    ## ---- Vérification des risques ----
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
    
    ## ---- Téléchargements (risques) ----
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
    
    ## ---- Boîtes de valeur (overview) ----
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
    
    ## ---- Graphiques (overview) ----
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
        if (!dir.exists("www")) dir.create("www")
        anim_save("www/bar_chart_race.gif", animation = anim)
        
        list(
            src = "www/bar_chart_race.gif",
            contentType = 'image/gif',
            width = 600,
            height = 400
        )
    }, deleteFile = FALSE)
    
    ## ---- Évaluation : table et graphiques ----
    output$evaluation_table <- renderDT({
        joined <- data_filtered() %>%
            left_join(visits_df, by = "patient_id") %>%
            select(patient_id, sexe, age, region, commune, niveau_risque, date_derniere_visite,
                   nombre_visites_medicales, nb_fois_detectable, duree_detectable_mois)
        datatable(joined, options = list(pageLength = 10, scrollX = TRUE))
    })
    output$download_eval_excel <- downloadHandler(
        filename = function() paste0("Donnees_evaluation_", Sys.Date(), ".xlsx"),
        content = function(file) {
            joined <- data_filtered() %>%
                left_join(visits_df, by = "patient_id") %>%
                select(patient_id, sexe, age, region, commune, niveau_risque, date_derniere_visite,
                       nombre_visites_medicales, nb_fois_detectable, duree_detectable_mois)
            writexl::write_xlsx(joined, path = file)
        }
    )
    output$trendVisitsPlot <- renderPlotly({
        trend_df <- df %>%
            mutate(month = floor_date(date_derniere_visite, "month")) %>%
            group_by(month) %>%
            summarise(visites = sum(nombre_visites_medicales, na.rm = TRUE))
        plot_ly(trend_df, x = ~month, y = ~visites, type = "scatter", mode = "lines+markers") %>%
            layout(title = "Tendance du nombre de visites médicales", xaxis = list(title = "Mois"), yaxis = list(title = "Nombre de visites"))
    })
    output$riskRegionPlot <- renderPlotly({
        risk_region <- df %>%
            group_by(region, niveau_risque) %>%
            summarise(n = n()) %>%
            group_by(region) %>%
            mutate(perc = n / sum(n) * 100)
        plot_ly(risk_region, x = ~region, y = ~perc, color = ~niveau_risque, type = "bar") %>%
            layout(title = "Proportion de risque par région", xaxis = list(title = "Région"), yaxis = list(title = "% de patients"))
    })
    
    ## ---- Validation des formulaires ----
    beneficiary_validator <- InputValidator$new()
    beneficiary_validator$add_rule("new_patient_id", sv_required())
    beneficiary_validator$add_rule("new_sexe", sv_required())
    beneficiary_validator$add_rule("new_age", sv_required())
    beneficiary_validator$add_rule("new_commune", sv_required())
    beneficiary_validator$add_rule("new_region", sv_required())
    beneficiary_validator$add_rule("new_type_arv", sv_required())
    beneficiary_validator$add_rule("new_niveau_risque", sv_required())
    beneficiary_validator$add_rule("new_proximite", sv_required())
    beneficiary_validator$enable()
    
    visit_validator <- InputValidator$new()
    visit_validator$add_rule("visit_patient_id", sv_required())
    visit_validator$add_rule("visit_date", sv_required())
    visit_validator$add_rule("visit_visites", sv_required())
    visit_validator$add_rule("visit_detectable", sv_required())
    visit_validator$add_rule("visit_duree", sv_required())
    visit_validator$enable()
    
    ## ---- Soumission d'un nouveau bénéficiaire ----
    observeEvent(input$submit_beneficiary, {
        if (!beneficiary_validator$is_valid()) {
            showNotification("Veuillez remplir tous les champs requis du bénéficiaire.", type = "error")
            return()
        }
        new_row <- data.frame(
            patient_id = input$new_patient_id,
            sexe = input$new_sexe,
            age = input$new_age,
            commune = input$new_commune,
            region = input$new_region,
            type_arv = input$new_type_arv,
            niveau_risque = input$new_niveau_risque,
            acces_clinique_proximite = input$new_proximite,
            latitude = input$new_latitude,
            longitude = input$new_longitude,
            stringsAsFactors = FALSE
        )
        beneficiaries_df <<- bind_rows(beneficiaries_df, new_row)
        output$beneficiary_message <- renderText("Nouveau bénéficiaire ajouté avec succès.")
        showNotification("Bénéficiaire ajouté.", type = "message")
        updateTextInput(session, "new_patient_id", value = "")
        updateNumericInput(session, "new_age", value = NA)
        updateTextInput(session, "new_commune", value = "")
        updateTextInput(session, "new_region", value = "")
        updateNumericInput(session, "new_latitude", value = NA)
        updateNumericInput(session, "new_longitude", value = NA)
        ## Mettre à jour la liste des patients dans le formulaire de visite
        updateSelectInput(session, "visit_patient_id", choices = beneficiaries_df$patient_id)
    })
    
    ## ---- Soumission d'une nouvelle visite ----
    observeEvent(input$submit_visit, {
        if (!visit_validator$is_valid()) {
            showNotification("Veuillez remplir tous les champs requis de la visite.", type = "error")
            return()
        }
        visit_row <- data.frame(
            patient_id = input$visit_patient_id,
            date_derniere_visite = input$visit_date,
            nombre_visites_medicales = input$visit_visites,
            nb_fois_detectable = input$visit_detectable,
            duree_detectable_mois = input$visit_duree,
            stringsAsFactors = FALSE
        )
        visits_df <<- bind_rows(visits_df, visit_row)
        output$visit_message <- renderText("Nouvelle visite enregistrée avec succès.")
        showNotification("Visite enregistrée.", type = "message")
        updateDateInput(session, "visit_date", value = Sys.Date())
        updateNumericInput(session, "visit_visites", value = 1)
        updateNumericInput(session, "visit_detectable", value = 0)
        updateNumericInput(session, "visit_duree", value = 0)
    })
    
    ## ---- Chatbot conversationnel ----
    observeEvent(input$pvchat_user_input, {
        req(input$pvchat_user_input)
        user_message <- input$pvchat_user_input
        msg_lower <- tolower(user_message)
        ## Répondre selon des mots clefs simples
        if (grepl("risque", msg_lower)) {
            high_risk_count <- nrow(data_filtered() %>% filter(niveau_risque == "Élevé"))
            response <- paste0("Actuellement, il y a ", high_risk_count, " patient(s) à risque élevé selon vos filtres.")
        } else if (grepl("stat", msg_lower)) {
            total <- nrow(data_filtered())
            high_risk <- nrow(data_filtered() %>% filter(niveau_risque == "Élevé"))
            response <- paste0("Statistiques: Total patients = ", total,
                               ", à risque élevé = ", high_risk,
                               " (", round(high_risk / total * 100, 1), "%).")
        } else if (grepl("bonjour|salut|hello", msg_lower)) {
            response <- "Bonjour! Posez-moi des questions sur les PVVIH pour recevoir des informations."
        } else {
            response <- "Je suis désolé, je ne comprends pas. Essayez de poser des questions sur le risque ou les statistiques."
        }
        chat_append("pvchat", list(role = "assistant", content = response))
    })
    
    ## Message de bienvenue dans le chatbot
    observe({
        chat_clear("pvchat")
        chat_append("pvchat", list(role = "assistant", content = "Bienvenue dans le chatbot CarisCareTrack. Posez une question pour commencer."))
    })
    
    ## ---- Intégration WhatsApp ----
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
    
    ## ---- Alertes ----
    observe({
        if (nrow(data_filtered() %>% filter(niveau_risque == "Élevé")) > 10) {
            showNotification("Attention: plus de 10 patients à risque élevé selon vos filtres!", type = "warning", duration = 10)
        }
    })
}

## ---- Lancement de l'application ----
shinyApp(ui, server)