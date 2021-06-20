

# library(DT)
# library(magrittr) # needs to be run every time you start R and want to use %>%
# library(rvest)
# library(dplyr)
# library(idmodelr)
# library(xml2)
# library(readr)
# library(shiny)
# library(plotly)
# library(shinydashboard)
# library(shinydashboardPlus)
# library(shinyWidgets)
# library(quantmod)
# library(ggplot2)
# library(leaflet)
library(highcharter)

rm(list = ls())
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("purrr"))
suppressPackageStartupMessages(library("highcharter"))


# # list all packages where an update is available
# old.packages()
# 
# # update all available packages
# update.packages()
# 
# # update, without prompts for permission/clarification
# update.packages(ask = FALSE)
# 

## import data

## data on patients
client<-read.csv2("clients.csv")

AGE<-client$AGE..ANS.
COMMUNE<-client$COMMUNE
GENRE<-client$GENRE
AFFECTION<-client$TYPE.D.AFFECTION
PROFESSION<-client$PROFESSION
CENTRE<-client$CENTRE.D.APPARTENANCE
PATIENTS<-client$A..CLIENTS
ACCUEIL<-client$ACCUEIL
DELAI<-client$DELAI.DE.RDV
QUALITE<-client$QUALITE.DES.SOINS.RECUS

## data on center
centre<-read.csv2("centre.csv")

NOM_CENTRE<-centre$NOM.DES.CENTRES
PATIENTS_RECUS<-centre$TOTAL..PATIENTS.RECUS.jours.
PATIENTS_CONS<-centre$TOTAL..PATIENTS.RECUS.EN.CONSULTATION
PATIENTS_MONTURES<-centre$TOTAL..PATIENTS.POUR.ACHAT.DE.MONTURE
PATIENTS_VERRES<-centre$TOTAL.PATIENTS.ACHAT.DE.VERRES
PATIENTS_TS<-centre$TOTAL..PATIENTS..TOUS.SERVICES
REFRACTOMETRE<-centre$TOTAL.EQUIPEMENTS.EN.REFRACTOMETRE
TONOMETRE<-centre$TOTAL.EQUIPEMENTS.EN.TONOMETRE.A.AIR
REFRACTEUR<-centre$TOTAL.EQUIPEMENTS.EN.REFRACTEUR.A.ESSAI.DE.VERRES
LAMPE_A_FENTE<-centre$TOTAL.EQUIPEMENTS.EN.LAMPE.A.FENTE
OCT<-centre$TOTAL.EQUIPEMENTS.EN.OCT
MONTURES<-centre$TOTAL..MONTURES.RECUES
VERRES<-centre$TOTAL..VERRES.RECUS

GRAPH<-read.csv2("camembert.csv")
atteint=GRAPH$A..ATTEINTE
effectif=GRAPH$EFFECTIF

# Define UI for application that draws 

ui<-dashboardPage(skin = "blue",
                  dashboardHeader(title = "My Dashboard"),
                  dashboardSidebar(
                      sidebarMenu(
                          menuItem("Centre", tabName = "centre", icon = icon("dashboard")),
                          menuItem("Localisation des patients", tabName = "carte", icon = icon("atlas")
                          ),
                          menuItem("Visualisations", tabName = "patient", icon = icon("spinner")
                          ))),
                  dashboardBody(
                      tabItems(
                          tabItem(## centre
                              tabName = "centre",
                              h2("Tableaux de bord"),
                              fluidRow(
                                  infoBox(
                                      "Moyenne des patients reçus par jour","75","Patients", icon = icon("dashboard"),
                                      color = "yellow"),
                                  infoBox("Moyenne des patients attendus par jour","120","Patients",icon = icon("dashboard"),
                                      color = "navy")),
                              fluidRow(
                                  box(title = "",
                                      solidHeader = T,
                                      width = 4,
                                      collapsible = T,
                                      div(highchartOutput("coc"))),
                                  box(title = "",
                                      solidHeader = T,
                                      width = 4,
                                      collapsible = T,
                                      div(highchartOutput("yop"))),
                                  box(title = "", solidHeader = T,
                                      width = 4, collapsible = T,
                                      highchartOutput("actesci")), 
                                  # row
                                  fluidRow(
                                      box(title = "patients",
                                          solidHeader = T,
                                          width = 6, 
                                          collapsible = T,
                                          collapsed = F,
                                          tags$p(highchartOutput("v3"))),
                                      box(title = "table", solidHeader = T,
                                          width = 6, collapsible = T,
                                          highchartOutput("v4"))),
                                  fluidRow(
                                      box(title = "Patients reçus par mois selon l'âge", solidHeader = T, width = 12,
                                          collapsible = T, 
                                          highchartOutput("v5")))
                                  # row
                                  )),
                          tabItem(##localisation des patients
                              tabName = "carte",
                              h2("Carte de localisation des patients"),
                              fluidRow(
                                  box(title = "carte", width = 12, solidHeader = F, collapsible = F,
                                      status = "primary", height = 1000,
                                      leafletOutput("carte", width = "100%", height = 1000)))),
                          tabItem(## patients
                              tabName = "patient",
                              h2("Patients"),
                              fluidRow(
                                  box(title = "Types d'affection des patients",
                                      solidHeader = T,
                                      width = 6,
                                      collapsible = T,
                                      div(highchartOutput("v6"))),
                                  box(title = "Type d'affection selon le centre", solidHeader=T,
                                      width = 6, collapsible = T,
                                      highchartOutput("type"))),
                              fluidRow(
                                  box(title = "Types de profession des patients", solidHeader = T,
                                      width = 12, collapsible = T,
                                      highchartOutput("v7"))),
                              fluidRow(
                                  box(title = "ACCUEIL DES PATIENTS SELON LE DELAI DE RDV", solidHeader=T,
                                      width = 6, collapsible = T,
                                      highchartOutput("ac")),
                                  box(title = "ACCUEIL DES PATIENTS SELON LA QUALITE DES SOINS", solidHeader = T,
                                      width = 6, collapsible = T, 
                                      highchartOutput("qs"))),
                               fluidRow(
                                  box(title = "Top 5 des types d'affections_COCODY", solidHeader = T, width = 4, collapsible = T,
                                      highchartOutput("cocody")),
                                  box(title = "Top 5 des types d'affections_YOPOUGON", solidHeader = T, width = 4, collapsible = T,
                                      highchartOutput("yopougon")),
                                  box(title = "Top 5 des types d'affections_ACTESCI", solidHeader = T, width = 4, collapsible = T,
                                      highchartOutput("Tout")))))))

server=function(input, output){
    long<-client$LONGITUDE
    latit<-client$LATITUDE
    couleur<-colorFactor(c("#0000FFFF", "#FF00FFFF"), domain=CENTRE)
    new_theme = theme(panel.background = element_blank(),
                      axis.line.x   = element_line(color='black'),
                      axis.line.y   = element_line(color='black'),
                      axis.ticks    = element_line(color='black'),
                      axis.title.x  = element_text(family="Times",face = "bold", size = 12),
                      axis.title.y  = element_text(family="Times",face = "bold", size = 12),
                      axis.text     = element_text(family="Trebuchet MS",face = "italic", size = 10),
                      legend.title  = element_text(family="Times",face = "bold", size = 8),
                      legend.text   = element_text(family="Trebuchet MS",face = "italic", size = 8))
    ## coté de la carte
    output$carte=renderLeaflet({
        leaflet(data = client)%>%
            addTiles()%>%
            addCircles(lng = ~long, lat = ~latit,
                       radius = 40, weight = 5,
                       popup = ~paste(CENTRE,":","<br></br>",PATIENTS, "<br></br>",PROFESSION,"<br></br>",AFFECTION,
                                      "<br></br>",AGE, "<br></br>",GENRE),
                       color = ~couleur(CENTRE), fillOpacity = 10)%>%
            addMarkers(
                c(-3.998356, -4.074356),
                c(5.348152, 5.347364),
                popup = c("ActesCi Cocody", "ActesCi Yopougon"))%>%
            addLegend(title = "Centre d'appartenance", position = "bottomright",
                      pal = couleur, values = CENTRE, labels = CENTRE)})
    ### coté des patients
    output$v6=renderHighchart({
        df <- data_frame(
            name = c("MYOPIE", "CATARACTE", "GLAUCOME","HYPERMETROPIE","PRESBYTIE","CECITE",
                     "DMLA","SYNDROME DE USHER","APHAKIE","RETINITE PIGMENTAIRE"),
            y = c(25,24,13,7,15,1,6,6,2,1),
            drilldown = tolower(name))
        df
        hc <- highchart() %>%
            hc_chart(type = "column") %>%
            hc_title(text = "Type d'affection des patients") %>%
            hc_xAxis(type = "category") %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(
                    boderWidth = 0,
                    dataLabels = list(enabled = TRUE))) %>%
            hc_add_series(
                data = list_parse(df),
                name = "Patients",
                colorByPoint = T)
        dfMYO <- data_frame(
            name = c("HOMME", "FEMME"),
            value = c(14,11))
        
        dfCATA <- data_frame(
            name = c("HOMME", "FEMME"),
            value = c(13,11))
        dfGLAU <- data_frame(
            name = c("HOMME", "FEMME"),
            value = c(5,8))
        dfHYP <- data_frame(
            name = c("HOMME", "FEMME"),
            value = c(4,3))
        dfPRES <- data_frame(
            name = c("HOMME", "FEMME"),
            value = c(8,7))
        dfCECI <- data_frame(
            name = c("HOMME", "FEMME"),
            value = c(0, 1))
        dfDMLA <- data_frame(
            name = c("HOMME", "FEMME"),
            value = c(3,3))
        dfSYN <- data_frame(
            name = c("HOMME", "FEMME"),
            value = c(2,4))
        dfAPHA <- data_frame(
            name = c("HOMME", "FEMME"),
            value = c(1,1))
        dfRET <- data_frame(
            name = c("HOMME", "FEMME"),
            value = c(0, 1))
        hc <- hc %>%
            hc_drilldown(
                allowPointDrilldown = TRUE,
                series = list(
                    list(
                        id = "myopie",
                        data = list_parse2(dfMYO)),
                    list(
                        id = "cataracte",
                        data = list_parse2(dfCATA)),
                    list(
                        id = "glaucome",
                        data = list_parse2(dfGLAU)),
                    list(
                        id = "hypermetropie",
                        data = list_parse2(dfHYP)),
                    list(
                        id = "presbytie",
                        data = list_parse2(dfPRES)),
                    list(
                        id = "cecite",
                        data = list_parse2(dfCECI)),
                    list(
                        id = "dmla",
                        data = list_parse2(dfDMLA)),
                    list(
                        id = "syndrome de usher",
                        data = list_parse2(dfSYN)),
                    list(
                        id = "aphakie",
                        data = list_parse2(dfAPHA)),
                    list(
                        id = "retinite pigmentaire",
                        data = list_parse2(dfRET))))
        hc
    })
    output$v7=renderHighchart({
        dm <- data_frame(
            name = c("CHEF D'ENTREPRISE", "COMMERCANT", "ELEVE","ETUDIANT","ENSEIGNANT"),
            y = c(17,15,16,22,30),
            drilldown = tolower(name))
        dm
        hc <- highchart() %>%
            hc_chart(type = "column") %>%
            hc_title(text = "TYPE DE PROFESSION DES PATIENTS") %>%
            hc_xAxis(type = "category") %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(
                    boderWidth = 0,
                    dataLabels = list(enabled = TRUE))) %>%
            hc_add_series(
                data = list_parse(dm),
                name = "PROFESSION DES PATIENTS",
                colorByPoint = T)
        dmCE <- data_frame(
            name = c("APHAKIE", "CATARACTE","DMLA","GLAUCOME","MYOPIE","SYNDROME DE USHER","RETINITE PIGMENTAIRE","CECITE","HYPERMETROPIE","PRESBYTIE"),
            value = c(2,5,1,3,4,1,1,0,0,0))
        dmCOM <- data_frame(
            name = c("PRESBYTIE", "CATARACTE","HYPERMETROPIE","GLAUCOME","MYOPIE","APHAKIE","CECITE","DMLA","RETINITE PIGMENTAIRE","SYNDROME DE USHER"),
            value = c(1,6,3,1,4,0,0,0,0,0))
        dmELE <- data_frame(
            name = c("MYOPIE", "PRESBYTIE","DMLA","RETINITE PIGMENTAIRE","HYPERMETROPIE","GLAUCOME","SYNDROME DE USHER","CECITE","APHAKIE","CATARACTE"),
            value = c(11,5,0,0,0,0,0,0,0,0))
        dmETU <- data_frame(
            name = c("CATARACTE","DMLA","GLAUCOME","MYOPIE","HYPERMETROPIE","PRESBYTIE","SYNDROME DE USHER","RETINITE PIGMENTAIRE","APHAKIE","CECITE"),
            value = c(5,4,4,5,1,3,0,0,0,0))
        dmENS <- data_frame(
            name = c("CECITE", "CATARACTE","DMLA","GLAUCOME","MYOPIE","SYNDROME DE USHER","HYPERMETROPIE","PRESBYTIE","APHAKIE","RETINITE PIGMENTAIRE"),
            value = c(1,8,1,5,1,5,3,6,0,0))
        hc <- hc %>%
            hc_drilldown(
                allowPointDrilldown = TRUE,
                series = list(
                    list(
                        id = "chef d'entreprise",
                        data = list_parse2(dmCE)),
                    list(
                        id = "commercant",
                        data = list_parse2(dmCOM)),
                    list(
                        id = "eleve",
                        data = list_parse2(dmELE)),
                    list(
                        id = "etudiant",
                        data = list_parse2(dmETU)),
                    list(
                        id = "enseignant",
                        data = list_parse2(dmENS))))
        hc
    })
    output$type=renderHighchart({
        dc <- data_frame(
            name = c("CENTRE_YOPOUGON","CENTRE_COCODY"),
            y = c(37,63),
            drilldown = tolower(name))
        dc
        hc <- highchart() %>%
            hc_chart(type = "column") %>%
            hc_title(text = "CENTRE") %>%
            hc_xAxis(type = "category") %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(
                    boderWidth = 0,
                    dataLabels = list(enabled = TRUE))) %>%
            hc_add_series(
                data = list_parse(dc),
                name = "Patients au total",
                colorByPoint = T)
        dcCY <- data_frame(
            name = c("APHAKIE", "CATARACTE","DMLA","GLAUCOME","MYOPIE","SYNDROME DE USHER","RETINITE PIGMENTAIRE","CECITE","HYPERMETROPIE","PRESBYTIE"),
            value = c(1,8,4,1,14,3,0,1,0,5))
        dcCC <- data_frame(
            name = c("PRESBYTIE", "CATARACTE","HYPERMETROPIE","GLAUCOME","MYOPIE","APHAKIE","CECITE","DMLA","RETINITE PIGMENTAIRE","SYNDROME DE USHER"),
            value = c(10,16,7,12,11,1,0,2,1,3))
        hc <- hc %>%
            hc_drilldown(
                allowPointDrilldown = TRUE,
                series = list(
                    list(
                        id = "centre_yopougon",
                        data = list_parse2(dcCY)),
                    list(
                        id = "centre_cocody",
                        data = list_parse2(dcCC))))
        hc
    })
    output$yopougon<-renderHighchart({
        dyopo <- data.frame(
            x = c(0, 1, 2, 3, 4),
            y = c(8, 14, 4, 3, 5),
            name = as.factor(c("Cataracte", "Myopie", "Presbytie", "DMLA", "Syndrome de USHER")))
        dyopo
        hc <- dyopo %>%
            hchart(
                "pie", hcaes(x = name, y = y),
                name = "Nombre d'affectés")
        hc
    })
    output$cocody<-renderHighchart({
        dT <- data.frame(
            x = c(0, 1, 2, 3, 4),
            y = c(16, 11, 10, 12, 7),
            name = as.factor(c("Cataracte", "Myopie", "Presbytie", "Glaucome", "Hypermétropie")))
        dT
        hc <- dT %>%
            hchart(
                "pie", hcaes(x = name, y = y),
                name = "Nombre d'affectés")
        hc
    })
    output$Tout<-renderHighchart({
        dCOCO <- data.frame(
            x = c(0, 1, 2, 3, 4),
            y = c(22, 25, 14, 13, 7),
            name = as.factor(c("Cataracte", "Myopie", "Presbytie", "Glaucome", "Hypermétropie")))
        dCOCO
        hc <- dCOCO %>%
            hchart(
                "pie", hcaes(x = name, y = y),
                name = "Nombre d'affectés")
        hc
    })
    ## coté des centres
    output$coc=renderHighchart({
        dp <- data_frame(
            name = "Centre_cocody",
            y = 630,
            drilldown = tolower(name))
        dp
        hc <- highchart() %>%
            hc_chart(type = "pie") %>%
            hc_title(text = "Total patients de ActesCi Cocody par mois") %>%
            hc_xAxis(type = "category") %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(
                    boderWidth = 0,
                    dataLabels = list(enabled = TRUE))) %>%
            hc_add_series(
                data = list_parse(dp),
                name = "Patients",
                colorByPoint = T)
        dpco <- data_frame(
            name = c("TOTAL_PATIENTS_RECUS_EN_CONSULTATION", "TOTAL_PATIENTS_POUR_ACHAT_DE_MONTURE","TOTAL_PATIENTS_ACHAT_DE_VERRES","TOTAL_PATIENTS_TOUS_SERVICES"),
            value = c(300,100,120,110))
        hc <- hc %>%
            hc_drilldown(
                allowPointDrilldown = TRUE,
                series = list(
                    list(
                        id = "centre_cocody",
                        data = list_parse2(dpco))))
        hc
    })
    output$yop=renderHighchart({
        dy <- data_frame(
            name = "Centre_yopougon",
            y = 370,
            drilldown = tolower(name))
        dy
        hc <- highchart() %>%
            hc_chart(type = "pie") %>%
            hc_title(text = "Total patients de ActesCi Yopougon par mois") %>%
            hc_xAxis(type = "category") %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(
                    boderWidth = 0,
                    dataLabels = list(enabled = TRUE))) %>%
            hc_add_series(
                data = list_parse(dy),
                name = "Patients",
                colorByPoint = T)
        dpyop <- data_frame(
            name = c("TOTAL_PATIENTS_RECUS_EN_CONSULTATION", "TOTAL_PATIENTS_POUR_ACHAT_DE_MONTURE","TOTAL_PATIENTS_ACHAT_DE_VERRES","TOTAL_PATIENTS_TOUS_SERVICES"),
            value = c(100,120,100,50))
        hc <- hc %>%
            hc_drilldown(
                allowPointDrilldown = TRUE,
                series = list(
                    list(
                        id = "centre_yopougon",
                        data = list_parse2(dpyop))))
        hc
    })
    output$actesci=renderHighchart({
        dA <- data_frame(
            name = "ACTESCI",
            y = 1000,
            drilldown = tolower(name))
        dA
        hc <- highchart() %>%
            hc_chart(type = "pie") %>%
            hc_title(text = "Total patients de ActesCi par mois") %>%
            hc_xAxis(type = "category") %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(
                    boderWidth = 0,
                    dataLabels = list(enabled = TRUE))) %>%
            hc_add_series(
                data = list_parse(dA),
                name = "Patients",
                colorByPoint = T)
        dpA <- data_frame(
            name = c("TOTAL_PATIENTS_RECUS_EN_CONSULTATION", "TOTAL_PATIENTS_POUR_ACHAT_DE_MONTURE","TOTAL_PATIENTS_ACHAT_DE_VERRES","TOTAL_PATIENTS_TOUS_SERVICES"),
            value = c(400,220,220,160))
        hc <- hc %>%
            hc_drilldown(
                allowPointDrilldown = TRUE,
                series = list(
                    list(
                        id = "actesci",
                        data = list_parse2(dpA))))
        hc
    })
    output$ac<-renderHighchart({
        dac <- data_frame(
            name = c("MAUVAIS_ACCUEIL","ACCUEIL_MOYEN","BON_ACCUEIL","TRES_BON_ACCUEIL","EXCELLENT_ACCUEIL"),
            y = c(20,20,20,30,10),
            drilldown = tolower(name))
        dac
        hc <- highchart() %>%
            hc_chart(type = "pie") %>%
            hc_title(text = "ACCUEIL DES PATIENTS") %>%
            hc_xAxis(type = "category") %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(
                    boderWidth = 0,
                    dataLabels = list(enabled = TRUE))) %>%
            hc_add_series(
                data = list_parse(dac),
                name = "ACCUEIL DES PATIENTS",
                colorByPoint = T)
        dhma <- data_frame(
            name = c("MAUVAIS", "MOYEN","BON","TRES BON","EXCELLENT"),
            value = c(2,6,10,2,0))
        dhmoy <- data_frame(
            name = c("MAUVAIS", "MOYEN","BON","TRES BON","EXCELLENT"),
            value = c(15,13,10,2,0))
        dhbon <- data_frame(
            name = c("MAUVAIS", "MOYEN","BON","TRES BON","EXCELLENT"),
            value = c(2,4,12,2,0))
        dhtbon <- data_frame(
            name = c("MAUVAIS", "MOYEN","BON","TRES BON","EXCELLENT"),
            value = c(3,6,18,3,0))
        dhexc <- data_frame(
            name = c("MAUVAIS", "MOYEN","BON","TRES BON","EXCELLENT"),
            value = c(1,2,5,2,0))
        hc <- hc %>%
            hc_drilldown(
                allowPointDrilldown = TRUE,
                series = list(
                    list(
                        id = "mauvais_accueil",
                        data = list_parse2(dhma)),
                    list(
                        id = "accueil_moyen",
                        data = list_parse2(dhmoy)),
                    list(
                        id = "bon_accueil",
                        data = list_parse2(dhbon)),
                    list(
                        id = "tres_bon_accueil",
                        data = list_parse2(dhtbon)),
                    list(
                        id = "excellent_accueil",
                        data = list_parse2(dhexc))))
        hc
    })
    output$qs<-renderHighchart({
        dqs <- data_frame(
            name = c("MAUVAIS_ACCUEIL","ACCUEIL_MOYEN","BON_ACCUEIL","TRES_BON_ACCUEIL","EXCELLENT_ACCUEIL"),
            y = c(20,20,20,30,10),
            drilldown = tolower(name))
        dqs
        hc <- highchart() %>%
            hc_chart(type = "pie") %>%
            hc_title(text = "QUALITE DES SOINS") %>%
            hc_xAxis(type = "category") %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(
                    boderWidth = 0,
                    dataLabels = list(enabled = TRUE))) %>%
            hc_add_series(
                data = list_parse(dqs),
                name = "QUALITE DES SOINS",
                colorByPoint = T)
        dqsma <- data_frame(
            name = c("MAUVAIS", "MOYEN","BON","TRES BON","EXCELLENT"),
            value = c(0,8,6,6,0))
        dqsmoy <- data_frame(
            name = c("MAUVAIS", "MOYEN","BON","TRES BON","EXCELLENT"),
            value = c(0,3,6,8,3))
        dqsbon <- data_frame(
            name = c("MAUVAIS", "MOYEN","BON","TRES BON","EXCELLENT"),
            value = c(0,3,7,7,3))
        dqstbon <- data_frame(
            name = c("MAUVAIS", "MOYEN","BON","TRES BON","EXCELLENT"),
            value = c(0,3,10,10,7))
        dqsexc <- data_frame(
            name = c("MAUVAIS", "MOYEN","BON","TRES BON","EXCELLENT"),
            value = c(0,0,4,3,3))
        hc <- hc %>%
            hc_drilldown(
                allowPointDrilldown = TRUE,
                series = list(
                    list(
                        id = "mauvais_accueil",
                        data = list_parse2(dqsma)),
                    list(
                        id = "accueil_moyen",
                        data = list_parse2(dqsmoy)),
                    list(
                        id = "bon_accueil",
                        data = list_parse2(dqsbon)),
                    list(
                        id = "tres_bon_accueil",
                        data = list_parse2(dqstbon)),
                    list(
                        id = "excellent_accueil",
                        data = list_parse2(dqsexc))))
        hc
    })
    output$v3<-renderHighchart({
        dv <- data_frame(
            name = c("Centre_cocody", "Centre_yopougon"),
            y = c(6,5),
            drilldown = tolower(name))
        dv
        hc <- highchart() %>%
            hc_chart(type = "column") %>%
            hc_title(text = "Equipements 1") %>%
            hc_xAxis(type = "category") %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(
                    boderWidth = 0,
                    dataLabels = list(enabled = TRUE))) %>%
            hc_add_series(
                data = list_parse(dv),
                name = "Equipements 1",
                colorByPoint = T)
        dv3c <- data_frame(
            name = c("TOTAL EQUIPEMENTS EN REFRACTOMETRE", "TOTAL EQUIPEMENTS EN TONOMETRE A AIR", "TOTAL EQUIPEMENTS EN OCT"),
            value = c(2,2,2))
        dv3y <- data_frame(
            name = c("TOTAL EQUIPEMENTS EN REFRACTOMETRE", "TOTAL EQUIPEMENTS EN TONOMETRE A AIR", "TOTAL EQUIPEMENTS EN OCT"),
            value = c(3,1,1))
        hc <- hc %>%
            hc_drilldown(
                allowPointDrilldown = TRUE,
                series = list(
                    list(
                        id = "centre_cocody",
                        data = list_parse2(dv3c)),
                    list(
                        id = "centre_yopougon",
                        data = list_parse2(dv3c))))
        hc
    })
    output$v4<-renderHighchart({
        dh <- data_frame(
            name = c("Centre_cocody", "Centre_yopougon"),
            y = c(385,500),
            drilldown = tolower(name))
        dh
        hc <- highchart() %>%
            hc_chart(type = "column") %>%
            hc_title(text = "Equipements 2") %>%
            hc_xAxis(type = "category") %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(
                    boderWidth = 0,
                    dataLabels = list(enabled = TRUE))) %>%
            hc_add_series(
                data = list_parse(dh),
                name = "Equipements 2",
                colorByPoint = T)
        dhc <- data_frame(
            name = c("TOTAL EQUIPEMENTS EN REFRACTEUR A ESSAI DE VERRES", "TOTAL EQUIPEMENTS EN LAMPE A FENTE", "TOTAL  MONTURES RECUES", "TOTAL  VERRES RECUS"),
            value = c(20,15,200,150))
        dhy <- data_frame(
            name = c("TOTAL EQUIPEMENTS EN REFRACTEUR A ESSAI DE VERRES", "TOTAL EQUIPEMENTS EN LAMPE A FENTE", "TOTAL  MONTURES RECUES", "TOTAL  VERRES RECUS"),
            value = c(30,20,250,200))
        hc <- hc %>%
            hc_drilldown(
                allowPointDrilldown = TRUE,
                series = list(
                    list(
                        id = "centre_cocody",
                        data = list_parse2(dhc)),
                    list(
                        id = "centre_yopougon",
                        data = list_parse2(dhy))))
        hc
    })
    output$v5<-renderHighchart({
        d <- data_frame(
            name = c("TOTAL_PATIENTS_RECUS_EN_CONSULTATION", "TOTAL_PATIENTS_POUR_ACHAT_DE_MONTURE","TOTAL_PATIENTS_ACHAT_DE_VERRES","TOTAL_PATIENTS_TOUS_SERVICES"),
            y = c(600,250,166,200),
            drilldown = tolower(name))
        d
        hc <- highchart() %>%
            hc_chart(type = "column") %>%
            hc_title(text = "Patients") %>%
            hc_xAxis(type = "category") %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(
                    boderWidth = 0,
                    dataLabels = list(enabled = TRUE))) %>%
            hc_add_series(
                data = list_parse(d),
                name = "Patients reçus par services selon l'âge",
                colorByPoint = T)
        d1 <- data_frame(
            name = c("8-13", "14-18","19-25","26-31","32-37","38-45","46-53","54-60","61-66"),
            value = c(50,100,92,60,15,22,66,120,75))
        d2 <- data_frame(
            name = c("8-13", "14-18","19-25","26-31","32-37","38-45","46-53","54-60","61-66"),
            value = c(18,22,63,45,20,15,30,12,25))
        d3 <- data_frame(
            name = c("8-13", "14-18","19-25","26-31","32-37","38-45","46-53","54-60","61-66"),
            value = c(8,20,15,13,19,16,30,40,5))
        d4 <- data_frame(
            name = c("8-13", "14-18","19-25","26-31","32-37","38-45","46-53","54-60","61-66"),
            value = c(20,25,29,16,10,8,22,30,40))
        hc <- hc %>%
            hc_drilldown(
                allowPointDrilldown = TRUE,
                series = list(
                    list(
                        id = "total_patients_recus_en_consultation",
                        data = list_parse2(d1)),
                    list(
                        id = "total_patients_pour_achat_de_monture",
                        data = list_parse2(d2)),
                    list(
                        id = "total_patients_achat_de_verres",
                        data = list_parse2(d3)),
                    list(
                        id = "total_patients_tous_services",
                        data = list_parse2(d4))))
        hc
    })
}

shinyApp(ui=ui, server=server)