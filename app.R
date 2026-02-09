library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(scales)
library(readr)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Analyse des délits par département"),
  
  sidebarLayout(
    sidebarPanel(
      # Sélection du type de graphique
      radioButtons(
        "type_graphique",
        "Type de graphique :",
        choices = c(
          "Profils détaillés" = "facettes",
          "Graphique empilé" = "empile"
        ),
        selected = "facettes"
      ),
      
      hr(),
      
      # Sélection du nombre de départements
      sliderInput(
        "nb_depts",
        "Nombre de départements à afficher :",
        min = 1,
        max = 3,
        value = 3,
        step = 1
      ),
      
      hr(),
      
      # Zone pour les menus déroulants des départements
      uiOutput("selecteurs_departements"),
      
      hr(),
      
      # Sélection des catégories (uniquement pour graphique en facettes)
      conditionalPanel(
        condition = "input.type_graphique == 'facettes'",
        checkboxGroupInput(
          "categories",
          "Catégories de délits :",
          choices = c(
            "Atteintes aux personnes",
            "Atteintes aux biens",
            "Éco-financier",
            "Stupéfiants",
            "Étrangers et séjour",
            "Administratif, professionnel et réglementaire",
            "Ordre public, autorité et sûreté",
            "Faux et usage de faux"
          ),
          selected = c(
            "Atteintes aux personnes",
            "Atteintes aux biens",
            "Éco-financier",
            "Stupéfiants",
            "Étrangers et séjour",
            "Administratif, professionnel et réglementaire",
            "Ordre public, autorité et sûreté",
            "Faux et usage de faux"
          )
        )
      ),
      
      hr(),
      
      # Bouton pour actualiser
      actionButton("actualiser", "Actualiser le graphique", 
                   class = "btn-primary", width = "100%")
    ),
    
    mainPanel(
      plotOutput("graphique", height = "800px")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Chargement et préparation des données
  donnees_preparees <- reactive({
    # Lecture du fichier
    delits <- read_csv2("delitDep.csv", na = "NA", locale = locale(encoding = "latin1"))
    
    # Création des macrocatégories
    macrocat <- delits %>%
      mutate(
        'Atteintes aux personnes' = rowSums(across(contains("Homicides") |
                                                     contains("Tentatives_homicides") |
                                                     contains("Coups_et_blessures") |
                                                     contains("Autres_coups_et_blessures") |
                                                     contains("Violences__mauvais_traitements") |
                                                     contains("Violences_autorité") |
                                                     contains("Atteintes_dignité") |
                                                     contains("Atteintes_sex") |
                                                     contains("Viols") |
                                                     contains("Harc") |
                                                     contains("Seques") |
                                                     contains("Prises_d_otages") |
                                                     contains("Règlements_compte")), na.rm = TRUE),
        
        'Atteintes aux biens' = rowSums(across(contains("Camb") |
                                                 contains("Vols_") |
                                                 contains("Autres_vols") |
                                                 contains("Destructions") |
                                                 contains("dégrada") |
                                                 contains("Incendies") |
                                                 contains("Violations_domicile") |
                                                 contains("Recels")), na.rm = TRUE),
        
        'Éco-financier' = rowSums(across(contains("Escroqueries") |
                                           contains("Banqueroutes") |
                                           contains("Fraudes_fiscales") |
                                           contains("Fausse_monnaie") |
                                           contains("Achats_ventes_sans_factures") |
                                           contains("Marchandage") |
                                           contains("Travail_clandestin") |
                                           contains("Prix_illicittes") |
                                           contains("Contrefaçons") |
                                           contains("cartes_crédit") |
                                           contains("chèques_volés") |
                                           contains("Infractions_chèques") |
                                           contains("Autres_délits_éco_financiers")), na.rm = TRUE),
        
        'Stupéfiants' = rowSums(across(contains("stup")), na.rm = TRUE),
        
        'Étrangers et séjour' = rowSums(across(contains("étranger") | 
                                                 contains("étrangers")), na.rm = TRUE),
        
        'Administratif, professionnel et réglementaire' = rowSums(across(contains("urbanisme") |
                                                                           contains("profession_règlementée") |
                                                                           contains("Fraudes_alimentaires") |
                                                                           contains("santé_publique") |
                                                                           contains("alcool_tabac") |
                                                                           contains("Chasse_pêche") |
                                                                           contains("animaux") |
                                                                           contains("courses__jeux") |
                                                                           contains("garde_mineurs")), na.rm = TRUE),
        
        'Ordre public, autorité et sûreté' = rowSums(across(contains("Outrages") |
                                                              contains("Menaces") |
                                                              contains("Proxénétisme") |
                                                              contains("armes_prohib") |
                                                              contains("Atteintes_aux_intérêts_fondamentaux") |
                                                              contains("Attentats") |
                                                              contains("interdiction_séjour")), na.rm = TRUE),
        
        'Faux et usage de faux' = rowSums(across(contains("Faux_") |
                                                   contains("faux_docs") |
                                                   contains("Faux_doc") |
                                                   contains("Autres_faux")), na.rm = TRUE)
      )
    
    # Sélection des colonnes
    newdelit <- select(macrocat, 'Dpt',
                       'Atteintes aux personnes',
                       'Atteintes aux biens',
                       'Éco-financier',
                       'Stupéfiants',
                       'Étrangers et séjour',
                       'Administratif, professionnel et réglementaire',
                       'Ordre public, autorité et sûreté',
                       'Faux et usage de faux') %>%
      # Formater les codes départements avec un zéro devant (01, 02, etc.)
      mutate(Dpt = sprintf("%02s", Dpt))
    
    return(newdelit)
  })
  
  # Liste des départements disponibles
  liste_departements <- reactive({
    newdelit <- donnees_preparees()
    # Retourner la liste des départements triés par ordre croissant
    return(sort(newdelit$Dpt))
  })
  
  # Générer dynamiquement les menus déroulants pour les départements
  output$selecteurs_departements <- renderUI({
    nb <- input$nb_depts
    depts <- liste_departements()
    
    # Créer une liste de selectInput
    lapply(1:nb, function(i) {
      selectInput(
        inputId = paste0("dept_", i),
        label = paste("Département", i, ":"),
        choices = depts,
        selected = depts[i]
      )
    })
  })
  
  # Récupérer les départements sélectionnés
  departements_selectionnes <- reactive({
    nb <- input$nb_depts
    
    # Récupérer les valeurs de tous les selectInput
    depts <- sapply(1:nb, function(i) {
      input[[paste0("dept_", i)]]
    })
    
    return(depts[!is.null(depts)])
  })
  
  # Génération du graphique
  output$graphique <- renderPlot({
    # Attendre le clic sur le bouton
    input$actualiser
    
    # Isoler les inputs pour éviter la réactivité automatique
    isolate({
      newdelit <- donnees_preparees()
      depts_selection <- departements_selectionnes()
      type_graph <- input$type_graphique
      
      # Pour le graphique empilé, utiliser toutes les catégories
      if (type_graph == "empile") {
        categories_selectionnees <- c(
          "Atteintes aux personnes",
          "Atteintes aux biens",
          "Éco-financier",
          "Stupéfiants",
          "Étrangers et séjour",
          "Administratif, professionnel et réglementaire",
          "Ordre public, autorité et sûreté",
          "Faux et usage de faux"
        )
      } else {
        # Pour le graphique en facettes, utiliser les catégories sélectionnées
        categories_selectionnees <- input$categories
      }
      
      # Validation
      if (length(categories_selectionnees) == 0 || length(depts_selection) == 0) {
        return(NULL)
      }
      
      # Calculer le total GLOBAL pour chaque département (toutes catégories)
      totaux_globaux <- newdelit %>%
        filter(Dpt %in% depts_selection) %>%
        mutate(Total_Global = rowSums(across(-Dpt), na.rm = TRUE)) %>%
        select('Dpt', 'Total_Global')
      
      # Filtrer les catégories sélectionnées
      colonnes_a_garder <- c("Dpt", categories_selectionnees)
      newdelit_filtre <- newdelit %>% select(all_of(colonnes_a_garder))
      
      # Filtrer par les départements sélectionnés et joindre avec les totaux globaux
      donnees_filtrees <- newdelit_filtre %>%
        filter(Dpt %in% depts_selection) %>%
        left_join(totaux_globaux, by = "Dpt") %>%
        mutate(Total_Selection = rowSums(across(-c(Dpt, Total_Global)), na.rm = TRUE))
      
      # Graphique en facettes
      if (type_graph == "facettes") {
        facet_data <- donnees_filtrees %>%
          mutate(Dpt_label = paste0("Département : ", Dpt, 
                                    "\nTotal des délits recensés : ", format(Total_Global, big.mark = " "),
                                    "\nTotal catégories sélectionnées : ", format(Total_Selection, big.mark = " "))) %>%
          # Garder l'ordre de sélection de l'utilisateur
          mutate(Dpt_label = factor(Dpt_label, levels = unique(Dpt_label[match(depts_selection, Dpt)]))) %>%
          pivot_longer(-c(Dpt, Total_Global, Total_Selection, Dpt_label), names_to = "Catégorie", values_to = "Nombre") %>%
          group_by(Dpt) %>%
          # Calculer le pourcentage par rapport au total GLOBAL (toutes catégories)
          mutate(Pourcentage = Nombre / Total_Global * 100) %>%
          ungroup()
        
        # Afficher en colonne (un graphique en dessous de l'autre)
        ncol_facet <- 1
        
        # Calculer le pourcentage maximum pour adapter l'échelle
        max_pct <- max(facet_data$Pourcentage, na.rm = TRUE)
        y_limit <- max(110, ceiling(max_pct / 10) * 10 + 10)
        
        p <- ggplot(facet_data, aes(x = reorder(Catégorie, -Pourcentage), 
                                    y = Pourcentage, 
                                    fill = Catégorie)) +
          geom_col(show.legend = FALSE) +
          geom_text(aes(label = paste0(round(Pourcentage, 1), "%")),
                    hjust = -0.1,
                    size = 4.5,
                    color = "gray30") +
          facet_wrap(~Dpt_label, ncol = ncol_facet) +
          coord_flip() +
          scale_fill_viridis_d(option = "plasma") +
          scale_y_continuous(
            labels = function(x) paste0(x, "%"),
            limits = c(0, y_limit),
            breaks = seq(0, 100, 10)
          ) +
          labs(
            title = paste0("Profil criminel des département(s) sélectionné(s)"),
            subtitle = "Répartition en % du total global de chaque département (toutes catégories confondues)",
            x = "",
            y = "Part du total global (%)"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            strip.text = element_text(
              face = "bold", 
              size = 13, 
              color = "white",
              lineheight = 0.9,
              margin = margin(5, 5, 5, 5)
            ),
            strip.background = element_rect(fill = "gray20", color = NA),
            plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 10)),
            plot.subtitle = element_text(size = 13, color = "gray40", hjust = 0.5, margin = margin(b = 15)),
            axis.text.y = element_text(size = 11, hjust = 1),
            axis.text.x = element_text(size = 11),
            axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
            panel.spacing = unit(2, "lines"),
            plot.margin = margin(20, 20, 20, 20),
            panel.grid.major.y = element_blank()
          )
        theme_minimal(base_size = 12) +
          theme(
            strip.text = element_text(
              face = "bold", 
              size = 11, 
              color = "white",
              lineheight = 0.9
            ),
            strip.background = element_rect(fill = "gray20", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
            axis.text.y = element_text(size = 9),
            axis.text.x = element_text(size = 9),
            panel.spacing = unit(1.5, "lines"),
            plot.margin = margin(20, 20, 20, 20)
          )
        
        return(p)
      }
      
      # Graphique empilé
      if (type_graph == "empile") {
        donnees_empilees <- donnees_filtrees %>%
          mutate(Dpt_label = paste0(Dpt, " (", format(Total_Global, big.mark = " "), ")")) %>%
          # Garder l'ordre de sélection inversé pour l'affichage
          mutate(Dpt_label = factor(Dpt_label, levels = rev(unique(Dpt_label[match(depts_selection, Dpt)])))) %>%
          pivot_longer(-c(Dpt, Total_Global, Total_Selection, Dpt_label), names_to = "Catégorie", values_to = "Nombre")
        
        p <- ggplot(donnees_empilees, aes(x = Dpt_label, y = Nombre, fill = Catégorie)) +
          geom_col(position = "fill", color = "white", linewidth = 0.3) +
          coord_flip() +
          scale_fill_viridis_d(option = "plasma") +
          scale_y_continuous(
            labels = percent,
            expand = c(0, 0)
          ) +
          labs(
            title = "Composition des délits par département",
            subtitle = "Répartition proportionnelle - Toutes catégories confondues",
            x = "Département (total des délits)",
            y = "Proportion (%)",
            fill = "Catégorie de délit"
          ) +
          theme_minimal(base_size = 12) +
          theme(
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
            legend.position = "right",
            legend.title = element_text(face = "bold", size = 10),
            legend.text = element_text(size = 9),
            axis.text.y = element_text(size = 10, face = "bold"),
            axis.text.x = element_text(size = 9),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = margin(20, 20, 20, 20)
          ) +
          guides(fill = guide_legend(ncol = 1))
        
        return(p)
      }
    })
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)