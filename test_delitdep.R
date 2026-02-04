library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(scales)

delits <- read_csv2("delitDep.csv", na="NA", locale=locale(encoding="latin1"))

macrocat <- delits %>%
  mutate('Atteintes aux personnes' = rowSums(across(contains("Homicides") |
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
                                                    contains("Autres_faux")), na.rm = TRUE))

newdelit <- select(macrocat, 'Dpt',
                   'Atteintes aux personnes',
                   'Atteintes aux biens',
                   'Éco-financier',
                   'Stupéfiants',
                   'Étrangers et séjour',
                   'Administratif, professionnel et réglementaire',
                   'Ordre public, autorité et sûreté',
                   'Faux et usage de faux')

delits_long <- newdelit %>% 
  pivot_longer('Atteintes aux personnes':'Faux et usage de faux')

delits_wide <- delits_long %>% 
  pivot_wider(names_from = name, values_from = value)

### ggplot
top_depts <- newdelit %>%
  mutate(Total = rowSums(across(-Dpt), na.rm = TRUE)) %>%
  arrange(desc(Total)) %>%
  head(9)

facet_data <- newdelit %>%
  filter(Dpt %in% top_depts$Dpt) %>%
  left_join(top_depts %>% select('Dpt', 'Total'), by = "Dpt") %>%
  mutate(Dpt_label = paste0("Département : ", Dpt, "\nTotal des délits recensés : ", format(Total, big.mark = " "))) %>%
  mutate(Dpt_label = factor(Dpt_label, levels = unique(Dpt_label[order(-Total)]))) %>%
  pivot_longer(-c(Dpt, Total, Dpt_label), names_to = "Catégorie", values_to = "Nombre") %>%
  group_by(Dpt) %>%
  mutate(Pourcentage = Nombre / sum(Nombre) * 100) %>%
  ungroup()

ggplot(facet_data, aes(x = reorder(Catégorie, -Pourcentage), 
                       y = Pourcentage, 
                       fill = Catégorie)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Pourcentage, 1), "%")),
            hjust = -0.1,
            size = 3,
            color = "gray30") +
  facet_wrap(~Dpt_label, ncol = 3) +
  coord_flip() +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 110),
    breaks = seq(0, 100, 10)
  ) +
  labs(
    title = "Profil criminel des 9 départements les plus représentés",
    subtitle = "Répartition en pourcentage par catégorie de délit - Échelle commune 0-100%",
    x = "",
    y = "Part du total (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(
      face = "bold", 
      size = 10, 
      color = "white",
      lineheight = 0.9
    ),
    strip.background = element_rect(fill = "gray20", color = NA),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
    axis.text.y = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(20, 20, 20, 20)
  )



top_depts_empile <- newdelit %>%
  mutate(Total = rowSums(across(-Dpt), na.rm = TRUE)) %>%
  arrange(desc(Total)) %>%
  head(9)

donnees_empilees <- newdelit %>%
  filter(Dpt %in% top_depts_empile$Dpt) %>%
  left_join(top_depts_empile %>% select('Dpt', 'Total'), by = "Dpt") %>%
  mutate(Dpt_label = paste0(Dpt, " (", format(Total, big.mark = " "), ")")) %>%
  mutate(Dpt_label = factor(Dpt_label, levels = rev(unique(Dpt_label[order(-Total)])))) %>%
  pivot_longer(-c(Dpt, Total, Dpt_label), names_to = "Catégorie", values_to = "Nombre")

ggplot(donnees_empilees, aes(x = Dpt_label, y = Nombre, fill = Catégorie)) +
  geom_col(position = "fill", color = "white") +
  coord_flip() +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(
    labels = percent,
    expand = c(0, 0)
  ) +
  labs(
    title = "Composition des délits par département",
    subtitle = "Répartition proportionnelle - Top 9 départements",
    x = "Département (total des délits)",
    y = "Proportion (%)",
    fill = "Catégorie de délit"
  ) +
  theme_minimal(base_size = 11) +
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