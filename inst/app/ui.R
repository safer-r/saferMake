library(shiny)
library(bslib)

ui <- fluidPage(
  # Conserve votre thème Flatly d'origine
  theme = bs_theme(bootswatch = "flatly"),
  
  tags$head(
    # Chargement du style Sphinx
    tags$link(rel = "stylesheet", href = "theme.css", type = "text/css"),
    
    tags$style(HTML("
      /* Structure globale Sphinx (Sidebar + Contenu) */
      body, html { height: 100%; margin:0; padding:0; background-color: #edf0f2 !important; scroll-behavior: smooth; }
      .wy-grid-for-nav { display: flex; min-height: 100vh; width: 100%; }
      
      /* 1) Barre latérale noire / bleu Sphinx (Forced Center Layout) */
      .wy-nav-side { width: 300px; background: #343131; color: #b3b3b3; flex-shrink: 0; position: fixed; height: 100vh; }
      
      .wy-side-nav-search { 
        background: #2980B9 !important; 
        padding: 20px 15px !important; 
        text-align: center !important; 
        display: flex !important;
        flex-direction: column !important;
        align-items: center !important;
        justify-content: center !important;
      }
      
      .wy-side-nav-search a { 
        display: block !important; 
        width: 100% !important;
        text-align: center !important; 
        color: white !important; 
        font-weight: bold !important; 
        font-size: 15px !important; 
        text-decoration: none !important; 
        word-wrap: break-word !important; 
        line-height: 1.4 !important; 
        white-space: pre-line !important; 
      }
      
      .wy-side-nav-search .version { 
        color: rgba(255,255,255,0.6) !important; 
        font-size: 11px !important; 
        margin-top: 5px !important; 
        text-align: center !important; 
      }
      
      /* 4) Liens de navigation de la sidebar style Sphinx (Table of Contents Blue Match) */
      .wy-menu-vertical { padding-top: 10px; }
      
      .wy-menu-vertical p.caption { 
        color: #2980B9 !important; 
        background: #262424 !important; 
        padding: 5px 20px !important; 
        margin: 0 !important; 
        font-size: 11px !important; 
        font-weight: bold !important; 
        text-transform: uppercase !important; 
        letter-spacing: 1px !important; 
      }
      
      .wy-menu-vertical ul { list-style: none; padding: 0; margin: 0; }
      .wy-menu-vertical ul li a { display: block; padding: 10px 20px; color: #d9d9d9; text-decoration: none; font-size: 13px; border-bottom: 1px solid #262424; }
      .wy-menu-vertical ul li a:hover { background: #444141; color: white; }
      
      /* 2 & 3) Page principale décalée pour laisser la place à la sidebar fixe */
      .wy-nav-content-wrap { flex-grow: 1; margin-left: 300px; background: #edf0f2; padding: 40px 20px; min-height: 100vh; }
      
      /* REMOVED GREY BORDER HERE (border: none !important) */
      .rst-content { background: #ffffff; padding: 30px; border: none !important; border-radius: 4px; }
      
      /* Style pour le bloc d'instructions de style Sphinx Notice */
      .instruction-box {
        background-color: #f3f6f6 !important;
        border-left: 4px solid #2980B9 !important;
        padding: 15px 20px !important;
        margin-bottom: 25px !important;
        border-radius: 0 4px 4px 0;
      }
      .instruction-box p {
        margin: 0 !important;
        line-height: 1.6 !important;
        color: #333333 !important;
        font-size: 14px !important;
      }
      
      /* Ajustements des titres de sections */
      h4 { color: #2980B9 !important; font-family: 'Lato', sans-serif; font-weight: bold; margin-top: 10px; }
      hr { border-top: 1px solid #e1e4e6; margin: 24px 0; }
      footer { margin-top: 40px; color: #999; font-size: 12px; }
    "))
  ),
  
  # Conteneur Grid principal
  div(class = "wy-grid-for-nav",
      
      # BARRE LATÉRALE NOIRE (SPHINX)
      tags$nav(class = "wy-nav-side",
          div(class = "wy-side-scroll",
              # 1) Titre complet dans le carré bleu en haut à gauche
              div(class = "wy-side-nav-search",
                  a(href = "#", HTML("function > safer-r function<br>converter")),
                  div(class = "version", "v1.0")
              ),
              # 4) Table des matières cliquable pointant vers les h4()
              div(class = "wy-menu wy-menu-vertical",
                  p(class = "caption", "Table of Contents"),
                  tags$ul(
                      tags$li(a(href = "#sec_code", "Code of your function")),
                      tags$li(a(href = "#sec_package", "Package name")),
                      tags$li(a(href = "#sec_report", "Report"))
                  )
              )
          )
      ),
      
      # PAGE PRINCIPALE
      tags$section(class = "wy-nav-content-wrap",
          div(class = "rst-content",
              div(role = "main", class = "document",
                  
                  # Insertion des instructions au tout début du contenu principal
                  div(class = "instruction-box",
                      tags$p(HTML("Fill the field.<br/>Click on the run button.<br/>Go to the newly created tab and complete the fields."))
                  ),
                  
                  # Réintégration exacte de votre structure d'origine (sans le titlePanel du haut)
                  fluidRow(
                      column(
                          width = 12, # Utilisation de toute la largeur de la zone blanche Sphinx
                          wellPanel(
                              style = "background-color: #fcfcfc; border: none; padding: 0; margin: 0;",
                              
                              # Section 1 : Code de la fonction
                              h4(id = "sec_code", "Code of your function"),
                              textAreaInput(
                                  inputId = "user_ini_fun",
                                  label   = "Paste your R function code here.",
                                  placeholder = "my_fun <- function(x){\n    x + 1\n}",
                                  rows = 15,
                                  width = "100%"
                              ),
                              
                              hr(),
                              
                              # Section 2 : Nom du package
                              h4(id = "sec_package", "Package name"),
                              textInput(
                                  inputId = "package_name",
                                  label   = "Leave empty for NULL, or type a package name",
                                  placeholder = "NULL"
                              ),
                              
                              hr(),

                              # Section 3 : Rapport
                              h4(id = "sec_report", "Report"),
                              textInput(
                                  inputId = "report_name", # Ajusté ici pour éviter le conflit d'ID avec package_name
                                  label   = "Leave empty for NULL, or type a package name",
                                  placeholder = "NULL"
                              ),
                              
                              hr(),

                              # Bouton Exécuter
                              div(
                                  style = "text-align: right;",
                                  downloadButton(
                                      outputId = "run_button",
                                      label    = "Run",
                                      class    = "btn-primary"
                                  )
                              )
                          )
                      )
                  )
                  
              ),
              
              # Footer style Sphinx
              tags$footer(
                  hr(),
                  div(role = "contentinfo",
                      p("© Copyright 2026. Built with Shiny & Sphinx Theme Style.")
                  )
              )
          )
      )
  ),
  
  # Script JavaScript Sphinx (Optionnel, gère le responsive mobile si inclus)
  tags$script(type = "text/javascript", src = "theme.js")
)
