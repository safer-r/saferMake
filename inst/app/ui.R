library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "theme.css", type = "text/css"),
    tags$style(HTML("
      /* Structure globale Sphinx (Sidebar + Contenu) */
      body, html { height: 100%; margin:0; padding:0; background-color: #edf0f2 !important; scroll-behavior: smooth; }
      .wy-grid-for-nav { display: flex; min-height: 100vh; width: 100%; }
      
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
      
      .wy-side-nav-search > a { 
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
      
      .wy-side-nav-search .version a {
        color: rgba(255,255,255,0.6) !important;
        font-size: 11px !important;
        font-weight: normal !important;
        text-decoration: none !important;
        display: inline !important;
      }
      .wy-side-nav-search .version a:hover {
        text-decoration: underline !important; 
      }
      
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
      
      .wy-nav-content-wrap { flex-grow: 1; margin-left: 300px; background: #edf0f2; padding: 40px 20px; min-height: 100vh; }
      
      .rst-content { background: #ffffff; padding: 30px; border: none !important; border-radius: 4px; }
      
      .input-instruction-label { 
        font-weight: bold; 
        color: #333333; 
        margin-bottom: 8px; 
        font-size: 14px; 
        display: block; 
      }
      
      .input-instruction-list { 
        margin: 5px 0 12px 0; 
        padding-left: 20px; 
        color: #555555; 
        font-size: 13px; 
        line-height: 1.5; 
        font-weight: normal; 
      }
      
      h4 { color: #2980B9 !important; font-family: 'Lato', sans-serif; font-weight: bold; margin-top: 10px; }
      hr { border-top: 1px solid #e1e4e6; margin: 24px 0; }
      footer { margin-top: 40px; color: #999; font-size: 12px; }
    "))
  ),
  
  div(class = "wy-grid-for-nav",
      
      # BARRE LATÉRALE (identique sur tous les "écrans")
      tags$nav(class = "wy-nav-side",
          div(class = "wy-side-scroll",
              div(class = "wy-side-nav-search",
                  a(href = "#", HTML("Function -> <i>safer-r</i> Function<br>Converter")),
                  div(class = "version", 
                      a(href = "https://github.com/safer-r/.github/blob/main/profile/backbone.R", 
                        "Backbone v19.5", target = "_blank"))
              ),
              # Table of Contents: dynamique, adaptée à l'écran courant
              uiOutput("toc")
          )
      ),
      
      # PAGE PRINCIPALE : le contenu interne change selon l'écran
      tags$section(class = "wy-nav-content-wrap",
          div(class = "rst-content",
              div(role = "main", class = "document", 
                  uiOutput("screen")
              )
          )
      )
  )
)