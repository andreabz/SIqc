#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      id = "navbar",
      theme = bslib::bs_theme(bootswatch = "cosmo",
                              version = 5,
                              "navbar-bg" = "#2780E3",
                              "navbar-brand-font-size" = "2rem"),
      title = "SI qc",
      window_title = "SI qc",
      inverse = TRUE,
      fluid = TRUE,
      collapsible = TRUE,
      lang = "it",

      # Navbar items ----
      bslib::nav_panel("Attività",
                       value = "tasks",
                       mod_01_plan_ui("tasks")),
      bslib::nav_menu("Risultati",
                      align = "right",
                      bslib::nav_panel("Ripetibilità",
                                       value = "repeatability",
                                       mod_02_repeatability_results_ui("repeatability")),
                      bslib::nav_panel("Giustezza", value = "trueness", ""),
                      bslib::nav_panel("Proficency test", value = "proficency_test", "")),
      bslib::nav_spacer(),
      bslib::nav_menu("Leggimi",
                      align = "right",
                      bslib::nav_panel("Per iniziare", value = "readme", ""),
                      bslib::nav_panel("Esempi", value = "examples", ""),
                      bslib::nav_panel("Validazione", value = "tests", ""),
                      bslib::nav_panel("Versioni", value = "news", ""),
                      bslib::nav_panel("Struttura", value = "structure", "")
      )

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SIqc"
    ),
    shinyjs::useShinyjs()
  )
}
