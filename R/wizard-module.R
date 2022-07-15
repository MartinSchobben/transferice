#' Wizard module
#'
#' The module creates a wizard that takes you through a selection of slides.
#'
#' @param id Namespace of the module.
#' @param tabs `Taglist` with elements to be included within the tabs. The
#'  number of elements determines the number of slides in the wizard.
#'
#' @export
wizard_ui <- function(id, tabs) {
  

  # create tabs
  tabs <- tab_creator(id, tabs)
  
  # create tab panel
  tabsetPanel(
    id = NS(id, "wizard"),
    type = "hidden",
    !!! tabs
  )
}
#' @rdname wizard_ui
#'
#' @export
wizard_server <- function(id, n) {

  moduleServer(id, function(input, output, session) {

    # functions to switch tabs
    switch_tab <- function(from, to) {
      observeEvent(input[[paste0("page_", from, to)]], {
        updateTabsetPanel(session, "wizard", selected = paste0("page_", to))
      })
    }

  # go forward
  purrr::map(seq_len(n)[-1], ~switch_tab(.x, .x - 1))

  # go back
  purrr::map(seq_len(n)[-n], ~switch_tab(.x, .x + 1))
  
  # color
  # clr <- thematic::thematic_get_theme()$sequential[1]
  # 
  # observe(message(paste0("background-color:", clr, ";")))
  
  
  })
}

tab_creator <- function(id, tabs) {

  range <- seq_along(tabs)

  # function to populate tab pane with buttons
  tab_buttons <- function(a, b, c, tab, title, id) {
    tabPanelBody(
      value = paste0("page_", a),
      fluidRow(column(width = 12, tab)),
      fluidRow(
        width = 12, 
        tags$hr(),
        fixedPanel(
          style = paste0("background-color:", grDevices::grey(0.8), ";opacity: 0.5;"), 
          bottom = 0, 
          left = 0, 
          right = 0,
          height = 100
        )
      ),
      fluidRow(
        column(
          width = 6,
          if (!is.na(b))
            fixedPanel(
              actionButton(NS(id, paste0("page_", a, b)), "prev"),
              bottom = 25, 
              left = 25,
              height = 50
            )
        ),
        column(
          width = 6,
          if (!is.na(c))
            fixedPanel(
              actionButton(NS(id, paste0("page_", a, c)), "next"),
              bottom = 25, 
              right = 25,
              height = 50
            )
        )
      )
    )
  }
  # execute function
  purrr::pmap(
    list(
      a = range,
      b = dplyr::lag(range),
      c = dplyr::lead(range),
      tab = tabs,
      title = names(tabs)
    ),
    tab_buttons,
    id = id
  )
}
