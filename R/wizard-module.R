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
  
  # header <- tags$head(
  #             tags$style(
  #               HTML("
  #               .footer {
  #                 position: absolute;
  #                 bottom: 0;
  #                 width: 100%;
  #                 height: 80px; /* Set the fixed height of the footer here */
  #                 background-color: #e5e5e5;
  #                }"
  #               )
  #             )
  #           )

  # create tabs
  tabs <- tab_creator(id, tabs)
  # create tab panel
  # tagList(
  #   header,

  # tabsetPanel(
  #   id = NS(id, "wizard"),
  #   type = "hidden",
  #   !!! tabs
  # ),
  
  navbarPage(
    title = div("Transferice", style ="text-align: center;"),
    id = NS(id, "wizard"),
    !!!tabs,
    position = "fixed-bottom",
    inverse = TRUE
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

  })
}

tab_creator <- function(id, tabs) {

  range <- seq_along(tabs)

  # function to populate tab pane with buttons
  tab_buttons <- function(a, b, c, tab, title, id) {
    tabPanel(
      "",
      value = paste0("page_", a),
      fluidRow(column(12, tab)),
      # 
      # 
      #           column(
      #             6,
                   if (!is.na(b))
                    absolutePanel(
                      div(
                      actionButton(NS(id, paste0("page_", a, b)), "prev"),
                      align = "left"
                    ),
                    bottom = 15,
                    left = 15,
                    fixed = TRUE
                  ),
                # ),
                # column(
                #   6,
                  if (!is.na(c))
                    absolutePanel(
                      div(
                        actionButton(NS(id, paste0("page_", a, c)), "next"),
                        align = "right"
                      ),
                      bottom = 15,
                      right = 15,
                      fixed = TRUE
                    )
              #   )
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
