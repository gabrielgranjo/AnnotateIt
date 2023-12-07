#' label_text_data
#'
#' This function initiates an interactive text annotation application using Shiny. It's designed to facilitate the labeling of text data for classification tasks. Users can annotate a specified number of text entries from a given data frame with predefined labels.
#'
#' @param data_frame_name A string specifying the name of the data frame in the global environment that contains the text to be labeled. This data frame should exist in the global environment prior to calling this function.
#' @param labels A character vector containing the possible labels for annotation. It should have at least two elements. This parameter defines the options available to the user for labeling the text data.
#' @param app_name Optional; a string that sets the title of the annotation application. Default is "Text Classification App". This name appears in the title panel of the Shiny app.
#' @param num_rows Optional; an integer indicating the number of rows to be randomly sampled from the data frame for labeling in a single session. Default is 10. This determines the workload for a single annotation session.
#' @param user_name Optional; a string representing the name of the user. Default is "user". This is used for personalizing the session and naming the output file containing the labeled data.
#' @export
#' @importFrom shiny fluidPage titlePanel
#' @importFrom shiny column reactiveVal observeEvent
#' @importFrom utils write.csv
#' @examples
#' if (interactive()) {
#'   df <- data.frame(text = c("Sample text 1", "Sample text 2"), stringsAsFactors = FALSE)
#'   label_text_data("df", c("Positive", "Negative"), "My Annotation App", 5, "user1")
#' }
label_text_data <- function(data_frame_name, labels, app_name = "Text Classification App", num_rows = 10, user_name = "user") {

  if (!exists(data_frame_name, .GlobalEnv)) {
    stop("Data frame not found in global environment")
  }

  if (!is.character(labels) || length(labels) < 2) {
    stop("Labels should be a character vector with at least two elements")
  }

  if (!is.numeric(num_rows) || num_rows <= 0) {
    stop("num_rows should be a positive number")
  }

  if (!is.character(user_name) || user_name == "") {
    stop("user_name should be a non-empty string")
  }

  data_frame <- get(data_frame_name, .GlobalEnv)
  data_frame$label <- NA  # Add a column for labels

  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("flatly"),
    shiny::titlePanel(app_name),
    shiny::tags$head(shiny::tags$style(htmltools::HTML("
      body { font-family: 'Arial', sans-serif; }
      .btn { margin: 5px; }
      .btn-lg { padding: 10px 20px; font-size: 18px; }
      #textDisplay { background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px; }
      .shiny-output-error { color: #a94442; }
      .shiny-output-error:before { content: 'Error: '; }
    "))),
    shiny::fluidRow(
      column(8, offset = 2,
             shiny::textOutput("progressDisplay"),
             shiny::textOutput("textDisplay"),
             shiny::uiOutput("labelButtons")
      )
    )
  )

  server <- function(input, output, session) {
    sampled_data <- reactiveVal(data_frame[sample(nrow(data_frame), min(num_rows, nrow(data_frame))), ])
    index <- reactiveVal(1)

    output$progressDisplay <- shiny::renderText({
      paste("Labeling text", index(), "of", num_rows)
    })

    output$textDisplay <- shiny::renderText({
      if(index() <= num_rows) {
        sampled_data()$text[index()]
      } else {
        "All texts have been labeled. Please close this session."
      }
    })

    output$labelButtons <- shiny::renderUI({
      lapply(labels, function(label) {
        shiny::actionButton(inputId = paste0("label_", label),
                     label = label,
                     class = "btn btn-primary btn-lg")
      })
    })

    lapply(labels, function(label) {
      observeEvent(input[[paste0("label_", label)]], {
        if(index() <= num_rows) {
          update_sampled_data <- sampled_data()
          update_sampled_data$label[index()] <- label
          sampled_data(update_sampled_data)
          index(index() + 1)

          if (index() > num_rows) {
            # Generate filename with timestamp and user name
            timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
            filename <- paste0(user_name, "_labeled_data_", timestamp, ".csv")

            # Save the labeled data to a CSV file
            write.csv(sampled_data(), filename, row.names = FALSE)

            shiny::showModal(shiny::modalDialog(
              title = "Session Complete",
              paste("You have finished labeling all the texts. The labeled data has been saved as", filename, ". To start another draw, please begin a new session."),
              footer = shiny::modalButton("Close")
            ))
          }
        }
      })
    })
  }

  shiny::runApp(list(ui = ui, server = server))
}
