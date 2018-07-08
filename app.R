library(shiny)

# which fields get saved 
fieldsAll <- c("req_last_name", "req_first_name","department", "designation", "r_num_years","pi_flag", "pi_last_name", "pi_first_name", "service_before", "project_title", "service_checklist_before","service_checklist_after")

##directory to save all responses
#responsesDir <- file.path("/home/jessie/consult_response")
responsesDir <- file.path("D:/consult_responses")

epochTime <- function() {
  as.integer(Sys.time())
}



# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}


# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}





# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "



fieldsMandatory <- c("req_first_name", "req_last_name", "department", "designation", "projec_title")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    #titlePanel("KTPH CRU Statistical Consultation Checklist"),
    
    div(id="header", h3("KTPH CRU Statistical Consultation Checklist"),
    h4("This app serves as informal survey (created by Wang Jiexun)")),   
   

   # DT::dataTableOutput("responsesTable"),

    div(
      id = "form",
      
      textInput("project_title", labelMandatory("Project Title"), ""),
   
      textInput("req_last_name", labelMandatory("Requestor Last Name"), ""),
      textInput("req_first_name", labelMandatory("Requestor First Name"), ""),
      selectInput("department", labelMandatory("Department"), c("","General Medicine", "Eye", "ODT")),
     
      selectInput("designation", labelMandatory("Designation"), c("","Nurse", "Senior Nurse","Pharmacist", "Dietician","Association consultant", "Consultant", "Senior consultant")), 
      sliderInput("r_num_years", "Working experience (by year)", 0, 25, 2, ticks = FALSE),
      checkboxInput("pi_flag", "I'm the PI.", FALSE),

      div(h6("If you are not PI, please type in PI name. Otherwise, just skip it.")), 

      textInput("pi_last_name", "PI Last Name", ""),
      textInput("pi_first_name", "PI First Name"),

     

      checkboxInput("service_before", "I(We) have requested consultation service from CRU biostatistician before", FALSE),
      selectInput("service_checklist_before", "Service checklist BEFORE consultation (multiple choices available)", 
       c("Study design", "Sample size/power calculation", "Data analysis", "Guide on data analysis", "Result interpretation", "Manuscript revision"), multiple=TRUE),
      selectInput("service_checklist_after", "Service checklist AFTER consultation (multiple choices available)", 
      c("Study design", "Sample size/power calculation", "Data analysis", "Guide on data analysis", "Result interpretation", "Manuscript revision"), multiple=TRUE), 
     actionButton("submit", "Submit", class = "btn-primary"),
   
      shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error", div(br(), tags$b("Error: "), span(id = "error_msg")))
      )    

    ),

    shinyjs::hidden(
          div(
            id = "thankyou_msg",
            h3("Thanks, your response was submitted successfully!"),
            actionLink("submit_another", "Submit another response")
          )
    )



  ),
  server = function(input, output, session) {
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    }) 


   # Gather all the form inputs (and add timestamp)
   formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
   })
   


    # When the Submit button is clicked, submit the response
    observeEvent(input$submit, {
      
      # User-experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Save the data (show an error message in case of error)
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
   })

  

    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
   })


    if(FALSE){
    # Show the responses in the admin table
    output$responsesTable <- DT::renderDataTable({
      data <- loadData()
      data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
      DT::datatable(
        data,
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE)
       )
     })
    }

  }
)


 
