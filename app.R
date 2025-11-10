## Shiny entrypoint for deployment (shinyapps.io expects app.R)
## Sources the app definitions from toggl_table.R and runs the app

source("toggl_table.R", local = TRUE)
shinyApp(ui, server)

