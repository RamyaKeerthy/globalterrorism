observeEvent(input$switchtab, {
  newtab <- switch(input$tabs, "intro" = "overview","overview" = "intro")
  updateTabItems(session, "tabs", newtab)
})

observeEvent(input$switchtab1, {
  newtab <- switch(input$tabs, "intro" = "trends","trends" = "intro")
  updateTabItems(session, "tabs", newtab)
})
observeEvent(input$switchtab2, {
  newtab <- switch(input$tabs, "intro" = "radar","radar" = "intro")
  updateTabItems(session, "tabs", newtab)
})

observeEvent(input$switchtab3, {
  newtab <- switch(input$tabs, "overview" = "intro","intro" = "overview")
  updateTabItems(session, "tabs", newtab)
})

observeEvent(input$switchtab4, {
  newtab <- switch(input$tabs, "trends" = "intro","intro" = "trends")
  updateTabItems(session, "tabs", newtab)
})

observeEvent(input$switchtab5, {
  newtab <- switch(input$tabs, "radar" = "intro","intro" = "radar")
  updateTabItems(session, "tabs", newtab)
})

observeEvent(input$switchtab6, {
  newtab <- switch(input$tabs, "trends" = "radar","radar" = "trends")
  updateTabItems(session, "tabs", newtab)
})

observeEvent(input$switchtab7, {
  newtab <- switch(input$tabs, "radar" = "trends","trends" = "radar")
  updateTabItems(session, "tabs", newtab)
})

observeEvent(input$switchtab8, {
  newtab <- switch(input$tabs, "overview" = "trends","trends" = "overview")
  updateTabItems(session, "tabs", newtab)
})

observeEvent(input$switchtab9, {
  newtab <- switch(input$tabs, "overview" = "radar","radar" = "overview")
  updateTabItems(session, "tabs", newtab)
})