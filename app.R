library(shiny)

# 定義 UI
ui <- fluidPage(
  titlePanel("題庫管理系統"),
  sidebarLayout(
    sidebarPanel(
      numericInput("id", "ID:", 1, min = 1),
      textInput("question", "題目:", "How much cheese ______ they need?"),
      textInput("options", "選項:", "(A) is (B) there (C) do (D) does"),
      textInput("answer", "答案:", "C"),
      actionButton("add", "添加題目"),
      br(),
      actionButton("refresh", "刷新題目列表")
    ),
    mainPanel(
      h3("題庫列表"),
      tableOutput("questionTable")
    )
  )
)

# 定義 Server logic
server <- function(input, output) {
  # 文件路徑
  filepath <- "questions.csv"
  
  # 載入或初始化題庫
  if (file.exists(filepath)) {
    questions <- reactiveVal(read.csv(filepath, stringsAsFactors = FALSE))
  } else {
    questions <- reactiveVal(data.frame(
      ID = integer(),
      Question = character(),
      Options = character(),
      Answer = character(),
      BlankPosition = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  # 監聽添加按鈕
  observeEvent(input$add, {
    new_question <- data.frame(
      ID = input$id,
      Question = input$question,
      Options = input$options,
      Answer = input$answer,
      BlankPosition = gregexpr("______", input$question)[[1]],
      stringsAsFactors = FALSE
    )
    # 更新題庫
    updated_questions <- rbind(questions(), new_question)
    questions(updated_questions)
    # 保存到 CSV
    write.csv(updated_questions, filepath, row.names = FALSE)
  })
  
  # 顯示題庫
  output$questionTable <- renderTable({
    questions()
  }, options = list(pageLength = 5))
  
  # 監聽刷新按鈕
  observeEvent(input$refresh, {
    questions(if (file.exists(filepath)) read.csv(filepath, stringsAsFactors = FALSE) else questions())
    output$questionTable <- renderTable({
      questions()
    }, options = list(pageLength = 5))
  })
}

# 執行應用
shinyApp(ui = ui, server = server)
