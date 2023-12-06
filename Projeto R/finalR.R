library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(plotly)
library(dplyr)

dados <- read.csv("C:\\Users\\gabri\\Documents\\Projeto R\\DailyDelhiClimateTest.csv")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ui <- dashboardPage(
  dashboardHeader(title = "Análise de Dados Climáticos"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descrição", tabName = "descricao_tab"),
      menuItem("Análise", tabName = "analise_tab"),
      menuItem("Análise Dupla", tabName = "analise_dupla_tab")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "descricao_tab",
        h2("Descrição dos Dados Climáticos"),
        HTML("O conjunto de dados é dedicado aos desenvolvedores que desejam treinar modelos de previsão do tempo para o clima da Índia. Este conjunto de dados fornece informações de 1º de janeiro de 2013 a 24 de abril de 2017 na cidade de Delhi, Índia. Os quatro parâmetros incluídos são: <strong>meantemp</strong>, <strong>humidity</strong>, <strong>wind_speed</strong> e <strong>meanpressure</strong>."),
        HTML("Créditos: Este conjunto de dados foi coletado da API do Weather Underground. A propriedade e o crédito do conjunto de dados pertencem a eles."),
        HTML("Prazo de entrega: A tarefa 4 deve ser entregue até 19 de outubro de 2019 (22:00). Qualquer kernel publicado após esse prazo será avaliado com apenas 50% da pontuação total."),
        HTML("Inspiração: Este conjunto de dados foi desenvolvido como parte da Tarefa 4 do Curso de Análise de Dados de 2019 na PES University, Bangalore.")
      ),
      tabItem(
        tabName = "analise_tab",
        h2("Análise dos Dados Climáticos"),
        dateRangeInput("date_range", "Selecione um Intervalo de Datas:",
                       start = min(dados$date), end = max(dados$date)),
        selectInput("variable", "Escolha uma Variável:",
                    choices = colnames(dados), selected = colnames(dados)[1]),
        actionButton("generate_analysis", "Gerar Análise"),
        dataTableOutput("analysis_results"),
        fluidRow(
          column(4, actionButton("line_plot_button", "Gráfico em Linha")),
          column(4, actionButton("histogram_button", "Histograma")),
          column(4, actionButton("boxplot_button", "Boxplot"))
        ),
        plotlyOutput("line_plot"),
        plotlyOutput("histogram_plot"),
        plotlyOutput("boxplot_plot")
      ),
      tabItem(
        tabName = "analise_dupla_tab",
        h2("Análise Dupla dos Dados Climáticos"),
        dateRangeInput("date_range_dupla", "Selecione um Intervalo de Datas:",
                       start = min(dados$date), end = max(dados$date)),
        selectInput("classe1", "Escolha a Primeira Classe:",
                    choices = colnames(dados), selected = colnames(dados)[1]),
        selectInput("classe2", "Escolha a Segunda Classe:",
                    choices = colnames(dados), selected = colnames(dados)[2]),
        actionButton("generate_dual_analysis", "Gerar Análise Dupla"),
        dataTableOutput("correlation_results"),
        plotlyOutput("line_plot_dual"),
        plotlyOutput("bar_plot_dual"),
        actionButton("scatterplot_button", "Scatterplot"),
        plotlyOutput("scatter_plot")
        
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$generate_analysis, {
    dados_filtrados <- dados %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
    
    variable_name <- input$variable
    
    mean_value <- mean(dados_filtrados[[variable_name]])
    mode_value <- Mode(dados_filtrados[[variable_name]])
    median_value <- median(dados_filtrados[[variable_name]])
    std_deviation <- sd(dados_filtrados[[variable_name]])
    
    analysis_data <- data.frame(
      Estatística = c("Média", "Moda", "Mediana", "Desvio Padrão", "Mínimo", "Máximo"),
      Valor = c(mean_value, mode_value, median_value, std_deviation, min(dados_filtrados[[variable_name]]), max(dados_filtrados[[variable_name]]))
    )
    
    output$analysis_results <- renderDT({
      datatable(
        analysis_data,
        options = list(
          searching = FALSE,
          paging = FALSE,
          ordering = FALSE
        )
      )
    })
  })
  
  output$line_plot <- renderPlotly({
    dados_filtrados <- dados %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
    
    variable_name <- input$variable
    line_plot <- plot_ly(dados_filtrados, x = ~date, y = ~dados_filtrados[[variable_name]], type = 'scatter', mode = 'lines')
    
    line_plot <- line_plot %>% layout(
      xaxis = list(title = "Data"),
      yaxis = list(title = variable_name)
    )
    
    return(line_plot)
  })
  
  observeEvent(input$histogram_button, {
    dados_filtrados <- dados %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
    
    variable_name <- input$variable
    histogram <- plot_ly(dados_filtrados, x = ~dados_filtrados[[variable_name]], type = 'histogram')
    
    histogram <- histogram %>% layout(
      xaxis = list(title = variable_name),
      yaxis = list(title = "Frequência")
    )
    
    output$histogram_plot <- renderPlotly({
      histogram
    })
  })
  
  observeEvent(input$boxplot_button, {
    dados_filtrados <- dados %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
    
    variable_name <- input$variable
    boxplot <- plot_ly(dados_filtrados, y = ~dados_filtrados[[variable_name]], type = 'box')
    
    boxplot <- boxplot %>% layout(
      yaxis = list(title = variable_name)
    )
    
    output$boxplot_plot <- renderPlotly({
      boxplot
    })
  })
  
  observeEvent(input$generate_dual_analysis, {
    dados_filtrados <- dados %>%
      filter(date >= input$date_range_dupla[1], date <= input$date_range_dupla[2])
    
    variable2_name <- input$classe2
    
    correlation <- cor(dados_filtrados[[variable1_name]], dados_filtrados[[variable2_name]])
    
    correlation_data <- data.frame(
      Variável1 = variable1_name,
      Variável2 = variable2_name,
      Correlação = correlation
    )
    
    output$correlation_results <- renderDT({
      datatable(
        correlation_data,
        options = list(
          searching = FALSE,
          paging = FALSE,
          ordering = FALSE
        )
      )
    })
  })
  
  output$line_plot_dual <- renderPlotly({
    dados_filtrados <- dados %>%
      filter(date >= input$date_range_dupla[1], date <= input$date_range_dupla[2])
    
    variable1_name <- input$classe1
    variable2_name <- input$classe2
    
    line_plot_dual <- plot_ly(dados_filtrados, x = ~date) %>%
      add_trace(y = ~dados_filtrados[[variable1_name]], name = variable1_name, type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~dados_filtrados[[variable2_name]], name = variable2_name, type = 'scatter', mode = 'lines')
    
    line_plot_dual <- line_plot_dual %>% layout(
      xaxis = list(title = "Data"),
      yaxis = list(title = "Valores das Variáveis")
    )
    
    return(line_plot_dual)
  })
  
  output$bar_plot_dual <- renderPlotly({
    dados_filtrados <- dados %>%
      filter(date >= input$date_range_dupla[1], date <= input$date_range_dupla[2])
    
    variable1_name <- input$classe1
    variable2_name <- input$classe2
    
    summary_data <- dados_filtrados %>%
      group_by(date) %>%
      summarize(Média_Var1 = mean(dados_filtrados[[variable1_name]]),
                Média_Var2 = mean(dados_filtrados[[variable2_name]]))
    
    bar_plot_dual <- plot_ly(summary_data, x = ~date) %>%
      add_trace(y = ~Média_Var1, name = variable1_name, type = 'bar', marker = list(color = "blue")) %>%
      add_trace(y = ~Média_Var2, name = variable2_name, type = 'bar', marker = list(color = "green"))
    
    bar_plot_dual <- bar_plot_dual %>% layout(
      xaxis = list(title = "Data"),
      yaxis = list(title = "Médias das Variáveis")
    )
    
    return(bar_plot_dual)
  })
  
  observeEvent(input$scatterplot_button, {
    dados_filtrados <- dados %>%
      filter(date >= input$date_range_dupla[1], date <= input$date_range_dupla[2])
    
    variable1_name <- input$classe1
    variable2_name <- input$classe2
    
    scatter_plot <- plot_ly(dados_filtrados, x = ~dados_filtrados[[variable1_name]], y = ~dados_filtrados[[variable2_name]], type = 'scatter', mode = 'markers')
    
    scatter_plot <- scatter_plot %>% layout(
      xaxis = list(title = variable1_name),
      yaxis = list(title = variable2_name)
    )
    
    output$scatter_plot <- renderPlotly({
      scatter_plot
    })
  })
}

shinyApp(ui, server)

    
    