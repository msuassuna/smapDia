######## Aplicativo SmapDia/SGB ###########
# Data: Janeiro, 2020
# Author: Marcus Suassuna Santos
# email: msuassuna@gmail.com
##########################################################

# Pacotes do R utilizados
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(plotly)

# Funcões que sao executadas pelo programa e implementadas separadamente
source("smap_daily.R")
source("ns.R")

# Arquivo UI com leiaute da pagina do aplicativo
ui <- tagList(
  
  navbarPage(
    theme = "cerulean",
    
    dashboardHeader(
      title = span(img(src = "logomarcacprmhorizontal_v2.jpg",
                       height = 50, width = 50*5),"\t",
                   "Aplicativo SmapDia/SGB"),
      titleWidth = 500
    ),
    
    # Painel introdutório com informacões do histórico do aplicativo, do SMAP e do metodo de otimizacao
    tabPanel(h4("Introducao"),
             
             mainPanel(
               tabsetPanel(
                 tabPanel(h4("Histórico"),
                   h4(strong("Sobre a producao e disponibilizacao do SmapDia/SGB")),
                   p("O aplicativo SmapDia/SGB foi produzido no intuito de auxiliar aplicacões com o modelo hidrológico SMAP em passo de tempo diario, utilizando dados inseridos pelos/as usuarios/as."),
                   p("O aplicativo e util para profissionais, estudantes e pesquisadores interessados em hidrologia com pouca experiência em programacao e/ou que desejem utilizar uma interface simples e amigavel de implementacao do modelo, com a possibilidade de realizar varios testes variando-se os parametros do modelo."),
                   p("Para isso, foi produzida uma interface simples e intuitiva para que seja possivel fazer uma serie de testes com o modelo hidrológico SMAP bem como para sua calibracao utilizando algoritmo genetico."),
                   p("O aplicativo foi produzido pelo Pesquisador em Geociências Marcus Suassuna Santos como parte de suas atividades na operacao de alertas no Sistema de Alerta de Eventos Criticos pelo Servico Geológico do Brasil (CPRM/SGB). Contudo, foi fundamental o auxilio de colegas de outras instituicoes neste aplicativo. Em especial, do engenheiro Cassio Rampinelli, Analista de Infraestrutura no Ministerio do Desenvolvimento Regional, mestre pela Universidade de Brasilia, que forneceu os codigos que utilizamos e adaptamos para o algoritmo genetico MOPSO."),
                   p("A elaboracao e disponibilizacao deste aplicativo esta alinhada com a missao do Servico Geológico do Brasil em",
                     strong(" gerar e disseminar conhecimento geocientifico com excelência, contribuindo para melhoria da qualidade de vida e desenvolvimento sustentavel do Brasil.")
                     ),
                   p("Brasilia, fevereiro de 2020."),
                   br()
                 ),
                 
                 tabPanel(h4("O Aplicativo"),
                   h4(strong("Detalhes de funcionamento do aplicativo SmapDia/SGB")),
                   p("O SmapDia/SGB foi produzido de modo a possibilitar uma interface fluida e intuitiva para a utilizacao do modelo hidrológico SMAP. Contudo, e importante explicitar alguns elementos acerta de sua estrutura e funcionamento."),
                   p("Alem deste painel introdutório, o aplicativo e composto por outras quatro partes:"),
                   
                   h4("Dados"),
                   p("No segundo painel, o/a usuario/a podera carregar seus próprios dados hidrológicos e visualizar aspectos gerais dessas informacões."),
                   p("O unico dado da bacia necessario para o modelo SMAP e o dado de area de drenagem (em km²). O nome da bacia e interessante caso o/a usuario/a deseje que ele apareca nos graficos produzidos."),
                   p("Alem desse dado basico, os dados hidrológicos devem ser introduzidos utilizando-se um arquivo .csv. O arquivo devera apresentar cabecalho e conter ao menos quatro colunas com os nomes Data, E (associados aos dados de Evapotranspiracao, em mm/dia), P (precipitacao, em mm/dia) e Q (vazao, em m³/s)."),
                   p("Para correta leitura dos dados, e preferivel que as colunas do arquivo de entrada sejam separadas por ponto-e-virgula e as casas decimais sejam virgula. Caso isso nao ocorra, o/a usuario/a tera algumas opcões de ajuste de formato de separacao de colunas e casas decimais no painel lateral."),
                   p("Para correta leitura dos dados, tambem e importante que na coluna de datas, nao exista qualquer valor faltante. Ou seja, que a diferenca entre as datas seja sempre de um dia. As datas devem estar no formato dd/mm/aaaa."),
                   p("Eventuais dados faltantes de vazao sao admissiveis na amostra de validacao e devem ser deixados em branco. Para o periodo de calibracao, nao pode haver nenhum dado faltante."),
                   p("O painel lateral permite que o/a usuario/a escolha diferentes partes da tabela para pre-visualizacao. O/A usuario/a pode optar por visualizar o inicio da tabela (primeiras 13 linhas) ou seu final (ultimas 13 linhas)."),
                   p("O carregamento do arquivo por parte do/a usuario/a e feito navegando em seus arquivos e pressionando o botao “Browse”. "),
                   
                   h4("SMAP"),
                   p("No terceiro painel, o/a usuario/a podera exercitar a calibracao do SMAP diario, avaliando a sensibilidade do modelo a cada um dos parametros modificando os parametros e visualizando o resultado dessas modificacões em tempo real."),
                   p("A proposta e a de que seja possivel observar o efeito de cada um dos parametros sobre a resposta do hidrograma, visualizando o efeito dessas alteracões em tempo real no hidrograma estimado."),
                   p("A primeira informacao necessaria nesta etapa e o inicio e fim do periodo de validacao dos dados. Alem dessas, as duas condicões iniciais e os seis parametros do modelo SMAP poderao ser ajustado, utilizando-se as barras no painel lateral. Mais detalhes desses valores sao fornecidos na descricao do modelo na próxima aba."),
                   p("A visualizacao do grafico de vazões observadas e estimadas sera ajustada automaticamente, bem como a estimativa do coeficiente de Nash-Sutcliffe e a relacao entre escoamento de base e superficial estimada pelo SMAP."),
                   
                   h4("Otimizacao"),
                   p("No quarto painel, o/a usuario/a podera otimizar os parametros calibrados por meio do algoritmo genetico Multi-Objective Particle Swarm Optimization (MOPSO) (Nascimento et al., 2009). 4 Funcões objetivo diferentes sao admitidas: o coeficiente de Nash-Sutcliffe, a curva de permanência de vazões, e a curva de permanência considerando apenas as vazões altas ou baixas. Neste painel, as condicões da bacia nao sao calibradas, sao apenas inseridos pelo/a usuario/a."),
                   p("Para os seis parametros do modelo, o/a usuario/a devera escolher uma faixa que acredite conter os melhores parametros do modelo. Sugere-se, que essa faixa contenha os valores estimados no painel anterior. O/A usuario/a deve inserir um limite inferior e superior para os parametros. O/A usuario/a tambem podera alterar os parametros do algoritmo MOPSO e ao final clicar em \"Rodar\"."),
                   
                   h4("Validacao"),
                   p("Na ultima aba, o/a usuario/a podera visualizar o resultado do modelo calibrado com dados de validacao. Os dados otimizados podem ser inseridos e o/a usuario/a podera ter ideia do desempenho do modelo com dados fora da amostra de calibracao. Tambem e estimado o coeficiente de Nash para a amostra de validacao."),
                   br(),br(),br(),br()
                 ),
                 
                 tabPanel(
                   h4("O SMAP Diario"),
                   h4(strong("O modelo hidrológico SMAP com passo diario")),
                   
                   p("O SMAP (Soil Moisture Accounting Procedure) e um modelo deterministico de simulacao hidrológica do tipo transformacao chuva-vazao. e classificado como um modelo concentrado conceitual, construido com possibilidades de rodar em passos diario, mensal e horario. Neste aplicativo, apenas a versao diaria e apresentada. Neste aplicativo, dados gerais quanto à formulacao e aplicacao do modelo sao apresentados. Para maiores detalhes, consultar as publicacões que deram origem ao modelo (Lopes et al., 1991), bem como o manual do SMAP que pode ser obtivo em Lopes (1999)."),
                   p("Em sua versao diaria, o modelo SMAP e constituido de três reservatórios e o estado desses reservatórios e atualizado a cada passo do modelo. Esses reservatórios sao:"),
                   tags$ul(
                     tags$li("Rsolo, ou reservatório superficial do solo (zona aerada);"), 
                     tags$li("Rsup, ou reservatório da superficie da bacia;"), 
                     tags$li("Rsub, ou reservatório subterraneo (zona saturada).")
                   ),
                   
                   p("Os fluxos entre os reservatórios sao:"),
                   tags$ul(
                     tags$li("P (chuva)"), 
                     tags$li("Es (escoamento superficial)"), 
                     tags$li("Ed (escoamento direto)"),
                     tags$li("Er (evapotranspiracao real)"), 
                     tags$li("Rec (recarga subterranea)"), 
                     tags$li("Eb (escoamento de base)")
                   ),
                   
                   p("Os parametros do modelo sao:"),
                   tags$ul(
                    tags$li("str ou capacidade de saturacao do solo (mm)"), 
                    tags$li("k2t ou constante de recessao do escoamento superficial (dias)"), 
                    tags$li("crec ou parametro de recarga subterranea (%)"),
                    tags$li("ai ou abstracao inicial (mm)"), 
                    tags$li("capc ou capacidade de campo (%)"), 
                    tags$li("kkt ou constante de recessao do escoamento basico (dias)")
                    ),
                   
                   p("Alem desses parametros, duas condicões iniciais da baxia sao necessarias: seu teor de umidade inicial (tuin, em %) e o escoamento de base inicial (ebin, em m³/s)"),
                   
                   p("Um fluxograma dos fluxos de agua entre os reservatórios do modelo SMAP e apresentado a seguir."),
                   img(src = "SMAP_2.jpg", height = 450, width = 600)
                   ),
                 
                 tabPanel(h4("O MOPSO"),
                   h4(em(strong("Multiobjective Particle Swarm Optimization"))),
                   p("O algoritmo MOPSO e um algoritmo de otimizacao que incorpora uma necessidade de atender a multiplos objetivos em um algoritmo de ptimizacao do tipo PSO ou ",em("Particle Swarm Optimization")),
                   p("Segundo Nascimento", em("et al."), "(2009), o metodo PSO", em("consiste em um metodo de simulacao baseado no comportamento social de bandos, no qual particulas, ou melhor, individuos como passaros, insetos ou peixes fazem uso de suas experiências e da experiência do próprio bando para encontrarem o ninho, alimento, ou outro objetivo. Assim, dispostas de forma aleatória, as particulas seguem em busca de um local que satisfaca um dado objetivo")),
                   p("Quando aplicado para otimizacao de parametros de modelos hidrológicos SMAP, os \"bandos\" correspondem ao conjunto de parametros do modelo hidrológico e o objetivo desse bando e minimizar a funcao objetivo. No caso deste aplicativo, a funcao objetivo a ser minimizada, e o coeficiente de Nash-Sutcliffe."),
                   p("O algoritmo MOPSO requer a introducao dos seguintes parametros:"),
                   
                   tags$ul(
                     tags$li("Particulas: numero de particulas do algoritmo genetico."), 
                     tags$li("Geracões: quantidade maxima de geracões do algoritmo - coresponde ao numero maximo de interacões do algoritmo."), 
                     tags$li("c1 e c2: grau de confianca ao melhor parametro de todas as particulas ou de cada particula individualmente, respectivamente."),
                     tags$li("w: inercia da particula."), 
                     tags$li("n: numero de parametros que serao otimizados")
                   ),
                   
                   p("Caso o/a usuario/a deseje obter  maiores detalhes acerca do metodo, sugere-se consultar o artigo Nascimento", em("et al."), "(2009), intitulado Avaliacao do Algoritmo Evolutivo Mopso na Calibracao Multiobjetivo do Modelo SMAP no Estado do Ceara, publicado pela Revista Brasileira de Recursos Hidricos.")
                   )
               )
               )
             ),
    
    # Aba em que sao inseridos dados da bacia e os dados hidrológicos
    tabPanel(h4("Dados"),
             sidebarPanel(
               tabsetPanel(
                 tabPanel("Dados da bacia",
                          
                          tabPanel("Dados da bacia",
                                   textInput("bacia",
                                             h4("Nome da bacia"), 
                                             value = "Nome"),
                                   tags$hr(),
                                   numericInput("area", 
                                                h4("area de drenagem (km²)"), 
                                                value = 1)
                                   )
                          
                 ),
                 
                 tabPanel("Upload de dados",
                          
                          fileInput("file1", "Escolha arquivo csv",
                                    multiple = FALSE,
                                    buttonLabel = "Busque o arquivo...",
                                    placeholder = "Nenhum arquivo selecionado",
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          tags$hr(),
                          
                          checkboxInput("header", "Apresentar cabecalho", TRUE),
                          
                          tags$hr(),
                          
                          # Separador de colunas do arquivo de entrada
                          radioButtons("sep", "Separados de coluna",
                                       choices = c(Virgula = ",",
                                                   Ponto_e_Virgula = ";",
                                                   Tab = "\t"),
                                       selected = ";"),
                          
                          tags$hr(),
                          
                          # Separador de casas decimais do arquivo de entrada
                          radioButtons("dec", "Decimal",
                                       choices = c(Virgula = ",",
                                                   Ponto = "."),
                                       selected = ","),
                          
                          tags$hr(),
                          
                          # Apresentar primeiras ou ultimas linhas da tabela do arquivo
                          radioButtons("disp", "Apresentar",
                                       choices = c(Inicio = "head",
                                                   Fim = "tail"),
                                       selected = "head"),
                          
                          
                          )
                 
               )
               
             ),
             # Painel principal da aba de dados com apresentacao da tabela e grafico
             mainPanel(
               tabsetPanel(
                 tabPanel("Dados",
                   tableOutput("contents"),
                   tags$hr(),
                   textOutput("warning1"),
                   tags$hr(),
                   textOutput("warning2")
                 ),
                 tabPanel("Graficos",
                   plotlyOutput("serie_Q")
                 )
               )
    
             )
             
             ),
    
    # Aba de simulacao do SMAP
    tabPanel(h4("SMAP"), 
             
             # Na aba lateral os parametros do SMAP podem ser ajustados
             sidebarPanel(h3("Parametros"),
                          
                          dateRangeInput("datesCal", 
                                         label = "Periodo de calibracao do SMAP",
                                         start = "2017-10-01",
                                         end = "2019-12-31"),
                          
                          tags$hr(),
                          h4("Condicões iniciais"),
                          numericInput("tuin", "Entre com o valor de tuin (%)", 18),
                          numericInput("ebin", "Entre com o valor de ebin (m³/s)", 52),
                          
                          tags$hr(),
                          h4("Parametros do modelo"),
                          sliderInput("sat",
                                      "Capacidade de saturacao do solo (mm):",
                                      min = 1,
                                      max = 1000,
                                      value = 100),
                          
                          sliderInput("k2t",
                                      "Coeficiente recessao escoamento superficial (dias):",
                                      min = 0.1,
                                      max = 100,
                                      value = 15),
                          
                          sliderInput("crec",
                                      "Coeficiente de recarga da agua subterranea (%):",
                                      min = 0.1,
                                      max = 100,
                                      value = 50),
                          
                          sliderInput("ai",
                                      "Abstracao inicial (mm):",
                                      min = 1,
                                      max = 100,
                                      value = 30),
                          
                          sliderInput("capcc",
                                      "Capacidade de campo (%):",
                                      min = 0,
                                      max = 100,
                                      value = 90),
                          
                          sliderInput("kkt",
                                      "Coeficiente de recessao da agua subterranea (dias):",
                                      min = 1,
                                      max = 200,
                                      value = 15),
                          br(),
                          tags$hr(),
                          downloadButton("downloadData", 'Download')
                          
             ),
             
             # Visualizacao dos resultados
             mainPanel(h3("Resultado das simulacões"),
               h4("Eficiência Nash–Sutcliffe:"),
               textOutput("nashSutcliffe"),
               tags$hr(),
               h4("Escoamento de base / Escoamento Superficial:"),
               textOutput("baseCoef"),
               tags$hr(),
               h4("Vazões observadas e vazões estimadas pelo modelo:"),
               plotlyOutput("smapQcalc"),
               tags$hr(),
               h4("Curvas de permanência observada e estimada pelo modelo:"),
               plotlyOutput("permCurva"),
               tags$hr()
               )
             ),
    
    # Aba em que e feita a otimizacao, por meio do MOPSO
    tabPanel(h4("Otimizacao"), 
             
             sidebarPanel(
               
               # Condicões iniciais do SMAP e limites de busca para o MOPSO
               h3("Parametros da otimizacao"),
               
               tags$hr(),
               h4("Funcao objetivo"),
               selectInput("objetivo", strong("Escolha a funcao objetivo"),
                           choices = c("Nash-Sutcliffe" = "NS",
                                       "Permanência" = "Perm",
                                       "Permanência Inferior" = "Inf",
                                       "Permanência Superior" = "Sup"),
                           multiple = FALSE, selected = "NS"),
               
               tags$hr(),
               h4("Condicões iniciais"),
               numericInput("tuinOpt", "Entre com o valor de tuin (%)", 18),
               numericInput("ebinOpt", "Entre com o valor de ebin (m³/s)", 52),
               
               tags$hr(),
               h4("Limites para busca na calibracao"),
               
               sliderInput("satOpt",
                           label = "Capacidade de saturacao do solo (mm):",
                           min = 0, max = 5000, value = c(100, 500)),
               sliderInput("k2tOpt",
                           label = "Coeficiente recessao escoamento superficial (dias):",
                           min = 0, max = 200, value = c(25, 75)),
               sliderInput("crecOpt",
                           label = "Coeficiente de recarga da agua subterranea (%):",
                           min = 0, max = 100, value = c(25, 75)),
               sliderInput("aiOpt",
                           label = "Abstracao inicial (mm):",
                           min = 0, max = 100, value = c(25, 75)),
               sliderInput("capccOpt",
                           label = "Capacidade de campo (%):",
                           min = 0, max = 100, value = c(25, 75)),
               sliderInput("kktOpt",
                           label = "Coeficiente de recessao da agua subterranea (dias):",
                           min = 0, max = 200, value = c(25, 75)),
               
               tags$hr(),
               h4("Parametros do MOPSO"),
               numericInput("particles", label = "Particulas", value = 50),
               numericInput("max_ger", label = "Maximo de geracões de particulas", value = 100),
               numericInput("c1", label = "Confianca na melhor particula", value = 1),
               numericInput("c2", label = "Confianca na simulacao individual", value = 1),
               numericInput("w", label = "Inercia da particula", value = 1),
               numericInput("n", label = "Numero de parametros do modelo", value = 6),
               
               tags$hr(),
               br(),
               actionButton("optim", "Rodar"),
               
               br(),br(),
               
               tags$hr(),
               helpText("Atencao! O download só e disponivel depois de rodar a otimizacao!"),
               shinyjs::useShinyjs(),
               downloadButton("downloadData2", 'Download')
               
               ),
             
             mainPanel(h3("Resultado da otimizacao"),
                       verbatimTextOutput("paraOpt"),
                       plotlyOutput("smapOutOpt"),
                       
                       h4("Curvas de permanência observada e estimada após otimização:"),
                       plotlyOutput("permCurvaOpt"),
                       
                       tags$hr(),
                       h4("Coeficiente de Nash Otimizado"),
                       verbatimTextOutput("nsOpt")
             )
             
             ),
    
    # Aqui o usuario pode inserir a amostra de validacao, bem como inserir dados novos
    tabPanel(h4("Validacao"),
             
             sidebarPanel(h3("Parametros da Validacao"),
                          tags$hr(),
                          h4("Parametros de inicializacao do modelo:"),
                          numericInput("tuinVal", "Entre com o valor de tuin (%)", 18),
                          numericInput("ebinVal", "Entre com o valor de ebin (m³/s)", 52),
                          
                          tags$hr(),
                          h4("Parametros ótimos do modelo SMAP:"),
                          numericInput("satVal",
                                       label = "Capacidade de saturacao do solo (mm):", 500),
                          numericInput("k2tVal",
                                       label = "Coeficiente recessao escoamento superficial (dias):", 50),
                          numericInput("crecVal",
                                       label = "Coeficiente de recarga da agua subterranea (%):", 50),
                          numericInput("aiVal",
                                       label = "Abstracao inicial (mm):", 50),
                          numericInput("capccVal",
                                       label = "Capacidade de campo (%):", 50),
                          numericInput("kktVal",
                                       label = "Coeficiente de recessao da agua subterranea (dias):", 50),
                          
                          tags$hr(),
                          h4("Periodo de validacao:"),
                          dateRangeInput("datesVal",
                                         label = "Inicio e fim da validacao",
                                         start = "2012-10-01",
                                         end = "2017-09-30"),
                          
                          tags$hr(),
                          downloadButton("downloadData3", 'Download')
                          
                          ),
             
             mainPanel(
               h3("Resultado da validacao"),
               h4("Sugestao de parametros:"),
               verbatimTextOutput("paraOpt2"),
               plotlyOutput("plotVal"),
               h4("Coeficiente de Nash-Sutcliffe para o periodo de validacao"),
               verbatimTextOutput("nsVal")
               )
             
             
             
             )
    
  )
)

# Funcao server
# Sempre que necessario, para se evitar travamento do programa, a funcao tryCatch foi utilizada
server <- function(input, output) {
  
  # show intro modal
  observeEvent("", {
    showModal(modalDialog(size = "l",
      includeHTML("intro_text.html"),
      easyClose = TRUE,
      footer = modalButton("Sair da introducao")
    ))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  # Leitura de arquivo .csv de entrada
  dataInput <- reactive({
    
    req(input$file1)
    
    tryCatch({
      
      df <- read.table(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       dec = input$dec)
      
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    
  })
  
  # Opcao de visualizacao de cabecalho ou fim da tabela
  output$contents <- renderTable({
    
    if(input$disp == "head") {
      return(head(dataInput(), 13))
    } else {
      return(tail(dataInput(), 13))
    }
    
  }) 
  
  # Verifica se os nomes das colunas estao corretos
  output$warning1 <- renderText({
    CS <- sum(grepl("E", colnames(dataInput())))+
      sum(grepl("P", colnames(dataInput())))+
      sum(grepl("Q", colnames(dataInput())))+
      sum(grepl("Data", colnames(dataInput())))
    
    if(CS < 4){
      return("Ha problemas nos cabecalhos da tabela.
             O arquivo de entrada deve ter ao menos 4 colunas com nomes: Data, E, P e Q")
    } else {
      return("Nomes das colunas estao corretos.")
    }
  })
  
  # Verifica se as datas estao corretas, sequencial com 1 dia de espacamento entre elas
  output$warning2 <- renderText({
    Datas <- as.Date(dataInput()[,"Data"], "%d/%m/%Y")
    DifData <- head(diff(Datas))
    return(ifelse(sum(DifData > 1) > 0,
           "Existem datas faltantes, corrija para correta visualizacao dos dados",
           "As datas estao ok!"))
    })
  
  # Gera o grafico de visualizacao dos dados de vazao
  output$serie_Q <- renderPlotly({
    tryCatch({
      dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                        as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                        by = "day")
      dados <- dataInput()
      
      a <- list(
        
      )
      
      plot1 <- plot_ly(
        x = dates,
        y = dados$Q,
        type = 'scatter',
        mode = 'lines',
        hoverinfo = 'text',
        text = ~paste('</br> Data: ', dates,
                      '</br> Vazão: ', round(dados$Q, 1))) %>%
        layout(title = paste("Vazão na bacia do rio", input$bacia,
                             "- Área de drenagem =", input$area, "km²"),
               xaxis = list(title = "Data"),
               yaxis = list (title = "Vazão (m³/s)"))
      
      },
      error = function(e) {
        plot1 <- plot_ly(
          x = 1,
          y = 1,
          type = 'scatter',
          mode = 'lines') %>%
          layout(title = paste("Vazão na bacia do rio", input$bacia),
                 annotations = paste("area de drenagem", input$area, "km²"),
                 xaxis = list(title = "Data"),
                 yaxis = list (title = "Vazão (m³/s)"))
        
        })
    })
  
  # Gera as visualizacões do resultado do SMAP com parametros introduzidos pelo/a usuario/a
  smapOutput <- reactive({
    
    # Limita as datas
    dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                      as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                      by = "day")
    
    epq <- dataInput()
    epq <- epq[dates >= input$datesCal[1] & dates <= input$datesCal[2],]
    epq <- select(epq, c("E","P","Q"))
    
    para <- array(NA, 6)
    names(para) <- c("sat","k2t","crec","ai","capcc","kkt")
    
    inic <- array(NA, 2)
    names(inic) <- c("tuin","ebin")
    inic[1] <- input$tuin
    inic[2] <- input$ebin
    
    para[1] <- input$sat
    para[2] <- input$k2t
    para[3] <- input$crec
    para[4] <- input$ai
    para[5] <- input$capcc
    para[6] <- input$kkt
    
    return(smap_daily(epq, para, inic, input$area))
    
  })
  
  # Gera tabela para download
  smapfileout <- reactive({
    
    dates <- seq.Date(input$datesCal[1], input$datesCal[2], by = "day")
    return(
      data.frame(
        "Dates" = dates,
        "Qcalc" = smapOutput()$Qcalc,
        "Qbase" = smapOutput()$Qbase,
        "Qsup" = smapOutput()$Qsup,
        "Rsol" = smapOutput()$Rsol[-length(smapOutput()$Rsol)],
        "Rsub" = smapOutput()$Rsub[-length(smapOutput()$Rsub)],
        "Rsup" = smapOutput()$Rsup[-length(smapOutput()$Rsup)],
        "Es" = smapOutput()$Es,
        "Eb" = smapOutput()$Eb,
        "Ed" = smapOutput()$Ed,
        "Er" = smapOutput()$Er,
        "Rec" = smapOutput()$Rec,
        "tu" = smapOutput()$tu
      )
    )
    
  })
  
  # Botao para download do resultado da simulacao
  output$downloadData <- downloadHandler(
    filename = function() paste0(input$bacia, "_SMAP.csv"),
    content = function(file) {
      write.csv2(smapfileout(), file, row.names = FALSE)
    }
  )
  
  # Gera grafico do SMAP com a vazao automaticamente
  output$smapQcalc <- renderPlotly({
    
    tryCatch({
      
      dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                        as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                        by = "day")
      
      datesSimul <- dates[dates >= input$datesCal[1] & dates <= input$datesCal[2]]
      
      smapRes <- smapOutput()
      obs <- dataInput()
      obs <- obs[dates >= input$datesCal[1] & dates <= input$datesCal[2],]
      
      plot1 <- plot_ly(
        x = datesSimul,
        y = obs$Q,
        type = 'scatter',
        mode = 'lines',
        name = "Observada",
        hoverinfo = 'text',
        text = ~paste('</br> Data: ', datesSimul,
                      '</br> Vazão: ', round(obs$Q, 1))) %>%
        layout(title = paste("Vazão observada e simulada na bacia do rio", input$bacia),
               xaxis = list(title = "Data"),
               yaxis = list (title = "Vazão (m³/s)")) %>%
        add_trace(x = datesSimul, y = smapRes$Qcalc, name = 'Simulada',
                  mode = 'lines',
                  text = ~paste('</br> Data: ', datesSimul,
                                '</br> Vazão: ', round(smapRes$Qcalc, 1)))%>%
        add_trace(x = datesSimul, y = smapRes$Qbase, name = 'Escoamento de Base',
                  mode = 'lines',
                  text = ~paste('</br> Data: ', datesSimul,
                                '</br> Vazão: ', round(smapRes$Qbase, 1)))  
      
    },
    
    error = function(e) {
      
      plot1 <- plot_ly(
        x = 1,
        y = 1,
        type = 'scatter',
        mode = 'lines') %>%
        layout(title = paste("Vazão na bacia do rio", input$bacia),
               annotations = paste("area de drenagem", input$area, "km²"),
               xaxis = list(title = "Data"),
               yaxis = list (title = "Vazão (m³/s)"))
      
      })
    
  })
  
  #### Curva de permanência
  
  output$permCurva <- renderPlotly({
    
    tryCatch({
      
      dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                        as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                        by = "day")
      
      datesSimul <- dates[dates >= input$datesCal[1] & dates <= input$datesCal[2]]
      
      smapRes <- smapOutput()
      obs <- dataInput()
      obs <- obs[dates >= input$datesCal[1] & dates <= input$datesCal[2],]
      
      Probs <- seq(1,0,-0.01)
      ObsQ <- as.numeric(quantile(obs$Q, probs = 1-Probs, na.rm = TRUE))
      SimulQ <- as.numeric(quantile(smapRes$Qcalc, probs = 1-Probs, na.rm = TRUE))
      
      plot1 <- plot_ly(
        x = Probs,
        y = ObsQ,
        type = 'scatter',
        mode = 'lines',
        name = "Observada",
        hoverinfo = 'text',
        text = ~paste('</br> Probabilidade: ', Probs,
                      '</br> Vazão: ', round(ObsQ, 1))) %>%
        layout(title = paste("Curvas de permanência observada e simulada na bacia do rio", input$bacia),
               xaxis = list(title = "Probabilidade"),
               yaxis = list (title = "Vazão (m³/s)")) %>%
        add_trace(x = Probs, y = SimulQ, name = 'Simulada',
                  mode = 'lines')
      
    },
    
    error = function(e) {
      
      plot2 <- plot_ly(
        x = 1,
        y = 1,
        type = 'scatter',
        mode = 'lines') %>%
        layout(title = paste("Vazão na bacia do rio", input$bacia),
               annotations = paste("area de drenagem", input$area, "km²"),
               xaxis = list(title = "Data"),
               yaxis = list (title = "Vazão (m³/s)"))
      
    })
    
  })
  
  # Atualiza o valor do coeficiente de Nash-Sutcliffe
  output$nashSutcliffe <- renderText({
    
    tryCatch({
      
      dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                        as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                        by = "day")
      
      datesSimul <- dates[dates >= input$datesCal[1] & dates <= input$datesCal[2]]
      smapRes <- smapOutput()
      obs <- dataInput()
      obs <- obs[dates >= input$datesCal[1] & dates <= input$datesCal[2],]
      return(round(1 - sum((smapRes$Qcalc - obs$Q)^2) / sum((obs$Q - mean(obs$Q))^2),4))
      
    },
    
    error = function(e) {
      
      print("Nao foi possivel calcular o coeficiente de Nash-Sutcliffe")
      
    })
    
  })
  
  output$baseCoef <- renderText({
    
    tryCatch({
      
      dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                        as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                        by = "day")
      
      datesSimul <- dates[dates >= input$datesCal[1] & dates <= input$datesCal[2]]
      smapRes <- smapOutput()
      return(mean(smapRes$Qbase) / mean(smapRes$Qcalc))
      
    },
    
    error = function(e) {
      
      print("Nao foi possivel calcular a relacao entre escoamento de base e total.")
      
    })
    
  })
  
  # Roda o algoritmo MOPSO
  Opt <- eventReactive(input$optim, {
    
    dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                      as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                      by = "day")
    
    epq <- dataInput()
    epq <- epq[dates >= input$datesCal[1] & dates <= input$datesCal[2],]
    epq <- select(epq, c("E","P","Q"))
    
    para <- array(NA, 6)
    names(para) <- c("sat","k2t","crec","ai","capcc","kkt")
    
    inic <- array(NA, 2)
    names(inic) <- c("tuin","ebin")
    inic[seq_along(inic)] <- c(input$tuinOpt, input$ebinOpt)
    
    POP <- matrix(NA, nrow = input$particles, ncol = input$n)
    limites <- cbind(Inferior = c(input$satOpt[1], input$k2tOpt[1], input$crecOpt[1], input$aiOpt[1], input$capccOpt[1], input$kktOpt[1]),
                     Superior = c(input$satOpt[2], input$k2tOpt[2], input$crecOpt[2], input$aiOpt[2], input$capccOpt[2], input$kktOpt[2]))
    
    for (i in 1:input$particles){  
      for (j in 1:input$n){
        POP[i,j] <- (limites[j,2] - limites[j,1]) * runif(1) + limites[j,1]
      }   
    }
    
    obj_func_particles <- array(NA, input$particles)
    obj_func_history <- array(NA, input$max_ger)
    coord_particles <- array(NA, input$particles)
    best_parameters <- matrix(NA, nrow = input$max_ger, ncol = input$n)
    vant <- matrix(0, nrow = input$particles, ncol = input$n)
    step <- (input$w - 0.4) / input$max_ger
    
    # Inicia a matriz com os valores maximos da velocidade
    vmax <- (limites[,2] - limites[,1]) * 0.2
    
    # Inicializando matriz "velocidade"
    v <- matrix(NA, nrow = input$particles, ncol = input$n)
    
    geracao <- 1
    while (geracao < input$max_ger) {
      
      for(i in 1:input$particles){
        para[seq_along(1:input$n)] <- POP[i,]
        
        if(input$objetivo == "NS"){
          obj_func_particles[i] <- Nash_Sutcliffe(epq[,"Q"],
                                                  smap_daily(epq, para, inic, input$area)$Qcalc)
        } else if (input$objetivo == "Perm") {
          obj_func_particles[i] <- prox_perm(epq[,"Q"],
                                                  smap_daily(epq, para, inic, input$area)$Qcalc)
        } else if (input$objetivo == "Inf") {
          obj_func_particles[i] <- prox_inf(epq[,"Q"],
                                             smap_daily(epq, para, inic, input$area)$Qcalc)
        } else {
          obj_func_particles[i] <- prox_sup(epq[,"Q"],
                                            smap_daily(epq, para, inic, input$area)$Qcalc)
        }
        
      }
      
      para[seq_along(1:input$n)] <- POP[which.min(obj_func_particles),]
      best_parameters[geracao,] <- para
      
      para_current <- array(NA, input$n)
      names(para_current) <- c("sat","k2t","crec","ai","capcc","kkt")
      para_current[seq_along(1:input$n)] <- POP[which.min(obj_func_particles),]
      
      if(geracao == 1){
        
        para_hist <- array(NA, input$n)
        names(para_hist) <- c("sat","k2t","crec","ai","capcc","kkt")
        obj_func_history[input$geracao] <- min(obj_func_particles)
        para_hist[seq_along(1:input$n)] <- POP[which.min(obj_func_particles),]
        
      } else {
        
        if(min(obj_func_particles) <
           min(obj_func_history[complete.cases(obj_func_history)])){
          
          obj_func_history[input$geracao] <- min(obj_func_particles)
          para_hist[seq_along(1:input$n)] <- POP[which.min(obj_func_particles),]
          
        } else {
          
          obj_func_history[geracao] <- obj_func_history[length(obj_func_history[complete.cases(obj_func_history)])]
        }
      }
      
      
      for (i in 1:input$particles){
        
        #Atualiza o vetor velocidade
        v[i,] <- (input$w * vant[i,]) +
          (input$c1 * runif(1) * (para_current - POP[i,]))+
          (input$c2 * runif(1) * (para_hist - POP[i,]))
        # Limita velocidade à velocidade maxima
        for(j in 1:input$n){
          if(abs(v[i,j]) > vmax[j]){
            v[i,j] = sign(v[i,j]) *vmax[j]
          }
        }
        
        # Atualiza a posicao da particula a partir da nova velocidade
        POP[i,] <- POP[i,] + v[i,] 
        # Atualiza a velocidade da particula para o passo posterior.
        vant[i] <- v[i] 
      }
      
      
      for(i in 1:input$n){
        for(j in 1:input$particles){
          if(POP[j,i] < limites[i,1]){
            POP[j,i] <- limites[i,1]
          }
          
          if(POP[j,i] > limites[i,2]){
            POP[j,i] <- limites[i,2]
          }
          
        }
      }
      
      return(para_hist)
      
      if(geracao > 5){
        criterio <- mean(abs(diff(tail(
          obj_func_history[complete.cases(obj_func_history)], 10)))) < 0.01
        if(criterio) stop("Converged")
      }
      
      geracao <- geracao + 1
      
    }
    
    
  })
  
  output$paraOpt <- renderText({
    
    Opt()
    
  })
  
  # Roda o SMAP com parametros otimizados
  smapOutput2 <- reactive({
    
    dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                      as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                      by = "day")
    
    epq <- dataInput()
    epq <- epq[dates >= input$datesCal[1] & dates <= input$datesCal[2],]
    epq <- select(epq, c("E","P","Q"))
    
    para <- array(NA, 6)
    names(para) <- c("sat","k2t","crec","ai","capcc","kkt")
    
    inic <- array(NA, 2)
    names(inic) <- c("tuin","ebin")
    inic[1] <- input$tuin
    inic[2] <- input$ebin
    
    para[1] <- Opt()[1]
    para[2] <- Opt()[2]
    para[3] <- Opt()[3]
    para[4] <- Opt()[4]
    para[5] <- Opt()[5]
    para[6] <- Opt()[6]
    
    return(smap_daily(epq, para, inic, input$area))
    
  })
  
  # Gera uma tabela para download do SMAP com os dados otimizados
  optfileout <- reactive({
    
    dates <- seq.Date(input$datesCal[1], input$datesCal[2], by = "day")
    
    return(
      data.frame(
        "Dates" = dates,
        "Qcalc" = smapOutput2()$Qcalc,
        "Qbase" = smapOutput2()$Qbase,
        "Qsup" = smapOutput2()$Qsup,
        "Rsol" = smapOutput2()$Rsol[-length(smapOutput2()$Rsol)],
        "Rsub" = smapOutput2()$Rsub[-length(smapOutput2()$Rsub)],
        "Rsup" = smapOutput2()$Rsup[-length(smapOutput2()$Rsup)],
        "Es" = smapOutput2()$Es,
        "Eb" = smapOutput2()$Eb,
        "Ed" = smapOutput2()$Ed,
        "Er" = smapOutput2()$Er,
        "Rec" = smapOutput2()$Rec,
        "tu" = smapOutput2()$tu
      )
    )
    
  })
  
  observe({
    if(input$optim > 0)
    shinyjs::enable("downloadData2")
    }
  )
  
  output$downloadData2 <- downloadHandler(
    
    filename = function() paste0(input$bacia, "_Opt_SMAP.csv"),
    
    content = function(file) {
      write.csv2(optfileout(), file, row.names = FALSE)
    }
  )
  
  output$smapOutOpt <- renderPlotly({
    
    tryCatch({
    
      dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                        as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                        by = "day")
      
      datesSimul <- dates[dates >= input$datesCal[1] & dates <= input$datesCal[2]]
      
      smapRes <- smapOutput2()
      obs <- dataInput()
      obs <- obs[dates >= input$datesCal[1] & dates <= input$datesCal[2],]
      
      plot1 <- plot_ly(
        x = datesSimul,
        y = obs$Q,
        type = 'scatter',
        mode = 'lines',
        name = "Observada",
        hoverinfo = 'text',
        text = ~paste('</br> Data: ', datesSimul,
                      '</br> Vazão: ', round(obs$Q, 1))) %>%
        layout(title = paste("Vazão observada e simulada na bacia do rio", input$bacia),
               xaxis = list(title = "Data"),
               yaxis = list (title = "Vazão (m³/s)")) %>%
        add_trace(x = datesSimul, y = smapRes$Qcalc, name = 'Simulada',
                  mode = 'lines',
                  text = ~paste('</br> Data: ', datesSimul,
                                '</br> Vazão: ', round(smapRes$Qcalc, 1)))%>%
        add_trace(x = datesSimul, y = smapRes$Qbase, name = 'Escoamento de Base',
                  mode = 'lines',
                  text = ~paste('</br> Data: ', datesSimul,
                                '</br> Vazão: ', round(smapRes$Qbase, 1))) 
    
    },
    
    error = function(e) {
      
      plot2 <- plot_ly(
        x = 1,
        y = 1,
        type = 'scatter',
        mode = 'lines') %>%
        layout(title = paste("Vazão na bacia do rio", input$bacia),
               annotations = paste("area de drenagem", input$area, "km²"),
               xaxis = list(title = "Data"),
               yaxis = list (title = "Vazão (m³/s)"))
      
    })
    
  })
  
  
  output$permCurvaOpt <- renderPlotly({
    
    tryCatch({
      
      dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                        as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                        by = "day")
      
      datesSimul <- dates[dates >= input$datesCal[1] & dates <= input$datesCal[2]]
      
      smapRes <- smapOutput2()
      obs <- dataInput()
      obs <- obs[dates >= input$datesCal[1] & dates <= input$datesCal[2],]
      
      Probs <- seq(1,0,-0.01)
      ObsQ <- as.numeric(quantile(obs$Q, probs = 1-Probs, na.rm = TRUE))
      SimulQ <- as.numeric(quantile(smapRes$Qcalc, probs = 1-Probs, na.rm = TRUE))
      
      plot1 <- plot_ly(
        x = Probs,
        y = ObsQ,
        type = 'scatter',
        mode = 'lines',
        name = "Observada",
        hoverinfo = 'text',
        text = ~paste('</br> Probabilidade: ', Probs,
                      '</br> Vazão: ', round(ObsQ, 1))) %>%
        layout(title = paste("Curvas de permanência observada e simulada na bacia do rio", input$bacia),
               xaxis = list(title = "Probabilidade"),
               yaxis = list (title = "Vazão (m³/s)")) %>%
        add_trace(x = Probs, y = SimulQ, name = 'Simulada',
                  mode = 'lines')
      
    },
    
    error = function(e) {
      
      plot2 <- plot_ly(
        x = 1,
        y = 1,
        type = 'scatter',
        mode = 'lines') %>%
        layout(title = paste("Vazão na bacia do rio", input$bacia),
               annotations = paste("area de drenagem", input$area, "km²"),
               xaxis = list(title = "Data"),
               yaxis = list (title = "Vazão (m³/s)"))
      
    })
    
  })
  

  output$nsOpt <- renderText({
    
    dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                      as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                      by = "day")
    
    datesSimul <- dates[dates >= input$datesCal[1] & dates <= input$datesCal[2]]
    
    smapRes <- smapOutput2()
    obs <- dataInput()
    obs <- obs[dates >= input$datesCal[1] & dates <= input$datesCal[2],]
    
    return(1 - sum((smapRes$Qcalc - obs$Q)^2) / sum((obs$Q - mean(obs$Q))^2))
    
    
  })
  
  
  smapOutputVal <- reactive({
    
    dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                      as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                      by = "day")
    
    epq <- dataInput()
    epq <- epq[dates >= input$datesVal[1] & dates <= input$datesVal[2],]
    epq <- select(epq, c("E","P","Q"))
    
    para <- array(NA, 6)
    names(para) <- c("sat","k2t","crec","ai","capcc","kkt")
    
    inic <- array(NA, 2)
    names(inic) <- c("tuin","ebin")
    
    inic[1] <- input$tuinVal
    inic[2] <- input$ebinVal
    
    para[1] <- input$satVal
    para[2] <- input$k2tVal
    para[3] <- input$crecVal
    para[4] <- input$aiVal
    para[5] <- input$capccVal
    para[6] <- input$kktVal
    
    return(smap_daily(epq, para, inic, input$area))
    
  })
  
  smapfileout3 <- reactive({
    
    n <- length(smapOutputVal()$Qcalc)
    dates <- seq.Date(from = input$datesVal[1], length.out = n, by = "day")
    
    return(
      data.frame(
        "Dates" = dates,
        "Qcalc" = smapOutputVal()$Qcalc,
        "Qbase" = smapOutputVal()$Qbase,
        "Qsup" = smapOutputVal()$Qsup,
        "Rsol" = smapOutputVal()$Rsol[-length(smapOutputVal()$Rsol)],
        "Rsub" = smapOutputVal()$Rsub[-length(smapOutputVal()$Rsub)],
        "Rsup" = smapOutputVal()$Rsup[-length(smapOutputVal()$Rsup)],
        "Es" = smapOutputVal()$Es,
        "Eb" = smapOutputVal()$Eb,
        "Ed" = smapOutputVal()$Ed,
        "Er" = smapOutputVal()$Er,
        "Rec" = smapOutputVal()$Rec,
        "tu" = smapOutputVal()$tu
      )
    )
    
  })
  
  output$downloadData3 <- downloadHandler(
    
    filename = function() paste0(input$bacia, "_Val_SMAP.csv"),
    
    content = function(file) {
      write.csv2(smapfileout3(), file, row.names = FALSE)
    }
  )
  
  output$paraOpt2 <- renderText({
    
    Opt()
    
  })
  
  output$plotVal <- renderPlotly({
    
    tryCatch({
      
      dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                        as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                        by = "day")
      datesSimul <- dates[dates >= input$datesVal[1] & dates <= input$datesVal[2]]
      
      smapRes <- smapOutputVal()
      obs <- dataInput()
      obs <- obs[dates >= input$datesVal[1] & dates <= input$datesVal[2],]
      
      plot1 <- plot_ly(
        x = datesSimul,
        y = obs$Q,
        type = 'scatter',
        mode = 'lines',
        name = "Observada",
        hoverinfo = 'text',
        text = ~paste('</br> Data: ', datesSimul,
                      '</br> Vazão: ', round(obs$Q, 1))) %>%
        layout(title = paste("Vazão observada e simulada na bacia do rio", input$bacia),
               xaxis = list(title = "Data"),
               yaxis = list (title = "Vazão (m³/s)")) %>%
        add_trace(x = datesSimul, y = smapRes$Qcalc, name = 'Simulada',
                  mode = 'lines',
                  text = ~paste('</br> Data: ', datesSimul,
                                '</br> Vazão: ', round(smapRes$Qcalc, 1)))%>%
        add_trace(x = datesSimul, y = smapRes$Qbase, name = 'Escoamento de Base',
                  mode = 'lines',
                  text = ~paste('</br> Data: ', datesSimul,
                                '</br> Vazão: ', round(smapRes$Qbase, 1))) 
      
      
      #plot(datesSimul, obs$Q, bty = "n", type = "l", xlab = "", ylab = "")
      #lines(datesSimul, smapRes$Qcalc, col = 2)
      #lines(datesSimul, smapRes$Qbase, col = 4, lty = 2)
      #mtext("Vazao (m³/s)", line = 2.5, side = 2)
      #mtext("Data", line = 2.5, side = 1)
      
      
    },
    
    error = function(e) {
      
      print("Nao foi possivel calcular o SMAP para o periodo de validacao.")
      
    })
  })
  
  
  output$nsVal <- renderText({
    
    tryCatch({
      
      dates <- seq.Date(as.Date(dataInput()$Data[1], "%d/%m/%Y"),
                        as.Date(dataInput()$Data[length(dataInput()$Data)], "%d/%m/%Y"),
                        by = "day")
      datesSimul <- dates[dates >= input$datesVal[1] & dates <= input$datesVal[2]]
      
      smapRes <- smapOutputVal()
      obs <- dataInput()
      obs <- obs[dates >= input$datesVal[1] & dates <= input$datesVal[2],]
      Index <- complete.cases(obs$Q)
      return(1 - sum((smapRes$Qcalc[Index] - obs$Q[Index])^2, na.rm = TRUE) /
               sum((obs$Q[Index] - mean(obs$Q[Index]))^2, na.rm = TRUE))
      
    },
    
    error = function(e) {
      
      print("Nao foi possivel calcular o SMAP para o periodo de validacao.")
      
    })
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

