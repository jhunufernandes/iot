infoC = read.table("/home/laftos/Projetos/MedidorPoluicao/Dados/Configuracao/listaSensor.csv", sep = ";", dec = ".", header = T)

barrLado = function(){
  d = dashboardSidebar(disable = FALSE,
                       sidebarMenu(
                         menuItem("Monitoramento", tabName = "mmu_realTmeGeral", icon = icon("binoculars")),
                         menuItem("Histórico", tabName = "historico", icon = icon("align-left"),
                                  menuSubItem("Poluição", tabName = "mmu_histPoluicao", icon = icon("globe")),
                                  menuSubItem("Temperatura", tabName = "mmu_histTemperatura", icon = icon("text-height")),
                                  
                                  
                                  radioButtons("radPeriodo", label = "Selecione o período a ser mostrado:", 
                                               choiceNames = c("Último dia", "Última semana"),
                                               choiceValues = c("ultDia", "ultSemana")),
                                  
                                  radioButtons("radAgrega", label = "Selecione um método de agregação:",
                                               choiceNames = c("Não agregar", "Máximo", "3° Quartil", "Mediana", "1° Quartil", "Mínimo"),
                                               choiceValues = c("agrN","agrMax", "agr3Q", "agrMed", "agr1Q", "agrMin"))
                                  # # dateRangeInput("dateIntervalo", label = "Selecione um período para a consulta:", 
                                  #                start = inicioMax, end = as.Date(now()), min = inicioMax,
                                  #                max = as.Date(now()), format = "dd-mm-yyyy", startview = "day", weekstart = 0,
                                  #                language = "pt_BR", separator = " até ")
                         ),
                         menuItem("Configuração", tabName = "mmu_config", icon = icon("cog")),
                         menuItem("Equipe", tabName = "mmu_equipe", icon = icon("users")),
                         menuItem("Sobre", tabName = "mmu_sobre", icon = icon("question"))
                       )
  )
  return(d)
}

cabecalho = function(){
  d = dashboardHeaderPlus(
    title = " MONITORAMENTO DE POLUIÇÃO  ",
    dropdownMenuOutput("messageMenu")
  )
  return(d)
}

poluicao_pageHistorico = function(){
  d = tabItem(tabName = "mmu_histPoluicao",
              h2("HISTÓRICO POLUIÇÃO", align = "center"),
              fluidRow(
                infoBoxOutput("box_poluicaoMax", width = 4),
                infoBoxOutput("box_poluicaoMedia", width = 4),
                infoBoxOutput("box_poluicaoMin", width = 4)
                #infoBoxOutput("box_poluicaoAmplitude", width = 3)
                
              ),
              fluidRow(
                infoBoxOutput("box_poluicaoQualiMaisFreq", width = 4),
                infoBoxOutput("box_poluicaoQualiMenosFreq", width = 4),
                infoBoxOutput("box_poluicaoQdt", width = 4)
                #infoBoxOutput("global_box")
              ),
              fluidRow(
                box(title = "Mapa do nível de poluição",
                    leafletOutput("map_histPoluicao", height = 530),
                    width = 12, collapsible = TRUE, status = "primary",
                    solidHeader = T, height = 600)
              ),
              fluidRow(
                box(title = "Série do nível de poluição",
                    plotlyOutput("plot_histPoluicao"),
                    width = 12, collapsible = TRUE, status = "primary",
                    solidHeader = T)
              )
  )
  return(d)
}

temperatura_pageHistorico = function(){
  d = tabItem(tabName = "mmu_histTemperatura",
              h2("HISTÓRICO TEMPERATURA", align = "center"),
              fluidRow(
                infoBoxOutput("box_temperaturaMax", width = 4),
                infoBoxOutput("box_temperaturaMedia", width = 4),
                infoBoxOutput("box_temperaturaMin", width = 4)
                #infoBoxOutput("box_temperaturaAmplitude", width = 3)
                
              ),
              fluidRow(
                infoBoxOutput("box_temperaturaQualiMaisFreq", width = 4),
                infoBoxOutput("box_temperaturaQualiMenosFreq", width = 4),
                infoBoxOutput("box_temperaturaQdt", width = 4)
                #infoBoxOutput("global_box")
              ),
              fluidRow(
                box(title = "Mapa de temperatura",
                    leafletOutput("map_histTemperatura", height = 530),
                    width = 12, collapsible = TRUE, status = "primary",
                    solidHeader = T, height = 600)
              ),
              fluidRow(
                box(title = "Série da temperatura",
                    plotlyOutput("plot_histTemperatura"),
                    width = 12, collapsible = TRUE, status = "primary",
                    solidHeader = T)
              )
  )
  return(d)
}

geral_pageRealTime = function(){
  d = tabItem(tabName = "mmu_realTmeGeral",
              h2("MONITORAMENTO EM TEMPO REAL", align = "center"),
              fluidRow(
                valueBoxOutput("box_poluicaoAtual", width = 5),
                valueBoxOutput("box_temperaturaAtual", width = 5),
                valueBoxOutput("box_ultimaAtualizacao", width = 2)
                
              ),
              fluidRow(
                box(title = "Localização Atual",
                    leafletOutput("map_atual"),
                    width = 12, collapsible = TRUE, status = "primary",
                    solidHeader = T)
              )
  )
  return(d)
}


config_page = function(){
  d = tabItem(tabName = "mmu_config",
              h2("CONFIGURAÇÕES", align = "center"),
              column(width = 2),
              boxPlus(
                checkboxInput("chk_alarmes", "Enviar alertas por e-mail?", infoC$ALARME),
                checkboxInput("chk_emailDiario", "Enviar relatório diário por e-mail?", infoC$EMAIL_DIARIO),
                checkboxInput("chk_emailSemanal", "Enviar relatório semanal por e-mail?", infoC$EMAIL_SEMANAL),
                checkboxInput("chk_emailMensal", "Enviar relatório mensal por e-mail?", infoC$EMAIL_MENSAL),
                textInput("txt_configEmail", "Novo e-mail:", value = infoC$EMAIL),
                passwordInput("txt_configSenha", "Nova senha:", value = infoC$SENHA),
                actionButton("btn_salvar", "Salvar"),
                width = 8, collapsible = TRUE, 
                closable = FALSE, 
                status = "warning",
                solidHeader = T),
              column(width = 2)
  )
  return(d)
}

equipe_page = function(){
  d = tabItem(tabName = "mmu_equipe",
              fluidRow(
                widgetUserBox(
                  title = "Leonardo Fonseca Larrubia",
                  subtitle = "8940960",
                  type = NULL,
                  width = 6,
                  src = "https://scontent.fcgh15-1.fna.fbcdn.net/v/t1.0-9/26195441_1648306671905776_1468977249504823709_n.jpg?_nc_cat=110&_nc_pt=1&_nc_ht=scontent.fcgh15-1.fna&oh=fb2c1397f5164159f8804fe2bcc8ee96&oe=5CAC08CA",
                  background = TRUE,
                  backgroundUrl = "https://scontent.fcgh15-1.fna.fbcdn.net/v/t1.0-9/26112158_1649553085114468_1607815453612499060_n.jpg?_nc_cat=103&_nc_pt=1&_nc_ht=scontent.fcgh15-1.fna&oh=a8f6717ded5439d065c0de4e2bf2cd28&oe=5C70786E",
                  # color = "aqua-active",
                  closable = FALSE
                  # "Some text here!",
                  # footer = "10/10"
                ),
                widgetUserBox(
                  title = "Jhunu Fernandes Araujo",
                  subtitle = "8913020",
                  type = NULL,
                  width = 6,
                  src = "https://scontent.fcgh15-1.fna.fbcdn.net/v/t1.0-9/40683687_2018389438239645_8942412224291405824_n.jpg?_nc_cat=102&_nc_pt=1&_nc_ht=scontent.fcgh15-1.fna&oh=5913441d63dab5072b664bf654eb96c8&oe=5C651815",
                  background = TRUE,
                  backgroundUrl = "https://scontent.fcgh15-1.fna.fbcdn.net/v/t1.0-9/15624_784486408296627_1669409774701658639_n.jpg?_nc_cat=111&_nc_pt=1&_nc_ht=scontent.fcgh15-1.fna&oh=e32a00278b8f33e3d60a9a5c82c4bf8c&oe=5C6E3158",
                  # color = "aqua-active",
                  closable = FALSE
                )
              ),
              fluidRow(
                widgetUserBox(
                  title = "Gustavo Giardullo Araujo",
                  subtitle = "9345703",
                  type = NULL,
                  width = 6,
                  src = "https://scontent.fcgh15-1.fna.fbcdn.net/v/t1.0-9/40971421_2085036404880023_8353540786029068288_n.jpg?_nc_cat=109&_nc_pt=1&_nc_ht=scontent.fcgh15-1.fna&oh=520fe08e00f6bcb27cc937a3a99ee1b5&oe=5C65A881",
                  background = TRUE,
                  backgroundUrl = "https://scontent.fcgh15-1.fna.fbcdn.net/v/t1.0-9/27459263_1814635251920141_941075183008253490_n.jpg?_nc_cat=106&_nc_pt=1&_nc_ht=scontent.fcgh15-1.fna&oh=54824e336e4e076ea73fe6eed609dbee&oe=5C67309D",
                  closable = FALSE
                  # "Some text here!",
                  # footer = "10/10"
                ),
                widgetUserBox(
                  title = "Guilherme Marques Zuchini",
                  subtitle = "8912610",
                  type = NULL,
                  width = 6,
                  src = "https://scontent.fcgh15-1.fna.fbcdn.net/v/t1.0-9/18892996_1306034646159585_3807569048783461842_n.jpg?_nc_cat=106&_nc_pt=1&_nc_ht=scontent.fcgh15-1.fna&oh=8fed9d0469de73e43c92daf283c97c59&oe=5C9E6EB3",
                  background = TRUE,
                  backgroundUrl = "https://scontent.fcgh15-1.fna.fbcdn.net/v/t1.0-9/26994385_1525233980906316_6183058465275598078_n.jpg?_nc_cat=110&_nc_pt=1&_nc_ht=scontent.fcgh15-1.fna&oh=02292a1949ddb613d0a0e1411cfc1e67&oe=5CA47D13",
                  closable = FALSE
                  # "Some text here!",
                  # footer = "10/10"
                )
              ),
              fluidRow(
                widgetUserBox(
                  title = "Carlos H. M. Madureira",
                  subtitle = "8042096",
                  type = NULL,
                  width = 6,
                  src = "https://scontent.fcgh15-1.fna.fbcdn.net/v/t1.0-9/18519751_10208657654628518_6389140047850960049_n.jpg?_nc_cat=104&_nc_pt=1&_nc_ht=scontent.fcgh15-1.fna&oh=d40a3d5bad9edb4a089456435b57b134&oe=5C705EB4",
                  background = TRUE,
                  backgroundUrl = "https://scontent.fcgh15-1.fna.fbcdn.net/v/t1.0-9/22405922_10209732555100358_1594326112120449551_n.jpg?_nc_cat=107&_nc_pt=1&_nc_ht=scontent.fcgh15-1.fna&oh=3855b64f3897f18fdee25bf5a2370dec&oe=5CA660DA",
                  closable = FALSE
                  # "Some text here!",
                  # footer = "10/10"
                ),
                widgetUserBox(
                  title = "Arthur de Sousa Valli",
                  subtitle = "9350713",
                  type = NULL,
                  width = 6,
                  src = "https://scontent.fcgh15-1.fna.fbcdn.net/v/t1.0-9/25398917_1382246701886890_1078217204938175954_n.jpg?_nc_cat=101&_nc_pt=1&_nc_ht=scontent.fcgh15-1.fna&oh=43b1dc81885fe8f0faddc7c5486121fd&oe=5C6A57A4",
                  background = TRUE,
                  backgroundUrl = "https://scontent.fcgh15-1.fna.fbcdn.net/v/t1.0-9/20476248_1263631587081736_710406888367800321_n.jpg?_nc_cat=100&_nc_pt=1&_nc_ht=scontent.fcgh15-1.fna&oh=e4f2ef17f43e8734f33cf0fe5ffd4dc7&oe=5C6FC9EC",
                  closable = FALSE
                  # "Some text here!",
                  # footer = "10/10"
                )
              )
  )
  return(d)
}

ui <- dashboardPagePlus(header = cabecalho(),
                    sidebar = barrLado(),
                    body =  dashboardBody(tabItems(poluicao_pageHistorico(),
                                                   temperatura_pageHistorico(),
                                                   geral_pageRealTime(),
                                                   equipe_page(),
                                                   config_page(),
                                                   tabItem(tabName = "mmu_sobre", 
                                                           column(width = 2),  
                                                           boxPlus(h2("SOBRE O PROJETO", align = "center"), includeMarkdown("Codigos/sobre.Rmd"), width = 8, closable = FALSE),
                                                           column(width = 2))
                    )
                    ),
                    skin = "blue")



