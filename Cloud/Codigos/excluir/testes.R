
# showModal(modalDialog(
#   title = "Atenção!",
#   textInput("txt_sensorID", "Digite o código do seu produto:"),
#   textInput("txt_sensorSenha", "Digite senha:"),
#
#   easyClose = TRUE,
#   footer = tagList(
#     actionButton("ok", "OK")
#   )
# ))


# tabEvento = readRDS("Dados/dadosEvento.rds")
# tail(tabEvento)
# ## NOTIFICACAO --------------------------------------------------------------------------------------------------
# mensgItem = list()
# #i = "pia4"
# for (i in unique(tabEvento$LOCAL)) {
#   #ultMed = tail(subset(tabEvento$MOMENTO, tabEvento$LOCAL==i), na.rm = T)
#
#   ultMed = max(subset(tabEvento$MOMENTO, tabEvento$LOCAL==i), na.rm = T)
#   dif = difftime(now(tzone = "America/Sao_Paulo"), ultMed, units = "days")
#   if(dif >= 3/24 & dif < 1){
#     mensgItem[[i]] = messageItem(from = paste0("Sensor de ", pontos$NOME[which(pontos$ID==i)]),
#                                  message = paste0("Não está funcionando há ", round(dif*24), " horas"),
#                                  icon = icon("exclamation-triangle"))
#   }else if(dif >= 1){
#     mensgItem[[i]] = messageItem(from =  paste0("Sensor de ", pontos$NOME[which(pontos$ID==i)]),
#                                  message = paste0("Não está funcionando há ", round(dif), " dias"),
#                                  icon = icon("exclamation-triangle"))
#   }
# }
#
# output$messageMenu <- renderMenu({
#   dropdownMenu(type = "messages", .list = mensgItem)
# })
#
#
# observe({
#
#   #inicio = as.Date("2018-10-1")
#   # fim = as.Date(now())
#   inicio = as.Date(input$dateIntervalo[1])
#   fim  = as.Date(input$dateIntervalo[2])
#   baseTempo = as.character(input$selBaseTemporal)
#
#   if(fim < inicio){
#     showModal(modalDialog(
#       title = "Atenção!",
#       "A data final não pode ser anterior a data data de início!",
#       easyClose = TRUE,
#       footer = NULL
#     ))
#   }else{
#
#     #imputar = input$ckbPreencherFalt
#
#     if(input$ckbPreencherFalt)
#       tabHora = readRDS("Dados/dadosHoraImp.rds")
#     else
#       tabHora = readRDS("Dados/dadosHora.rds")
#
#     tab = subset(tabHora, as.Date(tabHora$HORA_REF)<fim & as.Date(tabHora$HORA_REF)>inicio)
#
#     try(
#       if(baseTempo == "Por período do dia"){
#         tab = transfConsumoMedidoPeriodo(tab)
#       }
#       else if(baseTempo == "Por dia") {
#         tab = transfConsumoMedidoDia(tab)
#       }
#     )
#     semZero = input$ckbSemZero
#     try(global_carregaConsumo(input, output, tab, inicio, fim, baseTempo, semZero))
#     try(torneiras_carregaConsumo(input, output, tab, inicio, fim, baseTempo, semZero))
#     try(mictorios_carregaConsumo(input, output, tab, inicio, fim, baseTempo, semZero))
#     try(vasos_carregaConsumo(input, output, tab, inicio, fim, baseTempo, semZero))
#     try(pessoas_carregaContador(input, output, tab, inicio, fim, baseTempo, semZero))
#     try(analise_carrega(input, output, tab, inicio, fim, baseTempo, semZero))
#   }
# })











# shinyApp(
#   ui = basicPage(
#     actionButton("show", "Show modal dialog"),
#     verbatimTextOutput("dataInfo")
#   ),
#   
#   server = function(input, output) {
#     # reactiveValues object for storing current data set.
#     vals <- reactiveValues(data = NULL)
#     
#     # Return the UI for a modal dialog with data selection input. If 'failed' is
#     # TRUE, then display a message that the previous value was invalid.
#     dataModal <- function(failed = FALSE) {
#       modalDialog(
#         textInput("dataset", "Choose data set",
#                   placeholder = 'Try "mtcars" or "abc"'
#         ),
#         span('(Try the name of a valid data object like "mtcars", ',
#              'then a name of a non-existent object like "abc")'),
#         if (failed)
#           div(tags$b("Invalid name of data object", style = "color: red;")),
#         
#         footer = tagList(
#           modalButton("Cancel"),
#           actionButton("ok", "OK")
#         )
#       )
#     }
#     
#     # Show modal when button is clicked.
#     observeEvent(input$show, {
#       showModal(dataModal())
#     })
#     
#     # When OK button is pressed, attempt to load the data set. If successful,
#     # remove the modal. If not show another modal, but this time with a failure
#     # message.
#     observeEvent(input$ok, {
#       # Check that data object exists and is data frame.
#       if (!is.null(input$dataset) && nzchar(input$dataset) &&
#           exists(input$dataset) && is.data.frame(get(input$dataset))) {
#         vals$data <- get(input$dataset)
#         removeModal()
#       } else {
#         showModal(dataModal(failed = TRUE))
#       }
#     })
#     
#     # Display information about selected data
#     output$dataInfo <- renderPrint({
#       if (is.null(vals$data))
#         "No data selected"
#       else
#         summary(vals$data)
#     })
#   }
# )






# server2 <- function(port = 1901, timeout = 86400, host = "ec2-18-225-10-77.us-east-2.compute.amazonaws.com"){
#   library(lubridate)
#   library(stringr)
#   setwd("/home/laftos/Projetos/MedidorPoluicao/Dados/Desagregado/Sensores/")
#   #setwd("C:/Users/Leonardo/OneDrive/USP/Semestre10/Iot/projeto/poluicao/dados/")
#   hoje = now(tzone = "America/Sao_Paulo")
#   newLine = data.frame(hora = hour(hoje),
#                        min = minute(hoje),
#                        seg = second(hoje),
#                        latit = -23.5591836,
#                        longi = -46.748765,
#                        polui = 50,
#                        tempe = 20)
#   while(TRUE){
#     writeLines("Listening...")
#     con = socketConnection(host=host, port = port, blocking=TRUE,
#                            server=TRUE, open="r+", timeout= timeout)
#     
#     data <- readLines(con, 1)
#     
#     hoje = now(tzone = "America/Sao_Paulo")
#     sensor = word(data,1,sep = "\\;")
#     nameFile = paste0(sensor, "/", str_pad(day(hoje), 2 , pad="0"),
#                       str_pad(month(hoje), 2 , pad="0"),
#                       str_pad(year(hoje), 2 , pad="0"),
#                       ".csv")
#     latit = newLine$latit[1] + as.numeric(word(data,c(2),sep = ";"))
#     latit = ifelse(latit<(-47), -24,  
#                    ifelse(latit>(-46),-23, latit))
#     longi = newLine$longi + as.numeric(word(data,c(3),sep = ";"))
#     longi = ifelse(longi<(-24), -24,  
#                    ifelse(longi>(-23),-23, longi))
#     polui = newLine$polui + as.numeric(word(data,c(4),sep = ";"))
#     polui = ifelse(polui<(0), 0,  
#                    ifelse(polui>(300),300, polui))
#     tempe = newLine$tempe + as.numeric(word(data,c(5),sep = ";"))
#     tempe = ifelse(tempe<(-10), -10,  
#                    ifelse(tempe>(45), 45, tempe))
# 
#     newLine = data.frame(hora = hour(hoje),
#                          min = minute(hoje),
#                          seg = second(hoje),
#                          latit = latit,
#                          longi = longi,
#                          polui = polui,
#                          tempe = tempe)
#     
#     try(dir.create(sensor))
#     try(write.table(newLine, file = nameFile, sep = ";", dec = ",", append = TRUE, quote = FALSE,
#                     col.names = FALSE, row.names = FALSE))
#     close(con)
#   }
# }



server3 <- function(port = 1901, timeout = 86400, host = "ec2-18-225-10-77.us-east-2.compute.amazonaws.com"){
  library(lubridate)
  library(stringr)
  setwd("/home/laftos/Projetos/MedidorPoluicao/Dados/Desagregado/Sensores/")
  #setwd("C:/Users/Leonardo/OneDrive/USP/Semestre10/Iot/projeto/poluicao/dados/")
  while(TRUE){
    writeLines("Listening...")
    con = socketConnection(host=host, port = port, blocking=TRUE,
                           server=TRUE, open="r+", timeout= timeout)
    
    data <- readLines(con, 1)
    print(data)
    
    close(con)
  }
}
server3()