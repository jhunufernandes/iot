setwd("/home/laftos/Projetos/MedidorPoluicao")

source("Codigos/pacotes.R")
source("Codigos/func_auxiliar.R")
source("Codigos/func_corrigeDados.R")

sendEmail <- function(s="Alerta de ambiente de risco!", b="teste",  r){
  sender = "leon.larrubia@gmail.com"
  password <- as.character(read.table("/home/laftos/Projetos/MedidorPoluicao/Dados/Configuracao/senGmail.txt")[1,1])
  send.mail(
    from = sender,
    to = r,
    subject=paste(s),
    body =b,
    smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "MONITORAMENTO POLUIÇÃO", passwd = password, ssl = TRUE),
    authenticate = FALSE,
    #send = TRUE,
    html = FALSE
  )
}

server <- function(port = 1901, timeout = 86400, host = "ec2-18-225-10-77.us-east-2.compute.amazonaws.com"){
  library(lubridate)
  library(stringr)
  setwd("/home/laftos/Projetos/MedidorPoluicao/Dados/Desagregado/Sensores/")
  while(TRUE){
    writeLines("Listening...")
    con = socketConnection(host=host, port = port, blocking=TRUE,
                           server=TRUE, open="r+", timeout= timeout)
    
    data <- readLines(con, 1)
    print(data)
    hoje = now(tzone = "America/Sao_Paulo")
    sensor = word(data,1,sep = "\\;")
    nameFile = paste0(sensor, "/", str_pad(day(hoje), 2 , pad="0"),
                      str_pad(month(hoje), 2 , pad="0"),
                      str_pad(year(hoje), 2 , pad="0"),
                      ".csv")
    newLine = data.frame(hora = hour(hoje),
                         min = minute(hoje),
                         seg = second(hoje),
                         latit = as.numeric(word(data,c(2),sep = ";")),
                         longi = as.numeric(word(data,c(3),sep = ";")),
                         polui = as.numeric(word(data,c(4),sep = ";")),
                         temp = as.numeric(word(data,c(5),sep = ";")))
    
    info = read.table("/home/laftos/Projetos/MedidorPoluicao/Dados/Configuracao/listaSensor.csv", sep = ";", dec = ".", header = T)
    infoSensor = info[which(info$SENSOR==sensor),]
    if(as.numeric(infoSensor$ALARME) == 1){
      if (classificaPoluicao(newLine$polui) == "Péssimo" & infoSensor$POLUI_DATA_ULT_ATT < (now()-hours(1))) {
        infoSensor$POLUI_DATA_ULT_ATT = now()
        body = paste0("A qualidade do ar do ambiente em que você está atualmente é <b><font color=\"red\">PÉSSIMA!</font></b>")
        # sendEmail(b = body, r = as.character(infoSensor$EMAIL))
      }
      if (classificaTemperatura(newLine$temp) == "Muito Quente" || classificaTemperatura(newLine$temp) == "Muito Frio" & infoSensor$TEMP_DATA_ULT_ATT < (now()-hours(1))) {
        infoSensor$TEMP_DATA_ULT_ATT = now()
        if(classificaTemperatura(newLine$temp) == "Muito Quente")
          body = paste0("O ambiente em que você está atualmente é <b><font color=\"red\">MUITO QUENTE!</font></b>")
        else
          body = paste0("O ambiente em que você está atualmente é <b><font color=\"red\">MUITO FRIO!</font></b>")
          # sendEmail(b = body, r = as.character(infoSensor$EMAIL))
      }
    }

    try(dir.create(sensor))
    try(write.table(newLine, file = nameFile, sep = ";", dec = ",", append = TRUE, quote = FALSE,
                    col.names = FALSE, row.names = FALSE))
    close(con)
  }
}

server(port = 1902)
