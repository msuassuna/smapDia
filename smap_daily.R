######## SMAP passo diário ###########
# Data: Janeiro, 2020
# Author: Marcus Suassuna Santos
# Adaptado de: Cassio Guilherme Rampinelli (2013)
# email: msuassuna@gmail.com
##########################################################

# Verifica se o pacote dplyr está instalado
if("dplyr" %in% row.names(installed.packages())){
  library("dplyr")
} else {
  install.packages("dplyr")
  library("dplyr")
}

# Função SMAP
smap_daily <- function(EPQ, para, inic, Area){
  
  n <- nrow(EPQ)
  
  # Transformação dos parâmetros de recessão
  kk <- as.numeric(0.5 ^ (1/para["kkt"]))
  k2 <- as.numeric(0.5 ^ (1/para["k2t"]))
  
  # Vetores de vazões (em m³/s e mm): totais, base e escoamento superficial
  Qcalc <- array(NA, n)
  Qbase <- array(NA, n)
  Qsup <- array(NA, n)
  
  Es <- array(NA, n)
  Eb <- array(NA, n)
  Ed <- array(NA, n)
  
  # Vetores de reservatórios: água sobre solo, água subterrânea e superficial
  Rsol <- array(NA, n)
  Rsub <- array(NA, n)
  Rsup <- array(NA, n)
  
  # Evapotranspiração
  Er <- array(NA, n)
  
  # recarga do aquífero
  Rec <- array(NA, n)
  
  # Conteúdo de umidade do solo
  tu <- array(NA, n)
  
  # Parâmetros iniciais
  Rsol[1] <- inic["tuin"]/100 * para["sat"]
  Rsub[1] <- inic["ebin"] / (1 - kk) / Area * 86.4
  Rsup[1] <- 0
  
  # Roda o SMAP ao longo da série de dados
  for(i in 1:n){
    
    # Teor de umidade em função do reservatório superficial
    tu[i] <- Rsol[i] / as.numeric(para["sat"])
    
    # Excesso de chuva converte-se em escoamento superficial
    if(EPQ[i,"P"] > para["ai"]){
      Es[i] <- (EPQ[i,"P"] - as.numeric(para["ai"])) ^ 2 /
        (EPQ[i,"P"] - as.numeric(para["ai"]) + as.numeric(para["sat"]) - Rsol[i])
    } else {
      Es[i] <- 0
    }
    
    # Estima evapotranspiração
    if((EPQ[i,"P"] - Es[i]) > EPQ[i,"E"]){
      Er[i] <- as.numeric(EPQ[i,"E"])
    } else {
      Er[i] <- (EPQ[i,"P"] - Es[i]) + (EPQ[i,"E"] - (EPQ[i,"P"] - Es[i])) * tu[i]
    }
    
    # Estima recarga do aquífero
    if(Rsol[i] > para["capcc"]/100 * para["sat"]){
      Rec[i] <- para["crec"]/100 * tu[i] * (Rsol[i] - (para["capcc"]/100 * para["sat"]))
    } else {
      Rec[i] <- 0
    }
    
    # Atualiza volume armazenado no reservatório superficial
    Rsol[i+1] <- Rsol[i] + EPQ[i,"P"] - Es[i] - Er[i] - Rec[i]
    if(Rsol[i + 1] > para["sat"]){
      Rsol[i+1] <- as.numeric(para["sat"])
    }
    
    # Estimativa do escoamento direto e de base
    Ed[i] <- (Rsup[i] + Es[i]) * (1 - k2)
    Rsup[i+1] <- Rsup[i] + Es[i] - Ed[i]
    
    Eb[i] <- Rsub[i] * (1 - kk)
    Rsub[i+1] <- Rsub[i] - Eb[i] + Rec[i]
    
    Qsup[i] <- Ed[i] * Area / 86.4
    Qbase[i] <- Eb[i] * Area / 86.4
    
    Qcalc[i]  <- (Ed[i] + Eb[i]) * Area / 86.4
    
  }  
  
  # Retorna uma lista com todos os resultados do modelo
  
  Result <- list(
    "Qcalc" = Qcalc,
    "Qbase" = Qbase,
    "Qsup" = Qsup,
    "Rsol" = Rsol,
    "Rsub" = Rsub,
    "Rsup" = Rsup,
    "Es" = Es,
    "Eb" = Eb,
    "Ed" = Ed,
    "Er" = Er,
    "Rec" = Rec,
    "tu" = tu)
  
  return(Result)

}
