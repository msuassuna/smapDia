# Função objetivo Nash_Sutcliffe
Nash_Sutcliffe <- function(Qobs, Qcalc){
  
  NS <- (-1) * (1 -((sum((Qobs - Qcalc)^2)) /
                      (sum((Qobs - mean(Qobs))^2))))
  return(NS)
  
}

# Função objetivo pela proximidade da curva de permanência
prox_perm <- function(Qobs, Qcalc){
  
  Quants = c(0.99, seq(from = 0.95, to = 0.05, by = -0.05), 0.01)
  quant_obs <- quantile(Qobs, Quants)
  quant_clc <- quantile(Qcalc, Quants)
  NS2 <- (-1) * (1 - ((sum((quant_obs - quant_clc)^2))/
                        (sum((quant_obs - mean(quant_clc))^2))))
  return(NS2)
}

# Função objetivo pela proximidade da curva de permanência para vazões baixas
prox_inf <- function(Qobs, Qcalc){
  
  Quants = c(seq(from = 0.30, to = 0.05, by = -0.05), 0.01)
  quant_obs <- quantile(Qobs, Quants)
  quant_clc <- quantile(Qcalc, Quants)
  NS2 <- (-1) * (1 - ((sum((quant_obs - quant_clc)^2))/
                        (sum((quant_obs - mean(quant_clc))^2))))
  return(NS2)
}

# Função objetivo pela proximidade da curva de permanência para vazões altas
prox_sup <- function(Qobs, Qcalc){
  
  Quants = c(0.99, seq(from = 0.95, to = 0.7, by = -0.05))
  quant_obs <- quantile(Qobs, Quants)
  quant_clc <- quantile(Qcalc, Quants)
  NS2 <- (-1) * (1 - ((sum((quant_obs - quant_clc)^2))/
                        (sum((quant_obs - mean(quant_clc))^2))))
  return(NS2)
}