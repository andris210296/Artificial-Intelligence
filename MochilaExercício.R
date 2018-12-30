# Knapsack problem

# This code uses a Genetic Algorithm to verify what items should be brought
# by the best relation between weight and value


#install.packages("GA")

library("GA")

itens = c("Canivete","Feijão","Batatas","Lanterna","Saco de Dormir","Corda","Bússola")


mochila = function(bytes){
  
  colunas = c("Pontos","Peso")
  
  pontosPeso= c(10,1 ,20,5 ,15,10 ,2,1 ,30,7 ,10,5 ,30,1)
  
  tabelaItens = matrix(
    pontosPeso,ncol = 2, nrow = 7,
    byrow = TRUE, 
    dimnames = list(itens,colunas))
  
  
  pesoTotal = 0
  valorTotal = 0
  
  for(i in 1:7){
    if(bytes[i] == 1){
      pesoTotal = pesoTotal + tabelaItens[i,2]
      valorTotal = valorTotal + tabelaItens[i]
    }
    
  }
  
  if(pesoTotal > 15)
    valorTotal = 0
  
  return (valorTotal)
  
}

#algoritmo genetico
resultado = ga("binary", fitness = mochila,
               maxiter = 100, nBits =  7, popSize = 10,
               monitor = T, names = itens)

summary(resultado)

summary(resultado)$solution

plot(resultado)

