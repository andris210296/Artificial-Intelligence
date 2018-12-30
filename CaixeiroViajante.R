# Travelling salesman problem

# This code uses a Genetic Algorithm that given a list of cities and the distances
# between each pair of cities, what is the shortest possible route that visits each city


#install.packages("GA")


library(GA)

mapa = data.frame(Cidade=c("Lindem","Parika", "Lethen", "Rosignol", "New Amsterdam"),
                    Lindem=c(0,8,25,10,9),
                    Parika=c(8,0,13,10,10),
                    Lethen=c(25,13,0,12,14),
                    Rosignol=c(10,10,12,0,21),
                    NewAmsterdam=c(9,10,14,21,0))


caminho = function(combinacao){
  
  #combinacao = c(2,3,1,4,5)
  
  distancia = 0;  
  
  for (i in 1 : 5){
    
    proximo = i+1
    
    if(proximo <= 5){
      
      distancia = distancia + mapa[combinacao[i], combinacao[proximo] + 1]
    }
  }
  
  return (-distancia)
}

resultado <- ga(type="permutation", fitness=caminho,
                min=c(1,1,1,1,1), 
                max=c(5,5,5,5,5),popSize = 10, maxiter = 5000, 
                names=c("Linden","Parika","Lethem","Rosignol","New Amsterdam"),
                monitor = F)

#solucao
summary(resultado)$solution

#evolucao
plot(resultado)