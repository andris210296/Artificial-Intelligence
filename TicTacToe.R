# Tic Tac Toe

# This code is a reimplementation of Prof Fernando Amaral's code that uses Reinforcement Learning to play Tic Tac Toe


install.packages("ReinforcementLearning")
library(ReinforcementLearning)
control <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)
modelottt <- ReinforcementLearning(tictactoe, s = "State", a = "Action", r = "Reward", 
                                   s_new = "NextState", iter = 2, control = control)

verificavitoria = function(board)
{
  #diagonal1
  if(board[1,1] == board[2,2] && board[1,1] == board[3,3]){
    warning("diagonal1")
    return (board[1,1]);
  }
  
  #diagonal2
  if(board[1,3] == board[2,2] && board[1,3] == board[3,1]){
    warning("diagonal2")
    return (board[1,1]);
  }
  
  #Vertical
  for (i in 1:3) {
    if(board[1,i] == board[2,i] && board[1,i] == board[3,i]){
      warning("Vertical")
      return (board[i,1]);
    }
  }
  
  #Horizontal
  for (i in 1:3) {
    if(board[i,1] == board[i,2] && board[i,1] == board[i,3]){
      warning("horizontal")
      return (board[1,i]);
    }
  }
}


vertical = matrix(c("X","X","X","O","O","X","X","O","O"), nrow = 3, ncol = 3)
verificavitoria(vertical)

horizontal = matrix(c("O","O","X","O","X","O","O","X","O"), nrow = 3, ncol = 3)
verificavitoria(horizontal) 

diagonal1 = matrix(c("X","O","O","O","X","O","O","O","X"), nrow = 3, ncol = 3)
verificavitoria(diagonal1) 

diagonal2 = matrix(c("O","O","X","O","X","O","X","O","O"), nrow = 3, ncol = 3)
verificavitoria(diagonal2)

tie = matrix(c("x","O","X","O","X","O","O","X","O"), nrow = 3, ncol = 3)
verificavitoria(tie)
  

drawBoard = function(board){
  
  #draws the board with the pieces
  plot(0,type='n',axes=FALSE,ann=FALSE)
  abline(v=.87)
  abline(v=1.14)
  abline(h=-.34)
  abline(h=.32)
  
  
  text(0.735,.8,labels=ifelse(board[1,1]==""," ",board[1,1]),cex=7)
  text(1,.8,labels=ifelse(board[1,2]==""," ",board[1,2]),cex=7)
  text(1.257,.8,labels=ifelse(board[1,3]==""," ",board[1,3]),cex=7)
  
  
  text(0.735,0,labels=ifelse(board[2,1]==""," ",board[2,1]),cex=7)
  text(1,0,labels=ifelse(board[2,2]==""," ",board[2,2]),cex=7)
  text(1.257,0,labels=ifelse(board[2,3]==""," ",board[2,3]),cex=7)
  
  text(0.735,-.73,labels=ifelse(board[3,1]==""," ",board[3,1]),cex=7)
  text(1,-.73,labels=ifelse(board[3,2]==""," ",board[3,2]),cex=7)
  text(1.257,-.73,labels=ifelse(board[3,3]==""," ",board[3,3]),cex=7)

  
}
  
TestDrawBoard = matrix(c("","O","X","O","","O","","O","O"), nrow = 3, ncol = 3)
drawBoard(TestDrawBoard)


boardMatrixToModelotttView = function (board){
  # This function transforms the board matrix in a way that the structure modelottt understands, 
  # which means Matrix to [".BB...XX."]
  
  structure =""
  piece = ""
  for (i in 1:3) {
    for (j in 1:3){
      piece = "X"
      if(board[i,j] == ""){
        piece = "."
      }
      if(board[i,j] == "O"){
        piece = "B"
      }
      structure = paste(structure,piece,collapse = NULL, sep = "");
    }
  }
  
  return (c(structure));
}

TestBoardMatrixToModelotttView = matrix(c("","O","X","O","","O","","O","O"), nrow = 3, ncol = 3)
boardMatrixToModelotttView(TestBoardMatrixToModelotttView)



modelotttViewToboardMatrix = function(structure){
  
  structure = gsub("B","O",structure)
  arrayStructure = unlist(strsplit(structure,""))
  
  
  matrix(c(ifelse(arrayStructure[1]=="."," ",arrayStructure[1]),ifelse(arrayStructure[4]=="."," ",arrayStructure[4]),ifelse(arrayStructure[7]=="."," ",arrayStructure[7]),
           ifelse(arrayStructure[2]=="."," ",arrayStructure[2]),ifelse(arrayStructure[5]=="."," ",arrayStructure[5]),ifelse(arrayStructure[8]=="."," ",arrayStructure[8]),
           ifelse(arrayStructure[3]=="."," ",arrayStructure[3]),ifelse(arrayStructure[6]=="."," ",arrayStructure[6]),ifelse(arrayStructure[9]=="."," ",arrayStructure[9])), 
         nrow = 3, ncol = 3)
}


TestModelotttViewToboardMatrix = ".B.B.BXBB"
modelotttViewToboardMatrix(TestModelotttViewToboardMatrix)



startGame = function(){
  
  
  
  
}

