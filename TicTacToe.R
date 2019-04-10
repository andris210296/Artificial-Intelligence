# Tic Tac Toe

# This code is a reimplementation of Prof Fernando Amaral's code that uses Reinforcement Learning to play Tic Tac Toe


install.packages("ReinforcementLearning")
library(ReinforcementLearning)
control <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)
modelottt <- ReinforcementLearning(tictactoe, s = "State", a = "Action", r = "Reward",
                                   s_new = "NextState", iter = 2, control = control)

checkVictory= function(board)
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
checkVictory(vertical)

horizontal = matrix(c("O","O","X","O","X","O","O","X","O"), nrow = 3, ncol = 3)
checkVictory(horizontal) 

diagonal1 = matrix(c("X","O","O","O","X","O","O","O","X"), nrow = 3, ncol = 3)
checkVictory(diagonal1) 

diagonal2 = matrix(c("O","O","X","O","X","O","X","O","O"), nrow = 3, ncol = 3)
checkVictory(diagonal2)

tie = matrix(c("x","O","X","O","X","O","O","X","O"), nrow = 3, ncol = 3)
checkVictory(tie)
  

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


boardMatrixToArray = function (board){

  array = as.vector(t(board))
 
  for (i in 1:9) {
    if(array[i] == ""){
      array[i] = "."
      }
    if(array[i] == "O"){
      array[i] = "B"
    }
  }
  
  return (array);
}

TestBoardMatrixToArray = matrix(c("","O","X","O","","O","","O","O"), nrow = 3, ncol = 3)
boardMatrixToArray(TestBoardMatrixToArray)


modelotttViewToboardMatrix = function(structure){
  
  structure = gsub("B","O",structure)
  arrayStructure = unlist(strsplit(structure,""))
  
  
  matrix(c(ifelse(arrayStructure[1]=="."," ",arrayStructure[1]),ifelse(arrayStructure[4]=="."," ",arrayStructure[4]),ifelse(arrayStructure[7]=="."," ",arrayStructure[7]),
           ifelse(arrayStructure[2]=="."," ",arrayStructure[2]),ifelse(arrayStructure[5]=="."," ",arrayStructure[5]),ifelse(arrayStructure[8]=="."," ",arrayStructure[8]),
           ifelse(arrayStructure[3]=="."," ",arrayStructure[3]),ifelse(arrayStructure[6]=="."," ",arrayStructure[6]),ifelse(arrayStructure[9]=="."," ",arrayStructure[9])), 
         nrow = 3, ncol = 3)
}


TestModelotttViewToboardMatrix = c(".", "B", ".", "B", ".", "B", "X", "B", "B")
modelotttViewToboardMatrix(TestModelotttViewToboardMatrix)

isAValidPlay = function(board, play){
  if(boardMatrixToArray(board)[play] == "."){
    return (TRUE)
  }
  return (FALSE)
}

TestIsAValidPlay = matrix(c("","O","X","O","","O","","O","O"), nrow = 3, ncol = 3)
isAValidPlay(TestIsAValidPlay,1)
isAValidPlay(TestIsAValidPlay,2)


startGame = function(){
  
  startPlayer = ""
  computer = "ia"
  user = "user"
  
  pieceOne = "X"
  pieceTwo = "O"
  
  while (startPlayer != "x" && startPlayer != computer && startPlayer != user){
    print("Type 'ia' to make the computer start the first move")
    print("Type 'user' to make you start")
    print("Type 'x' to exit: ")
    
    startPlayer = readline(prompt = "Option:")
  }
  
  computerPiece = "";
  playerPiece = "";
  turn = "";
  
  if(startPlayer == computer){
    computerPiece = pieceOne
    playerPiece = pieceTwo
    
    turn = user
  }else{
    computerPiece = pieceTwo
    playerPiece = pieceOne
    
    turn = playerPiece
  }
  
  
  board = matrix(c(replicate(9,"")), nrow = 3, ncol = 3)
  drawBoard(board)
  
  if(startPlayer == computer){
    firstMoveX = sample(3,1)
    firstMoveY = sample(3,1)
    
    board[firstMoveX][firstMoveY] = computerPiece
    
    drawBoard(board)
    turn = user
    
  }else{
    print("Type '1.1' to '3.3' to choose a position to play")
    print("Example: 1.2, 1.3, 2.2 ...")
    playerMovement = readline(prompt = "Movement:")
    playerMovementArray = unlist(strsplit(playerMovement, "[.]"))
    board[strtoi(playerMovementArray[1], base = 0L),strtoi(playerMovementArray[2], base = 0L)] = playerPiece
    
    drawBoard(board)
    turn = computer
  }
  
  
  while(checkVictory(board) == ""){
    if(turn == computer){
      computerPlay =  ""
      while(isAValidPlay(board,computerPlay)){
      
        computerPlay = modelottt$Policy[paste(boardMatrixToArray(board),collapse="")]
        
      }
      
      drawBoard(board)
      turn = user
    }
    
    if(turn == user){
      
      userPlay = ""
      while(isAValidPlay(board,userPlay)){}
        print("Type '1.1' to '3.3' to choose a position to play")
        print("Example: 1.2, 1.3, 2.2 ...")
        userPlay = readline(prompt = "Movement:")
        userPlayArray = unlist(strsplit(playerMovement, "[.]"))
        board[strtoi(playerMovementArray[1], base = 0L),strtoi(userPlayArray[2], base = 0L)] = playerPiece
    }
    drawBoard(board)
    turn = computer
        
        
      
    }
  }
  
}

