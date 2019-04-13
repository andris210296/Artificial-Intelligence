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
  if(board[1,1] == board[2,2] && board[1,1] == board[3,3] &&
     board[1,1] != "" && board[2,2] != "" && board[3,3] != ""){

      warning("diagonal1")
      return (board[1,1]);
    
  }
  
  #diagonal2
  if(board[1,3] == board[2,2] && board[1,3] == board[3,1] &&
     board[1,3] != "" && board[2,2] != "" && board[3,1] != ""){
      warning("diagonal2")
      return (board[1,1]);

  }
  
  #Vertical
  for (i in 1:3) {
    if(board[1,i] == board[2,i] && board[1,i] == board[3,i] &&
       board[1,i] != "" && board[2,i] != "" && board[3,i] != ""){
      warning("Vertical")
      return (board[i,1]);
    }
  }
  
  #Horizontal
  for (i in 1:3) {
    if(board[i,1] == board[i,2] && board[i,1] == board[i,3] &&
       board[i,1] != "" && board[i,2] != "" && board[i,3] != ""){
      warning("horizontal")
      return (board[i,1]);
    }
  }
  
  return (FALSE)
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

TestCheckVictory6 = matrix(c("O","","","","X","","","","X"), nrow = 3, ncol = 3)
checkVictory(TestCheckVictory6)

TestCheckVictory7 = matrix(c("","","","","X","","","",""), nrow = 3, ncol = 3)
checkVictory(TestCheckVictory7)

TestCheckVictory8 = matrix(c("X","O","","O","X","","X","",""), nrow = 3, ncol = 3)
checkVictory(TestCheckVictory8)

TestCheckVictory9 = matrix(c("","","","X","","","","",""), nrow = 3, ncol = 3)
checkVictory(TestCheckVictory9)

TestCheckVictory10 = matrix(c("O","","X","X","O","X","O","","X"), nrow = 3, ncol = 3)
checkVictory(TestCheckVictory10)

TestCheckVictory11 = matrix(c("O","O","X","O","X","X","","","X"), nrow = 3, ncol = 3)
checkVictory(TestCheckVictory11)


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
  if(is.null(play) || play == ""){
    return (FALSE)
  }
  if(board[play[1],play[2]] == ""){
    return (TRUE)
  }
  return (FALSE)
}

TestIsAValidPlay = matrix(c("","O","X","O","","O","","O","O"), nrow = 3, ncol = 3)
isAValidPlay(TestIsAValidPlay,c(1,3))
isAValidPlay(TestIsAValidPlay,c(1,2))
TestIsAValidPlay2 = matrix(c("","","","","X","","","",""), nrow = 3, ncol = 3)
isAValidPlay(TestIsAValidPlay2,"")


randomPlay = function(board, piece){
  play = c("","")
  
  while(isAValidPlay(board, play) != TRUE){
    play = c(sample(3,1),sample(3,1))
  }
  board[play[1],play[2]] = piece
  return (board)
}

TestRandomPlay = matrix(c("","O","O","O","","O","","O","O"), nrow = 3, ncol = 3)
randomPlay(TestRandomPlay, "X")

numberToMatrixPosition = function(position){
  if(position == 1)  return(c(1,1))
  if(position == 2)  return(c(1,2))
  if(position == 3)  return(c(1,3))
  
  if(position == 4)  return(c(2,1))
  if(position == 5)  return(c(2,2))
  if(position == 6)  return(c(2,3))
  
  if(position == 7)  return(c(3,1))
  if(position == 8)  return(c(3,2))
  if(position == 9)  return(c(3,3))
}

TestNumberToMatrixPosition = 5
numberToMatrixPosition(TestNumberToMatrixPosition)


userPlay <- function() {
  print("Type '1.1' to '3.3' to choose a position to play")
  print("Example: 1.2, 1.3, 2.2 ...")
  userPlay = readline(prompt = "Movement:")
  userPlayArray = unlist(strsplit(userPlay, "[.]"))
}


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
    
    board = randomPlay(board, computerPiece)
    
    drawBoard(board)
    turn = user
    
  }else{
    playerMovementArray = userPlay()
    board[strtoi(playerMovementArray[1], base = 0L),strtoi(playerMovementArray[2], base = 0L)] = playerPiece
    
    drawBoard(board)
    turn = computer
  }
  
  
  while(checkVictory(board) == FALSE){
    if(turn == computer){
      computerPlay =  ""
      
      computerPlay = modelottt$Policy[paste(boardMatrixToArray(board),collapse="")]
     
      if(is.na(computerPlay)){
        board = randomPlay(board, computerPiece)
      }
      else if(isAValidPlay(board,numberToMatrixPosition(as.integer(substr(computerPlay,2,2))))){
        
        computerPlay = numberToMatrixPosition(as.integer(substr(computerPlay,2,2)))
        board[computerPlay[1],computerPlay[2]] = computerPiece
      }
      
      drawBoard(board)
      turn = user
    }
    
    if (checkVictory(board) == TRUE){
      break
    }
    
    if(turn == user){
      userPlayArray = ""
      
      while(isAValidPlay(board,userPlayArray) != TRUE){
        userPlayArray =  userPlay()
        userPlayArray = c(strtoi(userPlayArray[1], base = 0L),strtoi(userPlayArray[2], base = 0L))
        
      }
      
      board[userPlayArray[1],userPlayArray[2]] = playerPiece
      drawBoard(board)
      turn = computer
    }
    
    if (checkVictory(board) == TRUE){
      break
    }
    
  }
  drawBoard(board)
  
}
