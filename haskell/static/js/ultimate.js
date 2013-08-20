function sketchProc(p){
  // Originally written by James Irwin at https://www.khanacademy.org/cs/tic-tac-toe-ception/1676336506
  
  var botThinkingTime = 2505; //in ms for MonteCarloBot
  // whether the monteCarloBot should weight each outcome by how quickly it is
  // won/lost (this will hopefully dicourage Monte from losing quickly when it
  // can instead drag the game out, likewise it will hopefully encourage Monte
  // to choose options that will win the game more quickly). 
  var monteWeightByGameLength = true;
  //sometimes it might be better strategically to move somewhere that will give
  //the opponent a choice of boards to play on, but this will be very rare. To
  //help MonteCarloBot avoid giving their opponent a choice, count any loss
  //this number of times more than any win.  Monte needs this because it does a
  //bad job of evaluating when an opponent has choice because it will be
  //randomly placing the next move on all squares on the board, many which are
  //bad and a smart opponent will avoid.  Another possibility to handle this is
  //to run another monte carlo simulation for the opponent for that move with
  //choice and score them by that.  For now this is a simpler approach -- with
  //a high enough value Monte will avoid giving their opponent any choice
  //unless no matter what the opponent choose randomly they ALWAYS lost.
  var monteChoicePenalty=10;

  var useRule5b = false;

  var debugMode = false;
  var debugHistory = true;

  //the size of the buffer around each board
  var boardBuffer=5;

  //the size of the buffer around each players mark
  var cellBuffer=5;
  var cellSize=38;
  var xColor = p.color(255, 0, 0);
  var yColor = p.color(34, 54, 145);

  var playerX = {
    color : p.color(0, 0, 0),
    background: p.color(201, 106, 106),
    name: "X",

    drawShape: function(x,y) {
      //buffer
      var b = cellBuffer;
      //size of shape
      var s = cellSize-2*b;
      //width of line
      p.strokeWeight(3);
      p.stroke(this.color);
      p.line(x+b,y+b,x+b+s, y+b+s);
      p.line(x+b+s,y+b,x+b, y+b+s);
    }
  };

  var playerO = {
    color : p.color(0, 0, 0),
    background: p.color(105, 174, 199),
    name: "O",

    drawShape: function(x,y) {
      //buffer
      var b = 5;
      //size of shape
      var s = cellSize-2*b;
      //width of line
      p.strokeWeight(3);
      p.stroke(this.color);
      p.ellipse(x+b+s/2,y+b+s/2,s,s);
    }
  };

  var getGlobal = function (){
    return (function(){
      return this;
    }).call(null);
  };

  var g = getGlobal();
  var $ = g.$;

  //from jquery
  var arrayRemove = function(array, from, to) {
    var rest = array.slice((to || from) + 1 || array.length);
    array.length = from < 0 ? array.length + from : from;
    return array.push.apply(array, rest);
  };

  var Cell = function(row, col) {
    this.size = 38;
    //The row and col of the cell within its board
    this.row = row;
    this.col = col;
    this.owner = null;
    //keeps track of the winner of the board this cell is in
    this.winner = null;

    this.setPos = function(x,y){
      //The pixel coords of the cell
      this.x = x;
      this.y = y;
    };

    this.setWinner = function(winner) {
      this.winner = winner;
      this.setBackground(this.winner.background);
    };

    this.setBackground = function(color) {
      p.noStroke();
      p.fill(color);
      p.rect(this.x,this.y,this.size,this.size);
      if(this.owner) {
        this.owner.drawShape(this.x, this.y);   
      }
    };

    this.playCell = function(newOwner) {
      this.owner = newOwner;
      if(!this.owner) {
        return;
      }
      if(!this.winner) {
        this.setBackground(this.owner.background);          
      } else {
        this.setBackground(this.winner.background);
      }
    };
  };

  var TicTacToeBoard = function(row, col) {
    this.row = row;
    this.col = col;
    this.size=130;
    var b=boardBuffer;
    this.innerSize=this.size-2*boardBuffer;
    this.winner=null;

    this.cells=[[],[],[]];
    for(var i=0; i<3; i++){
      for(var j=0; j<3; j++){
        this.cells[i][j] = new Cell(i, j);
      }
    }

    this.clone = function() {
      var clone = clone(this);
      for(var i=0; i<3; i++){
        for(var j=0; j<3; j++){
          clone.cells[i][j] = clone(this.cells[i][j]);
        }
      }
      return clone;
    };

    this.highlight = function() {
      p.strokeWeight(3);
      p.stroke(255, 0, 0);
      p.noFill();
      p.rect(this.x+1,this.y+1, this.size-3, this.size-3);
    };

    this.unhighlight = function() {
      p.strokeWeight(3);
      p.stroke(255, 255, 255);
      p.noFill();
      p.rect(this.x+1,this.y+1, this.size-3, this.size-3);            
    };

    this.draw = function(y,x) {
      p.noStroke();

      //the position of the board on the screen
      this.y = y;
      this.x = x;
      //size of the board
      var s = this.size;
      //inner size of board
      var i = this.innerSize;
      //size of cell
      var c = 38;
      //width of dividing line
      var w = 3;
      //relative pos of first line
      var d1 = c;
      //relative pos of second line
      var d2 = 2*c+w;
      //relative pos of the cells
      this.p = [0,d1+w,d2+w];

      //set background color
      if(this.winner) {
        p.fill(this.winner.background);
      } else {
        //fill(255, 255, 0);
        p.fill(255, 255, 255);
      }

      //draw background 
      p.rect(this.x,this.y,s,s);

      p.fill(0, 0, 0);
      p.rect(this.x+b,this.y+b+d1,i,w);
      p.rect(this.x+b,this.y+b+d2,i,w);
      p.rect(this.x+b+d1,this.y+b,w,i);
      p.rect(this.x+b+d2,this.y+b,w,i);


      for(var row=0; row<3; row++){
        for(var col=0; col<3; col++){
          var cell = this.cells[row][col];
          cell.setPos(this.x+b+this.p[col],
              this.y+b+this.p[row]);
          if(cell.owner) {
            cell.playCell(cell.owner); 
          }
        }
      }
    };

    this.isFull = function() {
      for(var i=0; i<3; i++) {
        for(var j=0; j<3; j++){
          if(!this.cells[i][j].owner) {
            return false;
          }
        }
      }
      return true;
    };

    this.getEmptyCells = function(){
      /* Gets a list of the empty cells on the board
       *
       * TODO: This gets called a lot from the AI so we 
       * should probably cache it.
       */
      console.log("getempty", this.row, this.col)
      var emptyCells = [];
      for(var i=0; i<3; i++){
        for(var j=0; j<3; j++){
          if(!this.cells[i][j].owner){
            emptyCells.push(this.cells[i][j]);
          }
        }
      }
      console.log("return", emptyCells)

      return emptyCells;
    };

    this.getCellCoords = function(x,y) {
      var relX = x-this.x;
      var relY = y-this.y;
      var row, col;
      if (relY<b+this.p[1]){
        row = 0;
      } else if(relY<b+this.p[2]){
        row = 1;
      } else {
        row = 2;
      }
      if (relX<b+this.p[1]){
        col = 0;
      } else if(relX<b+this.p[2]){
        col = 1;
      } else {
        col = 2;
      }
      return [row, col];
    };

    this.playCellSilently = function(row, col, player) {
      /** Plays the cell without showing the UI */
      var cell = this.cells[row][col];
      cell.owner = player;
      if(!this.winner){
        var justWon = this.checkWon(row, col);
        if(justWon) {
          this.winner = player;
          for(var i=0; i<3; i++) {
            for(var j=0; j<3; j++) {
              this.cells[i][j].winner=player;
            }
          }
        }
        return justWon;
      } 
      return false;
    }; 

    this.playCell = function(row, col, player) {
      var cell = this.cells[row][col];

      if (!cell.owner) {
        cell.playCell(player);
        var won = false;
        if(!this.winner){
          won = this.checkWon(row, col);
          if(won) {
            this.winner = player;
            for(var i=0; i<3; i++) {
              for(var j=0; j<3; j++) {
                this.cells[i][j].setWinner(
                    player);
              }
            }
          }
        }
        return won;
      } else {
        debug("Should not get here");
      }
      return false;

    };

    this.checkWon = function(row, col) {
      /* check if the move at row, col won the board */
      var player = this.cells[row][col].owner;
      //check the row
      var i=0;
      while(i<3 && 
          this.cells[row][i].owner &&
          this.cells[row][i].owner.name === player.name){
            i += 1;
          }
      if(i === 3){
        return true;
      }

      //check the col
      i=0;
      while(i<3 && 
          this.cells[i][col].owner &&
          this.cells[i][col].owner.name === player.name){
            i += 1;
          }
      if(i === 3){
        return true;
      }        

      //check the upper left to lower right diagnol
      i=0;
      while(i<3 && 
          this.cells[i][i].owner &&
          this.cells[i][i].owner.name === player.name){
            i += 1;
          }
      if(i === 3){
        return true;
      }  

      //check the uper right to lower left diagnol
      i=0;
      while(i<3 && 
          this.cells[i][2-i].owner &&
          this.cells[i][2-i].owner.name === player.name){
            i += 1;
          }
      if(i === 3){
        return true;
      }  
      //None of the rows, cols, or diagnols were winning
      return false;
    };
  };

  var Game = function() {
    this.size = 400;
    this.currentPlayer = playerX;
    this.currentBoard = null;
    this.finished = false;
    this.winner = null;
    this.countFilled = 0;
    this.moveHistory = "";

    this.boards = [[],[],[]];
    for(var row=0; row<3; row++){
      for(var col=0; col<3; col++){   
        this.boards[row][col] = new TicTacToeBoard(row, col);
      }
    }

    this.draw = function() {
      p.noStroke();
      //size of board
      var s = 400;
      //size of cell
      var c = 130;
      //width of divider
      var w = 5;
      //pos of first line
      var d1 = c;
      //pos of second line
      var d2 = 2*c+w;
      //pos of the cells
      this.p = [0,d1+w,d2+w];
      p.fill(0, 0, 0);
      p.rect(0,d1,s,w);
      p.rect(0,d2,s,w);
      p.rect(d1,0,w,s);
      p.rect(d2,0,w,s);

      for(var row=0; row<3; row++){
        for(var col=0; col<3; col++){
          var b = this.boards[row][col];
          b.draw(this.p[row], this.p[col]);
        }
      }

    };

    this.playCellSilently = function(board_row, board_col,
        cell_row, cell_col) {
          /** Plays move without showing to the UI 
           *
           * This is useful for Bots. It assumes valid input. 
           **/
          this.moveHistory += "game.playCell("+board_row+","+
            board_col+","+
            cell_row+","+
            cell_col+");\n";
          var board = this.boards[board_row][board_col];
          var cell = board.cells[cell_row][cell_col];
          var justWon = board.playCellSilently(
              cell_row,cell_col, this.currentPlayer);
          //debug("playing at", board_row, board_col,
          //                             cell_row, cell_col);
          this.countFilled += 1;
          if(justWon) {
            if(this.checkWonGame(board_row, board_col,
                  true)) {
                    this.finished=true;
                    this.winner = this.currentPlayer;
                    return;
                  }
          }
          this.currentBoard = this.boards[cell_row][cell_col];
          if(this.currentBoard.isFull()) {
            this.currentBoard = null;
          } else if (this.currentBoard.winner && false) {
            this.currentBoard = null;
          } 
          if(this.countFilled === 81) {
            this.finished=true;
            return;
          }
          if(this.currentPlayer.name === playerX.name){
            this.currentPlayer = playerO;
          } else {
            this.currentPlayer = playerX;
          }
        };

    this.playCell = function(board_row, board_col,
        cell_row, cell_col) {
          this.moveHistory += "game.playCell("+board_row+","+
            board_col+","+
            cell_row+","+
            cell_col+");\n";
          var board = this.boards[board_row][board_col];
          if(this.currentBoard && 
              board !== this.currentBoard){
                //TODO: show error message or flash correct board
                debug("Error - wrong board");
                debug(this.currentBoard);
                return;
              }
          var cell = board.cells[cell_row][cell_col];

          if(cell.owner) {
            //Invalid play, most likely just held down mouse
            //over last move, so ignoring
            return;
          }

          var justWon = board.playCell(cell_row,cell_col,
              this.currentPlayer);
          this.countFilled += 1;
          if(this.currentBoard) {
            this.currentBoard.unhighlight();                
          }

          if(justWon) {
            if(this.checkWonGame(board_row, board_col)) {
              this.finished=true;
              this.winner = this.currentPlayer;
              this.showWinningScreen();
              return;
            }
          }

          this.currentBoard = this.boards[cell_row][cell_col];
          if(this.currentBoard.isFull()) {
            this.currentBoard = null;
            if(this.countFilled === 81) {
              this.finished=true;
              this.showDrawScreen();
            }
          } else if (this.currentBoard.winner && useRule5b) {
            this.currentBoard = null;
          } else {
            this.currentBoard.highlight();                
          }

          if(this.currentPlayer === playerX){
            this.currentPlayer = playerO;
          } else {
            this.currentPlayer = playerX;
          }
        };

    this.playAt = function(x,y) {
      var row = p.floor(y/(this.size/3));
      var col = p.floor(x/(this.size/3));
      var board = this.boards[row][col];
      var cell_coords = board.getCellCoords(x,y);
      var cell_row = cell_coords[0];
      var cell_col = cell_coords[1];
      this.playCell(row,col,cell_row,cell_col);
    };

    this.getValidMoves = function() {
      var validBoards = [];
      if(this.currentBoard) {
        validBoards.push(this.currentBoard);
      } else {
        //Choose a random board
        if(useRule5b) {
          validBoards=this.getNonFinishedBoards();
        } else {
          validBoards=this.getNonFullBoards();
        }
      }

      var validMoves = [];
      for(var i=0; i<validBoards.length; i++){
        var board = validBoards[i];
        var validCells=board.getEmptyCells();
        for (var j=0; j<validCells.length; j++){
          var cell = validCells[j];
          validMoves.push([board.row, board.col, 
              cell.row, cell.col]); 
        }

      }
      return validMoves;
    };

    this.getNonFinishedBoards = function() {
      /* Returns a list of the boards that aren't finished
       *
       * A board is considered finished if it is full or
       * if someone has won it.
       * TODO: This might get called a lot from 
       * the Bots so we should probably cache it.
       */
      var nonFinishedBoards=[];
      for(var i=0; i<3; i++){
        for(var j=0; j<3; j++){
          var board = this.boards[i][j];
          if(!board.winner && !board.isFull()){
            nonFinishedBoards.push(board);
          }
        }
      } 
      return nonFinishedBoards;
    };

    this.getNonFullBoards = function() {
      /* Returns a list of the non-full boards
       *
       * TODO: This might get called a lot from 
       * the Bots so we should probably cache it.
       */
      var nonFullBoards=[];
      for(var i=0; i<3; i++){
        for(var j=0; j<3; j++){
          if(!this.boards[i][j].isFull()){
            nonFullBoards.push(
                this.boards[i][j]);
          }
        }
      } 
      return nonFullBoards;
    };

    this.showDrawScreen = function() {
      p.fill(255, 255, 255);
      p.strokeWeight(3);
      p.stroke(46, 209, 65);
      var boxx = 95;
      var boxy = 150;
      p.rect(boxx, boxy, 210, 100);
      p.fill(0,0,0);
      p.textSize(40);
      p.text("The game", boxx+10, boxy+48);
      p.text("is a draw!", boxx+18, boxy+90);
    };

    this.showWinningScreen = function(player){
      if(player) {
        for(var i=0; i<3; i++) {
          for(var j=0; j<3; j++) {
            for(var k=0; k<3; k++) {
              for (var l=0; l<3; l++) {
                this.boards[i][j].cells[k][l].
                  setBackground(
                      this.currentPlayer
                      .background);
              }
            }
          }
        }
      }
      fill(255, 255, 255);
      strokeWeight(3);
      stroke(46, 209, 65);
      var boxx = 95;
      var boxy = 150;
      rect(boxx, boxy, 210, 100);
      var winnerImage = new Cell();
      winnerImage.setPos(boxx+10, boxy+10);
      winnerImage.playCell(this.currentPlayer);
      fill(this.currentPlayer.background);
      textSize(40);
      text("has won", boxx+55, boxy+48);
      text("the game!", boxx+17, boxy+90);
    };

    this.checkWonGame = function(row, col, silent) {
      /* check if the move at row, col won the board */
      var player = this.boards[row][col].winner;
      //check the row
      var i=0;
      while(i<3 && 
          this.boards[row][i].winner &&
          this.boards[row][i].winner.name === player.name
          ) {
            i += 1;
          }
      if(i === 3){
        if(!silent) {
          for(i=0; i<3; i++) {
            this.boards[row][i].highlight();
          }                
        }
        return true;
      }

      //check the col
      i=0;
      while(i<3 && 
          this.boards[i][col].winner &&
          this.boards[i][col].winner.name === player.name
          ) {
            i += 1;
          }
      if(i === 3){
        if(!silent) {
          for(i=0; i<3; i++) {
            this.boards[i][col].highlight();
          }                
        }
        return true;
      }        

      //check the upper left to lower right diagnol
      i=0;
      while(i<3 && 
          this.boards[i][i].winner &&
          this.boards[i][i].winner.name === player.name
          ) {
            i += 1;
          }
      if(i === 3){
        if(!silent) {
          for(i=0; i<3; i++) {
            this.boards[i][i].highlight();
          }               
        }
        return true;
      }  

      //check the uper right to lower left diagnol
      i=0;
      while(i<3 && 
          this.boards[i][2-i].winner &&
          this.boards[i][2-i].winner.name === player.name
          ) {
            i += 1;
          }
      if(i === 3){
        if(!silent) {
          for(i=0; i<3; i++) {
            this.boards[i][2-i].highlight();
          }                
        }
        return true;
      }  
      //None of the rows, cols, or diagnols were winning
      return false;
    };
  };

  var RandomBot = {
    getRandomValidMove: function(game) {
      var board;

      if(game.currentBoard) {
        board = game.currentBoard;
        console.log("current board", board)
      } else {
        //Choose a random board
        var nonFullBoards = game.getNonFullBoards();
        var boardNum = p.floor(p.random(0, nonFullBoards.length));
        if (nonFullBoards.length == 0){ 
          console.log("all full")
        }
        board = nonFullBoards[boardNum];
      }

      var emptyCells = board.getEmptyCells();

      if (emptyCells.length == 0) {
        //Choose a random board
        var nonFullBoards = game.getNonFullBoards();
        var boardNum = p.floor(p.random(0, nonFullBoards.length));
        if (nonFullBoards.length == 0){ 
          console.log("all full")
        }
        board = nonFullBoards[boardNum];
      }

      console.log(board.getEmptyCells())
      var cellNum = p.floor(p.random(0, emptyCells.length));
      var cell = emptyCells[cellNum];
      if (emptyCells.length == 0) {
        console.log("empty")
      }
      return [board.row, board.col, cell.row, cell.col];
    },

    playOutHidden: function(game) {
      /**Plays out the rest of the game hidden from the UI
       * and returns the winner.
       **/
      while (!game.finished) {
        var move = this.getRandomValidMove(game);
        game.playCellSilently(move[0], move[1], move[2], move[3]);
      }
      return game.winner;
    },

    play: function(game){
      var move = this.getRandomValidMove(game);
      game.playCell(move[0], move[1], move[2], move[3]);
    }   

  };

  var MonteCarloBot = $.extend(true, {},RandomBot);
  MonteCarloBot.play = function(game){
    debugGame = game;  
    var move;
    var validMoves = game.getValidMoves();

    //Create a cloned game with each of the possible next moves.
    var clones = [];
    var clone;

    for (var i=0; i<validMoves.length; i++) {
      move = validMoves[i];
      clone = $.extend(true, {}, game);
      clone.playCellSilently(move[0], move[1], move[2], move[3]);

      clones.push({
        boardRow: move[0],
        boardCol: move[1],
        cellRow: move[2],
        cellCol: move[3],
        clone: clone,
        wins: 0,
        weightedWins: 0,
        losses: 0,
        weightedLosses: 0,
        ties: 0
      });
    }

    //For each of the possible next moves
    var start = g.Date.now();
    var current = start;
    //allow 5 seconds to compute next moves
    var end = start + botThinkingTime;

    while (current < end) {
      for (i=0; i < clones.length; i++) {
        move = clones[i];
        var simgame = $.extend(true, {}, move.clone);
        var winner = RandomBot.playOutHidden(simgame);
        var weight = 1;
        if(monteWeightByGameLength) {
          //number of moves it took for 
          //sim to finish
          var f = simgame.countFilled - 
            game.countFilled;
          //number of moves remaining in game
          var r = 81 - 
            game.countFilled;
          // add a weight such that if the game finishes in 0 moves the
          // weight is 1, if it drags out until the end then it is worth only
          // 1/# moves till the end and anything in between is inversely
          // proportional to the number of moves it takes.
          weight = (r - f) / r;
        }

        if (winner === null) {
          move.ties += 1;
        } else 
          if(winner.name === game.currentPlayer.name){
            move.wins += 1;
            move.weightedWins += 1*weight;
          }  else {
            move.losses +=1;
            move.weightedLosses += 1* weight;
          }
      }
      current = g.Date.now();
    }

    var bestScore = -10000;
    var bestMove = null;
    for (i=0; i< clones.length; i++) {
      var penalty = 1;
      clone = clones[i];
      if (!clone.clone.currentBoard){
        penalty = monteChoicePenalty;
      }
      var score = (
          clone.weightedWins-
          clone.weightedLosses*penalty)/
        (clone.wins+clone.losses+clone.ties);
      debug(score, [clone.boardRow, clone.boardCol,             
          clone.cellRow, clone.cellCol], [clone.wins, clone.ties,
          clone.losses], clone.weightedWins, 
          clone.weightedLosses, penalty);
      if(score > bestScore) {
        bestScore = score;
        bestMove = clone;
      }
    }

    game.playCell(bestMove.boardRow, bestMove.boardCol, bestMove.cellRow, bestMove.cellCol);              
  };


  p.size(400, 400);
  p.background(255, 255, 255);

  var game = new Game();
  game.draw();

  p.mousePressed = function(){
    game.playAt(p.mouseX, p.mouseY);

    if(!game.finished) {
          MonteCarloBot.play(game);
    }
  }
};



function playTicTacToe(divel) {
  var canvas = document.createElement("canvas");
  $(canvas).attr('width', '400px');
  $(canvas).attr('height', '400px');
  divel.append(canvas)

  // attaching the sketchProc function to the canvas
  var processingInstance = new Processing(canvas, sketchProc);
}

