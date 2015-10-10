/**
 * @author Arek Bojarski
 * Game of Life
 * Another one implementation Conoway Game of Life. This is my favorite project for learning new computer language. 
 * Board is just list of coordinates alive points. Every alive point generate list of 8 coordinates of "neighborhood". 
 * Last step is just calculation number of "neighborhood score" and keeping alive points with 2 or 3 "neighborhood score".    
 * That's all! Have fan!   
 */
object Game {
  val dim = 100          //dimension of board, board is infinite. This value is just for generating and printing board 
  var m = genRandomGame  // initialization, first state
  
  /*
   * nextState
   * 1) generation neighbors elements. Every point generate 8 elements around himself
   * 2) counting occurrence of vicinity
   * 3) implementation rules of Game of Life, rejecting points with "bad neighborhood"
   * 4) extracting keys from map, at this moment we don't care about number of neighbors   
   * 5) converting Set to List
   */
  def nextState={
    m.flatMap(f=> for(dx <-List(-1,0,1); dy <- List(-1,0,1) if (!(dx ==0 && dy == 0)) ) yield (f._1+dx,f._2+dy) )
     .groupBy(f => f)            
     .map(f=> (f._1, f._2.size))      
     .filter(f=> f._2 match{
                      case 2 => m.contains(f._1)
                      case 3 => true
                      case _ => false
    })
    .keys
    .toList     
  }
  
  /*
   * Let's play
   * Run the game!
   */
  def main(args: Array[String]) {
    for (i <- 0 to 1000){
      if (i%100 ==0) Game.paintBoard
      Game.m = Game.nextState
    }    
  }
  
  /*
   * Generation random board 
   */  
  def genRandomGame = {    
    val maxElement = (dim * dim)/3
    (0 to maxElement).map(_ => ((new util.Random).nextInt(dim),(new util.Random).nextInt(dim)) ).toList    
  }

  /*
   * genGlider 
   * Generation of simple glider
   */
  def genGlider = {    
    List( (3,3)
              ,(4,4)      
       ,(5,2),(5,3),(5,4)                  
    )    
  }

  /*
   *  paintBoard
   *  Paint board on console    
   */
  def paintBoard{
        
	  for(i <- 0 to dim; j <- 0 to dim) {
      if (j==0 && i!=0) println
      if ( m.contains((i,j))) print("X")
      else print("_")
    }
    println
    println("-----------------------------------")    
    
  }
}
