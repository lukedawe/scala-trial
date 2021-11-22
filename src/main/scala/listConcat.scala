import scala.io.Source

object listConcat extends App {
  def concat (list1: List[Int], list2: List[Int]): List[Int] = {
    return(list1 ::: list2)
  }

  def count_words ():Int = {
    val filename = "src/main/scala/bleak-house.txt"
    var count = 0
    for (line <- Source.fromFile(filename).getLines) {
      for(word <- line.split(" ")){
        count +=1
      }
    }
    return count
  }

  // https://alvinalexander.com/scala/how-to-add-update-remove-elements-immutable-maps-scala/
  def count_chars () = {
    var map:Map[Char,Int] = Map()
    val filename = "src/main/scala/bleak-house.txt"
    var count = 0
    for (line <- Source.fromFile(filename).getLines) {
      for(word <- line.split(" ")){
        for(char <- word) {
          if(map contains char){
            count = map(char) + 1
            map = map + (char -> count)
          }
          else{
            map += (char -> 1)
          }
        }
      }
    }
    println(map)
  }

  def est_pi(prev_estimation: Double, divisor: Double, operator: String) : Double = {
    var current_estimation = 0.0

    // if the operator is plus
    if(operator == "p"){
      current_estimation = prev_estimation + 1/divisor
    }
    else if(operator == "m"){
      current_estimation = prev_estimation - 1/divisor
    }

    var difference = math.sqrt((current_estimation - prev_estimation)*(current_estimation - prev_estimation))
    if (difference < 0.001){
      println("current estimation: " + current_estimation * 4)
      println("previous estimation: " + prev_estimation)
      println("difference: " + difference)
      return current_estimation
    }
    else if(operator == "p"){
      est_pi(current_estimation, divisor + 2, "m")
    }
    else if(operator == "m"){
      est_pi(current_estimation, divisor + 2, "p")
    }

    return 0.0
  }

  //count_chars()
  println("Returned Value : " + est_pi(1.0, 3.0, "m"))

}
