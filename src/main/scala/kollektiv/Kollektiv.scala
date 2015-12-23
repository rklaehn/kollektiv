package kollektiv

object Kollektiv extends App {

  type Number = Double

  val minimumWage: Number = 800

  val minHourly: Number = 1600

  val maxHourly: Number = 2400

  val maxWage: Number = 4800

  val minHours: Number = 16

  require(minimumWage < minHourly)
  require(minHourly < maxHourly)
  require(maxHourly < maxWage)
  require(minHours > 0)

  /**
    *
    * @param profit total profit
    * @param hours a map containing the hours for each worker
    * @return a map of worker to net wage, and the amount going to the "Solidartopf"
    */
  def wages(profit: Number, hours: Map[String, Number]): (Map[String, Number], Number) = {
    require(!hours.isEmpty)
    require(hours.values.forall(_ >= minHours))
    val workers = hours.keys
    val n = workers.size
    val totalHours = hours.values.sum
    val profitPerHour = profit / totalHours
    val profitPerWorker = profit / n
    val profitByHours = hours.mapValues(_ * profitPerHour)
    if (profitPerWorker < minimumWage) {
      /*
      Sollte G kleiner oder gleich C * 800 € Netto sein, dann wird G
      unabhängig von den geleisteten Wochenstunden auf C gleichmäßig aufgeteilt.
      */
      // profit gets evenly distributed between all workers
      (workers.map(_ -> profitPerWorker).toMap, 0)
    } else if (profitPerWorker < minHourly) {
      /*
      Sollte G größer als 800 € Netto und kleiner als 1.600 € Netto sein, dann
      nähert sich die Ausschüttung immer stärker der stundenlinearen
      Ausschüttung an.
      */
      // linear interpolation factor between minimum wage and hourly range
      val f = (profitPerWorker - minimumWage) / (minHourly - minimumWage)
      (workers.map { worker => worker ->
        (f * profitPerWorker + (1 - f) * profitByHours(worker))
      }.toMap, 0)
    } else if (profitPerWorker < maxHourly) {
      /*
      Sollte G größer als C * 1.600 € Netto sein und kleiner als C * 2.400 €
      Netto, dann wird G exakt nach der Zahl der geleisteten Stunden W ausgezahlt.
      */
      (profitByHours, 0)
    } else if (profitPerWorker < maxWage) {
      /*
      Sollte G größer als C * 2.400 € Netto und kleiner als 4.800 € Netto
      sein, dann fließt ein langsam wachsender Anteil in einen Solidartopf S.
      Bei 2.400 € sind das 0 €, bei 4.800 €, die auf dem Konto der
      Kollektivista landen, landen 1.200 € im Solidartopf.
       */
      val f = (profitPerWorker - maxHourly) / (maxWage - maxHourly)
      // this range is not properly specified
      ???
    } else {
      /*
      4.800 € ist der Maximallohn, alles was darüber hinausgeht, fließt in den
      Solidartopf.
       */
      val surplus = profit - n * maxWage
      (workers.map(_ -> maxWage).toMap, surplus)
    }
  }

  // test cases
  println(wages(1000, Map("martin" -> 16, "sven" -> 16, "peter" -> 20)))
  println(wages(3000, Map("martin" -> 16, "sven" -> 16, "peter" -> 20)))
  println(wages(100000, Map("martin" -> 16, "sven" -> 16, "peter" -> 20)))
}