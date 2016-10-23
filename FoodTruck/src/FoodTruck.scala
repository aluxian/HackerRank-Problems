import scala.io.Source

case class TestCase(numPages: Int, pageSize: Int, addresses: List[Int])

case class Subscriber(timestamp: String, location: Location, phone: String)

case class Location(lat: Double, lon: Double)

case class Madhu(location: Location)

object Solution {

  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines

    val coords = lines.next().split(',')
    val madhu = Madhu(Location(coords(0).toDouble, coords(1).toDouble))
    val r = lines.next().toDouble

    // skip header
    lines.next()

    // read records
    val records = lines
      .takeWhile(_.nonEmpty)
      .map(_.split(','))
      .map(l => Subscriber(fixTimestamp(l(0)), Location(l(1).toDouble, l(2).toDouble), l(3)))
      .toList
      .groupBy(_.phone)
      .map {
        case (phone, entries) =>
          if (entries.size > 1) {
            entries
              .sortBy(_.timestamp)
              .reverse
              .head
          } else {
            entries.head
          }
      }
      .toList

    // process and print results
    val phones = run(madhu, r, records)
    println(phones.mkString(","))
  }

  def run(madhu: Madhu, searchRadius: Double, records: List[Subscriber]): List[String] = {
    records
      .map(s => (s.phone, haversine(s.location, madhu.location)))
      .filter { case (phone, distance) => distance < searchRadius }
      .map(_._1)
      .distinct
      .sorted
  }

  def fixTimestamp(timestamp: String): String = {
    timestamp.substring(6, 10) + "/" + timestamp
  }

  def haversine(loc1: Location, loc2: Location): Double = {
    import Math._
    val dLat = toRadians(loc2.lat - loc1.lat)
    val dLon = toRadians(loc2.lon - loc1.lon)
    val lat1Rad = toRadians(loc1.lat)
    val lat2Rad = toRadians(loc2.lat)
    val a = pow(sin(dLat / 2), 2) + pow(sin(dLon / 2), 2) * cos(lat1Rad) * cos(lat2Rad)
    val c = 2 * asin(sqrt(a))
    6378.137 * c
  }

}
