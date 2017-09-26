import java.io.{File, PrintWriter}
import model.{Client, Order}
import scala.io.Source

object MatchingOrders extends App {
  def get[B](filePath: String, parser: (String => Option[B])) = {
    Source.fromFile(filePath).getLines.flatMap(parser(_)).toList
  }

  val resourcePath = "src/main/resources"
  val clientsFilePath = resourcePath + "/clients.txt"
  val ordersFilePath = resourcePath + "/orders.txt"
  val resultFilePath = resourcePath + "/result.txt"

  val clients = get(clientsFilePath, Client.reads)
  val orders = get(ordersFilePath, Order.reads)

  val updatedClients = Client.startTrade(clients, orders)

  val resultFile = new PrintWriter(new File(resultFilePath))

  try
    updatedClients.foreach(client => resultFile.write(Client.writes(client)))
  finally
    resultFile.close()
}
