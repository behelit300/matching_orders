import model._
import org.scalatest.{FunSpec, Matchers}
import Constants._

class ClientTest extends FunSpec with Matchers {
  val clients = List(
    Client(clientNameC1, 100, 10, 10, 10, 10),
    Client(clientNameC2, 100, 10, 10, 10, 10),
    Client(clientNameC3, 100, 10, 10, 10, 10)
  )
  val orders = List(
    SellOrder(clientNameC1, stockNameA, 12, 4),
    BuyOrder(clientNameC2, stockNameB, 14, 2),
    BuyOrder(clientNameC3, stockNameA, 13, 2),
    SellOrder(clientNameC3, stockNameB, 13, 2)
  )
  val clientMap = clients.groupBy(_.name).mapValues(_.head)

  it("Client.transactionOne") {
    val deal = Deal(price = 10, amount = 5, stockNameA, clientBuyName = clientNameC1, clientSellName = clientNameC3)
    val resultClientMap = Map(
      clientNameC1 -> Client(clientNameC1, 50, 15, 10, 10, 10),
      clientNameC2 -> Client(clientNameC2, 100, 10, 10, 10, 10),
      clientNameC3 -> Client(clientNameC3, 150, 5, 10, 10, 10)
    )
    Client.transactionOne(clientMap, deal) shouldBe resultClientMap
  }

  it("Client.startTrade") {
    val resultClients = List(
      Client(clientNameC1, 124, 8, 10, 10, 10),
      Client(clientNameC2, 74, 10, 12, 10, 10),
      Client(clientNameC3, 102, 12, 8, 10, 10)
    )
    Client.startTrade(clients, orders) shouldBe resultClients
  }
}
