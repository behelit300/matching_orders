package model

case class Client(
  name: String,
  balanceUsd: Int,
  stockA: Int,
  stockB: Int,
  stockC: Int,
  stockD: Int
) {
  def increaseUsd(usd: Int) = {
    copy(balanceUsd = balanceUsd + usd)
  }

  def increaseStock(balance: Int, stockName: String) = stockName match {
    case "A" => copy(stockA = stockA + balance)
    case "B" => copy(stockB = stockB + balance)
    case "C" => copy(stockC = stockC + balance)
    case "D" => copy(stockD = stockD + balance)
  }
}

object Client extends ClientFileParse {
  type StockName = String
  type OrdersTuple = (List[BuyOrder], List[SellOrder])
  type StockOrderMap = Map[StockName, OrdersTuple]

  type ClientName = String
  type ClientMap = Map[ClientName, Client]

  def startTrade(clients: List[Client], orders: List[Order]): List[Client] = {
    val startClientMap = clients.groupBy(_.name).mapValues(_.head)

    val (newClientMap, newStockOrderMap) =
      orders.foldLeft((startClientMap: ClientMap, Map.empty: StockOrderMap)) {
        case ((clientMap, stockOrderMap), order) =>

          val (existingBuyOrders, existingSellOrders): OrdersTuple =
            stockOrderMap.getOrElse(order.stockName, (List.empty, List.empty))

          order match {
            case buyOrder: BuyOrder =>
              buyOrder.execute(existingSellOrders) match {
                case (restOfOrder: Option[BuyOrder], updSellOrders: List[SellOrder], deals: List[Deal]) =>
                  val updBuyOrders = restOfOrder
                    .map { buyOrder => (buyOrder :: existingBuyOrders).sortBy(Order.sortBuy) }
                    .getOrElse(existingBuyOrders)
                  val updBaseStock = stockOrderMap.updated(buyOrder.stockName, (updBuyOrders, updSellOrders))
                  val updClientMap = Client.transaction(clientMap, deals)
                  (updClientMap, updBaseStock)
              }
            case sellOrder: SellOrder =>
              sellOrder.execute(existingBuyOrders) match {
                case (restOfOrder: Option[SellOrder], updBuyOrders: List[BuyOrder], deals: List[Deal]) =>
                  val updSellOrders = restOfOrder
                    .map { sellOrder => (sellOrder :: existingSellOrders).sortBy(Order.sortSell) }
                    .getOrElse(existingSellOrders)
                  val updBaseStock = stockOrderMap.updated(sellOrder.stockName, (updBuyOrders, updSellOrders))
                  val updClientMap = Client.transaction(clientMap, deals)
                  (updClientMap, updBaseStock)
              }
          }
      }
    newClientMap.values.toList.sortBy(_.name)
  }

  def transactionOne(clientMap: ClientMap, deal: Deal): Map[String, Client] = {
    (for {
      clientBuy <- clientMap.get(deal.clientBuyName)
      clientSell <- clientMap.get(deal.clientSellName)
    } yield {
      val updClientBuy = clientBuy.increaseUsd(-deal.sum).increaseStock(deal.amount, deal.stockName)
      val updClientSell = clientSell.increaseUsd(deal.sum).increaseStock(-deal.amount, deal.stockName)
      clientMap
        .updated(updClientBuy.name, updClientBuy)
        .updated(updClientSell.name, updClientSell)
    }) getOrElse clientMap
  }

  def transaction(clientMap: ClientMap, deals: List[Deal]): Map[String, Client] = {
    deals.foldLeft(clientMap) {
      (updClientMap, deal) => transactionOne(updClientMap, deal)
    }
  }
}

trait ClientFileParse {
  def reads(s: String): Option[Client] = s.split("\\s+") match {
    case Array(name, balanceUsd, balanceA, balanceB, balanceC, balanceD) =>
      Some(Client(name, balanceUsd.toInt, balanceA.toInt, balanceB.toInt, balanceC.toInt, balanceD.toInt))
    case _ => None
  }

  def writes(client: Client): String = s"${client.productIterator.mkString("\t")}\n"
}