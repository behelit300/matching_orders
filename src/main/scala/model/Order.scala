package model

trait Order {
  val clientName: String
  val stockName: String
  val price: Int
  val amount: Int
  def canEqual(otherOrder: Order): Boolean
  def setAmount(amount: Int): Order
}

case class SellOrder(
  clientName: String,
  stockName: String,
  price: Int,
  amount: Int
) extends Order {
  def canEqual(otherOrder: Order): Boolean = otherOrder.isInstanceOf[SellOrder]

  def setAmount(amount: Int): SellOrder = copy(amount = amount)

  def execute(existingOrders: List[BuyOrder]) = {
    Order.executeOrder(this, existingOrders)
  }
}

case class BuyOrder(
  clientName: String,
  stockName: String,
  price: Int,
  amount: Int
) extends Order {
  def canEqual(a: Order): Boolean = a.isInstanceOf[BuyOrder]

  def setAmount(amount: Int): BuyOrder = copy(amount = amount)

  def execute(existingOrders: List[SellOrder]) = {
    Order.executeOrder(this, existingOrders)
  }
}

object Order extends OrderFileParse {
  val SELL_OPERATION = "s"
  val BUY_OPERATION = "b"

  val sortBuy = (order: Order) => -order.price
  val sortSell = (order: Order) => order.price

  def mergeOrders(buyOrder: BuyOrder, sellOrder: SellOrder): Option[Order] = {
    val diff = sellOrder.amount - buyOrder.amount
    if (diff > 0) {
      Some(sellOrder.setAmount(diff))
    } else if (diff < 0) {
      Some(buyOrder.setAmount(-diff))
    } else {
      None
    }
  }

  def executeOrder(
    order: Order, existingOrders: List[Order], deals: List[Deal] = List.empty
  ): (Option[Order], List[Order], List[Deal]) = {
    val notChange = (Some(order), existingOrders, deals)
    existingOrders.headOption
      .map { headOrder =>
        (order, headOrder) match {
          case (buyOrder: BuyOrder, sellOrder: SellOrder) => (buyOrder, sellOrder)
          case (sellOrder: SellOrder, buyOrder: BuyOrder) => (buyOrder, sellOrder)
        }
      }
      .filter { case (buyOrder, sellOrder) => sellOrder.price <= buyOrder.price }
      .map { case (buyOrder, sellOrder) =>
        val updatedDeals = deals :+ Deal(buyOrder, sellOrder)

        def removeHeadOrderAndNext(restOfOrder: Order) = executeOrder(restOfOrder, existingOrders.tail, updatedDeals)
        def modifyHeadOrder(restOfOrder: Order) = (None, restOfOrder :: existingOrders.tail, updatedDeals)
        def removeHeadOrder = (None, existingOrders.tail, updatedDeals)

        Order.mergeOrders(buyOrder, sellOrder) match {
          case Some(restOfOrder: Order) if restOfOrder.canEqual(order) => removeHeadOrderAndNext(restOfOrder)
          case Some(restOfOrder: Order) => modifyHeadOrder(restOfOrder)
          case None => removeHeadOrder
        }
      }
      .getOrElse(notChange)
  }
}

trait OrderFileParse {
  def reads(s: String): Option[Order] = s.split("\\s+") match {
    case Array(clientName, operationName, stockName, price, amount) =>
      val param = (clientName, stockName, price.toInt, amount.toInt)
      operationName match {
        case Order.SELL_OPERATION => Some(SellOrder.tupled(param))
        case Order.BUY_OPERATION => Some(BuyOrder.tupled(param))
        case _ => None
      }
    case _ => None
  }
}
