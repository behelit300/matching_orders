package model

case class Deal(
  price: Int,
  amount: Int,
  stockName: String,
  clientBuyName: String,
  clientSellName: String
) {
  val sum = price * amount
}

object Deal {
  def apply(buyOrder: BuyOrder, sellOrder: SellOrder): Deal = {
    val remainder = Math.max(sellOrder.amount - buyOrder.amount, 0)
    Deal(
      sellOrder.price,
      sellOrder.amount - remainder,
      sellOrder.stockName,
      buyOrder.clientName,
      sellOrder.clientName
    )
  }
}