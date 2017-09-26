import model.{BuyOrder, Deal, Order, SellOrder}
import org.scalatest.{FunSpec, Matchers}
import Constants._

class OrderTest extends FunSpec with Matchers {
  val buyOrder = BuyOrder(clientNameC1, stockNameA, price = 120, amount = 10)
  val sellOrder = SellOrder(clientNameC2, stockNameA, price = 100, amount = 7)

  describe("Order.merge") {
    it("buyOrder > sellOrder") {
      val buyOrder = BuyOrder(clientNameC1, stockNameA, price = 120, amount = 10)
      val sellOrder = SellOrder(clientNameC2, stockNameA, price = 100, amount = 7)
      Order.mergeOrders(buyOrder, sellOrder) shouldBe Some(buyOrder.setAmount(3))
    }
    it("buyOrder < sellOrder") {
      val buyOrder = BuyOrder(clientNameC1, stockNameA, price = 120, amount = 5)
      val sellOrder = SellOrder(clientNameC2, stockNameA, price = 100, amount = 7)
      Order.mergeOrders(buyOrder, sellOrder) shouldBe Some(sellOrder.setAmount(2))
    }
    it("buyOrder == sellOrder") {
      val buyOrder = BuyOrder(clientNameC1, stockNameA, price = 120, amount = 7)
      val sellOrder = SellOrder(clientNameC2, stockNameA, price = 100, amount = 7)
      Order.mergeOrders(buyOrder, sellOrder) shouldBe None
    }
  }

  describe("Order.executeOrder") {
    it("buy all") {
      val buyOrder = BuyOrder(clientNameC1, stockNameA, price = 130, amount = 12)
      val sellOrders = List(
        SellOrder(clientNameC2, stockNameA, price = 100, amount = 5), // full buy
        SellOrder(clientNameC3, stockNameA, price = 110, amount = 5), // full buy
        SellOrder(clientNameC4, stockNameA, price = 120, amount = 5), // buy 2, 3 - remainder
        SellOrder(clientNameC2, stockNameA, price = 120, amount = 10)
      )

      val resultSellOrders = List(
        SellOrder(clientNameC4, stockNameA, price = 120, amount = 3),
        SellOrder(clientNameC2, stockNameA, price = 120, amount = 10)
      )
      val resultDeals = List(
        Deal(100, 5, stockNameA, clientBuyName = clientNameC1, clientSellName = clientNameC2),
        Deal(110, 5, stockNameA, clientBuyName = clientNameC1, clientSellName = clientNameC3),
        Deal(120, 2, stockNameA, clientBuyName = clientNameC1, clientSellName = clientNameC4)
      )
      Order.executeOrder(buyOrder, sellOrders) shouldBe(None, resultSellOrders, resultDeals)
    }

    it("buy 15/30") {
      val buyOrder = BuyOrder(clientNameC1, stockNameA, price = 130, amount = 30)
      val sellOrders = List(
        SellOrder(clientNameC2, stockNameA, price = 100, amount = 5), // full buy
        SellOrder(clientNameC3, stockNameA, price = 110, amount = 5), // full buy
        SellOrder(clientNameC4, stockNameA, price = 120, amount = 5) // full buy
      )

      val resultSellOrders = List.empty
      val resultDeals = List(
        Deal(100, 5, stockNameA, clientBuyName = clientNameC1, clientSellName = clientNameC2),
        Deal(110, 5, stockNameA, clientBuyName = clientNameC1, clientSellName = clientNameC3),
        Deal(120, 5, stockNameA, clientBuyName = clientNameC1, clientSellName = clientNameC4)
      )
      Order.executeOrder(buyOrder, sellOrders) shouldBe(Some(buyOrder.setAmount(15)), resultSellOrders, resultDeals)
    }

    it("sell all") {
      val sellOrder = SellOrder(clientNameC1, stockNameA, price = 130, amount = 30)

      val buyOrders = List(
        BuyOrder(clientNameC2, stockNameA, price = 150, amount = 5), // full sell
        BuyOrder(clientNameC3, stockNameA, price = 140, amount = 5), // full sell
        BuyOrder(clientNameC4, stockNameA, price = 130, amount = 5), // full sell
        BuyOrder(clientNameC2, stockNameA, price = 130, amount = 15), // full sell
        BuyOrder(clientNameC2, stockNameA, price = 130, amount = 10)
      )
      // sell order remainder 15

      val resultBuyOrders = List(
        BuyOrder(clientNameC2, stockNameA, price = 130, amount = 10)
      )
      val resultDeals = List(
        Deal(130, 5, stockNameA, clientBuyName = clientNameC2, clientSellName = clientNameC1),
        Deal(130, 5, stockNameA, clientBuyName = clientNameC3, clientSellName = clientNameC1),
        Deal(130, 5, stockNameA, clientBuyName = clientNameC4, clientSellName = clientNameC1),
        Deal(130, 15, stockNameA, clientBuyName = clientNameC2, clientSellName = clientNameC1)
      )
      Order.executeOrder(sellOrder, buyOrders) shouldBe(None, resultBuyOrders, resultDeals)
    }

    it("sell 15/30") {
      val sellOrder = SellOrder(clientNameC1, stockNameA, price = 130, amount = 30)

      val buyOrders = List(
        BuyOrder(clientNameC2, stockNameA, price = 150, amount = 5), // full sell
        BuyOrder(clientNameC3, stockNameA, price = 140, amount = 5), // full sell
        BuyOrder(clientNameC4, stockNameA, price = 130, amount = 5), // full sell
        BuyOrder(clientNameC2, stockNameA, price = 120, amount = 5) // 120 < 130
      )
      // sell order remainder 15

      val resultBuyOrders = List(
        BuyOrder(clientNameC2, stockNameA, price = 120, amount = 5)
      )
      val resultDeals = List(
        Deal(130, 5, stockNameA, clientBuyName = clientNameC2, clientSellName = clientNameC1),
        Deal(130, 5, stockNameA, clientBuyName = clientNameC3, clientSellName = clientNameC1),
        Deal(130, 5, stockNameA, clientBuyName = clientNameC4, clientSellName = clientNameC1)
      )
      Order.executeOrder(sellOrder, buyOrders) shouldBe(Some(sellOrder.setAmount(15)), resultBuyOrders, resultDeals)
    }
  }
}
