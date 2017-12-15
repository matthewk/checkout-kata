package com.matthewk

import com.matthewk.domain.{Catalogue, Item, Price, PricingRules}
import com.matthewk.domain.SKU._

object InitialData extends ItemValidatorNel {
  val catalogue: Catalogue =
    Catalogue(
      Item("A".toSKU, Price(50)),
      Item("B".toSKU, Price(30)),
      Item("C".toSKU, Price(20)),
      Item("D".toSKU, Price(15))
    )


  val pricingRules: PricingRules = (for {
    r1 <- validateQuantityRule("A", 3, BigDecimal(130), catalogue.items.values.toList).toEither
    r2 <- validateQuantityRule("B", 2, BigDecimal(45), catalogue.items.values.toList).toEither
  } yield PricingRules(r1, r2))
    .fold(
      err => {
        println(err.map(_.message).toList.mkString(","))
        PricingRules.Empty
      },
      pr => pr
    )

}
