package com.matthewk.ui

import cats.data.NonEmptyList
import com.matthewk.{InputValidation, ItemValidatorNel}
import com.matthewk.domain._

trait Handler {
  def handle(ap: ApplicationParams, menuOptions: List[String], args: List[String] = List.empty[String]): ApplicationState
}

trait ViewBasketHandler extends Handler {
  self: Views =>

  override def handle(ap: ApplicationParams, menuOptions: List[String], args: List[String]): ApplicationState = {
    showBasketView(ap, menuOptions, None, None)
    ContinueState(ap)
  }
}

trait ViewCatalogueHandler extends Handler {
  self: Views =>

  override def handle(ap: ApplicationParams, menuOptions: List[String], args: List[String]): ApplicationState = {
    showCatalogueView(ap, menuOptions)
    ContinueState(ap)
  }
}

trait AddItemsHandler extends Handler {
  self: Views with InputValidation =>

  private def validate(args: List[String], ap: ApplicationParams) =
    ItemValidatorNel.validateAddItems(args.map(_.toUpperCase), ap.catalogue)

  private def successMessage(count: Int) = s"Successfully added $count items to your basket"

  private def validSKUsFromOriginalList(args: List[String], badSKUStr: List[String]) = {
    args.map(_.toUpperCase).filterNot(badSKUStr.toSet)
  }

  private def hasSKUsToAdd(args: List[String], badSKUStr: List[String]) = {
    badSKUStr.lengthCompare(args.size) < 0
  }

  private def extractSkuFromErrors(err: NonEmptyList[DomainValidation]) = {
    err.map {
      case SKUParseError(value)   => Some(value.toUpperCase())
      case ItemNotFoundError(sku) => Some(sku.toString.toUpperCase())
      case _                      => None
    }.toList.flatten
  }

  override def handle(ap: ApplicationParams, menuOptions: List[String], args: List[String]): ApplicationState = {

    def addOrError(
      args: List[String],
      params: ApplicationParams,
      success: Option[String] = None,
      errors: Option[List[DomainValidation]] = None
    ): ApplicationState = {
      validate(args, params).fold(
        err => {

          extractSkuFromErrors(err) match {
            case badSKUStr if hasSKUsToAdd(args, badSKUStr) =>
              addOrError(
                validSKUsFromOriginalList(args, badSKUStr),
                ap,
                None,
                Some(err.toList)
              )

            case _ =>
              showAddItemsView(ap, menuOptions, None, Some(err.toList))
              ContinueState(ap)
          }
        },
        i => {
          val newAp = ap.copy(basket = ap.basket.addItems(i))
          showAddItemsView(newAp, menuOptions, Some(successMessage(args.size)), errors)
          ContinueState(newAp)
        }
      )
    }

    addOrError(args, ap)
  }


}

trait ViewPricingRulesHandler extends Handler {
  self: Views =>
  override def handle(ap: ApplicationParams, menuOptions: List[String], args: List[String]): ApplicationState = {
    showOffersView(ap, menuOptions)
    ContinueState(ap)
  }
}

trait EmptyBasketHandler extends Handler {
  self: Views =>
  override def handle(ap: ApplicationParams, menuOptions: List[String], args: List[String]): ApplicationState = {
    val newAp = ap.copy(basket = Basket.Empty)
    showBasketView(newAp, menuOptions, Some("Successfully emptied your basket"), None)
    ContinueState(newAp)
  }
}

trait UnknownHandler extends Handler {
  self: Views =>
  def cmd: String
  override def handle(ap: ApplicationParams, menuOptions: List[String], args: List[String]): ApplicationState = {
    showBasketView(ap, menuOptions, None, Some(List(UnknownInputError(cmd))))
    ContinueState(ap)
  }
}