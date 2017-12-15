package com.matthewk.ui

import com.matthewk.domain.DomainValidation

trait Views {
  self: RenderType =>

  def showBasketView(
    applicationParams: ApplicationParams,
    menuOptions: List[String],
    success: Option[String],
    errors: Option[List[DomainValidation]]
  ): OutputType

  def showCatalogueView(
    applicationParams: ApplicationParams,
    menuOptions: List[String]
  ): OutputType

  def showOffersView(
    applicationParams: ApplicationParams,
    menuOptions: List[String]
  ): OutputType

  def showAddItemsView(
    ap: ApplicationParams,
    menuOptions: List[String],
    success: Option[String],
    errors: Option[List[DomainValidation]]
  ): OutputType

  def menuBar(items: List[String]): String
}
