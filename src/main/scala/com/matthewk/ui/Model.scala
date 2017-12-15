package com.matthewk.ui

import com.matthewk.domain.{Basket, Catalogue, Checkout}

sealed trait ApplicationState

case object QuitState extends ApplicationState
case class ContinueState(applicationParams: ApplicationParams) extends ApplicationState

case class ApplicationParams(
  checkout: Checkout,
  basket: Basket = Basket.Empty,
  catalogue: Catalogue = Catalogue.Empty,
  menuItems: List[String] = List.empty[String]
)