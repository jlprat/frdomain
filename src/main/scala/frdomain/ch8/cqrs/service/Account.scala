package frdomain.ch8
package cqrs
package service

import scalaz._
import Scalaz._

import org.joda.time.DateTime
import cqrs.lib.Aggregate

object common {
  type Amount = BigDecimal
  type Error = String

  val today = DateTime.now()
}

import common._

case class Balance(amount: Amount = 0)

case class Account(no: String, name: String, dateOfOpening: DateTime = today, dateOfClosing: Option[DateTime] = None, 
  balance: Balance = Balance(0)) extends Aggregate {
  def id = no
  def isClosed = dateOfClosing.isDefined
}

object Account {
  implicit val showAccount: Show[Account] = Show.shows { case a: Account => a.toString }
}



