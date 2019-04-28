/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

// model for the test JSON files, generated with https://json2caseclass.cleverapps.io/

object Australia {

  case class Geometry(
      `type`: String,
      coordinates: List[Double]
  )
  case class Properties(
      name: String,
      streetaddress: String,
      twitteraccount: String,
      facebookaccount: String,
      siteurl: String,
      frequencyfinderurl: String
  )
  case class Features(
      `type`: String,
      id: String,
      geometry: Geometry,
      geometry_name: String,
      properties: Properties
  )
  case class Properties2(
      name: String
  )
  case class Crs(
      `type`: String,
      properties: Properties2
  )
  case class R00tJsonObject(
      `type`: String,
      totalFeatures: Double,
      features: List[Features],
      crs: Crs
  )
}

object Bicoin {
  case class Spending_outpoints(
      tx_index: Double,
      n: Double
  )
  case class Prev_out(
      spent: Boolean,
      spending_outpoints: List[Spending_outpoints],
      tx_index: Double,
      `type`: Double,
      addr: String,
      value: Double,
      n: Double,
      script: String
  )
  case class Inputs(
      sequence: Double,
      witness: String,
      prev_out: Prev_out,
      script: String
  )
  case class Out(
      spent: Boolean,
      tx_index: Int,
      `type`: Int,
      addr: String,
      value: Int,
      n: Int,
      script: String
  )
  case class Txs(
      ver: Double,
      inputs: List[Inputs],
      weight: Double,
      relayed_by: String,
      out: List[Out],
      lock_time: Double,
      size: Double,
      double_spend: Boolean,
      time: Double,
      tx_index: Double,
      vin_sz: Double,
      hash: String,
      vout_sz: Double
  )
  case class R00tJsonObject(
      txs: List[Txs]
  )
}
