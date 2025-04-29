/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

class PrettyJsonSpec extends ByteArrayJsonSpec:

  val prettyJson =
    """|{
       |  "type": "FeatureCollection",
       |  "totalFeatures": 53,
       |  "features": [
       |    {
       |      "type": "Feature",
       |      "id": "ckan_d534c0e9_a9bf_487b_ac8f_b7877a09d162.1",
       |      "geometry": {
       |        "type": "Point",
       |        "coordinates": [
       |          138.613807,
       |          -34.887844
       |        ]
       |      },
       |      "geometry_name": "geom",
       |      "properties": {
       |        "name": "ABC Radio Adelaide",
       |        "streetaddress": "85 North East Road, Collinswood, SA 5081",
       |        "twitteraccount": "abcadelaide",
       |        "facebookaccount": "https://www.facebook.com/abcadelaide",
       |        "siteurl": "http://www.abc.net.au/adelaide/",
       |        "frequencyfinderurl": "http://www.abc.net.au/adelaide/programs/frequencies.htm"
       |      }
       |    }
       |  ],
       |  "crs": {
       |    "type": "name",
       |    "properties": {
       |      "name": "urn:ogc:def:crs:EPSG::4326"
       |    }
       |  },
       |  "foo": {
       |    "bar": {
       |      "empty-map": {},
       |      "empty-arr": []
       |    }
       |  }
       |}""".stripMargin

  override def encode[T: Encoder](value: T): String =
    Json.encode(value).withPrettyRendering(indent = 2).toUtf8String

  test("Pretty Rendering") {
    val dom = decode[Dom.Element](prettyJson)
    verifyEncoding(dom, prettyJson)
  }
