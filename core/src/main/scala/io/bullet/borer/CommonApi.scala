/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.lang.{StringBuilder => JStringBuilder}

trait CommonApi[Config]:

  /**
   * Configures the [[Config]] for this encoding/decoding run.
   */
  def withConfig(config: Config): this.type

  /**
   * Enables logging of the encoding/decoding progress to the console.
   * Each data item that is written/received by the application is pretty printed to the console on its own line.
   */
  def withPrintLogging(
      maxShownByteArrayPrefixLen: Int = 20,
      maxShownStringPrefixLen: Int = 50,
      maxShownArrayElems: Int = 20,
      maxShownMapEntries: Int = 20,
      initialGutterWidth: Int = 5,
      renderLevelCount: Boolean = true,
      renderEndOfInput: Boolean = true,
      renderCommas: Boolean = false,
      indentation: String = "    ",
      mapKeySep: String = "-> "): this.type

  /**
   * Enables logging of the encoding/decoding progress to the given [[JStringBuilder]].
   */
  def withStringLogging(
      stringBuilder: JStringBuilder,
      maxShownByteArrayPrefixLen: Int = 20,
      maxShownStringPrefixLen: Int = 50,
      maxShownArrayElems: Int = 20,
      maxShownMapEntries: Int = 20,
      initialGutterWidth: Int = 5,
      renderLevelCount: Boolean = true,
      renderEndOfInput: Boolean = true,
      renderCommas: Boolean = false,
      indentation: String = "    ",
      mapKeySep: String = "-> ",
      lineSep: String = System.lineSeparator(),
      mapValueOnNewLine: Boolean = false): this.type

  /**
   * Enables logging of the encoding/decoding progress to the given [[Logging.Logger]].
   */
  def withLogging(createLogger: Logging.LevelInfo => Logging.Logger): this.type

  /**
   * Allows for injecting additional custom logic into the encoding/decoding process.
   * Used, for example, for on-the-side [[Logging]].
   */
  def withTransformerAdded(transformer: Receiver.Transformer[Config]): this.type

private[borer] object CommonApi:

  abstract class Impl[Config](defaultConfig: Config, defaultTransformer: Receiver.Transformer[Config]):
    protected var config: Config                                    = defaultConfig
    protected var receiverTransformer: Receiver.Transformer[Config] = defaultTransformer

    final def withConfig(config: Config): this.type =
      this.config = config
      this

    final def withPrintLogging(
        maxShownByteArrayPrefixLen: Int,
        maxShownStringPrefixLen: Int,
        maxShownArrayElems: Int,
        maxShownMapEntries: Int,
        initialGutterWidth: Int,
        renderLevelCount: Boolean,
        renderEndOfInput: Boolean,
        renderCommas: Boolean,
        indentation: String,
        mapKeySep: String): this.type =
      withLogging { levelInfo =>
        new Logging.PrintLogger(
          info = levelInfo,
          maxShownByteArrayPrefixLen = maxShownByteArrayPrefixLen,
          maxShownStringPrefixLen = maxShownStringPrefixLen,
          maxShownArrayElems = maxShownArrayElems,
          maxShownMapEntries = maxShownMapEntries,
          initialGutterWidth = initialGutterWidth,
          renderLevelCount = renderLevelCount,
          renderEndOfInput = renderEndOfInput,
          renderCommas = renderCommas,
          indentation = indentation,
          mapKeySep = mapKeySep)
      }

    final def withStringLogging(
        sb: JStringBuilder,
        maxShownByteArrayPrefixLen: Int,
        maxShownStringPrefixLen: Int,
        maxShownArrayElems: Int,
        maxShownMapEntries: Int,
        initialGutterWidth: Int,
        renderLevelCount: Boolean,
        renderEndOfInput: Boolean,
        renderCommas: Boolean,
        indentation: String,
        mapKeySep: String,
        lineSep: String,
        mapValueOnNewLine: Boolean): this.type =
      withLogging { levelInfo =>
        new Logging.ToStringLogger(
          info = levelInfo,
          stringBuilder = sb,
          maxShownByteArrayPrefixLen = maxShownByteArrayPrefixLen,
          maxShownStringPrefixLen = maxShownStringPrefixLen,
          maxShownArrayElems = maxShownArrayElems,
          maxShownMapEntries = maxShownMapEntries,
          initialGutterWidth = initialGutterWidth,
          renderLevelCount = renderLevelCount,
          renderEndOfInput = renderEndOfInput,
          renderCommas = renderCommas,
          indentation = indentation,
          mapKeySep = mapKeySep,
          lineSep = lineSep,
          mapValueOnNewLine = mapValueOnNewLine)
      }

    final def withLogging(createLogger: Logging.LevelInfo => Logging.Logger): this.type =
      withTransformerAdded(Logging.transformer(createLogger))

    final def withTransformerAdded(transformer: Receiver.Transformer[Config]): this.type =
      val prevTransformer = receiverTransformer
      receiverTransformer =
        if (prevTransformer eq Receiver.nopTransformer[Config]) transformer
        else (r: Receiver, conf: Config) => transformer(prevTransformer(r, conf), conf)
      this
