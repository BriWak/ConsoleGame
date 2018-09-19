package app

import java.util.concurrent.ArrayBlockingQueue

import jline.console.{ConsoleReader, KeyMap, Operation}
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Future, blocking}

object CaptureKeyPresses {

  def apply(captureWhile: Boolean): ArrayBlockingQueue[Either[Operation, String]] = {
    val reader = new ConsoleReader()
    val keyPresses = new ArrayBlockingQueue[Either[Operation, String]](128)

    val inputHandling = Future {
      val km = KeyMap.keyMaps().get("vi-insert")
      while (captureWhile) {
        blocking {
          val c = reader.readBinding(km)
          val k: Either[Operation, String] =
            if (c == Operation.SELF_INSERT) Right(reader.getLastBinding)
            else Left(c match { case op: Operation => op })
          keyPresses.add(k)
        }
      }
    }
    keyPresses
    }
  }

