package scalavro.builder

import org.scalatest.Suite

import reflect.runtime.currentMirror
import tools.reflect.ToolBox

trait CodeTester {self: Suite =>
  private val toolbox = currentMirror.mkToolBox()
  def checkCodeValid(s: String): Unit = {
    val _ = toolbox.parse(s)
  }
}
