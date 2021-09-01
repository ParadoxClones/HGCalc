import java.awt.{Color, Graphics2D}
import java.awt.geom.{Ellipse2D, Line2D, Rectangle2D}
import java.awt.image.BufferedImage
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

class Axes (
             val width: Int,
             val height: Int,
             var minX: Int = 0,
             var maxX: Int = 40,
             var graphs: ListBuffer[Graph] = ListBuffer()
           ) {
  val canvas = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  val xAxisLen = 500.0
  val yAxisLen = 470.0

  def draw(): BufferedImage = {
    val g: Graphics2D = canvas.createGraphics()

    g.setColor(Color.WHITE)
    g.fillRect(0, 0, width, height)

    g.setColor(Color.BLACK)
    g.draw(new Rectangle2D.Double(105.0, 55.0, 1.0, yAxisLen + 20.0))
    g.draw(new Rectangle2D.Double(85.0, 524.0, xAxisLen + 21.0, 1.0))

    g.drawString("0.00%", 48, 530)
    for (i <- List.range(1, 10)) {
      g.draw(new Rectangle2D.Double(85.0, 525.0 - i * yAxisLen / 10.0, 20.0, 1.0))
      g.draw(new Rectangle2D.Double(105.0, 525.0 - i * yAxisLen / 10.0, xAxisLen, 0.1))
      g.drawString(i + "0.00%", 42, 530 - i * yAxisLen.toInt / 10)
    }
    g.draw(new Rectangle2D.Double(85.0, 525.0 - 10 * yAxisLen / 10.0, 20.0, 1.0))
    g.draw(new Rectangle2D.Double(105.0, 525.0 - 10 * yAxisLen / 10.0, xAxisLen, 0.1))
    g.drawString("100.00%", 36, 60)

    g.drawString(minX.toString, 100, 560)
    val partition = if (maxX - minX < 50) 5 else 10
    var i = minX + partition
    while (i < maxX) {
      g.draw(new Rectangle2D.Double(105.0 + i * xAxisLen / (maxX - minX), 525.0, 1.0, 20.0))
      g.draw(new Rectangle2D.Double(106.0 + i * xAxisLen / (maxX - minX), 55.0, 0.1, yAxisLen))
      g.drawString(i.toString, 100 + i * xAxisLen.toInt / (maxX - minX), 557)
      i += partition
    }
    g.draw(new Rectangle2D.Double(105.0 + maxX * xAxisLen / (maxX - minX), 525.0, 1.0, 20.0))
    g.draw(new Rectangle2D.Double(106.0 + maxX * xAxisLen / (maxX - minX), 55.0, 0.1, yAxisLen))
    g.drawString(maxX.toString, 100 + maxX * xAxisLen.toInt / (maxX - minX), 557)

    for (i <- graphs.indices) {
      g.setColor(graphs(i).colour)
      val xMultiplier = xAxisLen / (maxX - minX)
      val yMultiplier = yAxisLen - 1
      val points = graphs(i).calc.calculate()
      var oldX = 105.0 + (points.head.x - minX) * xMultiplier
      var oldY = 524.0 - points.head.chance * yMultiplier

      for (j <- 0 to maxX.min(points.length - 1)) {
        val x = 105.0 + (points(j).x - minX) * xMultiplier
        val y = 524.0 - points(j).chance * yMultiplier

        g.draw(new Ellipse2D.Double(x - 1, y - 1, 2.0, 2.0))
        g.draw(new Line2D.Double(oldX, oldY, x, y))
        oldX = x
        oldY = y
      }

      g.fill(new Rectangle2D.Double(620.0, 55.0 + 12.0 * i, 10.0, 10.0))
      g.setColor(Color.BLACK)
      g.drawString(graphs(i).name, 635, 65 + 12 * i)
    }

    g.dispose()

    canvas
  }
}
