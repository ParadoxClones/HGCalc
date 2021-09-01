import java.awt.{Color, Insets}
import javax.swing.ImageIcon
import scala.collection.mutable.ListBuffer
import scala.swing.event.{ButtonClicked, EditDone}
import scala.swing.{Alignment, Button, Dimension, FlowPanel, Frame, GridBagPanel, Label, MainFrame, SimpleSwingApplication, TextField}

object Main extends SimpleSwingApplication {
  def top: Frame = new MainFrame {
    title = "HGCalc"
    resizable = false

    // creates a button with the specified icon
    def iconButton(icon: String): Button = {
      val ib = new Button
      ib.icon = new ImageIcon(getClass.getResource(icon))
      ib.margin = new Insets(0, 0, 0, 0)
      ib
    }

    // panel with the graph
    val graphPanel: FlowPanel = new FlowPanel {
      background = Color.white
      preferredSize = new Dimension(800, 600)
    }

    // adds the graph to the graphPanel
    val axes = new Axes(800, 600)
    val graphLabel = new Label("", new ImageIcon(axes.draw()), Alignment.Left)
    graphPanel.contents += graphLabel

    // side panel which controls the graph
    val sidePanel: GridBagPanel = new GridBagPanel {
      def constraints(y: Int, anchor: GridBagPanel.Anchor.Value = GridBagPanel.Anchor.East)
      : Constraints = {
        val c = new Constraints
        c.gridy = y
        c.anchor = anchor
        c
      }

      // textfield with default text 'name'
      def defaultPanel(name: String, tf: TextField): FlowPanel = {
        val dp = new FlowPanel
        dp.contents ++= Seq(new Label(name), tf)
        dp
      }

      val colours: ListBuffer[Color] = ListBuffer(Color.BLUE, Color.GREEN, Color.MAGENTA, Color.ORANGE,
        Color.PINK, Color.RED)
      val symbolImages = List("gt.png", "gte.png", "equ.png", "lte.png", "lt.png")
      val symbols = List('>', '≥', '=', '≤', '<')

      var graphName = "Graph"
      var numGraphs = 0
      var symbolIndex = 1

      val current = new HGCalc()
      val nameField = new TextField(graphName, 12)
      val popSizeField = new TextField(current.defaultPopSize.toString, 2)
      val numSuccessField = new TextField(current.defaultNumSuccess.toString, 2)
      val sampleSizeField = new TextField(current.defaultSampleSize.toString, 2)
      val targetSuccessField = new TextField(current.defaultTargetSuccess.toString, 2)
      val calculateButton = new Button("Calculate")
      val symbolButton: Button = iconButton(symbolImages(symbolIndex))
      val maxField = new TextField(current.defaultMax.toString, 2)
      val answer = new Label()

      val buttonPanel = new FlowPanel
      buttonPanel.contents ++= Seq(calculateButton, symbolButton, answer)
      val maxPanel: FlowPanel = defaultPanel("Maximum = ", maxField)
      maxPanel.visible = false



      listenTo(`nameField`)
      listenTo(`popSizeField`)
      listenTo(`numSuccessField`)
      listenTo(`sampleSizeField`)
      listenTo(`targetSuccessField`)
      listenTo(`calculateButton`)
      listenTo(`symbolButton`)
      listenTo(`maxField`)
      reactions += {
            // reacts when an edit is done to one of the text fields
        case EditDone(`nameField`) =>
          graphName = nameField.text
        case EditDone(`popSizeField`) =>
          // if the text is a digit, update the value of popSize to it
          if (popSizeField.text forall Character.isDigit) {
            current.defaultPopSize = popSizeField.text.toInt



            if (current.dependent == "popSize") {
              current.dependent = "none"
              calculateButton.text = "Calculate"
            }
          }
            // if the text is "x", set popSize as the dependent
          else if (popSizeField.text == "x") {
            if (current.dependent != "none" && current.dependent != "popSize") {
              popSizeField.text = current.defaultPopSize.toString
            } else {
              current.dependent = "popSize"
              calculateButton.text = "Graph"
            }
          }
            // otherwise setback the textfield
          else
            popSizeField.text = current.defaultPopSize.toString

        case EditDone(`numSuccessField`) =>
          // if the text is a digit, update the value of numSuccess to it
          if (numSuccessField.text forall Character.isDigit) {
            current.defaultNumSuccess = numSuccessField.text.toInt

            if (current.dependent == "numSuccess") {
              current.dependent = "none"
              calculateButton.text = "Calculate"
            }
          }
          // if the text is "x", set numSuccess as the dependent
          else if (numSuccessField.text == "x") {
            if (current.dependent != "none" && current.dependent != "numSuccess") {
              numSuccessField.text = current.defaultNumSuccess.toString
            } else {
              current.dependent = "numSuccess"
              calculateButton.text = "Graph"
            }
          }
          // otherwise setback the textfield
          else
            numSuccessField.text = current.defaultNumSuccess.toString

        case EditDone(`sampleSizeField`) =>
          // if the text is a digit, update the value of sampleSize to it
          if (sampleSizeField.text forall Character.isDigit) {
            current.defaultSampleSize = sampleSizeField.text.toInt

            if (current.dependent == "sampleSize") {
              current.dependent = "none"
              calculateButton.text = "Calculate"
            }
          }
          // if the text is "x", set sampleSize as the dependent
          else if (sampleSizeField.text == "x") {
            if (current.dependent != "none" && current.dependent != "sampleSize") {
              sampleSizeField.text = current.defaultSampleSize.toString
            } else {
              current.dependent = "sampleSize"
              calculateButton.text = "Graph"
            }
          }
          // otherwise setback the textfield
          else
            sampleSizeField.text = current.defaultSampleSize.toString

        case EditDone(`targetSuccessField`) =>
          // if the text is a digit, update the value of targetSuccess to it
          if (targetSuccessField.text forall Character.isDigit) {
            current.defaultTargetSuccess = targetSuccessField.text.toInt

            if (current.dependent == "targetSuccess") {
              current.dependent = "none"
              calculateButton.text = "Calculate"
            }
          }
          // if the text is "x", set targetSuccess as the dependent
          else if (targetSuccessField.text == "x") {
            if (current.dependent != "none" && current.dependent != "targetSuccess") {
              targetSuccessField.text = current.defaultTargetSuccess.toString
            } else {
              current.dependent = "targetSuccess"
              calculateButton.text = "Graph"
            }
          }
          // otherwise setback the textfield
          else
            targetSuccessField.text = current.defaultTargetSuccess.toString

          // react if the button is clicked
        case ButtonClicked(`calculateButton`) =>
          // if there is no dependent, calculate the answer to the problem
          if (current.dependent == "none") {
            answer.text = ((current.calculate().head.chance * 100000).round / 1000.0).toString + "%"
            // if there is a dependent, graph the answer
          } else {
            axes.graphs += Graph(current.copy(), colours(numGraphs % 6), graphName)
            graphLabel.icon = new ImageIcon(axes.draw())
            numGraphs += 1
            maxPanel.visible = true
          }
        case ButtonClicked(`symbolButton`) =>
          symbolIndex = (symbolIndex + 1) % 5
          symbolButton.icon = new ImageIcon(getClass.getResource(symbolImages(symbolIndex)))
          current.symbol = symbols(symbolIndex)
        case EditDone(`maxField`) =>
          if ((maxField.text forall Character.isDigit) && maxField.text.toInt <= 100) {
            current.defaultMax = maxField.text.toInt
            axes.maxX = current.defaultMax
            graphLabel.icon = new ImageIcon(axes.draw())
          }
          else
            maxField.text = current.defaultMax.toString
      }

      // creates the text panels and button
      add(defaultPanel("Name: ", nameField), constraints(y = 0))
      add(defaultPanel("Population Size =", popSizeField), constraints(y = 1))
      add(defaultPanel("Successes in Population =", numSuccessField), constraints(y = 2))
      add(defaultPanel("Sample Size =", sampleSizeField), constraints(y = 3))
      add(defaultPanel("Target Successes =", targetSuccessField), constraints(y = 4))
      add(buttonPanel, constraints(y = 5, anchor = GridBagPanel.Anchor.West))
      add(maxPanel, constraints(y = 6))
    }

    // puts the sidePanel and graphPanel together
    contents = new GridBagPanel {
      def constraints(x: Int, anchor: GridBagPanel.Anchor.Value = GridBagPanel.Anchor.North, inset: Int = 0)
      : Constraints = {
        val c = new Constraints
        c.gridx = x
        c.anchor = anchor
        c.insets = new Insets(inset, inset, inset, inset)
        c
      }

      add(sidePanel, constraints(0))
      add(graphPanel, constraints(1, inset = 3))
    }

    centerOnScreen()
  }
}
