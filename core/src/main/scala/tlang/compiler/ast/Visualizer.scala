package tlang.compiler.ast

import org.graphstream.graph.implementations.SingleGraph
import org.graphstream.graph.{Edge, Node}
import tlang.compiler.ast.Trees._
import tlang.utils.Extensions._

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.TermSymbol

object Visualizer {

  val ClassColor       = "#EF767A"
  val MethodColor      = "#B0A1BA"
  val IdColor          = "#C5EBC3"
  val LiteralColor     = "#586994"
  val LastColor        = "#DDFFF7"
  val DefaultNodeColor = "#787878"
  val DefaultEdgeColor = "#A8A8A8"
  val DefaultTextColor = "#000000"
  val EdgeStart        = "#00FF00"
  val EdgeEnd          = "#0000FF"
  val startColor: Int  = Integer.parseInt(EdgeStart.replace("#", ""), 16)
  val endColor  : Int  = Integer.parseInt(EdgeEnd.replace("#", ""), 16)

  val styleSheet: String =
    s"""
       |node {
       |	size: 10px;
       |	fill-color: $DefaultNodeColor;
       |  stroke-width: 5;
       |  text-alignment: under;
       |  text-color: $DefaultTextColor;
       |  text-size: 16;
       |}
       |
       |edge {
       |  text-color: $DefaultTextColor;
       |  text-size: 12;
       |  text-alignment: along;
       |  text-background-mode: plain;
       |  text-background-color: white;
       |  fill-color: $DefaultEdgeColor;
       |}
       |
       |edge.Numbered {
       |  fill-mode: dyn-plain;
       |	fill-color: $EdgeStart, $EdgeEnd;
       |}
       |
       |node.Class {
       |	size: 50px;
       |	fill-color: $ClassColor;
       |}
       |
       |node.Method {
       |	size: 25px;
       |	fill-color: $MethodColor;
       |}
       |
       |node.Identifier {
       |	size: 15px;
       |	fill-color: $IdColor;
       |}
       |
       |node.Literal {
       |	size: 15px;
       |	fill-color: $LiteralColor;
       |}
       |
    """.stripMargin


  def apply(topTree: Tree): Unit = {
    val graph = new SingleGraph(topTree.getClass.getName)
    graph.addAttribute("ui.antialias")
    graph.addAttribute("ui.quality")
    graph.addAttribute("ui.stylesheet", styleSheet)

    graph.setStrict(false)
    graph.setAutoCreate(true)

    val traverser = new TreeToGraphTraverser(graph)
    traverser.traverse(topTree)
    graph.display()
    while (true) {

    }
  }

  class TreeToGraphTraverser(graph: SingleGraph) extends Trees.Traverser {

    private var id    = 0
    private val idMap = new java.util.IdentityHashMap[Tree, String]()

    override def _traverse(tree: Tree): Unit = {
      tree match {
        case CompilationUnit(_, classes, _) =>
          _traverse(classes)
        case ClassDecl(_, _, _, methods)    =>
          val nodeName = addNode(tree)
          methods.foreach { m =>
            val methodName = getId(m)
            graph.addEdge(nodeName + methodName, nodeName, methodName)
            Unit
          }
          _traverse(methods)
        case MethodDecl(_, _, _, _, stat)   =>
          val nodeName = addNode(tree)
          stat.ifDefined { s =>
            val statName = getId(s)
            graph.addEdge(nodeName + statName, nodeName, statName)
            _traverse(s)
          }
        case _: Modifier                    =>
        case t                              =>
          val nodeName = addNode(tree)
          val r = currentMirror.reflect(t)
          val fields = r.symbol.typeSignature.members.toStream.collect {
            case s if !s.isMethod => r.reflectField(s.asInstanceOf[TermSymbol])
          }

          fields.foreach { f =>
            val fieldName = f.symbol.name.toString
            f.get match {
              case l: List[_] => l.zipWithIndex.foreach {
                case (t: Tree, i) =>
                  val e = addEdge(nodeName, t, s"$fieldName${ i + 1 }")
                  val p = new java.lang.Double(i / l.size.asInstanceOf[Double])
                  e.addAttribute("ui.class", "Numbered")
                  e.setAttribute("ui.color", p)
              }
              case t: Tree    => addEdge(nodeName, t, fieldName)
              case _          =>
            }
          }
          super._traverse(t)
      }
    }

    private def getId(t: Tree): String = {
      if (idMap.containsKey(t))
        return idMap.get(t)

      id += 1
      val name = t.getClass.getSimpleName + id
      idMap.put(t, name)
      name
    }

    private def getLabel(t: Tree) = t match {
      case c: ClassDecl     => c.id.name
      case m: MethodDecl    => m.modifiers.map(_.getClass.getSimpleName).mkString(" ") + " " + m.signature
      case op: OperatorTree => op.opSign
      case Identifier(v)    => s"$v"
      case Literal(v)       => s"$v"
      case _                => t.getClass.getSimpleName
    }

    private def getUIClass(t: Tree) = t match {
      case _: ClassDecl  => "Class"
      case _: MethodDecl => "Method"
      case Identifier(_) => "Identifier"
      case Literal(_)    => "Literal"
      case _             => ""
    }
    private def addNode(t: Tree) = {
      val nodeName = getId(t)
      val node: Node = graph.addNode(nodeName)
      node.addAttribute("ui.label", getLabel(t))
      node.addAttribute("ui.class", getUIClass(t))
      nodeName
    }

    private def addEdge(parentName: String, child: Tree, name: String) = {
      val childName = getId(child)
      val e: Edge = graph.addEdge(parentName + childName, parentName, childName)
      e.addAttribute("ui.label", name)
      e
    }
  }

}
