import FSXtract.logEachObject
import xs4s.XmlElementExtractor.captureWithPartialFunctionOfElementNames
import xs4s.syntax.core.*
import math.BigDecimal as Decimal
import scala.collection.mutable.ArrayBuffer
import scala.xml.Elem
import scala.xml.Node
import pprint.pprintln
/**
 prod.csv
	 name:
	 prod chain: name
	 production cycle / day
	 running cost / day
	 input
	 amount / cycle
	 input cost / day
	 total monthlyCost
	 output
	 amount / cycle
	 income / day
 */
object Productions extends Extractor {
	import FillTypes.price
	import scala.Option.when
	val hourPerDay = 24

	abstract class Segment(cvs: CSV) {
		def toCsv: String

		def toCsv(data: Boolean): String = if (data) toCsv else cvs.ph
	}

	trait CSV {
		def ph: String
		def toCsv(seg: Option[Segment]): String = seg.fold(ph)(_.toCsv)
	}

	case class Amount( fillType: String, amount: Decimal, var store: Int = 0)
		extends Segment(Amount) {
		def this(e: Node) = this(
			(e \@ "fillType").toLowerCase,
			Decimal(e\@"amount")
		)
		var _value: Decimal = 0

		def value(cycles: Decimal): Decimal = {
			_value = round( cycles * amount * price(fillType))
			_value
		}
		override 
		def toCsv: String = str(fillType, amount, store+"", _value,"")
	}

	object Amount extends CSV {
		val ph = ",,,,"
	}

	case class Profit(var cost: Decimal = 0, var income: Decimal = 0)
		extends Segment(Profit) {
		def +=(p: Profit): Profit = {
			income = income + p.income
			cost = cost + p.cost
			this
		}
		def profit = income - cost
		def toCsv = str(round(profit), round(profit/cost, 2))
	}

	object Profit extends CSV {
		val ph = ","
	}
	case class IncomeLog(p: Profit) extends Segment(Profit) {
		def toCsv = p.income
	}
	object IncomeLog extends CSV {
		val ph = ""
	}

	case class Production( id: String,
								name: String,
								params: String,
								cyclesPerHour: Decimal,
								costsPerActiveHour: Decimal,
								inputs: Seq[Amount],
								outputs: Seq[Amount]
							) {
		lazy val cyclesPerDay = cyclesPerHour * hourPerDay
		lazy val costPerDay = costsPerActiveHour * hourPerDay
		lazy val profit = Profit(
			inputs.foldLeft(Decimal(0))((s,a) => s + a.value(cyclesPerDay)) + costPerDay,
			outputs.foldLeft(Decimal(0))((s,a) => s + a.value(cyclesPerDay))
		)
		def addStr(indent: Int, totalProfit: Option[Profit], sb: StringBuilder = new StringBuilder()): StringBuilder = {
			import Amount._
			for(i <- 0 until Math.max(inputs.size, outputs.size)) {
				sb++= (if(i == 0)
					str( cyclesPerDay, costPerDay, "")
				else
					nl + ("," * (indent + 2)))
				sb++= toCsv(when(i < inputs.size)( inputs(i)))
				sb++= toCsv(when(i < outputs.size)(outputs(i)))
				sb++= str(
					IncomeLog.toCsv(totalProfit.map(IncomeLog(_)).flatMap(when(i == 0)(_))),
					profit.toCsv(i == 0),
					Profit.toCsv(totalProfit.flatMap(when(i == 0)(_)) ))
			}
			sb
		}
	}

	val headers="name,prod,cyclesPerHour,costsPerHour,input,amount,output,amount,sell"
	case class ProductionPoint (
											file: os.Path,
											name: String,
											price: Decimal,
											productions: ArrayBuffer[Production] = ArrayBuffer.empty
										) extends Named {

		def toCsv(data: Boolean): String = if(data)
												str(name.split("_").last, price,"")
											else nl + ",,"

		override def toCsv: String = {
			val totalProfit = productions.foldLeft(Profit())((s,p) => s += p.profit)
			productions
				.foldLeft(new StringBuilder){ (sb, p) =>
					val firstRow = sb.isEmpty
					sb++= toCsv(firstRow)
					p.addStr(2, when(firstRow)(totalProfit), sb)
				}.result
		}

		def printShort(log: Boolean = logEachObject): Unit = if (log) {
			println(name + ":")
			productions.foreach( pprintln(_) )
		}
	}

	var last: ProductionPoint = null

	def updateCapacity(fillType: String, capacity: Int, s: Seq[Amount]*) =
		s.foreach(_.foreach(a => if(a.fillType == fillType) a.store = capacity))

	def xtractor(path: os.Path) = captureWithPartialFunctionOfElementNames {
		case Vector("placeable", "storeData") => withErrorLog(path) {
			(e: Elem) =>
				last = ProductionPoint(path, e\"name", e\"price")
				last
		}
		case Vector("placeable", "productionPoint", "productions", "production") => withErrorLog(path) {
			(e: Elem) =>
				last.productions += new Production(e\@"id",
					e\@"name",
					e\@"params",
					Decimal(e\@"cyclesPerHour"),
					Decimal(e\@"costsPerActiveHour"),
					(e\"inputs"\"_").map(new Amount(_)),
					(e\"outputs"\"_").map(new Amount(_)))
				null
		}
		case Vector("placeable", "productionPoint", "storage", "capacity") =>  withErrorLog(path) {
			(e: Elem) =>
				last.productions.foreach(p => updateCapacity((e \@ "fillType").toLowerCase,
					e \"@capacity",
					p.inputs, p.outputs))
				null
		}
	}

	type T = ProductionPoint
	val fileName=""

	override
	def collect(path: os.Path): Seq[ProductionPoint] = {
		all = os.walk(path, { p => p.last.startsWith("map") && os.isDir(p)})
			.filter(p => p.ext == "xml"
				&& os.read.lines
				.stream(p)
				.take(2).find( l => l.contains(" type=\"productionPoint") || l.contains(" type=\"greenhouse\"")).isDefined
			)
			.foldLeft(Seq.empty[ProductionPoint]) ((lst, p) => {
				println("Reading ProductionPoints from " + p)
				//var pp: ProductionPoint = null
				os.read.stream(p).readBytesThrough { is =>
					last = extract(is, xtractor(p)).toSeq.head
					/*						.filter(e => if (e.label == "storeData") { // Get the pp on the fly
												pp = ProductionPoint(p, (e\"name").text, Decimal((e\"price").text))
												false
											} else true
											)
											.foreach { n =>
												try pp.productions += new Production(n)
												catch {
													case t: Throwable => throw new Exception(s"Error parsing $n in " + p, t)
												}
											}
										*/
				}
				// Skip duplicates
				lst.find(e => e.name == last.name && e.productions == last.productions).fold {
					last.printShort()
					lst :+ last
				}{ _ =>
					print("Duplicate production point found:")
					last.printShort(true)
					lst
				}
			})
		all.sortWith(_.name.split("_").last < _.name.split("_").last)
	}
}