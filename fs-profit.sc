#!/usr/bin/env amm
/*
min SCALA_VERSION=2.13
	generates 3 files:
price.csv
	name:
	price: per m3
	showOnPriceTable:

fruits.csv
	neme:
	seedUsage: required seed m3 / ha
	harvest: harvested m3 / ha
	windrow: straw m3 / ha
	income: per ha

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
//> using scala 3
//> using dep com.lihaoyi::os-lib:0.9.3 // Required 4 scala-cli to work
//> using dep com.scalawilliam::xs4s-core:0.9.1
///> using dep com.lihaoyi::pprint:0.6.6
//> using dep com.lihaoyi::mainargs:0.6.1

import mainargs._
import math.BigDecimal.RoundingMode.HALF_DOWN
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import math.{BigDecimal => Decimal}
import collection.mutable.ArrayBuffer
import scala.Option.when
import $ivy.`com.scalawilliam::xs4s-core:0.9.1`
import pprint.pprintln

var logEachObject = false

@main
def run(@arg(short = 'g', doc = "path of the game data folder")
		  gamePath: String = "D:/Game/FS.22/data",
		  @arg(short = 'v', doc = "Log extra information during processing")
		  verbose: Flag) = {
	logEachObject = verbose.value
	printOut(FillTypes, os.Path(gamePath) / "maps", "price.csv")
	printOut(FruitTypes, os.Path(gamePath) / "maps", "fruits.csv")
	printOut(Productions, os.Path(gamePath) / "placeables", "prods.csv")
}

def printOut(xtractor: Extractor, p: os.Path, fileName: String): Unit = {
	val lst = xtractor.collect(p)
	val wr = new PrintWriter(fileName, StandardCharsets.UTF_8)
	//wr.println(xtractor.headers)
	lst.zipWithIndex.foreach{ case (e: xtractor.T, i: Int) =>
		wr.print(e.toCsvRow)
	}
	wr.close()
}

import scala.xml.Node
import xs4s.{XMLStream, XmlElementExtractor}
import xs4s.XmlElementExtractor._
import xs4s.syntax.core._

import java.io.InputStream
import scala.xml.Elem

implicit def strToBool(s: String): Boolean = s.equalsIgnoreCase("true")
implicit def boolToStr(b: Boolean): String  = if (b) "true" else ""
implicit def floatToStr(f: Float): String  = if (f ==0) "" else f.toString
implicit def decimalToStr(b: Decimal): String  = if (b == 0) ""
														else b.bigDecimal.stripTrailingZeros.toPlainString
def round(b: Decimal, scale: Int = 0) = b.setScale(scale, HALF_DOWN)
def str(s: String *) = s.mkString(",")
def extract[T](is: InputStream, x: XmlElementExtractor[T]) = XMLStream
	.fromInputStream(is)
	.extractWith(x)

import FillTypes.price
object Productions extends Extractor {
	val hourPerDay = 24

	abstract class Segment(cvs: CSV) {
		def toCsv: String

		def toCsv(data: Boolean): String = if (data) toCsv else cvs.ph
	}

	trait CSV {
		def ph: String
		def toCsv(seg: Option[Segment]): String = seg.fold(ph)(_.toCsv)
	}

	case class Amount( fillType: String, amount: Decimal)
		extends Segment(Amount) {
		def this(e: Node) = this(
			(e \@ "fillType").toLowerCase,
			Decimal(e\@"amount")
		)
		var _value: Decimal = 0

		def value(cycles: Decimal) = {
			_value = round( cycles * amount * price(fillType))
			_value
		}
		def toCsv = str(fillType, amount, _value, "")
	}

	object Amount extends CSV {
		val ph = ",,,"
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

	case class Production(
		id: String,
		name: String,
		params: String,
		cyclesPerHour: Decimal,
		costsPerActiveHour: Decimal,
		inputs: Seq[Amount],
		outputs: Seq[Amount]		
	) {
		def this(e: Node) = this(
			e\@"id",
			e\@"name",
			e\@"params",
			Decimal(e\@"cyclesPerHour"),
			Decimal(e\@"costsPerActiveHour"),
			(e\"inputs"\"_").map(n => new Amount(n)),
			(e\"outputs"\"_").map(n => new Amount(n))
		)
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
		//override def toCsvRow(i: Int) = productions.foldLeft(new StringBuilder){ (sb,p) =>
		def toCsv(data: Boolean) = if(data) 
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
		def printShort(log: Boolean = logEachObject) = if (log) {
			println(name + ":")
			productions.foreach( pprintln(_) )
		}
	}

	// Unused dummy 4 Extractor trait
	def xtractor(p: os.Path) = captureWithPartialFunctionOfElementNames {
		case Vector("production") =>
			(e: Elem) => ProductionPoint(null,"", 0)
	}

	val xextractor = captureWithPartialFunctionOfElementNames {
		case Vector("placeable", "storeData") =>
			(e: Elem) => e
		case Vector("placeable", "productionPoint", "productions", "production") =>
			(e: Elem) => e
	}

	type T = ProductionPoint
	val fileName=""

	override
	def collect(path: os.Path): Seq[ProductionPoint] = {
		all = os.walk(path, { p => p.last.startsWith("map") && os.isDir(p)})
			.filter(p => p.ext == "xml"
					&& os.read.lines
						.stream(p)
						.take(2).find(_.contains(" type=\"productionPoint")).isDefined
			)
			.foldLeft(Seq.empty[ProductionPoint]) ((lst, p) => {
				println("Reading ProductionPoints from " + p)
				var pp: ProductionPoint = null
				os.read.stream(p).readBytesThrough { is =>
					extract(is, xextractor)
						.filter(e => if (e.label == "storeData") { // Get the pp on the fly
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
				}
				// Skip duplicates
				lst.find(e => e.name == pp.name && e.productions == pp.productions).fold{
					pp.printShort()
					lst :+ pp
				}{ _ =>
					print("Duplicate production point found:")
					pp.printShort(true)
					lst
				}
			})
		all.sortWith(_.name.split("_").last < _.name.split("_").last)
	}
}

trait Extractor {
	trait Named {
		def name: String
		def toCsv: String
		def toCsvRow: String = toCsv + nl
	}
	type T <: Named

	def xtractor(p: os.Path): XmlElementExtractor[T]
	def headers: String
	def fileName: String
	def _filteredNames: Seq[String] = Seq.empty

	var actRow = 2
	def nl = {
		actRow += 1
		"\n"
	}
	lazy val className = getClass.getSimpleName
	def withErrorLog[A](p: os.Path) (f: Elem => A): Elem => A = { e =>
/*			if(logEachObject) {
				print("Extractor found:")
				pprintln(e)
			}*/
			try f(e)
			catch {
				case t: Throwable => throw new Exception(s"Error parsing $className from: $e in $p" , t)
			}
		}
	var all: Seq[T] = _

	def collect(path: os.Path): Seq[T] = {
		all = Seq.empty[T]
		os.walk(path, { p => p.last.startsWith("map") && os.isDir(p)})
			.find(_.last == fileName)
			.map{ path =>
				println (s"Reading $className from " + path)
				if(_filteredNames.nonEmpty)
					println ("Skipping:" + _filteredNames.mkString(","))
				os.read.stream(path).readBytesThrough { is =>
					extract(is, xtractor(path)).foreach {
						case f: T if !_filteredNames.exists(f.name.startsWith(_))  =>
							if(logEachObject) pprintln(f)
							all = all :+ f
						case _ =>
					}
				}
			}.getOrElse(throw new Exception(s"File $fileName not found in $path"))
		all
	}
}

object FillTypes extends Extractor {
	type T = FillType
	val headers = "name,price,showOnPriceTable"

	case class FillType(name: String, price: Decimal, showOnPriceTable: Boolean) extends Named {
		override def toCsv = str(name, price * 1000, showOnPriceTable)
	}
	override val _filteredNames = Seq("horse_","cow_","sheep_","pig_","chicken","weed","snow",
		"roadsalt","air","tarp","squarebale","roundbale","meadow","water")

	def xtractor(p: os.Path) = captureWithPartialFunctionOfElementNames {
		case Vector("map", "fillTypes", "fillType") => withErrorLog(p) {
			(e: Elem) =>
				FillType((e\@"name").toLowerCase,
					Decimal(e\"economy"\@"pricePerLiter"),
						  e\@"showOnPriceTable")
		}
	}
	val fileName = "maps_fillTypes.xml"

	def price(name: String): Decimal = {
		Option(all).flatMap( _.find(_.name == name).map(_.price) ).getOrElse {
			throw new Exception(s"FillType not found: $name")
		}
	}
}

object FruitTypes extends Extractor {
	type T = FruitType
	override val _filteredNames = Seq("meadow")
	case class FruitType(name: String, seedUsagePerSqm: Decimal, harvestLiterPerSqm: Decimal,
								windrowLitersPerSqm: Decimal, var chafFactor: Decimal = 0) extends Named {
		override def toCsv = str(name, seedUsagePerSqm * 10, harvestLiterPerSqm * 10,	windrowLitersPerSqm * 10,
			round(FillTypes.price( name) * harvestLiterPerSqm * 10000) // fruit income per ha
			, chafFactor
		)
	}

	val headers = "name,seedUsage,harvest,windrow"
	case class Converter(from: String, to: String, factor: Decimal)

	override
	def xtractor(p: os.Path) = captureWithPartialFunctionOfElementNames {
		case Vector("map", "fruitTypes", "fruitType") => withErrorLog(p) {
			(e: Elem) => //println ((e \ "cultivation" \@ "seedUsagePerSqm"), (e \ "harvest" \@ "literPerSqm"),(e \ "windrow" \@ "literPerSqm"))
				FruitType((e \@ "name").toLowerCase,
					Decimal(e \ "cultivation" \@ "seedUsagePerSqm"),
					Decimal(e \ "harvest" \@ "literPerSqm"),
					if ((e \ "windrow" \@ "name") == "straw")
						Decimal(e \ "windrow" \@ "litersPerSqm")
					else 0
				)
		}
		case Vector("map", "fruitTypeConverters", "fruitTypeConverter", "converter") => withErrorLog(p) {
			(e: Elem) =>
				val c = Converter((e \@ "from").toLowerCase, e \@ "to", Decimal(e \@ "factor"))
				if(c.to == "CHAFF") {
					all.find(_.name == c.from).foreach ( _.chafFactor = c.factor )
				}
				null
		}
	}
	val fileName = "maps_fruitTypes.xml"
}