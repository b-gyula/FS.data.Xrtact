import xs4s.XmlElementExtractor.captureWithPartialFunctionOfElementNames
import xs4s.syntax.core.*

import math.BigDecimal as Decimal
import scala.collection.mutable.ArrayBuffer
import scala.xml.{Elem, Node, XML}

/**
 prod.csv
	 name:
	 production cycle / day
	 running cost / day
	 input
	 amount / cycle
    storage
	 input cost / day
	 total monthlyCost
	 output
	 amount / cycle
	 storage
	 income / day
    total income (of all prod chains) /day
 	 profit / day
 	 profit ratio / day
 	 total profit / day
 	 total profit ratio / day
 */
object Productions extends Extractor {
	import FillTypes.price
	val hourPerDay = 24

	case class Amount(name: String, amount: Decimal, var store: Int = 0) extends Named {
		def this(e: Node) = this(
			(e \@ "fillType").toLowerCase,
			Decimal(e\@"amount")
		)

		/** worth of `cycles` runs; pure function instead of the old `_value` cache,
			so callers state which cycle count they mean (always `cyclesPerDay` today) */
		def value(cycles: Decimal): Decimal = round( cycles * amount * price(name))
	}

	case class Profit(var cost: Decimal = 0, var income: Decimal = 0) {
		def +=(p: Profit): Profit = {
			income = income + p.income
			cost = cost + p.cost
			this
		}
		def profit = income - cost
		def ratio = round(profit/cost, 2)
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
	}

	case class Factory(file: os.Path
							 , _name: String
							 , variant: String
							 , price: Decimal
							 , productions: ArrayBuffer[Production] = ArrayBuffer.empty
							) extends Named {
		var name = _name.split("_").last
		lazy val totalProfit = productions.foldLeft(Profit())((s,p) => s += p.profit)

//		def printShort(): Unit =  {
//			log(name + ":")
//			productions.foreach( pprint.log(_) )
//		}
	}

	var last: Factory = null

	def updateCapacity(fillType: String, capacity: Int, s: Seq[Amount]*) =
		s.foreach(_.foreach(a => if(a.name == fillType) a.store = capacity))

	def xtractor(path: os.Path) = captureWithPartialFunctionOfElementNames {
		case Vector("placeable", "storeData") => withErrorLog(path) {
			(e: Elem) =>
				val prms: String = e\"name" \@ "params"
				var name: String = e\"name"
				val variant = prms.split("[|]").last
				if(name.contains("%s")) {
					// Ugly hack for <name params="$l10n_shopItem_woodSellingStation|Wood-Mizer LT15">%s (%s)</name>
					// in data\placeables\brandless\productionPointsGeneric\sawmill\sawmill.xml
					//prms.split("[|]").lastOption.foreach(name += "_" + _)
					name = "sawmill"
				}
//				XML.loadString(s"<${name}/>")
				last = Factory(path, name, variant, e\"price")
				if (path.segments.exists(_.endsWith("Small"))) {
					last.name += "_small"
				}
				last
		}
		case Vector("placeable", "productionPoint", "productions", "production") => withErrorLog(path) {
			(e: Elem) =>
				last.productions += new Production(e\@"id",
					e\@"name",
					e\@"params",
					Decimal(e\@"cyclesPerHour"),
					Decimal(e\@"costsPerActiveHour"),
					(e\"inputs"\"_").map(new Amount(_)).sorted,
					(e\"outputs"\"_").map(new Amount(_)).sorted)
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

	type T = Factory

	override
	def apply(gamePath: String): Seq[T] = {
		_dataPath(gamePath)
		collect(if (directPath) dataPath else dataPath / "placeables")
	}

	override
	def collect(path: os.Path): Seq[Factory] = {
		all = os.walk(path, { p => p.last.startsWith("map") && os.isDir(p)})
			.filter(p => p.ext == "xml"
				&& os.read.lines
				.stream(p)
				.take(2).find( l => l.contains(" type=\"productionPoint") || l.contains(" type=\"greenhouse\"")).isDefined
			)
			.foldLeft(Seq.empty[Factory]) ((lst, p) => {
				log.info("Reading Productions from " + p)
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
				lst.find(e => e.name == last.name).fold {
					lst :+ last
				}{ p => // TODO validate
					if( p.productions == last.productions) {
						log.warning("Duplicate production point found:\n"+pprint(p))
						lst
					} else {
						last.name += ".2"
						lst :+ last
					}
				}
			})
		all.sorted
	}
}