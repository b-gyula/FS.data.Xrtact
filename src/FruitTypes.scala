import xs4s.XmlElementExtractor.captureWithPartialFunctionOfElementNames
import xs4s.syntax.core._
import math.BigDecimal as Decimal
import scala.xml.Elem
/**
 price.csv
	 name:
	 price: per m3
	 showOnPriceTable:
 */
object FruitTypes extends Extractor {
	type T = FruitType
	override val _filteredNames = Array("meadow")
	case class FruitType(name: String, seedUsagePerSqm: Decimal, harvestLiterPerSqm: Decimal,
								windrowLitersPerSqm: Decimal, var chafFactor: Decimal = 0) extends Named {
		override 
		def toCsv = str(name, seedUsagePerSqm * 10, harvestLiterPerSqm * 10,	windrowLitersPerSqm * 10,
			round(FillTypes.price( name) * harvestLiterPerSqm * 10000) // fruit income per ha
			, chafFactor
		)
	}

	override
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