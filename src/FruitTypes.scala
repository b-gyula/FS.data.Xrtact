import os.RelPath
import xs4s.XmlElementExtractor.captureWithPartialFunctionOfElementNames
import xs4s.syntax.core.*

import math.BigDecimal as Decimal
import scala.xml.Elem

/**
 price.csv
	 name:
	 price: per m3
	 showOnPriceTable:
 */
object FruitTypes extends Extractor("maps_fruitTypes.xml"
		,Array("meadow")) {
	type T = FruitType

	case class FruitType(name: String,
								seedUsagePerSqm: Decimal,
								harvestLiterPerSqm: Decimal,
								windrowLitersPerSqm: Decimal,
								var chafFactor: Decimal = 0) extends Named {
		override 
		def toCsv = cell(name, seedUsagePerSqm * 10, harvestLiterPerSqm * 10, windrowLitersPerSqm * 10,
			round(FillTypes.price( name) * harvestLiterPerSqm * 10000) // fruit income per ha
			, chafFactor
		)
	}

	override
	val headers = cell("name","seedUsage","harvest","windrow")
	
	case class Converter(from: String, to: String, factor: Decimal)

	override
	def xtractor(p: os.Path) = captureWithPartialFunctionOfElementNames {
		case Vector("map", "fruitTypes", "fruitType") => withErrorLog(p) {
			(e: Elem) => {
				val fileName = e \@ "filename"
				if (fileName.nonEmpty) {
					if (!directPath) {
						val path = dataPath / RelPath(fileName.replace("$data/", ""))
						os.read.stream(path).readBytesThrough { is =>
							extract(is, xtractor(path)).next
						}
					} else {
						null
					}
				}
				else { // F22
					FruitType((e \@ "name").toLowerCase,
						Decimal(e \ "cultivation" \@ "seedUsagePerSqm"),
						Decimal(e \ "harvest" \@ "literPerSqm"),
						if ((e \ "windrow" \@ "name") == "straw")
							Decimal(e \ "windrow" \@ "litersPerSqm")
						else 0
					)
				}
			}
		}
		case Vector( "foliageType", "fruitType") => withErrorLog(p) {
			(e: Elem) =>
				FruitType((e \@ "name").toLowerCase,
					Decimal(e \ "seeding" \@ "litersPerSqm"),
					Decimal(e \ "harvest" \@ "litersPerSqm"),
					if ((e \ "windrow" \@ "fillType") == "straw")
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
}