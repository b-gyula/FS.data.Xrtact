/**fruits.csv
	 neme:
	 seedUsage: required seed m3 / ha
	 harvest: harvested m3 / ha
	 windrow: straw m3 / ha
	 income: per ha
*/
import xs4s.XmlElementExtractor.captureWithPartialFunctionOfElementNames
import xs4s.syntax.core._
import math.BigDecimal as Decimal
import scala.xml.Elem

object FillTypes extends Extractor {
	type T = FillType
	override
	val headers = "name,price,showOnPriceTable"

	case class FillType(name: String, price: Decimal, showOnPriceTable: Boolean) extends Named {
		override def toCsv = str(name, price * 1000, showOnPriceTable)
	}
	
	override 
	val _filteredNames = Array("horse_","cow_","sheep_","pig_","chicken","weed","snow",
										"roadsalt","air","tarp","squarebale","roundbale","meadow")//,"water"

	override
	def xtractor(p: os.Path) = captureWithPartialFunctionOfElementNames {
		case Vector("map", "fillTypes", "fillType") => withErrorLog(p) {
			(e: Elem) =>
				val pricePerLiter = e\"economy"\@"pricePerLiter"
				FillType((e\@"name").toLowerCase,
					Decimal(if(pricePerLiter.isBlank) "0" else pricePerLiter),
					e\@"showOnPriceTable")
		}
	}
	override
	val fileName = "maps_fillTypes.xml"

	def price(name: String): Decimal = {
		Option(all).flatMap( _.find(_.name == name).map(_.price) ).getOrElse {
			throw new Exception(s"FillType not found: $name")
		}
	}
}