import xs4s.XmlElementExtractor

import java.nio.charset.StandardCharsets.UTF_8
import xs4s.XmlElementExtractor.captureWithPartialFunctionOfElementNames

import scala.math.BigDecimal as Decimal
import scala.xml.Elem
import xs4s.syntax.core.*

import scala.collection.mutable.Buffer

object Tractors extends Extractor {
	type T = Tractor
	val fileName=""

	case class Engine(name: String, hp:Int, price: Int) {
		def toCsv: String = str(name, hp.toString, price.toString)
	}

	case class Tractor(brand: String,
							 series: String,
							 price: Int,
							 hp: Int,
							 maxSpeed: Int,
							 cat: String,
							 extra: Buffer[String] = Buffer(),
							 engines: Buffer[Engine] = Buffer()
							)
		extends Named {
		val name = "" // Just for named
		override
		def toCsvRow: String = engines.foldLeft(new StringBuilder()) (
			(sb, e) => sb ++= toCsv.format(e.toCsv) + nl
		).result

		override
		def toCsv: String = str(brand, series, "%s", maxSpeed.toString, cat, extra.mkString("\"",", ","\""))
	}
	override
	val headers = "brand,series,name,hp,price,maxSpeed,cat,extra"
	override
	def collect(path: os.Path): Seq[Tractor] = {
		os.walk(path)
			.filter(p => if(!os.isDir(p) && p.ext == "xml") {
				val prefix = os.read(p, UTF_8, 0, 900)
				prefix.contains(" type=\"tractor")
					&& prefix.contains("<function>$l10n_function_tractor</function>")
			} else false )
			.foldLeft(Seq.empty[Tractor]) ((lst, f) => {
				println("Reading Tractor from " + f)
				lst ++ os.read.stream(f).readBytesThrough { is =>
					extract(is, xtractor(f)).filter(_!=null).toSeq
				}
			})
	}

	var base: Tractor = _
	val track = "track"
	override
	def xtractor(p: os.Path) = captureWithPartialFunctionOfElementNames {
		case Vector("vehicle", "storeData") => withErrorLog(p) {
				(e: Elem) => {
					val cat = (e \ "category").head.text
					val series: String = e \ "name"
					if (cat.startsWith("tractor")) {
						base = //Some(
							Tractor(e \ "brand"
								, if (series.endsWith(" Series")) series.substring(0, series.length - 7) else series
								, e \ "price"
								, e \ "specs" \ "power"
								, e \ "specs" \ "maxSpeed"
								, if (cat.length > 8) cat.substring(8) else ""
							)
						//)
						if ((e \ "functions" \ "function").exists(_.text == "$l10n_function_tractorFrontloader")) {
							base.extra += "front loader"
						}
						base
					} else null
				}
			}
		case Vector("vehicle", "motorized", "motorConfigurations", "motorConfiguration") => withErrorLog(p) {
			(e: Elem) =>
					base.engines += Engine(e \@ "name", e \ "@hp", base.price + e \ "@price")
					null
		}
		case Vector("vehicle", "cylindered", "movingParts") => withErrorLog(p) {
			(e: Elem) =>
				if ( (e\"movingPart").exists( _ \@ "node" == "steeringHydraulicLeft") ) {
					base.extra += "articulating"
				}
				null
		}
		case Vector("vehicle", "wheels", "wheelConfigurations", "wheelConfiguration", "crawlers") => withErrorLog(p) {
			(e: Elem) =>
				if ( !base.extra.contains( track ) ) {
					base.extra += track
				}
				null
		}
	}
}