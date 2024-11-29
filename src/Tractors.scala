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
	/** Return each tractor with only the first engine variant*/
	var firstEngineOnly = false
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
		val name = "" // Just for Named
		override
		def toCsvRow: String = engines.foldLeft(new StringBuilder()) (
			(sb, e) => sb ++= toCsv.format(e.toCsv) + nl
		).result

		override
		def toCsv: String = str(brand, series, "%s", maxSpeed.toString, cat, extra.mkString("\"",", ","\""))
	}
	override
	val headers = "brand,series,name,hp,price,maxSpeed,cat,extras"

	override
	def collect(path: os.Path): Seq[Tractor] = {
		os.walk(path)
			.filter(p => if(!os.isDir(p) && p.ext == "xml") {
				val prefix: String = os.read(p, UTF_8, 0, 1300)
				var ttractor = prefix.indexOf(" type=\"tractor") + 14
				if(ttractor > 14 && ( prefix.startsWith("\"", ttractor)
										 || prefix.startsWith("ReverseDriving\"", ttractor))) {
					// TODO implement include with update for JohnDeere 9RX in D:\game\FS.25\data\vehicles\johnDeere\series9RX\series9RX.xml
					ttractor = prefix.indexOf("<function>$l10n_function_tractor</function>", ttractor)
					if(ttractor > 0) {
						prefix.indexOf("<category>tractors", ttractor + 43) > 0
					} else false
				} else false
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
					var series: String = e \ "name"
					if(series.startsWith( "$l10n_shopItem_seriesX") ) {
						series = e \ "name" \@ "params"
					} else if (series.endsWith(" Series")) {
						series = series.substring(0, series.length - 7)
					}
					base = Tractor( e \ "brand"
										,series
										,e \ "price"
										,e \ "specs" \ "power"
										,e \ "specs" \ "maxSpeed"
										,if (cat.length > 8) cat.substring(8) else ""
					)
					if ((e \ "functions" \ "function").exists(_.text == "$l10n_function_tractorFrontloader")) {
						base.extra += "front loader"
					}
					base

				}
			}
		case Vector("vehicle", "motorized", "motorConfigurations", "motorConfiguration") => withErrorLog(p) {
			(e: Elem) =>
				if(!firstEngineOnly || (firstEngineOnly && base.engines.isEmpty)) {
					base.engines += Engine(e \@ "name", e \ "@hp", base.price + e \ "@price")
				}
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