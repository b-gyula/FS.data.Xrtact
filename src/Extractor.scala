import xs4s.{XMLStream, XmlElementExtractor}
import FSXtract.cellSeparator
import java.io.InputStream
import scala.math.BigDecimal as Decimal
import scala.math.BigDecimal.RoundingMode.HALF_DOWN
import scala.xml.{Elem, NodeSeq}
import xs4s.syntax.core.*

trait Extractor(val fileName: String = ""
					,val skipNames: Array[String] = Array()
	) {
	var directPath = false
	var dataPath: os.Path = _
	def _dataPath(gamePath:String) = {
		directPath = gamePath.startsWith(":")
		dataPath = if (directPath) os.Path(gamePath.substring(1))
									 else os.Path(gamePath) / "data"
	}

	trait Named {
		def name: String
		def toCsv: String
		def toCsvRow: String = toCsv + nl
	}

	type T <: Named

	def xtractor(p: os.Path): XmlElementExtractor[T]
	def headers: String

	var actRow = 2

	def nl: String = {
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

	def apply(gamePath: String): Seq[T] = {
		_dataPath (gamePath)
		collect(if (directPath) dataPath else dataPath / "maps")
	}

	def collect(path: os.Path): Seq[T] = {
		all = Seq.empty[T]
		os.walk(path, { p => p.last.startsWith("map") && os.isDir(p)})
			.find(_.last == fileName)
			.map{ path =>
				FSXtract.log(s"Reading $className from " + path)
				if(skipNames.nonEmpty) {
					FSXtract.log("Skipping:" + skipNames.mkString(","))
				}
				os.read.stream(path).readBytesThrough { is =>
					extract(is, xtractor(path)).foreach {
						case f: T if !skipNames.exists(f.name.startsWith(_))  =>
							all = all :+ f
						case _ =>
					}
				}
			}.getOrElse(throw new Exception(s"File '$fileName' not found in '$path'"))
		all
	}

	implicit def strToBool(s: String): Boolean = s.equalsIgnoreCase("true")

	implicit def boolToStr(b: Boolean): String = if (b) "true" else ""

	implicit def floatToStr(f: Float): String = if (f == 0) "" else f.toString

	implicit def nodeSeqToStr(ns: NodeSeq): String = ns.text

	implicit def nodeSeqToInt(ns: NodeSeq): Int = {
		val s = nodeSeqToStr(ns)
		if(s.isEmpty) 0 else s.toInt
	}

	implicit def nodeSeqToDec(ns: NodeSeq): Decimal = Decimal(nodeSeqToStr(ns))

	implicit def decimalToStr(b: Decimal): String = if (b == 0) ""
																	else b.bigDecimal.stripTrailingZeros.toPlainString

	def round(b: Decimal, scale: Int = 0) = b.setScale(scale, HALF_DOWN)

	def cell(s: String*) = s.mkString(cellSeparator)
	def emptyCells(i: Int) = cellSeparator * i

	def extract[T](is: InputStream, x: XmlElementExtractor[T]): Iterator[T] = XMLStream
		.fromInputStream(is)
		.extractWith(x)
}