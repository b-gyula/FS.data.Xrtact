import FSXtract.bVerbose
import xs4s.{XMLStream, XmlElementExtractor}

import java.io.InputStream
import scala.math.BigDecimal as Decimal
import scala.xml.{Elem, NodeSeq}
import xs4s.syntax.core.*

trait Extractor(val fileName: String = ""
					,val skipNames: Array[String] = Array()
	) extends WithLogger {

	var directPath = false
	var dataPath: os.Path = _
	def _dataPath(gamePath:String) = {
		directPath = gamePath.startsWith(":")
		dataPath = if (directPath) os.Path(gamePath.substring(1))
									 else os.Path(gamePath) / "data"
	}

	trait Named {
		def name: String
	}

	implicit val userOrd: Ordering[Named] = Ordering.by(_.name)

	def verboseLog(s: Any): Unit = if(bVerbose) pprint.pprintln(s)

	type T <: Named

	def xtractor(p: os.Path): XmlElementExtractor[T]

	lazy val className = getClass.getSimpleName

	def withErrorLog[A](p: os.Path) (f: Elem => A): Elem => A = { e =>
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
				log.info(s"Reading $className from " + path)
				if(skipNames.nonEmpty) {
					log.info("Skipping:" + skipNames.mkString(","))
				}
				os.read.stream(path).readBytesThrough { is =>
					extract(is, xtractor(path)).foreach {
						case f: T =>
							if (skipNames.exists(f.name.startsWith(_))) {
								verboseLog(f)
							} else {
								verboseLog(f)
								all = all :+ f
							}
						case _ =>
					}
				}
			}.getOrElse(throw new Exception(s"File '$fileName' not found in '$path'"))
		all.sorted
	}

	implicit def strToBool(s: String): Boolean = s.equalsIgnoreCase("true")

	implicit def nodeSeqToStr(ns: NodeSeq): String = ns.text

	implicit def nodeSeqToInt(ns: NodeSeq): Int = {
		val s = nodeSeqToStr(ns)
		if(s.isEmpty) 0 else s.toInt
	}

	implicit def nodeSeqToDec(ns: NodeSeq): Decimal = Decimal(nodeSeqToStr(ns))

	def round(b: Decimal, scale: Int = 0) = Fmt.round(b, scale)

	def extract[T](is: InputStream, x: XmlElementExtractor[T]): Iterator[T] = XMLStream
		.fromInputStream(is)
		.extractWith(x)
}