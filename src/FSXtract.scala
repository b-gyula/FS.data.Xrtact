import mainargs.*
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import pprint.pprintln
import scala.math.BigDecimal as Decimal

object FSXtract {
	var cellSeparator = ";"

	def log(msg: Any): Unit = System.err.println(msg)

	@main(doc="Extracts fruit, fill type prices, productions and tractors  " +
		"into fruits.csv, prices.csv, productions.csv and tractors.csv " +
		"from the game install folder specified by the first (p) parameter, " +
		"Individual types can be defined by the second (x) parameter e.g -x t -> Extract tractors only\n" +
		"\nCannot use multiple values in -x with : prefixed path!")
	def main(@arg(short = 'p', doc = "path of the game install folder. Prefixing with : the path is used as is and not added `/ data / ...`")
				gamePath: String = "D:/Game/FS'25",
				@arg(short = 'x', doc = "type of xtract: f - fruit prices, t - tractors, p - productions. t! will generate tractors with only the first engine variant")
				xtract: String = "ftp",
				@arg(short = 'v', doc = "Log extra information during processing")
				verbose: Flag = Flag(false),
			   @arg(short='s', doc = "separator used in output CSVs" )
				separator: String = ";"
			  ): Unit = {
		cellSeparator = separator
		val printer = CSVPrinter(gamePath, verbose.value, separator)
		if(xtract.contains('t') ) {
			Tractors.firstEngineOnly = xtract.contains("t!")
			printer.out(Tractors, "tractors.csv")
		}
		if (xtract.contains('f') || xtract.contains('p')) {
			printer.out(FillTypes, "prices.csv")
			if (xtract.contains('f')) {
				printer.out(FruitTypes, "fruits.csv")
			}
			if (xtract.contains('p')) {
				printer.out(Productions, "productions.csv")
			}
		}
	}

	def main(args: Array[String]): Unit =
		ParserForMethods(this).runOrExit(args, true)
}

trait Printer {
	def out( xtractor: Extractor, fileName: String): Unit
}

class CSVPrinter(gamePath: String, logEachObject: Boolean, separator:String) extends Printer {
	override
	def out(xtractor: Extractor, fileName: String): Unit = {
		val lst = xtractor(gamePath)
		val wr = new PrintWriter(fileName, StandardCharsets.UTF_8)
		wr.println(xtractor.headers)
		lst.foreach { (e: xtractor.T) =>
			if(logEachObject) pprintln(e)
			wr.print(e.toCsvRow)
		}
		wr.close()
	}
}
