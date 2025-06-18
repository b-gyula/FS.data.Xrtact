import mainargs.*
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import pprint.pprintln

object FSXtract {
	var logEachObject = false
	var cellSeparator = ";"
	@main(doc="Extracts fruit + fill type prices into fruits.csv and price.csv, productions into prods.csv and " +
		"tractors into tractors.csv from the game install folder specified by the first (p) parameter.\n" +
		"Individual types can be defined by the second (x) parameter e.g -x t -> Extract tractors only\n" +
		"\nDo not use multiple values in -x with : prefixed path!")
	def main(@arg(short = 'p', doc = "path of the game install folder. Prefixing with : the path is used as is and not added `/ data / ...`")
				gamePath: String = "D:/Game/FS.25",
				@arg(short = 'x', doc = "type of xtract: f - fruit prices, t - tractors, p - productions. t! will generate tractors with only the first engine variant")
				xtract: String = "ftp",
				@arg(short = 'v', doc = "Log extra information during processing")
				verbose: Flag = Flag(false),
			   @arg(short='s', doc = "separator")
				separator: String = ";"): Unit = {
		val directPath = gamePath.startsWith(":")
		val dataPath = if(directPath) os.Path(gamePath.substring(1))
										 else os.Path(gamePath) / "data"
		logEachObject = verbose.value
		cellSeparator = separator
		if(xtract.contains('t')) {
			Tractors.firstEngineOnly = xtract.contains("t!")
			printOut(Tractors, if (directPath) dataPath else dataPath / "vehicles", "tractors.csv")
		}
		if (xtract.contains('f') || xtract.contains('p')) {
			printOut(FillTypes, if (directPath) dataPath else dataPath / "maps", "prices.csv")
			if (xtract.contains('f')) {
				printOut(FruitTypes, if (directPath) dataPath else dataPath / "maps", "fruits.csv")
			}
			if (xtract.contains('p')) {
				printOut(Productions, if (xtract == "p/") os.Path(gamePath) else dataPath / "placeables", "productions.csv")
			}
		}
	}

	def printOut(xtractor: Extractor, p: os.Path, fileName: String): Unit = {
		val lst = xtractor.collect(p)
		val wr = new PrintWriter(fileName, StandardCharsets.UTF_8)
		wr.println(xtractor.headers)
		lst.foreach { (e: xtractor.T) =>
			if(logEachObject) pprintln(e)
			wr.print(e.toCsvRow)
		}
		wr.close()
	}

	def main(args: Array[String]): Unit =
		ParserForMethods(this).runOrExit(args, true)
}