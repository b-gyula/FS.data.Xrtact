import mainargs.*
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import pprint.pprintln

object FSXtract {
	var logEachObject = false

	@main
	def main(@arg(short = 'p', doc = "path of the game data folder")
				gamePath: String = "D:/Game/FS.22",
				@arg(short = 'x', doc = "type of xtract: f - fruit prices, t - tractors, p - productions")
				xtract: String = "ftp",
				@arg(short = 'v', doc = "Log extra information during processing")
				verbose: Flag): Unit = {
		val dataPath = os.Path(gamePath) / "data"
		logEachObject = verbose.value
		if(xtract.contains('t')) {
			printOut(Tractors, dataPath / "vehicles", "tractors.csv")
		}
		if(xtract.contains('f')) {
			printOut(FillTypes, dataPath / "maps", "price.csv")
			printOut(FruitTypes, dataPath / "maps", "fruits.csv")
		}
		if(xtract.contains('p')) {
			printOut(Productions, dataPath / "placeables", "prods.csv")
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

	def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args, true)
}