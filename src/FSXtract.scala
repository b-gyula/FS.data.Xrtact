import mainargs.*

import java.io.{FileInputStream, FileOutputStream, Reader}
import javax.xml.transform.stream.StreamSource

object FSXtract {
	var bVerbose = false
	@main(doc="Extracts fruit yields, prices, factories into JSON, XML (-x), CSVs (-c) or any other format " +
		"from the game install folder specified by the first (p) parameter. " +
		"Individual types can be defined by the (-e) parameter e.g -e p -> Extract factories only\n" +
		"\nCannot use multiple values in -x with : prefixed path!")
	def main(@arg(short = 'p', doc = "path of the game install folder. Prefixing with : the path is used as is and not added `/ data / ...`", positional = true)
				path: String = "D:/Game/FS'25",
				@arg(short = 'o', doc = "output path. 'fsinfo.json' by default", positional = true)
				out: Option[String] = None,
				@arg(short = 'e', doc = "type of xtract: c - crops, p - prices, t - tractors, f - factories. t! will generate tractors with only the first engine variant")
				extract: String = "ftpc",
				@arg(short='x',
					doc = "xsl applied on the XML output. If empty no transformation is applied, just the base XML is generated" )
				xsl: String = "/",
				@arg(short='c', doc = "generate CSV files with the given separator into crops.csv, prices.csv, factories.csv and tractors.csv" )
				csv: Option[String] = None,
				@arg(short = 'v', hidden=true, doc = "Log extra information during processing")
				verbose: Flag = Flag(false),
			): Unit = {
		bVerbose = verbose.value
		val defName = "fsinfo"
		val directPath = path.startsWith(":")
		var outFile = out.getOrElse(s"$defName.json")

		val xslt = Option(xsl.trim match {
				case "" =>
					outFile = out.getOrElse(s"$defName.xml")
					null

				case xsl =>	new StreamSource(
					if (xsl == "/") getClass.getResourceAsStream("json.xslt")
					else FileInputStream(xsl))
				})

		// NOTE tractor not generated when output is not CSV
		// FIXME
		val printer: Printer = csv.fold ( XMLPrinter(if(outFile.trim == "-") Console.out
															else new FileOutputStream(outFile.trim), xslt) )
										{ sep => CSVPrinter( if(sep.trim.isEmpty) ";" else sep.trim)}
		try {
			if (extract.contains('t') && csv.isDefined) {
				Tractors.firstEngineOnly = extract.contains("t!")
				printer.tractors(Tractors(path))
			}
			if (((extract.contains('c') || extract.contains('f')) && csv.isDefined)
				|| (csv.isEmpty && !directPath)){
				// FillTypes first: FruitTypes and Productions both look up FillTypes.price
				printer.fillTypes(FillTypes(path))
			}
			if (extract.contains('c')) {
				printer.fruitTypes(FruitTypes(path))
			}
			if (extract.contains('f')) {
				printer.productions(Productions(path))
			}
		} finally {
			printer.close()
		}
	}

	def main(args: Array[String]): Unit =
		ParserForMethods(this).runOrExit(args, true)
}
