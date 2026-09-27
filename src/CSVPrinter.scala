import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import pprint.pprintln
import scala.math.BigDecimal as Decimal
import Fmt.{bool, dec, round}
import FruitTypes.FruitType
/**
 * Owns every CSV layout rule (cell separator, placeholder runs, row breaks, header lines,
 * file names) so the models stay format agnostic.
 * Own file vs keeping the writer in FSXtract: XMLPrinter can share Printer
 * without CSV I/O deps, and file output stays in one place.
 */
class CSVPrinter( separator: String) extends Printer {
	private var wr: PrintWriter = _
	private var actRow = 2 // header line already written when the first data row follows

	/** row break + spreadsheet row counter; "\n" (not println) preserves the historical output */
	private def nl: String = {
		actRow += 1
		"\n"
	}

	private def cell(s: String*) = s.mkString(separator)
	private def emptyCells(i: Int) = separator * i
	private def row(s: String*) = wr.print(cell(s*) + nl)

	/** opens `name`, writes the header line and closes the writer even if the body throws */
	private def out(name: String, headers: String)(body: => Unit): Unit = {
		wr = new PrintWriter(name, StandardCharsets.UTF_8)
		wr.println(headers)
		try body finally close()
	}

	override
	def fillTypes(s: Seq[FillTypes.FillType]): Unit =
		out("prices.csv", cell("name","price","showOnPriceTable")) {
			s.foreach { e =>
				row(e.name, dec(e.pricePerM3), bool(e.showOnPriceTable))
			}
		}

	override
	def fruitTypes(s: Seq[FruitType]): Unit =
		extension (f: FruitType)
			def incomePerHa: Decimal = round(FillTypes.price(f.name) * f.yieldM3PerHa * 1000)
	
		out("crops.csv", cell("name","seed","yield","windrow")) {
			s.foreach { e =>
				row(e.name, dec(e.seedM3PerHa), dec(e.yieldM3PerHa), dec(e.windrowM3PerHa)
					, dec(e.incomePerHa), dec(e.chafFactor))
			}
		}

	/** one row per engine variant, replacing the old `"%s"` template trick in Tractor.toCsv */
	override
	def tractors(s: Seq[Tractors.Tractor]): Unit =
		out("tractors.csv", cell("brand","series","name","hp","price","maxSpeed","cat","extras")) {
			s.foreach { t =>
				t.engines.foreach { e =>
					row(t.brand, t.series, e.name, e.hp.toString, e.price.toString
						, t.maxSpeed.toString, t.cat, t.extra.mkString("\"",", ","\""))
				}
			}
		}

	override
	def productions(s: Seq[Productions.Factory]): Unit =
		out("factories.csv", cell("name","price","cycles/d","run costs/d","input","amount/c","storage","cost/d"
				,"product","amount/c","storage","income/d"
				,"total income/d","profit/d","profit ratio/d","total profit/d","total profit ratio/d")) {
			s.foreach { point }
		}

	/** a production point spans as many rows as its productions have input/output pairs */
	private def point(pp: Productions.Factory): Unit = {
		val total = pp.totalProfit
		pp.productions.zipWithIndex.foreach { (p, i) =>
			if (i == 0) wr.print(cell(pp.name, dec(pp.price), ""))
			else wr.print(nl + emptyCells(2))
			production(p, i == 0, total)
		}
		wr.print(nl)
	}

	/** empty placeholder runs keep the shared columns aligned under the header */
	private def production(p: Productions.Production, first: Boolean, total: Productions.Profit): Unit =
		for (i <- 0 until Math.max(p.inputs.size, p.outputs.size)) {
			if (i == 0) wr.print(cell(dec(p.cyclesPerDay), dec(p.costPerDay), ""))
			else wr.print(nl + emptyCells(4))
			amount(p.inputs.lift(i), p.cyclesPerDay)
			amount(p.outputs.lift(i), p.cyclesPerDay)
			val t = if (first && i == 0) Some(total) else None
			wr.print(cell(t.fold("")(x => dec(x.income))
				, if (i == 0) profit(p.profit) else emptyCells(1)
				, t.fold(emptyCells(1))(profit)))
		}

	private def amount(a: Option[Productions.Amount], cycles: Decimal): Unit =
		wr.print(a.fold(emptyCells(4))(x => cell(x.name, dec(x.amount), x.store.toString
			, dec(x.value(cycles)), "")))

	private def profit(p: Productions.Profit) = cell(dec(round(p.profit)), dec(p.ratio))

	override
	def close(): Unit = if (wr != null) {
		wr.close()
		wr = null
	}
}
