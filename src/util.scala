import java.util.logging.Logger
import scala.math.BigDecimal as Decimal
import scala.math.BigDecimal.RoundingMode.HALF_DOWN

/**
 * Visitor over the extracted domain collections.
 * One typed method per collection (instead of `out(Extractor, fileName)` with `Any` matching)
 * keeps layout knowledge in the printers and lets a format skip what it cannot render.
 */
trait Printer {
	def fillTypes(s: Seq[FillTypes.FillType]): Unit
	def fruitTypes(s: Seq[FruitTypes.FruitType]): Unit
	def tractors(s: Seq[Tractors.Tractor]): Unit
	def productions(s: Seq[Productions.Factory]): Unit
	def close(): Unit
}

/**
 * The only place where a value becomes text, so CSV and XML never diverge in formatting.
 * Zero renders as the empty string: the historical CSV convention for "no data".
 */
object Fmt {
	def dec(d: Decimal): String = if (d == 0) "" else d.bigDecimal.stripTrailingZeros.toPlainString
	def bool(b: Boolean): String = if (b) "true" else ""
	def flt(f: Float): String = if (f == 0) "" else f.toString
	def round(d: Decimal, scale: Int = 0): Decimal = d.setScale(scale, HALF_DOWN)
}

trait WithLogger {
	lazy val log = Logger.getGlobal
}