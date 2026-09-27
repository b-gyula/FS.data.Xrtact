import java.io.{File, OutputStream, PrintWriter, Reader, StringReader}
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.{StreamResult, StreamSource}
import scala.collection.mutable
import scala.xml.{Elem, MetaData, Node, Null, PrettyPrinter, Text, TopScope, UnprefixedAttribute, XML}

/**
 * Builds one `<fs>` document per schema.xsd and writes it to stdout on `close()`.
 * Trees are buffered per collection because the schema fixes the element order
 * (fruit, price, factory) while the visitor is called in extraction order.
 */
class XMLPrinter(val outStrm: OutputStream
					 ,val xsl: Option[StreamSource]) extends Printer, WithLogger {
	private var prices: Seq[FillTypes.FillType] = Nil
	private var fruits: Seq[FruitTypes.FruitType] = Nil
	private var factories: Seq[Productions.Factory] = Nil
	private val renamed = mutable.Set[String]() // log each rename only once

	override def fillTypes(s: Seq[FillTypes.FillType]): Unit = prices = s

	override def fruitTypes(s: Seq[FruitTypes.FruitType]): Unit = fruits = s

	override def productions(s: Seq[Productions.Factory]): Unit = factories = s

	/** the schema has no vehicle section; CSV is the only format covering tractors */
	override def tractors(s: Seq[Tractors.Tractor]): Unit =
		log.info("tractors skipped")
	
	override def close(): Unit = {
		if(xsl.nonEmpty) xslTransform()
		else {
			val out = new PrintWriter(outStrm)
			XML.write(out, doc, "utf-8", true, null)
			out.flush()
		}
		outStrm.close()
	}

	/** StreamSource over Elem.toString: DOMSource needs a w3c Node; converting scala.xml is more code */
	def xslTransform() = {
		val src = new StreamSource(new StringReader(doc.toString))
		TransformerFactory.newInstance()
			.newTransformer(xsl.get)
			.transform(src, new StreamResult(outStrm))
	}
	
	private def doc: Elem =
		el("fs", Nil,
			Seq(
				el("crop", Nil, fruits.map { f =>
					el(name(f.name), Seq("seed" -> Fmt.dec(f.seedM3PerHa)
						, "yield" -> Fmt.dec(f.yieldM3PerHa)
						, "windrow" -> Fmt.dec(f.windrowM3PerHa)))
				})
				/*  negative value mark a price that is not shown in the game's price table */
				, el("price", prices.map { p =>
													val m3 = p.pricePerM3
													name(p.name) -> Fmt.dec(if (p.showOnPriceTable) m3 else -m3)
					})
				, el("factory", Nil, factories.map { pp =>
					el(name(pp.name), Seq("price" -> Fmt.dec(pp.price), "variant" -> pp.variant),
						pp.productions.map { p =>
							el(name(p.id), Seq("cost" -> Fmt.dec(p.costPerDay)
													,"cycle" -> Fmt.dec(p.cyclesPerDay)
													,"profit" -> Fmt.dec(p.profit.ratio))
							, Seq(el("in", p.inputs.map(a => name(a.name) -> Fmt.dec(a.amount)))
								, el("out", p.outputs.map(a => name(a.name) -> Fmt.dec(a.amount)))))
					})
				})
			)
		)

	private def el(tag: String, attribs: Seq[(String, String)], kids: Seq[Node] = Nil): Elem =
		Elem(null, tag, metadata(attribs), TopScope, kids.isEmpty, kids *)

	/** empty values are dropped: `""` is not a valid `double` for the schema, and a repeated
	 * game name must not end up as a duplicate attribute */
	private def metadata(attribs: Seq[(String, String)]): MetaData =
		attribs.filter((_, v) => v.nonEmpty).distinctBy(_._1)
			.foldRight(Null: MetaData)( (kv, a) => new UnprefixedAttribute(kv._1, Text(kv._2), a))

	/** game names are arbitrary strings, XML names allow no spaces and no leading digit */
	private def name(s: String): String = {
//		try {
//			XML.loadString(s"<${s}/>")
//		} catch {
//			case t:Throwable => log.severe(t.toString)
//		}
		val n = s.replaceAll("[^A-Za-z0-9_.\\-]", "_")
		val v = if (n.isEmpty || !(n.head.isLetter || n.head == '_')) "_" + n else n
		if (v != s && renamed.add(s)) log.info(s"factory '$s' renamed to '$v'")
		v
	}
}
