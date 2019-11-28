package json

import cats._
import cats.implicits._

object Json {
  def charP(char: Char): Parser[Char] = (raw: String) => {
    raw.toList match {
      case c :: rest if c == char => Some((rest.mkString, c))
      case _ => None
    }
  }

  def stringP(str: String): Parser[String] = {
    str.toList.traverse(charP) fmap (_.mkString)
  }

  def spanP(f: Char => Boolean): Parser[String] = (raw: String) => {
    Some(raw.span(f).swap)
  }

  def nonEmpty(parser: Parser[String]): Parser[String] = (raw: String) => {
    parser(raw).filter { case (_, x) => !x.isEmpty }
  }

  lazy val ws: Parser[String] = spanP(Character.isWhitespace)

  def sepBy[A](sep: Parser[Char])(parser: Parser[A]): Parser[List[A]] = {
    (parser, many(sep *> parser)).mapN(_ :: _) <+> Parser.pure(List())
  }

  lazy val nullP: Parser[JsonValue] = stringP("null") fmap (_ => JsonNull)

  lazy val boolP: Parser[JsonValue] = {
    stringP("true") <+> stringP("false") fmap {
      case "true" => JsonBool(true)
      case "false" => JsonBool(false)
    }
  }

  lazy val numberP: Parser[JsonValue] = {
    nonEmpty(spanP(Character.isDigit)) fmap (_.toInt) fmap JsonNumber
  }

  lazy val stringLiteral: Parser[String] = charP('"') *> spanP(_ != '"') <* charP('"')

  lazy val jsonStringP: Parser[JsonValue] = stringLiteral fmap JsonString

  def arrayP: Parser[JsonValue] = (raw: String) => {
    val sep = sepBy(ws *> charP(',') <* ws)(_: Parser[JsonValue])
    (charP('[') *> ws *> sep(jsonValueP) <* ws <* charP(']') fmap JsonArray)(raw)
  }

  def objectP: Parser[JsonValue] = (raw: String) => {
    val sep = sepBy(ws *> charP(',') <* ws)(_: Parser[(String, JsonValue)])
    val keyValue = (stringLiteral, ws *> charP(':') <* ws, jsonValueP).mapN((key, _, value) => (key, value))
    (charP('{') *> ws *> (sep(keyValue) fmap (_.toMap)) <* ws <* charP('}') fmap JsonObject)(raw)
  }

  def jsonValueP: Parser[JsonValue] = {
    nullP <+> boolP <+> numberP <+> jsonStringP <+> arrayP <+> objectP
  }

  lazy val manyTest: Parser[List[JsonValue]] = many(jsonValueP)

  def many[A](parser: Parser[A]): Parser[List[A]] = (raw: String) => {
    parser(raw).flatMap({ case (raw1, x) => many(parser).fmap(x :: _)(raw1) }) <+> Some((raw, List()))
  }

  type Parser[A] = String => Option[(String, A)]

  object Parser extends Applicative[Parser] with SemigroupK[Parser] {
    override def pure[A](x: A): Parser[A] = (raw: String) => Some((raw, x))

    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = (raw: String) => {
      for {
        (raw1, f) <- ff(raw)
        (raw2, a) <- fa(raw1)
      } yield (raw2, f(a))
    }

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = (raw: String) => {
      fa(raw) fmap { case (str, a) => (str, f(a)) }
    }

    override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = (raw: String) => {
      x(raw) <+> y(raw)
    }
  }

  implicit val functorForParser: Functor[Parser] = new Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = Parser.map(fa)(f)
  }

  implicit val applicativeForParser: Applicative[Parser] = new Applicative[Parser] {
    override def pure[A](x: A): Parser[A] = Parser.pure(x)

    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = Parser.ap(ff)(fa)
  }

  implicit val semigroupKForParser: SemigroupK[Parser] = new SemigroupK[Parser] {
    override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = Parser.combineK(x, y)
  }
}

sealed abstract class JsonValue
case object JsonNull extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonBool(value: Boolean) extends JsonValue
case class JsonNumber(value: Int) extends JsonValue
case class JsonArray(value: List[JsonValue]) extends JsonValue
case class JsonObject(value: Map[String, JsonValue]) extends JsonValue
