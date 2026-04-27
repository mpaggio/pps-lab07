package ex1

import ex1.*
import ex1.Parsers.*
import org.scalatest.matchers.should.Matchers.*

class ParserTestsWithScalaTest extends org.scalatest.funsuite.AnyFunSuite:
  def parser = new BasicParser(Set('a', 'b', 'c'))
  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  // note we do not need a class name here, we use the structural type
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
  def sparser: Parser[Char] = "abc".charParser()
  def parserSTN = new ShortThenNParser(Set('X', 'Y', 'Z'), 3)

  test("Test basic parser"):
    parser.parseAll("aabc".toList) should be (true)
    parser.parseAll("aabcdc".toList) should be (false)
    assert(parser.parseAll("".toList))

  test("Test not empty parser"):
    assert(parserNE.parseAll("0101".toList))
    assert(!parserNE.parseAll("0123".toList))
    assert(!parserNE.parseAll(List()))

  test("Test not two consecutive parser"):
    assert(parserNTC.parseAll("XYZ".toList))
    assert(!parserNTC.parseAll("XYYZ".toList))
    assert(parserNTC.parseAll("".toList))

  test("Test not empty and not two consecutive parser"):
    assert(parserNTCNE.parseAll("XYZ".toList))
    assert(!parserNTCNE.parseAll("XYYZ".toList))
    assert(!parserNTCNE.parseAll("".toList))

  test("Test string parser"):
    assert(sparser.parseAll("aabc".toList))
    assert(!sparser.parseAll("aabcdc".toList))
    assert(sparser.parseAll("".toList))

  test("Test short then n parser"):
    assert(parserSTN.parseAll("XYZ".toList))
    assert(!parserSTN.parseAll("XYYZ".toList))
    assert(parserSTN.parseAll("".toList))
