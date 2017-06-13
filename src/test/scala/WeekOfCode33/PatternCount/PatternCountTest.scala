package WeekOfCode33.PatternCount

class PatternCountTest extends org.scalatest.FunSuite {

  test("It should match prefix occurance") {
    val result = PatternCount.count("1001abc", 0)

    assert(result == 1)
  }

  test("It matches two times in 10101") {
    val result = PatternCount.count("10101", 0)
    assert(result == 2)
  }

  test("It does not match in 10a01") {
    val result = PatternCount.count("10a01", 0)
    assert(result == 0)
  }

  test("It does not match in 10000") {
    val result = PatternCount.count("10000", 0)
    assert(result == 0)
  }

  test("It does not match in abcd") {
    val result = PatternCount.count("abcd", 0)
    assert(result == 0)
  }

  test("It does not match in 1") {
    val result = PatternCount.count("1", 0)
    assert(result == 0)
  }

  test("It works with long strings") {
    val string = "101" + "01" * 998 + "1"

    assert(2000 == string.length)

    val result = PatternCount.count(string, 0)
    assert(result == 999)
  }
}
