import scala.annotation.tailrec
import scala.util.Random

extension (n: Int)
  def dec: Int = n - 1
  def inc: Int = n + 1
  def even: Boolean = (n & 1) == 0
  def odd: Boolean = (n & 1) == 1

def sq(x: Int): Int = x * x
def sq(x: Float): Float = x * x
def sq(x: Double): Double = x * x


def ex_1_3(a: Int, b: Int, c: Int) =
  Seq(a, b, c) reduce math.min match
    case a => sq(b) * sq(c)
    case b => sq(a) * sq(c)
    case c => sq(a) * sq(b)

// 1.1.7 newton's method
def average(x: Double, y: Double) = (x + y) / 2
def improve(guess: Double, x: Double) = average(guess, x / guess)
def goodEnough(guess: Double, x: Double) = Math.abs(sq(guess) - x) < 0.0001
@tailrec def sqrtIter(guess: Double, x: Double): Double =
  if goodEnough(guess, x) then guess else sqrtIter(improve(guess, x), x)
def sqrt = sqrtIter(1.0, _)

// 1.6
def newIf_1_6[T](predicate: Boolean, thenResult: T, elseResult: T): T =
  predicate match
    case true => thenResult
    case false => elseResult

// will stack overflow
def sqrtIter_1_6(guess: Double, x: Double): Double =
  newIf_1_6(goodEnough(guess, x), guess, sqrtIter_1_6(improve(guess, x), x))

// 1.7
def goodEnough_1_7(guess: Double, x: Double) =
  (sq(guess) - x).abs < (x / 1E5)

// 1.8
def improve_1_8(x: Double, y: Double) =
  (x / sq(y) + 2 * y) / 3

def goodEnough_1_8(guess: Double, x: Double) =
  (Math.pow(guess, 3) - x).abs < (x / 1E5)

@tailrec def cubeRootIter_1_8(guess: Double, x: Double): Double =
  if goodEnough_1_8(guess, x) then guess else cubeRootIter_1_8(improve_1_8(guess, x), x)

def cubeRoot_1_8 = cubeRootIter_1_8(1.0, _)

def fact(n: Int) =
  if n > 0 then {
    @tailrec def core(acc: Int, x: Int): Int = if x == 1 then acc else core(acc * x, x.dec)

    core(1, n)
  } else
    throw RuntimeException("fact cannot accept n <= 0")

// 1.10
def A_1_10(x: Int, y: Int): Int =
  (x, y) match
    case (_, 0) => 0
    case (0, _) => 2 * y
    case (_, 1) => 2
    case _ => A_1_10(x.dec, A_1_10(x, y.dec))

def f_1_10 = A_1_10(0, _) // 2n
def g_1_10 = A_1_10(1, _) // 2^n
def h_1_10 = A_1_10(2, _) // 2^h(n-1), h(1) = 2
def k_1_10(n: Int) = 5 * sq(n)

def fib =
  @tailrec def core(a: Int, b: Int, count: Int): Int =
    if count == 0 then b else core(a + b, b, count.dec)

  core(1, 0, _)

def countChange =
  def fstDenomination(kindsOfCoins: Int) =
    kindsOfCoins match
      case 1 => 1
      case 2 => 5
      case 3 => 10
      case 4 => 25
      case 5 => 50

  def cc(amount: Int, kindsOfCoins: Int): Int =
    (amount, kindsOfCoins) match
      case (0, _) => 1
      case (x, _) if x < 0 => 0
      case (_, 0) => 0
      case _ => cc(amount, kindsOfCoins.dec) + cc(amount - fstDenomination(kindsOfCoins), kindsOfCoins)

  cc(_, 5)

def fn_1_11(n: Int) =
  @tailrec def core(fst: Int, snd: Int, thr: Int, x: Int): Int =
    if x <= 0 then fst else core(fst + 2 * snd + 3 * thr, fst, snd, x.dec)

  if n < 3 then n else core(2, 1, 0, n - 2)

def fn_1_11_2(n: Int): Int =
  if n >= 3 then {
    fn_1_11_2(n.dec) + fn_1_11_2(n - 2) * 2 + fn_1_11_2(n - 3) * 3
  } else n

// 1.12 pascal triangle
def fn_1_12(layer: Int) =
  def getPosNum_1_12(n: Int, currentLayer: Int, prevLayerLs: Vector[Int]) =
    if n == 0 || n == currentLayer.dec then 1 else prevLayerLs(n.dec) + prevLayerLs(n)

  def core(currentLayer: Int): Vector[Int] =
    if currentLayer == 1 then Vector(1) else {
      val prevLayer = core(currentLayer.dec)
      (for n <- 0 until currentLayer
        yield getPosNum_1_12(n, currentLayer, prevLayer)).toVector
    }

  (for n <- 1 until layer + 1 yield core(n)).toVector

// 1.15
def cube(x: Int) = x * x * x
def cube(x: Double) = x * x * x
def cube(x: Float) = x * x * x
def p(x: Double) = 3 * x - 4 * cube(x)
def sine(angle: Double): Double = if angle.abs <= 0.1 then angle else p(sine(angle / 3.0))

// 1.16
def exp_1_16(x: Int, n: Int): Int =
  if n == 0 then 1 else {
    @tailrec def core(x: Int, n: Int, prod: Int): Int =
      n match
        case 1 => prod
        case n if n.even => core(sq(x), n / 2, prod * x)
        case _ => core(x, n.dec, prod * x)

    core(x, n, x)
  }

// 1.17
def double_1_17(x: Int) = x + x
def double_1_17(x: Double) = x + x
def double_1_17(x: Float) = x + x

def halve_1_17(x: Int): Double =
  @tailrec def core(x: Double, quot: Double): Double =
    if x == double_1_17(quot) then quot else core(x, if x > 0 then quot - 1 else quot + 1)

  if x.even then core(x, x) else.5 + core(x.dec, x.dec)

def mul_1_17(x: Int, n: Int) =
  @tailrec def core(x: Int, n: Int, prod: Int): Int =
    n match
      case 1 => prod
      case n if n.even => core(double_1_17(x), halve_1_17(x).toInt, x + prod)
      case _ => core(x, n.dec, x + prod)

  core(x, n, x)

// 1.18 should be as same as 1.17

// 1.19!!! refer to sol
def fibIter_1_19(a: Int, b: Int, p: Int, q: Int, count: Int): Int =
  count match
    case 0 => b
    case n if n.even => fibIter_1_19(a, b, Vector(p, q) map sq reduce (_ + _), sq(q) + 2 * p * q, count / 2)
    case _ => fibIter_1_19(b * q + a * q + a * p, b * p + a * q, p, q, count.dec)

def fib_1_19 = fibIter_1_19(1, 0, 0, 1, _)

@tailrec def remainder(x: Int, y: Int): Int =
  x.compareTo(y) match
    case 0 => 0
    case n if n > 0 => remainder(x - y, y)
    case _ => x

@tailrec def gcd(a: Int, b: Int): Int =
  if b == 0 then a else gcd(b, remainder(a, b))

def divides(a: Int, b: Int) = remainder(b, a) == 0
@tailrec def findDivisor(n: Int, testDivisor: Int): Int =
  if sq(testDivisor) > n then
    n
  else if divides(testDivisor, n) then
    testDivisor
  else
    findDivisor(n, testDivisor.inc)

def smallestDivisor = findDivisor(_, 2)
def prime(n: Int) = n == smallestDivisor(n)

def expmod(base: Int, exp: Int, m: Int): Int =
  exp match
    case 0 => 1
    case n if n.even => remainder(sq(expmod(base, exp/2, m)), m)
    case _ => remainder(base * expmod(base, exp.dec, m), m)
def fermatTest(n: Int): Boolean =
  def tryIt(a: Int) = expmod(a,n,n) == a
  tryIt(1 + Random.nextInt(n.dec))

def fastPrime(n: Int, times: Int): Boolean =
  times == 0 || (fermatTest(n) && fastPrime(n, times.dec))