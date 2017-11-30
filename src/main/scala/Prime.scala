object Prime {
  def bigInts(start: BigInt): Stream[BigInt] =
    start #:: bigInts(start + 1)

  def primes: Stream[BigInt] = {
    def primesAcc(acc: Stream[BigInt]): Stream[BigInt] = {
        acc.head #:: primesAcc(acc.tail).filter(_ % acc.head != 0)
    }
    primesAcc(bigInts(2))
  }

  def factors(n: BigInt): Map[BigInt, Long] = ???
}
