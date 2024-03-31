package ext

extension (n: Int)
  def dec: Int = n - 1
  def inc: Int = n + 1
  def even: Boolean = (n & 1) == 0
  def odd: Boolean = (n & 1) == 1