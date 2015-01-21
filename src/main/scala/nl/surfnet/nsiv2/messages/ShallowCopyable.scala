package nl.surfnet.nsiv2
package messages

trait ShallowCopyable[A] {
  def shallowCopy(a: A): A
}
object ShallowCopyable {
  def apply[A](implicit instance: ShallowCopyable[A]) = instance

  def build[A](f: A => A): ShallowCopyable[A] = new ShallowCopyable[A] {
    override def shallowCopy(a: A): A = f(a)
  }
}
