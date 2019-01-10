package example
import monocle.Lens

object LensLaws {

  def getSet[S, A](l: Lens[S, A], s: S): Boolean =
    l.set(l.get(s))(s) == s

  def setGet[S, A](l: Lens[S, A], s: S, a: A): Boolean =
    l.get(l.set(a)(s)) == a

}