package tlang
package testutils

import org.markushauck.mockito._
import org.mockito.Mockito
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.OngoingStubbing

import scala.reflect.ClassTag

// Stolen from org.markusshauck but with an overloaded returns method instead of returnsMulti
// and changed verification syntax
object MockitoSugar extends MockitoSugar
trait MockitoSugar extends MockingSyntax
  with StubbingSyntax
  with ArgumentMatchingSyntax
  with MockSyntax
  with VerificationSyntax

trait StubbingSyntax {
  implicit class StubbingOps[A](mockee: => A) {

    @inline def returns(r: A, rs: A*): OngoingStubbing[A] = {
      rs.foldLeft(Mockito.when(mockee).thenReturn(r)) { (stub, next) => stub thenReturn next }
    }

    def returnsDefault(implicit default: MockDefault[A]): OngoingStubbing[A] = {
      Mockito.when(mockee).thenReturn(default.value)
    }

    def throws[E <: Throwable](e: E): OngoingStubbing[A] = {
      Mockito.when(mockee).thenThrow(e)
    }

    def callsRealMethod: OngoingStubbing[A] = {
      Mockito.when(mockee).thenCallRealMethod()
    }

    def answers(f: InvocationOnMock => A): OngoingStubbing[A] = {
      Mockito
        .when(mockee)
        .thenAnswer((invocation: InvocationOnMock) => f(invocation))
    }

    def forwardsArg(i: Int): OngoingStubbing[A] = {
      answers(_.getArgument[A](i))
    }
  }
}

trait VerificationSyntax {

  def *[T: ClassTag]: T = org.mockito.ArgumentMatchers.any[T]()

  class ThereWord {
    def are[T](calls: => T): Unit = calls
    def was[T](calls: => T): Unit = calls
    def were[T](calls: => T): Unit = calls
  }

  def there = new ThereWord

  def noMoreInteractions[A <: AnyRef](mock: => A): Unit = Mockito.verifyNoMoreInteractions(mock)
  def one[A <: AnyRef](mock: => A): A = Mockito.verify(mock, Mockito.times(1))
  def two[A <: AnyRef](mock: => A): A = Mockito.verify(mock, Mockito.times(2))
  def three[A <: AnyRef](mock: => A): A = Mockito.verify(mock, Mockito.times(3))

  def atLeastOne[A <: AnyRef](mock: => A): A = Mockito.verify(mock, Mockito.atLeast(1))
  def atMostOne[A <: AnyRef](mock: => A): A = Mockito.verify(mock, Mockito.atMost(1))
  def only[A <: AnyRef](mock: => A): A = Mockito.verify(mock, Mockito.only())
  def exactly[A <: AnyRef](n: Int)(mock: => A): A = Mockito.verify(mock, Mockito.times(n))
  def no[A <: AnyRef](mock: => A): A = Mockito.verify(mock, Mockito.never())
  def atLeast[A <: AnyRef](n: Int)(mock: => A): A = Mockito.verify(mock, Mockito.atLeast(n))
  def atMost[A <: AnyRef](n: Int)(mock: => A): A = Mockito.verify(mock, Mockito.atMost(n))
  def zeroInteractions[A <: AnyRef](mock: => A): Unit = Mockito.verifyZeroInteractions(mock)
}
