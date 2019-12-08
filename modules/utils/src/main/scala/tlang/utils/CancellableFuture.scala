package tlang
package utils

import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.{CancellationException, ExecutionContext, Future, Promise}


object CancellableFuture {

  type CancelFunction = () => Boolean

  def apply[T](fun: => T)(implicit ex: ExecutionContext): CancellableFuture[T] = {
    val promise = Promise[T]()
    val future = promise.future
    val aref = new AtomicReference[Thread](null)
    promise tryCompleteWith Future {
      val thread = Thread.currentThread
      aref.synchronized { aref.set(thread) }
      try fun finally {
        aref.synchronized { aref getAndSet null } ne thread
        //Deal with interrupted flag of this thread in desired
      }
    }

    CancellableFuture(future, () => {
      // We have to use stop() to kill the thread since we have no control
      // over execution so this warning is disabled.
      //noinspection ScalaDeprecation
      aref.synchronized { Option(aref getAndSet null) foreach { _.stop() } }
      promise.tryFailure(new CancellationException)
    })
  }

}

case class CancellableFuture[T](future: Future[T], cancel: CancellableFuture.CancelFunction)
