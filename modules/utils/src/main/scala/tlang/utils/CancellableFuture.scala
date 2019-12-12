package tlang
package utils

import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.{CancellationException, ExecutionContext, Future, Promise}

object CancellableFuture {

  type CancelFunction = () => Boolean

  def apply[T](fun: => T)(implicit ex: ExecutionContext): CancellableFuture[T] = {
    val promise = Promise[T]()
    val future = promise.future
    val threadRef = new AtomicReference[Thread](null)
    promise tryCompleteWith Future {
      val thread = Thread.currentThread
      threadRef.synchronized { threadRef.set(thread) }
      try fun finally {
        threadRef.synchronized { threadRef getAndSet null } ne thread
      }
    }

    CancellableFuture(future, () => {
      if (promise.isCompleted) {
        true
      } else {
        // We have to use stop() to kill the thread since we have no control
        // over execution so this warning is disabled.

        //noinspection ScalaDeprecation
        threadRef.synchronized { Option(threadRef getAndSet null) foreach { _.stop() } }
        promise.tryFailure(new CancellationException)
      }
    })
  }
}

case class CancellableFuture[T](future: Future[T], cancel: CancellableFuture.CancelFunction)
