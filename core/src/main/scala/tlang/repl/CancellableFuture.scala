package tlang.repl

import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.{CancellationException, ExecutionContext, Future, Promise}

object CancellableFuture {

  def apply[T](fun: => T)(implicit ex: ExecutionContext): (Future[T], () => Boolean) = {
    val p = Promise[T]()
    val f = p.future
    val aref = new AtomicReference[Thread](null)
    p tryCompleteWith Future {
      val thread = Thread.currentThread
      aref.synchronized { aref.set(thread) }
      try fun finally {
        aref.synchronized { aref getAndSet null } ne thread
        //Deal with interrupted flag of this thread in desired
      }
    }

    (f, () => {
      aref.synchronized { Option(aref getAndSet null) foreach { _.stop() } }
      p.tryFailure(new CancellationException)
    })
  }

}
