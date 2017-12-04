package tlang.utils

import java.util.concurrent.ForkJoinPool

import tlang.utils.Extensions._

import scala.collection.parallel.{ForkJoinTaskSupport, TaskSupport}

trait Executor {

  def map[T, F](inputs: List[T])(f: T => F): List[F]
  def flatMap[T, F](inputs: List[T])(f: T => Traversable[F]): List[F]
  def foreach[T, F](inputs: Traversable[T])(f: T => F): Unit

}

object SingleThreadExecutor extends Executor {

  def map[T, F](inputs: List[T])(f: T => F): List[F] = inputs map f
  def flatMap[T, F](inputs: List[T])(f: T => Traversable[F]): List[F] = inputs flatMap f
  def foreach[T, F](inputs: Traversable[T])(f: T => F): Unit = inputs foreach f

}


case class ParallellExecutor(parallelism: Int) extends Executor {

  private val taskSupport: TaskSupport = new ForkJoinTaskSupport(new ForkJoinPool(parallelism))

  def map[T, F](inputs: List[T])(f: T => F): List[F] = parCollection(inputs).map(f).toList
  def flatMap[T, F](inputs: List[T])(f: T => Traversable[F]): List[F] = parCollection(inputs).flatMap(f).toList
  def foreach[T, F](inputs: Traversable[T])(f: T => F): Unit = parCollection(inputs) foreach f

  private def parCollection[T](inputs: Traversable[T]) = inputs.par use { _.tasksupport = taskSupport }

}
