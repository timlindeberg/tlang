package tlang.repl.actors

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem}
import akka.dispatch.{Envelope, MailboxType, MessageQueue}
import com.typesafe.config.Config

class SingleMessageMailbox extends MailboxType {

  // This constructor signature must exist, it will be called by Akka
  def this(settings: ActorSystem.Settings, config: Config) = this()

  // The create method is called to create the MessageQueue
  final override def create(owner: Option[ActorRef], system: Option[ActorSystem]): MessageQueue =
    new MessageQueue {
      val message = new AtomicReference[Envelope]

      final def cleanUp(owner: ActorRef, deadLetters: MessageQueue): Unit =
        Option(message.get) foreach { deadLetters.enqueue(owner, _) }

      def enqueue(receiver: ActorRef, handle: Envelope): Unit = message.set(handle)

      def dequeue(): Envelope = message.getAndSet(null)

      def numberOfMessages: Int = if (hasMessages) 1 else 0

      def hasMessages: Boolean = message.get != null
    }
}