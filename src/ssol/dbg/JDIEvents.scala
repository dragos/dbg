package ssol.dbg

import java.io._

import com.sun.jdi._
import request.EventRequest


abstract class JDIEvent(request: EventRequest)

case class VMStartEvent(thread: ThreadReference)(req: EventRequest) extends JDIEvent(req)
case object VMDisconnectEvent extends JDIEvent(null)

case class VMDeathEvent()(request: EventRequest) extends JDIEvent(request)

case class ThreadStartEvent(thread: ThreadReference)(req: EventRequest) extends JDIEvent(req)
case class ThreadDeathEvent(thread: ThreadReference)(req: EventRequest) extends JDIEvent(req)

case class ClassPrepareEvent(referenceType: ReferenceType, thread: ThreadReference)(req: EventRequest) extends JDIEvent(req)
case class ClassUnloadEvent(name: String, sig: String)(req: EventRequest) extends JDIEvent(req)

case class MethodEntryEvent(location: Location)(req: EventRequest) extends JDIEvent(req)
case class MethodExitEvent(location: Location)(req: EventRequest) extends JDIEvent(req)
case class StepEvent(location: Location, thread: ThreadReference)(req: EventRequest) extends JDIEvent(req)
case class WatchpointEvent(obj: ObjectReference, field: Field, value: Value)(req: EventRequest) extends JDIEvent(req)
case class BreakpointEvent(thread: ThreadReference)(req: EventRequest) extends JDIEvent(req)

class EventStream(vm: VirtualMachine) extends Thread("event-handler") {
  var connected = true

  import collection.JavaConversions._
  override def run() {
    val queue = vm.eventQueue
    while (connected) {
      val set = queue.remove()
      for (ev <- set.eventIterator) {
        val msg = ev match {
          case e: event.VMStartEvent => VMStartEvent(e.thread)(e.request)
          case e: event.VMDisconnectEvent => VMDisconnectEvent
          case e: event.VMDeathEvent => VMDeathEvent()(e.request)
          case e: event.ThreadStartEvent => ThreadStartEvent(e.thread)(e.request)
          case e: event.ThreadDeathEvent => ThreadDeathEvent(e.thread)(e.request)
          case e: event.ClassPrepareEvent => ClassPrepareEvent(e.referenceType, e.thread)(e.request)
          case e: event.ClassUnloadEvent => ClassUnloadEvent(e.className, e.classSignature)(e.request)
          case e: event.MethodEntryEvent => MethodEntryEvent(e.location)(e.request)
          case e: event.MethodExitEvent => MethodExitEvent(e.location)(e.request)
          case e: event.StepEvent => StepEvent(e.location, e.thread)(e.request)
          case e: event.WatchpointEvent => WatchpointEvent(e.`object`, e.field, e.valueCurrent)(e.request)
          case e: event.BreakpointEvent => BreakpointEvent(e.thread)(e.request)
          case _ => null
        }
        Main.EventActor !? msg
        println("resuming")
        set.resume()
      }
    }
  }
}

class StreamRedirect(in: InputStream, out: OutputStream) extends Thread {
  val SIZE = 1024
  
  override def run() {
    val reader = new InputStreamReader(in)
    val writer = new OutputStreamWriter(out)
    val buf = new Array[Char](SIZE)
    var count = 0
    count = reader.read(buf, 0, SIZE)
    while (count >= 0) {
      writer.write(buf, 0, count)
      writer.flush()
      count = reader.read(buf, 0, SIZE)
    }
  }
}





