package ssol.dbg

import com.sun.jdi._
import event._
import request._
import scala.collection.JavaConversions._

import scala.actors._



object Main {

  var boxedObjects = 0L
  
  object EventActor extends ReplyReactor {
    def act() {
      loop {
        react {
          case ClassPrepareEvent(refType, thread) =>
            addMethodEntryBreakpoint("scala.runtime.BoxesRunTime.boxToInteger")
  
          case VMDeathEvent() =>
            this.exit()
          case VMDisconnectEvent =>
            this.exit()
            
          case bp @ BreakpointEvent(thread) =>
            boxedObjects += 1
            frameBoxes(thread.frames) += 1
            if (boxedObjects % 10000 == 0)
              println("Reached %d boxed integers".format(boxedObjects))
            
          case msg => 
            println("received: " + msg)
        }
      }
    }
  }

  import scala.collection._
  val frameBoxes = new mutable.HashMap[java.util.List[StackFrame], Long] withDefaultValue(0)
  
  def foo {}
  
  def log(pre: String, msg: String) {
    println("[%s] %s".format(pre, msg))
  }

  def info(msg: String) =
    if (verbose) log("info", msg)
  
  var mainClass = ""
  var options = ""
  var verbose = false
  var targetCP = "/Users/dragos/research/thesis/code/evaluation/classes"
  
  val vmManager = Bootstrap.virtualMachineManager
  val connector = vmManager.defaultConnector()
  var debugVm: VirtualMachine = _
    
  def main(args: Array[String]) {
    for (arg <- args) arg match {
      case "-connectors" =>
        println(vmManager.allConnectors.toList.mkString("\n"))
        
      case "-cp:<targetcp>" =>
        targetCP = arg.drop(4)

      case "-verbose" =>
        verbose = true
      case _ =>
    }
    
    val debugeeArgs = args.dropWhile(_.startsWith("-")).mkString(" ")
    
    if (debugeeArgs.isEmpty) usage()
    info("Using connector " + connector.toString)
    info("Using target classpath: " + targetCP)
    debugVm = launch(debugeeArgs)
    val eventPump = new EventStream(debugVm)
    EventActor.start()
    eventPump.start()
    
    new StreamRedirect(debugVm.process().getInputStream, System.out).start()
    new StreamRedirect(debugVm.process().getErrorStream, System.err).start()
   
    
    addClassPrepareRequest()
    
    debugVm.resume()
    eventPump.join
    
    println("Counted boxToInt: %d".format(boxedObjects))
  }
  
  def addClassPrepareRequest() {
    val req = debugVm.eventRequestManager().createClassPrepareRequest()
    req.addClassFilter("scala.runtime.BoxesRunTime")
    req.enable()
    info("added class prepare request " + req)
  }

  def addMethodEntryRequest() {
    val req = debugVm.eventRequestManager().createMethodEntryRequest()
    req.addClassFilter("scala.runtime.BoxesRunTime")
    req.enable()
    info("added method entry request " + req)
  }

    def addMethodEntryBreakpoint(method: String) {
    val (clsName, methodName) = method.splitAt(method lastIndexOf ".")
    info("adding method breakpoint at " + clsName + ":" + methodName.drop(1))
    import collection.JavaConversions._
    
    val methods = 
      for (cls <- debugVm.classesByName(clsName);
          m <- cls.methodsByName(methodName.drop(1))) yield m
    if (methods.isEmpty) {
      log("warning", "Could not install breakpoint at " + method)
      null
    } else {
      val req = debugVm.eventRequestManager().createBreakpointRequest(methods.head.location)
      req.enable()
      req
    }
  }

  val SCALA_LIB="/Users/dragos/workspace/git/scala/build/pack/lib/scala-library.jar"
  
  def launch(line: String): VirtualMachine = {
    info("debugee cmd line " + line)
    val args = connector.defaultArguments
    args("main").setValue(line)
    val cp = targetCP + java.io.File.pathSeparator + SCALA_LIB
    args("options").setValue("-cp " + cp)

    info("Launching with arguments " + args)
    connector.launch(args)
  }
  
  def report() {
    println("Boxed Ints: %d".format(boxedObjects))
    val (stack, boxes) = frameBoxes.max(Ordering.by { p: (java.util.List[StackFrame], Long) => p._2 })
    
    println("Highes number of boxes: %d".format(boxes))
    println(stack mkString "\n")
  }
  
  def usage() {
    println("""
debugger [options] mainClass [options]

-connectors      print all connectors found by the VM
-verbose         print additional information
-cp:<targetcp>   classpath for the debugged process
""")
    System.exit(1)
  }
}