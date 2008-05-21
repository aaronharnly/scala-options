package net.harnly.aaron.options

import scala.collection.mutable.{ListBuffer}

// Base class for options. 
// These are things that get listed when we ask for help,
// and optionally can accept string arguments & perform some kind of action,
// usually mutating a var.
case class OptionDefinition(
   canBeInvoked: Boolean,
   shortopt: String,
   longopt: String,
   description: String,
   action: String => Unit,
   gobbleNextArgument: Boolean
)

// ----- Some standard option types ---------
class SeparatorDefinition(
   description: String
) extends OptionDefinition(false,null,null,description, {a: String => {}}, false)

class ArgOptionDefinition(
   shortopt: String,
   longopt: String,
   description: String,
   action: String => Unit
) extends OptionDefinition(true,shortopt, longopt, description, action, true)

class IntArgOptionDefinition(
   shortopt: String,
   longopt: String,
   description: String,
   action: Int => Unit
) extends OptionDefinition(true,shortopt, longopt, description, {a: String => action(a.toInt)}, true)

class DoubleArgOptionDefinition(
   shortopt: String,
   longopt: String,
   description: String,
   action: Double => Unit
) extends OptionDefinition(true,shortopt, longopt, description, {a: String => action(a.toDouble)}, true)

class BooleanArgOptionDefinition(
   shortopt: String,
   longopt: String,
   description: String,
   action: Boolean => Unit
) extends OptionDefinition(true,shortopt, longopt, description, { a: String => 
   val boolValue = a.toLowerCase match {
      case "true" => true
      case "false" => false
      case "yes" => true
      case "no" => false
      case "1" => true
      case "0" => false
      case _ => throw new IllegalArgumentException("Expected a string I can interpret as a boolean")
   }
   action(boolValue)}, true)

class FlagOptionDefinition(
   shortopt: String,
   longopt: String,
   description: String,
   action: => Unit
)
extends OptionDefinition(true,shortopt, longopt, description, { a: String => action }, false)


// OptionParser is instantiated within your object,
// set up by an (ordered) sequence of invocations of #on,
// and then handed the command-line arguments.
case class OptionParser(warnOnUnknownArgument: Boolean)
{
   def this() = this(true)
   
   val options = new ListBuffer[OptionDefinition]
   
   // -------- Defining options ---------------
   def add(option: OptionDefinition)
   {
      options += option
   }
   
   def on(shortopt: String, longopt: String, description: String, action: String => Unit) =
      add(new ArgOptionDefinition(shortopt, longopt, description, action))

   def on(shortopt: String, longopt: String, description: String, action: => Unit) =
      add(new FlagOptionDefinition(shortopt, longopt, description, action))

   def separator(description: String) =
      add(new SeparatorDefinition(description))

   def help(shortopt: String, longopt: String) =
      add(new FlagOptionDefinition(shortopt, longopt, "Show this help", { this.showUsage; exit }))

   // we have to give these typed options separate names, because of &^@$! type erasure
   def onInt(shortopt: String, longopt: String, description: String, action: Int => Unit) =
      add(new IntArgOptionDefinition(shortopt, longopt, description, action))

   def onDouble(shortopt: String, longopt: String, description: String, action: Double => Unit) =
      add(new DoubleArgOptionDefinition(shortopt, longopt, description, action))

   def onBoolean(shortopt: String, longopt: String, description: String, action: Boolean => Unit) =
      add(new BooleanArgOptionDefinition(shortopt, longopt, description, action))
   
   // -------- Getting usage information ---------------
   def descriptions: Seq[String] = options.map( opt => opt match {
         case x if ! x.canBeInvoked => x.description
         case x if x.gobbleNextArgument => x.shortopt + " <value> or " + x.longopt + " <value>: " + x.description
         case _ => opt.shortopt + " or " + opt.longopt + ": " + opt.description
      })
   
   def usage: String = descriptions.mkString("\n")
   
   def showUsage = Console.err.println(usage)
      
   // -------- Parsing ---------------
   def parse(args: Seq[String])
   {
      var i = 0
      while (i < args.length) {
         val arg = args(i)
         val matchingOption = options.find( opt =>
            opt.canBeInvoked
            && (arg == opt.shortopt || arg == opt.longopt)
         )
         matchingOption match {
            case None => if (warnOnUnknownArgument) System.err.println("WARNING: Unknown argument '" + arg + "'")
            case Some(option) =>
               val argToPass = if (option.gobbleNextArgument) {
                  i += 1; args(i)                                       
               } else
                  ""
               option.action.apply(argToPass)
         }
         i += 1
      }
   }
}
