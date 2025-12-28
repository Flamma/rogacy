package net.asqueados.rogacy

@main
def main(args: String*): Unit = {
  val viewEverything = args.contains("--view-everything")
  Game.start(viewEverything)
}

