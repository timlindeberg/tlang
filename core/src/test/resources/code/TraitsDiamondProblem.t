// Ignore
new CowboyArtist().Draw() // res: A pretty painting

trait Drawable =

	Def Draw(): Unit

trait Cowboy : Drawable =
	Def Draw() = println("Bang!")

trait Artist : Drawable =
	Def Draw() = println("A pretty painting")

class CowboyArtist : Cowboy, Artist
