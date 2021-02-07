package no.jergan.scrapbook.fpinscala

object Chapter13 {


  trait IO {

    self =>

    def run: Unit

    def ++(io: IO): IO = new IO {
      def run = {
        self.run
        io.run
      }
    }
  }

}
