package no.jergan.scrapbook

object Arktekk {

  def faktor(partnere: Int, igjenAaAnsette: Int, aarFoerManBlirPartner: Int, nyansattePerAar: Int): Double =
    faktor(List.fill(partnere)(aarFoerManBlirPartner), igjenAaAnsette, aarFoerManBlirPartner, nyansattePerAar)

  def faktor(populasjon: List[Int], igjenAaAnsette: Int, aarFoerManBlirPartner: Int, nyansattePerAar: Int): Double = {
    val ansettesIAar = Math.min(nyansattePerAar, igjenAaAnsette)
    val etterAnsettelse = populasjon.appendedAll(List.fill(ansettesIAar)(0))
    val nye = etterAnsettelse.count(_ < aarFoerManBlirPartner)
    val partnere = etterAnsettelse.count(_ >= aarFoerManBlirPartner)
    if (nye == 0) 0 else nye.toDouble / partnere +
      faktor(etterAnsettelse.map(_ + 1), igjenAaAnsette - ansettesIAar, aarFoerManBlirPartner, nyansattePerAar)
  }

  def main(args: Array[String]): Unit =
  {
//    val tall = 871376
    val tall = 459555
    println(faktor(10, 10, 3, 10)) // denne bør bli 3
    println(faktor(11, 15, 2, 5))

    // Faktor returnerer antall nyansatt-aar per partner for hele perioden,
    // kan typisk ganges med overskuddet per nyansatt per aar for aa finne et totalbeloep per opprinnelig partner.
  }

}
