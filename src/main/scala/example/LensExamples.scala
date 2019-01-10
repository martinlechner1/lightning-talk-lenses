package example

import monocle.Lens
import monocle.macros.GenLens
import monocle.function.Cons.headOption

object LensExamples {

  case class Street(name: String, number: Int)
  case class Address(street: Street)
  case class Dealer(id: String, address: Address)
  case class Listing(id: String, dealer: Dealer)

  val listing = Listing("1", Dealer("1", Address(Street("bothestraße", 3))))

  def naiveProcessStreet(listing: Listing): Listing =
   listing.copy(
    dealer = listing.dealer.copy(
      address = listing.dealer.address.copy(
        street = listing.dealer.address.street.copy(
          name=listing.dealer.address.street.name.capitalize
        )
      )
    )
  )

  def primitiveLensProcessStreet(listing: Listing): Listing = 
    GenLens[Listing](_.dealer.address.street.name).modify(_.capitalize)(listing)

  // WTF ???
  // step back
  // what is a lens
  // pair of functions get and set
  // allow to zoom into a Product (e.g. case class in scala)

  // simple example get and set
  // get(s: S): A
  // set(a: A): S => S
  val idLens = Lens[Listing, String](_.id)(n => a => a.copy(id = n))

  def getId(listing: Listing): String = idLens.get(listing)
  def setId(listing: Listing, dealerId: String): Listing = idLens.set(dealerId)(listing)

  // modify operation
  def addPrefix(listing: Listing) = idLens.modify("p" + _)(listing)

  // Generation
  // GenLens Macro works for simple types

  // Composition
  val dealer = GenLens[Listing](_.dealer)
  val address = GenLens[Dealer](_.address)
  val street  = GenLens[Address](_.street)
  val streetName = GenLens[Street](_.name)
  val streetNameLens = dealer composeLens address composeLens street composeLens streetName

  def composeLensProcessStreet(listing: Listing): Listing = 
    streetNameLens.modify(_.capitalize)(listing)

}


