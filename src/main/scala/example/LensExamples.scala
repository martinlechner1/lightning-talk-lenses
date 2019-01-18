package example

import monocle.Lens
import monocle.macros.GenLens
import monocle.function.Cons.headOption

object LensExamples {

  case class Street(name: String, number: Int)
  case class Address(street: Street)
  case class Dealer(id: String, address: Address)
  case class Listing(id: String, dealer: Dealer)

  def naiveCapitalizeStreet(listing: Listing): Listing =
   listing.copy(
    dealer = listing.dealer.copy(
      address = listing.dealer.address.copy(
        street = listing.dealer.address.street.copy(
          name=listing.dealer.address.street.name.capitalize
        )
      )
    )
  )

  def lensCapitalizeStreet(listing: Listing): Listing = 
    GenLens[Listing](_.dealer.address.street.name)
      .modify(_.capitalize)(listing)

  // Lens constructor

  val idLens = Lens[Listing, String](_.id)(newId => listing => listing.copy(id = newId))
  
  // Usage example
  def getId(listing: Listing): String = idLens.get(listing)
  def setId(listing: Listing, dealerId: String): Listing = idLens.set(dealerId)(listing)

  // modify operation
  def addPrefix(listing: Listing) = idLens.modify("p" + _)(listing)

  // Generation macro
  val dealer = GenLens[Listing](_.dealer)
  val address = GenLens[Dealer](_.address)
  val street  = GenLens[Address](_.street)
  val streetName = GenLens[Street](_.name)
  
  // Lens Composition
  val streetNameLens = dealer composeLens address composeLens street composeLens streetName

  def composeLensProcessStreet(listing: Listing): Listing = 
    streetNameLens.modify(_.capitalize)(listing)

}


