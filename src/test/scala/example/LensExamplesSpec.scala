package example

import org.scalatest._
import LensExamples._

class LensExamplesSpec extends FlatSpec with Matchers {

  val streetCapitalizedListing = Listing("1", Dealer("1", Address(Street("Bothestraße", 3)))) 

  "Naive process street" should "capitalize the street" in {
    naiveProcessStreet(listing) shouldEqual streetCapitalizedListing 
  }

  "Primitive lens version" should "capitalize the street" in {
    primitiveLensProcessStreet(listing) shouldEqual streetCapitalizedListing
  }

  "Compose lens version" should "capitalize the street" in {
    composeLensProcessStreet(listing) shouldEqual streetCapitalizedListing
  }

  "Get" should "zoom into dealer id" in {
    getId(listing) shouldEqual "1"
  }

  "Set" should "set the dealer id" in {
    setId(listing, "42") shouldEqual Listing("42", Dealer("1", Address(Street("bothestraße", 3)))) 
 
  }

  "Add prefix" should "add p before listing id" in {
       addPrefix(listing) shouldEqual Listing("p1", Dealer("1", Address(Street("bothestraße", 3)))) 
 
  }

  "Lens getSet Law" should "hold" in {
    import LensLaws._
    getSet(streetNameLens, listing) shouldEqual true
    setGet(streetNameLens, listing, "abc") shouldEqual true
  }
}
