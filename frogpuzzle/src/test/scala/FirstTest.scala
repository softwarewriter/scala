import org.scalatest.flatspec.AnyFlatSpec

class FirstTest extends AnyFlatSpec {

    behavior of "An empty Set"

    it should "have size 0" in {
        assert(Set.empty.size === 0)
    }

    it should "produce NoSuchElementException when head is invoked" in {
        assertThrows[NoSuchElementException] {
            Set.empty.head
        }
    }

    it should "be cool" in {
        assert(true)
    }

    it should "be able to find my other scala code" in {
        assert(Pelle.m.equals("hei"))
    }

    //    println("pelle er best");
//    assert(true);
//    assert(Pelle.m.equals("nei"))


}
