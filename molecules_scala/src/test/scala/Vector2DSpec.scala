import org.scalatest._
import domain.Particle
import domain.vector._

class Vector2DSpec extends FlatSpec with Matchers {

  "A Vector2D" should "put 0.0 instead of empty coorinate(s)" in {
    val zeroVector = Vector2D()
    assert(zeroVector.x == 0.0)
    assert(zeroVector.y == 0.0)

    val oneEmptyCoordinateVector = Vector2D(10, 20)
    assert(oneEmptyCoordinateVector.x == 10)
    assert(oneEmptyCoordinateVector.y == 20)
  }

  it should "scale with numeric factor" in {
    val vector = Vector2D(2, 3)
    val scaledVector = vector.scale(20)

    assert(scaledVector.x == 40)
    assert(scaledVector.y == 60)
  }

  it should "have short notation for scaling" in {
    val vector = Vector2D(2, 3)
    val scaledVector = vector * 10

    assert(scaledVector.x == 20)
    assert(scaledVector.y == 30)
  }

  it should "have short notation for right-hand operation of scaling" in {
    val vector = Vector2D(2, 3)
    val scaledVector = 10 *: vector

    assert(scaledVector.x == 20)
    assert(scaledVector.y == 30)
  }

  it should "compute its own norm (length)" in {
    val vector = Vector2D(6, 8)

    assert(vector.length == 10)
  }


  "Two Vector2D objects" should "return new vector when added" in {
    val vector1 = Vector2D(2, 3)
    val vector2 = Vector2D(1, 5)

    val resultOfVectorSum = vector1.addVector(vector2)

    assert(resultOfVectorSum.x == 3)
    assert(resultOfVectorSum.y == 8)
  }

  it should "have short notation for adding" in {
    val vector1 = Vector2D(2, 3)
    val vector2 = Vector2D(1, 5)

    val resultOfVectorSum = vector1 + vector2

    assert(resultOfVectorSum.x == 3)
    assert(resultOfVectorSum.y == 8)
  }

  it should "return new vector when substracted" in {
    val vector1 = Vector2D(2, 3)
    val vector2 = Vector2D(1, 5)

    val resultOfVectorSum = vector1.subVector(vector2)

    assert(resultOfVectorSum.x == 1)
    assert(resultOfVectorSum.y == -2)
  }

  it should "have short notation for substracting" in {
    val vector1 = Vector2D(2, 3)
    val vector2 = Vector2D(1, 5)

    val resultOfVectorSum = vector1 - vector2

    assert(resultOfVectorSum.x == 1)
    assert(resultOfVectorSum.y == -2)
  }

  "A Vector2D.empty" should "return vector with 0.0 coorinates" in {
    val vector = Vector2D.empty

    assert(vector.x == 0.0)
    assert(vector.y == 0.0)
  }
}