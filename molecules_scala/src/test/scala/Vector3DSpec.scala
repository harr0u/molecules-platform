import org.scalatest._
import domain.Particle
import domain.geometry.vector._

class Vector3DSpec extends FlatSpec with Matchers {

  "A Vector3D" should "put 0.0 instead of empty coorinate(s)" in {
    val zeroVector = new Vector3D()
    assert(zeroVector.x == 0.0)
    assert(zeroVector.y == 0.0)

    val oneEmptyCoordinateVector = new Vector3D(10, 20)
    assert(oneEmptyCoordinateVector.x == 10)
    assert(oneEmptyCoordinateVector.y == 20)
    assert(oneEmptyCoordinateVector.z == 0)
  }

  it should "scale with numeric factor" in {
    val vector = new Vector3D(2, 3, 4)
    val scaledVector = vector.scale(10)

    assert(scaledVector.x == 20)
    assert(scaledVector.y == 30)
    assert(scaledVector.z == 40)
  }

  it should "have short notation for scaling" in {
    val vector = new Vector3D(2, 3, 4)
    val scaledVector = vector * 10

    assert(scaledVector.x == 20)
    assert(scaledVector.y == 30)
    assert(scaledVector.z == 40)
  }

  it should "have short notation for right-hand operation of scaling" in {
    val vector = new Vector3D(2, 3, 4)
    val scaledVector = 10 *: vector

    assert(scaledVector.x == 20)
    assert(scaledVector.y == 30)
    assert(scaledVector.z == 40)
  }

  it should "compute its own norm (length)" in {
    val vector = new Vector3D(1, 2, 2)

    assert(vector.length == 3)
  }


  "Two Vector3D objects" should "return new vector when added" in {
    val vector1 = new Vector3D(2, 3, 8)
    val vector2 = new Vector3D(1, 5, 1)

    val resultOfVectorSum = vector1.addVector(vector2)

    assert(resultOfVectorSum.x == 3)
    assert(resultOfVectorSum.y == 8)
    assert(resultOfVectorSum.z == 9)
  }

  it should "have short notation for adding" in {
    val vector1 = new Vector3D(2, 3, 8)
    val vector2 = new Vector3D(1, 5, 1)

    val resultOfVectorSum = vector1 + vector2

    assert(resultOfVectorSum.x == 3)
    assert(resultOfVectorSum.y == 8)
    assert(resultOfVectorSum.z == 9)
  }

  it should "return new vector when substracted" in {
    val vector1 = new Vector3D(2, 3, 8)
    val vector2 = new Vector3D(1, 5, 1)

    val resultOfVectorSum = vector1.subVector(vector2)

    assert(resultOfVectorSum.x == 1)
    assert(resultOfVectorSum.y == -2)
    assert(resultOfVectorSum.z == 7)
  }

  it should "have short notation for substracting" in {
    val vector1 = new Vector3D(2, 3, 8)
    val vector2 = new Vector3D(1, 5, 1)

    val resultOfVectorSum = vector1 - vector2

    assert(resultOfVectorSum.x == 1)
    assert(resultOfVectorSum.y == -2)
    assert(resultOfVectorSum.z == 7)
  }

  "A Vector3D.empty" should "return vector with 0.0 coorinates" in {
    val vector = Vector3D.empty

    assert(vector.x == 0.0)
    assert(vector.y == 0.0)
    assert(vector.z == 0.0)
  }
}