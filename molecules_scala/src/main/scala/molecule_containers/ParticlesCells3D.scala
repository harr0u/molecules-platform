package molecule_containers

import collection.Seq
import domain.Particle
import domain.vector.Vector3D
import molecules.ParticlesAbstractContainer

abstract class ParticlesCells3D(val particles: Seq[Particle[Vector3D]]) extends ParticlesAbstractContainer[Vector3D]{
  type Cell = Seq[Particle[Vector3D]] // Maybe mutable?

  type Row = Seq[Cell]
  type Layer = Seq[Row]
  type Cells = Seq[Layer]

  private val particlesNumber = particles.length

  // To find it need data about Box (width, height, length)
  protected val layersNumber: Int = 1 // cells.length
  protected val rowsNumber: Int = 1 // layer.length
  protected val cellsNumber: Int = 1 // row.length

  private val emptyCells: Cells = Seq(1 to layersNumber).map (_ =>
                                    Seq(1 to rowsNumber).map(_ =>
                                      Seq(1 to cellsNumber).map(_ =>
                                        Seq[Particle[Vector3D]]())))

  // State monad?
  private var currentCells: Cells = emptyCells
  def getCell(layerId: Int, rowId: Int, columnId: Int): Cell = currentCells(layerId)(rowId)(columnId)

  def particlesStream: LazyList[Particle[Vector3D]] = currentCells
                                                        .reduce(_ ++ _)
                                                        .reduce(_ ++ _)
                                                        .reduce(_ ++ _)
                                                        .to(LazyList)

  def particlePairsStream: LazyList[(Particle[Vector3D], Particle[Vector3D])] = {
    for {
      (particle1, i) <- particlesStream.zipWithIndex
      particle2 <- particlesStream.takeRight(particlesNumber - i - 1)
    } yield (particle1, particle2)
  }
}
