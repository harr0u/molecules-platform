package molecule_containers

import calculation.geometry.figures.{Box, Cube, CubicFigure}
import calculation.limit_conditions.LimitConditions

import domain.Particle
import domain.vector.Vector3D
import molecules.{ParticlesAbstractContainer, ParticlesChangeAction, ParticlesSimpleSet}

class ParticlesCells3D(
  private override val particles: Seq[Particle[Vector3D]],
  val limitConditions: LimitConditions[Vector3D, CubicFigure],
  val minimumCellLength: Double = 5.0
) extends ParticlesSimpleSet[Vector3D] (particles) {
  type Particle3D = Particle[Vector3D]
  type Cell = Seq[Particle3D] // Maybe mutable?

  type Row = Vector[Cell]
  type Layer = Vector[Row]
  type Cells = Vector[Layer]

  private val particlesNumber = particles.length

  private def findNumberOfCellsInDirection(directionLength: Double): Int = {
    Math.max(Math.floor(directionLength / minimumCellLength), 1).toInt
  }
  // cells.length
  protected val layersNumber: Int = findNumberOfCellsInDirection(limitConditions.boundaries.height)
  protected val cellHeight: Double = limitConditions.boundaries.height / layersNumber
  // layer.length
  protected val rowsNumber: Int = findNumberOfCellsInDirection(limitConditions.boundaries.length)
  protected val cellLength: Double = limitConditions.boundaries.length / layersNumber

  // row.length
  // TODO stupid name, change it in future
  protected val cellsNumber: Int = findNumberOfCellsInDirection(limitConditions.boundaries.width)
  protected val cellWidth: Double = limitConditions.boundaries.width / layersNumber

  private var emptyCells: Cells = (1 to layersNumber).toVector.map (_ =>
                                    (0 to rowsNumber).toVector.map(_ =>
                                      (0 to cellsNumber).toVector.map(_ =>
                                        Seq[Particle3D]())))

  private val currentCells: Cells = particles.foldLeft(emptyCells)(
    (accCells: Vector[Vector[Vector[Seq[Particle3D]]]], particle: Particle3D) => {
      (for {
        (i, j, k) <- getCellIndexesOfParticle(particle)
      } yield {
        accCells.updated(k,
          accCells(k).updated(j,
            accCells(k)(j).updated(i,
              accCells(k)(j)(i).appended(particle)
            )
          )
        )
      }).getOrElse(accCells)
    }
  )

  private def getCellIndexesOfParticle(particle: Particle3D): Option[(Int, Int, Int)] = {
    val i = Math.floor(particle.position.x / cellWidth).toInt
    val j = Math.floor(particle.position.y / cellLength).toInt
    val k = Math.floor(particle.position.z / cellHeight).toInt

    if ((i >= 0 && i < cellsNumber) && (j >= 0 && j < rowsNumber) && (k >= 0 && k < layersNumber)) {
      Some((i, j, k))
    } else {
      None
    }
  }

  override def particlesStream: LazyList[Particle3D] = currentCells
                                                        .reduce(_ ++ _)
                                                        .reduce(_ ++ _)
                                                        .reduce(_ ++ _)
                                                        .to(LazyList)

  override def applyChangeAction(action: ParticlesChangeAction[Vector3D]): ParticlesAbstractContainer[Vector3D] = {

  }

  def copy(
          particles: Seq[Particle3D] = particles,
          limitConditions: LimitConditions[Vector3D, CubicFigure] = limitConditions,
          currentCells: Cells = currentCells,
          minimumCellLength: Double = minimumCellLength,
          particlesNumber: Int = particlesNumber,
          layersNumber: Int = layersNumber,
          cellHeight: Double = cellHeight,
          rowsNumber: Int = rowsNumber,
          cellLength: Double = cellHeight,
          cellsNumber: Int = cellsNumber,
          cellsWidth: Double = cellWidth,
  ): ParticlesCells3D {
    
  }
}
