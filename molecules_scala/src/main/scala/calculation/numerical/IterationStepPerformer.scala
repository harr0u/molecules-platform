package calculation.numerical

abstract class IterationStepPerformer {
    def init(): Unit
    def iterationStep(): Unit
    def dispose(): Unit
}
