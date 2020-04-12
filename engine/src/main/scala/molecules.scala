import molecules.kafka
import molecules.kafka.ProducerKafka

object Molecules extends App {
  ProducerKafka.writeToKafka("test")
}