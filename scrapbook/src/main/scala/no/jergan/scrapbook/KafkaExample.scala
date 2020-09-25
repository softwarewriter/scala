package no.jergan.scrapbook

import java.util.Properties

import org.apache.kafka.common.serialization.{Serde, Serdes}
import org.apache.kafka.streams.KafkaStreams
import org.apache.kafka.streams.StreamsConfig._
import org.apache.kafka.streams.kstream.{KStream, KStreamBuilder, Predicate}

/**
 * Kafka streams example.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */

object KafkaExample extends App {

  val streamsConfiguration = new Properties()
  streamsConfiguration.put(APPLICATION_ID_CONFIG, "Streaming-QuickStart")
  streamsConfiguration.put(BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
  streamsConfiguration.put(DEFAULT_KEY_SERDE_CLASS_CONFIG, Serdes.String.getClass.getName)
  streamsConfiguration.put(DEFAULT_VALUE_SERDE_CLASS_CONFIG, Serdes.String.getClass.getName)

  val builder = new KStreamBuilder

  val kStream = builder.stream("InTopic")

  val upperCaseKStream = kStream.filter(new Predicate[String, String] {
    override def test(key: String, value: String): Boolean = key == value
  })

  //characters of values are now converted to upper case
  upperCaseKStream.to("OutTopic")

  //sending data to out topic
  val stream = new KafkaStreams(builder, streamsConfiguration)

  stream.start()


}
