package com.guizmaii.zio.htmx.persistence

import zio.json.{DecoderOps, EncoderOps, JsonDecoder, JsonEncoder}
import zio.{Duration, Ref, Task, ZLayer}

import java.time.Instant
import scala.annotation.nowarn

final case class Session(content: String, expiresAt: Instant) {
  def notExpired(now: Instant): Boolean = now.isBefore(expiresAt)

  def decode[A](implicit decoder: JsonDecoder[A]): Either[String, A] = content.fromJson[A]
}
object Session                                                {
  def make[A: JsonEncoder](content: A, expiresIn: Duration): Session =
    Session(
      expiresAt = Instant.now().plusSeconds(expiresIn.toSeconds),
      content = content.toJson,
    )
}

trait SessionIdEncoder[Id] {
  def encode(id: Id): String
}

trait SessionStorage[Id] {
  def store(id: Id, session: Session)(implicit encoder: SessionIdEncoder[Id]): Task[Unit]
  def get(id: Id)(implicit encoder: SessionIdEncoder[Id]): Task[Option[Session]]
  def invalidate(id: Id)(implicit encoder: SessionIdEncoder[Id]): Task[Unit]
}

object SessionStorage {
  @nowarn("msg=is never used")
  def inMemory[Id: zio.Tag]: ZLayer[Any, Nothing, SessionStorage[Id]] =
    ZLayer.scoped {
      for {
        ref <- Ref.Synchronized.make(Map.empty[String, Session])
      } yield new InMemorySessionStorage(ref)
    }
}

final class InMemorySessionStorage[Id](map: Ref[Map[String, Session]]) extends SessionStorage[Id] {
  override def store(id: Id, session: Session)(implicit encoder: SessionIdEncoder[Id]): Task[Unit] =
    map.update(_ + (encoder.encode(id) -> session))

  override def get(id: Id)(implicit encoder: SessionIdEncoder[Id]): Task[Option[Session]] =
    map.get.map(_.get(encoder.encode(id)))

  override def invalidate(id: Id)(implicit encoder: SessionIdEncoder[Id]): Task[Unit] =
    map.update(_ - encoder.encode(id))
}
