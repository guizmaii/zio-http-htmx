package com.guizmaii.zio.htmx.persistence

import zio.{Duration, Ref, Task, ZLayer}

import java.time.Instant
import java.util.UUID

final case class Session(content: String, expiresAt: Instant) {
  def notExpired(now: Instant): Boolean = now.isBefore(expiresAt)
}
object Session                                                {
  def make(content: String, expiresIn: Duration): Session =
    Session(
      expiresAt = Instant.now().plusSeconds(expiresIn.toSeconds),
      content = content,
    )
}

trait SessionStorage {
  def store(id: UUID, session: Session): Task[Unit]
  def get(id: UUID): Task[Option[Session]]
  def invalidate(id: UUID): Task[Unit]
}

object SessionStorage {
  val inMemory: ZLayer[Any, Nothing, SessionStorage] =
    ZLayer.scoped {
      for {
        ref <- Ref.Synchronized.make(Map.empty[UUID, Session])
      } yield new InMemorySessionStorage(ref)
    }
}

final class InMemorySessionStorage(map: Ref[Map[UUID, Session]]) extends SessionStorage {
  override def store(id: UUID, session: Session): Task[Unit] = map.update(_ + (id -> session))
  override def get(id: UUID): Task[Option[Session]]          = map.get.map(_.get(id))
  override def invalidate(id: UUID): Task[Unit]              = map.update(_ - id)
}
