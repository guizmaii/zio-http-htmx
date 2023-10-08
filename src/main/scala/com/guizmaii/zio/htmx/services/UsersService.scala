package com.guizmaii.zio.htmx.services

import com.guizmaii.zio.htmx.domain.User
import zio.{ULayer, ZLayer}

trait UsersService {
  def find(filter: Option[String]): List[User]
}

object UsersService {
  val live: ULayer[UserServiceLive] = ZLayer.succeed(new UserServiceLive)
}

final class UserServiceLive extends UsersService {

  private val users =
    List(
      User("Venus", "Grimes", "lectus.rutrum@Duisa.edu"),
      User("Fletcher", "Owen", "metus@Aenean.org"),
      User("William", "Hale", "eu.dolor@risusodio.edu"),
      User("TaShya", "Cash", "tincidunt.orci.quis@nuncnullavulputate.co.uk"),
      User("Kevyn", "Hoover", "tristique.pellentesque.tellus@Cumsociis.co.uk"),
      User("Jakeem", "Walker", "Morbi.vehicula.Pellentesque@faucibusorci.org"),
      User("Malcolm", "Trujillo", "sagittis@velit.edu"),
      User("Wynne", "Rice", "augue.id@felisorciadipiscing.edu"),
      User("Evangeline", "Klein", "adipiscing.lobortis@sem.org"),
      User("Jennifer", "Russell", "sapien.Aenean.massa@risus.com"),
      User("Rama", "Freeman", "Proin@quamPellentesquehabitant.net"),
      User("Jena", "Mathis", "non.cursus.non@Phaselluselit.com"),
      User("Alexandra", "Maynard", "porta.elit.a@anequeNullam.ca"),
      User("Tallulah", "Haley", "ligula@id.net"),
      User("Timon", "Small", "velit.Quisque.varius@gravidaPraesent.org"),
      User("Randall", "Pena", "facilisis@Donecconsectetuer.edu"),
      User("Conan", "Vaughan", "luctus.sit@Classaptenttaciti.edu"),
      User("Dora", "Allen", "est.arcu.ac@Vestibulumante.co.uk"),
      User("Aiko", "Little", "quam.dignissim@convallisest.net"),
      User("Jessamine", "Bauer", "taciti.sociosqu@nibhvulputatemauris.co.uk"),
      User("Gillian", "Livingston", "justo@atiaculisquis.com"),
      User("Laith", "Nicholson", "elit.pellentesque.a@diam.org"),
      User("Paloma", "Alston", "cursus@metus.org"),
      User("Freya", "Dunn", "Vestibulum.accumsan@metus.co.uk"),
      User("Griffin", "Rice", "justo@tortordictumeu.net"),
      User("Catherine", "West", "malesuada.augue@elementum.com"),
      User("Jena", "Chambers", "erat.Etiam.vestibulum@quamelementumat.net"),
      User("Neil", "Rodriguez", "enim@facilisis.com"),
      User("Freya", "Charles", "metus@nec.net"),
      User("Anastasia", "Strong", "sit@vitae.edu"),
      User("Bell", "Simon", "mollis.nec.cursus@disparturientmontes.ca"),
      User("Minerva", "Allison", "Donec@nequeIn.edu"),
      User("Yoko", "Dawson", "neque.sed@semper.net"),
      User("Nadine", "Justice", "netus@et.edu"),
      User("Hoyt", "Rosa", "Nullam.ut.nisi@Aliquam.co.uk"),
      User("Shafira", "Noel", "tincidunt.nunc@non.edu"),
      User("Jin", "Nunez", "porttitor.tellus.non@venenatisamagna.net"),
      User("Barbara", "Gay", "est.congue.a@elit.com"),
      User("Riley", "Hammond", "tempor.diam@sodalesnisi.net"),
      User("Molly", "Fulton", "semper@Naminterdumenim.net"),
      User("Dexter", "Owen", "non.ante@odiosagittissemper.ca"),
      User("Kuame", "Merritt", "ornare.placerat.orci@nisinibh.ca"),
      User("Maggie", "Delgado", "Nam.ligula.elit@Cum.org"),
      User("Hanae", "Washington", "nec.euismod@adipiscingelit.org"),
      User("Jonah", "Cherry", "ridiculus.mus.Proin@quispede.edu"),
      User("Cheyenne", "Munoz", "at@molestiesodalesMauris.edu"),
      User("India", "Mack", "sem.mollis@Inmi.co.uk"),
      User("Lael", "Mcneil", "porttitor@risusDonecegestas.com"),
      User("Jillian", "Mckay", "vulputate.eu.odio@amagnaLorem.co.uk"),
      User("Shaine", "Wright", "malesuada@pharetraQuisqueac.org"),
      User("Keane", "Richmond", "nostra.per.inceptos@euismodurna.org"),
      User("Samuel", "Davis", "felis@euenim.com"),
      User("Zelenia", "Sheppard", "Quisque.nonummy@antelectusconvallis.org"),
      User("Giacomo", "Cole", "aliquet.libero@urnaUttincidunt.ca"),
      User("Mason", "Hinton", "est@Nunc.co.uk"),
      User("Katelyn", "Koch", "velit.Aliquam@Suspendisse.edu"),
      User("Olga", "Spencer", "faucibus@Praesenteudui.net"),
      User("Erasmus", "Strong", "dignissim.lacus@euarcu.net"),
      User("Regan", "Cline", "vitae.erat.vel@lacusEtiambibendum.co.uk"),
      User("Stone", "Holt", "eget.mollis.lectus@Aeneanegestas.ca"),
      User("Deanna", "Branch", "turpis@estMauris.net"),
      User("Rana", "Green", "metus@conguea.edu"),
      User("Caryn", "Henson", "Donec.sollicitudin.adipiscing@sed.net"),
      User("Clarke", "Stein", "nec@mollis.co.uk"),
      User("Kelsie", "Porter", "Cum@gravidaAliquam.com"),
      User("Cooper", "Pugh", "Quisque.ornare.tortor@dictum.co.uk"),
      User("Paul", "Spencer", "ac@InfaucibusMorbi.com"),
      User("Cassady", "Farrell", "Suspendisse.non@venenatisa.net"),
      User("Sydnee", "Velazquez", "mollis@loremfringillaornare.com"),
      User("Felix", "Boyle", "id.libero.Donec@aauctor.org"),
      User("Ryder", "House", "molestie@natoquepenatibus.org"),
      User("Hadley", "Holcomb", "penatibus@nisi.ca"),
      User("Marsden", "Nunez", "Nulla.eget.metus@facilisisvitaeorci.org"),
      User("Alana", "Powell", "non.lobortis.quis@interdumfeugiatSed.net"),
      User("Dennis", "Wyatt", "Morbi.non@nibhQuisquenonummy.ca"),
      User("Karleigh", "Walton", "nascetur.ridiculus@quamdignissimpharetra.com"),
      User("Brielle", "Donovan", "placerat@at.edu"),
      User("Donna", "Dickerson", "lacus.pede.sagittis@lacusvestibulum.com"),
      User("Eagan", "Pate", "est.Nunc@cursusNunc.ca"),
      User("Carlos", "Ramsey", "est.ac.facilisis@duinec.co.uk"),
      User("Regan", "Murphy", "lectus.Cum@aptent.com"),
      User("Claudia", "Spence", "Nunc.lectus.pede@aceleifend.co.uk"),
      User("Genevieve", "Parker", "ultrices@inaliquetlobortis.net"),
      User("Marshall", "Allison", "erat.semper.rutrum@odio.org"),
      User("Reuben", "Davis", "Donec@auctorodio.edu"),
      User("Ralph", "Doyle", "pede.Suspendisse.dui@Curabitur.org"),
      User("Constance", "Gilliam", "mollis@Nulla.edu"),
      User("Serina", "Jacobson", "dictum.augue@ipsum.net"),
      User("Charity", "Byrd", "convallis.ante.lectus@scelerisquemollisPhasellus.co.uk"),
      User("Hyatt", "Bird", "enim.Nunc.ut@nonmagnaNam.com"),
      User("Brent", "Dunn", "ac.sem@nuncid.com"),
      User("Casey", "Bonner", "id@ornareelitelit.edu"),
      User("Hakeem", "Gill", "dis@nonummyipsumnon.org"),
      User("Stewart", "Meadows", "Nunc.pulvinar.arcu@convallisdolorQuisque.net"),
      User("Nomlanga", "Wooten", "inceptos@turpisegestas.ca"),
      User("Sebastian", "Watts", "Sed.diam.lorem@lorem.co.uk"),
      User("Chelsea", "Larsen", "ligula@Nam.net"),
      User("Cameron", "Humphrey", "placerat@id.org"),
      User("Juliet", "Bush", "consectetuer.euismod@vitaeeratVivamus.co.uk"),
      User("Caryn", "Hooper", "eu.enim.Etiam@ridiculus.org"),
    )

  override def find(filter: Option[String]): List[User] =
    filter match {
      case None    => users
      case Some(v) =>
        users.filter { user =>
          user.firstName.toLowerCase.contains(v.toLowerCase) ||
          user.lastName.toLowerCase.contains(v.toLowerCase) ||
          user.email.toLowerCase.contains(v.toLowerCase)
        }
    }
}
