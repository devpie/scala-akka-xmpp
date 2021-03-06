package xmpp

import org.scalatest.{ Spec, BeforeAndAfterAll }
import org.scalatest.matchers.ShouldMatchers
import akka.actor.Actor._
import akka.util.duration._
import akka.testkit.TestKit
import java.util.concurrent.TimeUnit
import akka.actor.{ ActorRef, Actor }
import org.scalatest.mock.MockitoSugar
import akka.testkit.TestProbe
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import org.scalatest.FunSpec
import akka.dispatch.OnSuccess

class XMPPSpec extends TestKit(ActorSystem()) with ShouldMatchers with FunSpec with MockitoSugar {

  describe("The Chat supervisor") {
    it("should boot with no problems") {
      val jid = JID("scalaxmpptestuser@jabber.ccc.de")
      val chatsupRef = system.actorOf(Props(new ChatSupervisor(jid, "123456")))
      system.stop(chatsupRef)
    }

    it("should create a chatter on request") {
      val jid = JID("scalaxmpptestuser@jabber.ccc.de")
      val me = JID("devpie@jabber.ccc.de")
      val chatsup = system.actorOf(Props(new ChatSupervisor(jid, "123456")))
      implicit val timeout = Timeout(1 second)
      (chatsup ? CreateChat(me)).mapTo[ActorRef] should not { be === None }
      system.stop(chatsup)
    }

    it("should send me a nice message") {
      val jid = JID("scalaxmpptestuser@jabber.ccc.de")
      val me = JID("devpie@jabber.ccc.de")
      val chatsup = system.actorOf(Props(new ChatSupervisor(jid, "123456")))
      implicit val timeout = Timeout(5 seconds)
      val response = (chatsup ? CreateChat(me))
      response.onSuccess[Unit]({
        case chatter: ActorRef =>
          chatter ! OutboundMessage("Hi, you!")
        case _ => fail()
      }).onFailure({
        case t => fail()
      })
      system.stop(chatsup)
    }

    it("should be able to connect to a Google domain") {
      val fakey = JID("text1@fluentsms.com")
      val chatsup = system.actorOf(
        Props(new ChatSupervisor(fakey, "Text1234", Some("talk.google.com"))))
      expectNoMsg(5 seconds)
      system.stop(chatsup)
    }

    // it("should allow me to send it a message") {
    //   val jid = JID("scalaxmpptestuser@jabber.ccc.de")
    //   val me  = JID("troutwine@jabber.org")
    //   val chatsup = actorOf(new ChatSupervisor(jid, "123456")).start
    //   (chatsup !! CreateChat(me)) match {
    //     case Some(chatter:ActorRef) =>
    //       chatter ! RegisterParent(testActor)
    //       chatter ! OutboundMessage("Reply back with 'hi'")
    //       within (10 seconds) {
    //         expectMsg(InboundMessage("hi"))
    //       }
    //     case None => fail()
    //   }
    //   chatsup.stop
    // }
  }

  describe("The JID extractor") {

    it("should extract the full-JID components") {
      val jid = JID("troutwine@jabber.org/helpful")
      jid.username should be === "troutwine"
      jid.domain should be === "jabber.org"
      jid.resource should be === Some("helpful")
    }

    it("should handle certain non-word characters in JIDs") {
      JID("troutwine-foo@jabber.org/wassup")
      JID("troutwine_foo@jabber.org/wassup")
      JID("troutwine.foo@jabber.org/wassup")
    }

    it("should extract partial JID components") {
      val jid = JID("troutwine@jabber.org")
      jid.username should be === "troutwine"
      jid.domain should be === "jabber.org"
      jid.resource should be === None
    }

    it("should convert to implict string") {
      val jid: String = JID("troutwine@jabber.org/helpful")
      jid should be === "troutwine@jabber.org/helpful"
    }

    it("should reject nonsense") {
      intercept[RuntimeException] {
        JID("hihowareyou?")
      }
    }

    it("should provide sane equality") {
      val jid0: JID = JID("troutwine@jabber.org/")
      val jid1: JID = JID("troutwine@jabber.org/")
      jid1 should be === jid0
      val jid2: JID = JID("troutwine@jabber.org/helpful")
      jid1 should not { be === jid2 }
      val jid3: JID = JID("morbo@earth.org")
      jid1 should not { be === jid3 }
      jid0 match {
        case `jid1` =>
        case _ => fail
      }
    }

    it("should enforce an arbitrary ordering") {
      val jid0: JID = JID("troutwine@jabber.org")
      val jid1: JID = JID("barkety@jabber.org")
      val jid2: JID = JID("oliver@jabber.org")
      jid1 should be >= jid0
      jid2 should be >= jid0
      jid1 should be >= jid2
    }
  }

  //  describe("Bot builder") {
  //    import Bot._
  //    import BotDefaults._
  //    import org.mockito.Mockito.verify
  //    
  //    it("should match messages from defined command triggers") {
  //      val ref = mock[ActorRef]
  //      
  //      val command = newRoomBot(ref).newCommand("dostuff", "dostuff help") { (room: ActorRef, args: Array[String]) =>
  //        room should be === ref
  //        args should be === Array("foo", "bar")
  //      }
  //      val boom: Trigger = { case _ => fail(new RuntimeException("didn't match")) }
  //      (command.trigger orElse boom)(ref, "!dostuff foo bar") 
  //      
  //      val alwaysTrue: Trigger = { case _ => 1 should be === 1 }
  //      val nonMatching = newRoomBot(ref).newCommand("wikiwiki", "wikiwiki help") { (room: ActorRef, args: Array[String]) => fail(new RuntimeException("matched but shouldn't")) }
  //      (nonMatching.trigger orElse alwaysTrue)(ref, "!dostuff foo bar")
  //    }
  //    
  //    it("should build room bot with generated help command") {
  //      val room = TestProbe()
  //      val builder = newRoomBot(room.ref)
  //      builder += ("wikiwiki", "wikiwiki help", { (room: ActorRef, args: Array[String]) => () })
  //      val bot = builder.result().start()
  //      room.expectMsgClass((100, TimeUnit.MILLISECONDS), classOf[RegisterParent])
  //      bot ! InboundMessage("!help")
  //      room.expectMsgPF((100, TimeUnit.MILLISECONDS)) {
  //        case help: String =>
  //          help should not be null
  //          help should startWith("help:")
  //          help.contains("wikiwiki: wikiwiki help") should be === true
  //      }
  //    }
  //  }

}

