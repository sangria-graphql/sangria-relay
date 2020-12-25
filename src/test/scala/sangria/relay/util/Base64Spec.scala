package sangria.relay.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Base64Spec extends AnyWordSpec with Matchers {
  val TestText =
    "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
  val TestBase64 =
    "TG9yZW0gSXBzdW0gaXMgc2ltcGx5IGR1bW15IHRleHQgb2YgdGhlIHByaW50aW5nIGFuZCB0eXBlc2V0dGluZyBpbmR1c3RyeS4gTG9yZW0gSXBzdW0gaGFzIGJlZW4gdGhlIGluZHVzdHJ5J3Mgc3RhbmRhcmQgZHVtbXkgdGV4dCBldmVyIHNpbmNlIHRoZSAxNTAwcywgd2hlbiBhbiB1bmtub3duIHByaW50ZXIgdG9vayBhIGdhbGxleSBvZiB0eXBlIGFuZCBzY3JhbWJsZWQgaXQgdG8gbWFrZSBhIHR5cGUgc3BlY2ltZW4gYm9vay4gSXQgaGFzIHN1cnZpdmVkIG5vdCBvbmx5IGZpdmUgY2VudHVyaWVzLCBidXQgYWxzbyB0aGUgbGVhcCBpbnRvIGVsZWN0cm9uaWMgdHlwZXNldHRpbmcsIHJlbWFpbmluZyBlc3NlbnRpYWxseSB1bmNoYW5nZWQuIEl0IHdhcyBwb3B1bGFyaXNlZCBpbiB0aGUgMTk2MHMgd2l0aCB0aGUgcmVsZWFzZSBvZiBMZXRyYXNldCBzaGVldHMgY29udGFpbmluZyBMb3JlbSBJcHN1bSBwYXNzYWdlcywgYW5kIG1vcmUgcmVjZW50bHkgd2l0aCBkZXNrdG9wIHB1Ymxpc2hpbmcgc29mdHdhcmUgbGlrZSBBbGR1cyBQYWdlTWFrZXIgaW5jbHVkaW5nIHZlcnNpb25zIG9mIExvcmVtIElwc3VtLg=="

  val TestUtf8Text =
    "Lorem Ipsum ist ein einfacher Demo-Text für die Print- und Schriftindustrie. Lorem Ipsum ist in der Industrie bereits der Standard Demo-Text seit 1500, als ein unbekannter Schriftsteller eine Hand voll Wörter nahm und diese durcheinander warf um ein Musterbuch zu erstellen. Es hat nicht nur 5 Jahrhunderte überlebt, sondern auch in Spruch in die elektronische Schriftbearbeitung geschafft (bemerke, nahezu unverändert). Bekannt wurde es 1960, mit dem erscheinen von \"Letraset\", welches Passagen von Lorem Ipsum enhielt, so wie Desktop Software wie \"Aldus PageMaker\" - ebenfalls mit Lorem Ipsum."
  val TestUtf8Base64 =
    "TG9yZW0gSXBzdW0gaXN0IGVpbiBlaW5mYWNoZXIgRGVtby1UZXh0IGbDvHIgZGllIFByaW50LSB1bmQgU2NocmlmdGluZHVzdHJpZS4gTG9yZW0gSXBzdW0gaXN0IGluIGRlciBJbmR1c3RyaWUgYmVyZWl0cyBkZXIgU3RhbmRhcmQgRGVtby1UZXh0IHNlaXQgMTUwMCwgYWxzIGVpbiB1bmJla2FubnRlciBTY2hyaWZ0c3RlbGxlciBlaW5lIEhhbmQgdm9sbCBXw7ZydGVyIG5haG0gdW5kIGRpZXNlIGR1cmNoZWluYW5kZXIgd2FyZiB1bSBlaW4gTXVzdGVyYnVjaCB6dSBlcnN0ZWxsZW4uIEVzIGhhdCBuaWNodCBudXIgNSBKYWhyaHVuZGVydGUgw7xiZXJsZWJ0LCBzb25kZXJuIGF1Y2ggaW4gU3BydWNoIGluIGRpZSBlbGVrdHJvbmlzY2hlIFNjaHJpZnRiZWFyYmVpdHVuZyBnZXNjaGFmZnQgKGJlbWVya2UsIG5haGV6dSB1bnZlcsOkbmRlcnQpLiBCZWthbm50IHd1cmRlIGVzIDE5NjAsIG1pdCBkZW0gZXJzY2hlaW5lbiB2b24gIkxldHJhc2V0Iiwgd2VsY2hlcyBQYXNzYWdlbiB2b24gTG9yZW0gSXBzdW0gZW5oaWVsdCwgc28gd2llIERlc2t0b3AgU29mdHdhcmUgd2llICJBbGR1cyBQYWdlTWFrZXIiIC0gZWJlbmZhbGxzIG1pdCBMb3JlbSBJcHN1bS4="

  "Base64" should {
    "encode string" in {
      Base64.encode(TestText) should be(TestBase64)
    }

    "encode bytes" in {
      Base64.encode(TestUtf8Text.getBytes("UTF-8")) should be(TestUtf8Base64)
    }

    "encode UTF-8 string" in {
      Base64.encode(TestUtf8Text) should be(TestUtf8Base64)
    }

    "decode base64 string" in {
      Base64.decode(TestBase64) should be(Some(TestText))
    }

    "decode UTF-8 base64 string" in {
      Base64.decode(TestBase64) should be(Some(TestText))
    }

    "return an empty string for an empty string" in {
      Base64.decode("") should be(Some(""))
    }

    "return None for base64 strings with to little valid bits" in {
      Base64.decode("a3222==") should be(None)
    }

    "return None for base64 strings with invalid characters" in {
      Base64.decode("foobär23") should be(None)
    }

    "return None for base64 strings with wrong 4-byte ending unit" in {
      Base64.decode("TQ=") should be(None)
    }
  }
}
