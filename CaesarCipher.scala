import scala.io.StdIn

object main extends App {
  print("Enter key: ")
  var k = StdIn.readInt()
  print("Enter text for encryption in LOWER CASE: ")
  var str1 = StdIn.readLine()
  print("Enter text for decryption in UPPER CASE: ")
  var str2 = StdIn.readLine()

  val (encrypted, decrypted) = cipher(str1, str2, k)

  println(s"Encrypted: $encrypted")
  println(s"Decrypted: $decrypted")
}


def encryption(p: Char, k: Int): Char = {
  val alphabetSize = 26
  val encryptedChar = (p - 'a' + k) % alphabetSize + 'a'
  encryptedChar.toChar
}


def decryption(C: Char, k: Int): Char = {
  val alphabetSize = 26
  val decryptedChar = (C.toLower - 'a' - k + alphabetSize) % alphabetSize + 'a'
  decryptedChar.toChar
}


def cipher(str1: String, str2: String, k: Int): (String, String) = {
  val encryptedString = str1.map { char =>
    if (char.isLetter && char.isLower)
      encryption(char, k)
    else
      char
  }.toUpperCase()

  val decryptedString = str2.map { char =>
    if (char.isLetter && char.isUpper)
      decryption(char, k)
    else
      char
  }.toLowerCase()

  (encryptedString, decryptedString)
}
