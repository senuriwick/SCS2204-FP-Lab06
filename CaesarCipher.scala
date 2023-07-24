import scala.io.StdIn

object main extends App {
  print("Enter key: ")
  var k = StdIn.readInt()
  print("Enter text for encryption: ")
  var str1 = StdIn.readLine()
  print("Enter text for decryption: ")
  var str2 = StdIn.readLine()

  val (encrypted, decrypted) = cipher(str1, str2, k)

  println(s"Encrypted: $encrypted")
  println(s"Decrypted: $decrypted")
}


def encryption(p: Char, k: Int): Char = {
  val alphabetSize = 26
  if (p.isLower) {
    val encryptedChar = (p - 'a' + k) % alphabetSize + 'a'
    encryptedChar.toChar
  } 
  else if (p.isUpper) {
    val encryptedChar = (p - 'A' + k) % alphabetSize + 'A'
    encryptedChar.toChar
  } 
  else {
    p    // For non-letter characters, return the character as it is
  }
}


def decryption(C: Char, k: Int): Char = {
  val alphabetSize = 26
  if (C.isLower) {
    val decryptedChar = (C - 'a' - k + alphabetSize) % alphabetSize + 'a'
    decryptedChar.toChar
  } 
  else if (C.isUpper) {
    val decryptedChar = (C - 'A' - k + alphabetSize) % alphabetSize + 'A'
    decryptedChar.toChar
  } 
  else {
    C    // For non-letter characters, return the character as it is
  }
}


def cipher(str1: String, str2: String, k: Int): (String, String) = {
  val encryptedString = str1.map { char =>
    if (char.isLetter) {
      encryption(char, k)
    }
    else {
      char
    }
  }

  val decryptedString = str2.map { char =>
    if (char.isLetter) {
      decryption(char, k)
    }
    else {
      char
    }
  }

  (encryptedString, decryptedString)
}

