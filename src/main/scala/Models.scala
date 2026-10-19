case class CharGroup(
                      name: String,
                      enabled: Boolean,
                      minimumCount: Int,
                      chars: String,
                      customChars: Boolean
                    )

case class Policy(
                   name: String,
                   description: String,
                   minimumLength: Int,
                   maximumLength: Int,
                   defaultLength: Int,
                   groups: List[CharGroup],
                   forbiddenCharacters: String,
                   allowRepeatedCharacters: Boolean,
                   maxConsecutive: Int,
                   allowSpaces: Boolean,
                   forbiddenSubstrings: List[String]
                 )

case class Config(defaultPolicy: String, logFile: String, policies: List[Policy])

class ConfigError(message: String) extends Exception(message)

object Groups {
    val names = List("lowercase", "uppercase", "digits", "special")

    def defaultChars(name: String): String = name match {
        case "lowercase" => "abcdefghijklmnopqrstuvwxyz"
        case "uppercase" => "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        case "digits" => "0123456789"
        case _ => "!@#$%^&*_-+="
    }

    def title(name: String): String = name match {
        case "lowercase" => "строчные буквы"
        case "uppercase" => "прописные буквы"
        case "digits" => "цифры"
        case _ => "специальные символы"
    }

    def has(name: String): String = name match {
        case "lowercase" => "строчную букву"
        case "uppercase" => "прописную букву"
        case "digits" => "цифру"
        case _ => "специальный символ"
    }

    def hasNot(name: String): String = name match {
        case "lowercase" => "строчной буквы"
        case "uppercase" => "прописной буквы"
        case "digits" => "цифры"
        case _ => "специального символа"
    }
}
