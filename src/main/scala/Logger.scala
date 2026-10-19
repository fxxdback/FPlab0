import java.nio.file.{Files, Paths, StandardOpenOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object Logger {
    var fileName = "password-generator.log"
    var warned = false

    def log(level: String, message: String): Unit = {
        val time = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))

        val oneLine = message.replace("\r", " ").replace("\n", " ")
        val line = time + " " + level + " " + oneLine + "\n"
        try {

            Files.write(Paths.get(fileName), line.getBytes("UTF-8"), StandardOpenOption.CREATE, StandardOpenOption.APPEND)
        } catch {
            case e: Exception =>
                if (!warned) {
                    println("Предупреждение: не удалось записать в журнал " + fileName + ". Работа продолжается без журнала.")
                    warned = true
                }
        }
    }

    def info(message: String): Unit = log("INFO ", message)

    def warn(message: String): Unit = log("WARN ", message)

    def error(message: String): Unit = log("ERROR", message)
}
