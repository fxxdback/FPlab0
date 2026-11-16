import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

object Main {

    var configFile = "config.json"
    var config: Option[Config] = None

    def main(args: Array[String]): Unit = {
        if (args.length > 0) {
            configFile = args(0)
        }

        Logger.info("Приложение запущено")
        println("Генератор паролей. Введите help, чтобы увидеть список команд.")
        loadConfig(true)

        var running = true
        lazy val menu: List[MenuEntry] = List(
            MenuEntry("policies", "policies", "показать список политик", _ => showPolicies()),
            MenuEntry("show-policy", "show-policy <имя>", "показать параметры политики", args => showPolicy(args)),
            MenuEntry("generate", "generate [имя] [--length N]", "сгенерировать пароль", args => generate(args)),
            MenuEntry("check", "check [имя]", "проверить пароль", args => check(args)),
            MenuEntry("new-policy", "new-policy", "создать новую политику", _ => newPolicy()),
            MenuEntry("reload", "reload", "перезагрузить конфигурацию", _ => loadConfig(false)),
            MenuEntry("help", "help", "показать эту справку", _ => printHelp(menu)),
            MenuEntry("exit", "exit", "выйти из программы", _ => running = false)
        )

        while (running) {
            print("> ")
            val line = readLine()
            if (line == null) {
                running = false
            } else {
                val parts = line.trim.split("\\s+").toList.filter(s => s.nonEmpty)
                if (parts.nonEmpty) {
                    val original = parts.head
                    val command = original.toLowerCase
                    val arguments = parts.tail

                    menu.find(item => item.name == command) match {
                        case Some(item) =>
                            try {
                                item.action(arguments)
                            } catch {
                                case e: Exception =>
                                    println("Внутренняя ошибка приложения. Команда не выполнена, но программа продолжает работу.")
                                    Logger.error("Внутренняя ошибка при выполнении команды " + command + ": " + e.getClass.getName)
                            }
                        case None => unknownCommand(original)
                    }
                }
            }
        }

        Logger.info("Приложение завершено")
    }


    def pad(s: String, width: Int): String = {
        var result = s
        while (result.length < width) {
            result += " "
        }
        result
    }

    def notLoaded(): Unit = {
        println("Ошибка: конфигурация не загружена.\n")
        println("Исправьте файл " + configFile + " и выполните команду reload.")
        Logger.warn("Команда отклонена: конфигурация не загружена")
    }

    def policyNotFound(c: Config, name: String): Unit = {
        println("Ошибка: политика \"" + name + "\" не найдена.\n")
        println("Доступные политики: " + c.policies.map(p => p.name).mkString(", "))
        Logger.warn("Политика не найдена")
    }


    def unknownCommand(original: String): Unit = {
        println("Неизвестная команда: " + original + ".")
        println("Введите help, чтобы увидеть список команд.")
        if (original.matches("[a-z-]+") && original.length <= 20) {
            Logger.warn("Неизвестная команда: " + original)
        } else {
            Logger.warn("Неизвестная команда: <скрыто>")
        }
    }

    def printHelp(menu: List[MenuEntry]): Unit = {
        println("Доступные команды:\n")
        for (item <- menu) {
            println("  " + pad(item.usage, 32) + item.description)
        }
    }

    def loadConfig(first: Boolean): Unit = {
        val errors = ListBuffer[String]()
        val newConfig = ConfigReader.loadConfig(configFile, errors)

        newConfig match {
            case Some(c) =>
                config = Some(c)
                Logger.fileName = c.logFile
                if (first) Logger.info("Конфигурация загружена") else Logger.info("Конфигурация успешно перезагружена")
                Logger.info("Загружено политик: " + c.policies.size)
                if (first) println("Загружено политик: " + c.policies.size)
                else println("Конфигурация перезагружена. Загружено политик: " + c.policies.size)
            case None =>
                for (e <- errors) {
                    println(e)
                    println()
                    Logger.error("Ошибка конфигурации: " + e)
                }
                if (config.isDefined) {
                    println("Конфигурация не перезагружена, продолжает использоваться прежняя.")
                    Logger.warn("Перезагрузка не выполнена, используется прежняя конфигурация")
                } else {
                    println("Конфигурация не загружена. Исправьте файл " + configFile + " и выполните команду reload.")
                    Logger.error("Конфигурация не загружена")
                }
        }
    }

    def showPolicies(): Unit = {
        config match {
            case None => notLoaded()
            case Some(c) =>
                var nameWidth = "Название".length
                for (p <- c.policies) {
                    if (p.name.length > nameWidth) nameWidth = p.name.length
                }
                nameWidth += 3

                println("Доступные политики:\n")
                println(pad("Название", nameWidth) + pad("Длина", 9) + "Описание")
                for (p <- c.policies) {
                    val lengths = p.minimumLength + "–" + p.maximumLength
                    println(pad(p.name, nameWidth) + pad(lengths, 9) + p.description)
                }
                println("\nПолитика по умолчанию: " + c.defaultPolicy)
                Logger.info("Показан список политик")
        }
    }

    def showPolicy(args: List[String]): Unit = {
        config match {
            case None => notLoaded()
            case Some(c) =>
                if (args.isEmpty) {
                    println("Ошибка: не указано имя политики.\n")
                    println("Использование: show-policy <имя>")
                    println("Доступные политики: " + c.policies.map(p => p.name).mkString(", "))
                    Logger.warn("Некорректные параметры команды show-policy: не указано имя политики")
                } else if (args.length > 1) {
                    println("Ошибка: слишком много параметров.\n")
                    println("Использование: show-policy <имя>")
                    Logger.warn("Некорректные параметры команды show-policy: лишние аргументы")
                } else {
                    c.policies.find(p => p.name == args.head) match {
                        case None => policyNotFound(c, args.head)
                        case Some(p) =>
                            printPolicy(p)
                            Logger.info("Показаны параметры политики: " + p.name)
                    }
                }
        }
    }

    def printPolicy(p: Policy): Unit = {
        println("Политика: " + p.name)
        println("Описание: " + p.description)
        println()
        println("Длина по умолчанию: " + p.defaultLength)
        println("Допустимая длина: от " + p.minimumLength + " до " + p.maximumLength)
        println()
        println("Разрешены:")
        val enabledGroups = p.groups.filter(g => g.enabled)
        var i = 0
        for (g <- enabledGroups) {
            var text = "- " + Groups.title(g.name)
            if (g.name == "special") text += " (" + g.chars + ")"
            if (g.minimumCount > 0) text += ", не менее " + g.minimumCount
            i += 1
            if (i < enabledGroups.length) text += ";" else text += "."
            println(text)
        }
        println()
        if (p.forbiddenCharacters.nonEmpty) {
            println("Запрещённые символы: " + p.forbiddenCharacters.toList.mkString(" "))
        }
        if (p.allowRepeatedCharacters) {
            println("Повторение символов: разрешено")
            if (p.maxConsecutive != Int.MaxValue) {
                println("Одинаковых символов подряд: не более " + p.maxConsecutive)
            }
        } else {
            println("Повторение символов: запрещено")
        }
        if (p.allowSpaces) println("Пробелы: разрешены") else println("Пробелы: запрещены")
        if (p.forbiddenSubstrings.nonEmpty) {
            println("Запрещённые подстроки: " + p.forbiddenSubstrings.mkString(", "))
        }
    }

    def generate(args: List[String]): Unit = {
        config match {
            case None => notLoaded()
            case Some(c) =>
                var policyName = c.defaultPolicy
                var nameGiven = false
                var hasLength = false
                var lengthText = ""
                var badArgs = false
                var i = 0
                while (i < args.length) {
                    if (args(i) == "--length") {
                        if (i + 1 < args.length) {
                            lengthText = args(i + 1)
                            hasLength = true
                            i += 2
                        } else {
                            badArgs = true
                            i += 1
                        }
                    } else if (args(i).startsWith("--")) {
                        badArgs = true
                        i += 1
                    } else {
                        if (nameGiven) badArgs = true
                        policyName = args(i)
                        nameGiven = true
                        i += 1
                    }
                }

                if (badArgs) {
                    println("Ошибка: некорректные параметры команды generate.\n")
                    println("Использование: generate [имя] [--length N]")
                    Logger.warn("Некорректные параметры команды generate")
                } else {
                    c.policies.find(p => p.name == policyName) match {
                        case None => policyNotFound(c, policyName)
                        case Some(p) => doGenerate(p, hasLength, lengthText)
                    }
                }
        }
    }

    def doGenerate(p: Policy, hasLength: Boolean, lengthText: String): Unit = {
        var length = p.defaultLength
        var ok = true

        if (hasLength) {
            try {
                length = lengthText.toInt
            } catch {
                case e: NumberFormatException =>
                    ok = false
                    println("Ошибка: длина \"" + lengthText + "\" не является целым числом.\n")
                    println("Пример:")
                    println("generate " + p.name + " --length " + p.defaultLength)
                    Logger.warn("Некорректные параметры команды generate: длина не является числом")
            }
        }

        if (ok && (length < p.minimumLength || length > p.maximumLength)) {
            ok = false
            println("Ошибка: длина " + length + " не разрешена политикой \"" + p.name + "\".\n")
            println("Допустимая длина: от " + p.minimumLength + " до " + p.maximumLength + ".\n")
            println("Пример:")
            println("generate " + p.name + " --length " + p.defaultLength)
            Logger.warn("Некорректные параметры команды generate: длина вне пределов политики " + p.name)
        }

        if (ok && length > 10000) {
            ok = false
            println("Ошибка: программа не генерирует пароли длиннее 10000 символов.")
            Logger.warn("Некорректные параметры команды generate: слишком большая длина")
        }

        val required = Passwords.requiredTotal(p)
        if (ok && required > length) {
            ok = false
            println("Ошибка: невозможно выполнить требования политики \"" + p.name + "\" при длине " + length + ".\n")
            println("Сумма обязательных символов: " + required)
            println("Запрошенная длина: " + length + "\n")
            println("Увеличьте длину, например: generate " + p.name + " --length " + required)
            Logger.warn("Генерация невозможна: длина меньше суммы обязательных символов, policy=" + p.name)
        }

        val poolSize = Passwords.allAvailable(p).length
        if (ok && !p.allowRepeatedCharacters && poolSize < length) {
            ok = false
            println("Ошибка: повторения запрещены, а разных символов только " + poolSize + ", и пароль длиной " + length + " составить нельзя.\n")
            println("Уменьшите длину (не больше " + poolSize + ") или разрешите повторения в политике.")
            Logger.warn("Генерация невозможна: не хватает разных символов, policy=" + p.name)
        }

        if (ok) {
            Passwords.generate(p, length) match {
                case Some(password) =>
                    println("Политика: " + p.name)
                    println("Длина: " + length)
                    println("Пароль: " + password)
                    Logger.info("Выполнена генерация: policy=" + p.name + ", length=" + length)
                case None =>
                    println("Ошибка: не удалось сгенерировать пароль по политике \"" + p.name + "\".\n")
                    println("Скорее всего, ограничения политики слишком жёсткие. Ослабьте политику или выберите другую длину.")
                    Logger.error("Не удалось сгенерировать пароль: policy=" + p.name)
            }
        }
    }

    def readPassword(): String = {
        val console = System.console()
        if (console != null) {
            val chars = console.readPassword("Введите пароль: ")
            if (chars == null) null else new String(chars)
        } else {
            println("(консоль не поддерживает скрытый ввод, пароль будет виден)")
            print("Введите пароль: ")
            readLine()
        }
    }

    def check(args: List[String]): Unit = {
        config match {
            case None => notLoaded()
            case Some(c) =>
                if (args.length > 1) {
                    println("Ошибка: слишком много параметров.\n")
                    println("Использование: check [имя]")
                    Logger.warn("Некорректные параметры команды check: лишние аргументы")
                } else {
                    val policyName = if (args.isEmpty) c.defaultPolicy else args.head
                    c.policies.find(p => p.name == policyName) match {
                        case None => policyNotFound(c, policyName)
                        case Some(p) =>
                            val password = readPassword()
                            if (password == null) {
                                println("Ввод прерван, проверка не выполнена.")
                                Logger.warn("Проверка не выполнена: ввод пароля прерван")
                            } else {
                                val results = Passwords.check(p, password)
                                println("\nРезультат проверки по политике " + p.name + ":\n")
                                for (r <- results) {
                                    if (r._1) println("[+] " + r._2) else println("[-] " + r._2)
                                }
                                val allOk = results.forall(r => r._1)
                                if (allOk) println("\nПароль соответствует политике " + p.name + ".")
                                else println("\nПароль не соответствует политике " + p.name + ".")
                                Logger.info("Выполнена проверка: policy=" + p.name + ", result=" + (if (allOk) "passed" else "failed"))
                            }
                    }
                }
        }
    }

    def askText(question: String, default: String): String = {
        var shown = ""
        if (default.nonEmpty) shown = " [" + default + "]"
        print(question + shown + ": ")
        val answer = readLine()
        if (answer == null || answer.trim.isEmpty) default else answer.trim
    }

    def askInt(question: String, default: Int): Int = {
        var result = default
        var done = false
        while (!done) {
            val answer = askText(question, default.toString)
            try {
                result = answer.toInt
                done = true
            } catch {
                case e: NumberFormatException => println("Нужно ввести целое число.")
            }
        }
        result
    }

    def askYesNo(question: String, default: Boolean): Boolean = {
        var result = default
        var done = false
        while (!done) {
            val answer = askText(question + " (д/н)", if (default) "д" else "н").toLowerCase
            if (answer == "д" || answer == "да" || answer == "y") {
                result = true
                done = true
            } else if (answer == "н" || answer == "нет" || answer == "n") {
                result = false
                done = true
            } else {
                println("Ответьте д или н.")
            }
        }
        result
    }

    def newPolicy(): Unit = {
        config match {
            case None => notLoaded()
            case Some(c) =>
                println("Создание новой политики. Чтобы отменить, оставьте имя пустым.")
                print("Имя политики: ")
                val nameLine = readLine()
                val name = if (nameLine == null) "" else nameLine.trim

                if (name.isEmpty) {
                    println("Создание политики отменено.")
                    Logger.info("Создание новой политики отменено")
                } else if (name.contains(" ") || name.startsWith("-")) {
                    println("Ошибка: имя не должно содержать пробелов и начинаться с \"-\".")
                } else if (c.policies.exists(p => p.name == name)) {
                    println("Ошибка: политика с именем \"" + name + "\" уже существует.")
                } else {
                    val description = askText("Описание", "")
                    val minLength = askInt("Минимальная длина", 8)
                    val maxLength = askInt("Максимальная длина", 64)
                    val defaultLength = askInt("Длина по умолчанию", 16)

                    val groups = ListBuffer[CharGroup]()
                    for (groupName <- Groups.names) {
                        if (askYesNo("Разрешить " + Groups.title(groupName) + "?", true)) {
                            val count = askInt("  Минимальное количество", 1)
                            var chars = Groups.defaultChars(groupName)
                            var custom = false
                            if (groupName == "special") {
                                val answer = askText("  Набор специальных символов", chars)
                                if (answer != chars) {
                                    chars = answer
                                    custom = true
                                }
                            }
                            groups += CharGroup(groupName, true, count, chars, custom)
                        } else {
                            groups += CharGroup(groupName, false, 0, Groups.defaultChars(groupName), false)
                        }
                    }

                    val forbiddenChars = askText("Запрещённые символы", "")
                    val allowRepeated = askYesNo("Разрешить повторение символов?", true)
                    var maxConsecutive = 1
                    if (allowRepeated) {
                        maxConsecutive = askInt("Максимум одинаковых символов подряд", 2)
                    }
                    val allowSpaces = askYesNo("Разрешить пробелы?", false)
                    val substringsText = askText("Запрещённые подстроки (через запятую)", "")
                    val substrings = substringsText.split(",").toList.map(s => s.trim).filter(s => s.nonEmpty)

                    val policy = Policy(name, description, minLength, maxLength, defaultLength, groups.toList,
                        forbiddenChars, allowRepeated, maxConsecutive, allowSpaces, substrings)

                    val policyErrors = ConfigReader.validatePolicy(policy)
                    if (policyErrors.nonEmpty) {
                        println()
                        for (e <- policyErrors) {
                            println(e)
                            println()
                            Logger.error("Ошибка в новой политике: " + e)
                        }
                        println("Политика не сохранена. Выполните new-policy ещё раз и исправьте параметры.")
                    } else {
                        println()
                        println("Итоговая политика:\n")
                        printPolicy(policy)
                        println()
                        if (askYesNo("Сохранить политику в файл " + configFile + "?", true)) {
                            try {
                                ConfigReader.addPolicyToFile(configFile, policy)
                                Logger.info("Создана новая политика: " + name)
                                println("Политика \"" + name + "\" сохранена.")
                                loadConfig(false) // перечитываем файл
                            } catch {
                                case e: Exception =>
                                    println("Ошибка: не удалось записать политику в файл " + configFile + ".")
                                    Logger.error("Не удалось сохранить новую политику в файл: " + e.getClass.getName)
                            }
                        } else {
                            println("Политика не сохранена.")
                            Logger.info("Новая политика не сохранена по выбору пользователя")
                        }
                    }
                }
        }
    }
}
