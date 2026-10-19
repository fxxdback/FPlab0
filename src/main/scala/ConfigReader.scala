import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer

object ConfigReader {

    def problem(policy: String, param: String, reason: String, hint: String): String = {
        "Ошибка в политике \"" + policy + "\".\n\n" +
          "Параметр: " + param + "\n" +
          "Причина: " + reason + "\n\n" +
          hint
    }

    def readJson(fileName: String): ujson.Value = {
        val path = Paths.get(fileName)
        if (!Files.exists(path)) {
            throw new ConfigError("Ошибка: файл конфигурации \"" + fileName + "\" не найден.\n\n" +
              "Положите файл в папку, из которой запускается программа, или укажите путь при запуске.")
        }

        var text = ""
        try {
            text = new String(Files.readAllBytes(path), "UTF-8")
        } catch {
            case e: java.io.IOException =>
                throw new ConfigError("Ошибка: не удалось прочитать файл \"" + fileName + "\": " + e.getMessage)
        }

        text = text.stripPrefix("\uFEFF")

        try {
            ujson.read(text)
        } catch {
            case e: Exception =>
                throw new ConfigError("Ошибка: файл \"" + fileName + "\" имеет неправильный формат JSON.\n\n" +
                  "Подробности: " + e.getMessage + "\n\n" +
                  "Проверьте скобки, запятые и кавычки.")
        }
    }

    def loadConfig(fileName: String, errors: ListBuffer[String]): Option[Config] = {
        try {
            val json = readJson(fileName)

            val root = json match {
                case o: ujson.Obj => o
                case _ => throw new ConfigError("Ошибка: в файле конфигурации должен быть JSON-объект { ... }.")
            }

            var defaultName = ""
            root.value.get("defaultPolicy") match {
                case Some(ujson.Str(s)) => defaultName = s
                case _ => errors += "Ошибка: в конфигурации не указана политика по умолчанию.\n\n" +
                  "Добавьте параметр \"defaultPolicy\" с именем одной из политик."
            }

            var logFile = "password-generator.log"
            root.value.get("logFile") match {
                case Some(ujson.Str(s)) => logFile = s
                case _ => ()
            }

            val policiesJson = root.value.get("policies") match {
                case Some(o: ujson.Obj) => o
                case _ => throw new ConfigError("Ошибка: в конфигурации нет раздела \"policies\".")
            }
            if (policiesJson.value.isEmpty) {
                throw new ConfigError("Ошибка: в конфигурации нет ни одной политики.\n\n" +
                  "Добавьте хотя бы одну политику в раздел \"policies\".")
            }

            val policies = ListBuffer[Policy]()
            for ((name, value) <- policiesJson.value) {
                try {
                    val policy = readPolicy(name, value)
                    val policyErrors = validatePolicy(policy)
                    if (policyErrors.isEmpty) {
                        policies += policy
                    } else {
                        errors ++= policyErrors
                    }
                } catch {
                    case e: ConfigError => errors += e.getMessage
                }
            }

            if (defaultName != "" && !policiesJson.value.contains(defaultName)) {
                errors += "Ошибка: политика по умолчанию \"" + defaultName + "\" не найдена в разделе \"policies\".\n\n" +
                  "Укажите в defaultPolicy одну из существующих политик."
            }

            if (errors.isEmpty) Some(Config(defaultName, logFile, policies.toList)) else None
        } catch {
            case e: ConfigError =>
                errors += e.getMessage
                None
        }
    }

    def need(obj: ujson.Obj, key: String, policy: String): ujson.Value = {
        obj.value.get(key) match {
            case Some(v) => v
            case None => throw new ConfigError(problem(policy, key, "обязательный параметр не задан.",
                "Добавьте параметр " + key + " в описание политики."))
        }
    }

    def asInt(v: ujson.Value, policy: String, key: String): Int = v match {
        case ujson.Num(d) if d == d.toInt => d.toInt
        case _ => throw new ConfigError(problem(policy, key, "ожидалось целое число.", "Укажите целое число, например 12."))
    }

    def asBool(v: ujson.Value, policy: String, key: String): Boolean = v match {
        case ujson.True => true
        case ujson.False => false
        case _ => throw new ConfigError(problem(policy, key, "ожидалось true или false.", "Напишите true или false без кавычек."))
    }

    def asString(v: ujson.Value, policy: String, key: String): String = v match {
        case ujson.Str(s) => s
        case _ => throw new ConfigError(problem(policy, key, "ожидалась строка.", "Возьмите значение в кавычки."))
    }

    def asStringList(v: ujson.Value, policy: String, key: String): List[String] = v match {
        case ujson.Arr(items) =>
            val result = ListBuffer[String]()
            for (item <- items) {
                result += asString(item, policy, key)
            }
            result.toList
        case _ => throw new ConfigError(problem(policy, key, "ожидался список строк.", "Пример: [\"admin\", \"qwerty\"]"))
    }

    def readGroup(policy: String, groupsObj: ujson.Obj, groupName: String): CharGroup = {
        val defaults = Groups.defaultChars(groupName)
        val where = "characterGroups." + groupName
        groupsObj.value.get(groupName) match {
            case None =>
                CharGroup(groupName, false, 0, defaults, false)
            case Some(g: ujson.Obj) =>
                val enabled = g.value.get("enabled") match {
                    case Some(v) => asBool(v, policy, where + ".enabled")
                    case None => true
                }
                val minCount = g.value.get("minimumCount") match {
                    case Some(v) => asInt(v, policy, where + ".minimumCount")
                    case None => 0
                }

                g.value.get("characters") match {
                    case Some(v) if groupName == "special" =>
                        CharGroup(groupName, enabled, minCount, asString(v, policy, where + ".characters"), true)
                    case _ =>
                        CharGroup(groupName, enabled, minCount, defaults, false)
                }
            case Some(_) =>
                throw new ConfigError(problem(policy, where, "описание группы должно быть объектом { ... }.",
                    "Напишите параметры группы в фигурных скобках."))
        }
    }

    def readPolicy(name: String, json: ujson.Value): Policy = {
        val obj = json match {
            case o: ujson.Obj => o
            case _ => throw new ConfigError(problem(name, "(вся политика)", "политика должна быть объектом { ... }.",
                "Опишите политику в фигурных скобках."))
        }

        val description = obj.value.get("description") match {
            case Some(v) => asString(v, name, "description")
            case None => ""
        }
        val minLength = asInt(need(obj, "minimumLength", name), name, "minimumLength")
        val maxLength = asInt(need(obj, "maximumLength", name), name, "maximumLength")
        val defaultLength = asInt(need(obj, "defaultLength", name), name, "defaultLength")

        val groupsObj = need(obj, "characterGroups", name) match {
            case o: ujson.Obj => o
            case _ => throw new ConfigError(problem(name, "characterGroups", "ожидался объект { ... }.",
                "Опишите группы символов в фигурных скобках."))
        }
        val groups = Groups.names.map(groupName => readGroup(name, groupsObj, groupName))

        val forbiddenChars = obj.value.get("forbiddenCharacters") match {
            case Some(v) => asString(v, name, "forbiddenCharacters")
            case None => ""
        }
        val allowRepeated = obj.value.get("allowRepeatedCharacters") match {
            case Some(v) => asBool(v, name, "allowRepeatedCharacters")
            case None => true
        }

        val maxConsecutive = obj.value.get("maximumConsecutiveIdenticalCharacters") match {
            case Some(v) => asInt(v, name, "maximumConsecutiveIdenticalCharacters")
            case None => Int.MaxValue
        }
        val allowSpaces = obj.value.get("allowSpaces") match {
            case Some(v) => asBool(v, name, "allowSpaces")
            case None => false
        }
        val substrings = obj.value.get("forbiddenSubstrings") match {
            case Some(v) => asStringList(v, name, "forbiddenSubstrings")
            case None => List()
        }

        Policy(name, description, minLength, maxLength, defaultLength, groups,
            forbiddenChars, allowRepeated, maxConsecutive, allowSpaces, substrings)
    }

    def validatePolicy(p: Policy): List[String] = {
        val errors = ListBuffer[String]()
        val enabledGroups = p.groups.filter(g => g.enabled)
        val forbidden = p.forbiddenCharacters.toSet

        if (p.minimumLength < 1) {
            errors += problem(p.name, "minimumLength", "минимальная длина не может быть меньше 1.",
                "Укажите minimumLength не меньше 1.")
        }

        if (p.maximumLength < p.minimumLength) {
            errors += problem(p.name, "maximumLength",
                "максимальная длина (" + p.maximumLength + ") меньше минимальной (" + p.minimumLength + ").",
                "Увеличьте maximumLength или уменьшите minimumLength.")
        }

        if (p.defaultLength < p.minimumLength || p.defaultLength > p.maximumLength) {
            errors += problem(p.name, "defaultLength",
                "длина по умолчанию (" + p.defaultLength + ") не входит в диапазон от " + p.minimumLength + " до " + p.maximumLength + ".",
                "Укажите defaultLength в диапазоне от " + p.minimumLength + " до " + p.maximumLength + ".")
        }

        if (enabledGroups.isEmpty) {
            errors += problem(p.name, "characterGroups", "не включена ни одна группа символов.",
                "Поставьте \"enabled\": true хотя бы для одной группы.")
        }

        for (g <- p.groups) {
            if (g.minimumCount < 0) {
                errors += problem(p.name, "characterGroups." + g.name + ".minimumCount",
                    "минимальное количество символов не может быть отрицательным (сейчас " + g.minimumCount + ").",
                    "Укажите 0 или положительное число.")
            }
            if (!g.enabled && g.minimumCount > 0) {
                errors += problem(p.name, "characterGroups." + g.name + ".minimumCount",
                    "группа " + g.name + " выключена, но для неё указано минимальное количество " + g.minimumCount + ".",
                    "Включите группу (\"enabled\": true) или поставьте minimumCount: 0.")
            }
        }

        var sum = 0
        for (g <- enabledGroups) {
            if (g.minimumCount > 0) sum += g.minimumCount
        }
        if (sum > p.maximumLength && enabledGroups.nonEmpty) {
            val biggest = enabledGroups.maxBy(g => g.minimumCount)
            errors += problem(p.name, "minimumCount / maximumLength",
                "сумма обязательных символов больше максимальной длины пароля.\n\n" +
                  "Сумма обязательных символов: " + sum + "\n" +
                  "Максимальная длина пароля: " + p.maximumLength,
                "Уменьшите minimumCount для группы " + biggest.name + "\nили увеличьте maximumLength.")
        } else if (sum > p.defaultLength && sum <= p.maximumLength) {
            errors += problem(p.name, "defaultLength",
                "длина по умолчанию (" + p.defaultLength + ") меньше суммы обязательных символов (" + sum + ").",
                "Увеличьте defaultLength хотя бы до " + sum + " или уменьшите minimumCount.")
        }

        for (g <- enabledGroups) {
            if (g.customChars) {
                val clash = g.chars.filter(c => forbidden.contains(c))
                if (clash.nonEmpty) {
                    errors += problem(p.name, "forbiddenCharacters",
                        "символы \"" + clash + "\" разрешены в группе " + g.name + " и одновременно запрещены.",
                        "Уберите их из characters или из forbiddenCharacters.")
                }
            }
            val left = g.chars.filter(c => !forbidden.contains(c))
            if (left.isEmpty) {
                errors += problem(p.name, "forbiddenCharacters",
                    "после удаления запрещённых символов группа " + g.name + " становится пустой.",
                    "Уберите символы этой группы из forbiddenCharacters или выключите группу.")
            }
        }

        if (!p.allowRepeatedCharacters) {
            val pool = Passwords.allAvailable(p)
            if (pool.length < p.minimumLength) {
                errors += problem(p.name, "allowRepeatedCharacters",
                    "повторения запрещены, но доступно только " + pool.length + " разных символов, а минимальная длина " + p.minimumLength + ".",
                    "Разрешите повторения, добавьте группы символов или уменьшите minimumLength.")
            } else if (pool.length < p.defaultLength) {
                errors += problem(p.name, "allowRepeatedCharacters",
                    "повторения запрещены, но доступно только " + pool.length + " разных символов, а длина по умолчанию " + p.defaultLength + ".",
                    "Разрешите повторения, добавьте группы символов или уменьшите defaultLength.")
            }
            for (g <- enabledGroups) {
                val available = Passwords.availableChars(p, g).length
                if (available > 0 && g.minimumCount > available) {
                    errors += problem(p.name, "characterGroups." + g.name + ".minimumCount",
                        "повторения запрещены, а в группе " + g.name + " только " + available + " разных символов, нужно " + g.minimumCount + ".",
                        "Уменьшите minimumCount или разрешите повторения.")
                }
            }
        }

        if (p.maxConsecutive < 1) {
            errors += problem(p.name, "maximumConsecutiveIdenticalCharacters",
                "значение не может быть меньше 1 (сейчас " + p.maxConsecutive + ").",
                "Укажите 1 или больше.")
        }

        if (p.forbiddenSubstrings.contains("")) {
            errors += problem(p.name, "forbiddenSubstrings", "в списке есть пустая строка.",
                "Удалите пустую строку из списка.")
        }

        errors.toList
    }

    def quote(s: String): String = {
        "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\t", "\\t") + "\""
    }

    def policyToJson(p: Policy): String = {
        val groupTexts = p.groups.map { g =>
            var text = quote(g.name) + ": { \"enabled\": " + g.enabled + ", \"minimumCount\": " + g.minimumCount
            if (g.customChars) {
                text += ", \"characters\": " + quote(g.chars)
            }
            text + " }"
        }
        var json = "{ \"description\": " + quote(p.description) +
          ", \"minimumLength\": " + p.minimumLength +
          ", \"maximumLength\": " + p.maximumLength +
          ", \"defaultLength\": " + p.defaultLength +
          ", \"characterGroups\": { " + groupTexts.mkString(", ") + " }" +
          ", \"forbiddenCharacters\": " + quote(p.forbiddenCharacters) +
          ", \"allowRepeatedCharacters\": " + p.allowRepeatedCharacters
        if (p.maxConsecutive != Int.MaxValue) {
            json += ", \"maximumConsecutiveIdenticalCharacters\": " + p.maxConsecutive
        }
        json += ", \"allowSpaces\": " + p.allowSpaces +
          ", \"forbiddenSubstrings\": [" + p.forbiddenSubstrings.map(s => quote(s)).mkString(", ") + "] }"
        json
    }

    def addPolicyToFile(fileName: String, p: Policy): Unit = {
        val path = Paths.get(fileName)
        val text = new String(Files.readAllBytes(path), "UTF-8").stripPrefix("\uFEFF")
        val json = ujson.read(text)
        val newPolicy = ujson.read(policyToJson(p))
        json.obj("policies").obj(p.name) = newPolicy
        Files.write(path, ujson.write(json, indent = 2).getBytes("UTF-8"))
    }
}
