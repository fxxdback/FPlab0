import scala.collection.mutable.ListBuffer

object Passwords {

    val random = new scala.util.Random(new java.security.SecureRandom())

    def availableChars(p: Policy, g: CharGroup): String = {
        val forbidden = p.forbiddenCharacters.toSet
        g.chars.filter(c => !forbidden.contains(c))
    }

    def allAvailable(p: Policy): String = {
        var result = ""
        for (g <- p.groups if g.enabled) {
            for (c <- availableChars(p, g)) {
                if (result.indexOf(c) < 0) {
                    result += c
                }
            }
        }
        result
    }

    def requiredTotal(p: Policy): Int = {
        var sum = 0
        for (g <- p.groups if g.enabled) {
            if (g.minimumCount > 0) sum += g.minimumCount
        }
        sum
    }

    def randomChar(from: String): Char = {
        from.charAt(random.nextInt(from.length))
    }

    def generate(p: Policy, length: Int): Option[String] = {
        var result: Option[String] = None
        var attempt = 0
        val pool = allAvailable(p) + (if (p.allowSpaces) " " else "")

        while (result.isEmpty && attempt < 1000) {
            attempt += 1
            val chars = ListBuffer[Char]()
            var failed = false

            for (g <- p.groups if g.enabled) {
                var i = 0
                while (i < g.minimumCount && !failed) {
                    var candidates = availableChars(p, g)
                    if (!p.allowRepeatedCharacters) {
                        candidates = candidates.filter(c => !chars.contains(c))
                    }
                    if (candidates.isEmpty) {
                        failed = true
                    } else {
                        chars += randomChar(candidates)
                        i += 1
                    }
                }
            }

            while (chars.length < length && !failed) {
                var candidates = pool
                if (!p.allowRepeatedCharacters) {
                    candidates = candidates.filter(c => !chars.contains(c))
                }
                if (candidates.isEmpty) {
                    failed = true
                } else {
                    chars += randomChar(candidates)
                }
            }

            if (!failed) {
                val password = random.shuffle(chars.toList).mkString
                val spaceAtEdge = password.startsWith(" ") || password.endsWith(" ")
                if (!spaceAtEdge) {
                    val results = check(p, password)
                    if (results.forall(r => r._1)) {
                        result = Some(password)
                    }
                }
            }
        }
        result
    }

    def check(p: Policy, password: String): List[(Boolean, String)] = {
        val results = ListBuffer[(Boolean, String)]()
        val forbidden = p.forbiddenCharacters.toSet

        val lengthOk = password.length >= p.minimumLength && password.length <= p.maximumLength
        if (lengthOk) {
            results += ((true, "Длина находится в пределах от " + p.minimumLength + " до " + p.maximumLength))
        } else {
            results += ((false, "Длина (" + password.length + ") выходит за пределы от " + p.minimumLength + " до " + p.maximumLength))
        }

        for (g <- p.groups if g.enabled && g.minimumCount > 0) {
            val available = availableChars(p, g)
            val count = password.count(c => available.indexOf(c) >= 0)
            val ok = count >= g.minimumCount
            if (g.minimumCount == 1) {
                if (ok) results += ((true, "Содержит " + Groups.has(g.name)))
                else results += ((false, "Не содержит " + Groups.hasNot(g.name)))
            } else {
                if (ok) results += ((true, Groups.title(g.name).capitalize + ": найдено " + count + " (нужно не менее " + g.minimumCount + ")"))
                else results += ((false, Groups.title(g.name).capitalize + ": найдено " + count + ", нужно не менее " + g.minimumCount))
            }
        }

        var allowed = ""
        for (g <- p.groups if g.enabled) {
            allowed += g.chars
        }

        val foreign = password.distinct.filter(c => allowed.indexOf(c) < 0 && c != ' ')
        if (foreign.isEmpty) {
            results += ((true, "Использует только символы разрешённых групп"))
        } else {
            results += ((false, "Содержит символы не из разрешённых групп: " + foreign.toList.mkString(" ")))
        }

        if (p.forbiddenCharacters.nonEmpty) {
            val bad = password.distinct.filter(c => forbidden.contains(c))
            if (bad.isEmpty) results += ((true, "Не содержит запрещённых символов"))
            else results += ((false, "Содержит запрещённые символы: " + bad.toList.mkString(" ")))
        }

        if (p.forbiddenSubstrings.nonEmpty) {
            val lower = password.toLowerCase
            val bad = p.forbiddenSubstrings.filter(s => s.nonEmpty && lower.contains(s.toLowerCase))
            if (bad.isEmpty) results += ((true, "Не содержит запрещённых подстрок"))
            else results += ((false, "Содержит запрещённые подстроки: " + bad.mkString(", ")))
        }

        if (!p.allowRepeatedCharacters) {
            val repeated = password.distinct.filter(c => password.count(x => x == c) > 1)
            if (repeated.isEmpty) results += ((true, "Не содержит повторяющихся символов"))
            else results += ((false, "Повторяются символы: " + repeated.toList.mkString(" ")))
        } else if (p.maxConsecutive != Int.MaxValue) {

            var longest = 0
            var current = 0
            var i = 0
            while (i < password.length) {
                if (i > 0 && password.charAt(i) == password.charAt(i - 1)) current += 1 else current = 1
                if (current > longest) longest = current
                i += 1
            }
            if (longest <= p.maxConsecutive) {
                results += ((true, "Не содержит " + moreThan(p.maxConsecutive) + " подряд"))
            } else {
                results += ((false, "Найдено " + longest + " одинаковых символов подряд, можно не более " + p.maxConsecutive))
            }
        }

        if (!p.allowSpaces) {
            if (password.indexOf(' ') >= 0) results += ((false, "Содержит пробелы, а они запрещены"))
            else results += ((true, "Не содержит пробелов"))
        }

        results.toList
    }

    def moreThan(n: Int): String = {
        if (n == 1) "одинаковых символов"
        else if (n == 2) "более двух одинаковых символов"
        else if (n == 3) "более трёх одинаковых символов"
        else if (n == 4) "более четырёх одинаковых символов"
        else "более " + n + " одинаковых символов"
    }
}
