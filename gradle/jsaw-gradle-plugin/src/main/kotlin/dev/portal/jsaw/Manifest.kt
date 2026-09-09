package dev.portal.jsaw

/**
 * Builds the versioned JSON manifest the compiler wasm reads on stdin,
 * and parses its single result line. Hand-rolled (the schema is small and
 * stable) so the plugin needs no JSON library.
 *
 * This must stay in lockstep with `crates/jsaw-wasi-bin/src/manifest.rs`.
 */
object Manifest {
    const val VERSION = 1

    fun build(
        entry: String,
        modules: List<String>,
        numericExports: Boolean,
        gcExportSuffix: String?,
        wasmOut: String?,
        javaOut: String?,
        swiftOut: String?,
    ): String {
        val sb = StringBuilder()
        sb.append('{')
        sb.append("\"version\":").append(VERSION).append(',')
        sb.append("\"entry\":").append(jsonString(entry)).append(',')
        sb.append("\"modules\":").append(modules.joinToString(",", "[", "]") { jsonString(it) }).append(',')
        sb.append("\"options\":{")
        sb.append("\"numericExports\":").append(numericExports)
        if (gcExportSuffix != null) {
            sb.append(",\"gcExportSuffix\":").append(jsonString(gcExportSuffix))
        }
        sb.append('}').append(',')
        sb.append("\"emit\":{")
        val emitParts = mutableListOf<String>()
        if (wasmOut != null) emitParts.add("\"wasm\":${jsonString(wasmOut)}")
        if (javaOut != null) emitParts.add("\"java\":${jsonString(javaOut)}")
        if (swiftOut != null) emitParts.add("\"swift\":${jsonString(swiftOut)}")
        sb.append(emitParts.joinToString(","))
        sb.append('}')
        sb.append('}')
        return sb.toString()
    }

    /** Extract `"error":"..."` from an error result line, or null. */
    fun parseError(resultLine: String): String? {
        if (!resultLine.contains("\"status\":\"error\"")) return null
        val key = "\"error\":\""
        val start = resultLine.indexOf(key)
        if (start < 0) return "unknown compiler error"
        var i = start + key.length
        val sb = StringBuilder()
        while (i < resultLine.length) {
            val c = resultLine[i]
            if (c == '\\' && i + 1 < resultLine.length) {
                sb.append(resultLine[i + 1])
                i += 2
            } else if (c == '"') {
                break
            } else {
                sb.append(c)
                i++
            }
        }
        return sb.toString()
    }

    fun isOk(resultLine: String): Boolean = resultLine.contains("\"status\":\"ok\"")

    private fun jsonString(s: String): String = buildString {
        append('"')
        for (c in s) {
            when (c) {
                '"' -> append("\\\"")
                '\\' -> append("\\\\")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> append(c)
            }
        }
        append('"')
    }
}
