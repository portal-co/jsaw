package dev.portal.jsaw

import com.dylibso.chicory.runtime.ImportValues
import com.dylibso.chicory.runtime.Instance
import com.dylibso.chicory.wasm.Parser
import com.dylibso.chicory.wasi.WasiExitException
import com.dylibso.chicory.wasi.WasiOptions
import com.dylibso.chicory.wasi.WasiPreview1
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.file.Path

/**
 * Runs the jsaw compiler (a wasm32-wasip1 binary) once via Chicory.
 *
 * The compiler reads a JSON manifest on stdin, reads module sources from
 * the preopened `/src`, writes outputs under the preopened `/out`, and
 * prints a single result JSON line on stdout. This runner is a thin,
 * synchronous wrapper; task-level concerns (caching, wiring) live in
 * [JsawCompileTask].
 */
object JsawRunner {

    data class RunResult(
        val exitCode: Int,
        val stdout: String,
        val stderr: String,
    )

    /**
     * Execute [compilerWasm] with [srcDir] preopened read-only at `/src`
     * and [outDir] preopened read-write at `/out`, feeding [manifestJson]
     * to stdin.
     */
    fun run(
        compilerWasm: Path,
        srcDir: Path,
        outDir: Path,
        manifestJson: String,
    ): RunResult {
        val stdout = ByteArrayOutputStream()
        val stderr = ByteArrayOutputStream()
        val stdin = ByteArrayInputStream(manifestJson.toByteArray(Charsets.UTF_8))

        val options = WasiOptions.builder()
            .withStdout(stdout)
            .withStderr(stderr)
            .withStdin(stdin)
            .withArguments(listOf("jsaw-compiler", "--src", "/src", "--out", "/out"))
            .withDirectory("/src", srcDir)
            .withDirectory("/out", outDir)
            .build()

        val exitCode = WasiPreview1.builder().withOptions(options).build().use { wasi ->
            val imports = ImportValues.builder()
                .addFunction(*wasi.toHostFunctions())
                .build()
            val instance = Instance.builder(Parser.parse(compilerWasm))
                .withImportValues(imports)
                // `_start` runs as the instance start section; do not also
                // invoke it explicitly below.
                .withStart(false)
                .build()
            try {
                instance.export("_start").apply()
                0
            } catch (e: WasiExitException) {
                // proc_exit(code) surfaces here; 0 means success too.
                e.exitCode()
            } catch (e: Exception) {
                // Any other trap/error: surface it on stderr for the task to
                // report, and treat as failure.
                stderr.write("\n[chicory] ${e.javaClass.name}: ${e.message}".toByteArray())
                1
            }
        }

        return RunResult(
            exitCode = exitCode,
            stdout = stdout.toString(Charsets.UTF_8),
            stderr = stderr.toString(Charsets.UTF_8),
        )
    }
}
