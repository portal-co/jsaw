package dev.portal.jsaw

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import java.io.File

/**
 * Directly exercises [JsawRunner] against the real compiler wasm, with no
 * Gradle machinery — isolates Chicory/WASI behavior from task wiring.
 */
class JsawRunnerTest {

    @TempDir
    lateinit var dir: File

    private fun compilerWasm(): File {
        val here = File(System.getProperty("user.dir"))
        val repoRoot = generateSequence(here) { it.parentFile }
            .firstOrNull { File(it, "crates/jsaw-wasi-bin").isDirectory() }
            ?: error("could not locate the repo root from $here")
        return File(repoRoot, "target/wasm32-wasip1/release/jsaw-wasi-bin.wasm")
    }

    @Test
    fun `runs the compiler end to end through chicory`() {
        val src = File(dir, "src").apply { mkdirs() }
        val out = File(dir, "out").apply { mkdirs() }
        File(src, "index.js").writeText("export function run(a) { return a + 1; }")

        val manifest = Manifest.build(
            entry = "index.js",
            modules = listOf("index.js"),
            numericExports = true,
            gcExportSuffix = null,
            wasmOut = "out/wasm/module.wasm",
            javaOut = "out/java",
            swiftOut = null,
        )

        val result = JsawRunner.run(
            compilerWasm = compilerWasm().toPath(),
            srcDir = src.toPath(),
            outDir = out.toPath(),
            manifestJson = manifest,
        )

        assertEquals(0, result.exitCode, "compiler should exit 0; stderr: ${result.stderr}")
        assertTrue(Manifest.isOk(result.stdout.lineSequence().first()), "should be ok: ${result.stdout}")
        assertTrue(File(out, "out/wasm/module.wasm").isFile, "wasm written")
        assertTrue(File(out, "out/java/pc/portal/mob/Mod.java").isFile, "java written")
    }
}
