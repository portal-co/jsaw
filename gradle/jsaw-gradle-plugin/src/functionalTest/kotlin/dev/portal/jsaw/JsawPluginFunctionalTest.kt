package dev.portal.jsaw

import org.gradle.testkit.runner.GradleRunner
import org.gradle.testkit.runner.TaskOutcome
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.io.File
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue

/**
 * Functional tests: apply the plugin to a real project and run the
 * compile task via TestKit, asserting the generated outputs and the
 * build-cache / up-to-date behavior.
 */
class JsawPluginFunctionalTest {

    @TempDir
    lateinit var projectDir: File

    private fun writeFixture(emitJava: Boolean = true, emitWasm: Boolean = true) {
        // Copy the JS fixture module set.
        val fixture = File(javaClass.getResource("/fixture/src/main/js")!!.toURI())
        val srcDir = File(projectDir, "src/main/js")
        srcDir.mkdirs()
        fixture.listFiles()!!.forEach { it.copyTo(File(srcDir, it.name)) }

        // The compiler wasm, built by `cargo build --target wasm32-wasip1`.
        // Pointed at explicitly (mode 1 of the plan) so the functional test
        // needs no published artifact.
        val compilerWasm = File(System.getProperty("jsaw.compilerWasm") ?: defaultCompilerWasm())
        assertTrue(compilerWasm.isFile, "compiler wasm should exist at $compilerWasm " +
            "(build with `cargo build -p jsaw-wasi-bin --target wasm32-wasip1 --release`)")

        File(projectDir, "settings.gradle.kts").writeText(
            """rootProject.name = "consumer""""
        )
        File(projectDir, "build.gradle.kts").writeText(
            """
            plugins {
                java
                id("dev.portal.jsaw")
            }

            jsaw {
                compilerWasm.set("${compilerWasm.absolutePath}")
                modules {
                    register("main") {
                        entry.set("index.js")
                        emitJava.set($emitJava)
                        emitWasm.set($emitWasm)
                    }
                }
            }
            """.trimIndent()
        )
    }

    private fun defaultCompilerWasm(): String {
        // Walk up from the working dir to the repo root (the dir holding
        // crates/jsaw-wasi-bin), then to the built compiler wasm.
        val here = File(System.getProperty("user.dir"))
        val repoRoot = generateSequence(here) { it.parentFile }
            .firstOrNull { File(it, "crates/jsaw-wasi-bin").isDirectory() }
            ?: error("could not locate the repo root from $here")
        return File(repoRoot, "target/wasm32-wasip1/release/jsaw-wasi-bin.wasm").absolutePath
    }

    private fun runner(vararg args: String): GradleRunner = GradleRunner.create()
        .withProjectDir(projectDir)
        .withArguments(*args, "--stacktrace")
        .withPluginClasspath()
        .withDebug(false)

    @Test
    fun `compiles a module set to valid WasmGC and Java`() {
        writeFixture(emitJava = true, emitWasm = true)
        val result = runner("compileJsawMain").build()

        assertEquals(TaskOutcome.SUCCESS, result.task(":compileJsawMain")?.outcome)

        // The WasmGC module is emitted and is a wasm binary.
        val wasm = File(projectDir, "build/generated/jsaw/main/wasm/module.wasm")
        assertTrue(wasm.isFile, "module.wasm should be generated")
        val header = wasm.readBytes().take(4).map { it.toInt() and 0xff }
        assertEquals(listOf(0x00, 0x61, 0x73, 0x6d), header, "wasm magic header")

        // Java sources are emitted under the package directory.
        val modJava = File(projectDir, "build/generated/jsaw/main/java/pc/portal/mob/Mod.java")
        assertTrue(modJava.isFile, "Mod.java should be generated")
        val modSrc = modJava.readText()
        assertTrue(modSrc.contains("package pc.portal.mob;"), "Mod.java has the package")
        // The export delegates exist for the two exported functions.
        assertTrue(modSrc.contains("run"), "Mod.java references the run export")
        assertTrue(modSrc.contains("count"), "Mod.java references the count export")
    }

    @Test
    fun `generated Java compiles when wired into the source set`() {
        writeFixture(emitJava = true, emitWasm = false)
        // compileJava should depend on compileJsawMain and compile the
        // generated sources.
        val result = runner("compileJava").build()
        assertEquals(TaskOutcome.SUCCESS, result.task(":compileJsawMain")?.outcome)
        assertEquals(TaskOutcome.SUCCESS, result.task(":compileJava")?.outcome)
        assertTrue(
            File(projectDir, "build/classes/java/main/pc/portal/mob/Mod.class").isFile,
            "Mod.class should be compiled from generated sources"
        )
    }

    @Test
    fun `task is up-to-date on a second run with unchanged inputs`() {
        writeFixture(emitJava = true, emitWasm = true)
        runner("compileJsawMain").build()
        val second = runner("compileJsawMain").build()
        assertEquals(
            TaskOutcome.UP_TO_DATE,
            second.task(":compileJsawMain")?.outcome,
            "unchanged inputs should make the task UP-TO-DATE"
        )
    }

    @Test
    fun `fails the build with a useful message on a compiler error`() {
        writeFixture()
        // Break the entry so the compiler reports a structured error.
        File(projectDir, "src/main/js/index.js").writeText(
            "import { nope } from './missing.js'; export function run() { return nope(); }"
        )
        val result = runner("compileJsawMain").buildAndFail()
        assertTrue(
            result.output.contains("jsaw compilation"),
            "failure should be attributed to jsaw: ${result.output}"
        )
    }
}
