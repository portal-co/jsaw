package dev.portal.jsaw

import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.file.ConfigurableFileCollection
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.provider.Property
import org.gradle.api.tasks.CacheableTask
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.InputDirectory
import org.gradle.api.tasks.InputFile
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.Optional
import org.gradle.api.tasks.OutputDirectories
import org.gradle.api.tasks.PathSensitive
import org.gradle.api.tasks.PathSensitivity
import org.gradle.api.tasks.TaskAction
import java.io.File

/**
 * Compiles one JS module set to its enabled targets by running the
 * wasip1-compiled jsaw compiler inside the Gradle daemon via Chicory.
 *
 * The task is cacheable: its inputs are the compiler wasm bytes, the
 * module-set sources, and the scalar options, and compilation is a pure
 * function of those (proven byte-identical by the M13 golden tests), so
 * up-to-date checks and the build cache are sound.
 */
@CacheableTask
abstract class JsawCompileTask : DefaultTask() {

    /** The wasm32-wasip1 compiler binary. */
    @get:InputFile
    @get:PathSensitive(PathSensitivity.NONE)
    abstract val compilerWasm: RegularFileProperty

    /** Directory holding the module set (the linker root). */
    @get:InputDirectory
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val sourceDir: DirectoryProperty

    @get:Input
    abstract val entry: Property<String>

    @get:Input
    abstract val emitWasm: Property<Boolean>

    @get:Input
    abstract val emitJava: Property<Boolean>

    @get:Input
    abstract val emitSwift: Property<Boolean>

    @get:Input
    abstract val numericExports: Property<Boolean>

    @get:Optional
    @get:Input
    abstract val gcExportSuffix: Property<String>

    /** Root of this task's generated outputs; each enabled target is a subdir. */
    @get:Internal
    abstract val outputRoot: DirectoryProperty

    // Declare the enabled target dirs as outputs. We expose the root as
    // internal and compute the three concrete dirs; Gradle treats the
    // whole root as the output via @OutputDirectories on a helper.
    @get:OutputDirectories
    val outputDirectories: List<File>
        get() {
            val root = outputRoot.get().asFile
            val dirs = mutableListOf<File>()
            if (emitWasm.get()) dirs.add(File(root, "wasm"))
            if (emitJava.get()) dirs.add(File(root, "java"))
            if (emitSwift.get()) dirs.add(File(root, "swift"))
            return dirs
        }

    /** The generated Java source dir, for wiring into a consuming source set. */
    @get:Internal
    val javaOutputDir: File
        get() = File(outputRoot.get().asFile, "java")

    init {
        group = "build"
        description = "Compiles a JS module set with the wasip1 jsaw compiler."
    }

    @TaskAction
    fun compile() {
        val root = outputRoot.get().asFile
        val srcDir = sourceDir.get().asFile
        require(srcDir.isDirectory) { "jsaw sourceDir ${srcDir} does not exist" }

        // Clean and stage outputs. The staging dir is the WASI `/out`
        // preopen; the manifest's emit paths are relative to it.
        root.deleteRecursively()
        val staging = File(root, "staging")
        staging.mkdirs()

        // Enumerate the module set: every .js/.mjs file under sourceDir,
        // keyed by its root-relative path with '/' separators (the linker
        // keys the binary resolves `./a.js` against).
        val modules = srcDir.walkTopDown()
            .filter { it.isFile && (it.extension == "js" || it.extension == "mjs") }
            .map { it.relativeTo(srcDir).invariantSeparatorsPath }
            .sorted()
            .toList()
        require(modules.isNotEmpty()) { "no .js/.mjs files under ${srcDir}" }
        require(entry.get() in modules) {
            "entry '${entry.get()}' is not among the module set: $modules"
        }

        val wasmOut = if (emitWasm.get()) "wasm/module.wasm" else null
        val javaOut = if (emitJava.get()) "java" else null
        val swiftOut = if (emitSwift.get()) "swift" else null

        val manifest = Manifest.build(
            entry = entry.get(),
            modules = modules,
            numericExports = numericExports.get(),
            gcExportSuffix = gcExportSuffix.orNull,
            wasmOut = wasmOut,
            javaOut = javaOut,
            swiftOut = swiftOut,
        )

        val result = JsawRunner.run(
            compilerWasm = compilerWasm.get().asFile.toPath(),
            srcDir = srcDir.toPath(),
            outDir = staging.toPath(),
            manifestJson = manifest,
        )

        val firstLine = result.stdout.lineSequence().firstOrNull() ?: ""
        if (result.exitCode != 0 || !Manifest.isOk(firstLine)) {
            val error = Manifest.parseError(firstLine)
                ?: "compiler exited ${result.exitCode}"
            throw GradleException(
                "jsaw compilation of ${entry.get()} failed: $error\n" +
                    (if (result.stderr.isNotBlank()) "compiler stderr:\n${result.stderr}" else "")
            )
        }

        // Move staged outputs from <root>/staging/{wasm,java,swift} up to
        // <root>/{wasm,java,swift}.
        for (target in listOf("wasm", "java", "swift")) {
            val staged = File(staging, target)
            if (staged.isDirectory) {
                val dest = File(root, target)
                dest.deleteRecursively()
                staged.renameTo(dest)
            }
        }
        staging.deleteRecursively()

        logger.lifecycle(
            "jsaw: compiled ${entry.get()} (${modules.size} modules) to " +
                listOfNotNull(
                    if (emitWasm.get()) "wasm" else null,
                    if (emitJava.get()) "java" else null,
                    if (emitSwift.get()) "swift" else null,
                ).joinToString(", ")
        )
    }
}
