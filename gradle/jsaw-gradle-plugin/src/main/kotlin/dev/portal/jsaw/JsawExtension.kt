package dev.portal.jsaw

import org.gradle.api.Action
import org.gradle.api.Named
import org.gradle.api.model.ObjectFactory
import org.gradle.api.provider.MapProperty
import org.gradle.api.provider.Property
import org.gradle.api.tasks.Nested
import java.io.Serializable
import javax.inject.Inject

/**
 * The `jsaw { ... }` project extension. Holds a container of named module
 * sets, each compiled by its own [JsawCompileTask].
 */
abstract class JsawExtension @Inject constructor(objects: ObjectFactory) {
    /** Named module sets, keyed by name (e.g. "main"). */
    val moduleSets: MutableMap<String, JsawModuleSet> = linkedMapOf()

    private val objects = objects

    /** Optional explicit compiler wasm override (absolute or project-relative). */
    val compilerWasm: Property<String> = objects.property(String::class.java)

    /** Optional included-build / subproject path that builds the compiler wasm. */
    val compilerProject: Property<String> = objects.property(String::class.java)

    fun modules(action: Action<in ModuleSetContainer>) {
        action.execute(ModuleSetContainer(moduleSets, objects))
    }
}

class ModuleSetContainer(
    private val map: MutableMap<String, JsawModuleSet>,
    private val objects: ObjectFactory,
) {
    fun register(name: String, action: Action<in JsawModuleSet>) {
        val spec = objects.newInstance(JsawModuleSet::class.java, name)
        action.execute(spec)
        map[name] = spec
    }
}

/**
 * One JS module set to compile. All properties are plain (not Gradle
 * managed-property) so the spec can be constructed eagerly inside the
 * extension's container; the task copies them into its own lazy inputs.
 */
abstract class JsawModuleSet @Inject constructor(private val name: String) : Named, Serializable {
    override fun getName(): String = name

    /** Entry module key relative to [sourceDir] (e.g. "index.js"). */
    abstract val entry: Property<String>

    /** Directory holding the module set (the linker root). */
    abstract val sourceDir: Property<String>

    /** Emit the WasmGC `.wasm` (output: `wasm/module.wasm`). */
    abstract val emitWasm: Property<Boolean>

    /** Emit Java sources (output: `java/`). */
    abstract val emitJava: Property<Boolean>

    /** Emit Swift sources (output: `swift/`). */
    abstract val emitSwift: Property<Boolean>

    /** Mirror of `ConvertOptions::numeric_exports`. */
    abstract val numericExports: Property<Boolean>

    /** Mirror of `ConvertOptions::gc_export_suffix` (null = none). */
    abstract val gcExportSuffix: Property<String>

    init {
        entry.convention("index.js")
        sourceDir.convention("src/main/js")
        emitWasm.convention(false)
        emitJava.convention(true)
        emitSwift.convention(false)
        numericExports.convention(true)
        gcExportSuffix.convention(null as String?)
    }
}
