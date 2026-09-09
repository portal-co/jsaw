package dev.portal.jsaw

import org.gradle.api.Plugin
import org.gradle.api.Project
import org.gradle.api.artifacts.Configuration
import org.gradle.api.attributes.Attribute
import org.gradle.api.plugins.JavaPlugin
import org.gradle.api.tasks.SourceSetContainer
import org.gradle.api.tasks.compile.JavaCompile
import java.io.File

/**
 * The `dev.portal.jsaw` plugin.
 *
 * For each named module set in `jsaw { modules { ... } }`, registers a
 * [JsawCompileTask] `compileJsaw<Name>` and, when Java emission is
 * enabled, wires the generated Java into the consuming source set.
 */
class JsawPlugin : Plugin<Project> {

    override fun apply(project: Project) {
        val extension = project.extensions.create("jsaw", JsawExtension::class.java)

        // The compiler wasm comes from (1) an explicit override, (2) a
        // published Maven artifact, or (3) a subproject building it. We
        // resolve it lazily into a detached configuration per task.
        val compilerConfig: Configuration = project.configurations.create("jsawCompiler") {
            it.isCanBeConsumed = false
            it.isCanBeResolved = true
            it.isVisible = false
        }
        // Default dependency: the published artifact matching the plugin
        // version. Overridden when compilerWasm/compilerProject is set.
        project.dependencies.add(
            compilerConfig.name,
            mapOf(
                "group" to "dev.portal.jsaw",
                "name" to "jsaw-compiler-wasm",
                "version" to (project.version.takeIf { it != Project.DEFAULT_VERSION }?.toString()
                    ?: PLUGIN_VERSION),
                "ext" to "wasm",
            )
        )

        project.afterEvaluate {
            extension.moduleSets.forEach { (name, spec) ->
                val taskName = "compileJsaw" + name.replaceFirstChar { it.uppercase() }
                val task = project.tasks.register(taskName, JsawCompileTask::class.java) { t ->
                    t.entry.set(spec.entry)
                    t.sourceDir.set(project.layout.projectDirectory.dir(spec.sourceDir.get()))
                    t.emitWasm.set(spec.emitWasm)
                    t.emitJava.set(spec.emitJava)
                    t.emitSwift.set(spec.emitSwift)
                    t.numericExports.set(spec.numericExports)
                    t.gcExportSuffix.set(spec.gcExportSuffix)
                    t.outputRoot.set(project.layout.buildDirectory.dir("generated/jsaw/$name"))

                    // Resolve the compiler wasm: explicit file > subproject > Maven.
                    val explicit = extension.compilerWasm.orNull
                    when {
                        !explicit.isNullOrBlank() ->
                            t.compilerWasm.set(project.layout.projectDirectory.file(explicit))
                        else ->
                            t.compilerWasm.fileProvider(
                                compilerConfig.elements.map { files ->
                                    files.single().asFile
                                }
                            )
                    }
                }

                // Wire generated Java into the main source set and make
                // compilation depend on this task.
                if (spec.emitJava.get()) {
                    project.plugins.withType(JavaPlugin::class.java) {
                        val sourceSets = project.extensions.getByType(SourceSetContainer::class.java)
                        sourceSets.named("main") { ss ->
                            ss.java.srcDir(task.map { it.javaOutputDir })
                        }
                        project.tasks.withType(JavaCompile::class.java).configureEach { jc ->
                            jc.dependsOn(task)
                        }
                    }
                }
            }
        }
    }

    companion object {
        // Keep in sync with the plugin's published version.
        const val PLUGIN_VERSION = "0.1.0"
    }
}
