plugins {
    `java-gradle-plugin`
    `maven-publish`
    kotlin("jvm") version "2.0.21"
}

group = "dev.portal.jsaw"
version = "0.1.0"

repositories {
    mavenCentral()
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(21))
    }
}

kotlin {
    compilerOptions {
        jvmTarget.set(org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_21)
    }
}

dependencies {
    // Chicory: pure-Java Wasm runtime with WASI p1 support. No JNI, so the
    // plugin runs in any Gradle daemon on any OS.
    implementation("com.dylibso.chicory:runtime:1.7.5")
    implementation("com.dylibso.chicory:wasi:1.7.5")

    // Compile-only against the Gradle API (provided by the daemon).
    compileOnly(gradleApi())

    testImplementation(platform("org.junit:junit-bom:5.10.2"))
    testImplementation("org.junit.jupiter:junit-jupiter")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

// Functional tests run against a real Gradle build via TestKit. Created
// before gradlePlugin{} so the plugin can add it as a test source set.
val functionalTest = sourceSets.create("functionalTest")
configurations["functionalTestImplementation"].extendsFrom(configurations["testImplementation"])
configurations["functionalTestRuntimeOnly"].extendsFrom(configurations["testRuntimeOnly"])

gradlePlugin {
    plugins {
        create("jsaw") {
            id = "dev.portal.jsaw"
            implementationClass = "dev.portal.jsaw.JsawPlugin"
        }
    }
    // TestKit functional tests get the plugin under development on their
    // classpath automatically.
    testSourceSets.add(functionalTest)
}

val functionalTestTask = tasks.register<Test>("functionalTest") {
    group = "verification"
    description = "Runs the TestKit functional tests against a real Gradle build."
    testClassesDirs = functionalTest.output.classesDirs
    classpath = functionalTest.runtimeClasspath
    useJUnitPlatform()
    // The compiler wasm is large; give the forked test JVM room.
    maxHeapSize = "2g"
}
tasks.named("check") { dependsOn(functionalTestTask) }

tasks.named<Test>("test") {
    useJUnitPlatform()
}
