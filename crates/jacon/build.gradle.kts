plugins{
    id ("java")
    id ("java-library")
    id ("maven-publish")
}
repositories{
    mavenCentral()
}
dependencies{
    api("io.github.jbock-java:either:1.5.2")
}