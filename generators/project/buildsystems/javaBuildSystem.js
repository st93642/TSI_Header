/**
 * Java Build System Generator
 * Generates Maven pom.xml and Gradle build.gradle for Java projects
 */

let vscode;
try {
    vscode = require('vscode');
} catch (e) {
    // vscode not available (running outside VS Code)
    vscode = null;
}

/**
 * Generate Maven pom.xml content with TSI header
 */
function generateMavenPomContent(projectName, tsiHeader) {
    return generateMavenPomXml(projectName, tsiHeader);
}

/**
 * Generate Maven pom.xml structure
 */
function generateMavenPomXml(projectName, tsiHeader) {
    return `<?xml version="1.0" encoding="UTF-8"?>
${tsiHeader}
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>lv.tsi</groupId>
    <artifactId>${projectName}</artifactId>
    <version>1.0.0</version>
    <packaging>jar</packaging>

    <name>${projectName}</name>
    <description>TSI Header Java Project - ${projectName}</description>

    <properties>
        <maven.compiler.source>17</maven.compiler.source>
        <maven.compiler.target>17</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <junit.version>5.9.2</junit.version>
    </properties>

    <dependencies>
        <!-- JUnit 5 for testing -->
        <dependency>
            <groupId>org.junit.jupiter</groupId>
            <artifactId>junit-jupiter</artifactId>
            <version>\${junit.version}</version>
            <scope>test</scope>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <!-- Maven Compiler Plugin -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.11.0</version>
                <configuration>
                    <source>17</source>
                    <target>17</target>
                </configuration>
            </plugin>

            <!-- Maven Surefire Plugin for running tests -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-surefire-plugin</artifactId>
                <version>3.0.0</version>
            </plugin>

            <!-- Maven JAR Plugin -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-jar-plugin</artifactId>
                <version>3.3.0</version>
                <configuration>
                    <archive>
                        <manifest>
                            <mainClass>lv.tsi.${projectName}.Main</mainClass>
                        </manifest>
                    </archive>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>`;
}

/**
 * Generate Gradle build.gradle content with TSI header
 */
function generateGradleBuildContent(projectName, tsiHeader) {
    return generateGradleBuildScript(projectName, tsiHeader);
}

/**
 * Generate Gradle build.gradle structure
 */
function generateGradleBuildScript(projectName, tsiHeader) {
    return `${tsiHeader}

plugins {
    id 'java'
    id 'application'
}

group = 'lv.tsi'
version = '1.0.0'
sourceCompatibility = '17'

repositories {
    mavenCentral()
}

dependencies {
    // JUnit 5 for testing
    testImplementation 'org.junit.jupiter:junit-jupiter:5.9.2'

    // Additional dependencies can be added here
}

application {
    mainClassName = 'lv.tsi.${projectName}.Main'
}

test {
    useJUnitPlatform()
}

jar {
    manifest {
        attributes 'Main-Class': 'lv.tsi.${projectName}.Main'
    }
}

task fatJar(type: Jar) {
    manifest.from jar.manifest
    classifier = 'all'
    from {
        configurations.runtimeClasspath.collect { it.isDirectory() ? it : zipTree(it) }
    } {
        exclude "META-INF/*.SF"
        exclude "META-INF/*.DSA"
        exclude "META-INF/*.RSA"
    }
    with jar
}

task runJar(dependsOn: jar) {
    doLast {
        javaexec {
            main = '-jar'
            args jar.archiveFile.get()
        }
    }
}`;
}

module.exports = {
    generateMavenPomContent,
    generateMavenPomXml,
    generateGradleBuildContent,
    generateGradleBuildScript
};