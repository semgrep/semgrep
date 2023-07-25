paths_to_transitivity = {
    "targets/dependency_aware/log4j/maven_dep_tree.txt": {
        "org.apache.logging.log4j:log4j-api": [],
        "org.apache.logging.log4j:log4j-core": [],
        "junit:junit": [],
        "org.apache.maven:maven-plugin-api": [
            {
                "package": "org.apache.maven:maven-model",
                "version": "3.8.6",
            },
            {
                "package": "org.apache.maven:maven-artifact",
                "version": "3.8.6",
            },
            {
                "package": "org.eclipse.sisu:org.eclipse.sisu.plexus",
                "version": "0.3.5",
            },
            {
                "package": "org.codehaus.plexus:plexus-utils",
                "version": "3.3.1",
            },
            {
                "package": "org.codehaus.plexus:plexus-classworlds",
                "version": "2.6.0",
            },
        ],
        "org.apache.maven:maven-model": [],
        "org.apache.maven:maven-artifact": [
            {"package": "org.apache.commons:commons-lang3", "version": "3.8.1"}
        ],
        "org.apache.commons:commons-lang3": [],
        "org.eclipse.sisu:org.eclipse.sisu.plexus": [
            {"package": "javax.annotation:javax.annotation-api", "version": "1.2"},
            {"package": "org.eclipse.sisu:org.eclipse.sisu.inject", "version": "0.3.5"},
            {
                "package": "org.codehaus.plexus:plexus-component-annotations",
                "version": "1.5.5",
            },
        ],
        "javax.annotation:javax.annotation-api": [],
        "org.eclipse.sisu:org.eclipse.sisu.inject": [],
        "org.codehaus.plexus:plexus-component-annotations": [],
        "org.codehaus.plexus:plexus-utils": [],
        "org.codehaus.plexus:plexus-classworlds": [],
        "org.apache.maven.plugin-tools:maven-plugin-annotations": [],
        "com.zaxxer:nuprocess": [
            {"package": "net.java.dev.jna:jna", "version": "5.11.0"}
        ],
        "net.java.dev.jna:jna": [],
    },
}
