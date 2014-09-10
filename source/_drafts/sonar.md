title: sonar
categories:
  - Quality Assurance
tags:
  - sonar
date: 2014-08-30 16:49:30
---

Sonar 运行错误:

```
SonarQube Runner 2.4
Java 1.7.0_45 Oracle Corporation (64-bit)
Mac OS X 10.9.3 x86_64
INFO: Runner configuration file: /Users/hezhiqiang/runtimes/sonar-runner-2.4/conf/sonar-runner.properties
INFO: Project configuration file: NONE
INFO: Default locale: "zh_CN", source code encoding: "UTF-8" (analysis is platform dependent)
INFO: Work directory: /Users/hezhiqiang/PhpstormProjects/sonar-examples-master/./.sonar
INFO: SonarQube Server 4.4
16:47:47.889 INFO  - Load global settings
16:47:48.110 INFO  - User cache: /Users/hezhiqiang/.sonar/cache
16:47:48.122 INFO  - Install plugins
16:47:48.265 INFO  - Install JDBC driver
16:47:48.272 WARN  - H2 database should be used for evaluation purpose only
16:47:48.272 INFO  - Create JDBC datasource for jdbc:h2:tcp://localhost/sonar
16:47:48.354 INFO  - Initializing Hibernate
INFO: ------------------------------------------------------------------------
INFO: EXECUTION FAILURE
INFO: ------------------------------------------------------------------------
Total time: 3.468s
Final Memory: 12M/466M
INFO: ------------------------------------------------------------------------
ERROR: Error during Sonar runner execution
ERROR: Unable to execute Sonar
ERROR: Caused by: You must define the following mandatory properties for 'Unknown': sonar.projectKey, sonar.projectName, sonar.projectVersion, sonar.sources
ERROR:
ERROR: To see the full stack trace of the errors, re-run SonarQube Runner with the -e switch.
ERROR: Re-run SonarQube Runner using the -X switch to enable full debug logging.
```

解决办法:

在运行`sonar-runner`的目录下创建`sonar-project.properties`文件:

```
# Required metadata
sonar.projectKey=java-sonar-runner-simple
sonar.projectName=Simple Java project analyzed with the SonarQube Runner
sonar.projectVersion=1.0
# Comma-separated paths to directories with sources (required)
sonar.sources=src
# Language
sonar.language=java
# Encoding of the source files
sonar.sourceEncoding=UTF-8
```

## Sonar 中支持

```
wget -O ./sonarqube-4.4/extensions/plugins/sonar-l10n-zh-plugin-1.8.jar http://repository.codehaus.org/org/codehaus/sonar-plugins/l10n/sonar-l10n-zh-plugin/1.8/sonar-l10n-zh-plugin-1.8.jar
./sonarqube-4.4/bin/macosx-universal-64/sonar.sh restart
```
