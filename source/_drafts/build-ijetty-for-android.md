title: 构建 android i-jetty
categories:
  - android
tags:
  - ijetty
date: 2014-08-29 14:19:20
---

本过程在数据中心的服务器上完成, 由于需要下载Android SDK, 需要耗费大量带宽,为了快速验证i-jetty能否通过编译,所以在服务器上完成此过程


1. 更新Android SDK

```
android update sdk --no-ui
```

2. 下载 `i-jetty`并修改多个`pom.xml`

```
git clone https://github.com/Arathi/i-jetty.git
```

编辑项目对象模型文件`pom.xml`:

```
cd i-jetty/i-jetty
vi pom.xml
```

修改 `<android.version>1.6_r2</android.version>` 为 `<android.version>4.1.1.4</android.version>`

其他支持的版本号可参考: http://repo2.maven.org/maven2/com/google/android/android/

把 `<android.sdk.version>4</android.sdk.version>` 改为 `<android.sdk.version>19</android.sdk.version>`


编辑项目文件 `i-jetty/i-jetty/i-jetty-ui/pom.xml` 找到 `build` 元素, 把`<platform>4</platform>`改为`<platform>19</platform>`

```
<build>
      <sourceDirectory>src</sourceDirectory>
      <plugins>
        <plugin>
          <groupId>com.jayway.maven.plugins.android.generation2</groupId>
          <artifactId>android-maven-plugin</artifactId>
          <version>3.8.0</version>
          <extensions>true</extensions>
          <configuration>
              <sdk>
                  <platform>19</platform> # 把这行的版本号改为19
              </sdk>
              <deleteConflictingFiles>true</deleteConflictingFiles>
              <extractDuplicates>true</extractDuplicates>
              <undeployBeforeDeploy>true</undeployBeforeDeploy>
          </configuration>
          <executions>
            <execution>
              <id>alignApk</id>
              <phase>package</phase>
              <goals>
                <goal>zipalign</goal>
              </goals>
            </execution>
          </executions>
         </plugin>
      </plugins>
   </build>
```

执行Build

```
mvn clean install
```

Android 项目中导入导入依赖`jar`包

1. 导入`i-jetty/i-jetty/i-jetty-ui/target/classes/unpacked-embedded-jars`所有的`jar`包
2. 导入`i-jetty/i-jetty/i-jetty-server/target/i-jetty-server-3.3.1.jar`



