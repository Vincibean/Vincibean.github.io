---
title: The Shaded Documentation of Maven Shade Plugin
site_title: Test
site_logo: Test
site_description: Test
author: Vincibean
author_image: Test
author_bio: Test
---
Shit Happens. Especially when dealing with many dependencies in a complex system, shit happens.   
As Java developers, we usually don’t deal with all the intricacies of versioning; this is due to the fact that deprecated methods and classes are simply annotated as @Deprecated, but maintained for backward compatibility. Yet, there is no actual requirement in this sense, so it may happen that the new version of a library deletes a method or a class (instead of annotating it as deprecated). This is exactly what happened to me during these days: I had a piece of software that made use of Guava, I needed to upload that software to a Spark cluster, and the cluster already had its version of Guava. While this usually doesn’t lead to any error (you could keep the newer version), in the case of Guava it sometimes happens that [something is removed between a version and the other](http://docs.guava-libraries.googlecode.com/git/javadoc/deprecated-list.html). What should you do then? Keep the most updated one? You would lose those methods that, in the meantime, became deprecated and were removed. Keep the older one? You may lose those new methods on which your software might depend.   
What to do then?   
A possible solution is provided by the [Maven Shade Plugin](https://maven.apache.org/plugins/maven-shade-plugin/) and, in particular, by its [relocation feature](https://maven.apache.org/plugins/maven-shade-plugin/examples/class-relocation.html). Most of the tutorial you’ll find (or, at least, most of the tutorials I found) simply describe how to use the Maven Shade Plugin to create a [fat Jar](http://stackoverflow.com/a/29925421), that is, a jar which contains all the libraries on which your project depends; yet, this isn’t enough if you need two different versions of the same library; in fact, the version of the library included in such a fat Jar could still clash with the version already in place. Even the official guide lacks some good documentation that thoroughly describes this functionality. That’s why I came up with this post.    
In order to avoid possible dependency clashes, you should trick the system into thinking that the library included in your fat Jar is different from the one already in place. That’s where the relocation plugin comes into play. It requires you to define: 
* in field < pattern > , a pattern for the dependency/dependencies to shade. In my case, I wanted to shade Guava, whose package name starts with com.google.common; hence, I simply entered “com.google”
* in field < shadedPattern > , you are required to enter yet another pattern; this one will be used for replacing the previous one. For instance, I entered “com.shaded.google”; this means that Maven will: a) turn package “com.google” in package “com.shaded.google”; b) inspect your code and turn all package declarations starting with “com.google” in package declarations starting with “com.shaded.google”.

```xml
<plugin>
	<groupId>org.apache.maven.plugins</groupId>
	<artifactId>maven-shade-plugin</artifactId>
	<version>2.4.2</version>
	<executions>
		<execution>
			<goals>
				<goal>shade</goal>
			</goals>
		</execution>
	</executions>
	<configuration>
		<relocations>
			<relocation>
				<pattern>com.google</pattern>
				<shadedPattern>com.shaded.google</shadedPattern>
			</relocation>
		</relocations>
		<finalName>some-shaded-software-${project.version}</finalName>
		<filters>
			<filter>
				<artifact>*:*</artifact>
				<excludes>
					<exclude>META-INF/*.SF</exclude>
					<exclude>META-INF/*.DSA</exclude>
					<exclude>META-INF/*.RSA</exclude>
				</excludes>
			</filter>
		</filters>
	</configuration>
</plugin>
```

This means that, in our example, even if the cluster still retains its version of Guava (with a name like com.google.xxx), my piece of software will use a similar package with an artificially different name (com.shaded.google.xxx) and the system will interpret this as a whole new package and hence dependency clashes will be avoided.     
As you certainly noticed, this plugin requires much more information besides the pattern - shadedPattern part. To be honest, I found [here](https://maven.apache.org/plugins/maven-shade-plugin/shade-mojo.html) that the rest was decently documented. Here some explanation:
*	groupId, artifactId, version, execution, goal: if you ever used a Maven plugin, you should be used with these. In summary, they mean that we are using the artifact (plugin) “maven-shade-plugin” belonging to the set of maven plugins (“org.apache.maven.plugins”) and that we are using version 2.4.2; the goal of this plugin is to “shade”, i.e. to invoke the Mojo that performs shading delegating to the Shader component;
*	configuration, relocation: we saw this part before, while talking about the relocation feature;
*	finalName: the name that will be given to the resulting fat Jar; in our case, we are giving it the name “some-shaded-software” (very specific!) and appending the version of the project; the final name will be something like “some-shaded-software-1.0.2”;
*	filter: for fine-grained control of which classes from the selected dependencies will be included, we are using artifact filters. You can use wildcards for the artifact identity. In this case, it excludes all signature related files from every artifact, regardless of its group or artifact id.
And that’s it! Hope it helps!
