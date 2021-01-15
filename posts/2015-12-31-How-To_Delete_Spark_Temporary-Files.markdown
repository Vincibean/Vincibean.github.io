---
title: How To Delete Spark Job Server's Temporary Files
site_title: Test
site_description: Test
author: Vincibean
author_image: Test
author_bio: Test
---
My colleagues and I are working on a system that should interoperate with Apache Spark. The idea behind this interoperability
is to send [Spark Job Server](https://github.com/spark-jobserver/spark-jobserver) a JAR package containing classes that, whenever invoked from the system, should deal with Spark.
   The issue is, it sometimes happens that a new update to this JAR package doesn't update it at all! You probably are already 
seeing where this is leading: the JAR package is somewhat retained by Spark Job Server, which then uses the old classes. It is necessary to erase the old files, then. In a Unix-like system, this can be accomplished by stopping Spark Job Server, 
then firing the following command:
      
      rm -rf /tmp/spark-jobserver/*
      
Please note that your instance of Spark Job Server may save temporary files in a different location. Moreover we use a rather old 
version of Spark Job Server: newer version may require different commands (or not require them at all!)
