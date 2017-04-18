---
ID: 140
post_title: Java Dockerfile.
author: BanditPig
post_date: 2017-04-18 21:53:04
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/04/18/java-dockerfile/
published: true
---
Docker - what's not to like.
Here's a docker file that installs Java 8 over an Ubuntu base.
It also installs Git and Maven. It takes a little while to build but does provide a full JDK with git and maven. 
<pre class="lang:sh decode:true "># Ubuntu  with Java 8 installed.
# Build image with:  docker build  -f Dockerfile_Java8  --rm=true -t=java8
# Run with: docker run -it java8
FROM ubuntu

MAINTAINER Mike
# Using &amp;&amp; prevents intermediate containers being generated
RUN apt-get update &amp;&amp; \
    apt-get upgrade -y &amp;&amp; \
    apt-get install -y  software-properties-common &amp;&amp; \
    add-apt-repository ppa:webupd8team/java -y &amp;&amp; \
    apt-get update &amp;&amp; \
    echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections &amp;&amp; \
    apt-get install -y oracle-java8-installer &amp;&amp; \
    apt-get clean
  
RUN apt-get install -y git &amp;&amp;  apt-get install -y maven
ENV JAVA_HOME /usr/lib/jvm/java-8-oracle
# Default command
CMD ["bash"]</pre>

To help remove unwanted images and stop any and all running containers here are two bash scripts.
 
<pre class="lang:haskell decode:true " title="dockerPurge.sh" >#!/bin/bash
#
# removes all images
docker rm -f $(docker ps -a -q)
docker rmi $(docker images -q)
</pre> 

 
<pre class="lang:sh decode:true " title="dockerStop.sh" >#!/bin/bash
#
# force stop any running containers
docker rm -f $(docker ps -a -q)
</pre>