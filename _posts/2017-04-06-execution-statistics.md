---
ID: 321
post_title: Execution Statistics.
author: BanditPig
post_date: 2017-04-06 19:02:03
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/04/06/execution-statistics/
published: true
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />Recently I needed  to find a way of logging the execution statistics of a number of methods in Java classes. The execution statistics included such things as number of invocations per unit of time, the longest and shortest execution time, the accumulated execution time per method etc. This needed to be done in a non-intrusive way - i.e. no direct modification of the code under examination.

This type of orthogonal processing what <a href="https://eclipse.org/aspectj/">AspectJ</a> is designed for. A way to use AspectJ for this is now described. The steps are
<ol>
 	<li>Setup Maven pom.xml with the required dependencies and the AspectJ build plugin.</li>
 	<li>Decide on what statistics are to be gathered.</li>
 	<li>Write a suitable annotation.</li>
 	<li>Create an AspectJ class to handle the annotation and output the statistics.</li>
</ol>
Here's the pom. It uses AspectJ 1.8.10 but referenced as a property for ease of modification.
<pre class="lang:xhtml decode:true " title="Maven pom.xml">&lt;?xml version="1.0" encoding="UTF-8"?&gt;
&lt;project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"&gt;
    &lt;modelVersion&gt;4.0.0&lt;/modelVersion&gt;

    &lt;groupId&gt;gitcommit.org&lt;/groupId&gt;
    &lt;artifactId&gt;aspects&lt;/artifactId&gt;
    &lt;version&gt;1.0&lt;/version&gt;
    &lt;packaging&gt;jar&lt;/packaging&gt;
    &lt;name&gt;aspects&lt;/name&gt;

    &lt;properties&gt;
        &lt;aspectj.version&gt;1.8.10&lt;/aspectj.version&gt;
    &lt;/properties&gt;


    &lt;dependencies&gt;
        &lt;dependency&gt;
            &lt;groupId&gt;org.aspectj&lt;/groupId&gt;
            &lt;artifactId&gt;aspectjrt&lt;/artifactId&gt;
            &lt;version&gt;1.8.10&lt;/version&gt;
        &lt;/dependency&gt;
        &lt;dependency&gt;
            &lt;groupId&gt;com.google.guava&lt;/groupId&gt;
            &lt;artifactId&gt;guava&lt;/artifactId&gt;
            &lt;version&gt;18.0&lt;/version&gt;
        &lt;/dependency&gt;
        &lt;dependency&gt;
            &lt;groupId&gt;org.slf4j&lt;/groupId&gt;
            &lt;artifactId&gt;slf4j-api&lt;/artifactId&gt;
            &lt;version&gt;1.7.2&lt;/version&gt;
        &lt;/dependency&gt;
        &lt;dependency&gt;
            &lt;groupId&gt;joda-time&lt;/groupId&gt;
            &lt;artifactId&gt;joda-time&lt;/artifactId&gt;
            &lt;version&gt;2.1&lt;/version&gt;
        &lt;/dependency&gt;
    &lt;/dependencies&gt;
    &lt;build&gt;
        &lt;plugins&gt;
            &lt;plugin&gt;
                &lt;groupId&gt;org.apache.maven.plugins&lt;/groupId&gt;
                &lt;artifactId&gt;maven-compiler-plugin&lt;/artifactId&gt;
                &lt;version&gt;3.1&lt;/version&gt;
                &lt;configuration&gt;
                    &lt;source&gt;1.8&lt;/source&gt;
                    &lt;target&gt;1.8&lt;/target&gt;
                    &lt;verbose&gt;true&lt;/verbose&gt;
                    &lt;fork&gt;true&lt;/fork&gt;
                &lt;/configuration&gt;
            &lt;/plugin&gt;
            &lt;plugin&gt;
                &lt;groupId&gt;org.codehaus.mojo&lt;/groupId&gt;
                &lt;artifactId&gt;aspectj-maven-plugin&lt;/artifactId&gt;
                &lt;version&gt;1.10&lt;/version&gt;
                &lt;configuration&gt;
                    &lt;source&gt;1.8&lt;/source&gt;
                    &lt;complianceLevel&gt;1.8&lt;/complianceLevel&gt;
                    &lt;includes&gt;
                        &lt;include&gt;**/*.java&lt;/include&gt;
                        &lt;include&gt;**/*.aj&lt;/include&gt;
                    &lt;/includes&gt;
                &lt;/configuration&gt;
                &lt;executions&gt;
                    &lt;execution&gt;
                        &lt;id&gt;compile_with_aspectj&lt;/id&gt;
                        &lt;goals&gt;
                            &lt;goal&gt;compile&lt;/goal&gt;
                        &lt;/goals&gt;
                    &lt;/execution&gt;
                    &lt;execution&gt;
                        &lt;id&gt;test-compile_with_aspectj&lt;/id&gt;
                        &lt;goals&gt;
                            &lt;goal&gt;test-compile&lt;/goal&gt;
                        &lt;/goals&gt;
                    &lt;/execution&gt;
                &lt;/executions&gt;
                &lt;dependencies&gt;
                    &lt;dependency&gt;
                        &lt;groupId&gt;org.aspectj&lt;/groupId&gt;
                        &lt;artifactId&gt;aspectjrt&lt;/artifactId&gt;
                        &lt;version&gt;${aspectj.version}&lt;/version&gt;
                    &lt;/dependency&gt;
                    &lt;dependency&gt;
                        &lt;groupId&gt;org.aspectj&lt;/groupId&gt;
                        &lt;artifactId&gt;aspectjtools&lt;/artifactId&gt;
                        &lt;version&gt;${aspectj.version}&lt;/version&gt;
                    &lt;/dependency&gt;
                &lt;/dependencies&gt;
            &lt;/plugin&gt;
        &lt;/plugins&gt;
    &lt;/build&gt;
&lt;/project&gt;
</pre>
The next piece of the puzzle is a class to represent the statistics.
<pre class="lang:java decode:true " title="MethodStatistics">package gitcommit.org;


import org.joda.time.DateTime;

/*
 This class hold basic data generated by timing method execution and accumulating repeated execution times.
 The method is the fully qualified method name and the invocationCount is the number of times the method is
 called over the samplePeriodDurationMS - measured in milliseconds.  The startTime and endTime   are
 the Linux absolute time values and there's corresponding human readable string values for these, i.e.
 startTimeStr and endTimeStr.
 The minimum and maximum values for one invocation are held in  minTimeNs and maxTimeNs and are in units of nanoseconds.
 The average execution time is also calculated and is in  avgTimeNs - again measured im nanoseconds.

  */
public class MethodStatistics    {

  private String method;
  private int invocationCount = 0;
  private long startTime = DateTime.now().getMillis();

  private long endTime;
  private long accumulatedTimeNs = 0;
  private long minTimeNs = Long.MAX_VALUE;
  private long maxTimeNs = 0;
  private long avgTimeNs = 0;
  private String startTimeStr;
  private String endTimeStr;
  private long samplePeriodDurationMs;

  public void updateValues(long time){

    invocationCount++;
    accumulatedTimeNs += time;
    if(time &lt; minTimeNs) minTimeNs = time;
    if(time &gt; maxTimeNs) maxTimeNs = time;

  }

  public void fixValues(String method){
    this.method = method;
    DateTime now = DateTime.now();
    endTimeStr = now.toString();
    endTime = now.getMillis();
    startTimeStr = new DateTime(startTime).toString();
    samplePeriodDurationMs = endTime - startTime;
    if(invocationCount != 0){
      avgTimeNs = accumulatedTimeNs / invocationCount;
    }
  }

  @Override
  public String toString() {
    return "MethodStatistics{" +
      "method='" + method + '\'' +
      "startTimeStr='" + startTimeStr + '\'' +
      ", endTimeStr='" + endTimeStr + '\'' +
      ", samplePeriodDurationMs=" + samplePeriodDurationMs +
      ", avgTimeNs=" + fmt(avgTimeNs) +
      ", maxTimeNs=" + fmt(maxTimeNs)+
      ", minTimeNs=" + fmt(minTimeNs) +
      ", accumulatedTimeNs=" +  fmt(accumulatedTimeNs) +
      ", endTime=" + endTime +
      ", startTime=" + startTime +
      ", invocationCount=" + invocationCount +

      '}';
  }

  //neat way of formatting to 3 dp.
  private double fmt(long t ){
    double v = (double) t / 1000000000.0;
    return (double)Math.round(v * 1000d) / 1000d;
  }
}
</pre>
This is the annotation. As can be seen it is applied to Methods and retained at runtime. The '<em>writePeriod</em>' determines how often the accumulated statistics are output.
<pre class="lang:java decode:true " title="The annotation">package gitcommit.org;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to measure the execution times of methods. Apply the annotation to the method and supply
 * a value for the writePeriod and the execution results - held in MethodStatistics will appear
 * in mongo every writePeriod seconds.
 * See {@link MethodStatistics}
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Monitor{

  public int writePeriod();   //in seconds
}
</pre>
Next is the Aspect which dictates how AspectJ applies itself.
<pre class="lang:java decode:true " title="The Aspect">package gitcommit.org;

import com.google.common.collect.Sets;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;


import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
Aspect to monitor method execution times.
See {@link MethodStatistics} and {@link Monitor}

 */
@Aspect
public class MonitorAspect {

  private static ConcurrentHashMap&lt;String, MethodStatistics&gt; statsMap = new ConcurrentHashMap&lt;&gt;();
  //two threads or half of avaliable cores
  private static ScheduledExecutorService scheduler =  Executors.newScheduledThreadPool(Math.max(2, Runtime.getRuntime().availableProcessors() / 2));
  private static Set timerNames = Sets.newConcurrentHashSet();

  //Just a simple println of the stats. A more practical mechanism would be to save to a database.
  private StatsSaver statsSaver = new StatsSaver() {
    @Override
    public void saveStats(MethodStatistics methodStatistics) {

      System.out.println(methodStatistics);
    }
  };

  @Around("execution(* *(..)) &amp;&amp; @annotation(monitor)")
  public Object around(ProceedingJoinPoint point, Monitor monitor) throws Throwable {
    final long startTimeNano = System.nanoTime();

    try {
      return point.proceed();
    }
    finally {

      final long endTimeNano = System.nanoTime();
      final long execTimeNano = endTimeNano - startTimeNano;
      MethodSignature signature = (MethodSignature) point.getSignature();
      String name = signature.getDeclaringTypeName()+ "." + signature.getName();

      MethodStatistics methodStatistics = statsMap.get(name);
      if(methodStatistics == null){
        methodStatistics = new MethodStatistics();
        statsMap.put(name, methodStatistics);
      }
      methodStatistics.updateValues(execTimeNano);

      //do we have a timer?
      if( ! timerNames.contains(name)){
        timerNames.add(name);
        StatsWriter writer = new StatsWriter(name);
        scheduler.scheduleWithFixedDelay(writer, monitor.writePeriod(), monitor.writePeriod(), TimeUnit.SECONDS);
      }
    }
  }
  private class StatsWriter implements Runnable{

    private final String methodName;

    public StatsWriter(String methodName) {
      this.methodName = methodName;
    }

    @Override
    public void run() {
      MethodStatistics methodStatistics = statsMap.remove(methodName);
      if(methodStatistics != null) {
        methodStatistics.fixValues(methodName);
        statsSaver.saveStats(methodStatistics);
      }
    }
  }
}
</pre>
The key part to this Aspect is the line
<span class="lang:java decode:true crayon-inline ">@Around("execution(* *(..)) &amp;&amp; @annotation(monitor)")</span> which says that for any method, in any package, taking any parameters and with any return type that has the Monitor annotation then execute the following '<em>around</em>' method. The '<em>point.proceed</em>' line allows the monitored method to execute but the '<em>finally</em>' section derives the statistics and creates, if needed, a thread that is run every '<em>monitor.writePeriod()</em>' seconds and it uses the '<em>StatsWriter</em>' class to delegate output of the statistics to a '<em>StatsSaver</em>'. All that remains now is to apply the annotation to wherever its is needed. As this is just simple demonstration code the statistics are just printed to the console. In production I used this to write to a MongoDB collection that had a <a href="https://docs.mongodb.com/manual/core/index-ttl/">time-to-live on the index.</a>
To try this out I created a rough and ready class with two methods annotated with '<em>@Monitor</em>'.
<pre class="lang:java decode:true ">package gitcommit.org;

public class StatsDemo {
    private int num = 0;

    @Monitor(writePeriod = 1)
    public void incNum(){

        num++;
        try {
            Thread.sleep((long)(Math.random() * 1000));
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    @Monitor(writePeriod = 3)
    public void decNum(){
        num++;

        try {

            Thread.sleep((long)(Math.random() * 2000));
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    public static void main(String[] args) {

        StatsDemo statsDemo = new StatsDemo();
        for (int i=0;i&lt;20; i++){
         statsDemo.decNum();
         statsDemo.incNum();
        }
        System.exit(0);
    }
}
</pre>
and the results
<pre class="lang:haskell decode:true">MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.462, maxTime=0.462, minTime=0.462, accumulatedTime=0.462, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.208, maxTime=0.208, minTime=0.208, accumulatedTime=0.208, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.decNum', avgTime=1.023, maxTime=1.688, minTime=0.308, accumulatedTime=3.07, invocationCount=3}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.044, maxTime=0.044, minTime=0.044, accumulatedTime=0.044, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.192, maxTime=0.192, minTime=0.192, accumulatedTime=0.192, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.decNum', avgTime=2.002, maxTime=2.002, minTime=2.002, accumulatedTime=2.002, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.355, maxTime=0.355, minTime=0.355, accumulatedTime=0.355, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.696, maxTime=0.696, minTime=0.696, accumulatedTime=0.696, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.decNum', avgTime=0.893, maxTime=1.268, minTime=0.286, accumulatedTime=2.68, invocationCount=3}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.287, maxTime=0.347, minTime=0.227, accumulatedTime=0.574, invocationCount=2}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.285, maxTime=0.285, minTime=0.285, accumulatedTime=0.285, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.decNum', avgTime=0.667, maxTime=1.72, minTime=0.03, accumulatedTime=2.002, invocationCount=3}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.848, maxTime=0.848, minTime=0.848, accumulatedTime=0.848, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.29, maxTime=0.29, minTime=0.29, accumulatedTime=0.29, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.decNum', avgTime=0.968, maxTime=1.629, minTime=0.306, accumulatedTime=1.935, invocationCount=2}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.582, maxTime=0.582, minTime=0.582, accumulatedTime=0.582, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.349, maxTime=0.349, minTime=0.349, accumulatedTime=0.349, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.decNum', avgTime=1.367, maxTime=1.367, minTime=1.367, accumulatedTime=1.367, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.79, maxTime=0.79, minTime=0.79, accumulatedTime=0.79, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.decNum', avgTime=1.197, maxTime=1.222, minTime=1.171, accumulatedTime=2.393, invocationCount=2}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.543, maxTime=0.543, minTime=0.543, accumulatedTime=0.543, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.669, maxTime=0.669, minTime=0.669, accumulatedTime=0.669, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.decNum', avgTime=0.902, maxTime=1.346, minTime=0.459, accumulatedTime=1.804, invocationCount=2}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.838, maxTime=0.838, minTime=0.838, accumulatedTime=0.838, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.722, maxTime=0.722, minTime=0.722, accumulatedTime=0.722, invocationCount=1}
MethodStatistics{method='gitcommit.org.StatsDemo.decNum', avgTime=0.953, maxTime=1.316, minTime=0.591, accumulatedTime=1.907, invocationCount=2}
MethodStatistics{method='gitcommit.org.StatsDemo.incNum', avgTime=0.7, maxTime=0.7, minTime=0.7, accumulatedTime=0.7, invocationCount=1}
</pre>