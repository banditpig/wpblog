---
ID: 160
post_title: Configuring an Embedded Tomcat Instance.
author: BanditPig
post_date: 2017-03-20 15:12:40
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/03/20/configuring-an-embedded-tomcat-instance/
published: true
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />An ongoing project I'm working on involves setting up SAML based Single Sign On (SSO). At the core of this task is a legacy REST system that uses Netty and has its own http request dispatch system along with a tightly coupled persistence framework. Essentially the system does not have an application server that could have been used to process SAML's Web Browser profile - a widely used means of obtaining authorisation from a SAML Identity Provider (IdP). I decided to investigate running Tomcat in embedded mode and so be able to use it for handling SAML messages.

So this post contains details of how to setup an embedded Tomcat (Tomcat 7.0.75) instance and have it host whatever web applications are needed. Later posts will give details on how to write and configure a SAML based IdP and also the steps needed to create and host a SSO Service Provider (SP).

The objective is to start an embedded Tomcat instance and load into it an (exploded) web app that is on the local filesystem in a location given by, say, "conf/webapp". This webapp will have servlets and jsp etc. specified by the usual web.xml and also have static content to be served from "conf/webapp". Furthermore the Tomcat instance should also work over https.

The first code snippet is the Maven dependencies:
<pre class="lang:default decode:true ">     &lt;properties&gt;
        &lt;tomcat.version&gt;7.0.75&lt;/tomcat.version&gt;
     &lt;/properties&gt;

     &lt;dependency&gt;
        &lt;groupId&gt;org.apache.tomcat.embed&lt;/groupId&gt;
        &lt;artifactId&gt;tomcat-embed-core&lt;/artifactId&gt;
        &lt;version&gt;${tomcat.version}&lt;/version&gt;
    &lt;/dependency&gt;
    &lt;dependency&gt;
        &lt;groupId&gt;org.apache.tomcat.embed&lt;/groupId&gt;
        &lt;artifactId&gt;tomcat-embed-logging-juli&lt;/artifactId&gt;
        &lt;version&gt;${tomcat.version}&lt;/version&gt;
    &lt;/dependency&gt;
    &lt;dependency&gt;
        &lt;groupId&gt;org.apache.tomcat.embed&lt;/groupId&gt;
        &lt;artifactId&gt;tomcat-embed-jasper&lt;/artifactId&gt;
        &lt;version&gt;${tomcat.version}&lt;/version&gt;
    &lt;/dependency&gt;
    &lt;dependency&gt;
        &lt;groupId&gt;org.apache.tomcat&lt;/groupId&gt;
        &lt;artifactId&gt;tomcat-jasper&lt;/artifactId&gt;
        &lt;version&gt;${tomcat.version}&lt;/version&gt;
    &lt;/dependency&gt;
    &lt;dependency&gt;
        &lt;groupId&gt;org.apache.tomcat&lt;/groupId&gt;
        &lt;artifactId&gt;tomcat-jasper-el&lt;/artifactId&gt;
        &lt;version&gt;${tomcat.version}&lt;/version&gt;
    &lt;/dependency&gt;
    &lt;dependency&gt;
        &lt;groupId&gt;org.apache.tomcat&lt;/groupId&gt;
        &lt;artifactId&gt;tomcat-jsp-api&lt;/artifactId&gt;
        &lt;version&gt;${tomcat.version}&lt;/version&gt;
    &lt;/dependency&gt;
    &lt;dependency&gt;
        &lt;groupId&gt;javax.servlet&lt;/groupId&gt;
        &lt;artifactId&gt;servlet-api&lt;/artifactId&gt;
        &lt;version&gt;2.5&lt;/version&gt;
        &lt;scope&gt;provided&lt;/scope&gt;
    &lt;/dependency&gt;
</pre>
The class TomcatRunner shows the code needed
<pre class="lang:java decode:true ">import org.apache.catalina.Context;
import org.apache.catalina.LifecycleException;
import org.apache.catalina.Service;
import org.apache.catalina.connector.Connector;
import org.apache.catalina.core.StandardContext;
import org.apache.catalina.startup.ContextConfig;
import org.apache.catalina.startup.Tomcat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.ServletException;
import java.io.File;


public class TomcatRunner {

  private static final Logger LOGGER = LoggerFactory.getLogger(TomcatRunner.class);


  private static Tomcat tomcat;
  public void startTomcat() throws LifecycleException {

    tomcat = new Tomcat()
    {
      @Override
      public Context addWebapp(String contextPath, String docBase) throws ServletException {
        Context context = null;
        try {
          context = new StandardContext();
          context.setName(contextPath);
          context.setPath(contextPath);
          context.setDocBase(docBase);
          context.setRealm(this.getHost().getRealm());
          ContextConfig contextConfig = new ContextConfig();
          context.addLifecycleListener(contextConfig);
          String pathToWebXml = docBase + "/web.xml";
          if (new File(pathToWebXml).exists()) {
            contextConfig.setDefaultWebXml(pathToWebXml);
          } else {
            contextConfig.setDefaultWebXml("org/apache/catalin/startup/NO_DEFAULT_XML");
          }
          host.addChild(context);
        } catch (Exception e) {
          LOGGER.error("Error deploying webapp", e);
        }
        return context;

      }
    };
    tomcat.setPort(8081);

    Service service = tomcat.getService();
    service.addConnector(sslConnector());



    try {
      tomcat.addWebapp("",new File("conf/ssoWebapp").getAbsolutePath());
      //add others here...
    } catch (ServletException e) {
      LOGGER.error("Problem loading SSO SP webapp. Could be fatal?");
    }

    tomcat.start();
    tomcat.getServer().await();

  }



  private static Connector sslConnector() {
    //These very much follow the server.xml values for a deployed tc.
    Connector connector = new Connector();
    connector.setPort(8443);
    connector.setSecure(true);
    connector.setScheme("https");
    connector.setAttribute("keystorePass", "password");
    connector.setAttribute("keystoreType", "JKS");
    connector.setAttribute("keystoreFile", "path to keystore..");
    connector.setAttribute("clientAuth",   "false");
    connector.setAttribute("protocol",     "HTTP/1.1");
    connector.setAttribute("sslProtocol",  "TLS");
    connector.setAttribute("maxThreads",   "200");
    connector.setAttribute("protocol",     "org.apache.coyote.http11.Http11NioProtocol");
    connector.setAttribute("SSLEnabled",   true);
    return connector;
  }

}
</pre>
and the key to it is the overridden method addWebapp which configures Tomcat with the "web.xml" held in "conf/webapp" as per the line:
<pre class="lang:java decode:true ">tomcat.addWebapp("",new File("conf/webapp").getAbsolutePath());
</pre>
The method sslConnector() creates the SSL settings and it very much follows what would be done when modifying Tomcats' server.xml file.

The final bit of the puzzle, serving static content, is enabled by having Tomcat's default servlet enabled for the serving of static content. This is done by having this snippet in the web.xml file.
<pre class="lang:xhtml decode:true ">&lt;servlet&gt;
    &lt;servlet-name&gt;default&lt;/servlet-name&gt;
    &lt;servlet-class&gt;org.apache.catalina.servlets.DefaultServlet&lt;/servlet-class&gt;
    &lt;init-param&gt;
        &lt;param-name&gt;debug&lt;/param-name&gt;
        &lt;param-value&gt;0&lt;/param-value&gt;
    &lt;/init-param&gt;
    &lt;init-param&gt;
        &lt;param-name&gt;listings&lt;/param-name&gt;
        &lt;param-value&gt;false&lt;/param-value&gt;
    &lt;/init-param&gt;
    &lt;load-on-startup&gt;1&lt;/load-on-startup&gt;
&lt;/servlet&gt;
&lt;servlet-mapping&gt;
    &lt;servlet-name&gt;default&lt;/servlet-name&gt;
    &lt;url-pattern&gt;/&lt;/url-pattern&gt;
&lt;/servlet-mapping&gt;</pre>
Finally, to start an embedded Tomcat, just create a new TomcatRunner and call the startTomcat() method. (In production you may want to consider only attempting to start Tomcat if one has not already been started.)

No doubt there are probably ways of doing all this programatically but personally I could not find complete documentation about setting up servlets, jsp pages, jsp error pages etc. in a programmatic way.