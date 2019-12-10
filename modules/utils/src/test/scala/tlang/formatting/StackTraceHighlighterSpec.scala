package tlang
package formatting

import tlang.formatting.textformatters.StackTraceHighlighter
import tlang.testutils.UnitSpec
import tlang.testutils.snapshot.SnapshotTesting

class StackTraceHighlighterSpec extends UnitSpec with SnapshotTesting {

  behavior of "A stack trace highlighter"

  it should "highlight stacktraces" in {
    val stackTraceHighlighter = StackTraceHighlighter(failOnError = true)(Formatter.PrettyFormatter)

    stackTraceHighlighter(
      """|Exception in thread "main" java.lang.RuntimeException: Compilation Failed with an unknown error
         |The error continued over multiple lines
         |ABCDEFG
         |
         |
         |	at scala.sys.package$.error(package.scala:27)
         |	at tlang.compiler.Main$.unknownError(Main.scala:148)
         |	at tlang.compiler.Main$.runCompiler(Main.scala:121)
         |	at tlang.compiler.Main$.main(Main.scala:92)
         |	at tlang.compiler.Main.main(Main.scala)
         |""".stripMargin
    ) should matchSnapshot

    stackTraceHighlighter(
      """|javax.servlet.ServletException: Something bad happened
         |	at com.example.myproject.OpenSessionInViewFilter.doFilter(OpenSessionInViewFilter.java:60)
         |	at org.mortbay.jetty.servlet.ServletHandler$CachedChain.doFilter(ServletHandler.java:1157)
         |	at com.example.myproject.ExceptionHandlerFilter.doFilter(ExceptionHandlerFilter.java:28)
         |	at org.mortbay.jetty.servlet.ServletHandler$CachedChain.doFilter(ServletHandler.java:1157)
         |	at com.example.myproject.OutputBufferFilter.doFilter(OutputBufferFilter.java:33)
         |	at org.mortbay.jetty.servlet.ServletHandler$CachedChain.doFilter(ServletHandler.java:1157)
         |	at org.mortbay.jetty.servlet.ServletHandler.handle(ServletHandler.java:388)
         |	at org.mortbay.jetty.security.SecurityHandler.handle(SecurityHandler.java:216)
         |	at org.mortbay.jetty.servlet.SessionHandler.handle(SessionHandler.java:182)
         |	at org.mortbay.jetty.handler.ContextHandler.handle(ContextHandler.java:765)
         |	at org.mortbay.jetty.webapp.WebAppContext.handle(WebAppContext.java:418)
         |	at org.mortbay.jetty.handler.HandlerWrapper.handle(HandlerWrapper.java:152)
         |	at org.mortbay.jetty.Server.handle(Server.java:326)
         |	at org.mortbay.jetty.HttpConnection.handleRequest(HttpConnection.java:542)
         |	at org.mortbay.jetty.HttpConnection$RequestHandler.content(HttpConnection.java:943)
         |	at org.mortbay.jetty.HttpParser.parseNext(HttpParser.java:756)
         |	at org.mortbay.jetty.HttpParser.parseAvailable(HttpParser.java:218)
         |	at org.mortbay.jetty.HttpConnection.handle(HttpConnection.java:404)
         |	at org.mortbay.jetty.bio.SocketConnector$Connection.run(SocketConnector.java:228)
         |	at org.mortbay.thread.QueuedThreadPool$PoolThread.run(QueuedThreadPool.java:582)
         |Caused by: com.example.myproject.MyProjectServletException
         |	at com.example.myproject.MyServlet.doPost(MyServlet.java:169)
         |	at javax.servlet.http.HttpServlet.service(HttpServlet.java:727)
         |	at javax.servlet.http.HttpServlet.service(HttpServlet.java:820)
         |	at org.mortbay.jetty.servlet.ServletHolder.handle(ServletHolder.java:511)
         |	at org.mortbay.jetty.servlet.ServletHandler$CachedChain.doFilter(ServletHandler.java:1166)
         |	at com.example.myproject.OpenSessionInViewFilter.doFilter(OpenSessionInViewFilter.java:30)
         |	... 27 more
         |Caused by: org.hibernate.exception.ConstraintViolationException: could not insert: [com.example.myproject.MyEntity]
         |	at org.hibernate.exception.SQLStateConverter.convert(SQLStateConverter.java:96)
         |	at org.hibernate.exception.JDBCExceptionHelper.convert(JDBCExceptionHelper.java:66)
         |	at org.hibernate.id.insert.AbstractSelectingDelegate.performInsert(AbstractSelectingDelegate.java:64)
         |	at org.hibernate.persister.entity.AbstractEntityPersister.insert(AbstractEntityPersister.java:2329)
         |	at org.hibernate.persister.entity.AbstractEntityPersister.insert(AbstractEntityPersister.java:2822)
         |	at org.hibernate.action.EntityIdentityInsertAction.execute(EntityIdentityInsertAction.java:71)
         |	at org.hibernate.engine.ActionQueue.execute(ActionQueue.java:268)
         |	at org.hibernate.event.def.AbstractSaveEventListener.performSaveOrReplicate(AbstractSaveEventListener.java:321)
         |	at org.hibernate.event.def.AbstractSaveEventListener.performSave(AbstractSaveEventListener.java:204)
         |	at org.hibernate.event.def.AbstractSaveEventListener.saveWithGeneratedId(AbstractSaveEventListener.java:130)
         |	at org.hibernate.event.def.DefaultSaveOrUpdateEventListener.saveWithGeneratedOrRequestedId(DefaultSaveOrUpdateEventListener.java:210)
         |	at org.hibernate.event.def.DefaultSaveEventListener.saveWithGeneratedOrRequestedId(DefaultSaveEventListener.java:56)
         |	at org.hibernate.event.def.DefaultSaveOrUpdateEventListener.entityIsTransient(DefaultSaveOrUpdateEventListener.java:195)
         |	at org.hibernate.event.def.DefaultSaveEventListefner.performSaveOrUpdate(DefaultSaveEventListener.java:50)
         |	at org.hibernate.event.def.DefaultSaveOrUpdateEventListener.onSaveOrUpdate(DefaultSaveOrUpdateEventListener.java:93)
         |	at org.hibernate.impl.SessionImpl.fireSave(SessionImpl.java:705)
         |	at org.hibernate.impl.SessionImpl.save(SessionImpl.java:693)
         |	at org.hibernate.impl.SessionImpl.save(SessionImpl.java:689)
         |	at sun.reflect.GeneratedMethodAccessor5.invoke(Unknown Source)
         |	at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)
         |	at java.lang.reflect.Method.invoke(Method.java:597)
         |	at org.hibernate.context.ThreadLocalSessionContext$TransactionProtectionWrapper.invoke(ThreadLocalSessionContext.java:344)
         |	at $Proxy19.save(Unknown Source)
         |	at com.example.myproject.MyEntityService.save(MyEntityService.java:59)
         |	at com.example.myproject.MyServlet.doPost(MyServlet.java:164)
         |	... 32 more
         |Caused by: java.sql.SQLException: Violation of unique constraint MY_ENTITY_UK_1: duplicate value(s) for column(s) MY_COLUMN in statement [...]
         |	at org.hsqldb.jdbc.Util.throwError(Unknown Source)
         |	at org.hsqldb.jdbc.jdbcPreparedStatement.executeUpdate(Unknown Source)
         |	at com.mchange.v2.c3p0.impl.NewProxyPreparedStatement.executeUpdate(NewProxyPreparedStatement.java:105)
         |	at org.hibernate.id.insert.AbstractSelectingDelegate.performInsert(AbstractSelectingDelegate.java:57)
         |	... 54 more
         |""".stripMargin
    ) should matchSnapshot

    stackTraceHighlighter(
      """|org.springframework.context.ApplicationContextException: Unable to start embedded container; nested exception is org.springframework.boot.context.embedded.EmbeddedServletContainerException: Unable to start embedded Tomcat
         |	at org.springframework.boot.context.embedded.EmbeddedWebApplicationContext.onRefresh(EmbeddedWebApplicationContext.java:133) [spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.context.support.AbstractApplicationContext.refresh(AbstractApplicationContext.java:474) ~[spring-context-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.boot.context.embedded.EmbeddedWebApplicationContext.refresh(EmbeddedWebApplicationContext.java:118) [spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:686) [spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.SpringApplication.run(SpringApplication.java:320) [spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at de.sveri.mct.Application.main(Application.java:72) [main/:na]
         |Caused by: org.springframework.boot.context.embedded.EmbeddedServletContainerException: Unable to start embedded Tomcat
         |	at org.springframework.boot.context.embedded.tomcat.TomcatEmbeddedServletContainer.initialize(TomcatEmbeddedServletContainer.java:98) ~[spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.tomcat.TomcatEmbeddedServletContainer.<init>(TomcatEmbeddedServletContainer.java:75) ~[spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.tomcat.TomcatEmbeddedServletContainerFactory.getTomcatEmbeddedServletContainer(TomcatEmbeddedServletContainerFactory.java:378) ~[spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.tomcat.TomcatEmbeddedServletContainerFactory.getEmbeddedServletContainer(TomcatEmbeddedServletContainerFactory.java:155) ~[spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.EmbeddedWebApplicationContext.createEmbeddedServletContainer(EmbeddedWebApplicationContext.java:157) [spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.EmbeddedWebApplicationContext.onRefresh(EmbeddedWebApplicationContext.java:130) [spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	... 5 common frames omitted
         |Caused by: org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'org.springframework.security.config.annotation.web.configuration.WebSecurityConfiguration': Injection of autowired dependencies failed; nested exception is org.springframework.beans.factory.BeanCreationException: Could not autowire method: public void org.springframework.security.config.annotation.web.configuration.WebSecurityConfiguration.setFilterChainProxySecurityConfigurer(org.springframework.security.config.annotation.ObjectPostProcessor,java.util.List) throws java.lang.Exception; nested exception is org.springframework.beans.factory.BeanExpressionException: Expression parsing failed; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'securityConfiguration': Injection of autowired dependencies failed; nested exception is org.springframework.beans.factory.BeanCreationException: Could not autowire field: private org.springframework.security.core.userdetails.UserDetailsService de.sveri.mct.config.SecurityConfiguration.userDetailsService; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userDetailsService': Injection of autowired dependencies failed; nested exception is org.springframework.beans.factory.BeanCreationException: Could not autowire field: private de.sveri.mct.repository.UserRepository de.sveri.mct.security.UserDetailsService.userRepository; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userRepository': Cannot create inner bean '(inner bean)#58e814e6' of type [org.springframework.orm.jpa.SharedEntityManagerCreator] while setting bean property 'entityManager'; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name '(inner bean)#58e814e6': Cannot resolve reference to bean 'entityManagerFactory' while setting constructor argument; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'entityManagerFactory' defined in class path resource [org/springframework/boot/autoconfigure/orm/jpa/HibernateJpaAutoConfiguration.class]: Invocation of init method failed; nested exception is javax.persistence.PersistenceException: [PersistenceUnit: default] Unable to build Hibernate SessionFactory
         |	at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor.postProcessPropertyValues(AutowiredAnnotationBeanPostProcessor.java:334) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.populateBean(AbstractAutowireCapableBeanFactory.java:1210) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:537) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:476) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory$1.getObject(AbstractBeanFactory.java:303) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:230) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:299) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:194) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.ConstructorResolver.instantiateUsingFactoryMethod(ConstructorResolver.java:368) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.instantiateUsingFactoryMethod(AbstractAutowireCapableBeanFactory.java:1119) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBeanInstance(AbstractAutowireCapableBeanFactory.java:1014) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:504) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:476) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory$1.getObject(AbstractBeanFactory.java:303) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:230) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:299) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:199) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.boot.context.embedded.ServletContextInitializerBeans.getOrderedBeansOfType(ServletContextInitializerBeans.java:209) ~[spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.ServletContextInitializerBeans.addAsRegistrationBean(ServletContextInitializerBeans.java:165) ~[spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.ServletContextInitializerBeans.addAsRegistrationBean(ServletContextInitializerBeans.java:160) ~[spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.ServletContextInitializerBeans.addAdaptableBeans(ServletContextInitializerBeans.java:143) ~[spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.ServletContextInitializerBeans.<init>(ServletContextInitializerBeans.java:74) ~[spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.EmbeddedWebApplicationContext.getServletContextInitializerBeans(EmbeddedWebApplicationContext.java:234) [spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.EmbeddedWebApplicationContext.selfInitialize(EmbeddedWebApplicationContext.java:221) [spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.EmbeddedWebApplicationContext.access$000(EmbeddedWebApplicationContext.java:84) [spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.EmbeddedWebApplicationContext$1.onStartup(EmbeddedWebApplicationContext.java:206) ~[spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.springframework.boot.context.embedded.tomcat.TomcatStarter.onStartup(TomcatStarter.java:54) ~[spring-boot-1.2.4.RELEASE.jar:1.2.4.RELEASE]
         |	at org.apache.catalina.core.StandardContext.startInternal(StandardContext.java:5156) ~[tomcat-embed-core-8.0.23.jar:8.0.23]
         |	at org.apache.catalina.util.LifecycleBase.start(LifecycleBase.java:150) ~[tomcat-embed-core-8.0.23.jar:8.0.23]
         |	at org.apache.catalina.core.ContainerBase$StartChild.call(ContainerBase.java:1409) ~[tomcat-embed-core-8.0.23.jar:8.0.23]
         |	at org.apache.catalina.core.ContainerBase$StartChild.call(ContainerBase.java:1399) ~[tomcat-embed-core-8.0.23.jar:8.0.23]
         |	at java.util.concurrent.FutureTask.run(FutureTask.java:266) ~[na:1.8.0_25]
         |	at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1142) ~[na:1.8.0_25]
         |	at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:617) ~[na:1.8.0_25]
         |	at java.lang.Thread.run(Thread.java:745) ~[na:1.8.0_25]
         |Caused by: org.springframework.beans.factory.BeanCreationException: Could not autowire method: public void org.springframework.security.config.annotation.web.configuration.WebSecurityConfiguration.setFilterChainProxySecurityConfigurer(org.springframework.security.config.annotation.ObjectPostProcessor,java.util.List) throws java.lang.Exception; nested exception is org.springframework.beans.factory.BeanExpressionException: Expression parsing failed; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'securityConfiguration': Injection of autowired dependencies failed; nested exception is org.springframework.beans.factory.BeanCreationException: Could not autowire field: private org.springframework.security.core.userdetails.UserDetailsService de.sveri.mct.config.SecurityConfiguration.userDetailsService; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userDetailsService': Injection of autowired dependencies failed; nested exception is org.springframework.beans.factory.BeanCreationException: Could not autowire field: private de.sveri.mct.repository.UserRepository de.sveri.mct.security.UserDetailsService.userRepository; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userRepository': Cannot create inner bean '(inner bean)#58e814e6' of type [org.springframework.orm.jpa.SharedEntityManagerCreator] while setting bean property 'entityManager'; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name '(inner bean)#58e814e6': Cannot resolve reference to bean 'entityManagerFactory' while setting constructor argument; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'entityManagerFactory' defined in class path resource [org/springframework/boot/autoconfigure/orm/jpa/HibernateJpaAutoConfiguration.class]: Invocation of init method failed; nested exception is javax.persistence.PersistenceException: [PersistenceUnit: default] Unable to build Hibernate SessionFactory
         |	at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor$AutowiredMethodElement.inject(AutowiredAnnotationBeanPostProcessor.java:649) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.annotation.InjectionMetadata.inject(InjectionMetadata.java:88) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor.postProcessPropertyValues(AutowiredAnnotationBeanPostProcessor.java:331) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	... 34 common frames omitted
         |Caused by: org.springframework.beans.factory.BeanExpressionException: Expression parsing failed; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'securityConfiguration': Injection of autowired dependencies failed; nested exception is org.springframework.beans.factory.BeanCreationException: Could not autowire field: private org.springframework.security.core.userdetails.UserDetailsService de.sveri.mct.config.SecurityConfiguration.userDetailsService; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userDetailsService': Injection of autowired dependencies failed; nested exception is org.springframework.beans.factory.BeanCreationException: Could not autowire field: private de.sveri.mct.repository.UserRepository de.sveri.mct.security.UserDetailsService.userRepository; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userRepository': Cannot create inner bean '(inner bean)#58e814e6' of type [org.springframework.orm.jpa.SharedEntityManagerCreator] while setting bean property 'entityManager'; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name '(inner bean)#58e814e6': Cannot resolve reference to bean 'entityManagerFactory' while setting constructor argument; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'entityManagerFactory' defined in class path resource [org/springframework/boot/autoconfigure/orm/jpa/HibernateJpaAutoConfiguration.class]: Invocation of init method failed; nested exception is javax.persistence.PersistenceException: [PersistenceUnit: default] Unable to build Hibernate SessionFactory
         |	at org.springframework.context.expression.StandardBeanExpressionResolver.evaluate(StandardBeanExpressionResolver.java:164) ~[spring-context-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.evaluateBeanDefinitionString(AbstractBeanFactory.java:1365) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultListableBeanFactory.doResolveDependency(DefaultListableBeanFactory.java:957) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultListableBeanFactory.resolveDependency(DefaultListableBeanFactory.java:942) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor$AutowiredMethodElement.inject(AutowiredAnnotationBeanPostProcessor.java:606) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	... 36 common frames omitted
         |Caused by: org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'securityConfiguration': Injection of autowired dependencies failed; nested exception is org.springframework.beans.factory.BeanCreationException: Could not autowire field: private org.springframework.security.core.userdetails.UserDetailsService de.sveri.mct.config.SecurityConfiguration.userDetailsService; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userDetailsService': Injection of autowired dependencies failed; nested exception is org.springframework.beans.factory.BeanCreationException: Could not autowire field: private de.sveri.mct.repository.UserRepository de.sveri.mct.security.UserDetailsService.userRepository; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userRepository': Cannot create inner bean '(inner bean)#58e814e6' of type [org.springframework.orm.jpa.SharedEntityManagerCreator] while setting bean property 'entityManager'; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name '(inner bean)#58e814e6': Cannot resolve reference to bean 'entityManagerFactory' while setting constructor argument; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'entityManagerFactory' defined in class path resource [org/springframework/boot/autoconfigure/orm/jpa/HibernateJpaAutoConfiguration.class]: Invocation of init method failed; nested exception is javax.persistence.PersistenceException: [PersistenceUnit: default] Unable to build Hibernate SessionFactory
         |	at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor.postProcessPropertyValues(AutowiredAnnotationBeanPostProcessor.java:334) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.populateBean(AbstractAutowireCapableBeanFactory.java:1210) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:537) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:476) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory$1.getObject(AbstractBeanFactory.java:303) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:230) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:299) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:199) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultListableBeanFactory.getBeansOfType(DefaultListableBeanFactory.java:523) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultListableBeanFactory.getBeansOfType(DefaultListableBeanFactory.java:512) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.security.config.annotation.web.configuration.AutowiredWebSecurityConfigurersIgnoreParents.getWebSecurityConfigurers(AutowiredWebSecurityConfigurersIgnoreParents.java:52) ~[spring-security-config-4.0.1.RELEASE.jar:4.0.1.RELEASE]
         |	at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method) ~[na:1.8.0_25]
         |	at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62) ~[na:1.8.0_25]
         |	at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43) ~[na:1.8.0_25]
         |	at java.lang.reflect.Method.invoke(Method.java:483) ~[na:1.8.0_25]
         |	at org.springframework.expression.spel.support.ReflectiveMethodExecutor.execute(ReflectiveMethodExecutor.java:112) ~[spring-expression-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.expression.spel.ast.MethodReference.getValueInternal(MethodReference.java:129) ~[spring-expression-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.expression.spel.ast.MethodReference.access$000(MethodReference.java:49) ~[spring-expression-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.expression.spel.ast.MethodReference$MethodValueRef.getValue(MethodReference.java:342) ~[spring-expression-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.expression.spel.ast.CompoundExpression.getValueInternal(CompoundExpression.java:88) ~[spring-expression-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.expression.spel.ast.SpelNodeImpl.getValue(SpelNodeImpl.java:120) ~[spring-expression-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.expression.spel.standard.SpelExpression.getValue(SpelExpression.java:242) ~[spring-expression-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.context.expression.StandardBeanExpressionResolver.evaluate(StandardBeanExpressionResolver.java:161) ~[spring-context-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	... 40 common frames omitted
         |Caused by: org.springframework.beans.factory.BeanCreationException: Could not autowire field: private org.springframework.security.core.userdetails.UserDetailsService de.sveri.mct.config.SecurityConfiguration.userDetailsService; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userDetailsService': Injection of autowired dependencies failed; nested exception is org.springframework.beans.factory.BeanCreationException: Could not autowire field: private de.sveri.mct.repository.UserRepository de.sveri.mct.security.UserDetailsService.userRepository; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userRepository': Cannot create inner bean '(inner bean)#58e814e6' of type [org.springframework.orm.jpa.SharedEntityManagerCreator] while setting bean property 'entityManager'; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name '(inner bean)#58e814e6': Cannot resolve reference to bean 'entityManagerFactory' while setting constructor argument; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'entityManagerFactory' defined in class path resource [org/springframework/boot/autoconfigure/orm/jpa/HibernateJpaAutoConfiguration.class]: Invocation of init method failed; nested exception is javax.persistence.PersistenceException: [PersistenceUnit: default] Unable to build Hibernate SessionFactory
         |	at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor$AutowiredFieldElement.inject(AutowiredAnnotationBeanPostProcessor.java:561) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.annotation.InjectionMetadata.inject(InjectionMetadata.java:88) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor.postProcessPropertyValues(AutowiredAnnotationBeanPostProcessor.java:331) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	... 62 common frames omitted
         |Caused by: org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userDetailsService': Injection of autowired dependencies failed; nested exception is org.springframework.beans.factory.BeanCreationException: Could not autowire field: private de.sveri.mct.repository.UserRepository de.sveri.mct.security.UserDetailsService.userRepository; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userRepository': Cannot create inner bean '(inner bean)#58e814e6' of type [org.springframework.orm.jpa.SharedEntityManagerCreator] while setting bean property 'entityManager'; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name '(inner bean)#58e814e6': Cannot resolve reference to bean 'entityManagerFactory' while setting constructor argument; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'entityManagerFactory' defined in class path resource [org/springframework/boot/autoconfigure/orm/jpa/HibernateJpaAutoConfiguration.class]: Invocation of init method failed; nested exception is javax.persistence.PersistenceException: [PersistenceUnit: default] Unable to build Hibernate SessionFactory
         |	at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor.postProcessPropertyValues(AutowiredAnnotationBeanPostProcessor.java:334) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.populateBean(AbstractAutowireCapableBeanFactory.java:1210) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:537) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:476) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory$1.getObject(AbstractBeanFactory.java:303) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:230) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:299) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:194) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultListableBeanFactory.findAutowireCandidates(DefaultListableBeanFactory.java:1120) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultListableBeanFactory.doResolveDependency(DefaultListableBeanFactory.java:1044) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultListableBeanFactory.resolveDependency(DefaultListableBeanFactory.java:942) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor$AutowiredFieldElement.inject(AutowiredAnnotationBeanPostProcessor.java:533) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	... 64 common frames omitted
         |Caused by: org.springframework.beans.factory.BeanCreationException: Could not autowire field: private de.sveri.mct.repository.UserRepository de.sveri.mct.security.UserDetailsService.userRepository; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userRepository': Cannot create inner bean '(inner bean)#58e814e6' of type [org.springframework.orm.jpa.SharedEntityManagerCreator] while setting bean property 'entityManager'; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name '(inner bean)#58e814e6': Cannot resolve reference to bean 'entityManagerFactory' while setting constructor argument; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'entityManagerFactory' defined in class path resource [org/springframework/boot/autoconfigure/orm/jpa/HibernateJpaAutoConfiguration.class]: Invocation of init method failed; nested exception is javax.persistence.PersistenceException: [PersistenceUnit: default] Unable to build Hibernate SessionFactory
         |	at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor$AutowiredFieldElement.inject(AutowiredAnnotationBeanPostProcessor.java:561) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.annotation.InjectionMetadata.inject(InjectionMetadata.java:88) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor.postProcessPropertyValues(AutowiredAnnotationBeanPostProcessor.java:331) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	... 75 common frames omitted
         |Caused by: org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'userRepository': Cannot create inner bean '(inner bean)#58e814e6' of type [org.springframework.orm.jpa.SharedEntityManagerCreator] while setting bean property 'entityManager'; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name '(inner bean)#58e814e6': Cannot resolve reference to bean 'entityManagerFactory' while setting constructor argument; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'entityManagerFactory' defined in class path resource [org/springframework/boot/autoconfigure/orm/jpa/HibernateJpaAutoConfiguration.class]: Invocation of init method failed; nested exception is javax.persistence.PersistenceException: [PersistenceUnit: default] Unable to build Hibernate SessionFactory
         |	at org.springframework.beans.factory.support.BeanDefinitionValueResolver.resolveInnerBean(BeanDefinitionValueResolver.java:313) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.BeanDefinitionValueResolver.resolveValueIfNecessary(BeanDefinitionValueResolver.java:129) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.applyPropertyValues(AbstractAutowireCapableBeanFactory.java:1477) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.populateBean(AbstractAutowireCapableBeanFactory.java:1222) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:537) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:476) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory$1.getObject(AbstractBeanFactory.java:303) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:230) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:299) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:194) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultListableBeanFactory.findAutowireCandidates(DefaultListableBeanFactory.java:1120) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultListableBeanFactory.doResolveDependency(DefaultListableBeanFactory.java:1044) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultListableBeanFactory.resolveDependency(DefaultListableBeanFactory.java:942) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor$AutowiredFieldElement.inject(AutowiredAnnotationBeanPostProcessor.java:533) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	... 77 common frames omitted
         |Caused by: org.springframework.beans.factory.BeanCreationException: Error creating bean with name '(inner bean)#58e814e6': Cannot resolve reference to bean 'entityManagerFactory' while setting constructor argument; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'entityManagerFactory' defined in class path resource [org/springframework/boot/autoconfigure/orm/jpa/HibernateJpaAutoConfiguration.class]: Invocation of init method failed; nested exception is javax.persistence.PersistenceException: [PersistenceUnit: default] Unable to build Hibernate SessionFactory
         |	at org.springframework.beans.factory.support.BeanDefinitionValueResolver.resolveReference(BeanDefinitionValueResolver.java:359) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.BeanDefinitionValueResolver.resolveValueIfNecessary(BeanDefinitionValueResolver.java:108) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.ConstructorResolver.resolveConstructorArguments(ConstructorResolver.java:634) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.ConstructorResolver.instantiateUsingFactoryMethod(ConstructorResolver.java:444) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.instantiateUsingFactoryMethod(AbstractAutowireCapableBeanFactory.java:1119) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBeanInstance(AbstractAutowireCapableBeanFactory.java:1014) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:504) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:476) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.BeanDefinitionValueResolver.resolveInnerBean(BeanDefinitionValueResolver.java:299) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	... 90 common frames omitted
         |Caused by: org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'entityManagerFactory' defined in class path resource [org/springframework/boot/autoconfigure/orm/jpa/HibernateJpaAutoConfiguration.class]: Invocation of init method failed; nested exception is javax.persistence.PersistenceException: [PersistenceUnit: default] Unable to build Hibernate SessionFactory
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.initializeBean(AbstractAutowireCapableBeanFactory.java:1574) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:539) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:476) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory$1.getObject(AbstractBeanFactory.java:303) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:230) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:299) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:194) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.BeanDefinitionValueResolver.resolveReference(BeanDefinitionValueResolver.java:351) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	... 98 common frames omitted
         |Caused by: javax.persistence.PersistenceException: [PersistenceUnit: default] Unable to build Hibernate SessionFactory
         |	at org.hibernate.jpa.boot.internal.EntityManagerFactoryBuilderImpl.persistenceException(EntityManagerFactoryBuilderImpl.java:1249) ~[hibernate-entitymanager-4.3.10.Final.jar:4.3.10.Final]
         |	at org.hibernate.jpa.boot.internal.EntityManagerFactoryBuilderImpl.access$600(EntityManagerFactoryBuilderImpl.java:120) ~[hibernate-entitymanager-4.3.10.Final.jar:4.3.10.Final]
         |	at org.hibernate.jpa.boot.internal.EntityManagerFactoryBuilderImpl$4.perform(EntityManagerFactoryBuilderImpl.java:860) ~[hibernate-entitymanager-4.3.10.Final.jar:4.3.10.Final]
         |	at org.hibernate.jpa.boot.internal.EntityManagerFactoryBuilderImpl$4.perform(EntityManagerFactoryBuilderImpl.java:850) ~[hibernate-entitymanager-4.3.10.Final.jar:4.3.10.Final]
         |	at org.hibernate.boot.registry.classloading.internal.ClassLoaderServiceImpl.withTccl(ClassLoaderServiceImpl.java:425) ~[hibernate-core-4.3.10.Final.jar:4.3.10.Final]
         |	at org.hibernate.jpa.boot.internal.EntityManagerFactoryBuilderImpl.build(EntityManagerFactoryBuilderImpl.java:849) ~[hibernate-entitymanager-4.3.10.Final.jar:4.3.10.Final]
         |	at org.springframework.orm.jpa.vendor.SpringHibernateJpaPersistenceProvider.createContainerEntityManagerFactory(SpringHibernateJpaPersistenceProvider.java:60) ~[spring-orm-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean.createNativeEntityManagerFactory(LocalContainerEntityManagerFactoryBean.java:343) ~[spring-orm-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.orm.jpa.AbstractEntityManagerFactoryBean.afterPropertiesSet(AbstractEntityManagerFactoryBean.java:318) ~[spring-orm-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.invokeInitMethods(AbstractAutowireCapableBeanFactory.java:1633) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.initializeBean(AbstractAutowireCapableBeanFactory.java:1570) ~[spring-beans-4.1.6.RELEASE.jar:4.1.6.RELEASE]
         |	... 105 common frames omitted
         |Caused by: org.hibernate.AnnotationException: Unknown mappedBy in: de.sveri.mct.domain.Topic.topic, referenced property unknown: de.sveri.mct.domain.Topic.topic
         |	at org.hibernate.cfg.OneToOneSecondPass.doSecondPass(OneToOneSecondPass.java:160) ~[hibernate-core-4.3.10.Final.jar:4.3.10.Final]
         |	at org.hibernate.cfg.Configuration.originalSecondPassCompile(Configuration.java:1697) ~[hibernate-core-4.3.10.Final.jar:4.3.10.Final]
         |	at org.hibernate.cfg.Configuration.secondPassCompile(Configuration.java:1426) ~[hibernate-core-4.3.10.Final.jar:4.3.10.Final]
         |	at org.hibernate.cfg.Configuration.buildSessionFactory(Configuration.java:1846) ~[hibernate-core-4.3.10.Final.jar:4.3.10.Final]
         |	at org.hibernate.jpa.boot.internal.EntityManagerFactoryBuilderImpl$4.perform(EntityManagerFactoryBuilderImpl.java:857) ~[hibernate-entitymanager-4.3.10.Final.jar:4.3.10.Final]
         |	... 113 common frames omitted
         |""".stripMargin
    ) should matchSnapshot

    stackTraceHighlighter(
      """|Exception in thread "main" java.lang.RuntimeException: Compilation Failed with an unknown error
         |The error continued over multiple lines
         |ABCDEFG  dasd  dsa d
         |   ----           dasd
         |   ddasd  dasd
         |           asdas
         |   asd
         |
         |	at scala.sys.package$.error(package.scala:27)
         |	at tlang.compiler.Main$.unknownError(Main.scala:148)
         |	at tlang.compiler.Main$.runCompiler(Main.scala:121)
         |	at tlang.compiler.Main$.main(Main.scala:92)
         |	at tlang.compiler.Main.main(Main.scala)
         |""".stripMargin
    ) should matchSnapshot
  }
}
