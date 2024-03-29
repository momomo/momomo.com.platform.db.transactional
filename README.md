<!---
-->

##### The absolute base of our database transaction libraries.

##### Dependencies 
* **[`momomo.com.platform.Core`](https://github.com/momomo/momomo.com.platform.Core)**
* **[`momomo.com.platform.Lambda`](https://github.com/momomo/momomo.com.platform.Lambda)**

##### Used by
* **[`momomo.com.platform.db.base.transactional.Hibernate`](https://github.com/momomo/momomo.com.platform.db.transactional.Hibernate)** 
* **[`momomo.com.platform.db.base.transactional.Spring`](https://github.com/momomo/momomo.com.platform.db.transactional.Spring)**

##### Maven dependencies available on maven central [search.maven.org](https://search.maven.org/search?q=com.momomo)
##### Dependency   
```xml
<dependency>
  <groupId>com.momomo</groupId>
  <artifactId>momomo.com.platform.db.base.transactional</artifactId>
  <version>5.0.2</version>
</dependency>                                                      
```                         
##### Repository
```xml
<repository>
    <id>maven-central</id>
    <url>http://repo1.maven.org/maven2</url>
</repository>
```                                                           

##### Our other, highlighted [repositories](https://github.com/momomo?tab=repositories)                          

* **[`momomo.com.platform.Core`](https://github.com/momomo/momomo.com.platform.Core)** Is essentially what makes the our the core of several of momomo.com's public releases and contains a bunch of Java utility.

* **[`momomo.com.platform.Lambda`](https://github.com/momomo/momomo.com.platform.Lambda)** Contains a bunch of `functional interfaces` similar to `Runnable`, `Supplier`, `Function`, `BiFunction`, `Consumer` `...` and so forth all packed in a easily accessed and understood intuitive pattern that are used plenty in our libraries. **`Lambda.V1E`**, **`Lambda.V2E`**, **`Lambda.R1E`**, **`Lambda.R2E`**, ...

* **[`momomo.com.platform.Obj`](https://github.com/momomo/momomo.com.platform.Obj)** Intuitive library that makes it easier for you to return multiple, fully defined objects on the fly from any method, any time rather than being limited to the default maximum of one. 

* **[`momomo.com.platform.Nanotime`](https://github.com/momomo/momomo.com.platform.Nanotime)** Allows for nanosecond time resolution when asking for time from Java Runtime in contrast with **`System.currentTimeMillis()`**.

* **[`momomo.com.platform.db.transactional.Hibernate`](https://github.com/momomo/momomo.com.platform.db.transactional.Hibernate)** A library to execute database commands in transactions without  having to use annotations based on Hibernate libraries. No Spring!

* **[`momomo.com.platform.db.transactional.Spring`](https://github.com/momomo/momomo.com.platform.db.transactional.Spring)** A library to execute database commands in transactions without  having to use annotations based on Spring libraries.

### Background

Is the base **`API`** for our databse related Transactional API. Is implemented in two flavours: 

* **[`momomo.com.platform.db.base.transactional.Hibernate`](https://github.com/momomo/momomo.com.platform.db.transactional.Hibernate)** 
* **[`momomo.com.platform.db.base.transactional.Spring`](https://github.com/momomo/momomo.com.platform.db.transactional.Spring)**      

Example applications making use of the library: 

* **[`momomo.com.example.app.Crypto.based.on.hibernate.libraries`](https://github.com/momomo/momomo.com.example.app.Crypto.based.on.hibernate.libraries)** 
* **[`momomo.com.example.app.Crypto.based.on.spring.libraries`](momomo.com.example.app.Crypto.based.on.spring.libraries)**     

### License
The full license can be found here [`MoL9`](https://raw.githubusercontent.com/momomo/momomo.com.yz.licenses/master/MoL9?raw=true?raw=true)

### Contribute
Send an email to `opensource{at}momomo.com` if you would like to contribute in any way, make changes or otherwise have thoughts and/or ideas on things to improve.
