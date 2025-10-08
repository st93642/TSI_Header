# 9 Data Persistence with PostgreSQL

Before you begin: Join our book community on Discord

Give your feedback straight to the author himself and chat to other early readers on our Discord server (find the "rust-web-programming-3e" channel under EARLY ACCESS SUBSCRIPTION).

`https://packt.link/EarlyAccess/` By this point in the book, the frontend for our application has been defined, and our app is working at face value. However, we know that our app is reading and writing from a JSON file.

In this chapter, we get rid of our JSON file and introduce a PostgreSQL database to store our data. We do this by setting up a database development environment using Docker. We then build data models in Rust to interact with the database, refactoring our app so that the create, edit, and delete endpoints interact with the database instead of the JSON file. Finally, we exploit Rust traits so any database handle that has implemented our database transaction traits can be swapped into the application with minimal effort.

In this chapter, we will cover the following topics:

Building our PostgreSQL database

Adding SQLX to our Data Access Layer

Defining our Database Transactions

Connecting our transactions to the core

Connecting our transactions to the server

Creating out database migrations

Refactoring our frontend

By the end of this chapter, you will be able to manage an application that performs reading, writing, and deleting data in a PostgreSQL database with data models. If we make changes to the data models, we will be able to manage them with migrations. You will also be able to utilize Rust traits to generalize a database interface so integrating other databases will be simple and scalable.

## Technical requirements

In this chapter, we will be using Docker to define, run a PostgreSQL database, and run it. This will enable our app to interact with a database on our local machine. Docker can be installed by following the instructions at `https://docs.docker.com/engine/install/`

We will also be using Docker-compose on top of Docker to orchestrate our Docker containers. This can be installed by following the instructions at `https://docs.docker.com/compose/install/`

## Building our PostgreSQL database

Up to this point in the book, we have been using a JSON file to store our to-do items. This has served us well so far. In fact, there is no reason why we cannot use a JSON file throughout the rest of the book to complete the tasks. However, if you do use a JSON file for production projects, you will come across some downsides.

### Why we should use a proper database

If the reads and writes to our JSON file increase, we can face some concurrency issues and data corruption. There is also no checking on the type of data. Therefore, another developer can write a function that writes different data to the JSON file, and nothing will stand in the way.

There is also an issue with migrations. If we want to add a timestamp to the to-do items, this will only affect new to-do items that we insert into the JSON file. Therefore, some of our to-do items will have a timestamp, and others won't, which would introduce bugs into our app. Our JSON file also has limitations in terms of filtering.

Right now, all we do is read the whole data file, alter an item in the whole dataset, and write the whole dataset to the JSON file. This is not effective and will not scale well. It also inhibits us from linking these to-do items to another data model-like user. Plus, we can only search right now using the status. If we used a SQL database that has a user table that is linked to a to-do item database, we would be able to filter to-do items based on the user, status, or title. We can even use a combination thereof. When it comes to running our database, we are going to use Docker. So why should we use Docker?

### Why use Docker?

To understand why we would use Docker we need to understand what Docker is. Docker essentially has containers that work like virtual machines but in a more specific and granular way. Docker containers isolate a single application and all the applications dependencies. These containers the run the application inside. Docker containers can then communicate with each other. Because Docker containers share a single common operating system, they are compartmentalized from one another and from the operating system at large meaning that containerized applications use less memory compared to virtual machines. Because of Docker containers, we can be more portable with our applications. If the Docker container runs on my machine, it will run on another machine that also has Docker. We can also package our applications meaning that extra packages specific for our application to run do not need to be installed separately including dependencies on the operating system level. As a result, Docker gives us great flexibility in web development as we can simulate servers and databases on our local machine.

### How to use Docker to run a database

With all this in mind, it makes sense to go through the extra steps necessary to set up a SQL database and run it. To do this, we are going to use Docker: a tool that helps us create and use containers. Containers themselves are Linux technology that package and isolate applications along with their entire runtime environment. Containers are technically isolated file systems but to help visualize what we are doing in this chapter you can think of them as mini lightweight virtual machines. These containers are made from images that can be downloaded from Dockerhub. We can insert our own code into these images before spinning up a container out of them as seen in the following figure:

Figure 7.1 – Relationship of Docker images and containers

With Docker, we can download an image like PostgreSQL database and run it in our development environment. Because of Docker, can spin up multiple databases and apps, and then shut them down as and when we need. First, we need to take stock of our containers by running the following command in the terminal:

```bash
docker container ls -a
```

If Docker is a fresh install, we get the following output:

```text
CONTAINER ID IMAGE COMMAND CREATED STATUS PORTS NAMES
```

As we can see, we have no containers. We also need to take stock of our images. This can be done by running the following terminal command:

```bash
docker image ls
```

The preceding command gives the following output:

```text
REPOSITORY TAG IMAGE ID CREATED SIZE
```

Again, if Docker is a fresh install, then there will be no containers.

There are other ways in which we can create a database in Docker. For instance, we can create our own DockerFile where we define our own operating system (OS), and configurations. However, we have docker-compose installed. Using docker-compose will make the database definition straightforward. It will also enable us to add more containers and services. To define our PostgreSQL database, we code the following YAML code in a docker-compose.yml file in the root directory:

```yaml
version: "3.7" services: postgres: container_name: 'to-do-postgres' image: 'postgres:11.2' restart: always ports: - '5432:5432' environment: - 'POSTGRES_USER=username' - 'POSTGRES_DB=to_do' - 'POSTGRES_PASSWORD=password'
```

In the preceding code, at the top of the file, we have defined the version. Older versions such as 2 or 1 have different styles in which the file is laid out. The different versions also support different arguments. At the time of writing this book, version 3 is the latest version. The following URL covers the changes between each docker-compose version: `https://docs.docker.com/compose/compose-file/compose-versioning/`

We then define our database service that is nested under the postgres tag. Tags like the postgres and services denote dictionaries, and lists are defined with - for each element. If we were to convert our docker-compose file to JSON, it would have the following structure:

```json
{ "version": "3.7", "services": { "postgres": { "container_name": "to-do-postgres", "image": "postgres:11.2", "restart": "always", "ports": [ "5432:5432" ], "environment": [ "POSTGRES_USER=username", "POSTGRES_DB=to_do", "POSTGRES_PASSWORD=password" ] } } }
```

In the preceding code, we can see that our services are a dictionary of dictionaries, denoting each service. Thus, we can deduce that we cannot have two tags with the same name, as we cannot have two dictionary keys the same. The previous code also tells us that we can keep stacking on service tags with their own parameters.

### Running a database in Docker

With our database service we have a name; so, when we look at our containers, we know what each container is doing in relation of the service such as a server or database. In terms of configuring the database and building it, we luckily pull the official postgres image. This image has everything configured for us, and Docker will pull it from the repository. The image is like a blueprint. We can spin up multiple containers with their own parameters from that one image that we pulled. We then define the restart policy as always. This means that the container will always restart when the containers exit. We can also define it to only restart based on a failure or stopping.

It should be noted that Docker containers have their own ports that are not open to the machine. However, we can expose container ports and map the exposed port to an internal port inside the Docker container. Considering these features, we can define our ports.

However, in our example, we will keep our definition simple. We state that we accept incoming traffic to the Docker container on port 5432 and route it through to the internal port 5432. We then define our environment variables, which are the username, the name of the database, and the password. While we are using generic, easy-to-remember passwords and usernames for this book, it is advised that you switch to more secure passwords and usernames if pushing to production. We can build a spin up for our system by navigating to the root directory where our docker-compose file is by running the following command:

```bash
docker-compose up
```

The preceding command will pull down the postgres image from the repository and start constructing the database. After a flurry of log messages, the terminal should come to rest with the following output:

```text
LOG: listening on IPv4 address "0.0.0.0", port 5432 LOG: listening on IPv6 address "::", port 5432 LOG: listening on Unix socket "/var/run/postgresql/.s.PGSQL.5432" LOG: database system was ready to accept connections
```

As you can see, the date and time will vary. However, what we are told here is that our database is ready to accept connections. Yes, it is really that easy. Therefore, Docker adoption is unstoppable. A Ctrl + C will stop our docker-compose; thus, shutting down our postgres container.

We now list all our containers with the following command:

```bash
docker container ls -a
```

The preceding command gives us the following output:

```text
CONTAINER ID IMAGE COMMAND c99f3528690f postgres:11.2 "docker-entrypoint.s…" CREATED STATUS PORTS 4 hours ago Exited (0) About a minute ago NAMES to-do-postgres
```

In the preceding output, we can see that all the parameters are there. The ports, however, are empty because we stopped our service.

### Exploring routing and ports in Docker

If we were to start our service again, and list our containers in another terminal, port 5432 would be under the PORTS tag. We must keep note of the CONTAINER ID as it's going to be unique and different/random for each container. We will need to reference these if we're accessing logs. When we are running docker-compose up we essentially use the following structure:

Figure 7.2 – Docker-compose serving our database

In Figure 6.2, we can see that our docker-compose uses a unique project name to keep containers and networks in their namespace. It must be noted that our containers are running on the localhost. Therefore, if we want to make a call to a container managed by docker-compose, we will have to make a localhost request. However, we must make the call to the port that is open from docker-compose and docker-compose will route it to the port that is defined in the docker-compose config yml file. For instance, we have two databases with the following yml file:

```yaml
version: "3.7" services: postgres: container_name: 'to-do-postgres' image: 'postgres:11.2' restart: always ports: - '5432:5432' environment: - 'POSTGRES_USER=username' - 'POSTGRES_DB=to_do' - 'POSTGRES_PASSWORD=password' postgres_two: container_name: 'to-do-postgres_two' image: 'postgres:11.2' restart: always ports: - '5433:5432' environment: - 'POSTGRES_USER=username' - 'POSTGRES_DB=to_do' - 'POSTGRES_PASSWORD=password'
```

In the preceding code, we can see that both of our databases accept traffic into their containers through port 5432. However, there would be a clash so one of the ports that we open with is port 5433, which is routed to port 5432 in the second database container which gives us the following layout:

Figure 7.3 – docker-compose serving multiple databases

This routing gives us flexibility when running multiple containers. We are not going to run multiple databases for our to-do application, so we should delete our postgres_two service. Once we have deleted our postgres_two service, we can run our docker-compose again and then list our containers with the following command:

```bash
docker image ls
```

The preceding command will now give us the following output:

```text
REPOSITORY TAG IMAGE ID postgres 11.2 3eda284d1840 CREATED SIZE 17 months ago 312MB
```

In the preceding output, we can see that our image has been pulled from the postgres repository. We also have a unique/random ID for the image, and we also have a date for when that image was created.

Now that we have a basic understanding on how to get our database up and running, we can run our docker-compose in the background with the following command:

```bash
docker-compose up -d
```

The preceding command just tells us which containers have been spun up with the following output:

```text
Starting to-do-postgres ... done
```

We can see our status when we list our containers with the following output:

```text
STATUS PORTS NAMES Up About a minute 0.0.0.0:5432->5432/tcp to-do-postgres
```

In the previous output, the other tags are the same, but we can also see that the STATUS tag tells us how long the container has been running, and which port it is occupying. Whilst our docker-compose is running in the background, it does not mean we cannot see what is going on. We can access the logs of the container anytime by calling the logs command and referencing the ID of the container by the following command:

```bash
docker logs c99f3528690f
```

The preceding command should give out the same output as our standard docker-compose up command. To stop our docker-compose we can run the stop command, shown as follows:

```bash
docker-compose stop
```

The preceding command will stop our containers in our docker-compose. It must be noted that this is different from the down command, shown as follows:

```bash
docker-compose down
```

The down command will also stop our containers. However, the down command will delete the container. If our database container is deleted, we will also lose all our data.

There is a configuration parameter called volumes that can prevent the deletion of our data when the container is removed; however, this is not essential for local development on our computers. In-fact, you will be wanting to delete containers and images from your laptop regularly. I did a purge on my laptop once of containers and images that I was no longer using, and this freed up 23GB!

Docker containers on our local development machines should be treated as temporary. Whilst docker containers are multiple, and more lightweight than standard virtual machines, they are not free. The idea behind docker running on our local machines is that we can simulate what running our application would be like on a server. If it runs in docker on our laptop, we can be certain that it will also run on our server, especially, if the server is being managed by a production-ready docker orchestration tool like Kubernetes.

### Running docker in the background with bash scripts

Docker can also help with consistent testing and development. We will want to be able to have the same results every time we run a test. We will also want to onboard other developers easily and enable them to tear down and spin up containers that will support development quickly and easily. I have personally seen development delayed when not supporting easy teardown and spin up procedures. For instance, when working on a complex application, the code that we are adding and testing out might scar the database. Reverting back might not be possible and deleting the database and starting again would be a pain as reconstructing this data might take a long time. The developer may not even remember how they constructed the data in the first place. There are multiple ways to prevent this from happening and will be cover these in Chapter 9, Testing Our Application Endpoints and Components.

For now, we will build a bash script that spins up our database in the background, waits until the connection to our database is ready, and then tears down our database. This will give us the foundations to build pipelines, tests, and onboarding packages to start development. To do this, we will create a directory in the root directory of our Rust web application called scripts. We can then create a scripts/wait_for_database.sh file housing the following code:

```bash
#!/bin/bash cd .. docker-compose up -d until pg_isready -h localhost -p 5432 -U username do echo "Waiting for postgres" sleep 2; done echo "docker is now running" docker-compose down
```

Using the preceding code, we move the current working directory of the script out of the scripts directory and into our root directory. We then start our docker-compose in the background. Next, we loop, pinging the 5432-port utilizing the pq_isready command to wait until our database is ready to accept connections.

[breakout box]

The bash command pg_isready might not be available on your computer. The pg_isready command usually comes with the installation of the PostgreSQL client. Alternatively, you can use following docker command instead of the pg_isready:

```bash
until docker run -it postgres --add-host host.docker.internal:host-gateway docker.io/postgres:14-alpine -h localhost -U username pg_isready
```

What is happening here is that we are using the postgres docker image to run our

[breakout box]

Once our database is running, we print out to the console that our database is running, and then tear down our docker-compose destroying the database container. Running the command that runs the wait_for_database.sh bash script will give the following output:

```bash
❯ sh wait_for_database.sh [+] Running 0/0 ⠋ Network web_app_default Creating 0.2s ⠿ Container to-do-postgres Started 1.5s localhost:5432 - no response Waiting for postgres localhost:5432 - no response Waiting for postgres localhost:5432 - accepting connections docker is now running [+] Running 1/1 ⠿ Container to-do-postgres Removed 1.2s ⠿ Network web_app_default Removed
```

From the preceding output, considering that we tell our loop to sleep for two seconds at every iteration of the loop, we can deduce that it took roughly four seconds for our newly spun up database to accept connections. Thus, we can say that we have achieved basic competency at managing local databases with Docker.

In this section, we set up our environment. We also understood the basics of docker enough to build, monitor, shutdown, and delete our database with just a few simple commands. Now, we can move on to the next section, where we'll be adding the SQLX crate to the data access layer.

## Adding SQLX to our Data Access Layer

Before we cover how to handle SQLX in our data access layer, it makes sense to explain why we are using SQLX. In previous editions we covered diesel for the data access as this was an ORM for postgres and Rust. However, you get more flexibility by just writing SQL, and the nature of Rust structs that can be augmented by traits means that the results can simply yield structs that we can directly work with. Furthermore, SQLX does not require a complex schema setup where all the columns need to match exactly when declaring the schema, instead we just write the SQL queries and pass in the struct that we want returned in the query. However, if you feel passionately that you want to use Diesel, do not worry, we are going to structure the data access layer in such a way that you can keep adding different data access methods. By the end of this section, we will have a data access layer that supports the JSON file storage method that we are currently using, and the SQLX powered Postgres.

Before we write any code, we should orientate ourselves with the refactor of the data access layer in our to-do nanoservice by looking that the following file layout:

├── Cargo.toml └── src ├── connections │ ├── mod.rs │ └── sqlx_postgres.rs ├── json_file.rs ├── lib.rs └── to_do_items ├── descriptors.rs ├── enums.rs ├── mod.rs ├── schema.rs └── transactions ├── create.rs ├── delete.rs ├── get.rs ├── mod.rs └── update.rs

Here, we can see that the outline of the data access layer has the following key sections that we should note:

connections/: Where the connection code is housed for connecting the data access layer to a data storage engine. For this chapter, we will house the connection pool for the Postgres database here.

to_do_items/: This directory houses all the code that is specific to the interaction of a data storage engine with to do items.

to_do_items/descriptors.rs: Houses placeholder structs for each storage engine supported for to-do items.

to_do_items/schema.rs: Houses the data structs that are passed to and from the data store for to do items.

to_do_items/transactions/: This is where traits are stored for each data transaction we want to perform on to-do items. We will use the traits as interfaces, and then implement those traits for any storage engine we want to support that transaction. We can then pass these data storage engines into functions that accept structs that have implemented the trait for the specific transaction.

Before we write any code, we should configure our data access layer's Cargo.toml file with the following dependencies:

```toml
# file: nanoservices/to_do/dal/Cargo.toml [features] json-file = ["serde_json"] sqlx-postgres = ["sqlx", "once_cell"] [dependencies] serde ={ version="1.0.197", features = ["derive"] } glue = { path = "../../../glue"} # for json-file serde_json ={ version="1.0.114", optional = true } # for sqlx-postgres sqlx = { version = "0.7.4", features = ["postgres", "json"], optional = true } once_cell = { version = "1.19.0", optional = true }
```

Here we can see that our data access layer is aiming to support both SQLX and standard JSON file storage engines depending on the feature that is selected. We can also see that we are selective with what crates we use. We do not want to be compiling SQLX if we are just using the json-file feature.

We are now ready to write code, so initially, we should make sure that all our code is available in our file with the code below:

```rust
// file: nanoservices/to_do/dal/src/lib.rs pub mod to_do_items; pub mod connections; #[cfg(feature = "json-file")] pub mod json_file;
```

The first module we should focus on is the connections as our trait implementations will require the connections. We can create our database connection with the following code:

```rust
// file: nanoservices/to_do/dal/src/connections/sqlx_postgres.rs use sqlx::postgres::{PgPool, PgPoolOptions}; use once_cell::sync::Lazy; use std::env; pub static SQLX_POSTGRES_POOL: Lazy<PgPool> = Lazy::new(|| { let connection_string = env::var("TO_DO_DB_URL").unwrap(); let max_connections = match std::env::var( "TO_DO_MAX_CONNECTIONS" ) { Ok(val) => val, Err(_) => "5".to_string() }.trim().parse::<u32>().map_err(|_e|{ "Could not parse max connections".to_string() }).unwrap(); let pool = PgPoolOptions::new() .max_connections(max_connections); pool.connect_lazy(&connection_string) .expect("Failed to create pool") });
```

Here we can see that we inspect the environment variables to get the URL of the Postgres database that we will connect to. We also check for the maximum number of connections for our connection pool. We use a connection pool to prevent us having to make a connection every time we want to perform a database transaction. A database connection pool is a limited number of database connections. When our application needs a database connection it will take the connection from the pool and then place the connection back into the pool when the application no longer needs the connection. If there are no connections left in the pool the application will wait until there is a connection available as seen in the following figure:

Figure 7.4 – Database Connection pool with a limit of three

We also must note that our connection code is wrapped in a lazy static, meaning that the code is evaluated once when initially called and then never again throughout the lifetime of the program, with the result of the connection code assigned to the SQLX_POSTGRES_POOL variable.

Now with our database connection pool defined, we can ensure that the connection pool is available to the rest of the code if the SQLX feature is activated with the code below:

```rust
// file: nanoservices/to_do/dal/src/connections/mod.rs #[cfg(feature = "sqlx-postgres")] pub mod sqlx_postgres;
```

We can now move onto our schema, as this is another dependency that is required for the storage engine transactions. However, before we do this, we must copy over the TaskStatus enum from our nanoservices/to_do/core/src/enums.rs file to our nanoservices/to_do/dal/src/to_do_items/enums.rs file. With this in place, we can start our schema with the following imports:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/schema.rs use std::fmt; use glue::errors::NanoServiceError; use serde::{Serialize, Deserialize}; use super::enums::TaskStatus; use std::collections::HashMap;
```

With these imports, we can build out our to-do item structs. However, we need two separate structs. In a postgrad database, we want to be able to assign an ID to each row of the database table. However, when we are inserting the to-do item into the database, we will not know the ID, therefore, we need to have a new to-do item that does not have an ID that takes for form below:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/schema.rs #[derive(Serialize, Deserialize, Debug, Clone)] pub struct NewToDoItem { pub title: String, pub status: TaskStatus }
```

Once we use the NewToDoItem to store the new to-do item into the database, we can make queries to the database to get the to-do item which is defined below:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/schema.rs #[derive(Serialize, Deserialize, Debug, Clone, PartialEq)] #[cfg_attr(feature = "sqlx-postgres", derive(sqlx::FromRow))] pub struct ToDoItem { pub id: i32, pub title: String, pub status: String }
```

Here, we can see that we derive the FromRow trait is our SQLX feature is activated, as we need the FromRow trait to pass the ToDoItem as the return type for the query.

Finally, before we move onto implementing the transactions, we must make our placeholder structs with the following code:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/descriptors.rs pub struct SqlxPostGresDescriptor; pub struct JsonFileDescriptor;
```

And make our code for the to-do items public with the following code:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/mod.rs pub mod schema; pub mod enums; pub mod descriptors; pub mod transactions;
```

And with this, we can move onto defining our database transactions.

## Defining our Database Transactions

Our transactions module is essentially the API for our data transactions. These transactions do not need interact with a data storage engine. Transactions could be API calls to another server, or a state machine. We will just focus on data storage engines, but when we implement these transactions, you will see how powerful and flexible they are.

Before we define any transactions however, we must ensure that our transactions are available with the code below:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/transactions/mod.rs pub mod create; pub mod delete; pub mod get; pub mod update;
```

We can start our transaction definitions with the create transaction. First, we must import the following:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/ // transactions/create.rs use crate::to_do_items::schema::{ToDoItem, NewToDoItem}; use glue::errors::NanoServiceError; use std::future::Future; #[cfg(feature = "json-file")] use super::super::descriptors::JsonFileDescriptor; #[cfg(feature = "json-file")] use crate::json_file::{get_all, save_all}; #[cfg(feature = "json-file")] use std::collections::HashMap; #[cfg(feature = "sqlx-postgres")] use crate::connections::sqlx_postgres::SQLX_POSTGRES_POOL; #[cfg(feature = "sqlx-postgres")] use super::super::descriptors::SqlxPostGresDescriptor; #[cfg(feature = "sqlx-postgres")] use glue::errors::NanoServiceErrorStatus;
```

Now, that is a lot of imports, but we can see that by the features, we are importing enough to support our file and SQLX Postgres storage engines.

Before we write any logic for either our file or Postgres engine, we must define our transaction signature with the trait below:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/ // transactions/create.rs pub trait SaveOne { fn save_one(item: NewToDoItem) -> impl Future<Output = Result<ToDoItem, NanoServiceError>> + Send; }
```

Here we can see that our trait function accepts a NewToDoItem struct and returns an async future that either returns a ToDoItem or NanoServiceError. We are returning a future with specific traits so we can be explicit to what traits are being implemented for the return type of our trait function. If we were to just use an async function in our trait, then we are not being explicit in if the Send trait needs to be implemented or not for the future. This means that we can use our future in multithreaded contexts which is our Tokio runtime. We must remember that Tokio is merely an implementation of an async runtime, other runtimes do exist, and some are not multithreaded. In-fact, Tokio even gives you the option to not run in a multi-threaded context, therefore implementing the Send trait is not essential when in a single threaded context of async in Tokio. However, we are going to use multithreading in our server, so we want to ensure that every async function that is returned by this trait can be sent over threads including the parameters and return values.

Now that we have our trait defined, we can implement these traits for our storage engines with the following code:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/ // transactions/create.rs #[cfg(feature = "sqlx-postgres")] impl SaveOne for SqlxPostGresDescriptor { fn save_one(item: NewToDoItem) -> impl Future<Output = Result<ToDoItem, NanoServiceError>> + Send { sqlx_postgres_save_one(item) } } #[cfg(feature = "json-file")] impl SaveOne for JsonFileDescriptor { fn save_one(item: NewToDoItem) -> impl Future<Output = Result<ToDoItem, NanoServiceError>> + Send { json_file_save_one(item) } }
```

Here, we can see that we merely pass the item into an async function that has not been awaited on, so when we call the trait function, the future is constructed and returned so the caller can await on the future.

We can now define these async functions starting with the Postgres one below:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/ // transactions/create.rs #[cfg(feature = "sqlx-postgres")] async fn sqlx_postgres_save_one(item: NewToDoItem) -> Result<ToDoItem, NanoServiceError> { let item = sqlx::query_as::<_, ToDoItem>(" INSERT INTO to_do_items (title, status) VALUES ($1, $2) RETURNING *" ).bind(item.title) .bind(item.status.to_string()) .fetch_one(&*SQLX_POSTGRES_POOL).await.map_err(|e| { NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?; Ok(item) }
```

Here we can see that we write an SQL query that inserts a new row into the table to_do_items. We will cover the creation of the to_do_items table in the migrations section. Once the SQL query is defined, we bind the fields of the input item to the query.

Do we have to use bind? Why can't we just format a string?

SQL queries are at risk of an attack called an SQL injection. This is where we inject another query into the SQL query. For instance, if we were just formatting a string and passing in the parameters to the string, we could type the title of the to-do item to be the following:

```sql
'); DROP TABLE to_do_items; --
```

The ; indicates that the query has finished, and that a new SQL query is about to run. The next SQL query in the string that we claimed was the title of the to-do item executes an SQL query that drops the entire table, wiping all our data. When working with SQL queries, it is important to use the sanitization checks that the SQL library provides to check and project against SQL injections. SQLX functions like bind check for special characters and escape the database transaction instead of executing it, protecting us from SQL injections.

Once we have configured our SQL query, we state that we are going to return the inserted row with the fetch_one and we pass in the database connection to the fetch_one function. We now have the newly inserted to-do item and the ID assigned to that item during the insert.

For our JSON file, we are not going to be using IDs, but we must have the same signature as the trait, therefore, our JSON file function takes the following form:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/ // transactions/create.rs #[cfg(feature = "json-file")] async fn json_file_save_one(item: NewToDoItem) -> Result<ToDoItem, NanoServiceError> { let mut tasks = get_all::<ToDoItem>().unwrap_or_else(|_| HashMap::new() ); let to_do_item = ToDoItem { id: 1, title: item.title, status: item.status.to_string() }; tasks.insert( to_do_item.title.to_string(), to_do_item.clone() ); let _ = save_all(&tasks)?; Ok(to_do_item) }
```

Here, we can see that we are merely assigning the ID to one and leave it at that. We could build our own ID system where we increase the ID by one every time we insert a row, but this would excessively bloat the chapter for a storage engine that we are moving away from. We do not need the ID of the to-do item to carry out the tasks, so it makes sense to keep the JSON file API functional but warn other developers to switch over to the Postgres database as soon as possible.

Our other transactions will be defined using the same structure. For our delete transaction we need the imports below:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/ // transactions/delete.rs use crate::to_do_items::schema::ToDoItem; use glue::errors::NanoServiceError; use std::future::Future; #[cfg(feature = "json-file")] use super::super::descriptors::JsonFileDescriptor; #[cfg(feature = "json-file")] use crate::json_file::{get_all, save_all}; #[cfg(feature = "json-file")] use std::collections::HashMap; #[cfg(feature = "sqlx-postgres")] use crate::connections::sqlx_postgres::SQLX_POSTGRES_POOL; #[cfg(feature = "sqlx-postgres")] use super::super::descriptors::SqlxPostGresDescriptor; #[cfg(any(feature = "json-file", feature = "sqlx-postgres"))] use glue::errors::NanoServiceErrorStatus;
```

And our delete trait takes the following form:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/ // transactions/delete.rs pub trait DeleteOne { fn delete_one(title: String) -> impl Future<Output = Result<ToDoItem, NanoServiceError>> + Send; }
```

Our implementations are defined below:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/ // transactions/delete.rs #[cfg(feature = "sqlx-postgres")] impl DeleteOne for SqlxPostGresDescriptor { fn delete_one(title: String) -> impl Future<Output = Result<ToDoItem, NanoServiceError>> + Send { sqlx_postgres_delete_one(title) } } #[cfg(feature = "json-file")] impl DeleteOne for JsonFileDescriptor { fn delete_one(title: String) -> impl Future<Output = Result<ToDoItem, NanoServiceError>> + Send { json_file_delete_one(title) } }
```

When it comes to our Postgres query, we use the title from the item passed into delete with the following function:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/ // transactions/delete.rs #[cfg(feature = "sqlx-postgres")] async fn sqlx_postgres_delete_one(title: String) -> Result<ToDoItem, NanoServiceError> { let item = sqlx::query_as::<_, ToDoItem>(" DELETE FROM to_do_items WHERE title = $1 RETURNING *" ).bind(item.id) .fetch_one(&*SQLX_POSTGRES_POOL).await.map_err(|e| { NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?; Ok(item) }
```

As for our JSON file to-do items, we use the title of the to-do item to delete the item with the code below:

```rust
// file: nanoservices/to_do/dal/src/to_do_items/ // transactions/delete.rs #[cfg(feature = "json-file")] async fn json_file_delete_one(title: String) -> Result<ToDoItem, NanoServiceError> { let mut tasks = get_all::<ToDoItem>().unwrap_or_else(|_| HashMap::new() ); let to_do_item = tasks.remove( &title ).ok_or_else(|| { NanoServiceError::new( "Item not found".to_string(), NanoServiceErrorStatus::NotFound ) })?; let _ = save_all(&tasks)?; Ok(to_do_item) }
```

We now need to implement the GetAll and UpdateOne traits for our storage engines. However, the structure is going to be repetitive and printing out the implementation of the GetAll and UpdateOne traits in the chapter will excessively bloat the chapter and derail the learning process. As this stage, you should be able to define and implement the GetAll and UpdateOne traits yourself for our storage engines. The code for the definition and implementation of the GetAll and UpdateOne traits is provided in the appendix. For guidance, the outlines of these traits take the following form:

```rust
pub trait GetAll { fn get_all() -> impl Future< Output = Result<Vec<ToDoItem>, NanoServiceError> > + Send; } pub trait UpdateOne { fn update_one(item: ToDoItem) -> impl Future<Output = Result< ToDoItem, NanoServiceError> > + Send; }
```

With all our transactions defined, we can now move onto connecting these transactions to our core of the nanoservice.

## Connecting our Transactions to the Core

Now that we have defined the interface of our storage engine using traits, our core can now be agnostic to the storage engine meaning that our core dependencies now have the following:

```toml
# nanoservices/to_do/core/Cargo.toml [dependencies] dal = { path = "../dal" } serde = { version = "1.0.197", features = ["derive"] } glue = { path = "../../../glue"}
```

We can now redefine our create endpoint with the code below:

```rust
// nanoservices/to_do/core/src/api/basic_actions/create.rs use dal::to_do_items::schema::{NewToDoItem, ToDoItem}; use dal::to_do_items::transactions::create::SaveOne; use glue::errors::NanoServiceError; pub async fn create<T: SaveOne>(item: NewToDoItem) -> Result<ToDoItem, NanoServiceError> { let created_item = T::save_one(item).await?; Ok(created_item) }
```

Here, we can see that the generic parameter declaring the SaveOne trait can be directly called without having to pass in any struct into the function. This is because the trait did not have any reference to self in the signatures of the function, therefore we do not need the state of the struct to call the trait. Because we do not need the state of the struct, we can just declare the type of struct and use all the logic implemented. This is very powerful, because we can then mount this function to our server with the specific handle that we want. We will cover mounting our Postgres functions to our server in the next section, for now, we will complete our switching over to generic references to storage engines.

For our delete interface, we have the following code:

```rust
// nanoservices/to_do/core/src/api/basic_actions/delete.rs use dal::to_do_items::transactions::delete::DeleteOne; use glue::errors::NanoServiceError; pub async fn delete<T: DeleteOne>(id: &str) -> Result<(), NanoServiceError> { let _ = T::delete_one(id.to_string()).await?; Ok(()) }
```

Here we can see that we still accept the title and return nothing, and our trait is performing the action on the storage. Here, we can see that our interface is getting more complicated but not really doing much apart from calling a function. This is because a to-do application is simply storing items in a database. The to-do application was chosen to prevent excessive bloat so we can just focus on concepts. However, our core of another more complicated application will be making calls on different storage engines, checking outcomes, performing calculations, and firing off processes such as statistics for dashboards, or emails. Remember, thanks to traits, the core is now the IO agnostic center where your business logic is coded. Your core would be slotted into a desktop app with frameworks like Tauri, just a binary on a computer that uses stdio to have data piped in and out of it. At the time of writing this, I am an Honorary Researcher in the bioengineering department at Kings College London. The projects are in the surgical robotics department, and using IO agonistic cores is essential. For instance, one operating theatre might have terrible signal due to lead lined walls to protect against radiation from scanning equipment. Therefore, we must interact with a cable. A lab at a central hospital might have access to GPUs, therefore interacting with those is very beneficial. However, we also need to consider that not every lab/operating theatre will have access to GPUs.

For our to-do application having a core may seem tedious a trivial, but sticking with this approach, will enable you to pivot, or support multiple interfaces and contexts. You cannot predict the future. Even building web apps, I have been halfway through, and a manager has said in a meeting that they have decided to go for a different database for the storage. This could be down to other features being needed for the roadmap, or just the licensing/pricing. This doesn't faze me because I am strict in my structing, so swapping out the database was not a hassle.

For our get core interface, we have the following code:

```rust
// nanoservices/to_do/core/src/api/basic_actions/get.rs use dal::to_do_items::schema::AllToDOItems; use dal::to_do_items::transactions::get::GetAll; use glue::errors::NanoServiceError; pub async fn get_all<T: GetAll>() -> Result<AllToDOItems, NanoServiceError> { let all_items = T::get_all().await?; AllToDOItems::from_vec(all_items) }
```

Here we can see that we have removed the single get interface. This is because we were not using it in our application. General rule is that if we are not using code, we should delete it. It cleans up the code and reduces the amount of code that we are maintaining. However, like all rules there are exceptions. Going back to my surgical robotics work, it would be short-sighted of me to delete the GPU interface if we do not use it for a particular lab that does not have access to a GPU.

Finally, we have the update. This is a good stage for you to attempt to define the update interface yourself. If you have attempted this yourself, then hopefully it follows the same approach:

```rust
// nanoservices/to_do/core/src/api/basic_actions/update.rs use dal::to_do_items::schema::ToDoItem; use glue::errors::NanoServiceError; use dal::to_do_items::transactions::update::UpdateOne; pub async fn update<T: UpdateOne>(item: ToDoItem) -> Result<(), NanoServiceError> { let _ = T::update_one(item).await?; Ok(()) }
```

Note that we are passing in a ToDoItem instead of a NewToDoItem because we already have the ID.

Our core is now updated, so we can move onto connecting our transactions to our server.

## Connecting our Transactions to the Server

We are now at the final stage of integrating our Postgres handle into our system. As we removed the reference to the type of storage from our core making it engine agonistic, we now must define our engine in our server dependencies with the following:

```toml
# nanoservices/to_do/networking/actix_server/Cargo.toml [dependencies] tokio = { version = "1.36.0", features = ["full"] } actix-web = "4.5.1" core = { path = "../../core" } dal = { path = "../../dal", features = ["sqlx-postgres"]} glue = { path = "../../../../glue", features = ["actix"] }
```

Here we can see that we have activated the sqlx-postgres feature otherwise our handle will not have implemented all the required traits to be used in our endpoints.

For our create endpoint, we first need to import the traits with the code below:

```rust
// nanoservices/to_do/networking/actix_server/src // /api/basic_actions/create.rs use dal::to_do_items::schema::NewToDoItem; use dal::to_do_items::transactions::{ create::SaveOne, get::GetAll };
```

Here we can see that we need two traits, because with every API call, we like to return the updated state to the frontend. With these two traits, we define the create function with the following code:

```rust
// nanoservices/to_do/networking/actix_server/src // /api/basic_actions/create.rs pub async fn create<T: SaveOne + GetAll>( token: HeaderToken, body: Json<NewToDoItem> ) -> Result<HttpResponse, NanoServiceError> { let _ = create_core::<T>(body.into_inner()).await?; Ok(HttpResponse::Created().json(get_all_core::<T>().await?)) }
```

This works because our Postgres handle has implanted all the traits for the storage, therefore, we both traits are satisfied. Here is where we can see even more flexibility. If for instance we cached the state in a datastore like Redis, and updated that cache with every transaction, then we could split up the trait requirements to <T: SaveOne, X: GetAll>. This means we could pass in two Postgres handles but pass in different ones if we need. We could even go as far as to implement a HTTP request for one of the traits to call another service. As long that the signature of the trait is respected the create endpoint will accept and utilize it.

For our delete endpoint our updated code takes the form below:

```rust
// nanoservices/to_do/networking/actix_server/src // src/api/basic_actions/delete.rs use dal::to_do_items::transactions::{ delete::DeleteOne, get::GetAll }; . . . pub async fn delete_by_name<T: DeleteOne + GetAll>(req: HttpRequest) -> Result<HttpResponse, NanoServiceError> { match req.match_info().get("name") { Some(name) => { delete_core::<T>(name).await?; }, None => { return Err( NanoServiceError::new( "Name not provided".to_string(), NanoServiceErrorStatus::BadRequest ) ) } }; Ok(HttpResponse::Ok().json(get_all_core::<T>().await?)) }
```

Our get is the simplest with the following updates:

```rust
// nanoservices/to_do/networking/actix_server/src // /api/basic_actions/get.rs use dal::to_do_items::transactions::get::GetAll; . . . pub async fn get_all<T: GetAll>() -> Result<HttpResponse, NanoServiceError> { Ok(HttpResponse::Ok().json(get_all_core::<T>().await?)) }
```

Finally, we have the update. This is a good time to try and implement the update endpoint yourself. If you attempted to do this, your code should look like the following:

```rust
// nanoservices/to_do/networking/actix_server/src // /api/basic_actions/update.rs use dal::to_do_items::transactions::{ update::UpdateOne, get::GetAll }; . . . pub async fn update<T: UpdateOne + GetAll>(body: Json<ToDoItem>) -> Result<HttpResponse, NanoServiceError> { let _ = update_core::<T>(body.into_inner()).await?; Ok(HttpResponse::Ok().json(get_all_core::<T>().await?)) }
```

Here, we can stop and admire the beauty that this approach and the Rust programming language gives us. It is verbose and succinct at the same time. Let's say two months from now you come back to this endpoint to see what the steps are. Because Rust is verbose, and this function just focuses on the management and flow of the HTTP request and response, we can instantly see that the ToDoItem is the JSON body of the request. We can also see that this endpoint is performing a save one and get all operation with a storage engine. We can then see that the body is passed into a core function to update the item, and then the get all is returned. You can see that this scales easily. If you have more core functions, you will be able to easily see how the HTTP request navigates through these. It also gives you the option to easily refactor the high level, swapping out and moving core functions like Lego blocks. When you get to big systems with complex endpoints, you with thank past you for taking this approach.

We now need to mount our Postgres handler to our views factory. The update to our views factory take the form below:

```rust
// nanoservices/to_do/networking/actix_server/src // /api/basic_actions/mod.rs . . . use dal::to_do_items::descriptors::SqlxPostGresDescriptor; pub fn basic_actions_factory(app: &mut ServiceConfig) { app.service( scope("/api/v1") .route("get/all", get().to( get::get_all::<SqlxPostGresDescriptor>) ) .route("create", post().to( create::create::<SqlxPostGresDescriptor>) ) .route("delete/{name}", delete().to( delete::delete_by_name::<SqlxPostGresDescriptor>) ) .route("update", put().to( update::update::<SqlxPostGresDescriptor>) ) ); }
```

We have now connected our server endpoints to our server with the SQLX handle. Our server is ready to talk to a database, so, we must prime our database for our server with migrations.

## Creating Our Database Migrations

For Postgres we need a table of to-do items to insert rows and perform queries. We can do this by running SQL scripts against the server on the startup of the server. These migrations can be generated using the SQLX client. To install the SQLX client, run the following command:

```bash
cargo install sqlx-cli
```

With the SQLX client installed, we can now move to the root directory of our data access layer for our to-do nanoservice and create a .env file with the following content:

```bash
DATABASE_URL=postgres://username:password@localhost/to_do
```

When running, the SQLX client will detect the .env file and export the content as environment variables to connect to the database. Now that we have environment variables to connect to the database, we can create our first migration with the command below:

```bash
sqlx migration add initial-setup
```

You should get a printout congratulating you on creating the migration and you should also get the following file structure generated:

├── migrations │ └── 20240523084625_initial-setup.sql

The number will vary because it is a timestamp. This SQL script is the initial script for our database setup. There will be more elaborate structures to migrations were we can run different scripts to move the migration version up and down on the database but these will come in the next chapter when we update our data models.

When we have multiple SQL scripts, the SQLX client will assume that the order to run SQL scripts is based off the number which is the timestamp. These SQL scripts will be run in ascending order. For our setup script, we must create our to-do items table with the following code:

```sql
-- nanoservices/to_do/dal/migrations/ -- 20240523084625_initial-setup.sql CREATE TABLE to_do_items ( id SERIAL PRIMARY KEY, title VARCHAR(255) UNIQUE NOT NULL, status VARCHAR(7) NOT NULL );
```

Here, we can see that we ensure the title is unique, so we do not have multiple rows with the same task title. The database will return an error if we try and insert a duplicate. It is always good to enforce your schema rules at the database layer, because you can then ensure that the data you are inserting into the database is what we want.

We could run our migrations manually; however, this is error prone, and makes the deployment and running processes more cumbersome. Instead, we can embed the SQL files into the Rust binary and get the server to run the migrations when starting. We can achieve this by creating a migrations file in our data access layer and building a migrations function with the following code:

```rust
// file: nanoservices/to_do/dal/src/migrations.rs use crate::connections::sqlx_postgres::SQLX_POSTGRES_POOL; pub async fn run_migrations() { println!("Migrating to-do database..."); let mut migrations = sqlx::migrate!("./migrations"); migrations.ignore_missing = true; let result = migrations.run(&*SQLX_POSTGRES_POOL) .await.unwrap(); println!( "to-do database migrations completed: {:?}", result ); }
```

We are aggressive with the unwraps here because if we cannot connect to the database, we do not want to run the rest of the application including the server. For our | file in our data access module we should now have the layout below:

```rust
// file: nanoservices/to_do/dal/src/lib.rs pub mod to_do_items; pub mod connections; #[cfg(feature = "sqlx-postgres")] pub mod migrations; #[cfg(feature = "json-file")] pub mod json_file;
```

We are now able to run our migrations in our networking server with the following code:

```rust
// nanoservices/to_do/networking/actix_server/src/main.rs use actix_web::{App, HttpServer}; mod api; use dal::migrations::run_migrations; #[tokio::main] async fn main() -> std::io::Result<()> { run_migrations().await; HttpServer::new(|| { App::new().configure(api::views_factory) }) .workers(4) .bind("127.0.0.1:8080")? .run() .await }
```

We also need to apply the run_migrations().await; to our ingress workspace before the ingress server is ran because both the individual to-do nanoservice, and the ingress service need to ensure that the migrations have been run before the server starts. We will do this in the next section.

All our rust code is now done, however, you might recall that we altered the schema for updating the to-do item. We now accept an ID, so we must update the schema in the frontend code before testing our integration.

## Refactoring our Frontend

The create API call for our frontend will slightly change the interface, enabling the sending of the ID when updating the to-do item from pending to done. This means altering the ToDoItem interface, and providing a NewToDoItem interface with the following code:

```typescript
// ingress/frontend/src/interfaces/toDoItems.ts export interface NewToDoItem { title: string; status: TaskStatus; } export interface ToDoItem { id: number; title: string; status: TaskStatus; }
```

This means that our create API call now takes the following form:

```typescript
// ingress/frontend/src/api/create.ts import { NewToDoItem, ToDoItems, TaskStatus } from "../interfaces/toDoItems"; import { postCall } from "./utils"; import { Url } from "./url"; export async function createToDoItemCall(title: string) { const toDoItem: NewToDoItem = { title: title, status: TaskStatus.PENDING }; return postCall<NewToDoItem, ToDoItems>( new Url().create, toDoItem, 201 ); }
```

And our update call now needs to pass in the ID with the following code:

```typescript
// ingress/frontend/src/api/update.ts import { ToDoItem, ToDoItems, TaskStatus } from "../interfaces/toDoItems"; import { putCall } from "./utils"; import { Url } from "./url"; export async function updateToDoItemCall( name: string, status: TaskStatus, id: number ) { const toDoItem: ToDoItem = { title: name, status: status, id: id }; return putCall<ToDoItem, ToDoItems>( new Url().update, toDoItem, 200 ); }
```

And that's it for our frontend refactor. Remember, our ToDoItems interface references the ToDoItem interface so everything else is automatically updated.

Finally, we must get the ingress running the migrations of the Postgres database for the to-do nanoservice. We must declare our data access layer in the ingress with the following dependency:

```toml
# ingress/Cargo.toml [dependencies] . . . to-do-dal = { path = "../nanoservices/to_do/dal", package = "dal", features = ["sqlx-postgres"] }
```

Here we can see that we point to the real package name which is "dal", but we might have other packages called "dal" from other nanoservices, so we call the dependency "to-do-dal" so when we reference the dal in our code, we use the crate name to-do-dal. This will prevent clashes in the future.

We can now move onto importing the dal in our main.rs file with the following code:

```rust
// ingress/src/main.rs . . . use to_do_dal::migrations::run_migrations as run_todo_migrations; . . .
```

And then we run our migrations in the main function with the code below:

```rust
// ingress/src/main.rs . . . #[tokio::main] async fn main() -> std::io::Result<()> { run_todo_migrations().await; HttpServer::new(|| { . . . }) .bind("0.0.0.0:8001")? .run() .await }
```

We also need an .env file in the root of our ingress workspace with the following contents:

```bash
// ingress/.env TO_DO_DB_URL=postgres://username:password@localhost/to_do
```

And our run server bash script needs to export these environment variables resulting in the code below:

```bash
# ingress/scripts/run_server.sh #!/usr/bin/env bash # navigate to directory SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )" cd $SCRIPTPATH cd .. export $(cat .env | xargs) cd frontend npm install npm run build cd .. cargo clean cargo run
```

And there we have it! We have integrated Postgres into our app. You can run it, and as long as your database is running, the migrations will run, and you can interact with your application in the browser.

## Summary

In this chapter, we performed a massive refactor. We swapped out an entire storage engine and got to grips with Docker. Although there was a lot of moving parts, due to the structure of the code, the refactor was clean, and the interfaces enabled us to make major changes in the data access layer, and then minimal changes throughout the system that reference the data access layer. Furthermore, because we utilized traits, we have made it easier for our us to implement other storage engines and swap them out when needed. On top of this, we embedded the SQL migrations into the Rust binary with the SQLX crate. So, we now have a singular Rust binary that runs migration scripts on the database, serves the frontend JavaScript app, and serves backend API endpoints to the frontend application. We are nearly at a full functioning web application that can be easily deployed. In the next chapter, we will finish up our application by handling user sessions.

## Questions

What are the advantages of having a database over a JSON file?

How do you create a migration?

How can we design out application to handle swapping out different database handles

If we wanted to create a user data model in Rust with a name, and age what should we do?

What is a connection pool and why should we use it?

What is an SQL injection

## Answers

The database has advantages in terms of multiple reads and writes at the same time. The database also checks the data to see if it is the right format before inserting it and we can do advanced queries with linked tables.

We install the SQLX client and define the database URL in the .env file. We then create migrations using the client, and write the desired schema required for the migration. We then build a function that runs the migrations on the database when called. This function can fire just before the server runs.

We exploit traits. First, we define a trait that has the function we need for the database operation. We then implement the trait for the storage engine we need. We then have a generic parameter for a function that is the API endpoint. This generic parameter must have the trait for the data transaction implemented. This gives us the power to then swap out to whatever handle we have implemented for the data transaction trait.

We define a NewUser struct with just the name as a string and age as an integer. We then create a User struct with the same field and an extra integer field which is the ID.

A connection pool pools a limited number of connections that connect to the database. Our application then passes these connections to the threads that need it. This keeps the number of connections connecting to the database limited to avoid the database being overloaded.

An SQL injection is where we insert another SQL query into a parameter of another SQL query. This enables us to run malicious queries against the database. For instance, if a name parameter was not sanitized and just put into the SQL query, we could enter another SQL query as the name, and that query would get run on the database.

## Appendix

```rust
// file: nanoservices/to_do/dal/src/to_do_items/ // transactions/get.rs use crate::to_do_items::schema::ToDoItem; use glue::errors::NanoServiceError; use std::future::Future; #[cfg(feature = "json-file")] use super::super::descriptors::JsonFileDescriptor; #[cfg(feature = "json-file")] use crate::json_file::get_all; #[cfg(feature = "json-file")] use std::collections::HashMap; #[cfg(feature = "sqlx-postgres")] use crate::connections::sqlx_postgres::SQLX_POSTGRES_POOL; #[cfg(feature = "sqlx-postgres")] use super::super::descriptors::SqlxPostGresDescriptor; #[cfg(feature = "sqlx-postgres")] use glue::errors::NanoServiceErrorStatus; pub trait GetAll { fn get_all() -> impl Future<Output = Result<Vec<ToDoItem>, NanoServiceError>> + Send; } #[cfg(feature = "sqlx-postgres")] impl GetAll for SqlxPostGresDescriptor { fn get_all() -> impl Future<Output = Result<Vec<ToDoItem>, NanoServiceError>> + Send { sqlx_postgres_get_all() } } #[cfg(feature = "json-file")] impl GetAll for JsonFileDescriptor { fn get_all() -> impl Future<Output = Result<Vec<ToDoItem>, NanoServiceError>> + Send { json_file_get_all() } } #[cfg(feature = "sqlx-postgres")] async fn sqlx_postgres_get_all() -> Result<Vec<ToDoItem>, NanoServiceError> { let items = sqlx::query_as::<_, ToDoItem>(" SELECT * FROM to_do_items" ).fetch_all(&*SQLX_POSTGRES_POOL).await.map_err(|e| { NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?; Ok(items) } #[cfg(feature = "json-file")] async fn json_file_get_all() -> Result<Vec<ToDoItem>, NanoServiceError> { let tasks = get_all::<ToDoItem>().unwrap_or_else(|_| HashMap::new() ); let items = tasks.values().cloned().collect(); Ok(items) }
```

```rust
// file: nanoservices/to_do/dal/src/to_do_items/ // transactions/update.rs use crate::to_do_items::schema::ToDoItem; use glue::errors::NanoServiceError; use std::future::Future; #[cfg(feature = "json-file")] use super::super::descriptors::JsonFileDescriptor; #[cfg(feature = "json-file")] use crate::json_file::{get_all, save_all}; #[cfg(feature = "json-file")] use std::collections::HashMap; #[cfg(feature = "sqlx-postgres")] use crate::connections::sqlx_postgres::SQLX_POSTGRES_POOL; #[cfg(feature = "sqlx-postgres")] use super::super::descriptors::SqlxPostGresDescriptor; #[cfg(any(feature = "json-file", feature = "sqlx-postgres"))] use glue::errors::NanoServiceErrorStatus; pub trait UpdateOne { fn update_one(item: ToDoItem) -> impl Future<Output = Result<ToDoItem, NanoServiceError>> + Send; } #[cfg(feature = "sqlx-postgres")] impl UpdateOne for SqlxPostGresDescriptor { fn update_one(item: ToDoItem) -> impl Future<Output = Result<ToDoItem, NanoServiceError>> + Send { sqlx_postgres_update_one(item) } } #[cfg(feature = "json-file")] impl UpdateOne for JsonFileDescriptor { fn update_one(item: ToDoItem) -> impl Future<Output = Result<ToDoItem, NanoServiceError>> + Send { json_file_update_one(item) } } #[cfg(feature = "sqlx-postgres")] async fn sqlx_postgres_update_one(item: ToDoItem) -> Result<ToDoItem, NanoServiceError> { let item = sqlx::query_as::<_, ToDoItem>(" UPDATE to_do_items SET title = $1, status = $2 WHERE id = $3 RETURNING *" ).bind(item.title) .bind(item.status.to_string()) .bind(item.id) .fetch_one(&*SQLX_POSTGRES_POOL).await.map_err(|e| { NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?; Ok(item) } #[cfg(feature = "json-file")] async fn json_file_update_one(item: ToDoItem) -> Result<ToDoItem, NanoServiceError> { let mut tasks = get_all::<ToDoItem>().unwrap_or_else(|_| HashMap::new() ); if !tasks.contains_key(&item.title) { return Err(NanoServiceError::new( format!("Item with name {} not found", item.title), NanoServiceErrorStatus::NotFound )); } tasks.insert(item.title.clone(), item.clone()); let _ = save_all(&tasks)?; Ok(item) }
```
