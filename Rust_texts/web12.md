# 13 Observability through logging

We now have a working to-do application that can either run as a single binary or run as multiple servers. However, we do not actually know what is going on inside our system. Let us say that our system makes a request from on server to another. How do we know that this request was made and what the response was. We don’t. We can try and work out what happened from the error message returned to the frontend, but this might not be clear. We also might not want to expose intricate details of the error to the frontend. To remedy this, we can produce logs of these requests and how the request travels through the system. This also gives us the power to inspect the steps that lead up to the error. Logging also enables us to keep an eye on the health of the system. In this chapter, we will cover the following:

What RESTful services are

Building frontend code on command

Logging via the terminal

Logging via the database

By the end of this chapter, you will be able to log what is going on in your program including all of the requests and response codes by implementing middleware for our logger. You will also be able to create background tasks where our program can send logs to this background task to send to the database, taking pressure off our main program. Finally, we can perform queries on our elasticsearch database to look for particular logs.

## Technical requirements

This chapter will be relying on the code in the previous chapter that can be found at the following link:

`https://github.com/PacktPublishing/Rust-Web-Programming-3E/tree/main/chapter13`

## What are RESTful services?

REST stands for representational state transfer. It is an architectural style for our application programming interface (API) to read (GET), update (PUT), create (POST), and delete (DELETE) our users and to-do items. The goal of a RESTful approach is to increase speed/performance, reliability, and the ability to grow by reusing components that can be managed and updated without affecting the system.

You may have noticed that before Rust, slow, high-level languages seemed to be a wise choice for web development. This is because they are quicker and safer to write. This is due to the main bottleneck for the speed of processing data in web development being the network connection speed. The RESTful design aims to improve the speed by economizing the system, such as reducing API calls, as opposed to just focusing on algorithm speed. With that in mind, in this section, we can explore the following RESTful concepts:

Layered system: This enables us to add extra functionality, such as authorization, without having to change the interface.

Uniform system: This simplifies and decouples the architecture, enabling whole parts of the application to evolve independently without clashing.

Statelessness: This ensures that our application does not directly save anything on the server. This has implications for microservices and cloud computing.

Logging: This enables us to peek into our application and see how it runs, exposing undesirable behavior even if there are no errors displayed.

Caching: This enables us to store data in hot memory to reduce the number of calls to a database.

Code on demand: This is where our backend server directly runs code on the frontend.

Here we can see that we are locking the standard output and writing it. This makes sense as printing to the terminal would not be very useful if half of one message got printed alongside half of another message. This lock mechanism results in a reduction in performance. For instance, it is good practice to log every request that is sent to the server. If we log our requests using println!, even if we have four threads processing requests, each thread processing a request would have to wait their turn to acquire the lock, essentially holding up all threads to a single bottleneck. To stop this from happening, we create a global logger that accepts all logs from all threads. This logger will also remain live for the entire duration of the program.

We can define our logger with the code below:

```rust
// glue/src/logger.rs
use tracing::Level;
use tracing_subscriber::FmtSubscriber;
pub fn init_logger() {
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .finish();
    tracing::subscriber::set_global_default(subscriber)
        .expect("Failed to set up logger");
}
```

At the start of our program, we will call the init_logger() function to initialize our global logger. We also do not want dependent programs having to install the tracing crate on their own dependences to log messages. Having to install tracing crate multiple times and lead to extra work when trying to update and maintain the tracing crate dependency. To prevent this, we create some wrapper functions around the tracing macros with the following code:

```rust
// glue/src/logger.rs
pub fn log_info(message: &str) {
    tracing::info!("{}", message);
}
pub fn log_warn(message: &str) {
    tracing::warn!("{}", message);
}
pub fn log_error(message: &str) {
    tracing::error!("{}", message);
}
pub fn log_debug(message: &str) {
    tracing::debug!("{}", message);
}
pub fn log_trace(message: &str) {
    tracing::trace!("{}", message);
}
```

And with this, we can now initialize our logger and produce log messages. However, what about logging every HTTP request sent to the server? Sure, we could write log_info for every API endpoint, but this would be a pain to write and maintain. Instead, we can build some middleware to log every HTTP request for us.While our running code in the browser does work, it will leave a dangling entry in our cache. If we want to remove the user session from the cache, we will have to make a HTTP request with the user ID. At this stage in the book, you should be able to add this API endpoint. Covering the addition of this API endpoint at this stage in the book will merely bloat the chapter. If you choose not to add this endpoint, your to-do application will work ok for the rest of the book, but the memory consumption of the cache will grow over time.

Now that we have covered code on command, we can move onto the focus of this chapter which is observability. We will start this topic with a basic terminal logger. But first, we should explore what logging is.

## What is logging?

So far, our application does not log anything. This does not directly affect the running of the app. However, there are some advantages to logging. Logging enables us to debug our applications. Right now, as we are developing locally, it may not seem like logging is really needed. However, in a production environment, there are many reasons why an application can fail, including Docker container orchestration issues. Logs that note what processes have happened can help us to pinpoint an error. We can also use logging to see when edge cases and errors arise for us to monitor the general health of our application. When it comes to logging, there are four types of logs that we can build:

Informational (info): This is general logging. If we want to track a general process and how it is progressing, we use this type of log. Examples of using this are starting and stopping the server and logging certain checkpoints that we want to monitor, such as HTTP requests.

Verbose: This is information such as the type defined in the previous point. However, it is more granular to inform us of a more detailed flow of a process. This type of log is mainly used for debugging purposes and should generally be avoided when it comes to production settings.

Warning: We use this when we are logging a process that is failing and should not be ignored. However, we can use this instead of raising an error because we do not want the service to be interrupted or the user to be aware of the specific error. The logs themselves are for us to be alerted of the problem to allow us to then act. Problems such as calls to another server failing are appropriate for this category.

Error: This is where the process is interrupted due to an error and we need to sort it out as quickly as possible. We also need to inform the user that the transaction did not go through. A good example of this is a failure to connect or insert data into a database. If this happens, there is no record of the transaction happening and it cannot be solved retroactively. However, it should be noted that the process can continue running.

If a warning comes up about the server failing to send an email, connect to another server to dispatch a product for shipping, and so on. Once we have sorted out the problem, we can retroactively make a database call to transactions in this timeframe and make the calls to the server with the right information.

In the worst case, there will be a delay. With the error type, we will not be able to make the database call as the server was interrupted by an error before the order was even entered in the database. Considering this, it is clear why error logging is highly critical, as the user needs to be informed that there is a problem and their transaction did not go through, prompting them to try again later.

We could consider the option of including enough information in the error logs to retroactively go back and update the database and complete the rest of the process when the issue is resolved, removing the need to inform the user. While this is tempting, we must consider two things. Log data is generally unstructured.

There is no quality control for what goes into a log. Therefore, once we have finally managed to manipulate the log data into the right format, there is still a chance that corrupt data could find its way into the database.

The second issue is that logs are not considered secure. They get copied and sent to other developers in a crisis and they can be plugged into other pipelines and websites, such as Bugsnag, to monitor logs. Considering the nature of logs, it is not good practice to have any identifiable information in a log.

Now we are all juiced up knowing that logs rock, we can start our logging journey by building a basic logger that writes to the terminal.

## Logging via the terminal

Considering our system has multiple servers, and these servers might be running by themselves, or all compiled into one binary in the ingress. Considering the flexibility of our system, we must define a logger in one place, and import this logger into any service that wants to log anything. We can supply all our servers with a logger via the glue workspace with the following directory layout:

```text
├── Cargo.toml
└── src
    ├── errors.rs
    ├── lib.rs
    ├── logger
    │   ├── logger.rs
    │   ├── mod.rs
    │   └── network_wrappers
    │       ├── actix_web.rs
    │       └── mod.rs
    └── token.rs
```

To integrate our logger, we must carry out the following steps:

Define a logger

Create a logging middleware

Integrate our logger into our servers

We can start by building out a basic logger.

### Defining a logger

Before we write any code, we must have the following dependencies in our | workspace:

```rust
# glue/Cargo.toml
. . .
[dependencies]
. . .
tracing = "0.1.4"
tracing-subscriber = "0.3.18"
futures-util = "0.3.30"
```

We will be using the tracing crate to define some logging functions, and the tracing-subscriber to define a logger that will subscribe to the log events being produced. Our basic logger is just printing to the terminal. Why are we not just using println!? If we use println!(“hello world”), we are essentially running the following code:

```rust
use std::io::{stdout, Write};
let mut lock = stdout().lock();
write!(lock, "hello world").unwrap();
```

Here we can see that we are locking the standard output and writing it. This makes sense as printing to the terminal would not be very useful if half of one message got printed alongside half of another message. This lock mechanism results in a reduction in performance. For instance, it is good practice to log every request that is sent to the server. If we log our requests using println!, even if we have four threads processing requests, each thread processing a request would have to wait their turn to acquire the lock, essentially holding up all threads to a single bottleneck. To stop this from happening, we create a global logger that accepts all logs from all threads. This logger will also remain live for the entire duration of the program.

We can define our logger with the code below:

```rust
// glue/src/logger.rs
use tracing::Level;
use tracing_subscriber::FmtSubscriber;
pub fn init_logger() {
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .finish();
    tracing::subscriber::set_global_default(subscriber)
        .expect("Failed to set up logger");
}
```

At the start of our program, we will call the init_logger() function to initialize our global logger. We also do not want dependent programs having to install the tracing crate on their own dependences to log messages. Having to install tracing crate multiple times and lead to extra work when trying to update and maintain the tracing crate dependency. To prevent this, we create some wrapper functions around the tracing macros with the following code:

```rust
// glue/src/logger.rs
pub fn log_info(message: &str) {
    tracing::info!("{}", message);
}
pub fn log_warn(message: &str) {
    tracing::warn!("{}", message);
}
pub fn log_error(message: &str) {
    tracing::error!("{}", message);
}
pub fn log_debug(message: &str) {
    tracing::debug!("{}", message);
}
pub fn log_trace(message: &str) {
    tracing::trace!("{}", message);
}
```

And with this, we can now initialize our logger and produce log messages. However, what about logging every HTTP request sent to the server? Sure, we could write log_info for every API endpoint, but this would be a pain to write and maintain. Instead, we can build some middleware to log every HTTP request for us.

### Creating a logging middleware

Previously we have implemented the FromRequest trait for our JWT. The FromRequest trait enabled us to extract the token from the header and pass the extracted ID from the token into the view. However, we must pass the JWT struct into the view for this process to work. For our purpose, we need our logger to automatically work on every request, and we want the logger to also log the response code. First, we create an Actix logger struct implement the Transform trait. The Transform trait is essentially the interface of a service factory. A service is an async function that converts a Request to a Response. For our middleware, we initially need to import the following code:

```rust
// glue/src/network_wrappers/actix_web.rs
use actix_web::{dev::ServiceRequest, dev::ServiceResponse, Error};
use actix_web::dev::{Transform, Service};
use futures_util::future::{ok, Ready};
use std::task::{Context, Poll};
use std::pin::Pin;
```

We can then implement the Transform trait for a struct called ActixLogger that we create with the code below:

```rust
// glue/src/network_wrappers/actix_web.rs
pub struct ActixLogger;
impl<S, B> Transform<S, ServiceRequest> for ActixLogger
where
    S: Service<
        ServiceRequest,
        Response = ServiceResponse<B>,
        Error = Error
    > + 'static,
    S::Future: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = LoggingMiddleware<S>;
    type Future = Ready<Result<
        Self::Transform,
        Self::InitError
    >>;
    fn new_transform(&self, service: S) -> Self::Future {
        ok(LoggingMiddleware { service })
    }
}
```

There is a lot going on here, but we can break it down by focusing on the new_transform function signature. Here we take in a service which is denoted by S which is the following definition:

```rust
S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error > + 'static,
S::Future: 'static,
```

This signature is essentially a `ServiceRequest`, that returns a response and is a future which is an async function. We can see that the new_transform function returns the service that is wrapped in a struct called LoggingMiddleware. We will build the LoggingMiddleware struct, and implement the `Service<ServiceRequest>` trait with the code below:

```rust
// glue/src/network_wrappers/actix_web.rs
pub struct LoggingMiddleware<S> {
    service: S,
}
impl<S, B> Service<ServiceRequest> for LoggingMiddleware<S>
where
    S: Service<
        ServiceRequest,
        Response = ServiceResponse<B>,
        Error = Error
    > + 'static,
    S::Future: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = Pin<
        Box<
            dyn futures_util::Future<
                Output = Result<
                    Self::Response,
                    Self::Error
                >
            >
        >
    >;
    fn poll_ready(&self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.service.poll_ready(cx)
    }
    fn call(&self, req: ServiceRequest) -> Self::Future {
        . . .
    }
}
```

Again, this is a lengthy chunk of code with a lot going on. However, we can see on the call function, we accept a request and return a Future. This Future is pinned because we do not want the future to move in memory because the thread will cycle back to the future to poll the future again and we do not want the thread to access memory where the future used to be. We also put the future in a box because we do not know what the size of the future, and the boxing puts the future on the heap memory. Inside our call function, we define the logging logic with the code below:

```rust
// glue/src/network_wrappers/actix_web.rs
fn call(&self, req: ServiceRequest) -> Self::Future {
    let fut = self.service.call(req);
    Box::pin(async move {
        let res = fut.await?;
        let req_info = format!(
            "{} {} {}",
            res.request().method(),
            res.request().uri(),
            res.status().as_str()
        );
        tracing::info!("Request: {}", req_info);
        Ok(res)
    })
}
```

We can see that we log the method, endpoint, and response code. Finally, we ensure that our logger is available in the library with the following declarations:

```rust
// glue/src/logger/network_wrappers/mod.rs
#[cfg(feature = "actix")]
pub mod actix_web;
// glue/src/logger/mod.rs
pub mod network_wrappers;
pub mod logger;
// glue/src/lib.rs
pub mod errors;
pub mod token;
pub mod logger;
```

With this, our logger is ready to be integrated into our servers.

### Integrating our logger into our servers

For our servers, they all follow the same template. We wrap the ActixLogger in our server definition. All our servers should have a layout like the following:

```rust
. . .
use glue::logger::{ logger::init_logger, network_wrappers::actix_web::ActixLogger };
use actix_cors::Cors;
#[tokio::main]
async fn main() -> std::io::Result<()> {
    init_logger();
    run_migrations().await;
    HttpServer::new(|| {
        let cors = Cors::default().allow_any_origin()
            .allow_any_method()
            .allow_any_header();
        App::new().wrap(ActixLogger)
            .wrap(cors)
            .configure(api::views_factory)
    })
    .workers(4)
    .bind("127.0.0.1:8081")?
    .run()
    .await
}
```

Here we can see that we initialize the logger with the init_logger function, and then carry on with the rest of the process. For the ingress we must run both migrations for both servers, but the outline is the same. Repeating the code three times would unnecessarily bloat the chapter.

If we run the ingress server, create a user, login, and create an item, we should get logs like the one below:

```text
2024-07-19T23:36:02.320369Z INFO glue::logger::network_wrappers::actix_web: Request: POST /api/v1/users/create 201
2024-07-19T23:36:02.767376Z INFO glue::logger::network_wrappers::actix_web: Request: GET /api/v1/auth/login 200
2024-07-19T23:36:22.298595Z INFO glue::logger::network_wrappers::actix_web: Request: GET /api/v1/auth/login 200
2024-07-19T23:36:22.310567Z INFO glue::logger::network_wrappers::actix_web: Request: GET /api/v1/get/all 200
2024-07-19T23:36:38.671171Z INFO glue::logger::network_wrappers::actix_web: Request: POST /api/v1/create 201
```

Here we can see that the timestamp is also present. This is by default for the logger and should be done as they help with putting together what has happened. Timestamps can also help us sort logs later of. For instance, writing a log to disk the very instance that the log was created is not essential. We could batch logs together in memory to perform a batch write if needed. If we have the timestamp, it does not matter what order the logs are written to disk.

Right now, are logging system is printing out to the terminal. This is ok for local development. However, our system might grow to multiple isolated servers running by themselves. If we have multiple servers running by themselves, we would like to have a single place where we can see all the logs. We can have certain Kubernetes based apps like Lens that enable you to just click on the pods in the cluster to inspect the logs. However, this requires that you have kubernetes running. You could be running different apps on different physical servers that are not connected. Or you could be running all your apps on one device without Docker, or just using docker-compose. Whatever your choice of deployment, a good bet is writing your logs to a database so they can be inspected by other applications and dashboards that have access to the database.

To achieve maximum flexibility, we can move onto building a logging mechanism for a remote database.

## Logging via a database

You may have heard of open-telemetry, or a range of dashboards such as Jaeger. However, at the time of writing this, a lot of the Rust libraries for such platforms lacking in documentation, maturity, and introducing breaking changes. To show you how to setup open telemetry and off the shelf dashboards would result in the book aging quickly. With the skills you have learnt in this book, there is nothing stopping you Googling the latest configuration steps to run these off the shelf projects. For the rest of this chapter, we are going to build our own async mechanism that shoots logs off to a database allowing you to query this database for specific logs. Our async mechanism is going to keep the pressure off the logging functionality directly as we log every request. If we had to await a response from the database for every request, we would slow down our request processing times down dramatically. Instead, we are going to shoot a log message off to an async actor, and the async actor is going to handle the logging of the message in the database.

Learning this approach will also give you the skillset to offload other tasks from the request process if needed using the actor approach. Before we set off building the logging mechanism however, we must define what an actor is.

### What is an actor?

An actor is an isolated piece of code that can have state. This actor only communicates to other parts of the program via messages. The actor can have its own internal state that can be updated due to messages. The actor can also send messages to other actors or even create actors. While I do personally like the actor model, we must be careful when are where we apply the actor model. There is some overhead sending messages and spawning tasks to run actors. So, if your actor is only going to do a couple of operations before finishing, then you might as well just call a couple of async functioning inside your future.

Actors really shine when you have a long running process, and you want to keep sending messages to. This can allow you to take pressure off the task you are currently performing. It can also keep resources allocated to one actor as opposed to needing to reallocate those resources. For instance, when we make a network connection, there is usually a handshake consisting of message back and forth to establish that connection. Having an actor maintain that connection and accept messages to send over that connection can reduce the number of handshakes you need to make. Another example is opening a file. When you open a file, the operating system needs to check if the file is there, if the reader has permissions, and must acquire a lock for that file. Instead of opening a file to perform one transaction to it and then closing it, we can have an actor maintain the handle of that file and write to it the messages that were sent to the actor. Another advantage to remember is that channels can also act as queues. Channels can have messages build up in them with the actor consuming them at the actors own pace as long as the computer has enough memory to keep the messages in memory, and the channel has enough allocated capacity.

For our logging, we are going to have an actor that consumes log messages from all over the program and send those logs to a database as shown in figure 13.1.

Figure 13.1 – Our logging actor

Now that we know what actors are and how we are going to use them, we can start building our remote logging system by building our actor.

### Building our logging actor

Before we build our actor, we are need the following crates in our glue workspace:

```rust
// glue/Cargo.toml
. . .
[dependencies]
. . .
serde_json = { version = "1.0.120", optional = true }
tokio = { version = "1.38.1", optional = true}
reqwest = { version = "0.12.5", optional = true }
chrono = { version = "0.4.38", optional = true }
once_cell = { version = "1.19.0", optional = true }
[features]
# default = ["elastic-logger", "actix"]
actix = ["actix-web"]
elastic-logger = [
    "serde_json",
    "tokio",
    "reqwest",
    "chrono",
    "once_cell"
]
```

Here we can see that our elastic-logger feature utilizes all the new dependencies, but we do not need them if we are not performing remote logging as we will not have an actor to support.

We then define our elastic logger actor model with the code below:

```rust
// glue/src/logger/mod.rs
pub mod network_wrappers;
pub mod logger;
#[cfg(feature = "elastic-logger")]
pub mod elastic_actor;
```

Inside our actor module, we must import the following before we can define our actor:

```rust
// glue/src/logger/elastic_actor.rs
use tokio::sync::mpsc;
use tokio::sync::mpsc::{Receiver, Sender};
use serde_json::json;
use reqwest::{Client, Body};
use chrono::Utc;
use once_cell::sync::Lazy;
use serde::Serialize;
```

Our actor is going to need to accept messages with enough data in to create a log and send it. These messages need to be sent over a channel. Our message takes the form below:

```rust
// glue/src/logger/elastic_actor.rs
#[derive(Debug, Serialize)]
struct LogMessage {
    level: String,
    message: String,
}
```

With this message, we can then build our send_log function with the following code:

```rust
// glue/src/logger/elastic_actor.rs
pub async fn send_log(level: &str, message: &str) {
    static LOG_CHANNEL: Lazy<Sender<LogMessage>> = Lazy::new(|| {
        let (tx, rx) = mpsc::channel(100);
        tokio::spawn(async move {
            elastic_actor(rx).await;
        });
        tx
    });
    LOG_CHANNEL.send(LogMessage {
        level: level.to_string(),
        message: message.to_string(),
    }).await.unwrap();
}
```

Here, we can see that we exploit the Lazy::new create our channel, spawn our actor, and return the channel sender. The Lazy::new only gets executed once, so no matter where we call the send_log function, we are sending messages to that one actor that has been spawned the first time the send_log function was called.

For our actor, we define the outline with the code below:

```rust
// glue/src/logger/elastic_actor.rs
async fn elastic_actor(mut rx: Receiver<LogMessage>) {
    let elastic_url = std::env::var(
        "ELASTICSEARCH_URL"
    ).unwrap();
    let client = Client::new();
    while let Some(log) = rx.recv().await {
        . . .
    }
}
```

Here we can see that we get the URL to the database, establish a HTTP client, and then run an infinite loop where we cycle through an interation of the loop every time a message is received from the channel. Once we get the message from the channel, we create a JSON body and send it via HTTP to the elastic search database with the following code:

```rust
// glue/src/logger/elastic_actor.rs
let body = json!({
    "level": log.level,
    "message": log.message,
    "timestamp": Utc::now().to_rfc3339()
});
let body = Body::from(serde_json::to_string(&body)
    .unwrap());
match client.post(&elastic_url)
    .header("Content-Type", "application/json")
    .header("Accept", "application/json")
    .body(body)
    .send()
    .await {
    Ok(result) => {},
    Err(e) => {
        eprintln!(
            "Failed to send log to Elasticsearch: {}",
            e
        );
    }
}
```

We can see that we directly unwrap the serialization of the message. This is because there will never be an error translating two strings and a timestamp to a JSON string. However, there could be an issue in sending the HTTP request to the elasticsearch database, therefore we handle the potential error when sending the HTTP request. This is because our actor is running in the background for the entire duration of the program. We would hate for a temporary network error to kill our actor blocking us from sending any more logs to the database.

Our actor can now run and accept messages if we call the send_log function anywhere in the codebase. We now need to call the send_log function in our logging function.

### Update logging functions

For our logging functions, we still need to log the message to the terminal so logs can still be recovered if there is a problem with our database. This sending of the message to the database should be triggered if the feature is enabled. Considering this, our | function remains the same giving us the following outline:

```rust
// glue/src/logger/logger.rs
. . .
#[cfg(feature = "elastic-logger")]
use super::elastic_actor::send_log;
pub fn init_logger() {
    . . .
}
```

Our functions now take the form below:

```rust
// glue/src/logger/logger.rs
pub async fn log_info(message: &str) {
    tracing::info!("{}", message);
    #[cfg(feature = "elastic-logger")]
    send_log("INFO", message).await;
}
pub async fn log_warn(message: &str) {
    tracing::warn!("{}", message);
    #[cfg(feature = "elastic-logger")]
    send_log("WARN", message).await;
}
pub async fn log_error(message: &str) {
    tracing::error!("{}", message);
    #[cfg(feature = "elastic-logger")]
    send_log("ERROR", message).await;
}
pub async fn log_debug(message: &str) {
    tracing::debug!("{}", message);
    #[cfg(feature = "elastic-logger")]
    send_log("DEBUG", message).await;
}
pub async fn log_trace(message: &str) {
    tracing::trace!("{}", message);
    #[cfg(feature = "elastic-logger")]
    send_log("TRACE", message).await;
}
```

With this, our system is now in place to send messages to the database. We now must update our Cargo.toml files for all our elastic-logger feature for the glue module.

// ingress/Cargo.toml
. . .
glue = { path = "../glue", features = ["actix", "elastic-logger"] }
. . .
// nanoservices/auth/networking/actix_server/Cargo.toml
. . .
glue = { path = "../../../../glue", features = ["actix", "elastic-logger"] }
. . .
// nanoservices/to_do/networking/actix_server/Cargo.toml
. . .
glue = { path = "../../../../glue", features = ["actix", "elastic-logger"] }
. . .

And now our system is fully ready to send logs to our database, we now must configure our database.

### Configuring our logging database

To enable our database to accept logs, we add the following service to our docker-compose.yml:

```yaml
# docker-compose.yml
. . .
elasticsearch:
  image: docker.elastic.co/elasticsearch/elasticsearch:7.13.4
  container_name: elasticsearch
  environment:
    - discovery.type=single-node
    - bootstrap.memory_lock=true
    - "ES_JAVA_OPTS=-Xms512m -Xmx512m"
  ulimits:
    memlock:
      soft: -1
      hard: -1
  ports:
    - "9200:9200"
    - "9300:9300"
```

For the elasticsearch database, we have the following configurations:

discovery.type=single-node: This configures Elasticsearch to run in single-node mode, which is suitable for development or testing environments.

bootstrap.memory_lock=true: This ensures that the JVM locks the memory used by Elasticsearch, preventing it from being swapped out by the operating system.

"ES_JAVA_OPTS=-Xms512m -Xmx512m": This sets the Java heap size options, specifying that both the initial and maximum heap sizes should be 512 MB.

memlock: This prevents the container from swapping memory to disk. Setting both soft and hard limits to -1 means unlimited memory lock.

9200:9200: Maps port 9200 of the container to port 9200 of the host. This is the default port for Elasticsearch HTTP API.

9300:9300: Maps port 9300 of the container to port 9300 of the host. This is the default port for Elasticsearch transport node communication.

Elasticsearch is an ideal database for storing logs because elasticsearch can handle high throughputs where we can distribute the writes over multiple nodes if needed. Elasticsearch also enables search throughout the logs, so we can search for keywords and log levels. There are also dashboards like Kibana that can connect to the database to offer a range of out of the box display options.

Due to the new database, our .env file should look like the following:

```text
TO_DO_DB_URL=postgres://username:password@localhost/to_do
AUTH_DB_URL=postgres://username:password@localhost/to_do
AUTH_API_URL=http://127.0.0.1:8081
JWT_SECRET=secret
CACHE_API_URL=redis://127.0.0.1:6379
ELASTICSEARCH_URL=http://localhost:9200/logs/_doc
```

And this is it, our system is now able to start sending logs out our database.

If we run our server, and then create a user a login using our ingress/scripts/create_login.sh script, we can make a CURL request to our database with the command below:

```bash
curl -X GET "http://localhost:9200/logs/_search?pretty" \
  -H 'Content-Type: application/json' -d'
{
  "query": {
    "match": {
      "level": "INFO"
    }
  }
}'
```

Here, we are making a query for all logs with a level of INFO. Our query gives us the following results:

```json
{
  "took" : 39,
  "timed_out" : false,
  "_shards" : {
    "total" : 1,
    "successful" : 1,
    "skipped" : 0,
    "failed" : 0
  },
  "hits" : {
    "total" : {
      "value" : 2,
      "relation" : "eq"
    },
    "max_score" : 0.18232156,
    "hits" : [
      {
        "_index" : "logs",
        "_type" : "_doc",
        "_id" : "NbbV15AB4EMJAczNwoWr",
        "_score" : 0.18232156,
        "_source" : {
          "level" : "INFO",
          "message" : "Request: POST /api/v1/users/create 201",
          "timestamp" : "2024-07-22T00:27:08.721360+00:00"
        }
      },
      {
        "_index" : "logs",
        "_type" : "_doc",
        "_id" : "NrbV15AB4EMJAczNwoWr",
        "_score" : 0.18232156,
        "_source" : {
          "level" : "INFO",
          "message" : "Request: GET /api/v1/auth/login 200",
          "timestamp" : "2024-07-22T00:27:09.167054+00:00"
        }
      }
    ]
  }
}
```

Here, we can see that both of our logs are here in the database. We can also see that we have a score. The higher the score, the more relevant the log is to the search. We can also see that the scores in the logs are the same as the maximum scores. Our level: “INFO” matches exactly. We could be more granular and produce even more tags on our logs. For instance, we could also put in a tag for the service and filter by this if we want. As your system gets more complex, logging on a database can be a lifesaver.

## Summary

In this chapter, we setup a basic logger, and then created an actor to take pressure off the logger when sending those logs to an elasticsearch database. With our logging setup, we can inspect logs from multiple distributed servers, and query those logs. Not only does this help us debug our systems and monitor the health of our system when it is live, but we also now have the skillset to pass off logs to a an actor running in the background so our server is not held up awaiting network calls to the elasticsearch database.

We have now reached the end of the development of the to-do application. You can always add more features and improve things, but we would merely be repeating the concepts that we covered in the book to achieve these new features. For instance, adding a logout button, and implementing the refresh token mechanism for authentication sessions are probably things you want to add to our to-do application, but covering these features would just bloat the book with repetition of updating the local storage in the frontend with a returned refreshed token, and adding a button on the page to hit the logout API. The rest of the book focuses on how to test your server, how to deploy the system, and some more advanced concepts like low-level networking. In the next chapter, we will cover unit testing. To be honest, like stated in the previous chapter, good testing practices are probably the most important software engineering skill that will set you aside from other developers. You will be able to develop complex systems at a safer and faster rate. I’ve spoken publicly about a range of things. When it comes to testing, the outreach is mainly from senior developers and tech leads with reputable backgrounds. Junior developers or developers who have never been pushed to develop their craft don’t really understand testing and how important and useful it is. Instead, they are performing multiple manual steps to test one edge case of their code. It frankly blows my mind that everyone agrees that manual steps for deployment and packaging are bad, and that we should automate them. Yet, these same developers will push back on unit testing, meaning they're ok with manual steps for checking their code once they've coded it. Running an isolated test that has automated all the steps needed to test the code you are building, saves you running those manual steps again and again throughout the day of coding. It also helps you pick up bugs so you're not firefighting later, and when you refactor, you just run the tests every time you make a small change because they're so quick and easy to run. You then know the instant you introduce some breaking code that it is breaking code because the effort threshold of running all the tests is so low, you do it frequently. So, no routing through the codebase to work out what change created the break. If you have a good unit and end-to-end testing strategy, then you will develop better quality code at a much faster rate than the average developer because the average developer just hasn't taken the time to refine their testing technique. Instead, they are performing multiple manual steps to test their code. This is the power of testing and test-driven development. The first benefit is the speed of development. One of the biggest reasons why I can work full time for a cutting-edge database company, push forward the application of surgical robotics in one of the biggest bioengineering departments in the world, write books, run a medical simulation software company that the German government uses with a friend, and have a family life with my wife and kids at the same time is mainly due to good unit and end-to-end testing techniques. Running an isolated test that has automated all the steps needed to test the code you are building, saves you running those manual steps again and again throughout the day of coding. It also helps you pick up bugs so you're not firefighting later, and when you refactor, you just run the tests every time you make a small change because they're so quick and easy to run. You then know the instant you introduce some breaking code that it is breaking code because the effort threshold of running all the tests is so low, you do it frequently. So, no routing through the codebase to work out what change created the break. If you have a good unit and end-to-end testing strategy, then you will develop better quality code at a much faster rate than the average developer because the average developer just hasn't taken the time to refine their testing technique. Instead, they are performing multiple manual steps to test their code. This is the power of testing and test-driven development.

## Questions

Why do we use an actor for logging?

What is the advantage of logging to a database?

How do we log every HTTP request?

What is the difference between info, warn, and error logs?

What is code on demand?

## Answers

We use an actor for logging so we can send logs to the database in the background without holding up the main request processing.

The advantage of logging to a database is that we can query the logs from multiple servers in one place, and we can search through the logs for specific keywords or log levels.

We use middleware to log every HTTP request.

Info logs are for general information, warn logs are for processes that are failing but the application can continue, and error logs are for processes that are interrupted due to an error.

Code on demand is where the backend server directly executes code on the frontend.
