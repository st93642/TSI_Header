# Caching auth sessions

While our authentication sessions work, we might as well get more control over them, and explore the concept of caching at the same time. We will do this by embedding our own Rust code directly in a Redis database, so we can perform a range of checks and updates in one call to the cache as opposed to making multiple calls to a database. Caching is a great tool to add to your belt, enabling you to reduce the time and resources used to serve data. In this chapter we cover the following:

What caching is

Setting up Redis

Building a Redis module in Rust

Building a Redis client in Rust

Connecting our cache to our servers

By the end of this chapter, you will be able to build custom caching functionality directly inside Redis to control the state of the user session. However, this caching skill can also be used in a variety of different problems. It must be noted that we are near the end of building out our main application. This means that you will be familiar with a lot of the approaches. This chapter might feel like you are just skimming through the code with repetitive patterns. This is a sign that you have learnt the main approaches of web development in Rust, and you are becoming competent in developing Rust web applications by yourself. Even though we are adding a new storage engine which is Redis, we want to keep our interfaces and IO module layouts consistent.

## Technical requirements

This chapter will be relying on the code in the previous chapter.

## What is caching

Server-side caching is a technique used to store frequently accessed data in a temporary storage location, or cache, on the server. Caching reduces the time and resources required to retrieve data on subsequent requests. By minimizing the need to repeatedly fetch data from slower storage layers, such as databases or external APIs, server-side caching can significantly enhance the performance and scalability of web applications. Generally, we can categorize data into the following:

Hot storage: This memory is quick to access but it also expensive. This is where the data is directly loaded into memory. We have limited amount of live memory so we must be sparing with it or willing to spend a lot. Caches generally consist of hot storage.

Warm storage: This is where the data is still instantly accessible, but it is stored on disk, resulting in a load that is still instant, but slower than hot storage. Warm storage is cheaper, enabling us to store terabytes of data cheaply. Databases generally store most of the data on disk and optimize queries with some caching.

Cold storage: This is where the storage becomes very cheap and reliable, but there is a delay in accessing the data. For instance, at home you would consider cold storage to be storing your data on an optical disk or external hard drive and removing it from the computer. It will take more time to load it as you must get the storage device from its place and insert the storage device into your computer so your computer can access it. However, your storage device is not getting the daily wear and tear of running inside your computer. Cold storage is the best choice for data that is access infrequently. Cloud environments offer a more automated version of cold storage, where the access to the data takes a while, and some cloud providers might charge per read. However, the long-term storage of untouched data in cloud cold storage services is very cheap. This is why old photos on a social media app might take longer to load, as these old photos might be stored in cold storage.

For caching, we generally use hot storage as we want to keep the access to the cached data quick. We are also not depending on our cache for permanent storage, so we do not worry if the cache is wiped. We can get the following benefits from caching:

Speed: By serving data from a server-side cache, applications can respond to user requests much faster than if they had to retrieve the same data from a database or generate it dynamically. This is particularly important for high-traffic websites and applications where milliseconds matter.

Reduced Load on Backend Systems: Server-side caching reduces the number of direct interactions with the database or other backend systems. This not only speeds up response times but also alleviates the load on these systems, allowing them to perform more efficiently and scale better under high demand.

Cost Efficiency: Reducing the frequency of database queries and external API calls can lower operational costs, as these resources are often metered based on usage. Efficient server-side caching can lead to significant savings, especially for large-scale applications.

Improved User Experience: Faster response times lead to a more responsive and engaging user experience. Users are less likely to abandon a slow application, improving retention and satisfaction rates.

We could simply use the RAM of our server as a cache; however, we want our system to be able to support multiple servers if needed. If data is cached in one server, and another server is hit, then we are not going to get the up-to-date cached data. Instead, we are going to need a detached data store. For this book we are going to use Redis.

## Setting up Redis

Redis is an open-source, in-memory data structure store that acts as a database, cache, and message broker. Its primary advantage lies in its high speed, which is largely due to its in-memory architecture. We are using Redis for caching because of the following advantages:

In-Memory Storage: By keeping all data in memory, Redis provides extremely fast read and write operations, achieving sub-millisecond response times. This makes it ideal for caching, where speed is crucial.

Single-Threaded Design: Redis uses an event-driven, single-threaded architecture. This simplifies data access patterns and minimizes context switching, which enhances performance. The single-threaded model helps avoid the complexity of multithreading and race conditions, resulting in consistent performance.

Efficient Data Structures: Redis supports a variety of data types, such as strings, hashes, lists, sets, and sorted sets. These structures are optimized for specific use cases, ensuring efficient data manipulation.

Horizontal Scalability: Redis can scale horizontally through sharding, allowing it to handle millions of requests per second, making it suitable for high-load applications.

Low Latency: The combination of in-memory data storage, optimized data structures, and a single-threaded model results in extremely low latency, enhancing user experience by speeding up response times.

To get our Redis database running, we could just directly reference the Redis image directly in our docker-compose and expose the port just like we do for our Postgres database. However, we are directly embedding our Rust code into the Redis database. We are essentially, building our cache on top of the Redis Docker image. Therefore, we are going to build our cache module and client in our nanoservices directory with the following file structure:

```text
└── nanoservices
    ├── auth
    ├── to_do
    └── user-session-cache
        ├── cache-client
        │   ├── Cargo.toml
        │   └── src
        │       └── lib.rs
        └── cache-module
            ├── Cargo.toml
            ├── Dockerfile
            └── src
                ├── lib.rs
                ├── processes
                │   ├── login.rs
                │   ├── logout.rs
                │   ├── mod.rs
                │   └── update.rs
                └── user_session.rs
```

Here, we can see that our cache-module has a Dockerfile. In our Dockerfile, we initially set the base Docker image which is Rust and install what we need to build the Rust Redis module with the following code:

```bash
# nanoservices/user-session-cache/cache-module/Dockerfile
FROM rust:latest as build
ENV PKG_CONFIG_ALLOW_CROSS=1
RUN apt-get update
RUN apt-get install libclang-dev -y
```

We then set the work directory inside of the image, copy all our Rust code for the Redis module into the image, and build the Rust module with the code below:

```dockerfile
# nanoservices/user-session-cache/cache-module/Dockerfile
WORKDIR /app
COPY . .
RUN cargo build --release
```

We then do a second layer in the Docker build where we just have the Redis image. We do not want all the excessive dependencies in the previous layer of the build, so we copy over the single binary that is the Redis module, expose the port, and finally run the Redis server with the module that we built with the following code:

```bash
# nanoservices/user-session-cache/cache-module/Dockerfile
FROM redis
COPY --from=build \
    /app/target/release/libcache_module.so \
    ./libcache_module.so
EXPOSE 6379
CMD ["redis-server", "--loadmodule", "./libcache_module.so"]
```

Finally, we point to this build in our docker compose file with the code below:

```yaml
# docker-compose.yml
. . .
cache:
  container_name: 'to-do-redis'
  build: './nanoservices/user-session-cache/cache-module'
  restart: always
  ports:
    - '6379:6379'
```

If we try and run our docker compose now, we will just get an error. Before we run our cache, we must build our Redis module.

## Building our Redis module

Before we write any code for our Redis module, we must understand the problem that we are trying to solve with our Redis module. Allowing a token to make authenticated requests with no expiration can be dangerous. If a hacker gets hold of the token, then there will be no limit to the requests that they can make. To solve this, we are going to use a cache that caches user login sessions. Each user login session will have a datetime of when the session was last interacted by. When we update the user session, we will increase a counter by one, and update the last interact by field to the time of the update. If the difference between the update and the last interacted by exceeds a cut off, the cache we have a time out. If the counter exceeds a cut off, we have a suggestion that the token needs refreshing. We could bake the expiry time into the token. It would be a simple solution, however, not only does building a user session cache teach us about caching, it also gives us more fine-grained control over the user session. For instance, we could have an admin server, and an admin user could choose to block a user. As soon as the admin user blocks the user, we could update the cache, instantly invalidating any requests after the blocking. If we did not have a cache and used an expiry time in the JWT, the user will still be able to make requests until the JWT expires and another token is needed.

To define our Redis module, we must have the following Cargo.toml file:

```toml
# nanoservices/user-session-cache/cache-module/Cargo.toml
[package]
name = "cache-module"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib"]

[dependencies]
redis-module = "2.0.7"
chrono = "0.4.38"
bincode = "1.3.3"
serde = { version = "1.0.218", features = ["derive"] }
```

The cdylib stands for "C dynamic library." This type of library is intended to be used from languages other than Rust, such as C, C++, or even Python. As Redis is written in C, our library will have to be a C dynamic library.

Now that we have our Cargo.toml defined, we can define our Rust Redis module with the code below:

```rust
// nanoservices/user-session-cache/cache-module/src/lib.rs
use redis_module::redis_module;
mod processes;
mod user_session;
use processes::{ login::login, update::update, logout::logout };
redis_module! {
    name: "user_sessions",
    version: 1,
    allocator: ( redis_module::alloc::RedisAlloc, redis_module::alloc::RedisAlloc ),
    data_types: [],
    commands: [
        ["login.set", login, "write fast deny-oom", 1, 1, 1],
        ["logout.set", logout, "write fast deny-oom", 1, 1, 1],
        ["update.set", update, "write fast deny-oom", 1, 1, 1],
    ]
}
```

Here, we can see that we have three commands, and the 1s for each command denotes that the first key, last key, and key step are all set to 1. This means that all the commands work with a single key. The "write fast deny-oom" means that we can write the key quickly, and that the write will be denied if the Redis cache runs out of memory.

To get these commands built out, we must follow the steps below:

Defining the user session

Building the login process

Building the logout process

Building the update process

We can now move onto building out the user session.

## Defining the user session

To define our user session, we must import the following:

```rust
// nanoservices/user-session-cache/cache-module/src/user_session.rs
use redis_module::{ Context, RedisString, RedisError, RedisResult, RedisValue };
use chrono::{DateTime, Utc, NaiveDateTime};
```

With this, our user session struct has the following definition:

```rust
// nanoservices/user-session-cache/cache-module/src/user_session.rs
pub struct UserSession {
    pub user_id: String,
    pub key: String,
    pub session_datetime: DateTime<Utc>,
}
```

Our user session then has the functions associated below:

```rust
// nanoservices/user-session-cache/cache-module/src/user_session.rs
impl UserSession {
    pub fn from_id(user_id: String) -> UserSession { . . . }
    pub fn check_timeout(&mut self, ctx: &Context) -> RedisResult { . . . }
    pub fn update_last_interacted(&self, ctx: &Context) -> RedisResult { . . . }
    pub fn get_counter(&self, ctx: &Context) -> RedisResult { . . . }
}
```

With these functions, we will be able to utilize our user session in our processes. We must be able to provide construct a key from our user ID, so we can start with the from_id function below:

```rust
// nanoservices/user-session-cache/cache-module/src/user_session.rs
pub fn from_id(user_id: String) -> UserSession {
    UserSession {
        user_id: user_id.clone(),
        key: format!("user_session_{}", user_id),
        session_datetime: Utc::now(),
    }
}
```

Here we can see that we attach a "user_session_" prefix to the ID for the key This means that we can have different inserts for different things associated for our user. For instance, after reading this chapter, you could build an items cache by prefixing "user_items_" to the user ID for the key, and our items will live in peace next to the auth session data without clashing.

We now move into our check_timeout function. This is going to be the most complex function for our session struct because the function gets the data of the session, calculates if the time is elapsed, and updates the counter if the time has not elapsed. To do this we initially get the RedisKeyWritable from the Redis context with the following code:

```rust
// nanoservices/user-session-cache/cache-module/src/user_session.rs
let key_string = RedisString::create(None, self.key.clone());
let key = ctx.open_key_writable(&key_string);
```

The Redis context essentially enables us to interact with the Redis engine. For our cache, we are using the context to write, get, delete, and update data in the key value store. Now that we have our key, we get the "last_interacted" field of the user authentication session and pass the string retrieved from the "last_interacted" field into a datetime with the code below:

```rust
// nanoservices/user-session-cache/cache-module/src/user_session.rs
let last_interacted_string = match key.hash_get("last_interacted")? {
    Some(v) => {
        match NaiveDateTime::parse_from_str(
            &v.to_string(),
            "%Y-%m-%d %H:%M:%S"
        ) {
            Ok(v) => v,
            Err(e) => {
                println!("Could not parse date: {:?}", e);
                return Err(RedisError::Str("Could not parse date"))
            }
        }
    },
    None => return Err( RedisError::Str("Last interacted field does not exist") )
};
```

We can see that if we cannot find the field or fail to parse the datetime, we return appropriate errors.

Why are we parsing strings?

There is nothing stopping us from just serializing and deserializing our entire session struct in and out of bytes using bincode. This would make our functions a lot shorter. For instance, we could set and get our struct with the code below:

```rust
let serialized = bincode::serialize(value).unwrap();
ctx.call("SET", &[key, &serialized])
let result = ctx.call("GET", &[key])?;
```

However, this means that we would be storing raw bytes that are native to Rust in our Redis cache. What if you wanted to build a server in another language that wanted to get data from the cache? There are also a range of dashboards and database viewers that provide graphical user interfaces for the data in Redis. Here, you would want to be able to directly see the data that is in the Redis cache. If bincode serialization is enough for you, go for it. However, whilst we are learning about caching, it makes sense to learn the extra steps to make your data accessible. Storing data in databases in the form of language specific bytes is usually quick an easy, but it does not come for free. In my experience, you do not want to scar your database with restrictive data formats.

Now that we have our last interaction datetime of the auth session, we can now get the timeout minutes and calculate if the time elapsed since the last interaction with the following code:

```rust
// nanoservices/user-session-cache/cache-module/src/user_session.rs
let timeout_mins = match key.hash_get("timeout_mins")? {
    Some(v) => v.to_string().parse::<i32>().unwrap(),
    None => return Err( RedisError::Str("Timeout mins field does not exist") )
};
let time_diff = self.session_datetime
    .naive_utc()
    .signed_duration_since(last_interacted_string)
    .num_minutes();
```

If the time elapsed is larger than than the cut off, we can then delete the entry and return a message that the session has timed out with the code below:

```rust
// nanoservices/user-session-cache/cache-module/src/user_session.rs
if time_diff > timeout_mins.into() {
    match key.delete(){
        Ok(_) => {},
        Err(_) => return Err( RedisError::Str("Could not delete key") )
    };
    return Ok(RedisValue::SimpleStringStatic("TIMEOUT"));
}
```

We have now passed the timeout check, finally, we check the counter. The counter is a way of just forcing a refresh of the token. For instance, if a user loves our app, and is constantly using our app 24 hours a day for a week, it would not timeout and the user would be using the same JWT for a week. So, we get the counter, increase the counter by one, and return a refresh message by one if the counter has exceeded a cut off. If we pass the counter check, we merely return an OK message to tell the user that the auth check is all good with the following code:

```rust
// nanoservices/user-session-cache/cache-module/src/user_session.rs
let mut counter = match self.get_counter(ctx)? {
    RedisValue::Integer(v) => v,
    _ => return Err(RedisError::Str("Could not get counter"))
};
counter += 1;
key.hash_set("counter", ctx.create_string(counter.to_string()));
if counter > 20 {
    return Ok(RedisValue::SimpleStringStatic("REFRESH"));
}
Ok(RedisValue::SimpleStringStatic("OK"))
```

Our most complex function is now done. We now must move onto our update_last_interacted where we update the "last_interacted" field. This is a good opportunity to try and building the function yourself. If attempted to write the function yourself, hopefully it looks like the code below:

```rust
// nanoservices/user-session-cache/cache-module/src/user_session.rs
pub fn update_last_interacted(&self, ctx: &Context) -> RedisResult {
    let key_string = RedisString::create(None, self.key.clone());
    let key = ctx.open_key_writable(&key_string);
    let formatted_date_string = self.session_datetime.format(
        "%Y-%m-%d %H:%M:%S"
    ).to_string();
    let last_interacted_string = RedisString::create(
        None,
        formatted_date_string
    );
    key.hash_set("last_interacted", ctx.create_string(
        last_interacted_string
    ));
    Ok(RedisValue::SimpleStringStatic("OK"))
}
```

This might seem a little confusing, as we are not calling a datetime now. Here we must think about the bigger context. We pass an ID to the Redis server when we are making a request. When the Redis server accepts the user ID, it must construct the user session from the id with the from_id function. Therefore, in our processes, we can construct the session, make some checks or operations, and then finally call our update_last_interacted function.

Finally, we must build our get_counter function. You can attempt to code this function yourself. If you do, hopefully it looks like the following code:

```rust
// nanoservices/user-session-cache/cache-module/src/user_session.rs
pub fn get_counter(&self, ctx: &Context) -> RedisResult {
    let key_string = RedisString::create(None, self.key.clone());
    let key = ctx.open_key_writable(&key_string);
    match key.hash_get("counter")? {
        Some(v) => {
            let v = v.to_string().parse::<i64>().unwrap();
            Ok(RedisValue::Integer(v))
        },
        None => Err(RedisError::Str( "Counter field does not exist" ) )
    }
}
```

And our session struct is now complete. We can now move onto building our processes, starting with our login.

## Building the login process

For our login process, we must have the Redis context and arguments passed in via the Redis command and return an OK message if everything is good. Considering the steps, the login process has the following outline:

```rust
// nanoservices/user-session-cache/cache-module/src/ // processes/login.rs
use redis_module::{ Context, NextArg, RedisError, RedisResult, RedisString, RedisValue };
use crate::user_session::UserSession;
pub fn login(ctx: &Context, args: Vec<RedisString>) -> RedisResult {
    . . .
    Ok(RedisValue::SimpleStringStatic("OK"))
}
```

At the start of the login function, we process our arguments passed in with the code below:

```rust
// nanoservices/user-session-cache/cache-module/src/ // processes/login.rs
if args.len() < 4 {
    return Err(RedisError::WrongArity);
}
let mut args = args.into_iter().skip(1);
let user_id = args.next_arg()?.to_string();
let timeout_mins = args.next_arg()?;
let perm_user_id = args.next_arg()?.to_string();
```

If we have less than four commands, we do not have enough. We skip the first argument because the first argument will be the login command. We then get the user ID, and the timeout minutes. With our arguments, we can then construct our session, key, and write the last interacted to the key value store with the following code:

```rust
// nanoservices/user-session-cache/cache-module/src/ // processes/login.rs
let user_session = UserSession::from_id(user_id);
user_session.update_last_interacted(ctx)?;
let key_string = RedisString::create(None, user_session.key);
```

Finally, we write the timeout minutes, permanent user ID, and counter to the key value store with the code below:

```rust
// nanoservices/user-session-cache/cache-module/src/ // processes/login.rs
let key = ctx.open_key_writable(&key_string);
key.hash_set("timeout_mins", ctx.create_string(timeout_mins));
key.hash_set("counter", ctx.create_string("0"));
key.hash_set("perm_user_id", ctx.create_string(perm_user_id));
```

and with this, our login process is done. We can now move onto our logout process.

## Building the logout process

For our logout process, we have the following outline:

```rust
// nanoservices/user-session-cache/cache-module/src/ // processes/logout.rs
use redis_module::{ Context, NextArg, RedisError, RedisResult, RedisString, RedisValue };
use crate::user_session::UserSession;
pub fn logout(ctx: &Context, args: Vec<RedisString>) -> RedisResult {
    . . .
    Ok(RedisValue::SimpleStringStatic("OK"))
}
```

Inside our logout function, we process the arguments with the code below:

```rust
// nanoservices/user-session-cache/cache-module/src/ // processes/logout.rs
if args.len() < 2 {
    return Err(RedisError::WrongArity);
}
let mut args = args.into_iter().skip(1);
let user_id = args.next_arg()?.to_string();
```

We then construct the key and session with the following code:

```rust
// nanoservices/user-session-cache/cache-module/src/ // processes/logout.rs
let user_session = UserSession::from_id(user_id);
let key_string = RedisString::create(None, user_session.key);
let key = ctx.open_key_writable(&key_string);
```

And finally, we delete the session from the key value store with the code below:

```rust
// nanoservices/user-session-cache/cache-module/src/ // processes/logout.rs
if key.is_empty() {
    return Ok(RedisValue::SimpleStringStatic("NOT_FOUND"));
}
match key.delete() {
    Ok(_) => {},
    Err(_) => return Err(RedisError::Str("Could not delete key"))
};
```

With our logout now defined, we can wrap up our cache module with the update command.

## Building the update process

For our update process, we have the following outline:

```rust
// nanoservices/user-session-cache/cache-module/src/ // processes/update.rs
use redis_module::{ Context, NextArg, RedisError, RedisResult, RedisString, RedisValue };
use crate::user_session::UserSession;
pub fn update(ctx: &Context, args: Vec<RedisString>) -> RedisResult {
    . . .
}
```

Inside our update function, we process the arguments with the code below:

```rust
// nanoservices/user-session-cache/cache-module/src/ // processes/update.rs
if args.len() < 2 {
    return Err(RedisError::WrongArity);
}
let mut args = args.into_iter().skip(1);
let user_id = args.next_arg()?.to_string();
```

We then define the use session with the following code:

```rust
// nanoservices/user-session-cache/cache-module/src/ // processes/update.rs
let mut user_session = UserSession::from_id(user_id);
let key_string = RedisString::create(None, user_session.key.clone());
let key = ctx.open_key_writable(&key_string);
if key.is_empty() {
    return Ok(RedisValue::SimpleStringStatic("NOT_FOUND"));
}
```

And finally, we check the timeout and handle the outcome with the code below:

```rust
// nanoservices/user-session-cache/cache-module/src/ // processes/update.rs
match &user_session.check_timeout(ctx)? {
    RedisValue::SimpleStringStatic("TIMEOUT") => {
        return Ok(RedisValue::SimpleStringStatic("TIMEOUT"));
    },
    RedisValue::SimpleStringStatic("REFRESH") => {
        user_session.update_last_interacted(ctx)?;
        return Ok(RedisValue::SimpleStringStatic("REFRESH"));
    },
    RedisValue::SimpleStringStatic("OK") => {
        user_session.update_last_interacted(ctx)?;
        let perm_user_id = match key.hash_get("perm_user_id")? {
            Some(perm_user_id) => perm_user_id,
            None => {
                return Err(RedisError::Str( "Could not get perm_user_id" ) );
            }
        };
        return Ok(RedisValue::SimpleString(perm_user_id.to_string()));
    },
    _ => {
        return Err(RedisError::Str("Could not check timeout"));
    }
};
```

We can see that we still update the last interacted with even if a refresh is returned. This is because we want to decouple the refresh mechanism from the timeout mechanism. If a developer wants to keep servicing JWT tokens when they need to be refreshed, then they can do so. If they do not want the session to ever timeout, they can set the timeout time for a year or so. With the update now completed, we can now say that our caching module is complete, and we can move onto building out our client.

## Building our Redis client

We do not know what the future will hold for our system. When developing web systems, are requirements will change as the problem evolves. Therefore, we have no way of knowing that servers will need to access the cache. Therefore, it makes sense to build a client that is accessible to any Rust server that needs it. For our client, we only need the make a connection to Redis in an async manner and return appropriate errors if needed. With these requirements in mind, the Cargo.toml file for our cache client takes the following form:

```toml
// nanoservices/user-session-cache/cache-client/Cargo.toml
[package]
name = "cache-client"
version = "0.1.0"
edition = "2021"

[dependencies]
redis = { version = "0.27.5", features = ["tokio-comp"] }
tokio = { version = "1.41.1", features = ["full"] }
glue = { path = "../../../glue"}
```

Before we build any of our functions for our cache processes, we must build a connection and string handle function as we will use these functions for our functions for the processes. For our lib.rs file that houses all our functions, we need the following imports:

```rust
// nanoservices/user-session-cache/cache-client/src/lib.rs
use std::error::Error;
use redis::aio::{ConnectionLike, MultiplexedConnection};
use redis::Value;
use glue::errors::{NanoServiceError, NanoServiceErrorStatus};
```

The definition of our get connection function takes the following form:

```rust
// nanoservices/user-session-cache/cache-client/src/lib.rs
async fn get_connnection(address: &str) -> Result<MultiplexedConnection, NanoServiceError> {
    let client = redis::Client::open(address).map_err(|e|{ NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?;
    let con = client.get_multiplexed_async_connection() .await .map_err(|e|{ NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?;
    Ok(con)
}
```

We can then handle our strings from the response of the Redis cache with the code below:

```rust
// nanoservices/user-session-cache/cache-client/src/lib.rs
fn unpack_result_string(result: Value) -> Result<String, NanoServiceError> {
    match result {
        Value::Status(s) => Ok(s),
        _ => Err(NanoServiceError::new( "Error converting the result into a string".to_string(), NanoServiceErrorStatus::Unknown ))
    }
}
```

With this, we are now ready to build our login function.

## Building the login/logout client

Our login function takes the following signature:

```rust
// nanoservices/user-session-cache/cache-client/src/lib.rs
pub async fn login( address: &str, user_id: &str, timeout_mins: usize, perm_user_id: i32 ) -> Result<(), NanoServiceError> {
    . . .
    Ok(())
}
```

Inside our login function, we get the connection and send the request with the code below:

```rust
// nanoservices/user-session-cache/cache-client/src/lib.rs
let mut con = get_connnection(address).await?;
let result = con .req_packed_command( &redis::cmd("login.set") .arg(user_id) .arg(timeout_mins) .arg(perm_user_id.to_string()) .clone(), ) .await.map_err(|e|{ NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?;
let result_string = unpack_result_string(result)?;
match result_string.as_str() {
    "OK" => Ok(()),
    _ => Err(NanoServiceError::new( format!("{:?}", result), NanoServiceErrorStatus::Unknown ))
}
```

We then match the result, and return an error if we do not get a value::Okay. With this, we can now login on our Redis cache. For our logout function, the approach is the same as the login function with the following code:

```rust
// nanoservices/user-session-cache/cache-client/src/lib.rs
pub async fn logout(address: &str, user_id: &str) -> Result<String, NanoServiceError> {
    let mut con = get_connnection(address).await?;
    let result = con .req_packed_command( &redis::cmd("logout.set") .arg(user_id) .clone(), ) .await.map_err(|e|{ NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?;
    let result_string = unpack_result_string(result)?;
    Ok(result_string)
}
```

Now we only have the update client to build.

## Building the update client

Our update function takes the following signature:

```rust
// nanoservices/user-session-cache/cache-client/src/lib.rs
#[derive(Debug)]
pub enum UserSessionStatus { Ok(i32), Refresh }
pub async fn update(address: &str, user_id: &str) -> Result<UserSessionStatus, NanoServiceError> {
    let mut con = get_connnection(address).await?;
    . . .
}
```

The command is defined with the code below:

```rust
// nanoservices/user-session-cache/cache-client/src/lib.rs
let result = con .req_packed_command( &redis::cmd("update.set") .arg(user_id) .clone(), ) .await.map_err(|e|{ NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?;
```

With the result, we can inspect the string, returning a status depending on the response string with the code below:

```rust
// nanoservices/user-session-cache/cache-client/src/lib.rs
let result_string = unpack_result_string(result)?;
match result_string.as_str() {
    "TIMEOUT" => {
        return Err(NanoServiceError::new( "Session has timed out".to_string(), NanoServiceErrorStatus::Unauthorized ));
    },
    "NOT_FOUND" => {
        return Err(NanoServiceError::new( "Session not found".to_string(), NanoServiceErrorStatus::Unauthorized ));
    },
    "REFRESH" => {
        return Ok(UserSessionStatus::Refresh)
    },
    _ => {}
}
```

Finally, we try and parse the response to an integer and return the user ID with the following code:

```rust
// nanoservices/user-session-cache/cache-client/src/lib.rs
let perm_user_id = match result.parse::<i32>() {
    Ok(perm_user_id) => perm_user_id,
    Err(_) => {
        return Err(NanoServiceError::new( "Error converting the result into a string".to_string(), NanoServiceErrorStatus::Unknown ));
    }
};
Ok(UserSessionStatus::Ok(perm_user_id))
```

And with this our Redis client is now ready. Finally, we can use our Redis cache by using our client in our servers.

## Connecting our cache

We currently have the raw functions that interact with our cache. However, we are going to utilize a procedure of verifying a user request throughout our system on any server. I have chosen to put our interface for checking the user session for the request in the auth kernel with the following file structure:

```text
└── nanoservices
    ├── auth
    │   ├── kernel
    │   │   ├── Cargo.toml
    │   │   └── src
    │   │   ├── api
    │   │   │   ├── . . .
    │   │   ├── lib.rs
    │   │   └── user_session
    │   │   ├── descriptors.rs
    │   │   ├── mod.rs
    │   │   ├── schema.rs
    │   │   └── transactions
    │   │   ├── get.rs
    │   │   └── mod.rs
```

The user session depends on the user database model and logging in the user for a user session. Therefore, it makes sense to put the interface for the user session cache in the auth server. It can be argued that the cache interface could be put in the data access layer for the auth server. At this point it is getting down to personal choice. I stuck with the kernel because the kernel is the workspace that other servers compile if they want to interact with the auth server. Putting it in the kernel also means that the other server does not have to compile auth database code that might not be meant for public consumption. Whatever choice we go for, we can see that our layout for the user session has schemas, transactions, and descriptors. This is an approach that we keep consistent with our IO interactions. Therefore, we could easily lift this code into another workspace if we needed to. Also, a developer is not going to get confused when they come across the cache module. With this, structure we can move onto building our cache kernel.

## Building our cache Kernel

For our Cargo.toml file, we add the following dependency:

```toml
# nanoservices/auth/kernel/Cargo.toml
[dependencies]
. . .
cache-client = { path = "../../user-session-cache/cache-client" }
```

Because we are now so used to this pattern, we can rifle through our initial setup. You might even be able to configure this yourself. If you have tried to configure the user session module, you should have the following:

Your lib.rs file:

```rust
// nanoservices/auth/kernel/src/lib.rs
pub mod api;
#[cfg(any(feature = "auth-core", feature = "reqwest"))]
pub mod user_session;
```

The user_session module is public under the features auth-core and reqwest because we are going to rely on getting the user from the database. Getting a user from the database requires either the auth-core or reqwest feature. The mod.rs file of the user session module:

```rust
// nanoservices/auth/kernel/src/user_session/mod.rs
pub mod transactions;
pub mod descriptors;
pub mod schema;
```

The descriptors.rs file:

```rust
// nanoservices/auth/kernel/src/user_session/descriptors.rs
pub struct RedisSessionDescriptor;
```

The schema.rs file:

```rust
// nanoservices/auth/kernel/src/user_session/schema.rs
pub struct UserSession {
    pub user_id: i32
}
```

The mod.rs file of the transaction module:

```rust
// nanoservices/auth/kernel/src/user_session/transactions/mod.rs
pub mod get;
```

And finally, the setup code for our get trait takes the following form:

```rust
// nanoservices/auth/kernel/src/user_session/transactions/get.rs
use std::future::Future;
use crate::user_session::schema::UserSession;
use glue::errors::{NanoServiceError, NanoServiceErrorStatus};
use cache_client::{update, UserSessionStatus, login};
use crate::api::users::get::get_user_by_unique_id;
use crate::user_session::descriptors::RedisSessionDescriptor;
pub trait GetUserSession {
    fn get_user_session(unique_id: String) -> impl Future<Output = Result<UserSession, NanoServiceError>>;
}
```

For our Redis descriptor we implement the GetUserSession trait with the code below:

```rust
// nanoservices/auth/kernel/src/user_session/transactions/get.rs
impl GetUserSession for RedisSessionDescriptor {
    fn get_user_session(unique_id: String) -> impl Future<Output = Result<UserSession, NanoServiceError>> {
        get_session_redis(unique_id)
    }
}
```

The function in our implementation takes the following signature:

```rust
// nanoservices/auth/kernel/src/user_session/transactions/get.rs
pub async fn get_session_redis(unique_id: String) -> Result<UserSession, NanoServiceError> {
    . . .
}
```

The get_session_redis function is going to be fired for every authorized request that we process. First, we get the URL of the Redis server and call the cache update function, with the following code:

```rust
// nanoservices/auth/kernel/src/user_session/transactions/get.rs
let address = std::env::var("CACHE_API_URL").map_err(|e|{ NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::BadRequest ) })?;
let user_id = update(&address, &unique_id).await?;
```

We then unpack the result. If the result if OK, we return the UserSession with the user ID. However, if a refresh is needed, we merely get the user by the unique ID from the auth server and call the login function for the cache to reset the session. This approach essentially bypasses the refresh mechanism ensuring that the JWT never expires. But the user session can still timeout due to inactivity. We bypassed the JWT refreshing to just avoid chapter bloat. If you wanted to enforce a refresh mechanism, you must carry out the following steps:

Create endpoint in the auth server to receive a JWT, and update the unique ID associated with the user in the database, and return the new unique ID.

Call this refresh endpoint if a refresh is returned from the cache.

Process the rest of the request

Return the result of the response with the refreshed token in the header of the request

Update the frontend HTTP requests to inspect the headers of responses for the refresh token, updating the local storage with the token if the token is present in the header.

For this book, the handling of the return from the | cache function is carried out by the code below:

```rust
// nanoservices/auth/kernel/src/user_session/transactions/get.rs
match user_id {
    UserSessionStatus::Ok(id) => Ok(UserSession { user_id: id }),
    UserSessionStatus::Refresh => {
        let user = get_user_by_unique_id( unique_id.clone() ).await?;
        let _ = login(&address, &unique_id, 20, user.id).await?;
        let user_id = update(&address, &unique_id).await?;
        match user_id {
            UserSessionStatus::Ok(id) => Ok(UserSession { user_id: id }),
            _ => Err(NanoServiceError::new( "Failed to update user session".to_string(), NanoServiceErrorStatus::Unknown) )
        }
    }
}
```

Our kernel is now complete. We can now move onto calling the kernel in our server.

## Calling the Kernel from our to-do server

For our server, we must pass in the GetUserSession trait and call it within the view with the following code:

```rust
// nanoservices/to_do/networking/actix_server/ // src/api/basic_actions/create.rs
. . .
use auth_kernel::user_session::transactions::get::GetUserSession;
pub async fn create<T, X>( token: HeaderToken, body: Json<NewToDoItem> ) -> Result<HttpResponse, NanoServiceError> where T: SaveOne + GetAll, X: GetUserSession {
    let session = X::get_user_session( token.unique_id ).await?;
    let _ = create_core::<T>( body.into_inner(), session.user_id ).await?;
    Ok(HttpResponse::Created().json( get_all_core::<T>(session.user_id).await? ))
}
```

It should not be a surprise that all the other actions in the API for the to-do server follow suit. The delete view is redefined with the code below:

```rust
// nanoservices/to_do/networking/actix_server/ // src/api/basic_actions/delete.rs
. . .
use auth_kernel::user_session::transactions::get::GetUserSession;
pub async fn delete_by_name<T, X>( token: HeaderToken, req: HttpRequest ) -> Result<HttpResponse, NanoServiceError> where T: DeleteOne + GetAll, X: GetUserSession {
    let session = X::get_user_session(token.unique_id).await?;
    . . .
    Ok(HttpResponse::Ok().json( get_all_core::<T>( session.user_id ).await? ))
}
```

And the get endpoint should look like the following:

```rust
// nanoservices/to_do/networking/actix_server/ // src/api/basic_actions/get.rs
. . .
use auth_kernel::user_session::transactions::get::GetUserSession;
pub async fn get_all<T, X>(token: HeaderToken) -> Result<HttpResponse, NanoServiceError> where T: GetAll, X: GetUserSession {
    let session = X::get_user_session(token.unique_id).await?;
    Ok(HttpResponse::Ok().json( get_all_core::<T>(session.user_id).await?) )
}
```

And finally, the update endpoint is redefined by the code below:

```rust
// nanoservices/to_do/networking/actix_server/ // src/api/basic_actions/update.rs
. . .
use auth_kernel::user_session::transactions::get::GetUserSession;
pub async fn update<T, X>( token: HeaderToken, body: Json<ToDoItem> ) -> Result<HttpResponse, NanoServiceError> where T: UpdateOne + GetAll, X: GetUserSession {
    let session = X::get_user_session(token.unique_id).await?;
    let _ = update_core::<T>( body.into_inner(), session.user_id ).await?;
    Ok(HttpResponse::Ok().json( get_all_core::<T>(session.user_id).await? ))
}
```

With all our endpoints updated, we pass the Redis descriptor into the views that are defined in the views factory with the following code:

```rust
// nanoservices/to_do/networking/actix_server/ // src/api/basic_actions/mod.rs
. . .
use auth_kernel::user_session::descriptors::RedisSessionDescriptor;
pub fn basic_actions_factory(app: &mut ServiceConfig) {
    app.service( scope("/api/v1") .route("get/all", get().to( get::get_all::< SqlxPostGresDescriptor, RedisSessionDescriptor >) ) .route("create", post().to( create::create::< SqlxPostGresDescriptor, RedisSessionDescriptor >) ) .route("delete/{name}", delete().to( delete::delete_by_name::< SqlxPostGresDescriptor, RedisSessionDescriptor >) ) .route("update", put().to( update::update::< SqlxPostGresDescriptor, RedisSessionDescriptor >) ) );
}
```

This is now done. We can sit back and appreciate what is going on here. Although there are a lot of minor changes in several files, but we have introduced a cache that checks the user session. Because of the way our code is structured, our changes easily slot in. We do not have to rip out and change huge chunks of code. Also, our complexity is contained. We can just look at the networking workspace and see how the HTTP request is handled throughout the life cycle of a request because the core logic is all abstracted away. If you wanted to use a file, other database, or just memory of the server for the cache, you can just implement another descriptor and slot in the descriptor easily. If we wanted to do this, we could do this with features as seen with the code below:

```rust
#[cfg(feature = "cache-postgres")] use auth_kernel::user_session:: descriptors::PostgresSessionDescriptor as CacheDescriptor; #[cfg(feature = "cache-redis")] use auth_kernel::user_session:: descriptors::RedisSessionDescriptor as CacheDescriptor; . . . get::get_all::< SqlxPostGresDescriptor, CacheDescriptor >
```

We are nearly finished connecting our user session cache to our system. All we need now is to call our auth kernel from our auth server as we need to create the user session in the cache when logging the user in.

## Calling the Kernel from our auth server

To call our kernel we now must add the core feature because our cache interface relies on the get user in the core. This means that our auth Cargo.toml must be updated with the following:

```toml
# nanoservices/auth/networking/actix_server/Cargo.toml
. . .
[dependencies]
. . .
auth-kernel = { path = "../../kernel", features = ["auth-core"], default-features = false }
```

Now if we had a less structured approach where we combine the HTTP code with the core code in one app, there would be a circular dependency. This circular dependency would arise from the auth kernel pulling from the auth server, and then the auth server requiring the auth kernel for the Redis interface. However, because we have kept our system isolated and concerned with only the scope they are built for. This has led to the layout shown in Figure 12.1.

Figure 12.1 – Our auth dependency graph

This is where the value of our structure is strongly demonstrated. As you build a more complex system, you do not know what will arise. However, our auth server has a Redis cache, and Postgres database to handle the auth management. Adding connecting other servers is as simple as connecting to the kernel. If we wanted to bring the cache check down to the core and no longer expose the cache to other services, it would be as simple as changing a few lines of code per API endpoint to achieve this.

Now that we have our layout, we can build our login transaction for our kernel. First we import the following:

```rust
// nanoservices/auth/kernel/src/user_session/transactions/login.rs
use std::future::Future;
use glue::errors::NanoServiceError;
use crate::user_session::descriptors::RedisSessionDescriptor;
use cache_client::login as cache_login;
```

We then define our trait for logging in a user session with the code below:

```rust
// nanoservices/auth/kernel/src/user_session/transactions/login.rs
pub trait LoginUserSession {
    fn login_user_session( address: &str, user_id: &str, timeout_mins: usize, perm_user_id: i32 ) -> impl Future<Output = Result<(), NanoServiceError>>;
}
```

Finally, we implement this trait for the Redis descriptor that just calls the login function from the Redis interface with the following code:

```rust
// nanoservices/auth/kernel/src/user_session/transactions/login.rs
impl LoginUserSession for RedisSessionDescriptor {
    fn login_user_session( address: &str, user_id: &str, timeout_mins: usize, perm_user_id: i32 ) -> impl Future<Output = Result<(), NanoServiceError>> {
        cache_login( address, user_id, timeout_mins, perm_user_id )
    }
}
```

Of course, we ensure that this trait is available to the rest of the kernel with the following code:

```rust
// nanoservices/auth/kernel/src/user_session/transactions/mod.rs
pub mod get;
pub mod login;
```

We then configure our login view to interact with the cache. First, we import the LoginUserSession trait with the following code:

```rust
// nanoservices/auth/networking/actix_server/src/ // api/auth/login.rs
. . .
use auth_kernel::user_session::transactions::login:: LoginUserSession;
```

Our login function then takes the signature below:

```rust
// nanoservices/auth/networking/actix_server/src/ // api/auth/login.rs
pub async fn login<T, X>(req: actix_web::HttpRequest) -> Result<HttpResponse, NanoServiceError> where T: GetByEmail, X: LoginUserSession {
    . . .
    Ok(HttpResponse::Ok().json(token))
}
```

Inside our login function, we get the credentials, token, and user with the following code:

```rust
// nanoservices/auth/networking/actix_server/src/ // api/auth/login.rs
let credentials = extract_credentials(req).await?;
let token = core_login::<T>( credentials.email.clone(), credentials.password ).await?;
let user = T::get_by_email(credentials.email).await?;
```

We then get our Redis URL and login the user session with the code below:

```rust
// nanoservices/auth/networking/actix_server/src/ // api/auth/login.rs
let url = std::env::var("CACHE_API_URL").map_err(|e|{ NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?;
let _ = X::login_user_session( &url, &user.unique_id, 20, user.id ).await?;
```

Our system is now ready with a cache. All we must do now is add the Redis URL to the .env file giving us the following contents:

```bash
# nanoservices/to_do/.env
TO_DO_DB_URL=postgres://username:password@localhost/to_do
AUTH_DB_URL=postgres://username:password@localhost/to_do
AUTH_API_URL=http://127.0.0.1:8081
JWT_SECRET=secret
CACHE_API_URL=redis://127.0.0.1:6379
```

We can then run our nanoservices/to_do/scripts/test_server_com.sh file and our create test will work. It must be noted that your development pace is probably speeding up due to you relying on the test_server_com.sh script to test the new feature. This is because you do not have to perform any manual steps to setup the databases, run the migrations, and then login. This is the power of testing and test-driven development. My editors tell me that the testing chapters of any book are the least read. Trust me, the testing chapters are some of the most important chapters to read once you can get a basic web application up and running. The first benefit is the speed of development. One of the biggest reasons why I can work full time for a cutting-edge database company, push forward the application of surgical robotics in one of the biggest bioengineering departments in the world, write books, run a medical simulation software company that the German government uses with a friend, and have a family life with my wife and kids at the same time is mainly down to good unit and end to end testing techniques. Running an isolated test that has automated all the steps needed to test the code you are building, saves you running those manual steps again and again throughout the day of coding. It also helps you pick up bugs so you're not firefighting later, and when you refactor, you just run the tests every time you make a small change because they're so quick and easy to run. You then know the instant you introduce some breaking code that it is breaking code because the effort threshold of running all the tests is so low, you do it frequently. So, no routing through the codebase to work out what change created the break. If you have a good unit and end-to-end testing strategy, then you will develop better quality code at a much faster rate than the average developer because the average developer just hasn't taken the time to refine their testing technique. Instead, they are performing multiple manual steps to test one edge case of their code. It frankly blows my mind that everyone agrees that manual steps for deployment and packaging are bad, and that we should automate them. Yet, these same developers will push back on unit testing, meaning they're ok with manual steps for checking their code once they've coded it.

## Summary

In this chapter, we essentially created a Redis module so we have a custom cache, and then we implement that cache into our system so multiple servers can interact with the cache. It must be noted that our cache enabled several checks and updates in just one call to the Redis cache as opposed to multiple different requests to the cache. This is a very powerful skill that you have just developed that will come in handy in multiple different situations. We also got a feel for how useful tests are when developing our code with our test script. At this stage you are technically familiar with Rust and the web ecosystem to build your own applications. However, I must stress that you will be much more productive once you have completed the unit testing and end-to-end testing chapters.

In the next chapter, we will explore observability and how to trace our async tasks and HTTP requests.

## Questions

At a high level, how did we build a Redis module in Rust?

How did we get the Docker build to house our module?

How did we integrate our cache into the HTTP layer of our servers?

What are the advantages of our approach to integrating the cache?

Let us say that we built an email server, and we want to check the auth session before sending a particular email. At a high level how could this be done?

## Answers

We used the Redis crate and macros to define the Redis module. We then compiled our Redis module as a dynamic C library, and then got our Redis server to load the module on startup. We could use our module via the commands we specified in the module when making a request to the Redis server.

We copied over the code of our Redis module into a Rust base image. We then build the Redis module. We then move over to the second stage of the build which is the Redis image, and then copy over our compiled module to the Redis image but nothing else. We then run the Redis server with our module loaded.

We created traits for the interaction with the cache. We then implemented a Redis handle for these traits, and then mounted those traits to the server views.

The main advantage is that we can implement other handles for different storage mechanisms. This means that we can easily unit test our code with test implementations of the cache. It also makes it easy to move the implementation of our cache interface around our system with a clean interface.

The email server must define the auth kernel as a dependency, and then use the get user session function from the kernel to check the user session before sending the email, and that is it.
