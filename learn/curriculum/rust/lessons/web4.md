# Chapter 5: Handling HTTP Requests

After the detour of understanding async on a deeper level, we are now going back to our project that we were working on in chapter two. So far, we have structured our to-do module in a flexible, scalable, and re-usable manner. However, this can only get us so far in terms of web programming// File: to_do/core/src/structs.rs

```rust
impl AllToDoItems {
    pub fn from_hashmap(all_items: HashMap<String, ToDoItem>) -> AllToDoItems {
        let mut pending = Vec::new();
        let mut done = Vec::new();
        for (_, item) in all_items {
            match item.status {
                TaskStatus::PENDING => pending.push(item),
                TaskStatus::DONE => done.push(item)
            }
        }
        AllToDoItems {
            pending,
            done
        }
    }
}
```

To reach users, we need to serve our to-do module to reach multiple people quickly without the user having to install Rust on their own computers. We can do this with a web framework. In this chapter, we will build on the core logic of our to-do items and connect this core logic to a server. By the end of this chapter, you will be able to build a server that has a data access layer, core layer, and networking layer. You will also get some exposure in handling error across all cargo workspaces, and refactoring some of our code as our requirements for our server get more defined over time.

We will start this journey by building a simple server that accepts incoming HTTP requests.

## Technical requirements

We will also be building on the server code we created in the chapter two which can be found at [PUT LINK HERE]

You can find the full source code that will be used in this chapter here:

[PUT LINK HERE]

## Launching a basic web server

In this section we are merely going to get an Actix server running before connecting our server to our core code.

Why Actix Web?

This is now the third edition of the book. There have been several people email me asking about a range of different web frameworks. The truth is the choice of framework is close to trivial. If you structure your code well, you should be able to switch these frameworks with minimal effort. Throughout this book, we will structure our code so we can multiple different web frameworks at the same time.

For our server, we are going to create a networking layer for our to-do nanoservice and put a cargo project for our actix server in that networking layer directory. With our server module, our file layout for our nanoservice should take the following form:

```text
├── Cargo.toml
└── to_do
    ├── core
    │   ├── . . .
    ├── dal
    │   ├── . . .
    └── networking
        └── actix_server
            ├── Cargo.toml
            └── src
                └── main.rs
```

With this new layout, our Cargo.toml at the root of our project should have the following workspaces defined:

```toml
# File: ./Cargo.toml
[workspace]
resolver = "2"
members = [
    "to_do/core",
    "to_do/dal",
    "to_do/networking/actix_server"
]
```

We are nearly finished boilerplate code. Before we write any server code, we need to define the server's dependencies in the server Cargo.toml file with the following:

```toml
# File: ./to_do/networking/actix_server/Cargo.toml
[package]
name = "actix_server"
version = "0.1.0"
edition = "2021"

[dependencies]
tokio = { version = "1.36.0", features = ["full"] }
actix-web = "4.5.1"
```

And we can now write our server main.rs file. First, we need to use the following structs and traits with the code below:

```rust
//! File: ./to_do/networking/actix_server/src/main.rs
use actix_web::{web, App, HttpServer, Responder, HttpRequest};
```

We will see how these structs and traits are used as and when we use them. Recall from the previous chapter, servers typically handle incoming requests by passing these requests as async tasks into the async runtime to be handled. It should not be a surprise that our server API endpoint is an async task defined by the following code:

```rust
// File: ./to_do/networking/actix_server/src/main.rs
async fn greet(req: HttpRequest) -> impl Responder {
    let name = req.match_info().get("name").unwrap_or("World");
    format!("Hello {}!", name)
}
```

With the preceding code, we can see that our API endpoint receives the HTTP request and returns anything that has implemented the Responder trait. We then extract the name from the endpoint of the URL or return a "World" if the name is not in the URL endpoint. Our view then returns a string. To see what we can automatically return as a response, we can check the Responder trait in the Actix web docs and scroll down to the Implementations on Foreign Types section as seen in figure 4.1.

Figure 4.1 – Implementations of Foreign types

Here in figure 4.1, we can see that we can essentially return strings and bytes. Now that we have our API endpoint, we can build our server with the code below:

```rust
// File: ./to_do/networking/actix_server/src/main.rs
#[tokio::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .route("/", web::get().to(greet))
            .route("/{name}", web::get().to(greet))
            .route("/say/hello", web::get().to(|| async { "Hello Again!" }))
    })
    .workers(4)
    .bind("127.0.0.1:8080")?
    .run()
    .await
}
```

In the preceding code, we can see that we are running our HttpServer after constructing it using the HttpServer::new function. Knowing what we know now, we can see that we have passed in a closure that returns and the App struct. We can see that we have used several different ways to define a view. We defined a view by building a function or an async closure. We assign this greet function that we have created to our application server as the route view with the `.route("/", web::get().to(greet))` command. We can also see that we can pass in the name from the URL to our greet function with the `.route("/{name}", web::get().to(greet))` command. Finally, we pass in a closure into the final route. With our configuration, if we run the following command:

```bash
cargo run -p actix_server
```

we will get the following printout:

```text
Finished dev [unoptimized + debuginfo] target(s) in 0.21s
Running `target/debug/actix_server`
```

We can see in the preceding output that right now there is no logging. This is expected and we will configure logging later. Now that our server is running, we should expect the following URL inputs and outputs on the browser:

`http://127.0.0.1:8080/` Hello World! `http://127.0.0.1:8080/maxwell` Hello maxwell! `http://127.0.0.1:8080/say/hello` Hello Again!

And here we have it, we have a server running! But our server is isolated. In the next section, we are going to connect our server to the core module. However, before we

## Connecting the core to the server

When it comes to connecting our networking layer to our core layer, to avoid confusion, we map our api modules on both the core and networking layers in the same way. To map both layers, we must add the following new files for the core:

```text
└── to_do
    └── core
        └── src
            ├── api
            │   ├── basic_actions
            │   │   ├── create.rs
            │   │   ├── delete.rs
            │   │   ├── get.rs
            │   │   ├── mod.rs
            │   │   └── update.rs
            │   └── mod.rs
            ├── lib.rs
            └── . . .
```

We can see that we are adding a file for each basic action that we expect to perform on a to-do task. It also must be noted that we added a src/lib.rs file. We can delete the src/main.rs file if we want to as our core module, we now be used by other cargo workspaces such as the networking layer. The networking layer will interact with the core module through the src/lib.rs file. The module file in the basic actions module now takes the following form:

```rust
//! File: to_do/core/src/api/basic_actions/mod.rs
pub mod create;
pub mod get;
pub mod delete;
pub mod update;
```

We are keeping the files public as we want the src/lib.rs file to expose them to another workspace that uses our core module with the code below:

```rust
//! File: to_do/core/src/lib.rs
pub mod api;
pub mod structs;
pub mod enums;
```

Our core module is now ready to be used in other cargo workspaces. For our server accommodate our core module, we need to add the following files:

```text
└── to_do
    └── networking
        └── actix_server
            ├── . . .
            └── src
                ├── api
                │   ├── basic_actions
                │   │   ├── create.rs
                │   │   ├── delete.rs
                │   │   ├── get.rs
                │   │   ├── mod.rs
                │   │   └── update.rs
                │   └── mod.rs
                └── main.rs
```

Here we are keeping our main.rs file because this cargo workspace is not going to be used elsewhere yet. However, we are going to be using the main.rs file to run the server. It should not be a surprise that the module file in the basic actions takes the following form:

```rust
//! File: to_do/networking/actix_server/src/api/basic_actions/mod.rs
pub mod create;
pub mod get;
pub mod delete;
pub mod update;
```

In the api module file we expose the basic actions with the code below:

```rust
//! File: to_do/networking/actix_server/src/api/mod.rs
pub mod basic_actions;
```

And declare the api module in our main.rs file with the code below:

```rust
//! File: to_do/networking/actix_server/src/main.rs
...
mod api;
...
```

And all our code is stitched up and ready to talk to each other. The only thing left to do is declare our core module in the Cargo.toml file with the following code:

```rust
//! File: to_do/networking/actix_server/Cargo.toml
...
[dependencies]
tokio = { version = "1.36.0", features = ["full"] }
actix-web = "4.5.1"
core = { path = "../../core" }
```

We can now run a cargo build command. If the build passes, we know that everything is going to place nice with each other. This is one of the many reasons why I love Rust. The compiler makes it hard for beginners to do basic things. However, when you start growing the complexity of your system, when the system compiles, you know that we are not going to get basic bugs in production.

Wait isn't this over-engineered?

It is reasonable to have some alarm bells go off at what we have just done. Our nanoservice now consists of three cargo workspaces, and all we have so far is a read and write to a basic JSON file, an isolated create function that talks to the JSON file, and an isolated server that just returns a couple of strings. We have produced a lot of files and directories just for this functionality.

The term "over-engineered" is vague, and as a result gets banded about. In my experience, a lot of people do not look at the bigger picture when accusing an approach to be over-engineered. As we stick with this approach, you will get to experience the flexibly that we have when running our application locally, or on a server. The ease of swapping out layers so our nanoservice will be able to run as a microservice in its own Docker container, or just compile into another cargo workspace will prevent over-engineering in the future, as you will not have to run multiple Docker containers to develop against the entire system. And as systems get big with multiple developers and teams, trust me, you will thank your past self that you took this approach.

Now that everything is compiling, we can move onto serving our to-do items, but we might need to refactor our to-do items first.

## Refactoring our to-do items

If we want to serve our to-do items, need to get our server to talk to our core, which will then get all the to-do items from our JSON file. This is where we do see some over-engineering which is our to-do item structs. Right now, just to represent an either pending or done item, we need to navigate between three structs. Our TaskStatus enum handles the logic or serialization of the status of these structs. Right now, is a good time to look at the structs in our core module and have a quick think on how you could represent them in a single vector to be displayed in the browser. Do not spend too much time on this as I warn you, working with the structs we have now is a fruitless task.

If you did try and work out how to convert the to-do items into a single vector, you may have considered wrapping the items in an enum, and then implementing a serialization trait to display these to-do items. This is like how we handle the writing of the to-do item to a file. However, this is a good time to listen to those alarm bells. We are having to do a lot of work to handle a variance of a status title. This complexity does not give us any advantages so we need to act now to prevent this over engineered approach from getting more embedded into our system as it will be harder to rip out the longer, we leave it. As David Farley says, the outcome of a surgical procedure is not better or safer because the surgeon uses a blunter knife. We do not have a safe system because we do not touch or change chunks of our code. In-fact it's the opposite. A sign of a safe well-designed system that is handled by skilled engineers is the ability to confidently change chunks of the system as we find out more, and still manage to keep the system stable. Right now, let us retreat to a safer, simpler position so the handling of our to-do items is easier to manage. We can do this by completely ripping out the to_do/core/src/structs/ directory and replacing it with a to_do/core/src/structs.rs file. Inside this file, we can define a to-do item with the following code:

```rust
//! File: to_do/core/src/structs.rs
use std::fmt;
use serde::{Serialize, Deserialize};
use crate::enums::TaskStatus;
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ToDoItem {
    pub title: String,
    pub status: TaskStatus
}
```

We can directly implement the serialization traits because our TaskStatus enum has already implemented the serialization traits. We also want our ToDoItem struct to print out in the same way our enum wrapping our two different previous structs did so by implementing the Display trait with the code below:

```rust
// File: to_do/core/src/structs.rs
impl fmt::Display for ToDoItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.status {
            TaskStatus::PENDING => write!(
                f,
                "Pending: {}",
                self.title
            ),
            TaskStatus::DONE => write!(
                f,
                "Done: {}",
                self.title
            ),
        }
    }
}
```

We now only need to refactor our create api function with following code:

```rust
//! File: to_do/core/src/api/basic_actions/create.rs
use crate::enums::TaskStatus;
use crate::structs::ToDoItem;
use dal::json_file::save_one;

pub fn create(title: &str, status: TaskStatus) -> Result<ToDoItem, String> {
    let item = ToDoItem {
        title: title.to_string(),
        status
    };
    let _ = save_one(&title.to_string(), &item)?;
    Ok(item)
}
```

We can see that the create function is simpler than our previous implementation. If you did not delete the src/main.rs file, we can clear our tasks.json file and run the commands below:

```bash
cargo run -- washing pending
cargo run -- coding done
```

We can then check our tasks.json JSON file which should have the following content:

```json
{
    "coding": {
        "title": "coding",
        "status": "DONE"
    },
    "washing": {
        "title": "washing",
        "status": "PENDING"
    }
}
```

Here, we can see that we still can access the task by the title as the key, but value has the title and status of the task. We did not change any of the code in the data access layer. Instead, we are seeing how our to-do item serializes. We are now ready to return all our to-do items to the browser.

## Serving our to-do items

We are now at the stage of serving all our items to the browser. However, before we touch any of the server code, we need to add another struct from our core module. We need a container that houses two lists of items for the pending and done items which takes the following form:

```rust
// File: to_do/core/src/structs.rs
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct AllToDoItems {
    pub pending: Vec<ToDoItem>,
    pub done: Vec<ToDoItem>
}
```

You might recall that we load the data from the JSON file in hashmap form, meaning that we are going to need a from_hashmap function for our AllToDoItems struct with the code below:

```rust
// File: to_do/core/src/structs.rs
impl AllToDOItems {
    pub fn from_hashmap(all_items: HashMap<String, ToDoItem>) -> AllToDOItems {
        let mut pending = Vec::new();
        let mut done = Vec::new();
        for (_, item) in all_items {
            match item.status {
                TaskStatus::PENDING => pending.push(item),
                TaskStatus::DONE => done.push(item)
            }
        }
        AllToDOItems {
            pending,
            done
        }
    }
}
```

We can now utilize our new container struct by building our get_all api function with the following code:

```rust
// File: to_do/core/src/api/basic_actions/get.rs
use dal::json_file::get_all as get_all_handle;
use crate::structs::{ ToDoItem, AllToDoItems };

pub fn get_all() -> Result<AllToDoItems, String> {
    Ok(AllToDoItems::from_hashmap(
        get_all_handle::<ToDoItem>()?
    ))
}
```

Wow, there is not much code there. However, we can see what type of data is being loaded as the value of the hashmap with the `get_all_handle::<ToDoItem>()`. We exploit the ? operator to reduce the need for a match statement, and then directly feed the data into the from_hashmap function.

Here, our core API function is fusing the logic of the core module, and the data access layer. The data from our core API function is then passed to the Actix API function which can then return the data to the browser as seen in figure 4.2.

Figure 4.2 – Pathway for getting all to-do items

We can now build our Actix API function with the code below:

```rust
//! File: to_do/networking/actix_server
//! /src/api/basic_actions/get.rs
use core::api::basic_actions::get::get_all as get_all_core;
use actix_web::HttpResponse;

pub async fn get_all() -> HttpResponse {
    let all_items = match get_all_core() {
        Ok(items) => items,
        Err(e) => return HttpResponse::InternalServerError().json(e)
    };
    HttpResponse::Ok().json(all_items)
}
```

The match statement does bloat the code a little bit, but we will handle this in the next section. We can see that if there is any error, we return it with an internal server error response code. We now must connect our get_all async function to our server. We should keep our API module endpoint definitions isolated as we want to be able to version control the different API modules. We can define the get all URI with the following code:

```rust
//! File: to_do/networking/actix_server
//! /src/api/basic_actions/mod.rs
. . .
use actix_web::web::{ServiceConfig, get, scope};

pub fn basic_actions_factory(app: &mut ServiceConfig) {
    app.service(
        scope("/api/v1")
            .route("get/all", get().to(get::get_all))
    );
}
```

In the preceding code, we can see that we pass in a mutable reference of a ServiceConfig struct. This enables us to define things like views to the server in different fields. The documentation on this struct states that it is to allow bigger applications to split up configuration into different files. We then apply a service to the ServiceConfig struct. We should define all backend API endpoints with "/api/" to differentiate backend endpoints from the frontend endpoints. We also have an "/v1/" to facilitate versioning. When there are breaking changes to the API, we should keep the previous version, but put the new breaking change API in the new version. We then alert users about the update in the version and give a date to when the lower version will be no longer supported or removed.

Now that we have defined our endpoint factory for our basic actions, we need one last factory that collects all the other factories and calls them with the code below:

```rust
//! File: to_do/networking/actix_server/src/api/mod.rs
pub mod basic_actions;
use actix_web::web::ServiceConfig;

pub fn views_factory(app: &mut ServiceConfig) {
    basic_actions::basic_actions_factory(app);
}
```

With this, we only need to configure our server with our views factory with the following code:

```rust
//! File: to_do/networking/actix_server/src/main.rs
#[tokio::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new().configure(api::views_factory)
    })
    .workers(4)
    .bind("127.0.0.1:8080")?
    .run()
    .await
}
```

If we run our server and hit our endpoint in the browser, we get the result seen in figure 4.3.

Figure 4.3 – Return data from get all API endpoint

And there we have it; our server is now returning the to-do items in a sorted fashion. However, remember that we were relying on a match statement in our web API function. We can reduce the need for this by defining our own error handing so we can exploit the ? operator in our API function. We have nearly wrapped up this chapter. We just need to handle those errors for the web server API function.

## Handling errors in API endpoints

We are at the final hurdle of this chapter where we want to get rid of the match statement in our API functions for our server. To do this, we can create an error type that can construct an HTTP response, so we can exploit the ? operator in our web API function and the server will simply respond with a HTTP response. However, this means that all our cargo workspaces need to share the same custom error types. This is where a glue module will come in. A glue module is a crate that all other workspaces install. With this glue module, all cargo workspaces can seamlessly pass the same error type between each other. Before we create this glue module, we must appreciate what is happening to our whole system. Our nanoservice is just sitting there. If we put the glue module next to our nanoservice, then it can get confusing as to what is what. To reduce the risk of confusion, we need to shift our nanoservice into its own directory called nanoservices. The nanoservices directory is where we will put other nanoservices such as the authentication service. Considering that the glue module is to be used everywhere, we can put the glue module in the root, giving us the following Cargo.toml file in our root:

```toml
[workspace]
resolver = "2"
members = [
    "glue",
    "nanoservices/to_do/core",
    "nanoservices/to_do/dal",
    "nanoservices/to_do/networking/actix_server"
]
```

And our glue module should have the file layout below:

```text
└── glue
    ├── Cargo.toml
    └── src
        ├── errors.rs
        └── lib.rs
```

We can now define the Cargo.toml file of the glue module with the following code:

```toml
# File: glue/Cargo.toml
[dependencies]
actix-web = { version = "4.5.1", optional = true }
serde = { version = "1.0.197", features = ["derive"] }
thiserror = "1.0.58"

[features]
actix = ["actix-web"]
```

Here we can see that the actix-web dependency is optional, and only used if the actix feature is enabled. This is because we only want to compile Actix web if we are directly mapping the error in the Actix server. There is no need for the core or data access layers to rely on the actix-web dependency. In the future we can add features of other web frameworks as and when we need them.

In the lib.rs file of the glue module we make the errors publicly available with the following code:

```rust
//! File: glue/src/lib.rs
pub mod errors;
```

We can now move onto our error definitions in our src/errors.rs file. First, we are going to need the following structs and traits:

```rust
//! File: glue/src/errors.rs
use serde::{Deserialize, Serialize};
use thiserror::Error;
use std::fmt;

#[cfg(feature = "actix")]
use actix_web::{ HttpResponse, error::ResponseError, http::StatusCode };
```

It must be noted that we are only using the actix-web dependency if the actix feature is enabled. We can now define the different statuses that the error can respond with using the following code:

```rust
// File: glue/src/errors.rs
#[derive(Error, Debug, Serialize, Deserialize, PartialEq)]
pub enum NanoServiceErrorStatus {
    #[error("Requested resource was not found")]
    NotFound,
    #[error("You are forbidden to access requested resource.")]
    Forbidden,
    #[error("Unknown Internal Error")]
    Unknown,
    #[error("Bad Request")]
    BadRequest,
    #[error("Conflict")]
    Conflict,
    #[error("Unauthorized")]
    Unauthorized
}
```

The `#[error("something")]` derive macro essentially implements the Display trait for our enum. If a field is decorated with `#[error("something")]`, then that field will display a "something" when the Display trait is utilized.

We can now define our nanoservice error with the code below:

```rust
// File: glue/src/errors.rs
#[derive(Serialize, Deserialize, Debug, Error)]
pub struct NanoServiceError {
    pub message: String,
    pub status: NanoServiceErrorStatus
}

impl NanoServiceError {
    pub fn new(message: String, status: NanoServiceErrorStatus) -> NanoServiceError {
        NanoServiceError {
            message,
            status
        }
    }
}
```

We now need to implement a response trait for our nanoservice error, but before we can do this, we must implement the Display trait for our nanoservice error with the following code:

```rust
// File: glue/src/errors.rs
impl fmt::Display for NanoServiceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
```

And with this, we can now implement the ResponseError trait for the nanoservice error with the code below:

```rust
// File: glue/src/errors.rs
#[cfg(feature = "actix")]
impl ResponseError for NanoServiceError {
    fn status_code(&self) -> StatusCode {
        . . .
    }
    fn error_response(&self) -> HttpResponse {
        . . .
    }
}
```

For our status code function, we need to merely map our status numb with the Actix web status code with the following code:

```rust
// File: glue/src/errors.rs
fn status_code(&self) -> StatusCode {
    match self.status {
        NanoServiceErrorStatus::NotFound => StatusCode::NOT_FOUND,
        NanoServiceErrorStatus::Forbidden => StatusCode::FORBIDDEN,
        NanoServiceErrorStatus::Unknown => StatusCode::INTERNAL_SERVER_ERROR,
        NanoServiceErrorStatus::BadRequest => StatusCode::BAD_REQUEST,
        NanoServiceErrorStatus::Conflict => StatusCode::CONFLICT,
        NanoServiceErrorStatus::Unauthorized => StatusCode::UNAUTHORIZED
    }
}
```

For our error response function, we need to construct a HTTP response from our status and message with the code below:

```rust
// File: glue/src/errors.rs
fn error_response(&self) -> HttpResponse {
    let status_code = self.status_code();
    HttpResponse::build(status_code).json(self.message.clone())
}
```

And our error can now construct a HTTP response. However, we do not want to have to write the map_err function every time we want to convert an error to a nanoservice error. We can prevent repetitive code with the following marco:

```rust
// File: glue/src/errors.rs
#[macro_export]
macro_rules! safe_eject {
    ($e:expr, $err_status:expr) => {
        $e.map_err(|x| NanoServiceError::new(
            x.to_string(),
            $err_status)
        )
    };
    ($e:expr, $err_status:expr, $message_context:expr) => {
        $e.map_err(|x| NanoServiceError::new(
            format!("{}: {}", $message_context, x.to_string()),
            $err_status
        ))
    };
}
```

Here, we can see that if we pass in an expression and the error status, we will map the error to our nanoservice error. If we also pass in a message context into the macro, we can also add the context of where the error is happening. This will save us from repeating ourselves.

We can now see how our errors are implemented. There is going to be a fair bit of refactoring, but it is better to correct all of this now than later where the refactor will require more work. Also, we will get some experience on what refactoring code feels like in our Rust system. We can start in our data access module. In the data access Cargo.toml file we add the following dependency:

```toml
# File: nanoservices/to_do/dal/Cargo.toml
[dependencies]
. . .
glue = { path = "../../../glue"}
```

We then use our error structs and macros in our json_file.rs file with the code below:

```rust
// File: nanoservices/to_do/dal/src/json_file.rs
. . .
use glue::errors::{ NanoServiceError, NanoServiceErrorStatus };
use glue::safe_eject;
```

We can now replace all our String error return types with our nanoservices error. For instance, our handle function takes the following form:

```rust
// File: nanoservices/to_do/dal/src/json_file.rs
fn get_handle() -> Result<File, NanoServiceError> {
    let file_path = env::var("JSON_STORE_PATH")
        .unwrap_or("./tasks.json".to_string());
    let file = safe_eject!(OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(&file_path), NanoServiceErrorStatus::Unknown, "Error reading JSON file" )?;
    Ok(file)
}
```

And our get all function is now defined with the code below:

```rust
// File: nanoservices/to_do/dal/src/json_file.rs
pub fn get_all<T: DeserializeOwned>() -> Result<HashMap<String, T>, NanoServiceError> {
    let mut file = get_handle()?;
    let mut contents = String::new();
    safe_eject!( file.read_to_string(&mut contents), NanoServiceErrorStatus::Unknown, "Error reading JSON file to get all tasks" )?;
    let tasks: HashMap<String, T> = safe_eject!( serde_json::from_str(&contents), NanoServiceErrorStatus::Unknown, "Error parsing JSON file" )?;
    Ok(tasks)
}
```

We can see how our safe_eject! macro lifts out the repetitive code and we just focus on defining the expression to be evaluated, the error status, and the optional context message of where the error is happening that will be presented alongside the error message. Our safe_eject! macro is mainly used for third party results, as we aim for all our functions to return the nanoservice error. We could go through all examples, but this would needlessly bloat the chapter. Luckily the Rust compiler will warn you when an error type does not match as we are using the ? operator throughout the codebase. In your core layer you will also need to add the glue package when converting all results to return a nanoservice error when erroring.

Once the core is filled out with our nanoservice errors, we can move onto our Actix server. Unlike the other two layers we will have the actix feature for our glue package with the following Cargo.toml file code:

```toml
# File: nanoservices/to_do/networking/actix_server/Cargo.toml
glue = { path = "../../../../glue", features = ["actix"] }
```

With our glue package our create function file for our API endpoint takes the form below:

```rust
//! File: nanoservices/to_do/networking/actix_server/
//! src/api/basic_actions/get.rs
use core::api::basic_actions::get::get_all as get_all_core;
use actix_web::HttpResponse;
use glue::errors::NanoServiceError;

pub async fn get_all() -> Result<HttpResponse, NanoServiceError> {
    Ok(HttpResponse::Ok().json(get_all_core()?))
}
```

And this is it! I appreciate that there has been a fair amount of reworking with the juggling of multiple workspaces. However, the hard tedious parts are now done. Adding new API endpoints from now on is going to be simple. Slotting in new nanoservices and layers will also be effortless. And with this in mind, we are ready to move onto our next chapter of processing HTTP requests.

## Summary

In this chapter we built a functioning nanoservice that has a core layer, data access layer, and a networking layer. While getting there took a bit of refactoring, we resulted in a well-structured server where all the layers compile together to be served by the networking layer. To aid in the error handling we also built a glue module that was used by all layers, enabling us to propagate the same error type through all the layers in the nanoservice. At this stage, slotting in other web framework or data storage approach is as simple as creating another cargo workspace in the networking layer, or another feature in the data access layer. With this chained factory mechanism, we can slot entire view modules in and out of the configuration when the server is being built.

In the next chapter, we will work with processing requests and responses. We will learn how to pass params, bodies, headers, and forms to views and process them returning JSON. We will be using these new methods with the to-do module we built in the previous chapter to enable our interaction with to-do items to achieve through server views.

## Questions

1. What parameter is passed into the HttpServer::new function and what does the parameter return?
2. How can we work out what data types an API function can return automatically?
3. How can we enable data handling across all cargo workspaces including the HTTP response?
4. Why is it a good idea to have /api/v1 in the endpoint of the URL?
5. How can we support multiple dependencies but be selective on what dependencies we compile depending on our needs such as networking dependencies?
6. For our networking API endpoints, we have view factories that are chained into the main API factory. What is the advantage of chaining factories?

## Answers

1. A closure is passed into the function. It has to return the App struct so the bind and run functions can be acted on them after the HttpServer::new function has fired.
2. We can look at the documentation for the Responder trait to see what data types have implemented the Responder trait.
3. We create a separate cargo workspace where we define an error struct. All other workspaces can then compile this workspace as a dependency, and return the error struct as the error type. For the HTTP response, we need to implement the ResponseError for our error.
4. The api part of the URL enables us to differentiate between frontend and backend HTTP requests. The v1 enables us to version control the API endpoint and enables us to support multiple versions.
5. We can use features. For example, we can define a dependency as optional and state that this dependency is used if we use a feature that we tether to that dependency. We can then optionally compile code of the `#[cfg(feature = "some-feature")]` code.
6. Chaining factories gives us flexibility on how individual modules are constructed and how they are orchestrated. The factory inside the module focuses on how the module is constructed, and the factory outside the module focuses on how the different modules are orchestrated. This isolation enables us to swap out modules as all the logic is maintained in the module. Chaining factories also enable us to keep bloat to a minimum. For instance, our main.rs file only focuses on the running the server, and is not overrun by mapping URL endpoints.
