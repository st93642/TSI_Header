# Chapter 6: Processing HTTP Requests

Up to this point, we have utilized the Actix web framework to serve basic views. However, this can only get us so far when it comes to extracting data from the request and passing data back to the user. In this chapter, we will fuse code from Chapter 3, Designing Your Web Application in Rust, and Chapter 5, Handling HTTP Requests, to build server views that process to-do items. We will then explore JSON serialization for extracting data and returning it to make our views more user friendly. We also extract data from the header with middleware before it hits the view. We will explore the concepts around data serialization and extracting data from requests by building out the create, edit, and delete to-do items endpoints for our to-do application.

In this chapter, we will cover the following topics:

- Passing parameters via the URL
- Passing data via the POST body
- Deleting resources using the DELETE method
- Updating resources using the PUT method
- Extracting data from HTTP request headers

Once you have finished this chapter you will be able to build a basic Rust server that can send and receive data in the URL, body using JSON, and the header of the HTTP request. This is essentially a fully functioning API Rust server without a proper database for data storage, authentication of users, or displaying of content in the browser. However, these concepts are covered in the next three chapters. You are on the home run for having a fully working Rust server up and running. Let's get started!

## Technical requirements

We will also be building on the server code we created in the Chapter 3, Designing Your Web Application in Rust which can be found at

You can find the full source code that will be used in this chapter here:

You will also be making requests to your server using CURL. You can download CURL directly through your OS package manager.

## Passing parameters via the URL

Passing parameters via the URL is the easiest way to pass data to the server. We can explore this concept by passing the name of a to-do item and returning that single to-do item to the client.

Before we touch the networking layer, we must build the core function that loads the data and filters for the to-do item by name which can be done with the following code:

```rust
//! File: nanoservices/to_do/core/src/api/basic_actions/get.rs
. . .
use glue::errors::{ NanoServiceError, NanoServiceErrorStatus };
. . .
pub fn get_by_name(name: &str) -> Result<ToDoItem, NanoServiceError> {
    Ok(get_all_handle::<ToDoItem>()?
        .remove(name)
        .ok_or(
            NanoServiceError::new(
                format!("Item with name {} not found", name),
                NanoServiceErrorStatus::NotFound
            )
        )?
    )
}
```

Note that our filter at this stage is not optional, as we are loading all the data and then extract the item that we want and then return it. We will optimize our filter in chapter eight when we move over to a proper database. For now, our filter works for us so we can continue to develop our features.

We can also see that we use the remove function to extract the item. This means that we do not have to clone the item that we are extracting, and we do not worry that the item has been removed from the HashMap because we are not going to write that HashMap to the file.

We now need to connect our core get_by_name function to our HTTP server. Before we wrap the core get_by_name function in a function that handles the HTTP request, we need to redefine our imports with the code below:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! src/api/basic_actions/get.rs
use core::api::basic_actions::get::{
    get_all as get_all_core,
    get_by_name as get_by_name_core
};
use actix_web::{ HttpResponse, HttpRequest };
use glue::errors::{ NanoServiceError, NanoServiceErrorStatus };
```

We can see that we are using the status and the HTTP request. Here, our incoming HTTP request needs to be accepted, and the name parameter must be extracted from the URL.

We then need to handle this extraction. We can do this with the following code:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! src/api/basic_actions/get.rs
pub async fn get_by_name(req: HttpRequest) -> Result<HttpResponse, NanoServiceError> {
    let name = match req.match_info().get("name") {
        Some(name) => name,
        None => {
            return Err(
                NanoServiceError::new(
                    "Name not provided".to_string(),
                    NanoServiceErrorStatus::BadRequest
                )
            )
        }
    };
    Ok(HttpResponse::Ok().json(get_by_name_core(name)?))
}
```

Here, we can see that if the name parameter is not found in the URL, we return a bad request. We then just return the data from the core get_by_name function, exploiting the ? operator, as a not found HTTP response will be automatically returned to the client if the item is not found.

We have nearly finished our get API endpoint. All we must do now is connect our API endpoint to our factory. We can do this by adding the following line:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! src/api/basic_actions/mod.rs
. . .
.route("get/{name}", get().to(get::get_by_name))
. . .
```

Now, if we have that to-do item in our JSON file and we run our server and then visit our individual get URL in the browser, we will get a response like the following:

Figure 5.1 – Our response from our individual GET endpoint

If we pass a to-do item name that we know is not in the JSON file, we will get a not found response as seen in figure 5.2.

Figure 5.2 – Our not found response from our individual GET endpoint

And here we have it, our get individual to-do item is now fully working.

Remember the flexibility!

It is reasonable to wonder why we have two different layers to just get the simple filter that returns one item. Remember, our core functionality is separated from the networking layer. Therefore, we can swap out the web framework easily if we need. Also, more importantly, we can compile this nanoservice into another program or server by just targeting the core. It is important to keep this flexibility in mind to keep the discipline of maintaining these layers.

Even though we can get our to-do items, we cannot yet do much with them. We must start altering the state of our to-do items. Our next step is building a create to-do item via post and JSON data.

## Passing data via POST body

While passing parameters via the URL is simple, there are a few problems with the URL approach. URLs can be cached by the browser and the user might end up selecting an autocompleted URL with data in the URL. This is good if we are using the URL to pass parameters to get data like visiting your profile on a social media application, however, we do not want to accidentally select a cached URL that alters data in the application.

Passing data in the URL is also limited to simple data types. For instance, if we wanted to pass a list of HashMaps to the server, it would be hard to pass such a data struct through the URL without doing some other form of serialization. This is where POST requests and JSON bodies come in. We can pass JSON data via the HTTP request body.

We have already built our create function, but we need to refactor this create function so that we can integrate the create function into our web server. Our create function refactor results in the code below:

```rust
//! File: nanoservices/to_do/core/src/api/basic_actions/create.rs
use crate::structs::ToDoItem;
use dal::json_file::save_one;
use glue::errors::NanoServiceError;

pub fn create(item: ToDoItem) -> Result<ToDoItem, NanoServiceError> {
    let _ = save_one(&item.title.to_string(), &item)?;
    Ok(item)
}
```

We remember that our ToDoItem struct has already implemented the serializing traits to the read and written to the JSON file. Here we have turned our create function into an async function. Right now we are not using any async functionality, but in the future we will be using async to connect to databases and make HTTP requests inside our API endpoints. Considering this, it makes sense just to make API functions in the core module async. If you kept the main.rs file in the core workspace, you now must delete it, or convert the main function in the main.rs file into async.

We can now wrap the core create function in our server. First, we need to use the following:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! /src/api/basic_actions/create.rs
use core::api::basic_actions::{
    create::create as create_core,
    get::get_all as get_all_core
};
use core::structs::ToDoItem;
use glue::errors::NanoServiceError;
use actix_web::{ HttpResponse, web::Json };
```

With these structs and traits, we can define our create API web endpoint with the code below:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! /src/api/basic_actions/create.rs
pub async fn create(body: Json<ToDoItem>) -> Result<HttpResponse, NanoServiceError> {
    let _ = create_core(body.into_inner())?;
    Ok(HttpResponse::Ok().json(get_all_core()?))
}
```

Wow that is compact! But there is a lot going on here. The Json struct implements traits that extract data from the body of the HTTP request before the create function is called. If the HTTP request body can be serialized into a ToDoItem struct, then this is done, and we have a ToDoItem struct passed into the create function from the HTTP request body. If an ToDoItem struct cannot be constructed from the JSON body of the HTTP request, then a bad request response is returned to the client with the serialization error message.

The `Json<ToDoItem>` works because we have implemented the Deserialize trait for the ToDoItem struct. We then insert our item into the JSON file we are currently using as storage with the create_core function. Finally, we get all the data from the JSON file with the get_all_core function and return the items in a JSON body. Again, this is not optimal, but it will work for now.

Can we pass a JSON body in a GET instead of a POST request?

Yes, you can. Personally, I find these method definitions trivial, and I like to put all the data that I am passing to the server in a JSON body because it is easier to handle as the requirements and app evolves. I also find it easier to handle the serialization into a struct. However, this is just my opinion, and while most HTTP clients will allow you to put JSON bodies in a GET request, at the time of writing this book, Axios, a JavaScript HTTP client, will not allow JSON bodies in GET requests.

We now need to plug our create API function into our web server. We start by importing the post function with the following code:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! /src/api/basic_actions/mod.rs
. . .
use actix_web::web::{ServiceConfig, get, scope, post};
. . .
```

And we then we define our view in our factory with the code below:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! /src/api/basic_actions/mod.rs
. . .
.route("create", post().to(create::create))
. . .
```

We can now run our server with our new create endpoint. While our server is running, we can test our endpoint with the following CURL command in our terminal:

```bash
curl -X POST http://127.0.0.1:8080/api/v1/create \
  -H "Content-Type: application/json" \
  -d '{"title": "writing", "status": "PENDING"}'
```

After the CURL command has been run, we then get the following response:

```json
{
  "pending":[
    {"title":"writing","status":"PENDING"},
    {"title":"washing","status":"PENDING"}
  ],
  "done":[
    {"title":"coding","status":"DONE"}
  ]
}
```

If we inspect our JSON file that we are using for storage, we should have the contents below:

```json
{
  "coding": {
    "title": "coding",
    "status": "DONE"
  },
  "writing": {
    "title": "writing",
    "status": "PENDING"
  },
  "washing": {
    "title": "washing",
    "status": "PENDING"
  }
}
```

We can also test our serialization of the JSON body with the following command:

```bash
curl -X POST http://127.0.0.1:8080/api/v1/create \
  -H "Content-Type: application/json" \
  -d '{"title": "writing", "status": "test"}'
```

The above command has a status that should not be accepted. When we run this command, we should get the following response:

```text
Json deserialize error: unknown variant `test`, expected `DONE` or `PENDING` at line 1 column 37
```

With this response, we can see that we are not going to be passing data that is going to corrupt the data store, and get a helpful response on why we are rejecting the incorrect data.

And there we have it, we can see that our POST create HTTP endpoint is now working. So, we can now get our items and add new ones. However, what if we create an item by accident and need to delete it? This is where DELETE methods come in.

## Deleting resources using the DELETE method

DELETE methods are like GET methods. We could technically pass data in the JSON body of the HTTP request, there is enough data in the URL that we can perform a delete. In our case, the title of the item is enough to delete the item from storage.

Before we define the core and server functions, we must ensure that our data access layer function returns the right message if we do not find the right item when deleting from our store. Some slight refactoring is needed where we return the right error if there was no item found in the store with the following code:

```rust
//! File: nanoservices/to_do/dal/src/json_file.rs
pub fn delete_one<T>(id: &str) -> Result<(), NanoServiceError>
where
    T: Serialize + DeserializeOwned + Clone + std::fmt::Debug,
{
    let mut tasks = get_all::<T>().unwrap_or(HashMap::new());
    match tasks.remove(id) {
        Some(_) => {
            save_all(&tasks)?;
            Ok(())
        },
        None => Err(
            NanoServiceError::new(
                format!("Task with title {} not found", id),
                NanoServiceErrorStatus::NotFound
            )
        )
    }
}
```

We can now define our core delete function with the code below:

```rust
//! File: nanoservices/to_do/core/src/basic_actions/delete.rs
use dal::json_file::delete_one;
use crate::structs::ToDoItem;
use glue::errors::NanoServiceError;

pub fn delete(id: &str) -> Result<(), NanoServiceError> {
    delete_one::<ToDoItem>(id)
}
```

We can then wrap the core delete function into our networking layer. The approach should run along the same lines as the get individual item API endpoint, as we are extracting the name parameter out of the URL. Now would be a good time to try and build the delete endpoint function for the server yourself.

If you attempted to build the delete endpoint function yourself, hopefully you will have used the following structs and traits:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! /src/api/basic_actions/delete.rs
use core::api::basic_actions::{
    delete::delete as delete_core,
    get::get_all as get_all_core
};
use actix_web::{ HttpResponse, HttpRequest };
use glue::errors::{ NanoServiceError, NanoServiceErrorStatus };
```

We then define the delete endpoint with the following code:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! /src/api/basic_actions/delete.rs
. . .
pub async fn delete_by_name(req: HttpRequest) -> Result<HttpResponse, NanoServiceError> {
    match req.match_info().get("name") {
        Some(name) => {
            delete_core(name)?;
        },
        None => {
            return Err(
                NanoServiceError::new(
                    "Name not provided".to_string(),
                    NanoServiceErrorStatus::BadRequest
                )
            )
        }
    };
    Ok(HttpResponse::Ok().json(get_all_core()?))
}
```

We now need to plug our delete_by_name API function into our web server. To do this, firstly we need to import the delete function with the following code:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! /src/api/basic_actions/mod.rs
. . .
use actix_web::web::{ServiceConfig, get, scope, post, delete};
. . .
```

And we then we define our view in our factory with the code below:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! /src/api/basic_actions/mod.rs
. . .
.route("delete/{name}", delete().to(delete::delete_by_name))
```

If we then run our server, we can make a delete request with the following command:

```bash
curl -X DELETE http://127.0.0.1:8080/api/v1/delete/writing
```

However, we get the following error when making the request:

```text
"Error parsing JSON file: trailing characters at line 10 column 4"
```

Thanks to all the effort that we put into our error handling; we know that there is something wrong with the parsing of the JSON file. Our safe_eject! macro has a string that has the context added to the error. So, if we go to our nanoservices/to_do/dal/src/json_file.rs file and search for "Error parsing JSON file", we will see that the error is occurring when we try and serialize the data loaded from the JSON file. If we inspect our JSON file, we should have something like the following:

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
"washing": {
  "title": "washing",
  "status": "PENDING"
}
```

Here we can see that there is some malformed JSON, implying that the file is not being cleared or truncated before writing the JSON data to the file. To fix this, we need to revert the JSON file back to the previous state, and create a writer handle that truncates the file, ensuring that the file is empty before we write to the file with the code below:

```rust
//! File: nanoservices/to_do/dal/src/json_file.rs
fn get_write_handle() -> Result<File, NanoServiceError> {
    let file_path = env::var("JSON_STORE_PATH")
        .unwrap_or("./tasks.json".to_string());
    let file = safe_eject!(OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true) // ensures file is empty
        .open(&file_path), NanoServiceErrorStatus::Unknown, "Error reading JSON file (write handle)" )?;
    Ok(file)
}
```

We can then swap our file handle in the save_all function with the following code:

```rust
//! File: nanoservices/to_do/dal/src/json_file.rs
pub fn save_all<T: Serialize>(tasks: &HashMap<String, T>) -> Result<(), NanoServiceError> {
    let mut file = get_write_handle()?;
    . . .
    Ok(())
}
```

We then rerun our delete CURL command and we should get the following printout in the terminal:

```text
"Error parsing JSON file: trailing characters at line 10 column 4"
```

Which will give us the printout below:

```json
{
  "pending":[
    {"title":"washing","status":"PENDING"}
  ],
  "done":[
    {"title":"coding","status":"DONE"}
  ]
}
```

This shows that we have successfully deleted our to-do item!

In this section we got to see how useful the context of the error was as our error bubbles up to the HTTP response, so while our system is not panicking, we also do not get a stack trace because the server has not crashed.

Deleting the writing to-do item is all well, but we also need to be able to edit the status of the to-do item. In our next section we will achieve this with PUT methods.

## Updating resources using the PUT method

PUT methods are essentially the same as a POST method. We want to put the data in the JSON body. However, the PUT method is typically used for updating resources that have already been created. This is the last endpoint that we create in this chapter. Here, we must accept a to-do item in the JSON body, get the item, and update it. This is a good point to try and build the API endpoint yourself.

If you tried to build your own API endpoint, hopefully you started with the core function. First, we import the following:

```rust
//! File: nanoservices/to_do/core/src/basic_actions/update.rs
use dal::json_file::{ get_all as get_all_handle, save_all };
use crate::structs::ToDoItem;
use glue::errors::{ NanoServiceError, NanoServiceErrorStatus };
```

And with these imports we define the update function with the code below:

```rust
//! File: nanoservices/to_do/core/src/basic_actions/update.rs
//! File: nanoservices/to_do/core/src/basic_actions/update.rs
use dal::json_file::{ get_all as get_all_handle, save_all };
use crate::structs::ToDoItem;
use glue::errors::{ NanoServiceError, NanoServiceErrorStatus };

pub fn update(item: ToDoItem) -> Result<(), NanoServiceError> {
    let mut all_items = get_all_handle::<ToDoItem>()?;
    if !all_items.contains_key(&item.title) {
        return Err(NanoServiceError::new(
            format!("Item with name {} not found", item.title),
            NanoServiceErrorStatus::NotFound
        ));
    }
    all_items.insert(item.title.clone(), item.clone());
    save_all(&all_items)?;
    Ok(())
}
```

Here, we can see that we get all the items, and check to see if the item we are trying to update is in the state. If it's not, we return an error. If it is, we then update the item with the new status and save it.

Everything is ready here to move onto defining the API function for the server, which takes the following form:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! /src/api/basic_actions/update.rs
use core::api::basic_actions::{
    update::update as update_core,
    get::get_all as get_all_core
};
use core::structs::ToDoItem;
use glue::errors::NanoServiceError;
use actix_web::{ HttpResponse, web::Json };

pub async fn update(body: Json<ToDoItem>) -> Result<HttpResponse, NanoServiceError> {
    let _ = update_core(body.into_inner())?;
    Ok(HttpResponse::Ok().json(get_all_core()?))
}
```

The preceding code should not be a surprise at this point. We are merely extracting the correct data from the HTTP request, calling a core function, and returning a HTTP response.

Finally, we import the put method with the code below:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! /src/api/basic_actions/mod.rs
. . .
use actix_web::web::{ServiceConfig, get, scope, post, delete, put};
. . .
```

And define the API endpoint view in the API factory with the following code:

```rust
//! File: nanoservices/to_do/networking/actix_server
//! /src/api/basic_actions/mod.rs
. . .
.route("update", put().to(update::update))
. . .
```

We can then run our server, and perform the CURL terminal command below:

```bash
curl -X POST http://127.0.0.1:8080/api/v1/update \
  -H "Content-Type: application/json" \
  -d '{"title": "washing", "status": "DONE"}'
```

Which gives the following printout:

```json
{
  "pending":[
  ],
  "done":[
    {"title":"coding","status":"DONE"},
    {"title":"washing","status":"DONE"}
  ]
}
```

This printout shows that our update API endpoint works. And this is it, we have all our endpoints to handle to-do items. But what about extracting data from headers? While we do not need to extract anything from HTTP request headers to handle our to-do items, we will need to extract data from headers for things such as authentication, and extracting data from headers does come under the concept of processing HTTP requests.

## Extracting data from HTTP request headers

In this section we will not implement full authentication, as this will be addressed in chapter 10, managing user sessions. Here we are just going to extract a string from the header.

When it comes to headers, we can store credentials, or metadata around the HTTP request. Before we get into inspecting headers of our HTTP requests, we must define our token in our glue workspace because we are going to use the token for authentication in the future. Every service should have the right to enforce authentication without having to rewrite the token implementation.

We can start our token definition with the following imports:

```rust
//! File: glue/src/token.rs
#[cfg(feature = "actix")]
use actix_web::{ dev::Payload, FromRequest, HttpRequest, };
#[cfg(feature = "actix")]
use futures::future::{Ready, ok, err};
#[cfg(feature = "actix")]
use crate::errors::{ NanoServiceError, NanoServiceErrorStatus };
```

We can see that all our imports are reliant on the actix feature being enabled. This is because the token is merely a string, and all the imports are needed to enable the header extraction of that string for an actix web server.

We can now define our token with the code below:

```rust
//! File: glue/src/token.rs
pub struct HeaderToken {
    pub message: String
}
```

As we can see, this struct merely houses a String which is going to be the token extracted from the header.

Now we move onto the building of the middleware that extracts the token with the following code:

```rust
//! File: glue/src/token.rs
#[cfg(feature = "actix")]
impl FromRequest for HeaderToken {
    type Error = NanoServiceError;
    type Future = Ready<Result<HeaderToken, NanoServiceError>>;
    fn from_request(req: &HttpRequest, _: &mut Payload) -> Self::Future {
        . . .
    }
}
```

Here, we return a result that can either be our nanoservice error, or a header token. The Ready that wraps this result essentially implements the Future trait where the poll function instantly returns a Ready state with the result, as we can see with the code below:

```rust
//! package: futures::future
impl<T> Future for Ready<T> {
    type Output = T;
    #[inline]
    fn poll(mut self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<T> {
        Poll::Ready(self.0.take().expect(
            "Ready polled after completion")
        )
    }
}
```

What this means is that our from_request function returns a future that is going to be processed by our tokio runtime. However, that future is going to instantly be resolved after one poll which is why there is no waker in the poll function of the Ready struct.

Now that we understand what our from_request function does, let's focus on the code inside of the from_request function. Initially, we must extract the token from the header and return an unauthorized error if there is now token with the following code:

```rust
//! File: glue/src/token.rs
let raw_data = match req.headers().get("token") {
    Some(data) => data,
    None => {
        return err(NanoServiceError {
            status: NanoServiceErrorStatus::Unauthorized,
            message: "token not in header under key 'token'"
                .to_string()
        })
    }
};
```

Note that we return an error wrapped in err. The lowercase err and ok just wrap the error or result in a Ready struct.

Now that we have extracted the data from the header, we can then convert the data to a string and return it with the code below:

```rust
//! File: glue/src/token.rs
let message = match raw_data.to_str() {
    Ok(token) => token.to_string(),
    Err(_) => {
        return err(NanoServiceError {
            status: NanoServiceErrorStatus::Unauthorized,
            message: "token not a valid string".to_string()
        })
    }
};
return ok(HeaderToken{message})
```

And our token is now ready to extract data from the header in the middleware.

Finally, we can declare the token module in our glue crate with the following code:

```rust
//! File: glue/src/mod.rs
pub mod errors;
pub mod token;
```

We can now update our create API endpoint with the code below:

```rust
//! File: nanoservices/to_do/core/src/api/basic_actions/create.rs
. . .
use glue::{ errors::NanoServiceError, token::HeaderToken };
. . .
pub async fn create(token: HeaderToken, body: Json<ToDoItem>) -> Result<HttpResponse, NanoServiceError> {
    println!("Token: {}", token.message);
    let _ = create_core(body.into_inner())?;
    Ok(HttpResponse::Ok().json(get_all_core()?))
}
```

Here, we can see that putting the HeaderToken as a parameter in the create function will execute the FromRequest trait and pass the result into the function if the FromRequest trait passed. This is the same as our `Json<ToDoItem>`.

We can now test our token by running our server and executing the following CURL command in our terminal:

```bash
curl -X POST http://127.0.0.1:8080/api/v1/create \
  -H "Content-Type: application/json" \
  -d '{"title": "writing", "status": "PENDING"}'
```

However, this time we do not get to create the item, instead, we get the error response below:

```text
"token not in header under key 'token'"
```

Here, we can see that we are getting blocked due to not having a token in the header. We can add a token to the header with the following CURL command:

```bash
curl -X POST http://127.0.0.1:8080/api/v1/create \
  -H "Content-Type: application/json" \
  -H "token: some token" \
  -d '{"title": "writing", "status": "PENDING"}'
```

Once this command is executed, we can see the following printout in the terminal running the server:

```text
Token: some token
```

And our response from the CURL gives us the JSON below:

```json
{
  "pending":[
    {"title":"writing","status":"PENDING"}
  ],
  "done":[
    {"title":"washing","status":"DONE"},
    {"title":"coding","status":"DONE"}
  ]
}
```

And we can see that our

## Summary

In this chapter, we have put all of what we have learned in the previous chapters to good use. We fused the logic from the core, which loads and saves to-do items from a JSON file, and looked at the to-do item process logic by using the basic views from Actix-web. With this, we have been able to see how the isolated modules click together. We will keep reaping the benefits of this approach in the next few chapters as we rip out the JSON file and replace it with a database.

We also managed to utilize the serde crate to serialize complex data structures. This allows our users to get the full state update returned to them when they make an edit. We also built on our knowledge of futures, async blocks, and closures to intercept requests before they reached the view. We can see that the power of Rust enables us to do some highly customizable things to our server, without us having to dig deep into the framework.

Thus, Rust has a strong future in web development. With a few lines of code, we can build our own middleware. Our JSON serialization structs were made possible with just one line of code, and the traits provided by Actix enabled us to define the parameter in the view function, thus enabling the view to automatically extract the data from the body and serialize it into the struct. This scalable, powerful, and standardized way of passing data is more concise than many high-level languages. We can now fully interact with and inspect every part of the HTTP request.

Now that we are processing and returning well-structured data to the user, we can start displaying it in an interactive way for our user to point and click when editing, creating, and deleting to-do items.

In the next chapter, we will be serving HTML, CSS, and JavaScript from the Actix-web server. This will enable us to see and interact with to-do items via a graphical user interface, with the JavaScript making API calls to the endpoints we defined in this chapter.

## Questions

1. What is the difference between a GET and POST request?
2. Why would we have middleware when we check credentials?
3. How do you enable a custom struct to be directly returned in a view?
4. How do you enact middleware for the server?
5. How do you enable a custom struct to serialize data into the view?

## Answers

1. A GET request can be cached, and there are limits to the types and amount of data that can be sent. A POST request has a body, which enables more data to be transferred. Also, it cannot be cached.
2. We use middleware to open the header and check the credentials before sending the request to the desired view. This gives us an opportunity to prevent the body being loaded by returning an auth error before loading the view, preventing the potentially malicious body.
3. For the struct to be directly returned, we will have to implement the Responder trait. During this implementation, we will have to define the respond_to function that accepts the HTTP request struct. The respond_to will be fired when the struct is returned.
4. To enact middleware we can implement the FromRequest trait for a struct, and then pass that struct as a parameter in the function of the API endpoint
5. We decorate the struct with the `#[derive(Deserialize)]` macro. Once we have done this, we define the parameter type to be wrapped in a JSON struct: `parameter: web::Json<ToDoItem>`.
