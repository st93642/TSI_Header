# 11 Communicating Between Servers

At this point in the book, we have two servers which are the authentication server and to-do server, however, they are not talking to each other yet. In microservices, we must be able to get our servers sending messages between each other. For our system, we must get our to-do server making requests to out authentication server. This request checks that the user is valid before we perform a database transaction on to-do items. As these items are related to the user ID passed in the request requesting the database transaction. To achieve this, the chapter will cover the following:

Getting users from auth with the unique ID

Making auth accessible to other servers

Tethering users to to-do items

Testing our server-to-server communication with bash

By the end of this chapter, you will be able to get servers talking to each other either directly in memory due to one server compiling another server into it, or by HTTP requests depending on the feature compilation. You will also be able to utilize Rust traits to generalize a database interface so integrating other databases will be simple and scalable.

## Technical requirements

This chapter will be relying on the code in the previous chapter that can be found at the following link:

`https://github.com/PacktPublishing/Rust-Web-Programming-3E/tree/main/chapter11`

## Getting users from auth with unique ID

Our authentication server now supports login and get users using email. For our scoped authentication sessions to work, we must create an API where we can get the user details from the unique ID because we have the unique ID from the JWT in the authentication session.

At this point in the book, we have added an number of API endpoints to servers, so like the others, we are going to start with the data access layer. At this point you should be familiar with adding API endpoints so feel free to tackle this yourself. The following subsections of adding this endpoint will be brief.

### Adding get by unique ID to dal

At this point, we are going to build on what we have already done, meaning that we are going to define a trait, and implement that trait for our Postgres descriptor. With our plan, our trait takes the following form:

```rust
// nanoservices/auth/dal/src/users/transactions/get.rs pub trait GetByUniqueId { fn get_by_unique_id(id: String) -> impl Future<Output = Result<User, NanoServiceError>> + Send; }
```

Our trait implementation can be achieved with the code below:

```rust
// nanoservices/auth/dal/src/users/transactions/get.rs impl GetByUniqueId for SqlxPostGresDescriptor { fn get_by_unique_id(id: String) -> impl Future<Output = Result<User, NanoServiceError>> + Send { sqlx_postgres_get_by_unique_id(id) } }
```

And finally, our function that makes the database call is achieved with the following code:

```rust
// nanoservices/auth/dal/src/users/transactions/get.rs async fn sqlx_postgres_get_by_unique_id(id: String) -> Result<User, NanoServiceError> { let item = sqlx::query_as::<_, User>(" SELECT * FROM users WHERE unique_id = $1" ).bind(id) .fetch_optional(&*SQLX_POSTGRES_POOL).await.map_err(|e| { NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?; match item { None => Err(NanoServiceError::new( "User not found".to_string(), NanoServiceErrorStatus::NotFound )), Some(item) => Ok(item) } }
```

And our database access is achieved. We must ensure that the get is available with the code below:

```rust
// nanoservices/auth/dal/src/users/transactions/mod.rs pub mod get; pub mod create;
```

And with our new API on our data access, we can move onto adding our get to the core.

### Adding get by unique ID to core

Our core can now support the getting of the user using the unique ID with the following code:

```rust
// nanoservices/auth/core/src/api/users/get.rs use auth_dal::users::schema::TrimmedUser; use auth_dal::users::transactions::get::GetByUniqueId; use glue::errors::NanoServiceError; pub async fn get_by_unique_id<T: GetByUniqueId>(id: String) -> Result<TrimmedUser, NanoServiceError> { let user = T::get_by_unique_id(id).await?; let trimmed_user: TrimmedUser = user.into(); Ok(trimmed_user) }
```

And again, we must ensure that our function must be made accessible with the code below:

```rust
// nanoservices/auth/core/src/api/users/mod.rs pub mod create; pub mod get;
```

Now our endpoint is ready to be exposed to the world in our networking layer.

### Adding get by unique ID to networking

When it comes to passing a unique ID to our networking layer, we can just use our header token, our token stores the unique ID. This enables us to securely call the API endpoint from the client, or just pass the token to the auth server from the to-do server. Our auth get endpoint can be defined with the code below:

```rust
// nanoservices/auth/networking/actix_server/src/api/users/get.rs use auth_dal::users::transactions::get::GetByUniqueId; use auth_core::api::users::get::get_by_unique_id as get_by_unique_id_core; use glue::errors::NanoServiceError; use glue::token::HeaderToken; use actix_web::HttpResponse; pub async fn get_by_unique_id<T: GetByUniqueId>(token: HeaderToken) -> Result<HttpResponse, NanoServiceError> { let user = get_by_unique_id_core::<T>(token.unique_id).await?; Ok(HttpResponse::Ok().json(user)) }
```

We can then mount the endpoint to the server with our Postgres handler with the following code:

```rust
// nanoservices/auth/networking/actix_server/src/api/users/mod.rs pub mod create; pub mod get; use auth_dal::users::descriptors::SqlxPostGresDescriptor; use actix_web::web::{ServiceConfig, scope, post, get}; pub fn users_factory(app: &mut ServiceConfig) { app.service( scope("/api/v1/users") .route("create", post().to( create::create::<SqlxPostGresDescriptor>) ) .route("get", get().to( get::get_by_unique_id::<SqlxPostGresDescriptor>) ) ); }
```

And now our auth server can return a user by the unique ID either by making a HTTP request or calling the core function. We can now move onto building the kernel of the auth server to make it easier for other servers to interact with the auth server either via the core or HTTP networking layer.

## Making auth accessible to other servers

To make our auth server accessible to another server, we build a kernel. Our authentication kernel takes the following file structure:

. . . └── nanoservices ├── auth │ ├── core │ │ ├── . . . │ ├── dal │ │ ├── . . . │ ├── kernel │ │ ├── Cargo.toml │ │ └── src │ │ ├── api │ │ │ ├── mod.rs │ │ │ └── users │ │ │ ├── get.rs │ │ │ └── mod.rs │ │ └── lib.rs │ └── networking │ └── . . . . . .

Here, we can see that the API structure is the same as our core and networking to maintain consistency. The kernel should enable the user to make requests to the auth server either via HTTP or calling the core function. To enable this, we need two different features, HTTP, and core giving us the following Cargo.toml file outline:

```toml
// nanoservices/auth/kernel/Cargo.toml [package] name = "auth-kernel" version = "0.1.0" edition = "2021" [dependencies] auth-core = { path = "../core", optional = true } auth-dal = { path = "../dal" } reqwest = {version = "0.12.5", optional = true, features = ["json"]} glue = { path = "../../../glue" } [features] http = ["reqwest"] core-postgres = ["auth-core"]
```

Here, we can see that the auth-core and reqwest dependencies are optional and are utilized if a particular feature is enabled. We can handle these feature dependencies in our get API with the code below:

```rust
// nanoservices/auth/kernel/src/api/users/get.rs #[cfg(any(feature = "auth-core", feature = "reqwest"))] mod common_imports { pub use auth_dal::users::schema::TrimmedUser; pub use glue::errors::NanoServiceError; } #[cfg(feature = "auth-core")] mod core_imports { pub use auth_core::api::users::get::get_by_unique_id as get_by_unique_id_core; pub use auth_dal::users::descriptors::SqlxPostGresDescriptor; } #[cfg(feature = "reqwest")] mod reqwest_imports { pub use reqwest::Client; pub use glue::errors::NanoServiceErrorStatus; pub use glue::token::HeaderToken; } #[cfg(any(feature = "auth-core", feature = "reqwest"))] use common_imports::*; #[cfg(feature = "auth-core")] use core_imports::*; #[cfg(feature = "reqwest")] use reqwest_imports::*;
```

With these imports, we can build our interface with the following code:

```rust
// nanoservices/auth/kernel/src/api/users/get.rs #[cfg(any(feature = "auth-core", feature = "reqwest"))] pub async fn get_user_by_unique_id(id: String) -> Result<TrimmedUser, NanoServiceError> { #[cfg(feature = "auth-core")] let user: TrimmedUser = get_by_unique_id_core::< SqlxPostGresDescriptor >(id).await?.into(); #[cfg(feature = "reqwest")] let user: TrimmedUser = get_user_by_unique_id_api_call(id) .await? .into(); return Ok(user) }
```

Here, we can see if we are using the core feature, we are calling the core function for the core feature, and a not yet defined function that calls the API endpoint via HTTP is the reqwest feature is enabled. We can also see that we are utilizing the into function to convert the response of both functions into the TrimmedUser struct because the TrimmedUser have implemented the `From<User>` trait.

Now that our interface is now built, we can define the signature of the HTTP call with the code below:

```rust
// nanoservices/auth/kernel/src/api/users/get.rs #[cfg(feature = "reqwest")] async fn get_user_by_unique_id_api_call(id: String) -> Result<TrimmedUser, NanoServiceError> { . . . }
```

Inside this function, we define the URL to the auth server with the following code:

```rust
// nanoservices/auth/kernel/src/api/users/get.rs let url = std::env::var("AUTH_API_URL").map_err(|e|{ NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::BadRequest ) })?; let full_url = format!("{}/api/v1/users/get", url);
```

We can then build an encoded token, pass that into the header of a request, and send that request with the code below:

```rust
// nanoservices/auth/kernel/src/api/users/get.rs let header_token = HeaderToken { unique_id: id }.encode()?; let client = Client::new(); let response = client .get(&full_url) .header("token", header_token) .header("Content-Type", "application/json") .header("Accept", "application/json") .send() .await .map_err(|e| { NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::BadRequest ) })?;
```

And finally, we can handle the response and return the user with the code below:

```rust
// nanoservices/auth/kernel/src/api/users/get.rs if response.status().is_success() { let trimmed_user = response .json::<TrimmedUser>() .await .map_err(|e| NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::BadRequest ))?; return Ok(trimmed_user) } else { return Err(NanoServiceError::new( format!("Failed to get user: {}", response.status()), NanoServiceErrorStatus::BadRequest, )) }
```

Remember, we must make sure that our get API is publicly available in the kernel module. With our kernel built out, we can make our auth kernel accessible to our to-do server by declaring the kernel in the Cargo.toml of the to-do server with the following code:

```toml
# nanoservices/to_do/core/Cargo.toml . . . auth-kernel = { path = "../../auth/kernel" } [features] memory = ["auth-kernel/core-postgres"] http = ["auth-kernel/http"]
```

And with this, our to-do server can get users by compiling the auth server and calling an API function or performing a HTTP request to the auth server.

We have maintained our flexibility with our deployment; however, our to-do server does not need to use user data right now. To make our user data useful to our to-do server, we must tether our to-do items to our user ID so users can see and edit their own items.

## Tethering users to to-do items

Right now, all our to-do items are globally accessible to any user that logs in. To silo our to-do items, we must carry out the following steps:

Link to-do items to user in the database

Add user ID to data access transactions

Add user ID to the core API functions

Add user ID to the networking API functions

Seeing as all the steps require the database to be updated, we will start with the database configuration.

### Linking our to-do items to users in the database

If we had both the user and to-do item tables in the same database, we could tether them using a foreign key as seen in figure 11.1.

Figure 11.1 – Foreign key association between our user and items

There are plenty of advantages to the foreign key approach. For instance, we would not be able to enter a user ID into the item table if the user ID did not exist in the user table. We could also perform a join query with the SQL below:

```sql
SELECT User.id AS user_id, User.username, Item.id AS todo_id, Item.title, status.status, FROM users JOIN todos ON users.id = todos.user_id;
```

This means that each row would contain elements from the item, and the user associated with that item.

However, our items are on the to-do server, and our users are on our auth server. Due to our setup, they could be on the same database, but we must also accommodate for the possibility of the items and users being on separate databases. Like everything in software engineering there is always trade-offs. Microservices enable you to break a massive system into components, and have individual teams work on these individual services. You can also use different languages, and deployment pipelines whilst being more complex will be smoother. However, none of this is for free. Microservices add complexity. To accommodate the possibility of two different databases, we have a third table where rows consist of user IDs and item IDs as seen in figure 11.2.

Figure 11.2 – A separate database table for logging items associations with users

This does increase the risk of us accidentally putting in an ID that does not exist, or not deleting items if we delete a user, as we must perform operations on this separate table. However, this does decouple the associations. For instance, we can have items that have no association to a user. This might be useful if you plan on building out teams in the application, and in those teams some items may not have been assigned to a user. With this additional table, we can also assign multiple users to one item, and multiple items to one user.

Now that we know how we are going to link our users with items, we can create a new migration for our to-do service. To automate our migration creation, we can create a bash script housing the following code:

```bash
#!/usr/bin/env bash # nanoservices/to_do/scripts/add_migration.sh # Check if argument is provided if [ $# -eq 0 ]; then echo "Usage: $0 <migration-name>" exit 1 fi # navigate to directory SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )" cd $SCRIPTPATH/../dal/migrations # create SQL script name current_timestamp=$(date +'%Y%m%d%H%M%S') description="$1" script_name="${current_timestamp}_${description}.sql" touch $script_name
```

Here, we can see that we exit the script if we do not provide an argument for the script. We then navigate to the migrations directory, create a timestamp, and create an SQL script with our argument, prefixed with a timestamp. This ensures that the migrations that we create will run in the order that they were created. Inside our newly formed migration file, we create the reference table with the code below:

```sql
-- nanoservices/to_do/dal/migrations/ -- 20240628025045_adding-user-connection.sql CREATE TABLE user_connections ( user_id INTEGER NOT NULL, to_do_id INTEGER NOT NULL, PRIMARY KEY (user_id, to_do_id) );
```

We have a combination of the user ID and the item ID to ensure that we do not insert duplicate associations.

Now that our database can link items to users, we can move onto adding this linking table to our schema in our data access layer with the following code:

```rust
// nanoservices/to_do/dal/src/to_do_items/schema.rs #[derive(Serialize, Deserialize, Debug, Clone)] #[cfg_attr(feature = "sqlx-postgres", derive(sqlx::FromRow))] pub struct UserConnection { pub user_id: i32, pub to_do_id: i32 }
```

We can see that we have declared that the UserConnection struct is only declared if our Postgres feature is activated. This is because we do not have the connection table if we are using a JSON file. With our schema done, our entire system can interact with the database for tethering to-do items to users. Therefore, we can move onto adding the user ID to the database transactions.

### Adding user IDs to data access transactions

For our transactions, we can start with the create with the following code:

```rust
// nanoservices/to_do/dal/src/to_do_items/transactions/create.rs pub type SaveOneResponse = Result<ToDoItem, NanoServiceError>; pub trait SaveOne { fn save_one(item: NewToDoItem, user_id: i32) -> impl Future<Output = SaveOneResponse> + Send; } #[cfg(feature = "sqlx-postgres")] impl SaveOne for SqlxPostGresDescriptor { fn save_one(item: NewToDoItem, user_id: i32) -> impl Future<Output = SaveOneResponse> + Send { sqlx_postgres_save_one(item, user_id) } } #[cfg(feature = "json-file")] impl SaveOne for JsonFileDescriptor { fn save_one(item: NewToDoItem, user_id: i32) -> impl Future<Output = SaveOneResponse> + Send { json_file_save_one(item, user_id) } }
```

Here, we can see that we merely pass in the user ID into the functions. For our sqlx_postgres_save_one function, we now have two database transactions. We create the item, and then use the ID of that item into the connections with the code below:

```rust
// nanoservices/to_do/dal/src/to_do_items/transactions/create.rs #[cfg(feature = "sqlx-postgres")] async fn sqlx_postgres_save_one(item: NewToDoItem, user_id: i32) -> SaveOneResponse { let item = sqlx::query_as::<_, ToDoItem>(" INSERT INTO to_do_items (title, status) VALUES ($1, $2) RETURNING *" ).bind(item.title) .bind(item.status.to_string()) .fetch_one(&*SQLX_POSTGRES_POOL).await.map_err(|e| { NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?; let _ = sqlx::query(" INSERT INTO user_connections (user_id, to_do_id) VALUES ($1, $2)" ).bind(user_id) .bind(item.id) .execute(&*SQLX_POSTGRES_POOL).await.map_err(|e| { NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?; Ok(item) }
```

And finally, for our JSON save file function takes the following form:

```rust
// nanoservices/to_do/dal/src/to_do_items/transactions/create.rs #[cfg(feature = "json-file")] async fn json_file_save_one(item: NewToDoItem, user_id: i32) -> SaveOneResponse { let mut tasks = get_all::<ToDoItem>().unwrap_or_else(|_| HashMap::new() ); let to_do_item = ToDoItem { id: 1, title: item.title, status: item.status.to_string() }; tasks.insert( to_do_item.title.to_string() + ":" + &user_id.to_string(), to_do_item.clone() ); let _ = save_all(&tasks)?; Ok(to_do_item) }
```

Here, we can see that we have created a key out of the title and user ID with the : delimiter. This means we can still have a single hashmap for a JSON file, but still separate the items depending on user ID.

Our delete transaction can take the same approach as our create transaction but in an inverse manner. This is a good time for you to try and code this section yourself. If you did attempt this, your trait implementations should look like the following code:

```rust
// nanoservices/to_do/dal/src/to_do_items/transactions/create.rs pub type DeleteOneResponse = Result<ToDoItem, NanoServiceError>; pub trait DeleteOne { fn delete_one(title: String, user_id: i32) -> impl Future<Output = DeleteOneResponse> + Send; } #[cfg(feature = "sqlx-postgres")] impl DeleteOne for SqlxPostGresDescriptor { fn delete_one(title: String, user_id: i32) -> impl Future<Output = DeleteOneResponse> + Send { sqlx_postgres_delete_one(title, user_id) } } #[cfg(feature = "json-file")] impl DeleteOne for JsonFileDescriptor { fn delete_one(title: String, user_id: i32) -> impl Future<Output = DeleteOneResponse> + Send { json_file_delete_one(title, user_id) } }
```

Your Postgres dele function should look like the code below:

```rust
// nanoservices/to_do/dal/src/to_do_items/transactions/create.rs #[cfg(feature = "sqlx-postgres")] async fn sqlx_postgres_delete_one(title: String, user_id: i32) -> DeleteOneResponse { let item = sqlx::query_as::<_, ToDoItem>(" DELETE FROM to_do_items WHERE title = $1 RETURNING *" ).bind(title) .fetch_one(&*SQLX_POSTGRES_POOL).await.map_err(|e| { NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?; let _ = sqlx::query(" DELETE FROM user_connections WHERE user_id = $1 AND to_do_id = $2" ).bind(user_id) .bind(item.id) .execute(&*SQLX_POSTGRES_POOL).await.map_err(|e| { NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?; Ok(item) }
```

And your delete function for the JSON file should be like the following code:

```rust
// nanoservices/to_do/dal/src/to_do_items/transactions/create.rs #[cfg(feature = "json-file")] async fn json_file_delete_one(title: String, user_id: i32) -> DeleteOneResponse { let mut tasks = get_all::<ToDoItem>().unwrap_or_else( |_| HashMap::new() ); let to_do_item = tasks.remove( &title + ":" + &user_id.to_string() ).ok_or_else(|| { NanoServiceError::new( "Item not found".to_string(), NanoServiceErrorStatus::NotFound ) })?; let _ = save_all(&tasks)?; Ok(to_do_item) }
```

We can now move onto our get transaction with the implementations below:

```rust
// nanoservices/to_do/dal/src/to_do_items/transactions/get.rs pub type GetAllResponse = Result<Vec<ToDoItem>, NanoServiceError>; pub trait GetAll { fn get_all(user_id: i32) -> impl Future<Output = GetAllResponse> + Send; } #[cfg(feature = "sqlx-postgres")] impl GetAll for SqlxPostGresDescriptor { fn get_all(user_id: i32) -> impl Future<Output = GetAllResponse> + Send { sqlx_postgres_get_all(user_id) } } #[cfg(feature = "json-file")] impl GetAll for JsonFileDescriptor { fn get_all(user_id: i32) -> impl Future<Output = GetAllResponse> + Send { json_file_get_all(user_id) } }
```

For our Postgres get function, we get all the item IDs that are associated with the user and query the items table that have IDs in the list of item IDs that are associated with the user. We can see this query unfold with the following code:

```rust
// nanoservices/to_do/dal/src/to_do_items/transactions/get.rs #[cfg(feature = "sqlx-postgres")] async fn sqlx_postgres_get_all(user_id: i32) -> GetAllResponse { let items = sqlx::query_as::<_, ToDoItem>(" SELECT * FROM to_do_items WHERE id IN ( SELECT to_do_id FROM user_connections WHERE user_id = $1 )") .bind(user_id) .fetch_all(&*SQLX_POSTGRES_POOL).await.map_err(|e| { NanoServiceError::new( e.to_string(), NanoServiceErrorStatus::Unknown ) })?; Ok(items) }
```

For our JSON file implementation, we load the file, get the key of each item, split the key with our delimiter to get the user ID, and push the item to a vector if the user ID extracted from the key matches the user ID passed into the function with the code below:

```rust
// nanoservices/to_do/dal/src/to_do_items/transactions/get.rs #[cfg(feature = "json-file")] async fn json_file_get_all(user_id: i32) -> GetAllResponse { let tasks = get_all::<ToDoItem>() .unwrap_or_else(|_| HashMap::new()); let items = tasks.values().cloned().collect(); let mut filtered_items: Vec<ToDoItem> = Vec::new(); for item in items { let key = item.id.to_string().split(":").nth(1).unwrap(); let item_user_id = key.parse::<i32>().unwrap(); if item_user_id == user_id { filtered_items.push(item); } } Ok(filtered_items) }
```

Finally, for our update, we have the following trait implementations:

```rust
// nanoservices/to_do/dal/src/to_do_items/transactions/update.rs pub type UpdateOneResponse = Result<ToDoItem, NanoServiceError>; pub trait UpdateOne { fn update_one(item: ToDoItem, user_id: i32) -> impl Future<Output = UpdateOneResponse> + Send; } #[cfg(feature = "sqlx-postgres")] impl UpdateOne for SqlxPostGresDescriptor { fn update_one(item: ToDoItem, _user_id: i32) -> impl Future<Output = UpdateOneResponse> + Send { sqlx_postgres_update_one(item) } } #[cfg(feature = "json-file")] impl UpdateOne for JsonFileDescriptor { fn update_one(item: ToDoItem, user_id: i32) -> impl Future<Output = UpdateOneResponse> + Send { json_file_update_one(item, user_id) } }
```

Because we are passing in the item to be altered, we already know the ID of the item, therefore, we do not need the user ID for the Postgres implementation. This is because the item ID directly is enough to get the correct item, and we are not updating the connections table as the item is still in the database after the updated and therefore still associated with the user. However, because we have the ID of the user in the key for the JSON file, we must utilize the user ID for the JSON file with the following code:

```rust
// nanoservices/to_do/dal/src/to_do_items/transactions/update.rs #[cfg(feature = "json-file")] async fn json_file_update_one(item: ToDoItem, user_id: i32) -> UpdateOneResponse { let mut tasks = get_all::<ToDoItem>().unwrap_or_else( |_| HashMap::new() ); let key = item.title.clone() + ":" + &user_id.to_string(); if !tasks.contains_key(&key) { return Err(NanoServiceError::new( format!("Item with name {} not found", item.title), NanoServiceErrorStatus::NotFound )); } tasks.insert(key, item.clone()); let _ = save_all(&tasks)?; Ok(item) }
```

And this is it, the data access layer is now fully tethered to the user ID for items. We now must pass the user ID into the core functions.

### Adding user IDs to core functions

For our core function, we must remember that our core does not want to have dependencies on other servers. Therefore, we merely pass in the user ID into our core API functions with the following code:

```rust
// nanoservices/to_do/core/src/api/basic_actions/create.rs pub async fn create<T: SaveOne>(item: NewToDoItem, user_id: i32) -> Result<ToDoItem, NanoServiceError> { let created_item = T::save_one(item, user_id).await?; Ok(created_item) } // nanoservices/to_do/core/src/api/basic_actions/delete.rs pub async fn delete<T: DeleteOne>(id: &str, user_id: i32) -> Result<(), NanoServiceError> { let _ = T::delete_one(id.to_string(), user_id).await?; Ok(()) } // nanoservices/to_do/core/src/api/basic_actions/get.rs pub async fn get_all<T: GetAll>(user_id: i32) -> Result<AllToDOItems, NanoServiceError> { let all_items = T::get_all(user_id).await?; AllToDOItems::from_vec(all_items) } // nanoservices/to_do/core/src/api/basic_actions/update.rs pub async fn update<T: UpdateOne>(item: ToDoItem, user_id: i32) -> Result<(), NanoServiceError> { let _ = T::update_one(item, user_id).await?; Ok(()) }
```

And our core is now tethered, now we must move onto adding the user ID to the networking layer.

### Adding user IDs to networking functions

For our networking, we must get the user ID from the auth server via the kernel. Therefore, we must declare our auth kernel with the code below:

```toml
// nanoservices/to_do/networking/actix_server/Cargo.toml . . . auth-kernel = { path = "../../../auth/kernel" } [features] auth-http = ["auth-kernel/http"] auth-core-postgres = ["auth-kernel/core-postgres"] default = ["auth-core-postgres"]
```

Here we can see that we propagate the features to the auth kernel. Therefore, if we activate the auth-http feature the, we will interact with our auth server via HTTP. If we activate the auth-core-postgres feature, we will be compiling the auth server into the to-do server and call the auth functions directly. The Postgres is our default.

We can see how this is implemented for the create endpoint with the following code:

```rust
// nanoservices/to_do/networking/actix_server/src // /api/basic_actions/create.rs . . . use auth_kernel::api::users::get::get_user_by_unique_id; pub async fn create<T, X>( token: HeaderToken, body: Json<NewToDoItem> ) -> Result<HttpResponse, NanoServiceError> where T: SaveOne + GetAll, X: GetUserSession { let session = X::get_user_session(token.unique_id).await?; let _ = create_core::<T>(body.into_inner(), session.user_id).await?; Ok(HttpResponse::Created().json(get_all_core::<T>( session.user_id ).await?)) }
```

Here we can see that we get the user unique ID from the token and get the user from the auth kernel. We can apply this approach to all the other endpoints defined below:

```rust
// nanoservices/to_do/networking/actix_server/src // /api/basic_actions/get.rs . . . use auth_kernel::api::users::get::get_user_by_unique_id; pub async fn get_all<T, X>(token: HeaderToken) -> Result<HttpResponse, NanoServiceError> where T: GetAll, X: GetUserSession { let session = X::get_user_session( token.unique_id ).await?; Ok(HttpResponse::Ok().json(get_all_core::<T>(session.user_id).await?)) }
```

For update:

```rust
// nanoservices/to_do/networking/actix_server/src // /api/basic_actions/update.rs . . . use auth_kernel::api::users::get::get_user_by_unique_id; pub async fn update<T, X>( token: HeaderToken, body: Json<ToDoItem> ) -> Result<HttpResponse, NanoServiceError> where T: UpdateOne + GetAll, X: GetUserSession { let session = X::get_user_session(token.unique_id).await?; let _ = update_core::<T>(body.into_inner(), session.user_id).await?; Ok(HttpResponse::Ok().json(get_all_core::<T>( session.user_id ).await?)) }
```

For delete:

```rust
// nanoservices/to_do/networking/actix_server/src // /api/basic_actions/delete.rs . . . use auth_kernel::api::users::get::get_user_by_unique_id; pub async fn delete_by_name<T, X>( token: HeaderToken, req: HttpRequest ) -> Result<HttpResponse, NanoServiceError> where T: DeleteOne + GetAll, X: GetUserSession { let session = X::get_user_session(token.unique_id).await?; match req.match_info().get("name") { Some(name) => { delete_core::<T>(name, session.user_id).await?; }, None => { . . . } }; Ok(HttpResponse::Ok().json(get_all_core::<T>( session.user_id ).await?)) }
```

And with this, our networking layer for our to-do server is now tethered with our user ID. This means that once logged in, a user can only see and perform operations that are tethered to the user.

Our system now has the to-do server talking to the auth server. If we were to run our ingress server, it would work as intended and you would only see the to-do items in the frontend that correspond to the user logged in. However, this is down to the to-do server calling the authentication server in the same binary as all the servers and frontend are compiled into one binary. We know the ingress server will work because it compiles. However, what about when both of our servers are running independently on different ports? Even if it compiles, the HTTP request to the auth server from the to-do server could be faulty. We must test our HTTP request.

## Testing our server-to-server communication with bash

To run our test, we essentially need to spin up out database, auth server, and to-do server. We then must run a HTTP request to create the user, login, and then create a to-do item. If we can create the to-do item, we know that the communication between both servers work, because we must call the authentication server to get the user ID to create the to-do item.

To run our test, we first must create the following .env file:

```bash
# nanoservices/to_do/.env TO_DO_DB_URL=postgres://username:password@localhost/to_do AUTH_DB_URL=postgres://username:password@localhost/to_do AUTH_API_URL=http://127.0.0.1:8081 JWT_SECRET=secret CACHE_API_URL=redis://127.0.0.1:6379
```

Now that we have the environment variables that are needed, we can start with the boilerplate code for our testing bash script with the following code:

```bash
#!/usr/bin/env bash # nanoservices/to_do/scripts/test_server_com.sh SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )" cd $SCRIPTPATH cd ../../../
```

This boilerplate code ensures that the working directory of the script is going to run at the base of our system. Now that we are in the root directory of our system, we can build our database and then run it in the background with the code below:

```bash
# nanoservices/to_do/scripts/test_server_com.sh docker-compose build docker-compose up -d sleep 1 export $(cat ./nanoservices/to_do/.env | xargs)
```

We can see that we perform a build first before running the database. This is because it could take a while to build the docker containers. If the build takes too long and we are running it in the background, the rest of the script could fail due to the database not being ready when running the servers. We can also see that we export the environment variables from our .env file.

Because the are server builds might also take a while, we want to build our servers before running them with the following code:

```bash
# nanoservices/to_do/scripts/test_server_com.sh cargo build \ --manifest-path ./nanoservices/to_do/networking/actix_server/Cargo.toml \ --features auth-http \ --release \ --no-default-features cargo build \ --manifest-path ./nanoservices/auth/networking/actix_server/Cargo.toml \ --release
```

Here, we can see that we are using –no-default-features and the auth-http feature for our to-do server. This is the ensure that we are compiling the auth kernel into the to-do server with HTTP requests.

We now can run our servers in the background with the code below:

```bash
# nanoservices/to_do/scripts/test_server_com.sh cargo run \ --manifest-path ./nanoservices/to_do/networking/actix_server/Cargo.toml \ --features auth-http \ --release --no-default-features & TO_DO_PID=$! cargo run \ --manifest-path ./nanoservices/auth/networking/actix_server/Cargo.toml \ --release & AUTH_PID=$!
```

The & means that the command will be run in the background. We can also see that we get the process IDs with the TO_DO_PID=$! and AUTH_PID=$! straight after their respective commands. We will reference these process IDs to kill the servers once we have finished with the servers.

Now our servers are running, and our database is also running. We can now create a user and login to assign the login result to the token variable with the following code:

```bash
# nanoservices/to_do/scripts/test_server_com.sh sleep 2 curl -X POST http://127.0.0.1:8081/api/v1/users/create \ -H "Content-Type: application/json" \ -d '{ "email": "test@gmail.com", "password": "password" }' token=$(curl \ -u test@gmail.com:password \ -X GET http://127.0.0.1:8081/api/v1/auth/login) token=$(echo "$token" | tr -d '\r\n' | sed 's/^"//' | sed 's/"$//')
```

The final line stripes the token of quotation marks, spaces, and new lines. We can now insert that token into the header of our create HTTP request with the code below:

```bash
# nanoservices/to_do/scripts/test_server_com.sh response=&(curl -X POST http://127.0.0.1:8080/api/v1/create \ -H "Content-Type: application/json" \ -H "token: $token" \ -d '{ "title": "code", "status": "PENDING" }') sleep 1 echo $response sleep 2
```

And finally, we kill the servers, and tear down the database container with the following code:

```bash
# nanoservices/to_do/scripts/test_server_com.sh kill $TO_DO_PID kill $AUTH_PID docker-compose down
```

Once we run this script, we should get the printout below:

. . . Migrating auth database... auth database migrations completed: () Migrating to-do database... to-do database migrations completed: () . . . {"pending":[{"id":1,"title":"code","status":"PENDING"}],"done":[]} [+] Running 2/1 ✔ Container to-do-postgres Removed 0.1s ✔ Network tethering-users-to-items_default Removed

There is more to the printout such as the build phase and printouts of CURL network stats, but including these would just bloat the chapter. What we can see here is that the migrations have happened, and our create HTTP request was successful. Our database was then removed.

Here we have it, our system can support two servers that talk to each other via HTTP.

## Summary

In this chapter, we got our to-do server communicating with the authentication server through a kernel, by either compiling the auth server into the to-do server or making a HTTP request to the authentication server. In a microservice structure, communicating between servers is essential. Here we have the flexibility of still running our system as a monolith app with one database, a monolith app with two separate databases, two separate servers with on database, or two separate servers with one database. Here, you have the ultimate flexibility when it comes to deploying your system. We will explore other protocols to communicate between Rust servers in chapter 19, Nanoservices where we will pass binary messages between servers via TCP. We will also explore more fundamental concepts around networking in chapter 21, rolling our own HTTP protocol from TCP with framing. In the next chapter, we will handle timeouts of our authentication sessions with caching.

## Questions

At a high level how did we make the user accessible via the unique ID?

How do we make the core API function available to other servers directly by compiling the core function into another server?

How do we expose the HTTP endpoint of one server to another?

There is a risk that two different servers will be running transactions on two different databases. How can we tether data from one service to another considering that we must accommodate the possibility of two different databases?

What is the risk of your database solution?

## Answers

We added the get user by unique ID function from the database to the data access layer. We then add a function referencing DAL function in the core. Finally, we add a HTTP layer around the core function in the networking layer.

We build another Rust workspace that is a kernel. This kernel has the API functions that we want to expose and the other server can compile the kernel to call the core functions.

We can add a HTTP feature to our kernel. This HTTP feature will not compile the core function, but instead construct a HTTP request for the endpoint. The interface stays the same so if we want to change our deployment strategy we do not have to change any code.

We can have a reference table on one of the databases that has the ID of the row of the data from one database and the ID of the row from another database. We can then read that table to work out what the data from one table is tethered to in a table from another database.

Data consistency is the risk. Unlike foreign keys pointing to tables in same database, our code might not update the data correctly and we could have data that is not logged in the reference table, or we could have dangling references due to the reference not being deleted when the data is deleted.
