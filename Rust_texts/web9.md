# 10 Managing user sessions

We now have our to-do server working. However, there is no authentication. Anybody can access the application and alter the to-do list. As we know, the internet just does not work like this. We must authenticate our users before we can allow them to alter to-do items. In this chapter, we are going to build an authentication server, and integrate it into our system so we can authenticate our users before allowing users to access the to-do items. In this chapter, we will cover the following:

Building an auth server

Defining our user data model

Storing passwords

Verifying passwords

Creating users

Refactoring our JWT

Building our login API

Adding authentication to our frontend

By the end of this chapter, you will be able to integrate another server into your system and have it compile into the same binary as the rest of the system. You will also be able to understand the basics of authentication so you can handle user accounts a login users so these users can make authenticated calls to the rest of the application.

## Technical requirements

This chapter will be relying on the code in the previous chapter that can be found at the following link:

`https://github.com/PacktPublishing/Rust-Web-Programming-3E/tree/main/chapter08`

## Building our Auth Server

For our authentication server, we must perform the following operations:

Create users

Login

Logout

Authentication servers will also handle other processes such as deleting users, blocking them, allocation roles and much more. However, there comes a point to where implementing extra authentication features adds bloat to the chapter with diminishing educational returns. By the end of this chapter, you will be able to understand the processes to add extra functions.

Our authentication server should be housed in the nanoservices/core directory. Our authentication server has the following file structure:

├── core │ ├── . . . ├── dal │ ├── . . . └── networking └── . . .

We will investigate each section in more detail. There is a lot of repetition compared to the to-do server when it comes to generic boilerplate of making database connections and running the server.

When it comes to duplication of code, there is always a trade-off. We can all appreciate that excessive duplication of code is not desirable. However, duplication also facilitates decoupling. Microservices as a general concept can be criticized as being excessive as your features are small. However, in my experience, these servers grow aggressively as the project progresses. Keeping the servers isolated enables us to break the servers out into their own repositories. Teams could work in their own assigned servers with a lot of freedom without having to coordinate with other teams if they wanted to do something like alter the way in which a database connection worked, or how a server listened to incoming requests. We could put the server and connection code into the glue module, however, there does not have to be a consistency across the entire system. At this point it is down to personal preference. Some people hate duplicating code at any point. However, in my personal experience, I find that we are constantly learning about our problem/business needs as our system evolves. Being able to keep servers isolated so they can be swapped out or deployed individually has been worth the duplicated code. For instance, I have depreciated individual services. Because they were isolated, we could keep them running without hindering the progression of other services until the new replacement of the service was built, tested, and deployed.

For our auth service modules, we will start with the data access layer.

## Data Access Layer

Our data access layer has the following directory outline:

dal └── Cargo.toml ├── migrations │ └── 20240523088625_initial-setup.sql └── src ├── connections │ ├── mod.rs │ └── sqlx_postgres.rs ├── lib.rs ├── migrations.rs └── users ├── descriptors.rs ├── mod.rs ├── schema.rs └── transactions ├── create.rs ├── get.rs └── mod.rs

To avoid bloat of the chapter, we will not cover every line of boilerplate code. First off, ensure that all the files are linked to the lib.rs file via the mod files. Our schema file is going to be empty at this point as we will build the user schema throughout the chapter. Same goes for the SQL migrations and transactions directory as these are all dependent on the user data model.

When it comes to our SQLX Postgres connection in the src/connections/sqlx_postgres.rs file, the code is the same as our to-do service database connection. The only difference is the environment variables where the connection URL is AUTH_DB_URL, and the max connections is AUTH_MAX_CONNECTIONS. Having a different environment variable for the connection information gives us the flexibility of having both of our servers pointing to the same database of their own separate ones. As our auth migrations will not clash with the to-do migrations, the database will be able to run them both without any problems. For the src/migrations.rs file the code is the same but with reference to auth as opposed to to-do in the print statements.

For our src/users/descriptors.rs file, we will only have the SqlxPostGresDescriptor struct to avoid bloat of the chapter as we will be using Postgres for the remainder of the book. However, if you want to add file support follow the same structure as the to-do server for the data access layer.

Finally, our Cargo.toml file has the following contents:

```toml
[package]
name = "auth-dal"
version = "0.1.0"
edition = "2021"
[dependencies]
serde ={ version="1.0.197", features = ["derive"] }
glue = { path = "../../../glue"}
sqlx = { version = "0.7.4", features = ["postgres", "json", "runtime-tokio"], optional = false }
once_cell = { version = "1.19.0", optional = false }
```

We can see that we called out package "auth-dal". While other cargo files can handle aliases to avoid clashes, workspaces cannot handle such clashes at the time of writing this book. We can also see that we have gotten rid of the features as we are just supporting Postgres here, and as a result, none of our dependencies are optional.

With this, our data access layer is ready to be plugged in and developed. Seeing as we have done a lot of linking files in one go, I usually perform a quick cargo build just to check everything is done correctly. Now our data access layer is done, we can move onto the core of the auth server.

## Core

Our core has the following file structure:

core └── Cargo.toml └── src ├── api │ ├── auth │ │ ├── login.rs │ │ ├── logout.rs │ │ └── mod.rs │ ├── mod.rs │ └── users │ ├── create.rs │ └── mod.rs └── lib.rs

Remember, our core does not have dependencies such as databases or networking, so our boilerplate code is zero. Here we just need to ensure that all the files are linked, and that the Cargo.toml file has the following contents:

```toml
[package]
name = "auth-core"
version = "0.1.0"
edition = "2021"
[dependencies]
auth-dal = { path = "../dal" }
serde = { version = "1.0.197", features = ["derive"] }
glue = { path = "../../../glue"}
```

And finally, we move onto our networking layer.

## Networking Layer

Our networking layer has the following file structure:

networking └── actix_server ├── Cargo.toml └── src ├── api │ ├── auth │ │ ├── login.rs │ │ ├── logout.rs │ │ └── mod.rs │ ├── mod.rs │ └── users │ ├── create.rs │ └── mod.rs └── main.rs

Again, we link all the files together and define the following api endpoint factories. For the users factory we have the definition below:

```rust
// nanoservices/auth/networking/actix_server/ // src/api/users/mod.rs
pub mod create;
use actix_web::web::ServiceConfig;
pub fn users_factory(app: &mut ServiceConfig) {
}
```

For the auth factory we have the following code:

```rust
// nanoservices/auth/networking/actix_server/ // src/api/auth/mod.rs
pub mod login;
pub mod logout;
use actix_web::web::ServiceConfig;
pub fn auth_factory(app: &mut ServiceConfig) {
}
```

And finally, our main api factory is defined below:

```rust
// nanoservices/auth/networking/actix_server/ // src/api/mod.rs
pub mod auth;
pub mod users;
use actix_web::web::ServiceConfig;
pub fn views_factory(app: &mut ServiceConfig) {
    users::users_factory(app);
    auth::auth_factory(app);
}
```

With our endpoint factories defined, our main file takes the following form:

```rust
// nanoservices/auth/networking/actix_server/ // src/main.rs
use actix_web::{App, HttpServer};
mod api;
use auth_dal::migrations::run_migrations;
#[tokio::main]
async fn main() -> std::io::Result<()> {
    run_migrations().await;
    HttpServer::new(|| {
        App::new().configure(api::views_factory)
    })
    .workers(4)
    .bind("127.0.0.1:8081")?
    .run()
    .await
}
```

Here we can see that we have increased the port number by one because we might want both our servers running as we do not want a clash for the same port.

Finally, our Cargo.toml file has the following dependencies:

```toml
# nanoservices/auth/networking/actix_server/Cargo.toml
[package]
name = "auth_actix_server"
version = "0.1.0"
edition = "2021"
[dependencies]
tokio = { version = "1.36.0", features = ["full"] }
actix-web = "4.5.1"
auth-core = { path = "../../core" }
auth-dal = { path = "../../dal" }
glue = { path = "../../../../glue", features = ["actix"] }
```

And here we have it, our auth server is now ready to develop on. We can start by building out the user data model.

## Defining Our User Data Model

When it comes to defining our user data model that is going to enable us to insert and get users from our database, we take the same approach that we did when creating our to-do item data model. This means we create a new user struct without an ID, and a user struct that has an ID due to it now being inserted in the database.

With our approach in mind, we can define the new user struct with the following code:

```rust
// nanoservices/auth/dal/src/users/schema.rs
use serde::{Serialize, Deserialize};
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct NewUser {
    pub email: String,
    pub password: String,
    pub unique_id: String
}
```

We can see that we have a unique ID as well as a standard ID. Unique IDs become useful when you need to expose an ID to the outside world. For instance, if we wanted to make our users verify that the email address they provided was real, we could send an email with a link containing the unique ID for the user to click on. Once the user has clicked on the link, we could match the unique ID in the link to the unique ID of the user and then verify the user. Once the user is verified, the unique ID can be changed. We will use the unique ID in our token-based login sessions. Putting the unique ID into the login token ensures that if someone else gets hod of the token, the information in the token will only be valid for a short while as we can change the unique ID when refreshing the token.

Now that our new user struct is done, we can now define our user struct with the code below:

```rust
// nanoservices/auth/dal/src/users/schema.rs
#[derive(Deserialize, Serialize, Debug, Clone, PartialEq, sqlx::FromRow)]
pub struct User {
    pub id: i32,
    pub email: String,
    pub password: String,
    pub unique_id: String
}
```

However, there is an issue here. We have a implemented the Serialize and Deserialize trait for the User struct, but we do not want to expose the password processed outside of the auth server. To remedy this, we also have a TrimmedUser struct that does not have password and can also be constructed from the User struct with the following code:

```rust
// nanoservices/auth/dal/src/users/schema.rs
#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
pub struct TrimmedUser {
    pub id: i32,
    pub email: String,
    pub unique_id: String
}
impl From<User> for TrimmedUser {
    fn from(user: User) -> Self {
        TrimmedUser {
            id: user.id,
            email: user.email,
            unique_id: user.unique_id
        }
    }
}
```

With our structs, we only need to define the SQL script for the database migrations with the code below:

```sql
-- nanoservices/auth/dal/migrations/*_initial_setup.sql
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    email VARCHAR NOT NULL UNIQUE,
    password VARCHAR NOT NULL,
    unique_id VARCHAR NOT NULL UNIQUE
);
```

Here, we can see that we ensure that the email is unique. This is because the email is the main way we communicate with the user. For instance, if the user wants to reset their password, we cannot have the reset link sent to two or more email addresses. We can see that the unique ID is also unique as we would want to use the unique ID for verification, password resets, and identifying login sessions.

As our migrations folder is embedded into the Rust binary and runs when the server starts up, we can move onto writing code that interacts with users in the database and just use it. However, before we write any endpoints for the auth server, we must write some functionality around verifying passwords and storing these passwords.

## Storing Passwords

When storing passwords, we cannot store these passwords in a raw string format. If we did and there was a data leak, all of the passwords would be compromised. To reduce the risk of passwords being compromised, we hash the passwords before storing them in the database. We also must create our unique IDs when creating users to be inserted into the database.

To enable us to write the code that stores passwords new users, we must add the following dependencies:

```toml
# nanoservices/auth/dal/Cargo.toml
argon2 = { version = "0.5.3", features = ["password-hash"]}
uuid = {version = "1.8.0", features = ["serde", "v4"]}
rand = "0.8.5"
```

The uses of the crates are described below:

Argon2: used for the password hashing and verification.

uuid: for the creation of unique IDs.

rand: random generation functionality used for creating a "salt string" for hashing the password.

With these dependencies, we now need to use the following so we can build our password hashing code:

```rust
// nanoservices/auth/dal/src/users/schema.rs
use glue::errors::{NanoServiceError, NanoServiceErrorStatus};
use argon2::{ Argon2, PasswordHasher, PasswordVerifier, password_hash::{ SaltString, PasswordHash } };
```

To ensure that the NewUser password is hashed, we can hash the password in the NewUser constructor that has the following outline:

```rust
// nanoservices/auth/dal/src/users/schema.rs
impl NewUser {
    pub fn new(email: String, password: String) -> Result<NewUser, NanoServiceError> {
        . . .
    }
}
```

Inside our constructor, we create a unique ID, and hashed password with the following code:

```rust
// nanoservices/auth/dal/src/users/schema.rs
// Fn => NewUser::new
let unique_id = uuid::Uuid::new_v4().to_string();
let salt = SaltString::generate(&mut rand::thread_rng());
let argon2_hasher = Argon2::default();
let hash = argon2_hasher.hash_password(
    password.as_bytes(),
    &salt
).map_err(|e|{ NanoServiceError::new(
    format!("Failed to hash password: {}", e),
    NanoServiceErrorStatus::Unknown
)})?.to_string();
Ok(NewUser {
    email,
    password: hash,
    unique_id
})
```

We can see that we salt and hash the password. Before we can explain salting, we must explain what hashing is.

Hashing is the process of converting a given input (in this case, a password) into a fixed-size string of characters, which typically appears as a seemingly random sequence of characters. This transformation is performed by a hash function. The key properties of a cryptographic hash function are:

Deterministic: The same input always produces the same hash output.

Irreversible: It should be computationally infeasible to reverse the hash to obtain the original input.

Unique: Even small changes to the input should produce significantly different hash outputs.

Collision-resistant: It should be highly unlikely for two different inputs to produce the same hash output.

In the provided code, the `Argon2::default()` represents the default configuration of the Argon2 hashing algorithm, which is a widely-used and secure hash function designed specifically for hashing passwords.

Salting involves adding a unique, random value (called a salt) to the password before hashing it. The primary purposes of salting are:

Uniqueness: Ensures that even if two users have the same password, their hashes will be different because each hash will include a unique salt.

Prevents Pre-computation Attacks: Salts prevent the use of precomputed tables (rainbow tables) for reversing hash values, as the same password will have different hashes when different salts are used.

Password Uniqueness: Even if two users choose the same password, the use of a unique salt ensures that the resulting hashes are different, adding another layer of security.

We combine salting and hashing for the following reasons:

Protection Against Hash Table Attacks: Without salts, an attacker could use precomputed hash tables to quickly look up and reverse hash values back into their original passwords.

Protection Against Brute-force Attacks: Hashing algorithms like Argon2 are designed to be computationally expensive, making brute-force attacks (trying all possible passwords) slower and more difficult.

Password Uniqueness: Even if two users choose the same password, the use of a unique salt ensures that the resulting hashes are different.

We have now handled the storing of passwords, but we cannot verify them yet.

## Verifying Passwords

For verifying passwords, we must get the hashed password from the database, hash the password passed in by the user for the login, and then compare them. The nuance around verifying passwords is beyond the scope of this book as cryptography is an entire field of itself. For our verify process, although verifying passwords does not have any direct relation to storing data, I am going to put the verifying password logic in the schema.rs in our data access layer. This is where I choose to break by rule around putting logic in a defined context. The verifying of the password does make sense to be in the core, however, the hashing of the password to be stored is already in the schema.rs in our data access layer. A developer looking at our schema.rs in our data access layer will quickly see how the password is hashed for storage and how the password is verified. If there was a lot more complexity around verifying a password to the point it needed an entire module, then it would be worth putting it into the core and making other developers work a little harder to mentally map the domain of passwords.

Our verification password code takes the following form:

```rust
// nanoservices/auth/dal/src/users/schema.rs
impl User {
    pub fn verify_password(&self, password: String) -> Result<bool, NanoServiceError> {
        . . .
    }
}
```

Inside our verify_password function, we verify the password with the code below:

```rust
// nanoservices/auth/dal/src/users/schema.rs
let argon2_hasher = Argon2::default();
let parsed_hash = PasswordHash::new(
    &self.password
).map_err(|e|{ NanoServiceError::new(
    format!("Failed to parse password hash: {}", e),
    NanoServiceErrorStatus::Unknown
)})?;
let is_valid = argon2_hasher.verify_password(
    password.as_bytes(),
    &parsed_hash
).is_ok();
Ok(is_valid)
```

We can see that we pass our stored password into a PasswordHash struct. The passing into the PasswordHash struct can result in an error because there is a change that the password that we are passing in is not hashed in the right format. For instance, the start of the hashed string should be the name of the algorithm used to generate the hash. Once we have the parsed hash, we then check to see if the password passed into the function is valid. If the password does not match, then we return a true, otherwise it is a false.

We now have everything we need to create our users and log them in.

## Creating Users

If we want to enable the creation of users, we must follow the same steps that we took when creating to-do items. These steps are the following:

Define insert user database transaction in the data access layer

Add a create user API endpoint in our core

Add a create user API endpoint in our networking layer

As there is some repetition here, this is a good place for you to practice your understanding of our system by carrying out the steps yourself. If you do attempt the steps yourself, hopefully your code will resemble the approach that we cover.

## Defining our create user database transactions

For our database transaction of inserting the user into the database, we need the following imports:

```rust
// nanoservices/auth/dal/src/users/transactions/create.rs
use crate::users::schema::{NewUser, User};
use glue::errors::{NanoServiceError, NanoServiceErrorStatus};
use std::future::Future;
use crate::connections::sqlx_postgres::SQLX_POSTGRES_POOL;
use super::super::descriptors::SqlxPostGresDescriptor;
```

We then must define our trait that is the template for inserting a user with the code below:

```rust
// nanoservices/auth/dal/src/users/transactions/create.rs
pub trait SaveOne {
    fn save_one(user: NewUser) -> impl Future<Output = Result< User, NanoServiceError> > + Send;
}
```

We then implement the trait with a placeholder function for our Postgres database descriptor with the following code:

```rust
// nanoservices/auth/dal/src/users/transactions/create.rs
impl SaveOne for SqlxPostGresDescriptor {
    fn save_one(user: NewUser) -> impl Future<Output = Result< User, NanoServiceError> > + Send {
        sqlx_postgres_save_one(user)
    }
}
```

Finally, we define our function that interacts with the database with the code below:

```rust
// nanoservices/auth/dal/src/users/transactions/create.rs
async fn sqlx_postgres_save_one(user: NewUser) -> Result<User, NanoServiceError> {
    let user = sqlx::query_as::<_, User>("
        INSERT INTO users (email, password, unique_id)
        VALUES ($1, $2, $3)
        RETURNING *
    " )
    .bind(user.email)
    .bind(user.password.to_string())
    .bind(user.unique_id)
    .fetch_one(&*SQLX_POSTGRES_POOL).await.map_err(|e| {
        NanoServiceError::new(
            e.to_string(),
            NanoServiceErrorStatus::Unknown
        )
    })?;
    Ok(user)
}
```

None of the previous code should be new, here, we are changing the type of data we are inserting, and the name of the table we are inserting in to.

We can now move onto defining our create endpoint in our core.

## Defining our core create API endpoint

For our create endpoint in our core, we only need the following code:

```rust
// nanoservices/auth/core/src/api/users/create.rs
use auth_dal::users::schema::{NewUser, User};
use auth_dal::users::transactions::create::SaveOne;
use glue::errors::NanoServiceError;
use serde::Deserialize;
#[derive(Deserialize)]
pub struct CreateUser {
    pub email: String,
    pub password: String
}
pub async fn create<T: SaveOne>(data: CreateUser) -> Result<User, NanoServiceError> {
    let user = NewUser::new(data.email, data.password)?;
    let created_item = T::save_one(user).await?;
    Ok(created_item)
}
```

We can now move onto defining our networking create API endpoint.

## Defining our networking create API endpoint

Our create endpoint for our networking layer takes the following form:

```rust
// nanoservices/auth/networking/actix_server/ // src/api/users/create.rs
use auth_dal::users::transactions::create::SaveOne;
use auth_core::api::users::create::{ create as create_core, CreateUser };
use auth_dal::users::schema::NewUser;
use glue::errors::NanoServiceError;
use actix_web::{ HttpResponse, web::Json };
pub async fn create<T: SaveOne>(body: Json<CreateUser>) -> Result<HttpResponse, NanoServiceError> {
    let _ = create_core::<T>(body.into_inner()).await?;
    Ok(HttpResponse::Created().finish())
}
```

And our create endpoint function is mounted to our server with the code below:

```rust
// nanoservices/auth/networking/actix_server/ // src/api/users/mod.rs
pub mod create;
use auth_dal::users::descriptors::SqlxPostGresDescriptor;
use actix_web::web::{ServiceConfig, scope, post};
pub fn users_factory(app: &mut ServiceConfig) {
    app.service(
        scope("/api/v1/users")
        .route("create", post().to( create::create::<SqlxPostGresDescriptor>) )
    );
}
```

And now our users are ready to be created. We will want to build out the login and logout API endpoints for our auth server but before we do this, we must refactor our header token to encode our user ID into the token.

## Refactoring our JWT

Right now, the token we extract from the header of a request is just a string. In this section, we are going to encode and decode user credentials into a Json Web Token (JWT) so our user can login once, and then make multiple authenticated API calls to our backend with the token. Because the token has a unique ID associated with the user, we not only know that the user is authenticated, but what user is making the requests. To make these changes, we must carry out the following steps:

Restructure our JWT for a unique ID

Create a get key function for encoding

Create a encode function to encode user credentials

Create a decode function to extract user credentials

Before we carry out any of these steps however, we must add the following dependency to our glue workspace:

```toml
# glue/Cargo.toml
jsonwebtoken = "9.3.0"
```

We then import the following:

```rust
// glue/src/token.rs
use crate::errors::{ NanoServiceError, NanoServiceErrorStatus };
use serde::{Serialize, Deserialize};
use jsonwebtoken::{ decode, encode, Algorithm, DecodingKey, EncodingKey, Header, Validation };
```

With these imports, we can now refactor our JWT.

## Restructuring our JWT

Our JWT now needs to house the unique ID of the user, have encoding/decoding functions, and update the FromRequest trait implementation. The main definition of our JWT struct has the following outline:

```rust
// glue/src/token.rs
#[derive(Debug, Serialize, Deserialize)]
pub struct HeaderToken {
    pub unique_id: String
}
impl HeaderToken {
    pub fn get_key() -> Result<String, NanoServiceError> {
        . . .
    }
    pub fn encode(self) -> Result<String, NanoServiceError> {
        . . .
    }
    pub fn decode(token: &str) -> Result<Self, NanoServiceError> {
        . . .
    }
}
```

We will cover the code inside those encoding/decoding functions in the following subsections. Finally, we recall that the outline for the FromRequest trait implementation takes the form below:

```rust
// glue/src/token.rs
#[cfg(feature = "actix")]
impl FromRequest for HeaderToken {
    type Error = NanoServiceError;
    type Future = Ready<Result<HeaderToken, NanoServiceError>>;
    fn from_request(req: &HttpRequest, _: &mut Payload) -> Self::Future {
        . . .
    }
}
```

Inside our from_request function, everything is the same apart from the return statement. After we have converted the token we extracted from the HTTP header into a string, we decode the token, constructing the HeaderToken struct in the process with the following code:

```rust
// glue/src/token.rs
let token = match HeaderToken::decode(&message) {
    Ok(token) => token,
    Err(e) => {
        return err(e)
    }
};
return ok(token)
```

Now that our token is being properly extracted and decoded, we can move onto our functions that enable the encoding/decoding of the token.

## Creating a get key function

When it comes to encoding and decoding our token, we want to ensure that processes outside of the server cannot access details of the user of the token. We also do not want the frontend to have the opportunity to alter the token. To prevent altering or access to details in the token, the server uses a secret key to encode and decode the token, ensuring that the token is always encoded outside of the server as depicted in figure 9.1.

Figure 9.1 – Use of token and key in a HTTP request

For our JWT, we are going to extract the token from the environment variables with the code below:

```rust
// glue/src/token.rs
pub fn get_key() -> Result<String, NanoServiceError> {
    std::env::var("JWT_SECRET").map_err(|e| {
        NanoServiceError::new(
            e.to_string(),
            NanoServiceErrorStatus::Unauthorized
        )
    })
}
```

We can see that we return an unauthorised error if we cannot get the environment variable because we cannot authorise the incoming request.

With our key, we can now encode our token in the next section.

## Encoding our token

We can encode our token with the following function:

```rust
// glue/src/token.rs
pub fn encode(self) -> Result<String, NanoServiceError> {
    let key_str = Self::get_key()?;
    let key = EncodingKey::from_secret(key_str.as_ref());
    return match encode(&Header::default(), &self, &key) {
        Ok(token) => Ok(token),
        Err(error) => Err(
            NanoServiceError::new(
                error.to_string(),
                NanoServiceErrorStatus::Unauthorized
            )
        )
    };
}
```

Here we can see that we initially get the key for encoding our token. We then parse this into an EncodingKey struct which can then be used to encode our HeaderToken struct into a token.

With our encoded token, the only thing left is to decode our encoded token into an HeaderToken struct.

## Decoding our token

We can encode our token with the code below:

```rust
// glue/src/token.rs
pub fn decode(token: &str) -> Result<Self, NanoServiceError> {
    let key_str = Self::get_key()?;
    let key = DecodingKey::from_secret(key_str.as_ref());
    let mut validation = Validation::new(Algorithm::HS256);
    validation.required_spec_claims.remove("exp");
    match decode::<Self>(token, &key, &validation) {
        Ok(token_data) => return Ok(token_data.claims),
        Err(error) => return Err(
            NanoServiceError::new(
                error.to_string(),
                NanoServiceErrorStatus::Unauthorized
            )
        )
    };
}
```

Here we can see that we get the key from the environment variable again. We then create a decoding key, with the HS256 algorithm. From this, we can deduce that the default encoding in the previous section was the HS256 algorithm. We then remove the "exp" from the claims. This means that we are removing the expiry time from the expected data in the encoded token.

Removing the "exp" from the token means that the token does not expire. If the token did expire, or the "exp" field was not in the token but we did not remove the "exp" from the expected claims, then the decoding of the token would result in an error. Removing the "exp" does make the handling of our application a lot easier as we do not have to refresh tokens because the tokens do not expire. However, it is also not as secure. For instance, if someone else got hold of a token, then they can make API on the behalf of the compromised user without any limitations.

In the next chapter we will look into managing user sessions with expiring tokens, but for now, our basic authentication token will work.

We now have mechanisms to create users and provide auth tokens, we can now build our login API endpoint.

## Building our Login API

In this section, if you attempt to code the endpoint yourself before reading the code, you are going to get the opportunity to test our knowledge of how to add API endpoints. The only thing that we are going code that has not been coded before is the API endpoint in the networking layer. To add the Login API endpoint, we must carry out the following steps:

Create a get user by email process in the data access

Create a login API function in the core

Create a login API endpoint in the networking

We can start with the data access layer.

## Getting users via email in data access

If you have attempted to code this yourself, hopefully it will follow a similar approach. First with import the following:

```rust
// nanoservices/auth/dal/src/users/transactions/get.rs
use crate::users::schema::User;
use glue::errors::{NanoServiceError, NanoServiceErrorStatus};
use std::future::Future;
use super::super::descriptors::SqlxPostGresDescriptor;
use crate::connections::sqlx_postgres::SQLX_POSTGRES_POOL;
```

We then define our trait with the code below:

```rust
// nanoservices/auth/dal/src/users/transactions/get.rs
pub trait GetByEmail {
    fn get_by_email(email: String) -> impl Future<Output = Result< User, NanoServiceError >> + Send;
}
```

And implement this trait for our Postgres descriptor with the following code:

```rust
// nanoservices/auth/dal/src/users/transactions/get.rs
impl GetByEmail for SqlxPostGresDescriptor {
    fn get_by_email(email: String) -> impl Future<Output = Result< User, NanoServiceError >> + Send {
        sqlx_postgres_get_by_email(email)
    }
}
```

Finally, we define our function that interacts with the Postgres database with the code below:

```rust
// nanoservices/auth/dal/src/users/transactions/get.rs
async fn sqlx_postgres_get_by_email(email: String) -> Result<User, NanoServiceError> {
    let item = sqlx::query_as::<_, User>("
        SELECT * FROM users WHERE email = $1
    " )
    .bind(email)
    .fetch_optional(&*SQLX_POSTGRES_POOL).await.map_err(|e| {
        NanoServiceError::new(
            e.to_string(),
            NanoServiceErrorStatus::Unknown
        )
    })?;
    match item {
        None => Err(NanoServiceError::new(
            "User not found".to_string(),
            NanoServiceErrorStatus::NotFound
        )),
        Some(item) => Ok(item)
    }
}
```

And our Postgres descriptor can get users via the email. We must note that we use the fetch_optional to check if the result in None. This is because we want to differentiate whether the user cannot be found or if the error is a result of something else.

Now our data access is working, we can move onto the core api for logging in a user.

## Creating our Core Login API

For our core login API, we just must see it to believe it. We import the following:

```rust
// nanoservices/auth/dal/src/api/auth/login.rs
use auth_dal::users::transactions::get::GetByEmail;
use glue::errors::{NanoServiceError, NanoServiceErrorStatus};
use glue::token::HeaderToken;
```

And then our login API function is defined with the code below:

```rust
// nanoservices/auth/dal/src/api/auth/login.rs
pub async fn login<T: GetByEmail>( email: String, password: String ) -> Result<String, NanoServiceError> {
    let user = T::get_by_email(email).await?;
    let outcome = user.verify_password(password)?;
    if outcome {
        Ok(HeaderToken{ unique_id: user.unique_id }.encode()?)
    } else {
        Err(NanoServiceError::new(
            "Invalid password".to_string(),
            NanoServiceErrorStatus::Unauthorized
        ))
    }
}
```

And here, the true power of our approach starts to become apparent. Because all of our errors match and can be converted to HTTP responses, we can exploit the ? operator. We can also appreciate that this function just shows the flow of the logic. We can see that we get the user by email , then verify the password, returning an encoded token if the password verification is successful. We get a bird's eye view of all the steps that our core function takes. And if we want to inspect the specific logic behind a process, we can just click on that function. Trust me, when you system gets more complex, checking user roles, and performing multiple database transactions, the ability to just click on the core API function and see a high-level view of the entire process is a life and time saver.

The only thing left to do for our login is mount our core login function to our server.

## Mounting our core login to our server

When it comes to mounting our core login function to the server, we are going to use basic authentication to send our credentials to the server. Basic Authentication is a simple authentication scheme built into the HTTP protocol. It involves sending the email and password in an HTTP header as a base64-encoded string. This method is used to verify the identity of a user or client trying to access a resource on a server.

Before we define our login API function, we must create a function that extracts the basic auth credentials from the HTTP request sent to the server. Before we write any code, we must add the following dependency:

```toml
// nanoservices/auth/networking/actix_server/Cargo.toml
base64 = "0.22.1"
```

With the base64 dependency, our extract credentials function takes the following outline:

```rust
// nanoservices/auth/networking/actix_server/ // src/extract_auth.rs
use actix_web::HttpRequest;
use glue::errors::{NanoServiceError, NanoServiceErrorStatus};
use base64::{Engine, engine::general_purpose};
#[derive(Debug)]
pub struct Credentials {
    pub email: String,
    pub password: String,
}
pub async fn extract_credentials(req: HttpRequest) -> Result<Credentials, NanoServiceError> {
    . . .
}
```

Inside our extract_credentials function, we extract the credentials from the header and convert the credentials to a string with the code below:

```rust
// nanoservices/auth/networking/actix_server/ // src/extract_auth.rs
let header_value = match req.headers().get("Authorization") {
    Some(auth_header) => auth_header,
    None => return Err(
        NanoServiceError::new(
            "No credentials provided".to_string(),
            NanoServiceErrorStatus::Unauthorized
        )
    ),
};
let encoded = match header_value.to_str() {
    Ok(encoded) => encoded,
    Err(_) => return Err(
        NanoServiceError::new(
            "Invalid credentials".to_string(),
            NanoServiceErrorStatus::Unauthorized
        )
    ),
};
```

Basic auth credentials start with the "Basic " string. If that string is not present, we must conclude that it is not basic auth as shown with the following code:

```rust
// nanoservices/auth/networking/actix_server/ // src/extract_auth.rs
if !encoded.starts_with("Basic ") {
    return Err(
        NanoServiceError::new(
            "Invalid credentials".to_string(),
            NanoServiceErrorStatus::Unauthorized
        )
    )
}
```

We then decode our credentials and convert the result of the decoding to a string with the code below:

```rust
// nanoservices/auth/networking/actix_server/ // src/extract_auth.rs
let base64_credentials = &encoded[6..];
let decoded = general_purpose::STANDARD.decode(
    base64_credentials
).map_err(|e|{ NanoServiceError::new(e.to_string(), NanoServiceErrorStatus::Unauthorized) })?;
let credentials = String::from_utf8(
    decoded
).map_err(|e|{ NanoServiceError::new(e.to_string(), NanoServiceErrorStatus::Unauthorized)} )?;
```

Finally, we extract the password and email and return them with the following code:

```rust
// nanoservices/auth/networking/actix_server/ // src/extract_auth.rs
let parts: Vec<&str> = credentials.splitn(2, ':').collect();
if parts.len() == 2 {
    let email = parts[0];
    let password = parts[1];
    return Ok(Credentials {
        email: email.to_string(),
        password: password.to_string(),
    });
} else {
    return Err(
        NanoServiceError::new("Invalid credentials".to_string(), NanoServiceErrorStatus::Unauthorized)
    )
}
```

With this extraction function, our login API endpoint can be defined with the code below:

```rust
// nanoservices/auth/networking/actix_server/ // src/api/auth/login.rs
use actix_web::HttpResponse;
use auth_dal::users::transactions::get::GetByEmail;
use glue::errors::NanoServiceError;
use crate::extract_auth::extract_credentials;
use auth_core::api::auth::login::login as core_login;
pub async fn login<T: GetByEmail>(req: actix_web::HttpRequest) -> Result<HttpResponse, NanoServiceError> {
    let credentials = extract_credentials(req).await?;
    let token = core_login::<T>( credentials.email, credentials.password ).await?;
    Ok(HttpResponse::Ok().json(token))
}
```

And finally, we mount our login API endpoint to our server with the code below:

```rust
// nanoservices/auth/networking/actix_server/ // src/api/auth/mod.rs
pub mod login;
pub mod logout;
use actix_web::web::{ServiceConfig, get, scope};
use auth_dal::users::descriptors::SqlxPostGresDescriptor;
pub fn auth_factory(app: &mut ServiceConfig) {
    app.service(
        scope("/api/v1/auth")
        .route("login", get().to( login::login::<SqlxPostGresDescriptor>) )
    );
}
```

And our login functionality is now ready, we can now interact with our server to see if it works.

## Interacting with our auth server

Our auth server can create users and login those users. We might as well plug our auth server into our ingress server and make some API calls.

First, we must declare our auth server and the auth data access layer in the ingress Cargo.toml file with the following code:

```toml
# ingress/Cargo.toml
auth_server = { path = "../nanoservices/auth/networking/actix_server", package = "auth_actix_server" }
auth-dal = { path = "../nanoservices/auth/dal", package = "auth-dal" }
```

We can now import these dependencies with the code below:

```rust
// ingress/src/main.rs
use auth_server::api::views_factory as auth_views_factory;
use auth_dal::migrations::run_migrations as run_auth_migrations;
```

We can now run our migrations and attach our auth endpoints to our ingress server. Once we have done this, our main function looks like the following:

```rust
// ingress/src/main.rs
#[tokio::main]
async fn main() -> std::io::Result<()> {
    run_todo_migrations().await;
    run_auth_migrations().await;
    HttpServer::new(|| {
        let cors = Cors::default().allow_any_origin()
            .allow_any_method()
            .allow_any_header();
        App::new()
        .configure(auth_views_factory)
        .configure(to_do_views_factory)
        .wrap(cors)
        .default_service(web::route().to(catch_all))
    })
    .bind("0.0.0.0:8001")?
    .run()
    .await
}
```

Finally, our .env file has the contents below:

```bash
// ingress/.env
TO_DO_DB_URL=postgres://username:password@localhost/to_do
AUTH_DB_URL=postgres://username:password@localhost/to_do
JWT_SECRET=secret
```

Now we can run our server. Both migrations will run, and we can then test our create user API endpoint with the CURL command below:

```bash
curl -X POST http://0.0.0.0:8001/api/v1/users/create \
-H "Content-Type: application/json" \
-d '{ "email": "test@gmail.com", "password": "password" }'
```

Nothing should happen in the terminal, but now our user has been created. Now we can login using the following command:

```bash
curl -u test@gmail.com:password -X GET \
http://0.0.0.0:8001/api/v1/auth/login
```

With this API request, we should get the printout below:

```json
"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1bmlxdWVfaWQiOiI5ODY5ZDllMi02ZjZjLTRlOTEtODM4Mi02YjAyMjMzOTYzOTQifQ.omz535P28m5KEfg6FVpBDMQr7sbidBCl6ju7VusGUl8"
```

And here we have it, our creation of the user and login was successful because we now have an auth token.

Now is the time to lock down all our to-do item calls. If we inspect our create to-do item API endpoint we have the following signature:

```rust
// nanoservices/to_do/networking/actix_server/ // src/api/basic_actions/create.rs
. . .
use glue::{ errors::NanoServiceError, token::HeaderToken };
. . .
pub async fn create<T: SaveOne + GetAll>( token: HeaderToken, body: Json<NewToDoItem>) -> Result<HttpResponse, NanoServiceError> {
    . . .
}
```

We can see that we imported the HeaderToken and put that token as a parameter in the function. As you might recall, putting the HeaderToken as a function parameter fires the middleware that extracts the token from the header and performs the auth checks. To lock down all our other to-do API endpoints by inserting the HeaderToken as function parameters for all our API functions in the basic actions. Locking down all our API endpoints means that our frontend will no longer work as they all require an authentication token. For the section, we will add authentication to our frontend.

## Adding Authentication to our frontend

To make our frontend compliant with authentication, we must carry out the following steps:

Build a login API call

Add tokens to our API calls

Build a login form component

Connect the login form to the app

Seeing as all our steps rely on the user being able to login successfully, we can start with building our login API call.

## Build a login API call

When it comes to mounting our core login function to the server, we are going to use basic authentication to send our credentials. This means that our API call function is going to be separate from the others as there are some extra steps. Seeing that our login API call is the only function that will utilise these steps, we might as well keep the login API call function separate and isolated.

We still want to utilise the functionality of the Url class so we will add the login endpoint to the Url class resulting in the following outline of the Url class:

```typescript
// ingress/frontend/src/api/url.ts
export class Url {
    baseUrl: string;
    create: string;
    getAll: string;
    update: string;
    login: string;
    constructor() {
        this.baseUrl = Url.getBaseUrl();
        this.create = `${this.baseUrl}api/v1/create`;
        this.getAll = `${this.baseUrl}api/v1/get/all`;
        this.update = `${this.baseUrl}api/v1/update`;
        this.login = `${this.baseUrl}api/v1/auth/login`;
    }
    . . .
}
```

Now our Url supports our login, we can define the signature of the login API call with the code:

```typescript
// ingress/frontend/src/api/login.ts
import axios from 'axios';
import { Url } from "./url";
export const login = async ( email: string, password: string ): Promise<string> => {
    . . .
};
```

Here, we take in a email and password, and return the auth token. Inside our login, we encode the password with the code below:

```typescript
// ingress/frontend/src/api/login.ts
const authToken = btoa(`${email}:${password}`);
```

We then send a request to the server for a login with the following code:

```typescript
// ingress/frontend/src/api/login.ts
try {
    const response = await axios({
        method: 'get',
        url: new Url().login,
        headers: {
            'Authorization': `Basic ${authToken}`,
            'Content-Type': 'application/json'
        },
    });
    return response.data;
}
```

If we are successful, we return the token. However, we also handle an error with the following:

```typescript
// ingress/frontend/src/api/login.ts
catch (error) {
    if (axios.isAxiosError(error)) {
        console.error('Login error:', error.response?.data);
    } else {
        console.error('Unexpected error:', error);
    }
    alert('Login failed. Please try again.');
}
```

If there is a failure, we alert the user with the alert function that creates a popup in the browser. We keep the error code vague because we do not want to give hints to someone who is trying to hack a user account.

With our login API call done, and API calls fresh in our minds, it makes sense to now add tokens to our API calls.

## Adding tokens to our API calls

Because we structured the bases of our API calls in one file, we only must go to one file and change one line in each of our call functions. For our example we can look at the postCall function with the code below:

```typescript
// ingress/frontend/src/api/utils.ts
export async function postCall<T, X>( url: string, body: T, expectedResponse: number) {
    . . .
    {
        headers: {
            'Content-Type': 'application/json',
            'token': localStorage.getItem('token')
        },
        validateStatus: () => true
    });
    return handleRequest(response, expectedResponse);
}
```

Here we can see that we have changed the token in the header from a string to localStorage.getItem('token'). This means that we can get the auth token from the local storage of the browser and insert it into our header of the API call. We can make this change with the following functions in the utils.ts file:

deleteCall

putCall

getCall

And now all our API calls have auth tokens. This also means that we can use the local storage to check for a token to work out if we are logged in of not, and we can also store our token in the local storage once our login API call was successful.

Now that we have our token mechanism figured out, and our login API call is defined, we're ready to make our login form component.

## Build a login form component

For our login form, we must take in an email, and a password, send that to our login API call function, and insert the token into local storage if the login was successful.

To achieve this, we import the following:

```typescript
// ingress/frontend/src/components/LoginForm.tsx
import React from 'react';
import '../Login.css';
import { login } from '../api/login';
interface LoginFormProps {
    setToken: (token: string) => void;
}
```

With the preceding imports and interface, we can define our login form component's signature with the code below:

```typescript
// ingress/frontend/src/components/LoginForm.tsx
export const LoginForm: React.FC<LoginFormProps> = ( { setToken } ) => {
    const [email, setEmail] = React.useState<string>("");
    const [password, setPassword] = React.useState<string>("");
    const submitLogin = () => {
        . . .
    };
    const handlePasswordChange = ( e: React.ChangeEvent<HTMLInputElement> ) => {
        setPassword(e.target.value);
    };
    const handleUsernameChange = ( e: React.ChangeEvent<HTMLInputElement> ) => {
        setEmail(e.target.value);
    };
    return (
        . . .
    );
};
```

With this outline, we can update our email and password put into the form and fire the | function when the button on the form is clicked with the following code:

```typescript
// ingress/frontend/src/components/LoginForm.tsx
return (
    <div className="login">
        <h1 className="login-title">Login</h1>
        <input type="text" className="login-input" placeholder="Email" autoFocus onChange={handleUsernameChange} value={email} />
        <input type="password" className="login-input" placeholder="Password" onChange={handlePasswordChange} value={password} />
        <button className="login-button" id="login-button" onClick={submitLogin}>Lets Go</button>
    </div>
);
```

You may have noticed the CSS classes. To avoid bloating this chapter, the CSS has been put in the appendix.

Finally, we handle the button click with the code below:

```typescript
// ingress/frontend/src/components/LoginForm.tsx
const submitLogin = () => {
    login(email, password).then( (response) => {
        setToken(response);
    } ).catch((error) => {
        console.error(error);
    });
};
```

With the call is successful, we call the setToken function that has been passed into the login form component.

We now should define our setToken function in our main application in the next section.

## Connect the login form to the app

In our main application component we will import our login form component with the following code:

```typescript
// ingress/frontend/src/index.tsx
import { LoginForm } from "./components/LoginForm";
```

Then our main application component has the changes below:

```typescript
// ingress/frontend/src/index.tsx
const App = () => {
    . . .
    const [loggedin, setLoggedin] = useState<boolean>( localStorage.getItem('token') !== null );
    function setToken(token: string) {
        localStorage.setItem('token', token);
        setLoggedin(true);
    }
    . . .
    React.useEffect(() => {
        const fetchData = async () => {
            if (wasmReady && loggedin) {
                . . .
            }
        };
        if (wasmReady && loggedin) {
            fetchData();
        }
    }, [wasmReady, loggedin]);
    if (localStorage.getItem('token') === null) {
        return <LoginForm setToken={setToken} />;
    }
    if (!data) {
        . . .
    } else {
        return (
            . . .
        );
    }
};
```

Here we can see that we ensure that the WASM and login is achieved before trying to get the to-do items. We also add the login dependency to the loading of the to-do items, because if the login status changes, we will want to potentially trigger getting the to-do items. If we did not have that as a dependency, then after logging in, we would be stuck on a loading screen because the getting of the to-do items would not be triggered after the change of the login status.

And now our auth system is working and if we try and access our application, we would get the login form displayed in figure 9.2.

Figure 9.2 – Our login form

And there we have it, our login is successful. Our to-do items are still global meaning that any user that has logged in, can access the same items as any other user. In the next chapter we will scope out the user sessions so they timeout, and users can only see to-do items that are tethered to the specific user.

## Summary

In this chapter, we did not just cover the basics of authentication, we added another server to our system. What is more, is that we compiled this server to the main ingress workspace, so both or our servers, SQL scripts for those servers, and the React frontend is in one binary. What is more, our auth server authenticates our user and issues a token for other requests.

Here we are really seeing how our system can scale. These servers can slot in and out of our system. Our authentication system is clearly defined and there is nothing stopping you from taking your authentication system and slotting it into another system on another project. Authentication can explode in complexity. I often find myself creating roles, and permissions, teams, and email processes to verify that the user's email is legitimate. Starting a server specifically just for authentication can seem a little excessive, however, you will be shocked at how quickly the complexity grows. Most people starting projects underestimate the complexity of managing users.

In the next chapter, we will scope our user sessions so they timeout, and users can only see to-do items that are tethered to the specific user. We will also cover how to get the to-do server to communicate with the auth server via HTTP or a memory call in the same binary.

## Questions

Why do we hash passwords before storing the password in the database?

What is the main advantage of a user having a JWT over storing a password?

How does a user store a JWT on the frontend?

What advantages do we get when use a salt when hashing a password?

What is the basic auth protocol?

## Answers

We hash the passwords in the database so if a hacker manages to gain access to the database, the raw passwords are not accessible.

If an attacker manages to obtain a JWT, it does not mean that they have direct access to the user's password. Also, if the tokens are setup to get refreshed, then the access the attacker must items has a limited timeframe.

The JWT can be stored in local HTML storage or cookies.

Using a salt when hashing a password protects against hash table attacks, brute-force attacks, and ensures that passwords are unique.

The basic auth protocol is where we put the username and password in the header of the HTTP request with the password being encoded via base64.

## Appendix

The CSS used for the login form:

```css
body {
    background: #2d343d;
}
.login {
    margin: 20px auto;
    width: 300px;
    padding: 30px 25px;
    background: white;
    border: 1px solid #c4c4c4;
    border-radius: 25px;
}
h1.login-title {
    margin: -28px -25px 25px;
    padding: 15px 25px;
    line-height: 30px;
    font-size: 25px;
    font-weight: 300;
    color: #ADADAD;
    text-align:center;
    background: #f7f7f7;
    border-radius: 25px 25px 0px 0px;
}
.login-input {
    width: 285px;
    height: 50px;
    margin-bottom: 25px;
    padding-left:10px;
    font-size: 15px;
    background: #fff;
    border: 1px solid #ccc;
    border-radius: 4px;
}
.login-input:focus {
    border-color:#6e8095;
    outline: none;
}
.login-button {
    width: 100%;
    height: 50px;
    padding: 0;
    font-size: 20px;
    color: #fff;
    text-align: center;
    background: #f0776c;
    border: 0;
    border-radius: 5px;
    cursor: pointer;
    outline:0;
}
.login-lost {
    text-align:center;
    margin-bottom:0px;
}
.login-lost a {
    color:#666;
    text-decoration:none;
    font-size:13px;
}
.loggedInTitle {
    font-family: "Helvetica Neue";
    color: white;
}
```
