# Chapter 1: Useful Rust Patterns for Web Programming

Knowing the syntax and borrowing rules of Rust can get us building programs. However, unlike dynamic programming languages, Rust has a strict type system. If you do not know how to get creative with traits, this can lead to you creating a lot of excessive code to solve problems. In this chapter, we will cover how to enforce certain parameter checks with traits increasing the flexibility of the parameters of functions accepting structs. We will also explore metaprogramming with macros to reduce the amount of repetitive code we have to write. These macros will also enable us to simplify and effectively communicate with other developers what our code does. We will also utilize the compiler to check the state of structs as they evolve, improving the safety of the program.

In this chapter, we will cover the following topics:

- Verifying with Traits
- Metaprogramming with macros
- Mapping messages with macros
- Configuring our functions with traits
- Checking struct state with the compiler

Once we have covered the main concepts in this chapter, you will be able to achieve more flexibly with your code without having to write loads of excessive code.

## Technical requirements

For this chapter, ensure you have Rust installed. The code examples can be run in any Rust environment.

## Verifying with traits

We can see enums can empower structs so that they can handle multiple types. This can also be translated for any type of function or data structure. However, this can lead to a lot of repetition. Take, for instance, a User struct. Users have a core set of values, such as a username and password. However, they could also have extra functionality based on roles. With users, we must check roles before firing certain processes based on the traits that the user has implemented. We can wrap up structs with traits by creating a simple program that defines users and their roles with the following steps:

We can define our users with the following code:

```rust
struct AdminUser { username: String, password: String } 
struct User { username: String, password: String }
```

We can see in the preceding code that the User and AdminUser structs have the same fields. For this exercise, we just need two different structs to demonstrate the effect traits have on them. Now that our structs are defined, we can move on to our next step, which is creating the traits.

The total traits that we have are create, edit, and delete. We will be implementing these traits in our structs, using them to assign permissions to our users. We can create these three traits with the following code:

```rust
trait CanEdit { fn edit(&self) { println!("admin is editing"); } } 
trait CanCreate { fn create(&self) { println!("admin is creating"); } } 
trait CanDelete { fn delete(&self) { println!("admin is deleting"); } }
```

We can see that the functions for the traits only take in self. We cannot make any references to the fields in the functions to self as we do not know what structs will be implemented. However, we can override functions when we implement the trait to the struct if needed.

If we are to return self, we will need to wrap it in a Box struct, as the compiler will not know the size of the struct being returned. We also must note that the signature of the function (input parameters and return values) must be the same as the original declaration of the trait if we overwrite the function for a struct.

Now that we have defined the traits, we can move on to the next step of implementing the traits to define roles for our user.

With our roles, we can make our admin have every permission and our user only the edit permission. This can be achieved with the following code:

```rust
impl CanDelete for AdminUser {} 
impl CanCreate for AdminUser {} 
impl CanEdit for AdminUser {} 
impl CanEdit for User { fn edit(&self) { println!("A standard user {} is editing", self.username); } }
```

From our previous step, we can remember that all the functions already worked for the admin by printing out that the admin is doing the action. Therefore, we do not have to do anything for the implementation of the traits for the admin. We can also see that we can implement multiple traits for a single struct. This adds a lot of flexibility. In our user implementation of the CanEdit trait, we have overwritten the edit function so that we can have the correct statement printed out.

Now that we have implemented the traits, our user structs have permission in the code to enter scopes that require those traits. We can now build the functions for using these traits in the next step.

We could utilize the functions in the traits by directly running them in the main function on the structs that have implemented them. However, if we do this, we will not see their true power in this exercise. We may also want this standard functionality throughout our program in the future when we span multiple files. The following code shows how we create functions that utilize the traits:

```rust
fn create<T: CanCreate>(user: &T) -> () { user.create(); } 
fn edit<T: CanEdit>(user: &T) -> () { user.edit(); } 
fn delete<T: CanDelete>(user: &T) -> () { user.delete(); }
```

The preceding notation is fairly like the lifetime annotation. We use angle brackets before the input definitions to define the trait we want to accept at T. We then state that we will accept a borrowed struct that has implemented the trait as &T. This means that any struct that implements that specific trait can pass through the function. Because we know what the trait can do, we can then use the functions associated with the trait defined in the parameter. However, because we do not know what struct is going to be passed through, we cannot utilize specific fields. But remember, we can overwrite a trait function to utilize struct fields when we implement the trait for the struct. This might seem rigid, but the process enforces good, isolated, decoupled coding that is safe. For instance, let's say we remove a function from a trait or remove a trait from a struct. The compiler would refuse to compile until all the effects of this change were complete. Thus, we can see that, especially for big systems, Rust is safe, and can save time by reducing the risk of silent bugs.

Now that we have defined the functions, we can use them in the main function in the next step.

We can test to see if all the traits work with the following code:

```rust
fn main() { 
    let admin = AdminUser{ username: "admin".to_string(), password: "password".to_string() }; 
    let user = User{ username: "user".to_string(), password: "password".to_string() }; 
    create(&admin); 
    edit(&admin); 
    edit(&user); 
    delete(&admin); 
}
```

We can see that the functions that accept traits are used just like any other function.

Running the entire program will give us the following printout:

```text
admin is creating 
admin is editing 
A standard user is editing 
admin is deleting
```

In our output, we can see that the overriding of the edit function for the User struct works. We can also add traits. For instance, we could have the following function definition:

```rust
fn cache<T: CanCreate + CanDelete>(user: &T) -> () { . . . }
```

Here, we are saying that the user must have the permission to create and delete entries. This leads me to the opinion that traits are more powerful than classical object inheritance. To illustrate, imagine a game where one function performs damage from one entity to another. You might model a base "player character" class with methods to take and deal damage, then derive Orc, Elf, Human, etc. That seems reasonable. However, what about buildings? Buildings could theoretically take damage, but they are not player characters, and buildings typically do not deal damage. At that point you end up rewriting structures or adding many if/else conditionals to decide which function to call. If instead you define a `DealDamage` trait and a `TakeDamage` trait, a function can accept any types that implement those traits and compose participants with very little friction. In my experience, developers who call Rust "rigid" are often not leveraging traits; when used well, traits make Rust more flexible than many object‑oriented approaches.

We have now learned enough about traits to be productive with web development. From here, traits get even more powerful, and we will be using them for some key parts of our web programming. For instance, several Rust web frameworks have traits that execute before the request is processed by the view/API endpoint. Implementing structs with these traits automatically loads the view function with the result of the trait function. This can be database connections, extraction of tokens from headers, or anything else we wish to work with.

There is one last concept that we need to tackle before we move on to the next chapter, and that is macros.

## Metaprogramming with macros

Metaprogramming can generally be described as a way in which a program can manipulate itself based on certain instructions. Considering the strong typing Rust has, one of the simplest ways in which we can meta program is by using generics. A classic example of demonstrating generics is through coordinates, as follows:

```rust
struct Coordinate <T> { x: T, y: T } 
fn main() { 
    let one = Coordinate{x: 50, y: 50}; 
    let two = Coordinate{x: 500, y: 500}; 
    let three = Coordinate{x: 5.6, y: 5.6}; 
}
```

In the preceding snippet, we can see that the Coordinate struct managed to take in and handle three different types of numbers. We can add even more variance to the Coordinate struct so we can have two different types of numbers in one struct with the following code:

```rust
struct Coordinate <T, X> { x: T, y: X } 
fn main() { 
    let one = Coordinate{x: 50, y: 500}; 
    let two = Coordinate{x: 5.6, y: 500}; 
    let three = Coordinate{x: 5.6, y: 50}; 
}
```

What is happening in the preceding code with generics is that the compiler is looking for all instances where the struct is used, creating structs with the types used when the compilation is running.

### Enhancing Generics with Traits

Generics do not just stop at allowing different data types into a struct or function. We can also set trait requirements for generics as seen below:

```rust
struct DbCacheHandle <T: CanCreate, X: CanDelete> { create_handle: T, delete_handle: X }
```

Here we can see that we can house delete and create handles in a struct. This would enable us to implement database caching handles for a range of different caching approaches and database backends. It is flexible and safe!

Now that we have covered generics, we can move on to the main mechanism of metaprogramming, macros.

Macros enable us to write code that writes code at compilation time. We've already been using macros in our print functions. The ! notation at the end of the function denotes that this is a macro that's being called.

Defining our own macros is a blend of declaring patterns and emitting code at compile time. To demonstrate this, here is a small, safer macro that capitalizes the first character of a mutable String variable:

```rust
macro_rules! capitalize {
    ($var:ident) => {
        if !$var.is_empty() {
            let mut chars: Vec<char> = $var.chars().collect();
            if let Some(first) = chars.get_mut(0) {
                *first = first.to_uppercase().next().unwrap();
            }
            $var = chars.into_iter().collect();
        }
    };
}

fn main() {
    let mut x = String::from("test");
    capitalize!(x);
    println!("{}", x);
}
```

Instead of using the term fn, we use the macro_rules! definition. We then say that $var is an identifier passed into the macro. We check if the string is not empty, convert it into a vector of chars, make the first char uppercase, and then convert it back to a string. Note that we don't return anything in the capitalize macro, and when we call the macro, we don't assign a variable to it. However, when we print the x variable at the end, we can see that it is capitalized. This does not behave like an ordinary function. We also must note that we didn't define a type, instead, we just said it was an identifier; the macro still does checks via traits. Passing an integer into the macro creates the following error:

```text
| capitalize!(32); | ---------------- in this macro invocation | = help: the trait `std::iter::FromIterator<char>` is not implemented for `{integer}`
```

Lifetimes, blocks, literals, paths, metaprogramming, and more, can also be passed instead of an expression. In web development, a lot of the macros are already defined in third-party packages. Because of this, we do not need to write macros ourselves to get a web app up and running. Instead, we will mainly be using derive macros out of the box. However, writing our own macros can be powerful in web programming, for example, when it comes to networking, where I have used my own macros to match messages sent over a socket to the correct function. We will explore this in the next section.

## Mapping Messages with Macros

Throughout the book, we will be using a web framework to match HTTP requests to the right function to be handled. However, there are times where you want to accept a message over a TCP connection and match the handling function based on the message received. (It does not have to be via TCP, I have found this type of macro to be useful for module interfaces or receiving messages via a channel.) In this section, we will implement a simple macro that will reduce the amount of code we need to write when mapping a struct to a function.

To map messages, we initially must define the data contracts that we will be mapping to our functions with the following code:

```rust
#[derive(Debug)] 
pub struct ContractOne { input_data: String, output_data: Option<String> } 
#[derive(Debug)] 
pub struct ContractTwo { input_data: String, output_data: Option<String> }
```

With these data contracts, we have an input field and an output field that is populated when the handling function has finished handling the data contract. Sometimes if there is a possibility that there is an error, then I like to put an optional error field in the contracts which are then populated with an error instead of the output field.

We now need to send one of these contracts over a channel, into a function, or over a network. However, we want the option of sending any contract we want. We can do this by wrapping the contracts in an enum like the code below:

```rust
#[derive(Debug)] 
pub enum ContractHandler { 
    ContractOne(ContractOne), 
    ContractTwo(ContractTwo), 
}
```

Again, if there could be an error around the transporting of the data such as a network error, then we could add an error variant to the enum, and that could be returned to the client. If you want to explore how error handling would look in this scenario, we will cover TCP messaging in chapter 20 where we build our own HTTP protocol on top of TCP connections.

Now that we have our contracts wrapped in an enum that can be sent, we must focus on the handling of these contracts with functions. We have the following functions to handle our contracts:

```rust
fn handle_contract_one(mut contract: ContractOne) -> ContractOne { 
    println!("{}", contract.input_data); 
    contract.output_data = Some("Output Data".to_string()); 
    contract 
} 
fn handle_contract_two(mut contract: ContractTwo) -> ContractTwo { 
    println!("{}", contract.input_data); 
    contract.output_data = Some("Output Data".to_string()); 
    contract 
}
```

While these functions are not exciting, they do simulate a process where the contract is accepted, updated, and then returned.

With our contracts and our handle functions in place, we can now focus on our macro that maps the contract to the handle function. Our macro has the following signature:

```rust
#[macro_export] 
macro_rules! register_contract_routes { 
    ( $handler_enum:ident, $fn_name:ident, $( $contract:ident => $handler_fn:path ),*) => { 
        . . . 
    }; 
}
```

The signature is a little daunting, so we will focus on the hardest expression. Once we understand that, everything else will fall into place. To understand the line $( $contract:ident => $handler_fn:path,*), we must break it down.

$( and ),*: These delimiters indicate a repetition pattern. The $( starts the repetition, and ),* means none or multiple expressions are separated by commas. This allows the macro to accept multiple contract => handler_fn pairs.

$contract:ident: $contract is a metavariable. In macros, metavariables are placeholders that will be matched and substituted with actual code or identifiers when the macro is expanded. :ident specifies that $contract should match an identifier. An identifier in Rust is a name used for variables, functions, structs, etc.

=>: This is a literal token that must appear exactly as is in the macro input. It separates the contract from the handler function path.

$handler_fn:path: $handler_fn is another metavariable. :path specifies that $handler_fn should match a path. In Rust, a path can be a simple identifier (like a function name) or a more complex qualified path (like module::submodule::function). Putting it all together, this line defines a macro rule that matches zero or more pairs of contract => handler_fn.

For the other two inputs, the $handler_enum:ident is the enum that houses the different data contracts, and the $fn_name:ident is the name of the function we want generated for handling all the mapping, as we may want multiple different mappers. We do not want name clashes.

Inside our macro, we define our function, and loop through all of our data contract and function mappings with the following code:

```rust
pub fn $fn_name(received_msg: $handler_enum) -> $handler_enum { 
    match received_msg { 
        msg => match msg { 
            $( $handler_enum::$contract(inner) => { 
                let executed_contract = $handler_fn(inner); 
                return $handler_enum::$contract( executed_contract ) 
            } )* 
        }, 
    } 
}
```

The $(...)* is the loop. We can see that we unwrap the data contract in the match statement, pass the unwrapped contract into the mapped function, and then wrap the response of that mapped function into our enum again, and return it.

We can now call our macro with our handler enum, contracts, and functions with the following code:

```rust
register_contract_routes!( 
    ContractHandler, 
    handle_contract, 
    ContractOne => handle_contract_one, 
    ContractTwo => handle_contract_two 
);
```

Here, we can see that it is much clearer as to what is going on. It is also scalable and repeatable. Our code is also maintainable. If we want to update the way in which the handle function is called, we only must do that update once in the macro as opposed to repeating ourselves around the codebase.

Finally, we can refine a contract, wrap it in the handle enum and call our mapping function with the code below:

```rust
fn main() { 
    let contract_one = ContractOne { 
        input_data: "Contract One".to_string(), 
        output_data: None,
    }; 
    let outcome = handle_contract( 
        ContractHandler::ContractOne(contract_one) 
    ); 
    println!("{:?}", outcome); 
}
```

Running this program will give us the following printout:

```text
Contract One 
ContractOne(ContractOne { input_data: "Contract One", output_data: Some("Output Data") })
```

And there we have it; we have effectively mapped our data contracts to functions using a macro! But let us not stop here. We can combine macros and traits to enable flexible configuration to our functions that we are mapping to.

## Configuring our functions with traits

In a rust web program, we generally have a series of layers and APIs. In terms of layers, we generally have a frontend, backend, and data access layer. With these layers, we can have multiple different frameworks and engines. Throughout the book we will be building out our web application so we can swap these frameworks and engines out.

To be honest, I personally find strong preferences to a particular framework or library to be a little strange. These frameworks should have minimal footprint on your code, with clear separation between the interfaces. With web frameworks, you still must run a server using the web framework. We will solve this in the book, but for a good trick I like to exploit enabling us to swap out different engines and crates is traits with no reference to self.

A trait with no reference to self enables us to configure functions at compile time. To demonstrate how this works, we are going to add another endpoint and data contract that gets a user by name from the database. Our database could be any database. We do not want our code to commit to having a particular data storage engine to run, this would not be flexible. We can start by defining our user struct with the following code:

```rust
#[derive(Debug)] 
pub struct User { name: String, age: u32 }
```

We then define a trait that lays out the signature of getting users with the following code:

```rust
trait GetUsers { 
    fn get_users() -> Vec<User>; 
    fn get_user_by_name(name: &str) -> Option<User> { 
        let users = Self::get_users(); 
        for user in users { 
            if user.name == name { 
                return Some(user); 
            } 
        } 
        None 
    } 
}
```

This is not an optimal implementation, as in normal database queries, we would perform the filter in the database and return the filtered data. However, for our example, this is easier to implement. We must note that the reference to Self in the get_user_by_name function is capitalized, meaning that we are referring to the struct implementing the trait, as opposed to an instance of that struct. Therefore, we do not need to create an instance of our struct to call the get_user_by_name function.

We can then implement our trait for a database engine like the example code below:

```rust
pub struct PostgresDB; 
impl GetUsers for PostgresDB { 
    fn get_users() -> Vec<User> { 
        vec![ 
            User { name: "John".to_string(), age: 30 }, 
            User { name: "Jane".to_string(), age: 25 }, 
        ] 
    } 
}
```

In a working application we would be making a connection to a database and passing in a query. However, databases take a while to setup and this chapter is about useful patterns. Don't worry, throughout the book, we will build out a working data access layer that initially uses files, and then migrates over to fully working postgres database running in Docker.

Now that our database handle works, we must define a contract that carries the input and output data needed for the database operation. This data contract takes the following definition:

```rust
#[derive(Debug)] 
pub struct GetUserContract { 
    pub name: String, 
    pub users: Option<User> 
}
```

We now have everything to define our function handle with the code below:

```rust
fn handle_get_user_by_name<T: GetUsers>(contract: GetUserContract) -> GetUserContract { 
    let user = T::get_user_by_name(&contract.name); 
    GetUserContract { 
        name: contract.name, 
        users: user 
    } 
}
```

Here, we can see that we reference T that must implement the GetUserContract trait. We do not make any reference to T in the parameters of the function; therefore, we do not need to pass in any instances of anything that has implemented the GetUserContract trait.

We then add our contract to our handler with the following code:

```rust
#[derive(Debug)] 
pub enum ContractHandler { 
    ContractOne(ContractOne), 
    ContractTwo(ContractTwo), 
    GetUserContract(GetUserContract) 
}
```

And our macro call now looks like the following:

```rust
register_contract_routes!( 
    ContractHandler, 
    handle_contract, 
    ContractOne => handle_contract_one, 
    ContractTwo => handle_contract_two, 
    GetUserContract => handle_get_user_by_name::<PostgresDB> 
);
```

Here, we can see that we have slotted our Postgres handler into the mapping. We can test to see if this works with the code below:

```rust
fn main() { 
    . . . 
    let get_user_contract = GetUserContract { 
        name: "John".to_string(), 
        users: None 
    }; 
    let outcome = handle_contract( 
        ContractHandler::GetUserContract( 
            get_user_contract 
        ) 
    ); 
    println!("{:?}", outcome); 
}
```

Running our code again will give us the following printout:

```text
Contract One 
ContractOne(ContractOne { input_data: "Contract One", output_data: Some("Output Data") }) 
GetUserContract( 
    GetUserContract { name: "John", users: Some(User { name: "John", age: 30 }) } 
)
```

This gives us a lot of power. We can slot any struct into that handle function if the struct has implemented the GetUsers trait. This struct could be a HTTP request to another server, a file handle, an in-memory store, or another database. We will exploit this approach again throughout the book, as over multiple chapters, we will build out a data access layer that can support multiple different storage engines.

No matter what you are building or what language you are building it in, clear boundaries between your code and external dependencies such as databases, HTTP calls and other IO operations is a must. There is no benefit in embedding it into your code. For instance, at the time of writing this book, I am working for the Rust database SurrealDB. It currently supports its own key value store, RocksDB, memory, WASM, and TiKv. It would make our lives a lot harder if we did not have clear interfaces between these storage engines.

Clean interfaces are nice to work with and powerful. However, with the type checking of Rust, someone (not me), could argue that excessive interfaces could result in a lot of different structs. This is where the type-state pattern comes in.

## Checking Struct State with the Compiler

There are times when a state of a struct and certain processes are just not appropriate anymore. A nice clear example would be a database transaction. While a database transaction is in progress, certain processes are appropriate such as adding another operation into the transaction. You may also want to commit your transaction or roll it back. However, once we have committed our transaction, we cannot roll it back or add another operation to the transaction, we need another transaction for this. Considering this, I would want my compiler to check and ensure that I am not passing in a transaction into certain functions once the transaction is no longer in progress. The compiler will not only give me instant feedback, it will also be safer, as it could be that only certain edge cases could cause a bug, increasing the chance of the bug slipping through tests.

To get this compiler checking, I need a state in my struct. I then need that state to change once certain processes have triggered. We can capture the different state of the transaction with the following code:

```rust
use std::marker::PhantomData; 
struct InProgress; 
struct Committed; 
struct RolledBack; 
struct Transaction<State> { 
    id: u32, 
    state: PhantomData<State>, 
}
```

Here, we can see that we are creating a type of transaction based on one of the structs that is the state of the transaction. The PhantomData is a placeholder that does not actually hold any data, but the compiler can take note of the phantom data when compiling.

We can then implement some functions for the Transaction struct with the code below:

```rust
impl Transaction<InProgress> { 
    fn new(id: u32) -> Self { 
        Transaction { 
            id, 
            state: PhantomData, 
        } 
    } 
    fn commit(self) -> Transaction<Committed> { 
        . . . 
    } 
    fn rollback(self) -> Transaction<RolledBack> { 
        . . . 
    } 
}
```

For our commit function, we have the following code:

```rust
fn commit(self) -> Transaction<Committed> { 
    println!("Transaction {} committed.", self.id); 
    Transaction { 
        id: self.id, 
        state: PhantomData, 
    } 
}
```

Here we can see that the `Transaction<InProgress>` is consumed, and the `Transaction<Committed>` is returned.

For our rollback function we have the definition below:

```rust
fn rollback(self) -> Transaction<RolledBack> { 
    println!("Transaction {} rolled back.", self.id); 
    Transaction { 
        id: self.id, 
        state: PhantomData, 
    } 
}
```

So now we are ready to have a function that only accepts a transaction that is in progress with the following code:

```rust
fn process_in_progress_transaction(tx: &Transaction<InProgress>) { 
    println!( 
        "Processing transaction {} which is in progress.", 
        tx.id 
    ); 
}
```

And here we have it, if a commit or rollback function was called on our transaction, we will not be able to pass the transaction into the function. We can test this with the code below:

```rust
fn main() { 
    let tx = Transaction::<InProgress>::new(1); 
    let tx = tx.commit(); 
    process_in_progress_transaction(&tx); 
}
```

And if we try and run it, we get the following printout:

```text
process_in_progress_transaction(&tx); 
------------------------------- 
^^^ expected `&Transaction<InProgress>`, found `&Transaction<Committed>`
```

That is now only quick feedback, it is also straight to the point of where the problem is. We are told that we have already committed our transaction before trying to pass it into the function. The only thing that has changed is the PhantomData field which is a zero sized struct.

We could use this type-state pattern for the following:

User sessions: when the user has logged out, the state changes. Or the state could be a user role, so only certain parts of the codebase are available to user structs that have a certain role.

Resource Allocation: Here we could ensure that resources are used effectively. We could have states such as Available, InUse, or Released.

Cache management: states such as Empty, Fetching, or Available could dictate what areas of the code are available for a struct handling a cache or access to a cache.

Multi-step processes: States such as Step1, Step2, Step3 etc could force progression of a struct ensuring it could never go back a step or skip a step.

We could keep going on but with this list, we get the picture of how the type-state pattern could be implemented and what problems it could solve.

## Summary

In this chapter we covered some useful patterns that will take our code to the next level. We can enable multiple permissions for a struct using multiple traits. We then scaled our ability to write Rust code with macros where we mapped data contracts with functions to handle them. Finally, we revisited traits to configure functions and implemented the type-state pattern to lock down our struct.

These approaches can help you solve problems in Rust in an elegant, secure, and scalable way. You can use these approaches to solve problems you generally come across outside of web programming too. Throughout the book, we will be revisiting these approaches to solve problems in web programming. Hopefully with these approaches, you can see that Rust is a safe, yet flexible and powerful language. Whenever I am feeling a little too sure of myself, I remind myself that some super smart people came up with the Rust programming language and traits. This always reminds me of how much smarter than me people can be. I hope you too are also in awe of this language.

In the next chapter we will be exploring how powerful the build and package manager tools are in Rust when we design the basics of our web application.

## Questions

- how can we enable multiple permissions for a struct?
- how can we ensure that two permissions are enabled for a struct that can be passed into a function?
- Let us say I wanted a struct that had an ID field and nothing more. I wanted that ID field to be a string in some cases, and an i32 in others. How could I achieve this?
- I want to have a function that calls an IO function in it, but I want the implementation of the IO call to be flexible, enabling other developers to slot in different implementations, how can I do this?
- Let us say I have a struct that passes through a process of steps, and I want to lock down my struct so it cannot be passed into functions outside of the current step. How can I do this?

## Answers

- We can declare a trait per permission. We can then implement multiple traits to a struct.
- We can ensure that two traits are implemented for the struct being passed into the function with <T: TraitA + TraitB>
- We would exploit generics where the generic parameter would map to the ID field, we could then create `StructA<i32>` and `StructA<String>`.
- We can define a trait where the functions for the trait do not have reference to self. This trait can then be passed to the function as a trait parameter with `function_a<T: SomeTrait>()` so we can define the function struct with `function_a<SomeStruct>()`.
- We can implement the type-state pattern.
