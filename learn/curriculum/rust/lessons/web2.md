# Chapter 3: Designing Your Web Application in Rust

We previously explored the syntax of Rust, enabling us to tackle memory management quirks and build data structures. However, just knowing syntax and memory management is not enough to effectively build fully working programs. As any experienced engineer will tell you, structuring code across multiple files and directories is an important aspect of building software.

In this chapter, we will build a basic command-line to-do program. We manage the dependencies needed to build our command-line program with Rust's Cargo. Our program will be structured in a scalable way where we build and manage our own modules, which will be imported into other areas of the program and utilized. We will learn these concepts by building a to-do application spanning multiple files that creates, edits, and deletes to-do tasks. This application will save our multiple to-do application files locally, and we will be able to interact with our application using a command-line interface.

In this chapter, we will cover the following topics:

- Managing a software project with Cargo
- Structuring code through nanoservices
- Creating a task using our data access layer

By the end of this chapter, you will be able to build applications in Rust that can be packaged and used. You will also be able to use third-party packages in your code. As a result, you will be able to build any command-line application that does not require a server or a graphical user interface if you understand the problem you are trying to solve and can break it down into logical chunks.

## Technical requirements

For this chapter, ensure you have Rust installed. The code examples can be run in any Rust environment.

## Managing a software project with Cargo

When compiling Rust code, the Rust compiler goes through similar steps to C and C++. Rust files are compiled into object files using optimization techniques. These object files are then linked with required libraries, and a binary output file is produced. Lucky for us, these processes are hidden unlike C and C++, making it easier. Before we explore the package manager that is Cargo, we must cover how to perform basic Rust compilation.

### Basic Rust Compilation

We should start by building a basic single-file application. To do this, we initially must create a file called hello_world.rs in a local directory to house the Rust code that we are going to compile.

The .rs extension denotes that the file is a Rust file. To be honest, it does not matter what the extension is. If there is viable Rust code written in that file, the compiler will compile and run it without any issues. However, having different extensions might confuse other developers and code editors and cause problems when importing code from other Rust files. So, it is best to use .rs when naming your Rust files.

Inside our hello_world.rs file, we can have the following code:

```rust
fn main() { println!("hello world"); }
```

This is no different from our first code block in the previous chapter. Now that we have defined our entry point in our hello_world.rs file, we can compile the file with the following command:

rustc hello_world.rs

Once the compilation has finished, there will be a binary file in the same directory that can be run. If we compile it on Windows, we can run the binary with the following command:

.\hello_world.exe

If we compile it on Linux or macOS, we can run it with the following command:

./hello_world

Because we only built a simple hello world example, hello world will just be printed out. While this can be useful when building a simple application in one file, it is not recommended for managing programs spanning multiple files. It is not even recommended when relying on third-party modules. This is where Cargo comes in.

Cargo manages everything, including the running, testing, documentation, building/compiling, and third-party module dependencies, out of the box with a few simple commands. We will cover these commands throughout this chapter. From what we have seen when running our hello world example, we must compile the code before we can run it, so we can now move on to the next section where we build a basic application using Cargo.

### Building with Cargo

Building with Cargo is straightforward. All we must do is navigate to a directory where we want to build our project and run the following command:

cargo new web_app

The preceding command builds a basic Cargo Rust project. If we explore this application, we'll see the following structure:

└── web_app ├── Cargo.toml └── src └── main.rs

We can see that there is only one Rust file, and this is the main.rs file that is housed in the src directory. If you open the main.rs file, you will see that this is the same as the file that we made in the previous section. It is an entry point with the default code printing out hello world to the console. The dependencies and metadata for our project are defined in the Cargo.toml file.

The Cargo.toml file also houses version , name and edition fields under the package section. Every few years, Rust releases a new "edition" which may include new syntax and features but maintains backward compatibility. The editions allow you to opt into new features gradually. The name and version are details around the package that you are building.

It is possible for your code to become "stale" if you don't keep your Cargo.toml file up to date. This means you might miss out on performance improvements, new features, or important fixes. Rust has three release channels: stable, beta, and nightly. New features and fixes are first introduced in the nightly channel, tested in beta, and then released in the stable channel every six weeks. You can switch to nightly but it is not recommended as the syntax might change. I have only used nightly when there is a feature really needed that is not stable yet.

To keep your code up to date, you can utilize the following cargo commands:

cargo update: This command updates your dependencies to the latest versions within the specified constraints.

rustup: Rustup is a toolchain installer that makes it easy to switch between Rust versions and channels, ensuring you can always use the latest stable, beta, or nightly versions.

cargo audit: This tool checks your dependencies for security vulnerabilities and other issues, helping you stay on top of potential problems.

If we want to run our program, we do not need to navigate to the main.rs file and run rustc. Instead, we can use Cargo and run it with the following command:

cargo run

When you do this, you will see the project compile and run with the following printout:

Compiling web_app v0.1.0 (/Users/maxwellflitton/Documents/ github/books/Rust-Web_Programming-three/chapter02/web_app) Finished dev [unoptimized + debuginfo] target(s) in 0.15s Running `target/debug/web_app` hello world

Your printout will be slightly different because the base directory will be different. At the bottom, you will see hello world, which is what we expect. We can also see that the printout states that the compilation is unoptimized and that it is running in target/debug/web_app. We can navigate directly to the target/debug/web_app binary and run it just like we did in the previous section, as this is where the binary is stored. The target directory is where the files for compiling, running, and documenting our program reside. If we attach our code to a GitHub repository, we must make sure that the target directory is ignored by GitHub by putting it in the .gitignore file. This should be automatically added by the cargo tool but it is always a good habit to check.

Right now, we are running the unoptimized version of our Rust program. This means that it is slower to run, but quicker to compile. This makes sense as when we are developing, we will be compiling multiple times. However, if we want to run the optimized version, we can use the following command:

cargo run --release

The preceding command gives us the following printout:

Finished release [optimized] target(s) in 2.63s Running `target/release/web_app` hello world

In the preceding output, we can see that our optimized binary is in the target/release/web_app path.

Now that we have got our basic builds done, we can start to use Cargo to utilize third-party crates.

### Shipping crates with Cargo

The rand crate is available for generating random numbers. The documentation for this crate is clear and well-structured with links to structs, traits, and modules. This is not a reflection of the rand crate itself, this is standard documentation for Rust that we will cover in the next section.

To use this crate in our project, we open the Cargo.toml file and add the rand crate under the [dependencies] section, as follows:

[dependencies] rand = "0.8.5"

Now that we've defined our dependency, we can use the rand crate to build a random number generator.

We will not be using a random number generator for our to-do application, however, generating random numbers is a nice easy introduction to using third-party crates using the ThreadRng struct. The ThreadRng struct is a random number generator that generates an f64 value between 0 and 1, which is elaborated on in the rand crate documentation:

```rust
// src/main.rs use rand::prelude::*; fn generate_float(generator: &mut ThreadRng) -> f64 { let placeholder: f64 = generator.gen(); return placeholder * 10.0 } fn main() { let mut rng: ThreadRng = rand::thread_rng(); let random_number = generate_float(&mut rng); println!("{}", random_number); }
```

In the preceding code, we have defined a function called generate_float, which uses the crate to generate and return a float between 0 and 10. Once we've done this, we print the number. The implementation of the rand crate is handled by the rand documentation. Our use statement imports the rand crate. When using the rand create for generating a float, the documentation tells us to import (*) from the rand::prelude module, which simplifies the importing of common items.

You can view the documentation for Rand at the official Rust documentation site.

With a few clicks on the introduction page of the rand documentation, we can dig into the declarations of the structs and functions used in the demonstration.

Now that our code is built, we can run our program with the cargo run command. While Cargo is compiling, it pulls code from the rand crate and compiles that into the binary. We can also note that there is now a cargo.lock file. As we know that cargo.toml is for us to describe our own dependencies, cargo.lock is generated by Cargo and we should not edit it ourselves as it contains exact information about our dependencies.

It is generally a good idea not to commit cargo.lock files to your git if you are building a library. However, if you are building an application, committing the cargo.lock file can help builds being reproducible. I personally avoid committing the cargo.lock files but I have worked on big professional projects that do.

This seamless functionality combined with the easy-to-use documentation shows how Rust improves the development process through significant gains via the development ecosystem as well as the quality of the language. However, all these gains from the documentation are not purely dependent on the third-party libraries; we can also autogenerate our own documentation.

### Documenting with Cargo

Speed and safety are not the only benefits of picking a language such as Rust to develop in. Over the years, the software engineering community keeps learning and growing. Simple things such as good documentation can make or break a project. To demonstrate this, we can define Markdown language within the Rust file with the following code:

```rust
// src/main.rs /// This function generates a float number using a number /// generator passed into the function. /// /// # Arguments /// * generator (&mut ThreadRng): the random number /// generator to generate the random number /// /// # Returns /// (f64): random number between 0 -> 10 fn generate_float(generator: &mut ThreadRng) -> f64 { let placeholder: f64 = generator.gen(); return placeholder * 10.0 }
```

In the preceding code, we've denoted the Markdown with the /// markers. This does two things: it tells other developers who look at the code what the function does and renders Markdown in our autogeneration.

You can also define code tests in the documentation that runs tests when the testing suite is run. This is unique to Rust and ensures that the code in the documentation works. In chapter 12: Unit testing, we will cover how to write and run these tests.

Before we run the document command, we can define and document a basic user struct and a basic user trait to also show how these are documented:

```rust
/// This trait defines the struct to be a user. trait IsUser { /// This function proclaims that the struct is a user. /// /// # Arguments /// None /// /// # Returns /// (bool) true if user, false if not fn is_user() -> bool { return true } } /// This struct defines a user /// /// # Attributes /// * name (String): the name of the user /// * age (i8): the age of the user struct User { name: String, age: i8 }
```

Now that we have documented a range of different structures, we can run the auto-documentation process with the following command:

cargo doc --open

We can see that the documentation is rendered in the same way as the rand crate:

Figure 2.1 – Documentation view of the web app

In the preceding screenshot, we can see that web_app is a crate. We can also see that the documentation of the rand crate is involved (if we look at the bottom left of the screenshot, we can see the rand crate documentation just above our web_app crate documentation). If we click on the User struct, we can see the declaration of the struct, the Markdown that we wrote for the attributes, and the trait implications, as shown in the following figure:

Figure 2.2 – Documentation on struct

It must be noted that in future sections of the book, we will not include Markdown in the code snippets to maintain readability. However, Markdown-documented code is provided in the book's GitHub repo.

Now that we have a well-documented, running Cargo project, we need to be able to pass parameters into it to enable different configurations to run depending on the context.

### Interacting with Cargo

Now that we have our program running and using third-party modules, we can start to interact with our Rust programs through command-line inputs. This is where we start to build out our web application for most of the book. We will keep everything isolated, giving us maximum flexibility. Initially, our project will take the following file structure:

├── Cargo.toml └── to_do └── core ├── Cargo.toml └── src └── main.rs

Here, we have defined our to_do service. Inside the to_do service, we have a core module. The core module is where we run our core logic which is handling the creation of to-do items. Later, we will build out the data access module and networking modules for the to_do service.

Before we define our root cargo file, we must have a basic skeleton cargo file with the following contents:

`// File: ./to_do/core/Cargo.toml` [package] name = "core" version = "0.1.0" edition = "2021" [dependencies]

Because we have multiple Rust crates in our project, it will help to define where the Rust projects are in the repository, in workspaces. We can define our workspaces with the following code:

## File: ./Cargo.toml [workspace] resolver = "2" members = [ "to_do/core" ]

With these workspaces, when we run our cargo commands, these commands will be run on all our workspaces. We can also see that we have introduced a resolver.

A resolver in Rust's Cargo is a configuration setting that determines which version of Cargo's dependency resolution algorithm to use. It dictates how Cargo handles the dependencies, optional dependencies, and feature sets of a project. By specifying the resolver, such as resolver="2", in the Cargo.toml file, developers can take advantage of the improved dependency resolution algorithm introduced in Cargo 1.51. This ensures more accurate and flexible handling of dependencies, leading to consistent and predictable builds across different environments. Defining the resolver helps maintain stability, compatibility, and clarity in how dependencies are managed throughout the project's lifecycle.

Now, let us get some basic interaction with our system where we can pass in commands to our program using the command line. To enable our program to have some flexibility depending on the context, we need to be able to pass parameters into our program and keep track of the parameters in which the program is running. We can do this using the std (standard library) identifier with the code below in our file:

```rust
//! File: to_do/core/src/main.rs use std::env; fn main() { let args: Vec<String> = env::args().collect(); println!("{:?}", args); }
```

In the preceding code, we can see that we collect the arguments passed into the program into a vector and then print out the arguments in debug mode. Let us run the following command to run our core with the arguments ["one", "two", "three"]:

cargo run -p core one two three

Running the preceding command gives the following printout:

["target/debug/core", "one", "two", "three"]

Here, we can see that our args vector has the arguments that we passed in. This is not surprising as many other languages also accept arguments passed into the program via the command line. We must note as well that the path to the binary is also included.

I am using the project named core, hence the target/debug/core path.

We can also see from the command-line arguments that we are running in debug mode. Let us try to run a release version of our program with the following command:

cargo run -p core --release one two three

We would receive the following printout:

["target/release/core", "one", "two", "three"]

From the preceding output, we can see that --release is not in our vector. However, this does give us some extra functionality to play with.

So far, we have passed in some basic commands; however, this is not helpful or scalable. There would also be a lot of boilerplate code written for us to implement help guides for users. To scale our command-line interface, we can lean on the clap crate to handle arguments passed into the program, with the following dependency:

## File: to_do/core/Cargo.toml [dependencies] clap = { version = "4.5.2", features = ["derive"] }

At the time of writing this, the clap crate requires the rustc 1.74 minimum. If you need to update your Rust version, run the rustup update command.

To flesh out our understanding of command-line interfaces, we can develop a toy application that merely takes in a few commands and prints them out. To do this, we must import what we need from the clap crate in the main.rs file with the following code:

```rust
//! File: to_do/core/src/main.rs use clap::Parser;
```

We now get to define the parameters that need to be passed into our program with the code below:

```rust
//! File: to_do/core/src/main.rs /// Simple program for booking #[derive(Parser, Debug)] #[command(version, about, long_about = None)] struct Args { /// first name of user #[arg(short, long)] first_name: String, /// last name of user #[arg(short, long)] last_name: String, /// age of the user #[arg(short, long, default_value_t = 1)] age: u8, }
```

Here we need to keep the comments in the code above, as these comments will be shown in the help menu in the terminal. Here, we can see that the first name and last name are strings, and we also require an age, but we have a default of one. To consume and print out these arguments, we only need the following code:

fn main() { let args = Args::parse(); println!("{:?}", args.first_name); println!("{:?}", args.last_name); println!("{:?}", args.age); }

Now that we have a working example of how to pass command-line arguments, we can interact with our application to see how it displays by running the following command:

cargo run -p core -- --help

The middle -- before --help tells Cargo to pass all the arguments after -- into clap as opposed to cargo.

The preceding command will give us the following printout:

Simple program for booking Usage: core [OPTIONS] --first-name &lt;FIRST_NAME&gt; --last-name &lt;LAST_NAME&gt; Options: -f, --first-name &lt;FIRST_NAME&gt; first name of user -l, --last-name &lt;LAST_NAME&gt; last name of user -a, --age `&lt;AGE&gt;` age of the user [default: 1] -h, --help Print help -V, --version Print version

In the preceding output, we can see how to directly interact with our compiled binary file. We also have a nice help menu.

To interact with Cargo, we need to run the following command to run our core while passing in arguments via clap:

cargo run -p core -- --first-name maxwell --last-name flitton --age 34

The preceding command will give the following printout:

"maxwell" "flitton" 34

We can see that the parsing works as we have two strings and an integer. The reason why crates such as clap are useful is that they are essentially self-documenting. Developers can look at the code and know what arguments are being accepted and view the metadata around them. Users can get help on the inputs by merely passing in the help parameter. This approach reduces the risk of the documentation becoming outdated as it is embedded in the code that executes it. If you accept command-line arguments, it is advised that you use a crate such as clap for this purpose.

Now that we have explored structuring our command-line interface so it can scale, we can investigate structuring our code over multiple files to scale it in the next section.

## Structuring code through Nanoservices

We can now begin our journey of building a web application. In the rest of this chapter, we will not touch a web framework or build an HTTP listener. This will happen in the next chapter. However, we will construct a to-do module that will interact with a JSON file. It is going to be structured in such a way that it can be inserted into any web application that we build with minimal effort. This to-do module will enable us to create, update, and delete to-do items. We will then interact with this via the command line. The process here is to explore how to build well-structured code that will scale and be flexible. To gain an understanding of this, we will break down the building of this module into the following chunks:

Building to-do structs

Managing structs with an API

Storing tasks with our data access layer using a JSON file (we will replace this with a proper database in later chapters).

Before we start tackling these steps, we are going to investigate the overall structure of our application. Our to_do directory is going to house several cargo projects. These cargo projects will build on each other to create a to-do service, which can be a server that runs by itself as a microservice in a cluster. However, because our service comprises of isolated cargo projects, we will also be able to compile our to_do service directly into another service as a module. I personally have used this approach when building medical simulation software for the German government. I called the approach "nanoservices", due to being able to compile my entire cluster into one binary. But, if I wanted to, any of those nanoservices could evolve into a microservice if I needed it to. This also simplified and sped up the local builds when developing locally.

To see how nanoservices are built, we can start by building out our to-do structs.

### Building to-do structs

Right now, we only have two structs for to-do items: ones that are waiting to be done and others that are already done. However, we might want to introduce other categories. For instance, we could add a backlog category, or an on-hold task for tasks that have been started but for one reason or another are blocked.

To avoid mistakes and repetitive code, we can build a Base struct and have that be utilized by other structs. The Base struct houses common fields and functions. An alteration of the Base struct will propagate to all other to-do structs. We will also need to define the type of to-do item. We could hardcode in strings for pending and done; however, this is not scalable and is also error prone. To avoid this, we will use an enum to classify and define the presentation of the type of to-do item.

Before we write any code, we need to define the file structure for our to-do structs. Our whole entire application now needs to have the following outline:

├── Cargo.toml └── to_do └── core ├── Cargo.toml └── src ├── api │ └── mod.rs ├── enums.rs ├── main.rs └── structs ├── base.rs ├── done.rs ├── mod.rs └── pending.rs

Here we can see that we have defined two modules, api and structs in the core of the to_do service. You might note that there is a mod.rs in the api and structs directories. The mod.rs enables us to declare files in the module. For instance, we can declare the files in the structs module with the following code:

```rust
//! File: to_do/core/src/structs/mod.rs mod base; pub mod done; pub mod pending;
```

This means that the done.rs and pending.rs are publicly accessible inside and outside of the structs module. However, the base.rs file is only accessible inside the structs module because there is no pub keyword. This is because we are not expecting the struct in the base.rs file to be exposed outside of the structs module.

We now must declare the api, structs and enums modules in our main.rs file to include our modules in the build with the following code:

```rust
//! File: to_do/core/src/main.rs mod enums; mod structs; mod api;
```

If we compile our code, we will not have any problems. This means that our modules are being compiled into the Rust binary.

Now that we have everything compiling, we need to decide what to work on first. The enums are isolated with no dependencies. In fact, our enum supplies all the structs. Therefore, we will start with our enum in the to_do/core/src/enums.rs file. Our enum is defining the status of the task with the following code:

```rust
//! File: to_do/core/src/enums.rs pub enum TaskStatus { DONE, PENDING }
```

This will work in the code when it comes to defining the status of the task. However, if we want to write to a file or database, we are going to have to build a method to enable our enum to be represented in a string format. We can do this by implementing the Display trait for TaskStatus. First, we must import the format module to implement the Display trait with the following code:

```rust
//! File: to_do/core/src/enums.rs use std::fmt;
```

We can then implement the Display trait for the TaskStatus struct with the following code:

```rust
//! File: to_do/core/src/enums.rs impl fmt::Display for TaskStatus { fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { match &self { &Self::DONE => {write!(f, "DONE")}, &Self::PENDING => {write!(f, "PENDING")} } } }
```

The implementation of the Display trait means that we can use the to_string function for our TaskStatus struct. If we were to use the TaskStatus in the code below:

println!("{}", TaskStatus::DONE); println!("{}", TaskStatus::PENDING); let outcome = TaskStatus::DONE.to_string(); println!("{}", outcome);

We would get the following printout:

DONE PENDING DONE

Here, we can see that when we pass TaskStatus into println!, the Display trait is automatically utilized.

Now that we have our TaskStatus enum defined, we can move onto building out the Base struct, which will be used to construct the other structs. We can define the Base struct in the /to_do/core/src/structs/base.rs file with the following code:

```rust
//! File: to_do/core/src/structs/base.rs use super::super::enums::TaskStatus; pub struct Base { pub title: String, pub status: TaskStatus }
```

From the import at the top of the file, we can access the TaskStatus enum using super::super. We know that the TaskStatus enum is in a higher directory. From this, we can deduce that super gives us access to what is declared in the /to_do/core/src/structs/mod.rs file of the current directory. So, using super::super in a file in the /to_do/core/src/ directory gives us access to what is defined in the to_do/core/src/main.rs file.

Now that we have our Base struct, we can build our Pending and Done structs. This is when we use composition to utilize our Base struct in our /to_do/ core/src/structs/pending.rs file with the following code:

```rust
//! File: core/src/structs/pending.rs use super::base::Base; use super::super::enums::TaskStatus; pub struct Pending { pub super_struct: Base } impl Pending { pub fn new(input_title: &str) -> Self { let base = Base{ title: input_title.to_string(), status: TaskStatus::PENDING }; return Pending{super_struct: base} } }
```

Through the preceding code, we can see that our super_struct field houses our Base struct. We utilize our enum and define the status to be pending. This means that we only must pass the title into the constructor, and we have a struct with a title and a status of pending. Considering this, coding our Done struct should be straightforward in our /to_do/structs/done.rs file with the following code:

```rust
//! File: core/src/structs/done.rs use super::base::Base; use super::super::enums::TaskStatus; pub struct Done { pub super_struct: Base } impl Done { pub fn new(input_title: &str) -> Self { let base = Base { title: input_title.to_string(), status: TaskStatus::DONE }; return Done{super_struct: base} } }
```

We can see that there is not much difference from the Pending struct definition apart from the TaskStatus enum having a DONE status. At this point it might seem excessive to write two separate structs. Right now, we are in discovery phase. If the functionality of our structs increase in the future, our structs are decoupled, meaning we can update the struct functionality without any pain. However, if the complexity does not explode, we could investigate refactoring the structs into one struct. However, we must also note that this approach is not the only legitimate way. Some developers like to start in just one page and branch out when the complexity increases. I personally do not like this approach as I have seen code get highly coupled before the decision is made to break it out, making the refactor harder. No matter what approach you take, if you keep track of the complexity, keep the code decoupled, and can refactor when needed, you are all good.

We have now made a basic module and exposed it to the to_do/core/src/main.rs file. For now, we can write some basic code that will use our module to create a task that is pending and another that is completed. This can be done with the following code:

```rust
//! File: to_do/core/src/main.rs mod enums; mod structs; mod api; use structs::done::Done; use structs::pending::Pending; fn main() { let done = Done::new("shopping"); println!("{}", done.super_struct.title); println!("{}", done.super_struct.status); let pending = Pending::new("laundry"); println!("{}", pending.super_struct.title); println!("{}", pending.super_struct.status); }
```

In the preceding code, we imported our structs and created a pending and done struct. Running our code will give us the following printout:

shopping DONE laundry PENDING

This stops the main.rs file from being overloaded with excessive code. If we were to stack more types of items that can be created, such as the on-hold or backlog items, the code in main.rs would balloon. This is where APIs come in, which we will explore in the next step.

### Managing structs with an API

Right now, we have a core module in our to_do service, and we are now going to build out our api module. The api module essentially is an interface from outside with our core module. A separate api module might seem excessive, however, having an api module gives us ability to have multiple different types of external interactions, as seen in figure 2.3.

Figure 2.3 – The flexibility of an API layer

The api module gives other developers a clean interface to interact with. This clean interface enables processes like a HTTP request, or input from a command line to interact with our core as long as we can get our process to adhere with the API interface. We also get a simple auto documentation benefit. If we must revisit our code, we can just look at the api module to see what interfaces are available as opposed to routing around in the core module. Separating the modules also helps with the separation of concerns. We focus on the core logic in the core module, whereas the api module handles the versioning, deprecating, responses, and messages around the interface. Both modules can quickly balloon in complexity when refactoring and new features get added.

For now, our api module takes the following form:

├── Cargo.toml └── to_do └── core ├── Cargo.toml └── src ├── api │ ├── basic_actions │ │ ├── create.rs │ │ └── mod.rs │ └── mod.rs

Our api module is already defined in our main.rs file. So, all we need is the following declarations to have our create in the api module plugged in:

```rust
//! File: to_do/core/src/api/mod.rs pub mod basic_actions; //! File: to_do/core/src/api/basic_actions/mod.rs pub mod create;
```

We are now ready to create our create API function in the to_do/core/src/api/basic_actions/create.rs file. First, we need the following imports:

```rust
//! File: to_do/core/src/api/basic_actions/create.rs use std::fmt; use crate::structs::{ done::Done, pending::Pending, }; use crate::enums::TaskStatus;
```

We can see that we are going to accept a TaskStatus and we are either going to return a Done or Pending struct. However, these are two different types of structs. To account for this, we are going to wrap these two structs into an enum with the code below:

```rust
//! File: to_do/core/src/api/basic_actions/create.rs pub enum ItemTypes { Pending(Pending), Done(Done), }
```

For our example right now, we are just going to print out our structs. It seems a little excessive to have to perform a match statement to just print out these two structs. Therefore, we are going to implement the Display trait for the ItemTypes enum. We have seen an implementation of the Display trait in the previous chapter. Now is a good time to try and implement the Display trait yourself. If you have attempted this, your code should have a form like the following code:

```rust
//! File: to_do/core/src/api/basic_actions/create.rs impl fmt::Display for ItemTypes { fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { match self { ItemTypes::Pending(pending) => write!( f, "Pending: {}", pending.super_struct.title ), ItemTypes::Done(done) => write!( f, "Done: {}", done.super_struct.title ), } } }
```

We can now build our create function which will accept a status and a title, returning an ItemTypes enum with the code below:

```rust
//! File: to_do/core/src/api/basic_actions/create.rs pub fn create(title: &str, status: TaskStatus) -> ItemTypes { match status { TaskStatus::PENDING => { ItemTypes::Pending(Pending::new(&title)) }, TaskStatus::DONE => { ItemTypes::Done(Done::new(&title)) }, } }
```

Here we can see that if we increase the types of structs that we support, we can just stack them in the create function. We can also see the power of decoupling here. For instance, let us make up a rule that only users of a certain credential can create done tasks, we can perform the check and implement the logic in this create function. The checking of the user is a different concern to the creating and handling of tasks.

We now have everything ready to interact with our create API in our main.rs file as seen with the code below:

```rust
//! File: to_do/core/src/main.rs mod enums; mod structs; mod api; use api::basic_actions::create::create; fn main() { let to_do_item = create( "washing", enums::TaskStatus::PENDING ); println!("{}", to_do_item); }
```

Here, we can see that the code needed to interact with our create API is greatly simplified. Running the code gives us the following printout:

Pending: washing

Now that we have our API working, we need to build out a basic data storage system to store our to-do tasks.

### Storing Tasks with our Data Access Layer

Our data access layer will handle all the interactions between logic and persistent storage. In this section we will cover features allowing us to selectively compile the type of storage we want. We will store our tasks in a JSON file, but we will add support for databases in chapter 8. Right now, we just want to get some storage system working so we can continue to develop our application with minimal dependencies.

JSON file considerations

Choosing to store our tasks in a JSON file is far from optimal. Every time we perform an operation on a file, we must make calls to the operating system checking to see if the file exists, if the user has the correct permissions, and if the file is not locked. Async reads and writes are not widely supported, we cannot index, meaning we cannot select rows to load. This means that we must load the entire data into memory before we can perform an operation. JSON files are rarely a substitute for a proper database.

Our data access layer module takes the following form:

└── to_do ├── core │ ├── Cargo.toml │ └── src │ . . . └── dal ├── Cargo.toml └── src ├── json_file.rs └── lib.rs

Here we can see that we have a lib.rs file in place of a main.rs. This means that the cargo project is a library and our entry point into that library is the lib.rs file. We will compile our data access layer (dal) library as a dependency into our core module to be used when we need it.

Now that we have the dal workspace, we need to declare the workspace in the root Cargo.toml with the following code:

## File: ./Cargo.toml [workspace] resolver = "2" members = [ "to_do/core", "to_do/dal" ]

Before we write any dal code, we need to define the Cargo.toml for the dal library which takes the following form:

## File: to_do/dal/Cargo.toml [package] name = "dal" version = "0.1.0" edition = "2021" [features] json-file = ["serde_json", "serde"] [dependencies] serde_json = { version="1.0.114", optional = true } serde = { version="1.0.197", optional = true }

Here, we have added a features section with the feature json-file. The serde dependencies are utilised for the serialisation of data. We can also see that both of our serde dependencies are optional. This means that the serde dependencies will only be compiled if directed to and are not compiled into the library by default. Our json-file feature declares that it needs both of our serde dependencies if the library is compiled with the json-file feature. This gives us fine grained control over what is compiled. For instance, in chapter 9, we will add Postgres support to our dal library. Based on the feature, our core module will be able to either just compile the code and dependencies needed for JSON file storage, or for Postgres, or both! With all the strict type checking, we can be brave on just compiling multiple different libraries with different features. If everything does not match up, then Rust will refuse to compile, so we don't have to stay up at night worrying if the interface for one feature is the same as other features.

We now need to declare our JSON file code in our lib.rs file with the code below:

```rust
//! File: to_do/dal/src/lib.rs #[cfg(feature = "json-file")] pub mod json_file;
```

Here, what we are doing is compiling the declaration of our json_file module if the json-file feature is activated.

We can declare our dal library in our core Cargo.toml file with the following code:

## File: to_do/core/Cargo.toml [dependencies] dal = { path = "../dal", features = ["json-file"] }

If we run a build, we can see that our entire system compiles with no problems, meaning that we can now access our JSON file storage functions within our core module! Now that everything is plugged in, we can build our functions that interact with out JSON file for storage.

Because we do not want to commit much effort to a placeholder method of reading and writing to a JSON file, we will have all our basic functions in the json_file.rs file of the dal library. In this file, we initially need the following traits and structs:

```rust
//! File: to_do/dal/src/json_file.rs use serde::{de::DeserializeOwned, Serialize}; use std::collections::HashMap; use std::env; use std::fs::{OpenOptions, File}; use std::io::{Read, Write};
```

We will cover these uses as and when we define our functions. For our JSON data access, we will just build a total of 6 functions. This makes the flow easy to handle and read, and we do not plan on any complexity exploding, therefore, there will not be any need for abstractions.

Before we define any functions that interact with a file, we need a function that gets the file handle.

A file handle is an integer value which is used by the operating system to uniquely identify a file which was open at the request of a user.

This function can then be called by all other functions performing an operation on the file. The file handle function can be defined by the code below:

```rust
// File: to_do/dal/src/json_file.rs fn get_handle() -> Result<File, String> { let file_path = env::var("JSON_STORE_PATH").unwrap_or( "./tasks.json".to_string() ); let file = OpenOptions::new() .read(true) .write(true) .create(true) .open(&file_path) .map_err( |e| format!("Error opening file: {}", e) )?; Ok(file) }
```

Here, we can see that we look for an environment variable called JSON_STORE_PATH. If this variable is not defined, we then default to the path being ./tasks.json.

We can now move onto the most basic function which is merely reading the file and returning all the results. This function is defined using the code below:

```rust
// File: to_do/dal/src/json_file.rs pub fn get_all<T: DeserializeOwned>() -> Result<HashMap<String, T>, String> { let mut file = get_handle()?; let mut contents = String::new(); file.read_to_string(&mut contents).map_err( |e| format!("Error reading file: {}", e) )?; let tasks: HashMap<String, T> = serde_json::from_str( &contents).map_err(|e| format!("Error parsing JSON: {}", e) )?; Ok(tasks) }
```

Here we are starting to utilize the power of generics and traits. If the struct has implemented the DeserialiseOwned trait, a hashmap of those structs mapped by keys as strings, can be loaded and returned from the file. Note that we map errors to strings and that we get to use the ? operator because the error type matches. We will investigate custom error types in chapter 4 as we will be able to construct custom HTTP responses based on the error.

We also want to save all the tasks that we have to the file. This is where we pass in the hashmap into the function and write it to the file with the following code to save all our to-do items:

```rust
// File: to_do/dal/src/json_file.rs pub fn save_all<T: Serialize>(tasks: &HashMap<String, T>) -> Result<(), String> { let mut file = get_handle()?; let json = serde_json::to_string_pretty(tasks).map_err( |e| format!("Error serializing JSON: {}", e) )?; file.write_all(json.as_bytes()).map_err( |e| format!("Error writing file: {}", e) )?; Ok(()) }
```

We can see that the save function has the same use of generics that the get function has. We now have everything we need. However, our application will be doing a lot of operations on a single task. We can define these operations below, so we do not have to repeat them anywhere else in the application. These functions are slightly repetitive with some variance. It would be a good idea to try and complete these functions yourself. If you do, then hopefully they look like the functions below:

```rust
// File: to_do/dal/src/json_file.rs pub fn get_one<T: DeserializeOwned + Clone>(id: &str) -> Result<T, String> { let tasks = get_all::<T>()?; match tasks.get(id) { Some(t) => Ok(t.clone()), None => Err(format!("Task with id {} not found", id)) } } pub fn save_one<T>(id: &str, task: &T) -> Result<(), String> where T: Serialize + DeserializeOwned + Clone, { let mut tasks = get_all::<T>().unwrap_or_else( |_| HashMap::new() ); tasks.insert(id.to_string(), task.clone()); save_all(&tasks) } pub fn delete_one<T>(id: &str) -> Result<(), String> where T: Serialize + DeserializeOwned + Clone, { let mut tasks = get_all::<T>().unwrap_or( HashMap::new() ); tasks.remove(id); save_all(&tasks) }
```

Our data access layer is now fully defined. We can move onto utilizing the data access layer in the core.

### Creating a Task using our DAL

We now shift our focus back to creating a task. When creating this task, we need to store the newly created task using the data access layer. Storing the task in JSON will involve some serialization, therefore, we need to ensure that we have the serde crate installed, and our Cargo.toml file should have the following dependencies:

## File: to_do/core/Cargo.toml [dependencies] dal = { path = "../dal", features = ["json-file"] } serde = { version = "1.0.197", features = ["derive"] } clap = { version = "4.5.4", features = ["derive"] }

We can now start writing code. We now need to enable our TaskStatus to deserialize and serialize so we can write the task status of the to-do item to the JSON file. We also want our status to construct from a string.

Before we write any additional code to the TaskStatus enum, we need to use the following code:

```rust
//! File: to_do/core/src/enums.rs use serde::{Serialize, Deserialize};
```

We can then then apply the serde traits with the code below:

```rust
// File: to_do/core/src/enums.rs #[derive(Serialize, Deserialize, Debug, Clone)] pub enum TaskStatus { DONE, PENDING }
```

And finally, we can then define a from_string function for our TaskStatus enum with the following code:

```rust
// File: to_do/core/src/enums.rs impl TaskStatus { pub fn from_string(status: &String) -> Result<TaskStatus, String> { match status.to_uppercase().as_str() { "DONE" => Ok(TaskStatus::DONE), "PENDING" => Ok(TaskStatus::PENDING), _ => Err(format!("Invalid status: {}", status)) } } }
```

Now, we only need to slot in our data access function in our create API. In our create API file, we use the function below:

```rust
// File: to_do/core/src/api/basic_actions/create.rs use dal::json_file::save_one;
```

With this data access function, our create API function now has the following form:

```rust
// File: to_do/core/src/api/basic_actions/create.rs pub fn create(title: &str, status: TaskStatus) -> Result<ItemTypes, String> { let _ = save_one(&title.to_string(), &status)?; match &status { TaskStatus::PENDING => { Ok(ItemTypes::Pending(Pending::new(&title))) }, TaskStatus::DONE => { Ok(ItemTypes::Done(Done::new(&title))) }, } }
```

We can see how flexible our storage implementation is. When we change out our storage later, it will not be a headache. This means that we have low technical debt right now. Even though our implementation has taken longer than just bashing out all our logic into one file, future refactors will be easier and quicker.

We can now rewrite our main.rs file to use everything that we have done so far. First things first, our use statements are defined by the code below:

```rust
//! File: to_do/core/src/main.rs mod enums; mod structs; mod api; use api::basic_actions::create::create; use crate::enums::TaskStatus; use clap::Parser;
```

With these use statements defined, our main function is now smaller, as seen in the following code:

```rust
// File: to_do/core/src/main.rs #[derive(Parser, Debug)] #[command(version, about, long_about = None)] struct Args { #[arg(short, long)] title: String, #[arg(short, long)] status: String, } fn main() -> Result<(), String> { let args = Args::parse(); let status_enum = TaskStatus::from_string( &args.status )?; let to_do_item = create( &args.title, status_enum )?; println!("{}", to_do_item); Ok(()) }
```

We can see that our main function returns the same result signature, the TaskStatus::from_string and create functions can use the ? operator. Now we can test to see if our system works with the following terminal commands:

cargo run -- --title coding --status pending cargo run -- --title washing --status done

If we inspect our JSON file after those commands, we should have the form below:

{ "washing": "DONE", "coding": "PENDING" }

And here we have it, a basic terminal command that stores tasks in a JSON file!

## Summary

What we have essentially done in this chapter is build a program that accepts some command-line inputs, interacts with a file, and edits it depending on the command and data from that file. The data is simple: a title and a status. We could have done this all in the main function with multiple match statements and if, else if, and else blocks. However, this is not scalable. Instead, we built structs that inherited other structs, which then implemented traits.

We also defined the structure of our to-do service with workspaces. Here, we enabled our service to have multiple layers so we can swap approaches to storage easily. We also explored how to structure a basic service with a main and data access layer.

In chapter 5, we will build out our service on how to handle HTTP requests using a web framework. However, before we handle HTTP requests, we will explore async in detail, as we use async rust to handle incoming HTTP requests. Chapter 4 is an isolated chapter, meaning that you do not need to read the async chapter if you just want to get to grips with web programming. In this case, you can skip to chapter 5.

## Questions

- What does the --release argument in Cargo do when added to a build and run?
- How can you enable an enum or struct to be passed into the println! Macro?
- In Nanoservices, how does a basic service help keep technical debt down?
- How does returning the same error type in functions reduce the amount of code we write?
- How do we point to another cargo project so we can compile that cargo project into another cargo project?
- If we have a cargo project, how can we choose to include an optional dependency to be compiled into our project?

## Answers

- In a build, the --release argument compiles the program in an optimized way as opposed to a debug compilation. In a run, the --release argument points to an optimized binary as opposed to the debug binary. An optimized binary takes longer to compile but will run at a faster pace.
- If you implement the fmt::Display trait for the struct or enum where you have to write a fmt function that calls the write! macro. This fmt function will be called when we pass the struct or enum that has implemented the fmt::Display trait.
- A basic service in Nanoservices keeps technical debt down by having different layers such as a core, and data access layer. These layers keep concepts isolated so they can be slotted in and out. For instance, with features, we can easily swap out the storage type when needed because the storage logic is defined in the data access layer.
- If we have two functions that return the signatures Result<(), String> and Result<i32, String>, we can use the ? operator to avoid match statements because both of them have the same error type.
- Under [dependencies] in the Cargo.toml file, we can point to the path of the cargo project that we want to add using syntax such as the following: dal = {path = "../dal"}.
- We can opt in and out of dependencies with features. We can set optional to true for the dependency, and then declare that optional dependency for a feature.
