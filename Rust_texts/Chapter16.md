# Fundamentals of Asynchronous Programming: Async, Await, Futures, and Streams

Many operations we ask the computer to do can take a while to finish. It would be nice if we could do something else while we are waiting for those long-running processes to complete. Modern computers offer two techniques for working on more than one operation at a time: parallelism and concurrency. Once we start writing programs that involve parallel or concurrent operations, though, we quickly encounter new challenges inherent to asynchronous programming, where operations may not finish sequentially in the order they were started.

This chapter builds on Chapter 16’s use of threads for parallelism and concurrency by introducing an alternative approach to asynchronous programming: Rust’s Futures, Streams, the `async` and `await` syntax that supports them, and the tools for managing and coordinating between asynchronous operations.

Let’s consider an example. Say you’re exporting a video you’ve created of a family celebration, an operation that could take anywhere from minutes to hours. The video export will use as much CPU and GPU power as it can. If you had only one CPU core and your operating system didn’t pause that export until it completed—that is, if it executed the export synchronously—you couldn’t do anything else on your computer while that task was running. That would be a pretty frustrating experience. Fortunately, your computer’s operating system can, and does, invisibly interrupt the export often enough to let you get other work done simultaneously.

Now say you’re downloading a video shared by someone else, which can also take a while but does not take up as much CPU time. In this case, the CPU has to wait for data to arrive from the network. While you can start reading the data once it starts to arrive, it might take some time for all of it to show up. Even once the data is all present, if the video is quite large, it could take at least a second or two to load it all. That might not sound like much, but it’s a very long time for a modern processor, which can perform billions of operations every second. Again, your operating system will invisibly interrupt your program to allow the CPU to perform other work while waiting for the network call to finish.

The video export is an example of a CPU-bound or compute-bound operation. It’s limited by the computer’s potential data processing speed within the CPU or GPU, and how much of that speed it can dedicate to the operation. The video download is an example of an IO-bound operation, because it’s limited by the speed of the computer’s input and output; it can only go as fast as the data can be sent across the network.

In both of these examples, the operating system’s invisible interrupts provide a form of concurrency. That concurrency happens only at the level of the entire program, though: the operating system interrupts one program to let other programs get work done. In many cases, because we understand our programs at a much more granular level than the operating system does, we can spot opportunities for concurrency that the operating system can’t see.

For example, if we’re building a tool to manage file downloads, we should be able to write our program so that starting one download won’t lock up the UI, and users should be able to start multiple downloads at the same time. Many operating system APIs for interacting with the network are blocking, though; that is, they block the program’s progress until the data they’re processing is completely ready.

Note: This is how most function calls work, if you think about it. However, the term blocking is usually reserved for function calls that interact with files, the network, or other resources on the computer, because those are the cases where an individual program would benefit from the operation being non-blocking.

We could avoid blocking our main thread by spawning a dedicated thread to download each file. However, the overhead of those threads would eventually become a problem. It would be preferable if the call didn’t block in the first place. It would also be better if we could write in the same direct style we use in blocking code, similar to this:

```rust
let data = fetch_data_from(url).await;
println!("{data}");
```

That is exactly what Rust’s async (short for asynchronous) abstraction gives us. In this chapter, you’ll learn all about async as we cover the following topics:

• How to use Rust’s `async` and `await` syntax
• How to use the async model to solve some of the same challenges we looked at in Chapter 16
• How multithreading and async provide complementary solutions, that you can combine in many cases

## Parallelism and Concurrency

We’ve treated parallelism and concurrency as mostly interchangeable so far. Now we need to distinguish between them more precisely, because the differences will show up as we start working.

Consider the different ways a team could split up work on a software project. You could assign a single member multiple tasks, assign each member one task, or use a mix of the two approaches.

When an individual works on several different tasks before any of them is complete, this is concurrency. Maybe you have two different projects checked out on your computer, and when you get bored or stuck on one project, you switch to the other. You’re just one person, so you can’t make progress on both tasks at the exact same time, but you can multi-task, making progress on one at a time by switching between them (see Figure 17-1).

Figure 17-1: A concurrent workflow, switching between Task A and Task B

When the team splits up a group of tasks by having each member take one task and work on it alone, this is parallelism. Each person on the team can make progress at the exact same time (see Figure 17-2).

Figure 17-2: A parallel workflow, where work happens on Task A and Task B independently

In both of these workflows, you might have to coordinate between different tasks. Maybe you thought the task assigned to one person was totally independent from everyone else’s work, but it actually requires another person on the team to finish their task first. Some of the work could be done in parallel, but some of it was actually serial: it could only happen in a series, one task after the other, as in Figure 17-3.

Figure 17-3: A partially parallel workflow, where work happens on Task A and Task B independently until Task A3 is blocked on the results of Task B3.

Likewise, you might realize that one of your own tasks depends on another of your tasks. Now your concurrent work has also become serial.

Parallelism and concurrency can intersect with each other, too. If you learn that a colleague is stuck until you finish one of your tasks, you’ll probably focus all your efforts on that task to “unblock” your colleague. You and your coworker are no longer able to work in parallel, and you’re also no longer able to work concurrently on your own tasks.

The same basic dynamics come into play with software and hardware. On a machine with a single CPU core, the CPU can perform only one operation at a time, but it can still work concurrently. Using tools such as threads, processes, and async, the computer can pause one activity and switch to others before eventually cycling back to that first activity again. On a machine with multiple CPU cores, it can also do work in parallel. One core can be performing one task while another core performs a completely unrelated one, and those operations actually happen at the same time.

When working with async in Rust, we’re always dealing with concurrency. Depending on the hardware, the operating system, and the async runtime we are using (more on async runtimes shortly), that concurrency may also use parallelism under the hood.

Now, let’s dive into how async programming in Rust actually works.

## Futures and the Async Syntax

The key elements of asynchronous programming in Rust are futures and Rust’s `async` and `await` keywords.

A future is a value that may not be ready now but will become ready at some point in the future. (This same concept shows up in many languages, sometimes under other names such as task or promise.) Rust provides a `Future` trait as a building block so that different async operations can be implemented with different data structures but with a common interface. In Rust, futures are types that implement the `Future` trait. Each future holds its own information about the progress that has been made and what “ready” means.

You can apply the `async` keyword to blocks and functions to specify that they can be interrupted and resumed. Within an async block or async function, you can use the `await` keyword to await a future (that is, wait for it to become ready). Any point where you await a future within an async block or function is a potential spot for that async block or function to pause and resume. The process of checking with a future to see if its value is available yet is called polling.

Some other languages, such as C# and JavaScript, also use `async` and `await` keywords for async programming. If you’re familiar with those languages, you may notice some significant differences in how Rust does things, including how it handles the syntax. That’s for good reason, as we’ll see!

When writing async Rust, we use the `async` and `await` keywords most of the time. Rust compiles them into equivalent code using the `Future` trait, much as it compiles `for` loops into equivalent code using the `Iterator` trait. Because Rust provides the `Future` trait, though, you can also implement it for your own data types when you need to. Many of the functions we’ll see throughout this chapter return types with their own implementations of `Future`. We’ll return to the definition of the trait at the end of the chapter and dig into more of how it works, but this is enough detail to keep us moving forward.

This may all feel a bit abstract, so let’s write our first async program: a little web scraper. We’ll pass in two URLs from the command line, fetch both of them concurrently, and return the result of whichever one finishes first. This example will have a fair bit of new syntax, but don’t worry—we’ll explain everything you need to know as we go.

## Our First Async Program

To keep the focus of this chapter on learning async rather than juggling parts of the ecosystem, we’ve created the `trpl` crate (`trpl` is short for “The Rust Programming Language”). It re-exports all the types, traits, and functions you’ll need, primarily from the [futures](https://crates.io/crates/futures) and [tokio](https://tokio.rs/) crates. The `futures` crate is an official home for Rust experimentation for async code, and it’s actually where the `Future` trait was originally designed. Tokio is the most widely used async runtime in Rust today, especially for web applications. There are other great runtimes out there, and they may be more suitable for your purposes. We use the `tokio` crate under the hood for `trpl` because it’s well tested and widely used.

In some cases, `trpl` also renames or wraps the original APIs to keep you focused on the details relevant to this chapter. If you want to understand what the crate does, we encourage you to check out [its source code](https://github.com/rust-lang/book/tree/main/packages/trpl). You’ll be able to see what crate each re-export comes from, and we’ve left extensive comments explaining what the crate does.

Create a new binary project named `hello-async` and add the `trpl` crate as a dependency:

```bash
$cargo new hello-async
$cd hello-async
$cargo add trpl
```

Now we can use the various pieces provided by `trpl` to write our first async program. We’ll build a little command line tool that fetches two web pages, pulls the `<title>` element from each, and prints out the title of whichever page finishes that whole process first.

### Defining the page_title Function

Let’s start by writing a function that takes one page URL as a parameter, makes a request to it, and returns the text of the title element (see Listing 17-1).

Filename: src/main.rs

```rust
use trpl::Html;

async fn page_title(url: &str) -> Option<String> {
    let response = trpl::get(url).await;
    let response_text = response.text().await;
    Html::parse(&response_text)
        .select_first("title")
        .map(|title| title.inner_html())
}
```

Listing 17-1: Defining an async function to get the title element from an HTML page

First, we define a function named `page_title` and mark it with the `async` keyword. Then we use the `trpl::get` function to fetch whatever URL is passed in and add the `await` keyword to await the response. To get the text of the response, we call its `text` method, and once again await it with the `await` keyword. Both of these steps are asynchronous. For the `get` function, we have to wait for the server to send back the first part of its response, which will include HTTP headers, cookies, and so on, and can be delivered separately from the response body. Especially if the body is very large, it can take some time for it all to arrive. Because we have to wait for the entirety of the response to arrive, the `text` method is also async.

We have to explicitly await both of these futures, because futures in Rust are lazy: they don’t do anything until you ask them to with the `await` keyword. (In fact, Rust will show a compiler warning if you don’t use a future.) This might remind you of Chapter 13’s discussion of iterators in the section [Processing a Series of Items With Iterators](https://doc.rust-lang.org/book/ch13-02-iterators.html). Iterators do nothing unless you call their `next` method—whether directly or by using `for` loops or methods such as `map` that use `next` under the hood. Likewise, futures do nothing unless you explicitly ask them to.

This laziness allows Rust to avoid running async code until it’s actually needed.

Note: This is different from the behavior we saw in the previous chapter when using `thread::spawn` in [Creating a New Thread with spawn](https://doc.rust-lang.org/book/ch16-01-threads.html#creating-a-new-thread-with-spawn), where the closure we passed to another thread started running immediately. It’s also different from how many other languages approach async. But it’s important for Rust to be able to provide its performance guarantees, just as it is with iterators.

Once we have `response_text`, we can parse it into an instance of the `Html` type using `Html::parse`. Instead of a raw string, we now have a data type we can use to work with the HTML as a richer data structure. In particular, we can use the `select_first` method to find the first instance of a given CSS selector. By passing the string `"title"`, we’ll get the first `<title>` element in the document, if there is one. Because there may not be any matching element, `select_first` returns an `Option<ElementRef>`. Finally, we use the `Option::map` method, which lets us work with the item in the `Option` if it’s present, and do nothing if it isn’t. (We could also use a `match` expression here, but `map` is more idiomatic.) In the body of the function we supply to `map`, we call `inner_html` on the `title` to get its content, which is a `String`. When all is said and done, we have an `Option<String>`.

Notice that Rust’s `await` keyword goes after the expression you’re awaiting, not before it. That is, it’s a postfix keyword. This may differ from what you’re used to if you’ve used `async` in other languages, but in Rust it makes chains of methods much nicer to work with. As a result, we can change the body of `page_title` to chain the `trpl::get` and `text` function calls together with `await` between them, as shown in Listing 17-2.

Filename: src/main.rs

```rust
let response_text = trpl::get(url).await.text().await;
```

Listing 17-2: Chaining with the `await` keyword

With that, we have successfully written our first async function! Before we add some code in `main` to call it, let’s talk a little more about what we’ve written and what it means.

When Rust sees a block marked with the `async` keyword, it compiles it into a unique, anonymous data type that implements the `Future` trait. When Rust sees a function marked with `async`, it compiles it into a non-async function whose body is an async block. An async function’s return type is the type of the anonymous data type the compiler creates for that async block.

Thus, writing `async fn` is equivalent to writing a function that returns a future of the return type. To the compiler, a function definition such as the `async fn page_title` in Listing 17-1 is equivalent to a non-async function defined like this:

```rust
use std::future::Future;
use trpl::Html;

fn page_title(url: &str) -> impl Future<Output = Option<String>> {
    async move {
        let text = trpl::get(url).await.text().await;
        Html::parse(&text)
            .select_first("title")
            .map(|title| title.inner_html())
    }
}
```

Let’s walk through each part of the transformed version:

• It uses the `impl Trait` syntax we discussed back in Chapter 10 in the [“Traits as Parameters”](https://doc.rust-lang.org/book/ch10-02-traits.html#traits-as-parameters) section.
• The returned trait is a `Future` with an associated type of `Output`. Notice that the `Output` type is `Option<String>`, which is the same as the original return type from the `async fn` version of `page_title`.
• All of the code called in the body of the original function is wrapped in an `async move` block. Remember that blocks are expressions. This whole block is the expression returned from the function.
• This async block produces a value with the type `Option<String>`, as just described. That value matches the `Output` type in the return type. This is just like other blocks you have seen.
• The new function body is an `async move` block because of how it uses the `url` parameter. (We’ll talk much more about `async` versus `async move` later in the chapter.)

Now we can call `page_title` in `main`.

### Determining a Single Page’s Title

To start, we’ll just get the title for a single page. In Listing 17-3, we follow the same pattern we used in Chapter 12 to get command line arguments in the [Accepting Command Line Arguments](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html) section. Then we pass the first URL `page_title` and await the result. Because the value produced by the future is an `Option<String>`, we use a `match` expression to print different messages to account for whether the page had a `<title>`.

Filename: src/main.rs

```rust
async fn main() {
    let args: Vec<String> = std::env::args().collect();
    let url = &args[1];
    match page_title(url).await {
        Some(title) => println!("The title for {url} was {title}"),
        None => println!("{url} had no title"),
    }
}
```

Listing 17-3: Calling the `page_title` function from `main` with a user-supplied argument

Unfortunately, this code doesn’t compile. The only place we can use the `await` keyword is in async functions or blocks, and Rust won’t let us mark the special `main` function as `async`.

```text
error[E0752]: `main` function is not allowed to be `async`
 --> src/main.rs:6:1
  |
6 | async fn main() {
  | ^^^^^^^^^^^^^^^ `main` function is not allowed to be `async`
```

The reason `main` can’t be marked `async` is that async code needs a runtime: a Rust crate that manages the details of executing asynchronous code. A program’s `main` function can initialize a runtime, but it’s not a runtime itself. (We’ll see more about why this is the case in a bit.) Every Rust program that executes async code has at least one place where it sets up a runtime and runs the futures.

Most languages that support async bundle a runtime, but Rust does not. Instead, there are many different async runtimes available, each of which makes different tradeoffs suitable to the use case it targets. For example, a high-throughput web server with many CPU cores and a large amount of RAM has very different needs than a microcontroller with a single core, a small amount of RAM, and no heap allocation ability. The crates that provide those runtimes also often supply async versions of common functionality such as file or network I/O.

Here, and throughout the rest of this chapter, we’ll use the `run` function from the `trpl` crate, which takes a future as an argument and runs it to completion. Behind the scenes, calling `run` sets up a runtime that’s used to run the future passed in. Once the future completes, `run` returns whatever value the future produced.

We could pass the future returned by `page_title` directly to `run`, and once it completed, we could match on the resulting `Option<String>`, as we tried to do in Listing 17-3. However, for most of the examples in the chapter (and most async code in the real world), we’ll be doing more than just one async function call, so instead we’ll pass an `async` block and explicitly await the result of the `page_title` call, as in Listing 17-4.

Filename: src/main.rs

```rust
fn main() {
    let args: Vec<String> = std::env::args().collect();

    trpl::run(async {
        let url = &args[1];
        match page_title(url).await {
            Some(title) => println!("The title for {url} was {title}"),
            None => println!("{url} had no title"),
        }
    })
}
```

Listing 17-4: Awaiting an async block with `trpl::run`

When we run this code, we get the behavior we expected initially:

```text
$ cargo run -- https://www.rust-lang.org
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.05s
     Running `target/debug/async_await 'https://www.rust-lang.org'
The title for https://www.rust-lang.org was
            Rust Programming Language
```

Phew—we finally have some working async code! But before we add the code to race the two sites against each other, let’s briefly turn our attention back to how futures work.

Each await point—that is, every place where the code uses the `await` keyword—represents a place where control is handed back to the runtime. To make that work, Rust needs to keep track of the state involved in the async block so that the runtime can kick off some other work and then come back when it’s ready to try advancing the first one again. This is an invisible state machine, as if you’d written an enum like this to save the current state at each await point:

```rust
enum PageTitleFuture<'a> {
    Initial { url: &'a str },
    GetAwaitPoint { url: &'a str },
    TextAwaitPoint { response: trpl::Response },
}
```

Writing the code to transition between each state by hand would be tedious and error-prone, however, especially when you need to add more functionality and more states to the code later. Fortunately, the Rust compiler creates and manages the state machine data structures for async code automatically. The normal borrowing and ownership rules around data structures all still apply, and happily, the compiler also handles checking those for us and provides useful error messages. We’ll work through a few of those later in the chapter.

Ultimately, something has to execute this state machine, and that something is a runtime. (This is why you may come across references to executors when looking into runtimes: an executor is the part of a runtime responsible for executing the async code.)

Now you can see why the compiler stopped us from making `main` itself an async function back in Listing 17-3. If `main` were an async function, something else would need to manage the state machine for whatever future `main` returned, but `main` is the starting point for the program! Instead, we called the `trpl::run` function in `main` to set up a runtime and run the future returned by the `async` block until it is done.

Note: Some runtimes provide macros so you can write an async `main` function. Those macros rewrite `async fn main() { ... }` to be a normal `fn main`, which does the same thing we did by hand in Listing 17-4: call a function that runs a future to completion the way `trpl::run` does.

Now let’s put these pieces together and see how we can write concurrent code.

### Racing Our Two URLs Against Each Other

In Listing 17-5, we call `page_title` with two different URLs passed in from the command line and race them.

Filename: src/main.rs

```rust
use trpl::{Either, Html};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    trpl::run(async {
        let title_fut_1 = page_title(&args[1]);
        let title_fut_2 = page_title(&args[2]);

        let (url, maybe_title) =
            match trpl::race(title_fut_1, title_fut_2).await {
                Either::Left(left) => left,
                Either::Right(right) => right,
            };

        println!("{url} returned first");
        match maybe_title {
            Some(title) => println!("Its page title was: '{title}'"),
            None => println!("It had no title."),
        }
    })
}
```

Listing 17-5: Racing our two URLs against each other

We begin by calling `page_title` for each of the user-supplied URLs. We save the resulting futures as `title_fut_1` and `title_fut_2`. Remember, these don’t do anything yet, because futures are lazy and we haven’t yet awaited them. Then we pass the futures to `trpl::race`, which returns a value to indicate which of the futures passed to it finishes first.

Note: Under the hood, `race` is built on a more general function, `select`, which you will encounter more often in real-world Rust code. A `select` function can do a lot of things that the `trpl::race` function can’t, but it also has some additional complexity that we can skip over for now.

Either future can legitimately “win,” so it doesn’t make sense to return a `Result`. Instead, `race` returns a type we haven’t seen before, `trpl::Either`. The `Either` type is somewhat similar to a `Result` in that it has two cases. Unlike `Result`, though, there is no notion of success or failure baked into `Either`. Instead, it uses `Left` and `Right` to indicate “one or the other”:

```rust
enum Either<A, B> {
    Left(A),
    Right(B),
}
```

The `race` function returns `Left` with the output from the first future argument it finishes first, or `Right` with the output of the second future argument if that one finishes first. This matches the order the arguments appear in when calling the function: the first argument is to the left of the second argument.

We also update `page_title` to return the same URL passed in. That way, if the page that returns first does not have a `<title>` we can resolve, we can still print a meaningful message. With that information available, we wrap up by updating our `println!` output to indicate both which URL finished first and what, if any, the `<title>` is for the web page at that URL.

You have built a small working web scraper now! Pick a couple URLs and run the command line tool. You may discover that some sites are consistently faster than others, while in other cases the faster site varies from run to run. More importantly, you’ve learned the basics of working with futures, so now we can dig deeper into what we can do with async.

## Applying Concurrency with Async

In this section, we’ll apply async to some of the same concurrency challenges we tackled with threads in chapter 16. Because we already talked about a lot of the key ideas there, in this section we’ll focus on what’s different between threads and futures.

In many cases, the APIs for working with concurrency using async are very similar to those for using threads. In other cases, they end up being quite different. Even when the APIs look similar between threads and async, they often have different behavior—and they nearly always have different performance characteristics.

### Creating a New Task with spawn_task

The first operation we tackled in [Creating a New Thread with Spawn](https://doc.rust-lang.org/book/ch16-01-threads.html#creating-a-new-thread-with-spawn) was counting up on two separate threads. Let’s do the same using async. The `trpl` crate supplies a `spawn_task` function that looks very similar to the `thread::spawn` API, and a `sleep` function that is an async version of the `thread::sleep` API. We can use these together to implement the counting example, as shown in Listing 17-6.

Filename: src/main.rs

```rust
use std::time::Duration;

fn main() {
    trpl::run(async {
        trpl::spawn_task(async {
            for i in 1..10 {
                println!("hi number {i} from the first task!");
                trpl::sleep(Duration::from_millis(500)).await;
            }
        });

        for i in 1..5 {
            println!("hi number {i} from the second task!");
            trpl::sleep(Duration::from_millis(500)).await;
        }
    });
}
```

Listing 17-6: Creating a new task to print one thing while the main task prints something else

As our starting point, we set up our `main` function with `trpl::run` so that our top-level function can be async.

Note: From this point forward in the chapter, every example will include this exact same wrapping code with `trpl::run` in `main`, so we’ll often skip it just as we do with `main`. Don’t forget to include it in your code!

Then we write two loops within that block, each containing a `trpl::sleep` call, which waits for half a second (500 milliseconds) before sending the next message. We put one loop in the body of a `trpl::spawn_task` and the other in a top-level `for` loop. We also add an `await` after the `sleep` calls.

This code behaves similarly to the thread-based implementation—including the fact that you may see the messages appear in a different order in your own terminal when you run it:

```text
hi number 1 from the second task!
hi number 1 from the first task!
hi number 2 from the first task!
hi number 2 from the second task!
hi number 3 from the first task!
hi number 3 from the second task!
hi number 4 from the first task!
hi number 4 from the second task!
hi number 5 from the first task!
```

The calls to `thread::sleep` force a thread to stop its execution for a short duration, allowing a different thread to run. The threads will probably take turns, but that isn’t guaranteed: it depends on how your operating system schedules the threads. In this run, the main thread printed first, even though the print statement from the spawned thread appears first in the code. And even though we told the spawned thread to print until `i` is `9`, it only got to `5` before the main thread shut down.

If you run this code and only see output from the main thread, or don’t see any overlap, try increasing the numbers in the ranges to create more opportunities for the operating system to switch between the threads.

### Waiting for All Threads to Finish Using join Handles

The code in Listing 17-6 not only stops the spawned thread prematurely most of the time due to the main thread ending, but because there is no inherent guarantee about the order in which parts of your code on different threads will run, we also can’t guarantee that the spawned thread will get to run at all!

We can fix the problem of the spawned thread not running or of it ending prematurely by saving the return value of `thread::spawn` in a variable. The return type of `thread::spawn` is `JoinHandle<T>`. A `JoinHandle<T>` is an owned value that, when we call the `join` method on it, will wait for its thread to finish. Listing 17-7 shows how to use the `JoinHandle<T>` of the thread we created in Listing 17-6 and how to call `join` to make sure the spawned thread finishes before `main` exits.

Filename: src/main.rs

```rust
let handle = trpl::spawn_task(async {
    for i in 1..10 {
        println!("hi number {i} from the first task!");
        trpl::sleep(Duration::from_millis(500)).await;
    }
});

for i in 1..5 {
    println!("hi number {i} from the second task!");
    trpl::sleep(Duration::from_millis(500)).await;
}

handle.await.unwrap();
```

Listing 17-7: Using `await` with a join handle to run a task to completion

This updated version runs until both loops finish.

```text
hi number 1 from the second task!
hi number 2 from the second task!
hi number 1 from the first task!
hi number 3 from the second task!
hi number 2 from the first task!
hi number 4 from the second task!
hi number 3 from the first task!
hi number 4 from the first task!
hi number 5 from the first task!
hi number 6 from the first task!
hi number 7 from the first task!
hi number 8 from the first task!
hi number 9 from the first task!
```

The two threads continue alternating, but the main thread waits because of the call to `handle.join()` and does not end until the spawned thread is finished.

But let’s see what happens when we instead move `handle.join()` before the `for` loop in `main`, like this:

Filename: src/main.rs

```rust
let handle = trpl::spawn_task(async {
    for i in 1..10 {
        println!("hi number {i} from the first task!");
        trpl::sleep(Duration::from_millis(500)).await;
    }
});

handle.await.unwrap();

for i in 1..5 {
    println!("hi number {i} from the second task!");
    trpl::sleep(Duration::from_millis(500)).await;
}
```

The main thread will wait for the spawned thread to finish and then run its `for` loop, so the output won’t be interleaved anymore, as shown here:

```text
hi number 1 from the spawned thread!
hi number 2 from the spawned thread!
hi number 3 from the spawned thread!
hi number 4 from the spawned thread!
hi number 5 from the spawned thread!
hi number 6 from the spawned thread!
hi number 7 from the spawned thread!
hi number 8 from the spawned thread!
hi number 9 from the spawned thread!
hi number 1 from the main thread!
hi number 2 from the main thread!
hi number 3 from the main thread!
hi number 4 from the main thread!
```

Small details, such as where `join` is called, can affect whether or not your threads run at the same time.

### Using move Closures with Threads

We’ll often use the `move` keyword with closures passed to `thread::spawn` because the closure will then take ownership of the values it uses from the environment, thus transferring ownership of those values from one thread to another. In [“Capturing References or Moving Ownership”](https://doc.rust-lang.org/book/ch13-01-closures.html#capturing-references-or-moving-ownership) in Chapter 13, we discussed `move` in the context of closures. Now we’ll concentrate more on the interaction between `move` and `thread::spawn`.

Notice in Listing 17-6 that the closure we pass to `thread::spawn` takes no arguments: we’re not using any data from the main thread in the spawned thread’s code. To use data from the main thread in the spawned thread, the spawned thread’s closure must capture the values it needs. Listing 17-8 shows an attempt to create a vector in the main thread and use it in the spawned thread. However, this won’t work yet, as you’ll see in a moment.

Filename: src/main.rs

```rust
// [This code does not compile!]
use std::thread;

fn main() {
    let v = vec![1, 2, 3];

    let handle = thread::spawn(|| {
        println!("Here's a vector: {v:?}");
    });

    handle.join().unwrap();
}
```

Listing 17-8: Attempting to use a vector created by the main thread in another thread

The closure uses `v`, so it will capture `v` and make it part of the closure’s environment. Because `thread::spawn` runs this closure in a new thread, we should be able to access `v` inside that new thread. But when we compile this example, we get the following error:

```text
$ cargo run
   Compiling threads v0.1.0 (file:///projects/threads)
error[E0373]: closure may outlive the current function, but it borrows `v`, which is owned by the current function
 --> src/main.rs:6:32
   |
6 |     let handle = thread::spawn(|| {
  |                                ^^ may outlive borrowed value `v`
7 |         println!("Here's a vector: {v:?}");
  |                                     - `v` is borrowed here
  |
note: function requires argument type to outlive `'static`
 --> src/main.rs:6:18
   |
6  |      let handle = thread::spawn(|| {
  |  __________________^
7 | |         println!("Here's a vector: {v:?}");
8 | |     });
  | |______^
help: to force the closure to take ownership of `v` (and any other referenced variables), use the `move` keyword
  |
6 |     let handle = thread::spawn(move || {
  |                                ++++

For more information about this error, try `rustc --explain E0373`.
error: could not compile `threads` (bin "threads") due to 1 previous error
```

Rust infers how to capture `v`, and because `println!` only needs a reference to `v`, the closure tries to borrow `v`. However, there’s a problem: Rust can’t tell how long the spawned thread will run, so it doesn’t know whether the reference to `v` will always be valid.

Listing 17-9 provides a scenario that’s more likely to have a reference to `v` that won’t be valid.

Filename: src/main.rs

```rust
// [This code does not compile!]
use std::thread;

fn main() {
    let v = vec![1, 2, 3];

    let handle = thread::spawn(|| {
        println!("Here's a vector: {v:?}");
    });

    drop(v); // oh no!

    handle.join().unwrap();
}
```

Listing 17-9: A thread with a closure that attempts to capture a reference to `v` from a main thread that drops `v`

If Rust allowed us to run this code, there’s a possibility that the spawned thread would be immediately put in the background without running at all. The spawned thread has a reference to `v` inside, but the main thread immediately drops `v`, using the `drop` function we discussed in Chapter 15. Then, when the spawned thread starts to execute, `v` is no longer valid, so a reference to it is also invalid. Oh no!

To fix the compiler error in Listing 17-8, we can use the error message’s advice:

```text
help: to force the closure to take ownership of `v` (and any other referenced variables), use the `move` keyword
  |
6 |     let handle = thread::spawn(move || {
  |                                ++++
```

By adding the `move` keyword before the closure, we force the closure to take ownership of the values it’s using rather than allowing Rust to infer that it should borrow the values. The modification to Listing 17-8 shown in Listing 17-10 will compile and run as we intend.

Filename: src/main.rs

```rust
use std::thread;

fn main() {
    let v = vec![1, 2, 3];

    let handle = thread::spawn(move || {
        println!("Here's a vector: {v:?}");
    });

    handle.join().unwrap();
}
```

Listing 17-10: Using the `move` keyword to force a closure to take ownership of the values it uses

We might be tempted to try the same thing to fix the code in Listing 17-9 where the main thread called `drop` by using a `move` closure. However, this fix will not work because what Listing 17-9 is trying to do is disallowed for a different reason. If we added `move` to the closure, we would move `v` into the closure’s environment, and we could no longer call `drop` on it in the main thread. We would get this compiler error instead:

```text
$ cargo run
   Compiling threads v0.1.0 (file:///projects/threads)
error[E0382]: use of moved value: `v`
  --> src/main.rs:10:10
   |
4  |     let v = vec![1, 2, 3];
   |         - move occurs because `v` has type `Vec<i32>`, which does not implement the `Copy` trait
5  |
6  |     let handle = thread::spawn(move || {
  |                                ------- value moved into closure here
10 |     drop(v); // oh no!
   |          ^ value used here after move

For more information about this error, try `rustc --explain E0382`.
error: could not compile `threads` (bin "threads") due to 1 previous error
```

Rust’s ownership rules have saved us again! We got an error from the code in Listing 17-8 because Rust was being conservative and only borrowing `v` for the thread, which meant the main thread could theoretically invalidate the spawned thread’s reference. By telling Rust to move ownership of `v` to the spawned thread, we’re guaranteeing to Rust that the main thread won’t use `v` anymore. If we change Listing 17-9 in the same way, we’re then violating the ownership rules when we try to use `v` in the main thread. The `move` keyword overrides Rust’s conservative default of borrowing; it doesn’t let us violate the ownership rules.

Now that we’ve covered what threads are and the methods supplied by the thread API, let’s look at some situations in which we can use threads.

### Counting Up on Two Tasks Using Message Passing

Sharing data between futures will also be familiar: we’ll use message passing again, but this time with async versions of the types and functions. We’ll take a slightly different path than we did in [Using Message Passing to Transfer Data Between Threads](https://doc.rust-lang.org/book/ch16-02-message-passing.html) to illustrate some of the key differences between thread-based and futures-based concurrency. In Listing 17-11, we’ll begin with just a single async block—not spawning a separate task as we spawned a separate thread.

Filename: src/main.rs

```rust
let (tx, mut rx) = trpl::channel();

let val = String::from("hi");
tx.send(val).unwrap();

let received = rx.recv().await.unwrap();
println!("received '{received}'");
```

Listing 17-11: Creating an async channel and assigning the two halves to `tx` and `rx`

Here, we use `trpl::channel`, an async version of the multiple-producer, single-consumer channel API we used with threads back in Chapter 16. The async version of the API is only a little different from the thread-based version: it uses a mutable rather than an immutable receiver `rx`, and its `recv` method produces a future we need to await rather than producing the value directly. We can send messages from the sender to the receiver. Notice that we don’t have to spawn a separate thread or even a task; we merely need to await the `rx.recv` call.

Note: Under the hood, all of this async code runs in an async block in a `trpl::run` call, everything within it can avoid blocking. However, the code outside it will block on the `run` function returning. That’s the whole point of the `trpl::run` function: it lets you choose where to block on some set of async code, and thus where to transition between sync and async code. In most async runtimes, `run` is actually named `block_on` for exactly this reason.

Notice two things about this example. First, the message will arrive right away. Second, although we use a future here, there’s no concurrency yet. Everything in the listing happens in sequence, just as it would if there were no futures involved.

Let’s address the first part by sending a series of messages and sleeping in between them, as shown in Listing 17-12.

Filename: src/main.rs

```rust
let (tx, mut rx) = trpl::channel();

let vals = vec![
    String::from("hi"),
    String::from("from"),
    String::from("the"),
    String::from("future"),
];

for val in vals {
    tx.send(val).unwrap();
    trpl::sleep(Duration::from_millis(500)).await;
}

while let Some(value) = rx.recv().await {
    println!("received '{value}'");
}
```

Listing 17-12: Sending and receiving multiple messages over the async channel and sleeping with an `await` between each message

In addition to sending the messages, we need to receive them. In this case, because we know how many messages are coming in, we could do that manually by calling `rx.recv().await` four times. In the real world, though, we’ll generally be waiting on some unknown number of messages, so we need to keep waiting until we determine that there are no more messages.

In Listing 16-10, we used a `for` loop to process all the items received from a synchronous channel. Rust doesn’t yet have a way to write a `for` loop over an asynchronous series of items, however, so we need to use a loop we haven’t seen before: the `while let` conditional loop. This is the loop version of the `if let` construct we saw back in the section [Concise Control Flow with if let and let else](https://doc.rust-lang.org/book/ch06-03-if-let.html). The loop will continue executing as long as the pattern it specifies continues to match the value.

The `rx.recv` call produces a future, which we await. The runtime will pause the future until it is ready. Once a message arrives, the future will resolve to `Some(message)` as many times as a message arrives. When the channel closes, regardless of whether any messages have arrived, the future will instead resolve to `None` to indicate that there are no more values and thus we should stop polling—that is, stop awaiting.

The `while let` loop pulls all of this together. If the result of calling `rx.recv().await` is `Some(message)`, we get access to the message and we can use it in the loop body, just as we could with `if let`. If the result is `None`, the loop ends. Every time the loop completes, it hits the await point again, so the runtime pauses it again until another message arrives.

The code now successfully sends and receives all of the messages. Unfortunately, there are still a couple of problems. For one thing, the messages do not arrive at half-second intervals. They arrive all at once, 2 seconds (2,000 milliseconds) after we start the program. For another, this program never exits! Instead, it waits forever for new messages. You will need to shut it down using ctrl-c.

Let’s start by examining why the messages come in all at once after the full delay, rather than coming in with delays between each one. Within a given async block, the order in which `await` keywords appear in the code is also the order in which they’re executed when the program runs.

There’s only one async block in Listing 17-12, so everything in it runs linearly. There’s still no concurrency. All the `tx.send` calls happen, interspersed with all of the `trpl::sleep` calls and their associated await points. Only then does the `while let` loop get to go through any of the `await` points on the `recv` calls.

To get the behavior we want, where the sleep delay happens between each message, we need to put the `tx` and `rx` operations in their own async blocks, as shown in Listing 17-13. Then the runtime can execute each of them separately using `trpl::join`, just as in the counting example. Once again, we await the result of calling `trpl::join`, not the individual futures. If we awaited the individual futures in sequence, we would just end up back in a sequential flow—exactly what we’re trying not to do.

Filename: src/main.rs

```rust
let tx_fut = async {
    let vals = vec![
        String::from("hi"),
        String::from("from"),
        String::from("the"),
        String::from("future"),
    ];

    for val in vals {
        tx.send(val).unwrap();
        trpl::sleep(Duration::from_millis(500)).await;
    }
};

let rx_fut = async {
    while let Some(value) = rx.recv().await {
        println!("received '{value}'");
    }
};

trpl::join(tx_fut, rx_fut).await;
```

Listing 17-13: Separating `send` and `recv` into their own `async` blocks and awaiting the futures for those blocks

With the updated code in Listing 17-13, the messages get printed at 500-millisecond intervals, rather than all in a rush after 2 seconds.

The program still never exits, though, because of the way the `while let` loop interacts with `trpl::join`:

• The future returned from `trpl::join` completes only once both futures passed to it have completed.
• The `tx` future completes once it finishes sleeping after sending the last message in `vals`.
• The `rx` future won’t complete until the `while let` loop ends.
• The `while let` loop won’t end until awaiting `rx.recv` produces `None`.
• Awaiting `rx.recv` will return `None` only once the other end of the channel is closed.
• The channel will close only if we call `rx.close` or when the sender side, `tx`, is dropped.
• We don’t call `rx.close` anywhere, and `tx` won’t be dropped until the outermost async block passed to `trpl::run` ends.
• The block can’t end because it is blocked on `trpl::join` completing, which takes us back to the top of this list.

We could manually close `rx` by calling `rx.close` somewhere, but that doesn’t make much sense. Stopping after handling some arbitrary number of messages would make the program shut down, but we could miss messages. We need some other way to make sure that `tx` gets dropped before the end of the function.

Right now, the async block where we send the messages only borrows `tx` because sending a message doesn’t require ownership, but if we could move `tx` into that async block, it would be dropped once that block ends. In the Chapter 13 section [Capturing References or Moving Ownership](https://doc.rust-lang.org/book/ch13-01-closures.html#capturing-references-or-moving-ownership), you learned how to use the `move` keyword with closures, and, as discussed in the Chapter 16 section [Using move Closures with Threads](https://doc.rust-lang.org/book/ch16-01-threads.html#using-move-closures-with-threads), we often need to move data into closures when working with threads. The same basic dynamics apply to async blocks, so the `move` keyword works with async blocks just as it does with closures.

In Listing 17-14, we change the block used to send messages from `async` to `async move`. When we run this version of the code, it shuts down gracefully after the last message is sent and received.

Filename: src/main.rs

```rust
let (tx, mut rx) = trpl::channel();

let tx_fut = async move {
    let vals = vec![
        String::from("hi"),
        String::from("from"),
        String::from("the"),
        String::from("future"),
    ];

    for val in vals {
        tx.send(val).unwrap();
        trpl::sleep(Duration::from_millis(500)).await;
    }
};

let rx_fut = async {
    while let Some(value) = rx.recv().await {
        println!("received '{value}'");
    }
};

trpl::join(tx_fut, rx_fut).await;
```

Listing 17-14: A revision of the code from Listing 17-13 that correctly shuts down when complete

This async channel is also a multiple-producer channel, so we can call `clone` on `tx` if we want to send messages from multiple futures, as shown in Listing 17-15.

Filename: src/main.rs

```rust
let (tx, mut rx) = trpl::channel();

let tx1 = tx.clone();
let tx1_fut = async move {
    let vals = vec![
        String::from("hi"),
        String::from("from"),
        String::from("the"),
        String::from("future"),
    ];

    for val in vals {
        tx1.send(val).unwrap();
        trpl::sleep(Duration::from_millis(500)).await;
    }
};

let rx_fut = async {
    while let Some(value) = rx.recv().await {
        println!("received '{value}'");
    }
};

let tx_fut = async move {
    let vals = vec![
        String::from("more"),
        String::from("messages"),
        String::from("for"),
        String::from("you"),
    ];

    for val in vals {
        tx.send(val).unwrap();
        trpl::sleep(Duration::from_millis(1500)).await;
    }
};

trpl::join3(tx1_fut, tx_fut, rx_fut).await;
```

Listing 17-15: Using multiple producers with async blocks

First, we clone `tx`, creating `tx1` outside the first async block. We move `tx1` into that block just as we did before with `tx`. Then, later, we move the original `tx` into a new async block, where we send more messages on a slightly slower delay. We happen to put this new async block after the async block for receiving messages, but it could go before it just as well. The key is the order in which the futures are awaited, not in which they’re created.

Both of the async blocks for sending messages need to be `async move` blocks so that both `tx` and `tx1` get dropped when those blocks finish. Otherwise, we’ll end up back in the same infinite loop we started out in. Finally, we switch from `trpl::join` to `trpl::join3` to handle the additional future.

Now we see all the messages from both sending futures, and because the sending futures use slightly different delays after sending, the messages are also received at those different intervals.

```text
received 'hi'
received 'more'
received 'from'
received 'messages'
received 'the'
received 'for'
received 'future'
received 'you'
```

This is a good start, but it limits us to just a handful of futures: two with `join`, or three with `join3`. Let’s see how we might work with more futures.

## Working with Any Number of Futures

When we switched from using two futures to three in the previous section, we also had to switch from using `join` to using `join3`. It would be annoying to have to call a different function every time we changed the number of futures we wanted to join. Happily, we have a macro form of `join` to which we can pass an arbitrary number of arguments. It also handles awaiting the futures itself. Thus, we could rewrite the code from Listing 17-15 to use `join!` instead of `join3`, as in Listing 17-16.

Filename: src/main.rs

```rust
trpl::join!(tx1_fut, tx_fut, rx_fut);
```

Listing 17-16: Using `join!` to wait for multiple futures

This is definitely an improvement over swapping between `join` and `join3` and `join4` and so on! However, even this macro form only works when we know the number of futures ahead of time. In real-world Rust, though, pushing futures into a collection and then waiting on some or all the futures of them to complete is a common pattern.

To check all the futures in some collection, we’ll need to iterate over and join on all of them. The `trpl::join_all` function accepts any type that implements the `Iterator` trait, which you learned about back in [The Iterator Trait and the next Method](https://doc.rust-lang.org/book/ch13-02-iterators.html#the-iterator-trait-and-the-next-method) Chapter 13, so it seems like just the ticket. Let’s try putting our futures in a vector and replacing `join!` with `join_all` as shown in Listing 17-17.

```rust
// [This code does not compile!]
let futures = vec![tx1_fut, rx_fut, tx_fut];

trpl::join_all(futures).await;
```

Listing 17-17: Storing anonymous futures in a vector and calling `join_all`

Unfortunately, this code doesn’t compile. Instead, we get this error:

```text
error[E0308]: mismatched types
  --> src/main.rs:45:37
   |
10 |         let tx1_fut = async move {
   |                       ---------- the expected `async` block
...
24 |         let rx_fut = async {
   |                      ----- the found `async` block
...
45 |         let futures = vec![tx1_fut, rx_fut, tx_fut];
   |                                     ^^^^^^ expected `async` block, found a different `async` block
   |
   = note: expected `async` block `{async block@src/main.rs:10:23: 10:33}`
           found `async` block `{async block@src/main.rs:24:22: 24:27}`
   = note: no two async blocks, even if identical, have the same type
   = help: consider pinning your async block and casting it to a trait object
```

This might be surprising. After all, none of the async blocks returns anything, so each one produces a `Future<Output = ()>`. Remember that `Future` is a trait, though, and that the compiler creates a unique enum for each async block. You can’t put two different hand-written structs in a `Vec`, and the same rule applies to the different enums generated by the compiler.

To make this work, we need to use trait objects, just as we did in [“Returning Errors from the run function”](https://doc.rust-lang.org/book/ch12-03-improving-error-handling-and-modularity.html) in Chapter 12. (We’ll cover trait objects in detail in Chapter 18.) Using trait objects lets us treat each of the anonymous futures produced by these types as the same type, because all of them implement the `Future` trait.

Note: In [Using an Enum to Store Multiple Values](https://doc.rust-lang.org/book/ch08-01-vectors.html#using-an-enum-to-store-multiple-types) in Chapter 8, we discussed another way to include multiple types in a `Vec`: using an enum to represent each type that can appear in the vector. We can’t do that here, though. For one thing, we have no way to name the different types, because they are anonymous. For another, the reason we reached for a `Vec` and `join_all` in the first place was to be able to work with a dynamic collection of futures where we only care that they have the same output type.

We start by wrapping each future in the `vec!` in a `Box::new`, as shown in Listing 17-18.

```rust
// [This code does not compile!]
let futures =
    vec![Box::new(tx1_fut), Box::new(rx_fut), Box::new(tx_fut)];

trpl::join_all(futures).await;
```

Listing 17-18: Using `Box::new` to align the types of the futures in a `Vec`

Unfortunately, this code still doesn’t compile. In fact, we get the same basic error we got before for both the second and third `Box::new` calls, as well as new errors referring to the `Unpin` trait. We’ll come back to the `Unpin` errors in a moment. First, let’s fix the type errors on the `Box::new` calls by explicitly annotating the type of the `futures` variable (see Listing 17-19).

```rust
// [This code does not compile!]
let futures: Vec<Box<dyn Future<Output = ()>>> =
    vec![Box::new(tx1_fut), Box::new(rx_fut), Box::new(tx_fut)];
```

Listing 17-19: Fixing the rest of the type mismatch errors by using an explicit type declaration

This type declaration is a little involved, so let’s walk through it:

1. The innermost type is the future itself. We note explicitly that the output of the future is the unit type `()` by writing `Future<Output = ()>`.
2. Then we annotate the trait with `dyn` to mark it as dynamic.
3. The entire trait reference is wrapped in a `Box`.
4. Finally, we state explicitly that `futures` is a `Vec` containing these items.

That already made a big difference. Now when we run the compiler, we get only the errors mentioning `Unpin`. The first part of the message tell us that the first async block (`src/main.rs:8:23: 20:10`) does not implement the `Unpin` trait and suggests using `pin!` or `Box::pin` to resolve it. Later in the chapter, we’ll dig into a few more details about `Pin` and `Unpin`. For the moment, though, we can just follow the compiler’s advice to get unstuck. In Listing 17-20, we start by importing `Pin` from `std::pin`. Next we update the type annotation for `futures`, with a `Pin` wrapping each `Box`. Finally, we use `Box::pin` to pin the futures themselves.

```rust
use std::pin::Pin;

// --snip--

let futures: Vec<Pin<Box<dyn Future<Output = ()>>> =
    vec![Box::pin(tx1_fut), Box::pin(rx_fut), Box::pin(tx_fut)];
```

Listing 17-20: Using `Pin` and `Box::pin` to make the `Vec` type check

If we compile and run this, we finally get the output we hoped for:

```text
received 'hi'
received 'more'
received 'from'
received 'messages'
received 'the'
received 'for'
received 'future'
received 'you'
```

Phew!

There’s a bit more to explore here. For one thing, using `Pin<Box<T>>` adds a small amount of overhead from putting these futures on the heap with `Box`—and we’re only doing that to get the types to line up. We don’t actually need the heap allocation, after all: these futures are local to this particular function. As noted before, `Pin` is itself a wrapper type, so we can get the benefit of having a single type in the `Vec`—the original reason we reached for `Box`—without doing a heap allocation. We can use `Pin` directly with each future, using the `std::pin::pin` macro.

However, we must still be explicit about the type of the pinned reference; otherwise, Rust will still not know to interpret these as dynamic trait objects, which is what we need them to be in the `Vec`. We therefore add `pin` to our list of imports from `std::pin`. Then we can `pin!` each future when we define it and define `futures` as a `Vec` containing pinned mutable references to the dynamic future type, as in Listing 17-21.

```rust
use std::pin::{Pin, pin};

// -- snip --

let tx1_fut = pin!(async move {
    // --snip--
});

let rx_fut = pin!(async {
    // --snip--
});

let tx_fut = pin!(async move {
    // --snip--
});

let futures: Vec<Pin<&mut dyn Future<Output = ()>>> =
    vec![tx1_fut, rx_fut, tx_fut];
```

Listing 17-21: Using `Pin` directly with the `pin!` macro to avoid unnecessary heap allocations

We got this far by ignoring the fact that we might have different `Output` types. For example, in Listing 17-22, the anonymous future for `a` implements `Future<Output = u32>`, the anonymous future for `b` implements `Future<Output = &str>`, and the anonymous future for `c` implements `Future<Output = bool>`.

```rust
let a = async { 1u32 };
let b = async { "Hello!" };
let c = async { true };

let (a_result, b_result, c_result) = trpl::join!(a, b, c);
println!("{a_result}, {b_result}, {c_result}");
```

Listing 17-22: Three futures with distinct types

We can use `trpl::join!` to await them, because it allows us to pass in multiple future types and produces a tuple of those types. We cannot use `trpl::join_all`, because it requires all of the futures passed in to have the same type.

Remember, that error is what got us started on this adventure with `Pin`!

This is a fundamental tradeoff: we can either deal with a dynamic number of futures with `join_all`, as long as they all have the same type, or we can deal with a set number of futures with the `join` functions or the `join!` macro, even if they have different types. This is the same scenario we’d face when working with any other types in Rust. Futures are not special, even though we have some nice syntax for working with them, and that’s a good thing.

### Racing Futures

When we “join” futures with the `join` family of functions and macros, we require all of them to finish before we move on. Sometimes, though, we only need some future from a set to finish before we move on—kind of similar to racing one future against another.

In Listing 17-23, we once again use `trpl::race` to run two futures, `slow` and `fast`, against each other.

```rust
let slow = async {
    println!("'slow' started.");
    trpl::sleep(Duration::from_millis(100)).await;
    println!("'slow' finished.");
};

let fast = async {
    println!("'fast' started.");
    trpl::sleep(Duration::from_millis(50)).await;
    println!("'fast' finished.");
};

trpl::race(slow, fast).await;
```

Listing 17-23: Using `race` to get the result of whichever future finishes first

Each future prints a message when it starts running, pauses for some amount of time by calling and awaiting `sleep`, and then prints another message when it finishes. Then we pass both `slow` and `fast` to `trpl::race` and wait for one of them to finish. (The outcome here isn’t too surprising: `fast` wins.) Unlike when we used `race` back in [“Our First Async Program”](https://doc.rust-lang.org/book/ch17-01-futures-and-syntax.html#our-first-async-program), we just ignore the `Either` instance it returns here, because all of the interesting behavior happens in the body of the async blocks.

Notice that if you flip the order of the arguments to `race`, the order of the “started” messages changes, even though the `fast` future always completes first. That’s because the implementation of this particular `race` function is not fair. It always runs the futures passed in as arguments in the order in which they’re passed. Other implementations are fair and will randomly choose which future to poll first. Regardless of whether the implementation of race we’re using is fair, though, one of the futures will run up to the first `await` in its body before another task can start.

Recall from [Our First Async Program](https://doc.rust-lang.org/book/ch17-01-futures-and-syntax.html#our-first-async-program) that at each await point, Rust gives a runtime a chance to pause the task and switch to another one if the future being awaited isn’t ready. The inverse is also true: Rust only pauses async blocks and hands control back to a runtime at an await point. Everything between await points is synchronous.

That means if you do a bunch of work in an async block without an await point, that future will block any other futures from making progress. You may sometimes hear this referred to as one future starving other futures. In some cases, that may not be a big deal. However, if you are doing some kind of expensive setup or long-running work, or if you have a future that will keep doing some particular task indefinitely, you’ll need to think about when and where to hand control back to the runtime.

By the same token, if you have long-running blocking operations, async can be a useful tool for providing ways for different parts of the program to relate to each other.

But how would you hand control back to the runtime in those cases?

### Yielding Control to the Runtime

Let’s simulate a long-running operation. Listing 17-24 introduces a `slow` function.

```rust
fn slow(name: &str, ms: u64) {
    thread::sleep(Duration::from_millis(ms));
    println!("'{name}' ran for {ms}ms");
}
```

Listing 17-24: Using `thread::sleep` to simulate slow operations

This code uses `std::thread::sleep` instead of `trpl::sleep` so that calling `slow` will block the current thread for some number of milliseconds. We can use `slow` to stand in for real-world operations that are both long-running and blocking.

In Listing 17-25, we use `slow` to emulate doing this kind of CPU-bound work in a pair of futures.

```rust
let a = async {
    println!("'a' started.");
    slow("a", 30);
    slow("a", 10);
    slow("a", 20);
    trpl::sleep(Duration::from_millis(50)).await;
    println!("'a' finished.");
};

let b = async {
    println!("'b' started.");
    slow("b", 75);
    slow("b", 10);
    slow("b", 15);
    slow("b", 350);
    trpl::sleep(Duration::from_millis(50)).await;
    println!("'b' finished.");
};

trpl::race(a, b).await;
```

Listing 17-25: Using `thread::sleep` to simulate slow operations

To begin, each future only hands control back to the runtime after carrying out a bunch of slow operations. If you run this code, you will see this output:

```text
'a' started.
'a' ran for 30ms
'a' ran for 10ms
'a' ran for 20ms
'b' started.
'b' ran for 75ms
'b' ran for 10ms
'b' ran for 15ms
'b' ran for 350ms
'a' finished.
```

As with our earlier example, `race` still finishes as soon as `a` is done. There’s no interleaving between the two futures, though. The `a` future does all of its work until the `trpl::sleep` call is awaited, then the `b` future does all of its work until its own `trpl::sleep` call is awaited, and finally the `a` future completes. To allow both futures to make progress between their slow tasks, we need await points so we can hand control back to the runtime. That means we need something we can await!

We can already see this kind of handoff happening in Listing 17-25: if we removed the `trpl::sleep` at the end of the `a` future, it would complete without the `b` future running at all. Let’s try using the `sleep` function as a starting point for letting operations switch off making progress, as shown in Listing 17-26.

```rust
let one_ms = Duration::from_millis(1);

let a = async {
    println!("'a' started.");
    slow("a", 30);
    trpl::sleep(one_ms).await;
    slow("a", 10);
    trpl::sleep(one_ms).await;
    slow("a", 20);
    trpl::sleep(one_ms).await;
    println!("'a' finished.");
};

let b = async {
    println!("'b' started.");
    slow("b", 75);
    trpl::sleep(one_ms).await;
    slow("b", 10);
    trpl::sleep(one_ms).await;
    slow("b", 15);
    trpl::sleep(one_ms).await;
    slow("b", 350);
    trpl::sleep(one_ms).await;
    println!("'b' finished.");
};
```

Listing 17-26: Using `sleep` to let operations switch off making progress

In Listing 17-26, we add `trpl::sleep` calls with await points between each call to `slow`. Now the two futures’ work is interleaved:

```text
'a' started.
'a' ran for 30ms
'b' started.
'b' ran for 75ms
'a' ran for 10ms
'b' ran for 10ms
'a' ran for 20ms
'b' ran for 15ms
'a' finished.
```

The `a` future still runs for a bit before handing off control to `b`, because it calls `slow` before ever calling `trpl::sleep`, but after that the futures swap back and forth each time one of them hits an await point. In this case, we have done that after every call to `slow`, but we could break up the work in whatever way makes the most sense to us.

We don’t really want to sleep here, though: we want to make progress as fast as we can. We just need to hand back control to the runtime. We can do that directly, using the `yield_now` function. In Listing 17-27, we replace all those `sleep` calls with `yield_now`.

```rust
let a = async {
    println!("'a' started.");
    slow("a", 30);
    trpl::yield_now().await;
    slow("a", 10);
    trpl::yield_now().await;
    slow("a", 20);
    trpl::yield_now().await;
    println!("'a' finished.");
};

let b = async {
    println!("'b' started.");
    slow("b", 75);
    trpl::yield_now().await;
    slow("b", 10);
    trpl::yield_now().await;
    slow("b", 15);
    trpl::yield_now().await;
    slow("b", 350);
    trpl::yield_now().await;
    println!("'b' finished.");
};
```

Listing 17-27: Using `yield_now` to let operations switch off making progress

This code is both clearer about the actual intent and can be significantly faster than using `sleep`, because timers such as the one used by `sleep` often have limits on how granular they can be. The version of `sleep` we are using, for example, will always sleep for at least a millisecond, even if we pass it a `Duration` of one nanosecond. Again, modern computers are fast: they can do a lot in one millisecond!

You can see this for yourself by setting up a little benchmark, such as the one in Listing 17-28. (This isn’t an especially rigorous way to do performance testing, but it suffices to show the difference here.)

```rust
let one_ns = Duration::from_nanos(1);
let start = Instant::now();
async {
    for _ in 1..1000 {
        trpl::sleep(one_ns).await;
    }
}
.await;
let time = Instant::now() - start;

println!(
    "'sleep' version finished after {} seconds.",
    time.as_secs_f32()
);

let start = Instant::now();
async {
    for _ in 1..1000 {
        trpl::yield_now().await;
    }
}
.await;
let time = Instant::now() - start;
println!(
    "'yield' version finished after {} seconds.",
    time.as_secs_f32()
);
```

Listing 17-28: Comparing the performance of `sleep` and `yield_now`

Here, we skip all the status printing, pass a one-nanosecond `Duration` to `trpl::sleep`, and let each future run by itself, with no switching between the futures. Then we run for 1,000 iterations and see how long the future using `trpl::sleep` takes compared to the future using `trpl::yield_now`.

The version with `yield_now` is way faster!

This means that async can be useful even for compute-bound tasks, depending on what else your program is doing, because it provides a useful tool for structuring the relationships between different parts of the program. This is a form of cooperative multitasking, where each future has the power to determine when it hands over control via await points. Each future therefore also has the responsibility to avoid blocking for too long. In some Rust-based embedded operating systems, this is the only kind of multitasking!

In real-world code, you won’t usually be alternating function calls with await points on every single line, of course. While yielding control in this way is relatively inexpensive, it’s not free. In many cases, trying to break up a compute-bound task might make it significantly slower, so sometimes it’s better for overall performance to let an operation block briefly. Always measure to see what your code’s actual performance bottlenecks are. The underlying dynamic is important to keep in mind, though, if you are seeing a lot of work happening in serial that you expected to happen concurrently!

### Building Our Own Async Abstractions

We can also compose futures together to create new patterns. For example, we can build a `timeout` function with async building blocks we already have. When we’re done, the result will be another building block we could use to create still more async abstractions.

Listing 17-29 shows how we would expect this `timeout` to work with a slow future.

```rust
let slow = async {
    trpl::sleep(Duration::from_millis(100)).await;
    "I finished!"
};

match timeout(slow, Duration::from_millis(10)).await {
    Ok(message) => println!("Succeeded with '{message}'"),
    Err(duration) => {
        println!("Failed after {} seconds", duration.as_secs())
    }
}
```

Listing 17-29: Using our imagined `timeout` to run a slow operation with a time limit

Let’s implement this! To begin, let’s think about the API for `timeout`:

• It needs to be an async function itself so we can await it.
• Its first parameter should be a future to run. We can make it generic to allow it to work with any future.
• Its second parameter will be the maximum time to wait. If we use a `Duration`, that will make it easy to pass along to `trpl::sleep`.
• It should return a `Result`. If the future completes successfully, the `Result` will be `Ok` with the value produced by the future. If the timeout elapses first, the `Result` will be `Err` with the duration that the timeout waited for.

Listing 17-30 shows this declaration.

```rust
async fn timeout<F: Future>(
    future_to_try: F,
    max_time: Duration,
) -> Result<F::Output, Duration> {
    // Here is where our implementation will go!
}
```

Listing 17-30: Defining the signature of `timeout`

That satisfies our goals for the types. Now let’s think about the behavior we need: we want to race the future passed in against the duration. We can use `trpl::sleep` to make a timer future from the duration, and use `trpl::race` to run that timer with the future the caller passes in.

We also know that `race` is not fair, polling arguments in the order in which they are passed. Thus, we pass `future_to_try` to `race` first so it gets a chance to complete even if `max_time` is a very short duration. If `future_to_try` finishes first, `race` will return `Left` with the output from `future_to_try`. If `timer` finishes first, `race` will return `Right` with the timer’s output of `()`.

In Listing 17-31, we match on the result of awaiting `trpl::race`.

```rust
use trpl::Either;

// --snip--

fn main() {
    trpl::run(async {
        let slow = async {
            trpl::sleep(Duration::from_secs(5)).await;
            "Finally finished"
        };

        match timeout(slow, Duration::from_secs(2)).await {
            Ok(message) => println!("Succeeded with '{message}'"),
            Err(duration) => {
                println!("Failed after {} seconds", duration.as_secs())
            }
        }
    });
}

async fn timeout<F: Future>(
    future_to_try: F,
    max_time: Duration,
) -> Result<F::Output, Duration> {
    match trpl::race(future_to_try, trpl::sleep(max_time)).await {
        Either::Left(output) => Ok(output),
        Either::Right(_) => Err(max_time),
    }
}
```

Listing 17-31: Defining `timeout` with `race` and `sleep`

If the `future_to_try` succeeds and we get a `Left(output)`, we return `Ok(output)`. If the sleep timer elapses instead and we get a `Right(())`, we ignore the `()` with `_` and return `Err(max_time)` instead.

With that, we have a working `timeout` built out of two other async helpers. If we run our code, it will print the failure mode after the timeout:

```text
Failed after 2 seconds
```

Because futures compose with other futures, you can build really powerful tools using smaller async building blocks. For example, you can use this same approach to combine timeouts with retries, and in turn use those with operations such as network calls (one of the examples from the beginning of the chapter).

We’ve now seen a number of ways to work with multiple futures at the same time. Up next, we’ll look at how we can work with multiple futures in a sequence over time with streams. Here are a couple more things you might want to consider first, though:

• We used a `Vec` with `join_all` to wait for all of the futures in some group to finish. How could you use a `Vec` to process a group of futures in sequence instead? What are the tradeoffs of doing that?

• Take a look at the `futures::stream::FuturesUnordered` type from the `futures` crate. How would using it be different from using a `Vec`? (Don’t worry about the fact that it’s from the `stream` part of the crate; it works just fine with any collection of futures.)

## Processing a Series of Asynchronous Values with Streams

So far in this chapter, we’ve mostly stuck to individual futures. The one big exception was the async channel we used. Recall how we used the receiver for our async channel earlier in this chapter in the [“Message Passing”](https://doc.rust-lang.org/book/ch17-02-concurrency-with-async.html#message-passing) section. The async `recv` method produces a sequence of items over time. This is an instance of a much more general pattern known as a stream.

We saw a sequence of items back in Chapter 13, when we looked at the `Iterator` trait in [The Iterator Trait and the next Method](https://doc.rust-lang.org/book/ch13-02-iterators.html#the-iterator-trait-and-the-next-method) section, but there are two differences between iterators and the async channel receiver. The first difference is time: iterators are synchronous, while the channel receiver is asynchronous. The second is the API. When working directly with `Iterator`, we call its synchronous `next` method. With the `trpl::Receiver` stream in particular, we called an asynchronous `recv` method instead. Otherwise, these APIs feel very similar, and that similarity isn’t a coincidence. A stream is like an asynchronous form of iteration. Whereas the `trpl::Receiver` specifically waits to receive messages, though, the general-purpose stream API is much broader: it provides the next item the way `Iterator` does, but asynchronously.

The similarity between iterators and streams in Rust means we can actually create a stream from any iterator. As with an iterator, we can work with a stream by calling its `next` method and then awaiting the output, as in Listing 17-30.

Filename: src/main.rs

```rust
use trpl::StreamExt;

fn main() {
    trpl::run(async {
        let values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let iter = values.iter().map(|n| n * 2);
        let mut stream = trpl::stream_from_iter(iter);

        while let Some(value) = stream.next().await {
            println!("The value was: {value}");
        }
    });
}
```

Listing 17-30: Creating a stream from an iterator and printing its values

We start with an array of numbers, which we convert to an iterator and then call `map` on to double all the values. Then we convert the iterator into a stream using the `trpl::stream_from_iter` function. Next, we loop over the items in the stream as they arrive with the `while let` loop.

Unfortunately, when we try to run the code, it doesn’t compile, but instead it reports that there’s no `next` method available:

```text
error[E0599]: no method named `next` found for struct `Iter` in the current scope
  --> src/main.rs:10:40
   |
10 |         while let Some(value) = stream.next().await {
   |                                        ^^^^
   |
   = note: the full type name has been written to 'file:///projects/async-await/target/debug/deps/async_await-575db3dd3197d257.long-type-14490787947592691573.txt'
   = note: consider using `--verbose` to print the full type name to the console
help: the following traits which provide `next` are implemented but not in scope; perhaps you want to import one of them
   |
1  + use crate::trpl::StreamExt;
   |
1  + use futures_util::stream::stream::StreamExt;
   |
1  + use std::iter::Iterator;
   |
help: there is a method `try_next` with a similar name
   |
10 |         while let Some(value) = stream.try_next().await {
   |                                        ~~~~~~~~

```

As this output explains, the reason for the compiler error is that we need the right trait in scope to be able to use the `next` method. Given our discussion so far, you might reasonably expect that trait to be `Stream`, but it’s actually `StreamExt`. Short for extension, `Ext` is a common pattern in the Rust community for extending one trait with another.

We’ll explain the `Stream` and `StreamExt` traits in a bit more detail at the end of the chapter, but for now all you need to know is that the `Stream` trait defines a low-level interface that effectively combines the `Iterator` and `Future` traits. `StreamExt` supplies a higher-level set of APIs on top of `Stream`, including the `next` method as well as other utility methods similar to those provided by the `Iterator` trait. `Stream` and `StreamExt` are not yet part of Rust’s standard library, but most ecosystem crates use the same definition.

The fix to the compiler error is to add a `use` statement for `trpl::StreamExt`, as in Listing 17-31.

Filename: src/main.rs

```rust
use trpl::StreamExt;

fn main() {
    trpl::run(async {
        let values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let iter = values.iter().map(|n| n * 2);
        let stream = trpl::stream_from_iter(iter);

        let mut filtered =
            stream.filter(|value| value % 3 == 0 || value % 5 == 0);

        while let Some(value) = filtered.next().await {
            println!("The value was: {value}");
        }
    });
}
```

Listing 17-31: Successfully using an iterator as the basis for a stream

With all those pieces put together, this code works the way we want! What’s more, now that we have `StreamExt` in scope, we can use all of its utility methods, just as with iterators. For example, in Listing 17-32, we use the `filter` method to filter out everything but multiples of three and five.

Filename: src/main.rs

```rust
use trpl::StreamExt;

fn main() {
    trpl::run(async {
        let values = 1..101;
        let iter = values.map(|n| n * 2);
        let stream = trpl::stream_from_iter(iter);

        let mut filtered =
            stream.filter(|value| value % 3 == 0 || value % 5 == 0);

        while let Some(value) = filtered.next().await {
            println!("The value was: {value}");
        }
    });
}
```

Listing 17-32: Filtering a stream with the `StreamExt::filter` method

Of course, this isn’t very interesting, since we could do the same with normal iterators and without any async at all. Let’s look at what we can do that is unique to streams.

### Composing Streams

Many concepts are naturally represented as streams: items becoming available in a queue, chunks of data being pulled incrementally from the filesystem when the full data set is too large for the computer’s memory, or data arriving over the network over time. Because streams are futures, we can use them with any other kind of future and combine them in interesting ways. For example, we can batch up events to avoid triggering too many network calls, set timeouts on sequences of long-running operations, or throttle user interface events to avoid doing needless work.

Let’s start by building a little stream of messages as a stand-in for a stream of data we might see from a WebSocket or another real-time communication protocol, as shown in Listing 17-33.

Filename: src/main.rs

```rust
use trpl::{ReceiverStream, Stream, StreamExt};

fn main() {
    trpl::run(async {
        let mut messages = get_messages();

        while let Some(message) = messages.next().await {
            println!("{message}");
        }
    });
}

fn get_messages() -> impl Stream<Item = String> {
    let (tx, rx) = trpl::channel();

    let messages = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"];
    for message in messages {
        tx.send(format!("Message: '{message}'")).unwrap();
    }

    ReceiverStream::new(rx)
}
```

Listing 17-33: Using the `rx` receiver as a `ReceiverStream`

First, we create a function called `get_messages` that returns `impl Stream<Item = String>`. For its implementation, we create an async channel, loop over the first 10 letters of the English alphabet, and send them across the channel.

We also use a new type: `ReceiverStream`, which converts the `rx` receiver from the `trpl::channel` into a `Stream` with a `next` method. Back in `main`, we use a `while let` loop to print all the messages from the stream.

When we run this code, we get exactly the results we would expect:

```text
Message: 'a'
Message: 'b'
Message: 'c'
Message: 'd'
Message: 'e'
Message: 'f'
Message: 'g'
Message: 'h'
Message: 'i'
Message: 'j'
```

Again, we could do this with the regular `Receiver` API or even the regular `Iterator` API, though, so let’s add a feature that requires streams: adding a timeout that applies to every item in the stream, and a delay on the items we emit, as shown in Listing 17-34.

Filename: src/main.rs

```rust
use std::{pin::pin, time::Duration};
use trpl::{ReceiverStream, Stream, StreamExt};

fn main() {
    trpl::run(async {
        let mut messages =
            pin!(get_messages().timeout(Duration::from_millis(200)));

        while let Some(result) = messages.next().await {
            match result {
                Ok(message) => println!("{message}"),
                Err(reason) => eprintln!("Problem: {reason:?}"),
            }
        }
    })
}
```

Listing 17-34: Using the `StreamExt::timeout` method to set a time limit on the items in a stream

We start by adding a timeout to the stream with the `timeout` method, which comes from the `StreamExt` trait. Then we update the body of the `while let` loop, because the stream now returns a `Result`. The `Ok` variant indicates a message arrived in time; the `Err` variant indicates that the timeout elapsed before any message arrived. We `match` on that result and either print the message when we receive it successfully or print a notice about the timeout. Finally, notice that we pin the messages after applying the timeout to them, because the timeout helper produces a stream that needs to be pinned to be polled.

However, because there are no delays between messages, this timeout does not change the behavior of the program. Let’s add a variable delay to the messages we send, as shown in Listing 17-35.

Filename: src/main.rs

```rust
fn get_messages() -> impl Stream<Item = String> {
    let (tx, rx) = trpl::channel();

    trpl::spawn_task(async move {
        let messages = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"];
        for (index, message) in messages.into_iter().enumerate() {
            let time_to_sleep = if index % 2 == 0 { 100 } else { 300 };
            trpl::sleep(Duration::from_millis(time_to_sleep)).await;

            tx.send(format!("Message: '{message}'")).unwrap();
        }
    });

    ReceiverStream::new(rx)
}
```

Listing 17-35: Sending messages through `tx` with an async delay without making `get_messages` an async function

In `get_messages`, we use the `enumerate` iterator method with the `messages` array so that we can get the index of each item we’re sending along with the item itself. Then we apply a 100-millisecond delay to even-index items and a 300-millisecond delay to odd-index items to simulate the different delays we might see from a stream of messages in the real world. Because our timeout is for 200 milliseconds, this should affect half of the messages.

To sleep between messages in the `get_messages` function without blocking, we need to use async. However, we can’t make `get_messages` itself into an async function, because then we’d return a `Future<Output = Stream<Item = String>>` instead of a `Stream<Item = String>>`. The caller would have to await `get_messages` itself to get access to the stream. But remember: everything in a given future happens linearly; concurrency happens between futures. Awaiting `get_messages` would require it to send all the messages, including the sleep delay between each message, before returning the receiver stream. As a result, the timeout would be useless. There would be no delays in the stream itself; they would all happen before the stream was even available.

Instead, we leave `get_messages` as a regular function that returns a stream, and we spawn a task to handle the async `sleep` calls.

Note: Calling `spawn_task` in this way works because we already set up our runtime; had we not, it would cause a panic. Other implementations choose different tradeoffs: they might spawn a new runtime and avoid the panic but end up with a bit of extra overhead, or they may simply not provide a standalone way to spawn tasks without reference to a runtime. Make sure you know what tradeoff your runtime has chosen and write your code accordingly!

Now our code has a much more interesting result. Between every other pair of messages, a `Problem: Elapsed(())` error.

```text
--snip--
Interval: 38
Interval: 39
Interval: 40
Message: 'a'
Interval: 41
Interval: 42
Interval: 43
--snip--
```

The timeout doesn’t prevent the messages from arriving in the end. We still get all of the original messages, because our channel is unbounded: it can hold as many messages as we can fit in memory. If the message doesn’t arrive before the timeout, our stream handler will account for that, but when it polls the stream again, the message may now have arrived.

You can get different behavior if needed by using other kinds of channels or other kinds of streams more generally. Let’s see one of those in practice by combining a stream of time intervals with this stream of messages.

### Merging Streams

First, let’s create another stream, which will emit an item every millisecond if we let it run directly. For simplicity, we can use the `sleep` function to send a message on a delay and combine it with the same approach we used in `get_messages` of creating a stream from a channel. The difference is that this time, we’re going to send back the count of intervals that have elapsed, so the return type will be `impl Stream<Item = u32>`, and we can call the function `get_intervals` (see Listing 17-36).

Filename: src/main.rs

```rust
fn get_intervals() -> impl Stream<Item = u32> {
    let (tx, rx) = trpl::channel();

    trpl::spawn_task(async move {
        let mut count = 0;
        loop {
            trpl::sleep(Duration::from_millis(1)).await;
            count += 1;
            tx.send(count).unwrap();
        }
    });

    ReceiverStream::new(rx)
}
```

Listing 17-36: Creating a stream with a counter that will be emitted once every millisecond

We start by defining a `count` in the task. (We could define it outside the task, too, but it’s clearer to limit the scope of any given variable.) Then we create an infinite loop. Each iteration of the loop asynchronously sleeps for one millisecond, increments the count, and then sends it over the channel. Because this is all wrapped in the task created by `spawn_task`, all of it—including the infinite loop—will get cleaned up along with the runtime.

This kind of infinite loop, which ends only when the whole runtime gets torn down, is fairly common in async Rust: many programs need to keep running indefinitely. With async, this doesn’t block anything else, as long as there is at least one await point in each iteration through the loop.

Now, back in our main function’s async block, we can attempt to merge the `messages` and `intervals` streams, as shown in Listing 17-37.

Filename: src/main.rs

```rust
        let messages = get_messages().timeout(Duration::from_millis(200));
        let intervals = get_intervals();
        let merged = messages.merge(intervals);

        while let Some(result) = merged.next().await {
```

Listing 17-37: Attempting to merge the `messages` and `intervals` streams

We start by calling `get_intervals`. Then we merge the `messages` and `intervals` streams with the `merge` method, which combines multiple streams into one stream that produces items from any of the source streams as soon as the items are available, without imposing any particular ordering. Finally, we loop over that combined stream instead of over `messages`.

At this point, neither `messages` nor `intervals` needs to be pinned or mutable, because both will be combined into the single `merged` stream. However, this call to `merge` doesn’t compile! (Neither does the `next` call in the `while let` loop, but we’ll come back to that.) This is because the two streams have different types. The `messages` stream has the type `Timeout<impl Stream<Item = String>>`, where `Timeout` is the type that implements `Stream` for a `timeout` call. The `intervals` stream has the type `impl Stream<Item = u32>`. To merge these two streams, we need to transform one of them to match the other. We’ll rework the intervals stream, because messages is already in the basic format we want and has to handle timeout errors (see Listing 17-38).

Filename: src/main.rs

```rust
        let messages = get_messages().timeout(Duration::from_millis(200));
        let intervals = get_intervals()
            .map(|count| format!("Interval: {count}"))
            .timeout(Duration::from_secs(10));
        let merged = messages.merge(intervals).take(20);
        let mut stream = pin!(merged);

        while let Some(result) = stream.next().await {
```

Listing 17-38: Aligning the type of the the `intervals` stream with the type of the `messages` stream

First, we can use the `map` helper method to transform the `intervals` into a string. Second, we need to match the `Timeout` from `messages`. Because we don’t actually want a timeout for `intervals`, though, we can just create a timeout which is longer than the other durations we are using. Here, we create a 10-second timeout with `Duration::from_secs(10)`. Finally, we need to make `stream` mutable, so that the `while let` loop’s `next` calls can iterate through the stream, and pin it so that it’s safe to do so.

That gets us almost to where we need to be. Everything type checks. If you run this, though, there will be two problems. First, it will never stop! You’ll need to stop it with ctrl-c. Second, the messages from the English alphabet will be buried in the midst of all the interval counter messages:

```text
--snip--
Interval: 38
Interval: 39
Interval: 40
Message: 'a'
Interval: 41
Interval: 42
Interval: 43
--snip--
```

Listing 17-39 shows one way to solve these last two problems.

Filename: src/main.rs

```rust
        let messages = get_messages().timeout(Duration::from_millis(200));
        let intervals = get_intervals()
            .map(|count| format!("Interval: {count}"))
            .throttle(Duration::from_millis(100))
            .timeout(Duration::from_secs(10));
        let merged = messages.merge(intervals).take(20);
        let mut stream = pin!(merged);
```

Listing 17-39: Using `throttle` and `take` to manage the merged streams

First, we use the `throttle` method on the `intervals` stream so that it doesn’t overwhelm the `messages` stream. Throttling is a way of limiting the rate at which a function will be called—or, in this case, how often the stream will be polled. Once every 100 milliseconds should do, because that’s roughly how often our messages arrive.

To limit the number of items we will accept from a stream, we apply the `take` method to the `merged` stream, because we want to limit the final output, not just one stream or the other.

Now when we run the program, it stops after pulling 20 items from the stream, and the intervals don’t overwhelm the messages. We also don’t get `Interval: 100` or `Interval: 200` or so on, but instead get `Interval: 1`, `Interval: 2`, and so on—even though we have a source stream that can produce an event every millisecond. That’s because the `throttle` call produces a new stream that wraps the original stream so that the original stream gets polled only at the throttle rate, not its own "native" rate. We don’t have a bunch of unhandled interval messages we’re choosing to ignore. Instead, we never produce those interval messages in the first place! This is the inherent "laziness" of Rust’s futures at work again, allowing us to choose our performance characteristics.

```text
Interval: 1
Message: 'a'
Interval: 2
Interval: 3
Problem: Elapsed(())
Interval: 4
Message: 'b'
Interval: 5
Message: 'c'
Interval: 6
Interval: 7
Problem: Elapsed(())
Interval: 8
Message: 'd'
Interval: 9
Message: 'e'
Interval: 10
Interval: 11
Problem: Elapsed(())
Interval: 12
```

There’s one last thing we need to handle: errors! With both of these channel-based streams, the `send` calls could fail when the other side of the channel closes—and that’s just a matter of how the runtime executes the futures that make up the stream. Up until now, we’ve ignored this possibility by calling `unwrap`, but in a well-behaved app, we should explicitly handle the error, at minimum by ending the loop so we don’t try to send any more messages. Listing 17-40 shows a simple error strategy: print the issue and then `break` from the loops.

```rust
fn get_messages() -> impl Stream<Item = String> {
    let (tx, rx) = trpl::channel();

    trpl::spawn_task(async move {
        let messages = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"];

        for (index, message) in messages.into_iter().enumerate() {
            let time_to_sleep = if index % 2 == 0 { 100 } else { 300 };
            trpl::sleep(Duration::from_millis(time_to_sleep)).await;

            if let Err(send_error) = tx.send(format!("Message: '{message}'")) {
                eprintln!("Cannot send message '{message}': {send_error}");
                break;
            }
        }
    });

    ReceiverStream::new(rx)
}

fn get_intervals() -> impl Stream<Item = u32> {
    let (tx, rx) = trpl::channel();

    trpl::spawn_task(async move {
        let mut count = 0;
        loop {
            trpl::sleep(Duration::from_millis(1)).await;
            count += 1;

            if let Err(send_error) = tx.send(count) {
                eprintln!("Could not send interval {count}: {send_error}");
                break;
            };
        }
    });

    ReceiverStream::new(rx)
}
```

Listing 17-40: Handling errors and shutting down the loops

As usual, the correct way to handle a message send error will vary; just make sure you have a strategy.

Now that we’ve seen a bunch of async in practice, let’s take a step back and dig into a few of the details of how `Future`, `Stream`, and the other key traits Rust uses to make async work.

## Traits for Async Programming

In the previous chapters, we’ve seen how async functions and futures work. But what if we want to define traits that include async methods? For example, we might want to define a trait for a database connection that has an async method for querying data.

Let’s start with a simple example. Suppose we want to define a trait for something that can be started asynchronously. In Listing 17-42, we try to define such a trait.

```rust
trait Startable {
    async fn start(&self);
}
```

Listing 17-42: Attempting to define a trait with an async method

This code looks straightforward, but it won’t compile. The problem is that async functions return futures, and traits can’t directly include methods that return futures in this way.

To fix this, we need to use the `async-trait` crate, which provides a macro to make this work. In Listing 17-43, we use the `async_trait` macro.

```rust
use async_trait::async_trait;

#[async_trait]
trait Startable {
    async fn start(&self);
}
```

Listing 17-43: Using the `async_trait` macro to define a trait with async methods

The `async_trait` macro transforms the trait so that async methods are represented as methods that return `Pin<Box<dyn Future<Output = T> + Send + '_>>`. This allows traits to include async methods.

Now we can implement this trait for a struct. In Listing 17-44, we implement `Startable` for a `Server` struct.

```rust
use async_trait::async_trait;

#[async_trait]
trait Startable {
    async fn start(&self);
}

struct Server {
    name: String,
}

#[async_trait]
impl Startable for Server {
    async fn start(&self) {
        println!("Starting server {}", self.name);
        trpl::sleep(Duration::from_millis(100)).await;
        println!("Server {} started", self.name);
    }
}
```

Listing 17-44: Implementing an async trait for a struct

This code defines a `Server` struct and implements the `Startable` trait for it. The implementation includes an async `start` method that simulates starting a server.

We can also define traits with async methods that return values. In Listing 17-45, we define a trait for a calculator that can perform async computations.

```rust
use async_trait::async_trait;

#[async_trait]
trait Calculator {
    async fn add(&self, a: i32, b: i32) -> i32;
    async fn multiply(&self, a: i32, b: i32) -> i32;
}

struct SimpleCalculator;

#[async_trait]
impl Calculator for SimpleCalculator {
    async fn add(&self, a: i32, b: i32) -> i32 {
        trpl::sleep(Duration::from_millis(10)).await;
        a + b
    }

    async fn multiply(&self, a: i32, b: i32) -> i32 {
        trpl::sleep(Duration::from_millis(10)).await;
        a * b
    }
}
```

Listing 17-45: Defining a trait with async methods that return values

This code defines a `Calculator` trait with async methods for addition and multiplication. It then implements the trait for a `SimpleCalculator` struct.

We can also define associated types in async traits. In Listing 17-47, we define a trait for a data source that can load data asynchronously.

```rust
use async_trait::async_trait;

#[async_trait]
trait DataSource {
    type Data;
    type Error;

    async fn load(&self, id: &str) -> Result<Self::Data, Self::Error>;
}

struct Database {
    connection_string: String,
}

#[async_trait]
impl DataSource for Database {
    type Data = String;
    type Error = String;

    async fn load(&self, id: &str) -> Result<Self::Data, Self::Error> {
        println!("Loading data for {} from database", id);
        trpl::sleep(Duration::from_millis(50)).await;
        Ok(format!("Data for {}", id))
    }
}
```

Listing 17-47: Using associated types in async traits

This code defines a `DataSource` trait with associated types for the data and error types. It then implements the trait for a `Database` struct.

Async traits can also be used with generic parameters. In Listing 17-48, we define a generic trait for processing items asynchronously.

```rust
use async_trait::async_trait;

#[async_trait]
trait Processor<T> {
    async fn process(&self, item: T) -> T;
}

struct Doubler;

#[async_trait]
impl Processor<i32> for Doubler {
    async fn process(&self, item: i32) -> i32 {
        trpl::sleep(Duration::from_millis(5)).await;
        item * 2
    }
}
```

Listing 17-48: Using generics with async traits

This code defines a generic `Processor` trait that can process items of any type asynchronously. It then implements the trait for a `Doubler` that doubles integers.

We can also define default implementations for async methods in traits. In Listing 17-49, we add a default implementation to our `Processor` trait.

```rust
use async_trait::async_trait;

#[async_trait]
trait Processor<T> {
    async fn process(&self, item: T) -> T {
        item
    }
}

struct Doubler;

#[async_trait]
impl Processor<i32> for Doubler {
    async fn process(&self, item: i32) -> i32 {
        trpl::sleep(Duration::from_millis(5)).await;
        item * 2
    }
}

struct Identity;

#[async_trait]
impl Processor<i32> for Identity {
    // Uses the default implementation
}
```

Listing 17-49: Default implementations in async traits

This code adds a default implementation to the `process` method that just returns the item unchanged. The `Identity` struct uses this default implementation, while `Doubler` overrides it.

We can also define traits with async methods that have different visibility. In Listing 17-50, we define a trait with a public async method and a private async method.

```rust
use async_trait::async_trait;

#[async_trait]
trait Secretive {
    async fn public_method(&self);

    // Private methods are not allowed in traits
    // async fn private_method(&self);
}

struct Spooky;

#[async_trait]
impl Secretive for Spooky {
    async fn public_method(&self) {
        println!("Boo! I'm a spooky ghost.");
    }

    // async fn private_method(&self) {
    //     println!("You can't see me!");
    // }
}
```

Listing 17-50: Traits with async methods of different visibility

This code defines a `Secretive` trait with a public async method `public_method`. It also shows an attempt to define a private async method, which is not allowed in traits. The `Spooky` struct implements the `Secretive` trait.

We can also use async traits with objects for dynamic dispatch. In Listing 17-51, we define a function that takes a trait object and calls an async method.

```rust
use async_trait::async_trait;

#[async_trait]
trait Notifier {
    async fn notify(&self, message: &str);
}

struct EmailNotifier;

#[async_trait]
impl Notifier for EmailNotifier {
    async fn notify(&self, message: &str) {
        println!("Sending email: {}", message);
    }
}

struct SmsNotifier;

#[async_trait]
impl Notifier for SmsNotifier {
    async fn notify(&self, message: &str) {
        println!("Sending SMS: {}", message);
    }
}

async fn send_notification(notifier: &dyn Notifier, message: &str) {
    notifier.notify(message).await;
}
```

Listing 17-51: Using async traits with objects

This code defines a `Notifier` trait with an async method `notify`. It then implements the trait for `EmailNotifier` and `SmsNotifier` structs. The `send_notification` function takes a trait object and calls the `notify` method.

Async traits are a powerful way to define abstractions over async code. They allow us to write generic, reusable async interfaces. However, they do come with some overhead due to the boxing of futures. In performance-critical code, you might want to consider other approaches, like using generics with bounds instead of trait objects.

In the final section of this chapter, we'll look at how async code integrates with threads and the broader Rust ecosystem.

## Futures, Tasks, and Threads

So far in this chapter, we’ve focused on async Rust as a way to write concurrent code that’s efficient and safe. But async code doesn’t exist in a vacuum—it has to run somewhere. In this section, we’ll look at how futures, tasks, and threads work together in Rust’s async ecosystem.

### Futures and Tasks

We’ve been using futures throughout this chapter, but let’s take a step back and think about what they really are. A future represents a value that might not be ready yet. When we call an async function, we get a future that will eventually resolve to the function’s return value.

But futures don’t do anything by themselves. They need to be polled to make progress. Polling a future asks it: “Are you ready yet? If not, what do you need to become ready?” Futures that aren’t ready yet return `Poll::Pending` and register a waker that will be called when they’re ready to make more progress.

This is where tasks come in. A task is a unit of work that can be scheduled and executed. Tasks wrap futures and are responsible for polling them when they’re ready. In async Rust, we typically don’t create tasks directly—instead, we use async runtimes that manage tasks for us.

Let’s look at how this works with the `trpl` runtime we’ve been using. In Listing 17-52, we create a simple async function and run it.

```rust
async fn hello() {
    println!("Hello from async!");
}

fn main() {
    trpl::run(async {
        hello().await;
    });
}
```

Listing 17-52: Running an async function with `trpl::run`

When we call `trpl::run`, it creates a task for the future returned by the async block, then polls that task until the future completes. The runtime handles all the details of scheduling and waking tasks.

### Spawning Tasks

Often, we want to run multiple tasks concurrently. We can use `spawn` functions to create new tasks. In Listing 17-53, we spawn two tasks that run concurrently.

```rust
async fn task1() {
    for i in 1..=5 {
        println!("Task 1: {}", i);
        trpl::sleep(Duration::from_millis(100)).await;
    }
}

async fn task2() {
    for i in 1..=5 {
        println!("Task 2: {}", i);
        trpl::sleep(Duration::from_millis(150)).await;
    }
}

fn main() {
    trpl::run(async {
        let handle1 = trpl::spawn(task1());
        let handle2 = trpl::spawn(task2());

        handle1.await;
        handle2.await;
    });
}
```

Listing 17-53: Spawning concurrent tasks

This code spawns two tasks that print numbers at different intervals. The `spawn` function returns a `JoinHandle` that we can await to wait for the task to complete.

Tasks are lightweight compared to threads. Creating thousands of tasks is much cheaper than creating thousands of threads. This makes async Rust suitable for applications that need to handle many concurrent operations, like web servers.

### Tasks and Threads

While tasks are lightweight, they still need to run on threads. Async runtimes typically have a thread pool that executes tasks. When a task is blocked (for example, waiting on I/O), the runtime can suspend it and run other tasks on the same thread.

In Listing 17-54, we see how tasks can be moved between threads.

```rust
async fn task() {
    println!("Task running on thread {:?}", std::thread::current().id());
    trpl::sleep(Duration::from_millis(100)).await;
    println!("Task still on thread {:?}", std::thread::current().id());
}

fn main() {
    trpl::run(async {
        let handle = trpl::spawn(task());
        handle.await;
    });
}
```

Listing 17-54: Tasks can move between threads

This code prints the thread ID before and after sleeping. Depending on the runtime implementation, the task might be resumed on a different thread after sleeping.

### Blocking Operations

Not all operations are async. Sometimes we need to call blocking code, like synchronous file I/O or CPU-intensive computations. If we call blocking code directly in an async function, it will block the entire thread, preventing other tasks from running.

To handle this, we can use `spawn_blocking` to run blocking operations on a separate thread pool. In Listing 17-55, we use `spawn_blocking` for a CPU-intensive task.

```rust
fn fibonacci(n: u32) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        _ => fibonacci(n - 1) + fibonacci(n - 2),
    }
}

async fn async_fibonacci(n: u32) -> u64 {
    trpl::spawn_blocking(move || fibonacci(n)).await
}

fn main() {
    trpl::run(async {
        let result = async_fibonacci(40).await;
        println!("Fibonacci(40) = {}", result);
    });
}
```

Listing 17-55: Using `spawn_blocking` for CPU-intensive work

This code calculates a Fibonacci number using a blocking recursive function. By wrapping it in `spawn_blocking`, we ensure that the blocking computation doesn’t prevent other async tasks from running.

### Integrating with Threads

Sometimes we need to integrate async code with existing threaded code. We can use channels to communicate between async tasks and threads. In Listing 17-56, we create a thread that sends messages to an async task.

```rust
use std::thread;
use trpl::channel;

fn main() {
    trpl::run(async {
        let (tx, rx) = channel();

        let handle = thread::spawn(move || {
            for i in 1..=5 {
                tx.send(format!("Message {}", i)).unwrap();
                thread::sleep(Duration::from_millis(100));
            }
        });

        let mut rx = rx.into_stream().fuse();
        while let Some(message) = rx.next().await {
            println!("Received: {}", message);
        }

        handle.join().unwrap();
    });
}
```

Listing 17-56: Communicating between threads and async tasks

This code spawns a thread that sends messages through a channel. The async code receives these messages as a stream.

We can also convert async code to run on threads. In Listing 17-57, we use `block_on` to run async code from a synchronous context.

```rust
use futures::executor::block_on;

async fn async_task() {
    println!("Running async task");
    trpl::sleep(Duration::from_millis(100)).await;
    println!("Async task complete");
}

fn main() {
    println!("Starting");
    block_on(async_task());
    println!("Done");
}
```

Listing 17-57: Running async code with `block_on`

This code uses `block_on` from the `futures` crate to run an async function from the synchronous `main` function.

### Choosing Between Async and Threads

When should you use async, and when should you use threads? Here are some guidelines:

- Use async for I/O-bound workloads where you need to handle many concurrent operations (like web servers or network clients).
- Use threads for CPU-bound workloads that don’t benefit from async’s cooperative scheduling.
- Use async when you need fine-grained control over concurrency and want to avoid the overhead of threads.
- Use threads when you have existing synchronous code that you want to run concurrently.

Async and threads can also work together. For example, you might have a thread pool for CPU work and async tasks for I/O.

### Async Runtimes

There are several async runtimes available for Rust:

- **Tokio**: A popular, full-featured runtime with excellent performance and ecosystem support.
- **async-std**: Provides async versions of standard library functions.
- **smol**: A minimal runtime focused on simplicity.
- **trpl**: The runtime we’ve been using in this chapter, designed for learning.

Each runtime has its own strengths and APIs, but they all follow similar patterns for tasks, futures, and concurrency.

### Summary

Async Rust provides powerful tools for writing concurrent, efficient code. Futures represent asynchronous computations, tasks manage the execution of futures, and threads provide the underlying execution environment. By understanding how these pieces fit together, you can write high-performance applications that make the most of Rust’s safety guarantees.

The async ecosystem in Rust is still evolving, but the foundations we’ve covered in this chapter will serve you well as you build async applications. Whether you’re writing web servers, network clients, or other concurrent systems, async Rust gives you the tools to write safe, efficient code.
