# Chapter 4: Async Rust

We are so close to building Rust code that handles HTTP requests. However, before we do that, we should really understand async programming in Rust, as the average web framework utilizes async code. However, it must be noted that you do not need to understand async code fully to code web servers in Rust. I have met plenty of web programmers building adequate servers who do not know how async code works under the hood. Feel free to skip this chapter if you are stretched for time or do not want to understand async rust code at a deeper level. If you are unsure, I would highly advise that you complete this chapter as understanding how async code works will give you a stronger ability to debug issues and avoid pitfalls. It will also enable you to fully utilize async Rust fully, diversifying the solutions you can offer in web programming.

In this chapter, we will cover the following topics:

- understanding async programming
- Understanding async and await
- Implementation of our own async task queue
- Exploring high-level concepts of tokio
- Implementing a HTTP server in Hyper

By the end of this chapter, you will understand what async programming is, and how async tasks are processed by building your own async task queue. You will also be able to implement your own futures by implementing your own async sleep function. Finally, you will apply the skills to a web context by building a HTTP 1 and HTTP 2 server in top of tokio using the Hyper crate.

## Technical requirements

This chapter relies on CURL which can be installed by visiting the following link:

`https://help.ubidots.com/en/articles/2165289-learn-how-to-install-run-curl-on-windows-macosx-linux`

## Understanding asynchronous programming

Up until this chapter, we have been writing code in a sequential manner. This is good enough for standard scripts. However, in web development, asynchronous programming is important, as there are multiple requests to servers, and API calls introduce idle time. In some other languages, such as Python, we can build web servers without touching any asynchronous concepts. While asynchronous concepts are utilized in these web frameworks, the implementation is defined under the hood. However, for most Rust web frameworks, async rust is directly implemented for handling endpoints.

When it comes to utilizing asynchronous code, there are two main concepts we must understand:

- Processes: A process is a program that is being executed. It has its own memory stack, registers for variables, and code.
- Threads: A thread is a lightweight process that is managed independently by a scheduler. However, it does share data with other threads and the main program.

This is demonstrated in the classic diagram below:

Figure 3.1 – Relationship between threads and processes [Source: Cburnett (2007) (`https://commons.wikimedia.org/wiki/File:Multithreaded_process.svg`), CC BY-SA 3.0 (`https://creativecommons.org/licenses/by-sa/3.0/deed.en`)]

Now that we understand what threads are and what relation they have to our code on a high-level basis, we can play with a toy example to understand how to utilize threads in our code and see the effects of these threads firsthand. A classic example is to build a basic function that merely sleeps, blocking time. This can simulate a time-expensive function such as a network request. We can run it sequentially with the following code:

```rust
use std::{thread, time};
fn do_something(number: i8) -> i8 {
    println!("number {} is running", number);
    let two_seconds = time::Duration::new(2, 0);
    thread::sleep(two_seconds);
    return 2
}
fn main() {
    let now = time::Instant::now();
    let one: i8 = do_something(1);
    let two: i8 = do_something(2);
    let three: i8 = do_something(3);
    println!("time elapsed {:?}", now.elapsed());
    println!("result {}", one + two + three);
}
```

Running the preceding code will give us the following printout:

```text
number 1 is running
number 2 is running
number 3 is running
time elapsed 6.0109845s
result 6
```

In the preceding output, we can see that our time-expensive functions run in the order that we expect them to. It also takes just over six seconds to run the entire program, which makes sense since we are running three expensive functions which sleep at two seconds each. Our expensive function also returns a two. When we add the results of all three expensive functions together, we are going to get a result of size, which is what we have.

We can speed up our program to roughly two seconds for the entire program by spinning up three threads at the same time and waiting for them to complete before moving on. Waiting for the threads to complete before moving on is called joining. So, before we start spinning off threads, we must import the join handler with the following code:

```rust
use std::thread::JoinHandle;
```

We can now spin up threads in our main function with the following code:

```rust
let now = time::Instant::now();
let thread_one: JoinHandle<i8> = thread::spawn( || do_something(1));
let thread_two: JoinHandle<i8> = thread::spawn( || do_something(2));
let thread_three: JoinHandle<i8> = thread::spawn( || do_something(3));
let result_one = thread_one.join();
let result_two = thread_two.join();
let result_three = thread_three.join();
println!("time elapsed {:?}", now.elapsed());
println!("result {}", result_one.unwrap() + result_two.unwrap() + result_three.unwrap());
```

Running the preceding code gives us the following printout:

```text
number 1 is running
number 3 is running
number 2 is running
time elapsed 2.002991041s
result 6
```

As we can see, the whole process took just over 2 seconds to run. This is because all three threads are running concurrently. We can also notice that thread three is fired before thread two. Do not worry if you get a sequence of 1, 2, 3. Threads finish in an indeterminate order. The scheduling is deterministic; however, there are thousands of events happening under the hood that require the CPU to do something. As a result, the exact time slices that each thread gets is never the same. These tiny changes add up. Because of this, we cannot guarantee that the threads will finish in a determinate order.

Looking back at how we spin off threads, we can see that we pass a closure into our thread. If we try and just pass the do_something function through the thread, we get an error complaining that the compiler expected an FnOnce<()> closure and found an i8 instead. This is because a standard closure implements the FnOnce<()> public trait, whereas our do_something function simply returns i8. When FnOnce<()> is implemented, the closure can only be called once. This means that when we create a thread, we can ensure that the closure can only be called once, and then when it returns, the thread ends. As our do_something function is the final line of the closure, i8 is returned. However, it has to be noted that just because the FnOnce<()> trait is implemented, it does not mean that we cannot call it multiple times. This trait only gets called if the context requires it. This means that if we were to call the closure outside of the thread context, we could call it multiple times.

Notice that we also directly unwrap our results. From what we know, we can deduce that the join function on the JoinHandle struct returns a Result which we also know can be an Err or Ok. We know it is going to be ok unwrapping the result directly because we are merely sleeping and then returning an integer. We also printed out the results, which were indeed integers. However, our error is not what you would expect. The full Result type we get is `Result<i8, Box<dyn Any + Send>>`.

We already know what a Box is; however, the dyn Any + Send seems new. dyn is a keyword that we use to indicate what type of trait is being used. Any + Send are two traits that must be implemented. The Any trait is for dynamic typing, meaning that the data type can be anything. The Send trait means that it is safe to be moved from one thread to another. Send also means that it is safe to copy from one thread to another. Now that we understand this, we could handle the results of threads by merely matching the Result outcome, and then downcasting the error into a String to get the error. There is more we can do with threads such as give them names or pass data between them with channels. However, the focus of this book is web programming, not an entire book on async Rust.

We now understand how to spin up threads in Rust, what they return, and how to handle them. With this information, we can move onto our next section of understanding the async and await syntax that is going to be used in our web server.

## Understanding async and await

The async and await syntax manages the same concepts covered in the previous section; however, there are some nuances. Instead of simply spawning off threads, we create futures and then manipulate them as and when needed.

In computer science, a future is an unprocessed computation. This is where the result is not yet available, but when we call or wait, the future will be populated with the result of the computation. Another way of describing this is that a future is a way of expressing a value that is not yet ready. As a result, a future is not exactly a thread. In fact, threads can use futures to maximize their potential. For instance, let us say that we have several network connections. We could have an individual thread for each network connection. This is better than sequentially processing all connections, as a slow network connection would prevent other faster connections being processed down the line until it itself is processed, resulting in a slower processing time overall. However, spinning up threads for every network connection is not free as we must allocate memory for the thread. Instead, we can have a future for each network connection. These network connections can be processed by a thread from a thread pool when the future is ready. Therefore, we can see why futures are used in web programming as there are a lot of concurrent connections.

Futures can also be referred to as promises, delays, or deferred.

To explore futures, we will create a new Cargo project, and utilize the futures created in the Cargo.toml file:

```toml
[dependencies]
tokio = { version = "1.36.0", features = ["full"] }
```

With the preceding crate installed, we can import what we need in our main.rs using the following code:

```rust
use std::{thread, time};
```

We can define futures by merely using async syntax. We can now define our do_something function with the following code:

```rust
async fn do_something(number: i8) -> i8 {
    println!("number {} is running", number);
    let two_seconds = time::Duration::new(2, 0);
    thread::sleep(two_seconds);
    return 2
}
```

Our do_something function essentially does what the code says it does, which is print out what number it is, sleep for two seconds, and then return an integer. However, if we were to directly call it, we would not get an i8. Instead, calling our do_something function directly will give us a `Future<Output = i8>`.

We can run our future and time it in the main function with the following code:

```rust
#[tokio::main(worker_threads = 1)]
async fn main() {
    let now = time::Instant::now();
    let future_one = do_something(1);
    let outcome = future_one.await;
    println!("time elapsed {:?}", now.elapsed());
    println!("Here is the outcome: {}", outcome);
}
```

We can see that the `[tokio::main]` macro enables us to ruse our main function into async. Running the preceding code will give us the following printout:

```text
number 1 is running
time elapsed 2.00018789s
Here is the outcome: 2
```

This is what is expected. However, it must be noted that if we enter an extra sleep function before, we utilize the await with the following code:

```rust
#[tokio::main(worker_threads = 1)]
async fn main() {
    let now = time::Instant::now();
    let future_one = do_something(1);
    let two_seconds = time::Duration::new(2, 0);
    thread::sleep(two_seconds);
    let outcome = future_one.await;
    println!("time elapsed {:?}", now.elapsed());
    println!("Here is the outcome: {}", outcome);
}
```

We will get the following printout:

```text
number 1 is running
time elapsed 4.000269667s
Here is the outcome: 2
```

Thus, we can see that our future does not execute until we apply an executor using await.

We can send our async task to the executor straight away and then wait on it later with the code below:

```rust
#[tokio::main(worker_threads = 1)]
async fn main() {
    let now = time::Instant::now();
    let future_one = tokio::spawn(do_something(1));
    let two_seconds = time::Duration::new(2, 0);
    thread::sleep(two_seconds);
    let outcome = future_one.await.unwrap();
    println!("time elapsed {:?}", now.elapsed());
    println!("Here is the outcome: {}", outcome);
}
```

Which gives us the following printout:

```text
number 1 is running
time elapsed 2.005152292s
Here is the outcome: 2
```

Here we can see that our time elapsed has halved! This is because our tokio::spawn sends the task to the tokio worker thread to be executed while the main thread processes the sleep function in the main function. However, if we increase the number of tasks spawning by one with the code below:

```rust
let future_one = tokio::spawn(do_something(1));
let future_two = tokio::spawn(do_something(2));
let two_seconds = time::Duration::new(2, 0);
thread::sleep(two_seconds);
let outcome = future_one.await.unwrap();
let _ = future_two.await.unwrap();
```

Our time elapsed increases to roughly four seconds. This is because we only have one worker thread running for the async runtime, and the sleep function is blocking. We could increase the number of threads to two to get the time elapsed back down to two seconds. However, this is not scalable as we could have thousands of async tasks. It would not make sense to spin up thousands of threads. Instead, we can make our sleep function async by using the tokio sleep function for our do_something function, giving this function the following form:

```rust
async fn do_something(number: i8) -> i8 {
    println!("number {} is running", number);
    let two_seconds = time::Duration::new(2, 0);
    tokio::time::sleep(two_seconds).await;
    return 2
}
```

The await syntax of the sleep function enables the task executor to switch to other async tasks to progress them while the sleep duration passes. If we run our main function again, we can see that the duration time is back down to two seconds.

We have now seen the potential benefits of async rust. However, do we really understand what is going on under the hood? To solidify our knowledge of what is going under hood, let's implement our own async task queue.

## Implementing our own async task queue

To get an appreciation for what is happening when we see async code in our web server, we are going to implement our own queue that schedules async tasks by carrying out the following steps:

1. Creating an async task queue that schedules async tasks so the main branch can spawn such tasks.
2. Building our own async sleep functionality.
3. Test running our async queue in the main thread.

Before we implement any of these steps, we need the following dependencies:

```toml
[dependencies]
async-task = "4.7.0"
futures-lite = "2.2.0"
once_cell = "1.19.0"
flume = "0.11.0"
```

When we go through the steps, we will demonstrate how and when we will use such dependencies. Before we get the chance however, we also need to use the following structs and traits:

```rust
use std::{future::Future, panic::catch_unwind, thread};
use std::time::{Duration, Instant};
use std::pin::Pin;
use std::task::{Context, Poll};
use async_task::{Runnable, Task};
use futures_lite::future;
use once_cell::sync::Lazy;
```

We are now ready to move onto our first step: creating an async task queue.

Our async queue needs to be ergonomic. This means that we must have a function that a developer can use to just spawn the task and not think about anything else, just like the tokio::spawn function we used in the previous section. Our spawn_task function takes the following form:

```rust
fn spawn_task<F, T>(future: F) -> Task<T>
where
    F: Future<Output = T> + Send + 'static,
    T: Send + 'static,
{
    . . .
}
```

We can see that we pass in a future and return a task. Here, the future needs to implement the Future trait for the same outcome type of the task that is returned from the function. We can also see that the future needs to implement the Send trait, meaning that the future can be sent between threads. This makes sense as we will have another thread processing async tasks.

We must also note that the future needs to have a 'static lifetime. This means that the future needs to have a lifetime of the entire program. This is because a developer has the right to send a future to be spawned and never ever wait for that future to be finished. We could also have a future that just never completes. Therefore, we could have async tasks being processed for the entire lifetime of the program. We can also see that the task needs to be sent between threads and that it should also have a 'static lifetime for the same reasons as with the future.

Inside our spawn_task function we need to define our async queue which is done using the code below:

```rust
static QUEUE: Lazy<flume::Sender<Runnable>> = Lazy::new(|| {
    . . .
});
```

Our queue is a flume channel with runnables in it. A channel is essentially a queue with a sender and receiver. The sender and receiver can be in different threading, meaning that channels can send data over threads. Every time we spawn a task, we get a runnable handle that the runnable is used to poll the task to see if it has completed.

Our QUEUE variable in the sender that we can use to send futures to be scheduled and ran. We can also see that there is a Lazy::new. This means that the code inside the lazy block only gets evaluated once and the outcome and state of that lazy block is stored throughout the lifetime of the program. This means that no matter how many times we call the spawn_task function, we only construct one queue, once. All other spawn_task function calls after the initial call are referencing the same queue defined in the spawn_task function's initial call.

Inside our lazy block, we define our channel, spawn a thread, and process incoming futures from the channel in that thread, returning the transmitter so that our code outside the lazy block can send messages to in the following code:

```rust
static QUEUE: Lazy<flume::Sender<Runnable>> = Lazy::new(|| {
    let (tx, rx) = flume::unbounded::<Runnable>();
    thread::spawn(move || {
        while let Ok(runnable) = rx.recv() {
            let _ = catch_unwind(|| runnable.run());
        }
    });
    tx
});
```

The runnable.run() function is polling the future. We will cover polling in the next step when we build our own sleeping future. The catch_unwind basically catches any panics. We do not need the catch_unwind function to get our system running, however it would be a brittle system if a developer builds an async function that errored, and this erroring broke our entire async runtime.

Finally, we need to schedule our runnable when we send it to our queue to be polled, returning a task handle so the developer spawning the task can wait for the result with the code below:

```rust
let schedule = |runnable| QUEUE.send(runnable).unwrap();
let (runnable, task) = async_task::spawn(future, schedule);
runnable.schedule();
return task
```

Our spawn_task function is now complete. Let us move onto building our own sleep future.

In the previous step, we mentioned polling as a concept. In Rust, a future is essentially a coroutine that keeps its own state and can be polled. When the future is polled, the polling function can either return a ready or pending outcome. If the outcome is ready, when the future is completed, the return value is given to the context polling the future. If the future is pending, then the future is put into a cycle to be polled again. The exact implementations of the cycle to be polled can differ between async runtimes, but essentially, the async executor will come back to the future to poll it again until the future is ready.

For our sleep future, we do not want to hold up the executor, otherwise the executor will not be able to process other futures while our sleeping future is blocking the executor thread. We can achieve this using timestamps instead of a sleep function. This approach means that we create a timestamp of when the sleep future was created, and we also give a duration of the sleep. These two values mean that we can check the current time of when the future is polled and calculate the time elapsed. We can see this in action when we create our poll function.

Before we do that, we must define our async sleep struct with the following code:

```rust
struct AsyncSleep {
    start_time: Instant,
    duration: Duration,
}
impl AsyncSleep {
    fn new(duration: Duration) -> Self {
        Self {
            start_time: Instant::now(),
            duration,
        }
    }
}
```

Now that our struct is defined, we can implement the Future trait for our struct with the code below:

```rust
impl Future for AsyncSleep {
    type Output = ();
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        . . .
    }
}
```

The Pin stops the AsyncSleep struct from moving in memory, so we do not hit the wrong memory address when polling the future. The context is where we get the waker so that we can wakeup the future so that the future can be polled again. Unfortunately, we must remember that this is a web programming book, and we are covering async to understand web programming at a deeper level. Entire books have been written on async Rust, if we keep going deeper into async, the topic of async would consume the whole book, so we will move onto the logic inside our poll function that takes the following form:

```rust
let elapsed_time = self.start_time.elapsed();
if elapsed_time >= self.duration {
    Poll::Ready(())
} else {
    cx.waker().wake_by_ref();
    Poll::Pending
}
```

Here we can see that we calculate the elapsed time. If the time has elapsed, we then return a ready. If not, we return a pending. Calculating the elapsed time means that our sleeping future will temporarily block the executor to see if the time has elapsed. If the time has not elapsed, then the executor moves onto the next future to poll and will come back later to check if the time has elapsed again by polling it again. This means that we can process thousands of sleep functions on our single thread. With this in mind, we can now move onto running our async code in our main function.

When it comes to calling our async sleep future, we can stack our future in another future, which is an async function with the following code:

```rust
async fn sleeping(label: u8) {
    println!("sleeping {}", label);
    AsyncSleep::new(Duration::from_secs(3)).await;
    println!("progressing sleep {}", label);
    AsyncSleep::new(Duration::from_secs(2)).await;
    println!("done sleeping {}", label);
}
```

Here, we can see that our async function merely sleeps twice and prints out simple progress statements as the async function progresses.

We can test our async code in our main function with the code below:

```rust
fn main() {
    let handle_one = spawn_task(sleeping(1));
    let handle_two = spawn_task(sleeping(2));
    let handle_three = spawn_task(sleeping(3));
    println!("before the sleep");
    std::thread::sleep(Duration::from_secs(5));
    println!("before the block");
    future::block_on(handle_one);
    future::block_on(handle_two);
    future::block_on(handle_three);
}
```

Here, we can see that we spawn three async tasks that are all going to sleep for 5 seconds each. We then block the main thread. This is to test that our async sleeps are truly async. If they are not, then we will not get through all our sleep functions before the main sleep is finished.

We have now implemented everything we need to run our program. In running it, we get the following printout:

```text
before the sleep
sleeping 1
sleeping 2
sleeping 3
progressing sleep 1
progressing sleep 2
progressing sleep 3
done sleeping 1
done sleeping 2
done sleeping 3
before the block
```

Here, we can see that all our sleep functions execute before the sleep in the main thread has finished, this means that our system is truly async! However, if this was a bit of a headache for you, do not worry, in Rust web programming, you will not be asked to manually implement async code for your server. Most of the async functionality has been built for you, but we do need to have a grasp of what's going under the hood when we are calling these async implementations.

Now that we have seen what async is, we need to understand it in the bigger context of web programming. We will start this by exploring high level concepts of tokio.

## Exploring high-level concepts of tokio

There is some contention about this, but tokio is the main async runtime that web frameworks run on. Unlike our single threaded async queue, tokio has multiple threads with task stealing as seen in figure 3.2.

Figure 3.2 – Speeding up the Tokio runtime [Source: Tokio Documentation (2019) (`https://tokio.rs/blog/2019-10-scheduler`)]

Here, we can see that the tokio runtime has multiple worker threads that all have their own queues. On top of multiple workers, tokio also implements "task stealing". This is where the worker will steal a task from another queue if that worker does not have its own tasks to process. In the first section of this chapter, we used tokio to display some functionality of async runtimes. However, we restricted the number of worker threads to one to avoid the task stealing from masking how blocking works. We can see that our simple implementation of an async task queue does not really match up to the full features that a runtime like tokio offers.

Unless you have some specific needs for your runtime, it is advised that you use an established runtime like tokio for your application. Another concept to seriously consider is the interaction of other crates. For instance, a HTTP request is a good use of async, as there is no CPU usage when waiting for a response from a server. Because the CPU is idle, it makes sense to switch context and process something else when waiting for a response from the server. This is why most of the networking libraries and crates support async. However, when someone builds out a library that supports async, the library must support that specific runtime. This may be seen as controversial by some but when a library supports async, chances are that they have implemented async interfaces for the tokio runtime.

To get an appreciation for how tokio and other async runtimes are used in web programming, we can look at the standard example of a TCP server supported by tokio using the code below:

```rust
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let addr = env::args()
        .nth(1)
        .unwrap_or_else(|| "127.0.0.1:8080".to_string());
    let listener = TcpListener::bind(&addr).await?;
    println!("Listening on: {}", addr);
    loop {
        // Asynchronously wait for an inbound socket.
        let (mut socket, _) = listener.accept().await?;
        tokio::spawn(async move {
            // process the TCP request
        });
    }
}
```

Here, we can see that the TCP listener is defined in the main thread. We also run a continuous loop in the main thread listening for new incoming TCP requests. When we get a new TCP request, we spawn a new async task to process that request, and then go back to listening for more incoming TCP requests. This means that our main thread is not held up processing requests. Instead, our main thread can spend all the time listening for requests.

Seeing as HTTP is built on top of TCP, we can assume that most HTTP web frameworks will be passing their requests off to the async runtime to be processed. This highlights a potentially big problem. If our async tasks handling a request have a long blocking component, this could severely slow down our ability to process new incoming requests. You will see this advice a lot online. The general mantra is not to perform CPU heavy tasks in async. This one-liner is not true for all use cases however. Essentially, an async runtime gives you an easy way to access a thread pool and for those worker threads to cycle through async tasks polling them and coming back to them if there are multiple awaits in the async task. If the context makes sense, you can use your async runtime how you like. However, in terms of using an async runtime to handle incoming network requests, we should avoid CPU heavy async tasks otherwise we will block new incoming requests from being processed. This does not mean that we can never have CPU intensive code for our API endpoints though.

In tokio, we can handle blocking tasks with the tokio::spawn_blocking function. This essentially passes the blocking async task to another thread pool, so our handling of new incoming requests is not blocked. To get a feel for how tokio handles incoming http requests, we can move onto implementing a HTTP server with Hyper.

## Implementing a HTTP server in Hyper

The Hyper crate is essentially layer layer above tokio. While we will not be using the Hyper crate throughout the book as the low-level nature of the Hyper crate would require us to write a lot of boilerplate code, exploring a simple Hyper server will help us to appreciate the basic handling of a HTTP request.

Before we write any server code, we will need the following dependencies:

```toml
[dependencies]
hyper = { version = "1.2.0", features = ["full"] }
tokio = { version = "1.36.0", features = ["full"] }
http-body-util = "0.1.1"
hyper-util = { version = "0.1.3", features = ["full"] }
```

With these dependencies, we are going to use the following structs and traits in our main.rs file with the code below:

```rust
use std::convert::Infallible;
use std::net::SocketAddr;
use http_body_util::Full;
use hyper::body::Bytes;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Request, Response};
use hyper_util::rt::TokioIo;
use tokio::net::TcpListener;
```

We can see how the preceding code is used as we build the server. Before we write any code in the main function, we can build our function that handles all the incoming requests with the following code:

```rust
async fn handle(body: Request<hyper::body::Incoming>) -> Result<Response<Full<Bytes>>, Infallible> {
    println!("Received a request");
    println!("Headers: {:?}", body.headers());
    println!("Body: {:?}", body.into_body());
    Ok(Response::new(Full::new(Bytes::from("Hello, World!"))))
}
```

Here, we can see that we just print out the header of the request, and the body. After printing out the request data, we merely return a simple hello world message. Our handle function would be a good place to perform routing to other async functions but for our example, we are just going to return the simple message no matter what endpoint, or method you use. We can see that the error type is Infallible. This basically means that it cannot happen. Infallible is essentially a placeholder to map results. This makes sense in our case as you can see that we have no unwraps to access the data we are printing. Throughout the book we will build our own custom error handling, but for this simple example, Infallible is useful to us.

For our main function, we run our server with the code below:

```rust
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    let listener = TcpListener::bind(addr).await?;
    loop {
        let (stream, _) = listener.accept().await?;
        let io = TokioIo::new(stream);
        tokio::task::spawn(async move {
            . . .
        });
    }
}
```

As we can see, we are binding a TCP listener to a port in the main thread. We also run an infinite loop in the main thread listening to incoming requests, and spawning async tasks to handle these requests. The difference here is the following line:

```rust
let io = TokioIo::new(stream);
```

The TokioIo struct is from the Hyper utils crate. This TokioIo struct is essentially a wrapper around the implementation of the Tokio IO traits.

Now that we have adapted the tokio stream of bytes from the request, we can now work with the hyper crate. Inside our | block we handle the stream of bytes with the following code:

```rust
if let Err(err) = http1::Builder::new()
    .serve_connection(io, service_fn(handle))
    .await {
    println!("Error serving connection: {:?}", err);
}
```

Here, we serve a HTTP 1 connection our handle function. If we run our server using the cargo run command, we can make a HTTP GET request by putting the server URL in the browser to get the result shown in figure 3.3.

Figure 3.3 – Browser view of our Hyper server HTTP request

If we look at our terminal that is running our server, the data in the request header and body will be printed out (providing the printout in the book would just needlessly bloat the chapter). We can see that our is running as we expected, but what about HTTP 2?

HTTP 2 is a binary protocol. With HTTP 2 you can also do multiplexing, and encryption, which is the HTTPS that you see on most (nearly all) modern websites. We will cover HTTPS in chapter 15. Right now, we are going to bypass the encryption, and just implement HTTP 2 for our Hyper server.

Before we alter our server, we need to use the additional following:

```rust
use hyper::server::conn::http2;
use hyper::rt::Executor;
use std::future::Future;
```

We now need to implement our async runtime adapter for Hyper so that our Hyper server can send the requests to the async runtime with the following code:

```rust
#[derive(Clone)]
struct TokioExecutor;
impl<F> Executor<F> for TokioExecutor
where
    F: Future + Send + 'static,
    F::Output: Send + 'static,
{
    fn execute(&self, future: F) {
        tokio::spawn(future);
    }
}
```

We can put whatever we want in the execute function if it is spawning a task on an async runtime. As we can see, the traits needed for the future being passed into the execute function are the same requirements for the spawn_task function we defined when building our own async task queue.

We can now swap our HTTP 1 builder out for our HTTP 2 builder with the code below:

```rust
if let Err(err) = http2::Builder::new(TokioExecutor)
```

However, if we run our server again and try to refresh our browser page, we will get a response that the page is not working as depicted in figure 3.4.

Figure 3.4 – Browser view of our Hyper server HTTP 2 request

This is because, by default, most browsers implement HTTPS connections when performing a HTTP 2 request. Seeing as we are not implementing HTTPS right now, we must test our connection using CURL to bypass this step with the following command:

```bash
curl --http2-prior-knowledge http://127.0.0.1:3000/
```

The CURL terminal command then returns the hello world message. With this example, we can now see how async runtimes interact with web servers.

## Summary

We have come to the end of our async exploration. As mentioned earlier in the chapter, we could keep going to the point of writing an entire book on async rust, however, we have learnt enough async rust to navigate web programming in rust efficiently.

We now understand what an async future is and how it passes through the async runtime. Furthermore, we also know how web servers utilize async runtimes to handle incoming network requests. Now that you have a deeper understanding of async, we are going to be handling HTTP requests in the next chapter with async functions and a web framework.

## Questions

1. What is the difference between multiple threads and multiple async tasks?
2. How can I create an async sleep future that is non-blocking?
3. How does a web service typically use an async runtime to handle incoming network requests?
4. Why is it a bad idea to have CPU intensive async functions for API endpoints?
5. How can we handle CPU intensive async functions?

## Answers

1. A thread has it's own memory and processes CPU tasks. Multiple threads can process multiple CPU computations at the same time. Async tasks have their own state and can be polled to see if they are completed or not. Async tasks are usually for non-blocking tasks such as waiting for a response from a network. Because these async tasks are non-blocking, a single thread can handle multiple async tasks, looping through and polling tasks to see if they are finished or not.

2. We can create a non-blocking sleep future by creating a struct that has a field with the time that the struct was created, and another field for the duration of the sleep. We then implement the Future trait for the struct where the poll function gets the current time, calculates the time elapsed from the created field, and return a Ready if the duration has passed, or a Pending if the duration has not passed.

3. A typical web service creates a TCP listener on a port in the main thread. The service then starts an infinite loop in the main thread listening for incoming network requests. Once an incoming network request is accepted, the bytes from the network request are then passed into an async task and that async task is passed to the async runtime to handle so that the main thread can go back to listening for incoming requests.

4. An async function is an async task that gets processed on the async runtime. If the async task is too CPU intensive, it can block the worker threads of the async runtime which will block the processing and progression of new incoming network requests.

5. We can handle CPU intensive async functions by using the tokio::spawn_blocking function. This essentially passes the blocking async task to another thread pool, so our handling of new incoming requests is not blocked.
