# Advanced Features

By now, you’ve learned the most commonly used parts of the Rust programming
language. Before we do one more project, in Chapter 21, we’ll look at a few
aspects of the language you might run into every once in a while, but may not
use every day. You can use this chapter as a reference for when you encounter
any unknowns. The features covered here are useful in very specific situations.
Although you might not reach for them often, we want to make sure you have a
grasp of all the features Rust has to offer.

In this chapter, we’ll cover:

* Unsafe Rust: how to opt out of some of Rust’s guarantees and take
  responsibility for manually upholding those guarantees
* Advanced traits: associated types, default type parameters, fully qualified
  syntax, supertraits, and the newtype pattern in relation to traits
* Advanced types: more about the newtype pattern, type aliases, the never type,
  and dynamically sized types
* Advanced functions and closures: function pointers and returning closures
* Macros: ways to define code that defines more code at compile time

It’s a panoply of Rust features with something for everyone! Let’s dive in!

## Unsafe Rust

All the code we’ve discussed so far has had Rust’s memory safety guarantees
enforced at compile time. However, Rust has a second language hidden inside it
that doesn’t enforce these memory safety guarantees: it’s called _unsafe
Rust_. It works just like regular Rust, but gives you extra “superpowers.”

Unsafe Rust exists because, by nature, static analysis is conservative. When
the compiler tries to determine whether or not code upholds the guarantees,
it’s better for it to reject some valid programs than to accept some invalid
programs. Although the code might be okay, if Rust’s compiler doesn’t have
enough information to be confident, it will reject the code. In these cases,
you can use unsafe code to tell the compiler, “Trust me, I know what I’m
doing.” The downside is that you use it at your own risk: if you misuse unsafe
code, problems like null pointer dereferencing can occur.

Another reason Rust has an unsafe alter ego is that the underlying computer
hardware is inherently unsafe. If Rust didn’t let you do unsafe operations,
you couldn’t do certain tasks. Rust needs to allow you to do low-level
systems programming, such as directly interacting with the operating system
or even writing your own operating system. Working with low-level systems
programming is one of the goals of the language.

### Unsafe Superpowers

To switch to unsafe Rust, use the `unsafe` keyword and then start a new block
that holds the unsafe code. You can take five actions in unsafe Rust that you
can’t in safe Rust, which we call “unsafe superpowers.” Those superpowers
include the ability to:

* Dereference a raw pointer
* Call an unsafe function or method
* Access or modify a mutable static variable
* Implement an unsafe trait
* Access fields of a `union`

It’s important to understand that `unsafe` doesn’t turn off the borrow checker
or disable any other of Rust’s safety checks: if you use a reference in unsafe
code, it will still be checked. The `unsafe` keyword only gives you access to
these five features that are then not checked by the compiler for memory
safety. You’ll still get some degree of safety inside of an unsafe block.

In addition, `unsafe` does not mean the code inside the block is necessarily
dangerous or that it will definitely have memory safety problems: the intent
is that as a programmer, you’ll ensure the code inside an `unsafe` block will
access memory in a valid way.

People are fallible, and mistakes will happen, but by requiring these five
unsafe operations to be inside blocks annotated with `unsafe`, you’ll know
that any memory safety related errors must be within an `unsafe` block. Keep
`unsafe` blocks small; you’ll thank yourself later when you investigate
memory bugs.

To isolate unsafe code as much as possible, it’s best to enclose unsafe code
within a safe abstraction and provide a safe API, which we’ll discuss later
in the chapter when we examine unsafe functions and methods. Parts of the
standard library are implemented as safe abstractions over unsafe code that’s
been audited. Wrapping unsafe code in a safe abstraction prevents uses of
`unsafe` from leaking out into all the places that you or your users might
want to use the functionality implemented with `unsafe` code, because using a
safe abstraction is safe.

Let’s look at each of the five unsafe superpowers in turn. We’ll also look at
some abstractions that provide a safe interface to unsafe code.

#### Dereferencing a Raw Pointer

In “Smart Pointers” in Chapter 15, we mentioned that the borrow checker
prevents dangling references and double references. Raw pointers are
different: they

* Are allowed to ignore the borrowing rules by having both immutable and
  mutable pointers or multiple mutable pointers to the same location
* Aren’t guaranteed to point to valid memory
* Can be null
* Don’t implement any automatic cleanup

By opting out of having Rust enforce these guarantees, you can give up
guaranteed safety in exchange for greater performance or the ability to
interface with another language or hardware where Rust’s guarantees don’t
apply.

Listing 20-1 shows how to create an immutable and a mutable raw pointer from
references.

```rust
let mut num = 5;

let r1 = &num as *const i32;
let r2 = &mut num as *mut i32;
```

Listing 20-1: Creating raw pointers from references

Notice that we don’t include the `unsafe` keyword in this code. We can create
raw pointers in safe code, but we can’t dereference raw pointers outside an
unsafe block.

To demonstrate this, next we’ll create a raw pointer whose validity we can’t
be so certain of. Listing 20-2 shows how to create a raw pointer to an
arbitrary location in memory. Trying to use arbitrary memory is undefined:
there might be data at that address or there might not, the compiler might
optimize the code so there isn’t even a memory access, or the program might
error with a segmentation fault. Usually, there is no good reason to write
code like this, but it is possible.

```rust
let address = 0x012345usize;
let r = address as *const i32;
```

Listing 20-2: Creating a raw pointer to an arbitrary memory address

We’ve created raw pointers by using `as` to cast an immutable and a mutable
reference into their corresponding raw pointer types. Because we created them
directly from references guaranteed to be valid, we know these particular raw
pointers are valid, but we can’t make that assumption about just any raw
pointer.

To demonstrate this, we use the dereference operator `*` on a raw pointer that
requires an `unsafe` block, as shown in Listing 20-3.

```rust
let mut num = 5;

let r1 = &num as *const i32;
let r2 = &mut num as *mut i32;

unsafe {
    println!("r1 is: {}", *r1);
    println!("r2 is: {}", *r2);
}
```

Listing 20-3: Dereferencing raw pointers within an `unsafe` block

Creating a pointer does no harm; it’s only when we try to access the value
that it points at that we might end up dealing with an invalid value.

Note also that in Listing 20-1 and 20-3, we created `*const i32` and `*mut i32`
raw pointers that both pointed to the same memory location, where `num` is
stored. If we instead tried to create an immutable and a mutable reference to
`num`, the code would not have compiled because Rust’s ownership rules don’t
allow a mutable reference at the same time as any immutable references. With
raw pointers, we can create a mutable raw pointer and an immutable raw pointer
to the same location and change data through the mutable raw pointer, possibly
creating a data race. Be careful!

With all of these dangers, why would you ever use raw pointers? One major use
case is when interfacing with C code, as you’ll see in the next subsection on
unsafe functions. Another scenario is when building safe abstractions that the
borrow checker doesn’t understand. We’ll introduce unsafe functions and then
look at an example of a safe abstraction that uses unsafe code.

#### Calling an Unsafe Function or Method

The second type of operation that requires an unsafe block is calling unsafe
functions. Unsafe functions and methods look exactly like regular functions
and methods, but they have an extra `unsafe` before the rest of the
definition. The `unsafe` keyword in this context indicates the function has
requirements we need to uphold when we call this function, because Rust can’t
guarantee we’ve met these requirements. By calling an unsafe function within
an `unsafe` block, we’re saying that we’ve read this function’s documentation
and take responsibility for upholding the function’s contracts.

Here is an unsafe function that has no safe equivalent:

```rust
unsafe fn dangerous() {}

unsafe {
    dangerous();
}
```

We must call the `dangerous` function within a separate `unsafe` block. If we
try to call `dangerous` without the `unsafe` block, we’ll get an error:

```text
error[E0133]: call to unsafe function `dangerous` is unsafe and requires unsafe function or block
 --> src/main.rs:4:5
  |
4 |     dangerous();
  |     ^^^^^^^^^^^ call to unsafe function
```

By putting the `unsafe` block around our call to `dangerous`, we’re asserting
to Rust that we’ve read the function’s documentation, we understand how to use
it properly, and we’ve verified that we’re fulfilling the contract of the
function.

Unsafe functions bodies are effectively `unsafe` blocks, so to perform other
unsafe operations within an unsafe function, we don’t need to add another
`unsafe` block.

#### Creating a Safe Abstraction over Unsafe Code

Just because a function contains unsafe code doesn’t mean we need to mark the
entire function as unsafe. In fact, wrapping unsafe code in a safe function is
a common abstraction. As an example, let’s study the `split_at_mut` function
from the standard library, which requires some unsafe code. We’ll explore how
we might implement it. This safe method is defined on mutable slices: it takes
one slice and makes it two by splitting the slice at the index given as an
argument. Listing 20-4 shows how to use `split_at_mut`.

```rust
let mut v = vec![1, 2, 3, 4, 5, 6];

let r = &mut v[..];

let (a, b) = r.split_at_mut(3);

assert_eq!(a, &mut [1, 2, 3]);
assert_eq!(b, &mut [4, 5, 6]);
```

Listing 20-4: Using the safe `split_at_mut` function

We can’t implement this function using only safe Rust. An attempt might look
like Listing 20-5, which doesn’t compile. For simplicity, we’ll implement
`split_at_mut` as a function rather than a method and only for slices of
`i32` values rather than for a generic type `T`.

```rust
fn split_at_mut(slice: &mut [i32], mid: usize) -> (&mut [i32], &mut [i32]) {
    let len = slice.len();

    assert!(mid <= len);

    (&mut slice[..mid], &mut slice[mid..])
}
```

Listing 20-5: An attempted implementation of `split_at_mut` using only safe Rust

This function first gets the total length of the slice. Then it asserts that
the index given as a parameter is within the slice by checking whether it’s
less than or equal to the length. The assertion means that if we pass an
index that is greater than the length, the function will panic before it
attempts to use that index.

Then we return two mutable slices in a tuple: one from the start of the
original slice to the `mid` index, and another from the `mid` index to the
end.

When we try to compile this code, we’ll get an error:

```text
error[E0499]: cannot borrow `*slice` as mutable more than once at a time
 --> src/main.rs:6:30
  |
4 | fn split_at_mut(slice: &mut [i32], mid: usize) -> (&mut [i32], &mut [i32]) {
  |                    - let's call the lifetime of this reference `'1`
5 |     let len = slice.len();
6 |     (&mut slice[..mid], &mut slice[mid..])
  |      ----------------  ^^^^^^^^^^^^^^^^^^ `*slice` was mutably borrowed here in the previous borrow
  |      |
  |      first mutable borrow occurs here
  |      second mutable borrow occurs here
7 | }
```

Rust’s borrow checker can’t understand that we’re borrowing different parts
of the slice; it only knows that we’re borrowing from the same slice twice.
Borrowing different parts of a slice is fundamentally okay because the two
slices aren’t overlapping, but Rust isn’t smart enough to know this. When we
know something is okay, but Rust doesn’t, it’s time to reach for unsafe code.

Listing 20-6 shows how to use an `unsafe` block, a raw pointer, and some calls
to unsafe functions to make the implementation of `split_at_mut` work.

```rust
use std::slice;

fn split_at_mut(slice: &mut [i32], mid: usize) -> (&mut [i32], &mut [i32]) {
    let len = slice.len();
    let ptr = slice.as_mut_ptr();

    assert!(mid <= len);

    unsafe {
        (
            slice::from_raw_parts_mut(ptr, mid),
            slice::from_raw_parts_mut(ptr.add(mid), len - mid),
        )
    }
}
```

Listing 20-6: Using unsafe code in the implementation of the `split_at_mut` function

Recall from “The Slice Type” in Chapter 4 that slices are a pointer to some
data and the length of the slice. We use the `as_mut_ptr` method to access the
raw pointer of a slice and then we can use the `from_raw_parts_mut` function
from the `slice` module to create new slices from raw pointers. The
`from_raw_parts_mut` function is unsafe because it takes a raw pointer and
must trust that this pointer is valid. The `add` method on raw pointers is
also unsafe, because it must trust that the offset location is also a valid
pointer. Therefore, we had to put an `unsafe` block around our calls to
`from_raw_parts_mut` and `ptr.add` so we could call them. By looking at the
code and by adding the assertion that `mid` must be less than or equal to
`len`, we can tell that all the raw pointers used within the `unsafe` block
will be valid pointers to data within the slice. This is an acceptable and
appropriate use of `unsafe`.

Note that we don’t need to mark the resulting `split_at_mut` function as
`unsafe`, and we can call this function from safe Rust. We’ve created a safe
abstraction to the unsafe code with an implementation of the function that
uses `unsafe` code in a safe way, because it creates only valid pointers from
the data this function has access to.

In contrast, the use of `slice::from_raw_parts_mut` in Listing 20-7 would
likely crash when the slice is used. This code takes an arbitrary memory
location and creates a slice 10,000 items long.

```rust
use std::slice;

let address = 0x01234usize;
let r = address as *mut i32;

let slice: &[i32] = unsafe {
    slice::from_raw_parts_mut(r, 10000)
};
```

Listing 20-7: Creating a slice from an arbitrary memory location

We don’t own the memory at this arbitrary location, and there’s no guarantee
that the slice this code creates contains valid `i32` values. Using `slice`
would be undefined behavior.

#### Using `extern` Functions to Call External Code

Sometimes, your Rust code might need to interact with code written in another
language. For this, Rust has the keyword `extern` that facilitates the creation
and use of a _Foreign Function Interface_ (FFI). An FFI is a way for a
programming language to define functions and enable a different (foreign)
programming language to call those functions.

Listing 20-8 demonstrates how to set up an integration with the `abs` function
from C. Functions declared within `extern` blocks are always unsafe to call
from Rust code. The reason is that other languages don’t enforce Rust’s rules
and guarantees, and Rust can’t check them, so responsibility falls on the
programmer to ensure safety.

```rust
extern "C" {
    fn abs(input: i32) -> i32;
}

fn main() {
    unsafe {
        println!("Absolute value of -3 according to C: {}", abs(-3));
    }
}
```

Listing 20-8: Declaring and calling an `extern` function defined in another language

Within the `extern "C"` block, we list the names and signatures of external
functions from another language we want to call. The `"C"` part defines which
_application binary interface_ (ABI) the external function uses: the ABI
defines how to call the function at the assembly level. The `"C"` ABI is the
most common and follows the C programming language’s ABI.

> #### Calling Rust Functions from Other Languages
>
> We can also use `extern` to create an interface that allows other languages
> to call Rust functions. Instead of an `extern` block, we add the `extern`
> keyword and specify the ABI to use just before the `fn` keyword. We also
> need to add a `#[no_mangle]` annotation to tell the Rust compiler not to
> mangle the name of this function. Mangling is when a compiler changes the
> name we’ve given a function to a different name that contains more
> information for other parts of the compilation process, but is less
> readable for humans. Every programming language compiler mangles names
> slightly differently, so for a Rust function to be nameable by other
> languages, we must disable the Rust compiler’s name mangling.
>
> In the following example, we make the `call_from_c` function accessible from
> C code, after it’s compiled to a shared library and linked from C:
>
> ```rust
> #[no_mangle]
> pub extern "C" fn call_from_c() {
>     println!("Just called a Rust function from C!");
> }
> ```
>
> This usage of `extern` does not require `unsafe`.

#### Accessing or Modifying a Mutable Static Variable

In this book, we haven’t yet talked about _global variables_, which Rust does
support but can be problematic with Rust’s ownership rules. If two threads are
accessing the same mutable global variable, it can cause a data race.

In Rust, global variables are called _static variables_. Listing 20-9 shows an
example declaration and use of a static variable with a string slice as a
value.

```rust
static HELLO_WORLD: &str = "Hello, world!";

fn main() {
    println!("name is: {}", HELLO_WORLD);
}
```

Listing 20-9: Defining and using an immutable static variable

Static variables are similar to constants, which we discussed in “Constants”
in Chapter 3. The names of static variables are in `SCREAMING_SNAKE_CASE` by
convention. Static variables can only store references with the `'static`
lifetime, which means the Rust compiler can figure out the lifetime and we
aren’t required to annotate it explicitly. Accessing an immutable static
variable is safe.

A subtle difference between constants and immutable static variables is that
values in a static variable have a fixed address in memory. Using the value
will always access the same data. Constants, on the other hand, are allowed to
duplicate their data whenever they’re used. Another difference is that static
variables can be mutable. Accessing and modifying mutable static variables is
_unsafe_. Listing 20-10 shows how to declare, access, and modify a mutable
static variable called `COUNTER`.

```rust
static mut COUNTER: u32 = 0;

fn add_to_count(inc: u32) {
    unsafe {
        COUNTER += inc;
    }
}

fn main() {
    add_to_count(3);

    unsafe {
        println!("COUNTER: {}", COUNTER);
    }
}
```

Listing 20-10: Reading from or writing to a mutable static variable is unsafe

As with regular variables, we specify mutability using the `mut` keyword. Any
code that reads or writes from `COUNTER` must be within an `unsafe` block. This
code compiles and prints `COUNTER: 3` as we expected because it’s single
threaded. Having multiple threads access `COUNTER` would likely result in data
races.

With mutable data that is globally accessible, it’s difficult to ensure there
are no data races, which is why Rust considers mutable static variables to be
unsafe. Where possible, it’s preferable to use the concurrency techniques and
thread-safe smart pointers we discussed in Chapter 16 so the compiler checks
that data accessed from different threads is done safely.

#### Implementing an Unsafe Trait

The final action that works only with `unsafe` is implementing an unsafe
trait. A trait is unsafe when at least one of its methods has some invariant
that the compiler can’t verify. We declare that a trait is `unsafe` by adding
the `unsafe` keyword before `trait` and marking the implementation of the trait
as `unsafe` too, as shown in Listing 20-11.

```rust
unsafe trait Foo {
    // methods go here
}

unsafe impl Foo for i32 {
    // method implementations go here
}
```

Listing 20-11: Defining and implementing an unsafe trait

By using `unsafe impl`, we’re promising that we’ll uphold the invariants that
the compiler can’t verify.

As an example, recall the `Sync` and `Send` marker traits we discussed in
“Extensible Concurrency with the `Sync` and `Send` Traits” in Chapter 16: the
compiler implements these traits automatically if our types are composed
entirely of `Send` and `Sync` types. If we implement a type that contains a
type that is not `Send` or `Sync`, such as raw pointers, and we want to mark
that type as `Send` or `Sync`, we must use `unsafe`. Rust can’t verify that our
type upholds the guarantees that it can be safely sent across threads or
accessed from multiple threads; therefore, we need to do those checks manually
and indicate as such with `unsafe`.

#### Accessing Fields of a Union

The final action related to `unsafe` is accessing fields of a _union_. A
`union` is similar to a `struct`, but only one declared field is used in a
particular instance at one time. Unions are primarily used to interface with
unions in C code. Accessing union fields is unsafe because Rust can’t
guarantee the type of the data currently being stored in the union instance.
You can learn more about unions in the Rust Reference.

### When to Use Unsafe Code

Using `unsafe` to take one of the five actions (superpowers) we’ve discussed
isn’t wrong or frowned upon. But it is trickier to get `unsafe` code correct
because the compiler can’t help uphold memory safety. When you have a reason
to use `unsafe` code, you can do so, and having the explicit `unsafe` annotation
makes it easier to track down the source of problems when they occur.

## Advanced Traits

We first covered traits in “Traits: Defining Shared
Behavior” in Chapter 10, but we
didn’t discuss the more advanced details. Now that you know more about Rust, we
can get into the nitty-gritty.

### Associated Types

_Associated types_ connect a type placeholder with a trait such that the trait
method definitions can use these placeholder types in their signatures. The
implementor of the trait will specify the concrete type to be used instead of the
placeholder type for the particular implementation. That way, we can define a
trait that uses some types without needing to know exactly what those types are
until the trait is implemented.

We’ve described most of the advanced features in this chapter as being rarely
needed. Associated types are somewhere in the middle: they’re used more rarely
than features explained in the rest of the book but more commonly than many of
the other features discussed in this chapter.

One example of a trait with an associated type is the `Iterator` trait that the
standard library provides. The associated type is named `Item` and stands in
for the type of the values the type implementing the `Iterator` trait is
iterating over. The definition of the `Iterator` trait is as shown in Listing
20-13.

```rust
pub trait Iterator {
    type Item;

    fn next(&mut self) -> Option<Self::Item>;
}
```

Listing 20-13: The definition of the `Iterator` trait with an associated type `Item`

The type `Item` is a placeholder, and the `next` method’s definition shows that
it will return values of type `Option<Self::Item>`. Implementors of the
`Iterator` trait will specify the concrete type for `Item`, and the `next`
method will return an `Option` containing a value of that concrete type.

Associated types might seem similar to generics, in that the latter allow us
to define a function without specifying what types it can handle. To examine
the difference between the two concepts, we’ll look at an implementation of the
`Iterator` trait on a type named `Counter` that specifies the `Item` type is
`u32`:

```rust
struct Counter {}

impl Iterator for Counter {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        // --snip--
    }
}
```

This syntax seems comparable to that of generics. So why not just define the
`Iterator` trait with generics, as shown in Listing 20-14?

```rust
pub trait Iterator<T> {
    fn next(&mut self) -> Option<T>;
}
```

Listing 20-14: A hypothetical definition of the `Iterator` trait using generics

The difference is that when using generics, as in Listing 20-14, we must
annotate the types in each implementation; because we can also implement
`Iterator<String> for Counter` or any other type, we could have multiple
implementations of `Iterator` for `Counter`. In other words, when a trait has a
generic parameter, it can be implemented for a type multiple times, changing
the concrete types of the generic type parameters each time. When we use the
`next` method on `Counter`, we would have to provide type annotations to
indicate which implementation of `Iterator` we want to use.

With associated types, we don’t need to annotate types because we can’t
implement a trait on a type multiple times. In Listing 20-13 with the
definition that uses associated types, we can choose what the type of `Item`
will be only once because there can be only one `impl Iterator for Counter`. We
don’t have to specify that we want an iterator of `u32` values everywhere we
call `next` on `Counter`.

Associated types also become part of the trait’s contract: implementors of the
trait must provide a type to stand in for the associated type placeholder.
Associated types often have a name that describes how the type will be used,
and documenting the associated type in the API documentation is a good practice.

### Default Generic Type Parameters and Operator Overloading

When we use generic type parameters, we can specify a default concrete type for
the generic type. This eliminates the need for implementors of the trait to
specify a concrete type if the default type works. You specify a default type
when declaring a generic type with the `<PlaceholderType=ConcreteType>` syntax.

A great example of a situation where this technique is useful is _operator
overloading_, in which you customize the behavior of an operator (such as `+`)
in particular situations.

Rust doesn’t allow you to create your own operators or overload arbitrary
operators. But you can overload the operations and corresponding traits listed
in `std::ops` by implementing the traits associated with the operator. For
example, in Listing 20-15 we overload the `+` operator to add two `Point`
instances together. We do this by implementing the `Add` trait on a `Point`
struct:

```rust
use std::ops::Add;

#[derive(Debug, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

fn main() {
    assert_eq!(Point { x: 1, y: 0 } + Point { x: 2, y: 3 },
               Point { x: 3, y: 3 });
}
```

Listing 20-15: Implementing the `Add` trait to overload the `+` operator for `Point` instances

The `add` method adds the `x` values of two `Point` instances and the `y`
values of two `Point` instances to create a new `Point`. The `Add` trait has
an associated type named `Output` that determines the type returned from the
`add` method.

The default generic type in this code is within the `Add` trait. Here is its
definition:

```rust
trait Add<Rhs=Self> {
    type Output;

    fn add(self, rhs: Rhs) -> Self::Output;
}
```

This code should look generally familiar: a trait with one method and an
associated type. The new part is `Rhs=Self`: this syntax is called _default
type parameters_. The `Rhs` generic type parameter (short for “right-hand
side”) defines the type of the `rhs` parameter in the `add` method. If we don’t
specify a concrete type for `Rhs` when we implement the `Add` trait, the type
of `Rhs` will default to `Self`, which will be the type we’re implementing
`Add` on.

When we implemented `Add` for `Point`, we used the default for `Rhs` because we
wanted to add two `Point` instances. Let’s look at an example of implementing
the `Add` trait where we want to customize the `Rhs` type rather than using
the default.

We have two structs, `Millimeters` and `Meters`, holding values in different
units. This thin wrapper around an existing type in a newtype pattern, as we
described in “Using the Newtype Pattern for Type Safety and Abstraction” in
Chapter 19, is useful for type safety and making our intent more explicit. We
want to add values in millimeters to values in meters and have the
implementation of `Add` do the conversion correctly. We can implement `Add`
for `Millimeters` with `Meters` as the `Rhs`, as shown in Listing 20-16.

```rust
use std::ops::Add;

struct Millimeters(u32);
struct Meters(u32);

impl Add<Meters> for Millimeters {
    type Output = Millimeters;

    fn add(self, other: Meters) -> Millimeters {
        Millimeters(self.0 + (other.0 * 1000))
    }
}
```

Listing 20-16: Implementing the `Add` trait on `Millimeters` to add `Millimeters` to `Meters`

To add `Millimeters` and `Meters`, we specify `impl Add<Meters>` to set the
value of the `Rhs` type parameter instead of using the default of `Self`.

You’ll use default type parameters in two main ways:

* To extend a type without breaking existing code
* To allow customization in specific cases most users won’t need

The `Add` trait is an example of the second purpose: usually, you’ll add two
like types, but the `Add` trait provides the ability to customize beyond that.
Using a default type parameter in the `Add` trait definition means you don’t
have to specify the extra parameter most of the time. In other words, a bit of
implementation boilerplate isn’t needed, making it easier to use the trait.

The first purpose is similar, but in reverse: if you want to add a type
parameter to an existing trait, you can give it a default to allow extension of
the functionality without breaking the existing implementation code.

### Disambiguating Between Methods with the Same Name

It can happen that two traits both require a method with the same name, and
Rust won’t know which one you’re referring to if you try to call that method
on a type that implements both traits. Or, when implementing both traits on a
type, Rust doesn’t know which implementation of the method you want if you
don’t disambiguate.

For example, in Listing 20-17, we have traits `Pilot` and `Wizard` that both
have a method named `fly`. Each has a different implementation of the method.
Then we implement both traits on a type `Human` that already has a method
named `fly` implemented on it.

```rust
trait Pilot {
    fn fly(&self);
}

trait Wizard {
    fn fly(&self);
}

struct Human;

impl Pilot for Human {
    fn fly(&self) {
        println!("This is your captain speaking.");
    }
}

impl Wizard for Human {
    fn fly(&self) {
        println!("Up!");
    }
}

impl Human {
    fn fly(&self) {
        println!("*waving arms furiously*");
    }
}
```

Listing 20-17: Two traits are defined to have a `fly` method and are implemented on the `Human` type, and a `fly` method is implemented on `Human` directly

When we call `fly` on an instance of `Human`, the compiler defaults to calling
the method that is directly implemented on the type, as shown in Listing 20-18.

```rust
fn main() {
    let person = Human;
    person.fly();
}
```

Listing 20-18: Calling `fly` on an instance of `Human`

Running this code will print `*waving arms furiously*`, showing that Rust
called the `fly` method implemented on `Human` directly.

To call the `fly` methods from either the `Pilot` trait or the `Wizard` trait,
we need to use more explicit syntax to specify which `fly` method we mean.
Listing 20-19 demonstrates this syntax.

```rust
fn main() {
    let person = Human;

    Pilot::fly(&person);
    Wizard::fly(&person);
    person.fly();
}
```

Listing 20-19: Specifying which trait’s `fly` method we want to call

Specifying the trait name before the method name clarifies to Rust which
implementation of `fly` we want to call. We could also write
`Human::fly(&person)`, which is equivalent to `person.fly()` in Listing 20-19,
but this is a bit longer to write if we don’t need to disambiguate.

Running this code prints:

```rust
This is your captain speaking.
Up!
*waving arms furiously*
```

Because the `fly` method takes a `self` parameter, if we had two _types_ that
both implement one _trait_, Rust could figure out which implementation of a
trait to use based on the type of `self`.

However, associated functions that are part of traits don’t have a `self`
parameter. When two types in the same scope implement that trait, Rust can’t
figure out which implementation you mean unless you use fully qualified syntax.
For example, the `animal` traits in Listing 20-20 have an associated function
`baby_name` that we want to call.

```rust
trait Animal {
    fn baby_name() -> String;
}

struct Dog;

impl Dog {
    fn baby_name() -> String {
        String::from("Spot")
    }
}

impl Animal for Dog {
    fn baby_name() -> String {
        String::from("puppy")
    }
}

fn main() {
    println!("A baby dog is called a {}", Dog::baby_name());
}
```

Listing 20-20: A trait with an associated function and a type with an associated function of the same name that also implements the trait

This code is for an animal shelter that wants to name all puppies Spot, which
is implemented in the `baby_name` associated function that is defined on `Dog`.
The `Dog` type also implements the trait `Animal`, which describes
characteristics that all animals have. Baby dogs are called puppies, and that
is expressed in the implementation of the `Animal` trait on `Dog` in the
`baby_name` function associated with the `Animal` trait.

In `main`, we call the `Dog::baby_name` function, which calls the associated
function defined on `Dog` directly. This code prints:

```rust
A baby dog is called a Spot
```

We want to call the baby name function from the `Animal` trait instead, so we
need to use fully qualified syntax, as shown in Listing 20-21.

```rust
fn main() {
    println!("A baby dog is called a {}", Animal::baby_name());
}
```

Listing 20-21: Using fully qualified syntax to call the `baby_name` function from the `Animal` trait

Running this code will print `A baby dog is called a puppy`, which is what we
wanted.

In summary, fully qualified syntax is defined as follows:

```rust
<Type as Trait>::function(receiver_if_method, next_arg, ...);
```

For associated functions, there would be no `receiver_if_method`: there would
only be the list of other arguments. You could use fully qualified syntax
everywhere that you call functions or methods. However, Rust allows you to
omit any part of this syntax that Rust can figure out from the other
information in the program. You only need to use this more verbose syntax in
cases where there are multiple implementations that use the same name and Rust
needs help to identify which implementation you want to call.

### Using Supertraits

Sometimes, you might write a trait definition that depends on another trait:
for a type to implement the first trait, you want to require that type to also
implement the second trait. You would do this so that your trait definition
can use the associated items of the second trait. The trait your trait
definition is relying on is called a _supertrait_ of your trait.

For example, let’s say we want to make an `OutlinePrint` trait with an
`outline_print` method that will print a value framed in asterisks. That is,
given a `Point` struct that contains `x` and `y` coordinates that implement
`Display` to result in `(x, y)`, when we call `outline_print` on a `Point`
instance that has `1` for `x` and `3` for `y`, it should print:

```rust
**********
* (1, 3) *
**********
```

In the implementation of `outline_print`, we want to use the `Display` trait’s
functionality. Therefore, we need to specify that the `OutlinePrint` trait
will work only for types that also implement `Display` and provide the
functionality that `OutlinePrint` needs. We can do that in the trait
definition by specifying `OutlinePrint: Display`. This technique is similar to
adding a trait bound to the trait. Listing 20-22 shows an implementation of
the `OutlinePrint` trait.

```rust
use std::fmt;

trait OutlinePrint: fmt::Display {
    fn outline_print(&self) {
        let output = self.to_string();
        let len = output.len();
        println!("{}", "*".repeat(len + 4));
        println!("* {} *", output);
        println!("{}", "*".repeat(len + 4));
    }
}
```

Listing 20-22: Implementing the `OutlinePrint` trait that requires the functionality from `Display`

Because we’ve specified that `OutlinePrint` requires the `Display` trait, we
can use the `to_string` function that is automatically implemented for any
type that implements `Display`. If we tried to use `to_string` without adding a
colon and specifying the `Display` trait after the trait name, we’d get an
error saying that no method named `to_string` was found for the type `&Self`
in the current scope.

To fix this, we implement `Display` on `Point` and satisfy the trait bound that
`OutlinePrint` requires, like so:

```rust
use std::fmt;

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}
```

Then implementing the `OutlinePrint` trait on `Point` will work, and we can
call `outline_print` on a `Point` instance to display it within an outline of
asterisks.

### Using the Newtype Pattern to Implement External Traits

In “Implementing a Trait on a Type” in Chapter 10, we mentioned the orphan rule that states we’re only
allowed to implement a trait on a type if either the trait or the type, or
both, are local to our crate. It’s possible to get around this restriction
using the _newtype pattern_, which involves creating a new type in a tuple
struct. (We covered tuple structs in “Using Tuple Structs Without Named Fields
to Create Different Types” in Chapter 5.) The
tuple struct will have one field and be a thin wrapper around the type for
which we want to implement a trait. Then the wrapper type is local to our
crate, and we can implement the trait on the wrapper. _Newtype_ is a term that
originates from the Haskell programming language. There is no runtime
performance penalty for using this pattern, and the wrapper type is elided at
compile time.

As an example, let’s say we want to implement `Display` on `Vec<T>`, which the
orphan rule prevents us from doing directly because the `Display` trait and
the `Vec<T>` type are defined outside our crate. We can make a `Wrapper`
struct that holds an instance of `Vec<T>`; then we can implement `Display` on
`Wrapper` and use the `Vec<T>` value as shown in Listing 20-23.

```rust
use std::fmt;

struct Wrapper(Vec<String>);

impl fmt::Display for Wrapper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]", self.0.join(", "))
    }
}

fn main() {
    let w = Wrapper(vec![String::from("hello"), String::from("world")]);
    println!("w = {}", w);
}
```

Listing 20-23: Creating a `Wrapper` type around `Vec<String>` to implement `Display`

The implementation of `Display` uses `self.0` to access the inner `Vec<T>`,
because `Wrapper` is a tuple struct and `Vec<T>` is the item at index 0 in the
tuple. Then we can use the functionality of the `Display` type on `Wrapper`.

The downside of using this technique is that `Wrapper` is a new type, so it
doesn’t have the methods of the value it’s holding. We would have to implement
all the methods of `Vec<T>` directly on `Wrapper` such that they delegate to
`self.0`, which would allow us to treat `Wrapper` exactly like a `Vec<T>`. If
we don’t want the `Wrapper` type to have all the methods of the inner type—for
example, to restrict the `Wrapper` type’s behavior—we would have to implement
just the methods we do want manually.

This newtype pattern is also useful even when traits are not involved. Let’s
switch focus and look at some advanced ways to interact with Rust’s type system.

## Advanced Types

The Rust type system has some features that we’ve so far mentioned but haven’t
yet discussed. We’ll start by discussing newtypes in general as we examine why
newtypes are useful as types. Then we’ll move on to type aliases, a feature
similar to newtypes but with slightly different semantics. We’ll also discuss
the `!` type and dynamically sized types.

### Using the Newtype Pattern for Type Safety and Abstraction

The newtype pattern is useful for tasks beyond those we’ve discussed so far,
including statically enforcing that values are never confused and indicating
the units of a value. You saw an example of using newtypes to indicate units in
Listing 20-16: recall that the `Millimeters` and `Meters` structs wrapped `u32`
values in a newtype. If we wrote a function that takes a `Millimeters` type,
we couldn’t accidentally pass a value of type `Meters` to it, or vice versa.
Another example is when you want to distinguish between an ID and a pointer
that happens to also be an integer, or many other situations.

The newtype pattern also has another use: abstracting away some implementation
details of a type. The new type can expose a public API that is different from
the API of the private inner type if we used the type directly. Newtypes can
also hide internal implementation details. For example, we could provide a
`People` type to wrap a `HashMap<i32, String>` that stores a person’s ID
associated with their name. Code using `People` would only interact with the
public API we provide, such as a method to add a name string to the `People`
collection; that code wouldn’t need to know that we assign an `i32` ID to
names internally. Newtypes are a lightweight way to achieve encapsulation to
hide implementation details, as we discussed in “Encapsulation that Hides
Implementation Details” in Chapter 17.

### Creating Type Synonyms with Type Aliases

Along with the newtype pattern, Rust provides the ability to declare a _type
alias_ to give an existing type another name. For this we use the `type`
keyword. For example, we can create the alias `Kilometers` to `i32` like so:

```rust
type Kilometers = i32;
```

Now, the alias `Kilometers` is a _synonym_ for `i32`; unlike the `Millimeters`
and `Meters` types we created in Listing 20-16, `Kilometers` is not a separate,
new type. Values that have the type `Kilometers` will be treated the same as
values of type `i32`:

```rust
type Kilometers = i32;

let x: i32 = 5;
let y: Kilometers = 5;

println!("x + y = {}", x + y);
```

Because `Kilometers` and `i32` are the same type, we can add values of both
types and pass `Kilometers` values to functions that take `i32` parameters.
However, using this method, we don’t get the type-checking benefits that we
get from the newtype pattern discussed earlier. In other words, if we mix up
`Kilometers` and `i32` values somewhere, the compiler will not give us an
error.

The main use case for type synonyms is to reduce repetition. For example, we
might have a lengthy type like this:

```rust
Box<dyn Fn() + Send + 'static>
```

Writing this lengthy type in function signatures and as type annotations all
over the code can be tiresome and error prone. Imagine having a project full of
code like that in Listing 20-24.

```rust
let f: Box<dyn Fn() + Send + 'static> = Box::new(|| println!("hi"));

fn takes_long_type(f: Box<dyn Fn() + Send + 'static>) {
    // --snip--
}

fn returns_long_type() -> Box<dyn Fn() + Send + 'static> {
    // --snip--
}
```

Listing 20-24: Using a long type in many places

A type alias makes this code more manageable by reducing the repetition. In
Listing 20-25, we’ve introduced an alias named `Thunk` for the verbose type and
can replace all uses of the type with the shorter alias `Thunk`.

```rust
type Thunk = Box<dyn Fn() + Send + 'static>;

let f: Thunk = Box::new(|| println!("hi"));

fn takes_long_type(f: Thunk) {
    // --snip--
}

fn returns_long_type() -> Thunk {
    // --snip--
}
```

Listing 20-25: Introducing a type alias, `Thunk`, to reduce repetition

This code is much easier to read and write! Choosing a meaningful name for a
type alias can help communicate your intent as well (_thunk_ is a word for code
to be evaluated at a later time, so it’s an appropriate name for a closure that
gets stored).

Type aliases are also commonly used with the `Result<T, E>` type for reducing
repetition. Consider the `std::io` module in the standard library. I/O
operations often return a `Result<T, E>` to handle situations when operations
fail to work. This library has a `std::io::Error` struct that represents all
possible I/O errors. Many of the functions in `std::io` will be returning
`Result<T, E>` where the `E` is `std::io::Error`, such as these functions in
the `Write` trait:

```rust
use std::fmt;
use std::io::Error;

pub trait Write {
    fn write(&mut self, buf: &[u8]) -> Result<usize, Error>;
    fn flush(&mut self) -> Result<(), Error>;

    fn write_all(&mut self, buf: &[u8]) -> Result<(), Error>;
    fn write_fmt(&mut self, fmt: fmt::Arguments) -> Result<(), Error>;
}
```

The `Result<..., Error>` is repeated a lot. As such, `std::io` has this type
alias declaration:

```rust
type Result<T> = Result<T, std::io::Error>;
```

Because this declaration is in the `std::io` module, we can use the fully
qualified alias `std::io::Result<T>`; that is, a `Result<T, E>` with the `E`
filled in as `std::io::Error`. The `Write` trait function signatures end up
looking like this:

```rust
pub trait Write {
    fn write(&mut self, buf: &[u8]) -> Result<usize>;
    fn flush(&mut self) -> Result<()>;

    fn write_all(&mut self, buf: &[u8]) -> Result<()>;
    fn write_fmt(&mut self, fmt: fmt::Arguments) -> Result<()>;
}
```

The type alias helps in two ways: it makes code easier to write _and_ it gives
us a consistent interface across `std::io`. Because it’s an alias, it’s just
another `Result<T, E>`, which means we can use any methods that work on
`Result<T, E>` with it, as well as special syntax like the `?` operator.

### The Never Type That Never Returns

Rust has a special type named `!` that’s known in type theory lingo as the
_empty type_ because it has no values. We prefer to call it the _never type_
because it stands in the place of the return type when a function will never
return. Here is an example:

```rust
fn bar() -> ! {
    // --snip--
}
```

This code is read as “the function `bar` returns never.” Functions that return
never are called _diverging functions_. We can’t create values of the type `!`
so `bar` can never possibly return.

But what use is a type you can never create values for? Recall the code from
Listing 2-5 in Chapter 2, part of the number guessing game; we’ve reproduced a
bit of it here in Listing 20-26.

```rust
let guess: u32 = match guess.trim().parse() {
    Ok(num) => num,
    Err(_) => continue,
};
```

Listing 20-26: A `match` with an arm that ends in `continue`

At the time, we skipped over some details in this code. In Chapter 6 in
“Matching with `Option<T>`” in Chapter 6, we
covered that `match` arms must all return the same type. So, for example, the
following code doesn’t work:

```rust
let guess = match guess.trim().parse() {
    Ok(_) => 5,
    Err(_) => "hello",
};
```

The type of `guess` in this code would have to be an integer _and_ a string,
and Rust requires that `guess` have only one type. So what does `continue`
return? As you might have guessed from the chapter title, `continue` has a `!`
value. That is, when Rust computes the type of `guess`, it looks at both match
arms. The first arm has the value `5` of type `u32`, and the second arm has a
`!` value. Because `!` can never have a value, Rust decides that the type of
`guess` is `u32`.

The formal way of describing this behavior is that expressions of type `!` can
be coerced into any other type. We’re allowed to end the `match` arm with
`continue` because `continue` doesn’t return a value; instead, it moves control
back to the top of the loop, so in the `Err` case, we never assign a value to
`guess`.

The never type is useful with the `panic!` macro as well. Remember the `unwrap`
function that we call on `Option<T>` values to produce a value or panic? Here
is its definition:

```rust
impl<T> Option<T> {
    pub fn unwrap(self) -> T {
        match self {
            Some(val) => val,
            None => panic!("called `Option::unwrap()` on a `None` value"),
        }
    }
}
```

In this code, the same thing happens as in the `match` in Listing 20-26: Rust
sees that `val` has type `T` and `panic!` has type `!`, so the result of the
overall `match` expression is `T`. This code works because `panic!` doesn’t
produce a value; it ends the program. In the `None` case, we won’t be returning
a value from `unwrap`, so this code is valid.

One more expression that has type `!` is a `loop`:

```rust
print!("forever ");

loop {
    print!("and ever ");
}
```

Here, the loop never ends, so `!` is the value of the expression. However, this
wouldn’t be true if we included a `break`, because the loop would terminate
with a value when it got to the `break`.

### Dynamically Sized Types and the `Sized` Trait

Due to Rust’s need to know certain details, such as how much space to allocate
for a value of a particular type, there is a corner of its type system that can
be confusing: the concept of _dynamically sized types_. Sometimes referred to
as _DSTs_ or _unsized types_, these types let us write code using values whose
size we can know only at runtime.

Let’s dig into the details of a dynamically sized type that we’ve been using
throughout this book: `str`. That’s right, not `&str`, but `str` by itself, is
a DST. We can’t know how long the string is until runtime, meaning we can’t
create a variable of type `str`, nor can we take an argument of type `str`. Consider
this code, which does not work:

```rust
let s1: str = "Hello there!";
let s2: str = "How's it going?";
```

Rust needs to know how much memory to allocate for any value of a particular
type, and all values of a type must use the same amount of memory. If Rust
allowed us to write this code, these two `str` values would need to take up the
same amount of space. But they have different lengths: `s1` needs 12 bytes of
storage and `s2` needs 15. This is why it’s not possible to create a variable
holding a dynamically sized type.

So what do we do? In this case, you already know the answer: we make the types
of `s1` and `s2` `&str` rather than `str`. If you think back to “String Slices”
in Chapter 4, we said that the slice data structure stores the starting
position and the length of the slice.

So although a `&T` is a single value that stores the memory address of where
the `T` is located, a `&str` is _two_ values: the address of the `str` and its
length. As such, we can know the size of a `&str` value at compile time: it’s
twice the length of a `usize`. That is, we always know the size of a `&str`,
no matter how long the string it refers to is. In general, this is the way in
which dynamically sized types are used in Rust: they have an extra bit of
metadata that stores the size of the dynamic information. The golden rule of
dynamically sized types is that we must always put values of dynamically sized
types behind a pointer of some kind.

We can combine `str` with all kinds of pointers: for example, `Box<str>` or
`Rc<str>`. In fact, you’ve seen this before but with a different dynamically
sized type: traits. Every trait is a dynamically sized type we can refer to by
using the name of the trait. In Chapter 18 in “Using Trait Objects That Allow
for Values of Different Types” in Chapter 18, we mentioned that to use traits as
trait objects, we must put them behind a pointer, like `&dyn Trait` or
`Box<dyn Trait>` (`Rc<dyn Trait>` would work too).

To work with DSTs, Rust provides the `Sized` trait to determine whether or not
a type’s size is known at compile time. This trait is automatically implemented
for everything whose size is known at compile time. In addition, Rust
implicitly adds a bound on `Sized` to every generic function. That is, a
generic function definition like this:

```rust
fn generic<T>(t: T) {
    // --snip--
}
```

is actually treated as though we had written this:

```rust
fn generic<T: Sized>(t: T) {
    // --snip--
}
```

By default, generic functions will work only on types that have a known size at
compile time. However, you can use the following special syntax to relax this
restriction:

```rust
fn generic<T: ?Sized>(t: &T) {
    // --snip--
}
```

A trait bound on `?Sized` means “`T` may or may not be `Sized`” and this
notation overrides the default that generic types must have a known size at
compile time. The `?Trait` syntax with this meaning is only available for
`Sized`, not for any other traits.

Also note that we switched the type of the `t` parameter from `T` to `&T`.
Because the type might not be `Sized`, we need to use it behind some kind of
pointer. In this case, we’ve chosen a reference.

Next, let’s talk about functions and closures!

## Advanced Functions and Closures

This section explores some advanced features related to functions and closures,
including function pointers and returning closures.

### Function Pointers

We’ve talked about how to pass closures to functions; you can also pass regular
functions to functions! This technique is useful when you want to pass a
function you’ve already defined rather than defining a new closure. Functions
coerce to the type `fn` (with a lowercase _f_), not to be confused with the
`Fn` closure trait. The `fn` type is called a _function pointer_. Passing
functions with function pointers will allow you to use functions as arguments
to other functions.

The syntax for specifying that a parameter is a function pointer is similar to
that of closures, as shown in Listing 20-27, where we’ve defined a function
`add_one` that adds 1 to its parameter. The function `do_twice` takes two
parameters: a function pointer to any function that takes an `i32` parameter
and returns an `i32`, and one `i32` value. The `do_twice` function calls the
function `f` twice, passing it the `arg` value, then adds the two function call
results together. The `main` function calls `do_twice` with the arguments
`add_one` and `5`.

```rust
fn add_one(x: i32) -> i32 {
    x + 1
}

fn do_twice(f: fn(i32) -> i32, arg: i32) -> i32 {
    f(arg) + f(arg)
}

fn main() {
    let answer = do_twice(add_one, 5);

    println!("The answer is: {}", answer);
}
```

Listing 20-27: Using the `fn` type to accept a function pointer as an argument

This code prints `The answer is: 12`. We specify that the parameter `f` in
`do_twice` is an `fn` that takes one parameter of type `i32` and returns an
`i32`. We can then call `f` in the body of `do_twice`. In `main`, we can pass
the function name `add_one` as the first argument to `do_twice`.

Unlike closures, `fn` is a type rather than a trait, so we specify `fn` as the
parameter type directly rather than declaring a generic type parameter with one
of the `Fn` traits as a trait bound.

Function pointers implement all three of the closure traits (`Fn`, `FnMut`, and
`FnOnce`), meaning you can always pass a function pointer as an argument for a
function that expects a closure. It’s best to write functions using a generic
type and one of the closure traits so your functions can accept either
functions or closures.

That said, one example of where you would want to only accept `fn` and not
closures is when interfacing with external code that doesn’t have closures: C
functions can accept functions as arguments, but C doesn’t have closures.

As an example of where you could use either a closure defined inline or a named
function, let’s look at a use of the `map` method provided by the `Iterator`
trait in the standard library. To use the `map` method to turn a vector of
numbers into a vector of strings, we could use a closure, as in Listing 20-28.

```rust
let list_of_numbers = vec![1, 2, 3];
let list_of_strings: Vec<String> =
    list_of_numbers.iter().map(|i| i.to_string()).collect();
```

Listing 20-28: Using a closure with the `map` method to convert numbers to strings

Or we could name a function as the argument to `map` instead of the closure.
Listing 20-29 shows what this would look like.

```rust
let list_of_numbers = vec![1, 2, 3];
let list_of_strings: Vec<String> =
    list_of_numbers.iter().map(ToString::to_string).collect();
```

Listing 20-29: Using the `String::to_string` function with the `map` method to convert numbers to strings

Note that we must use the fully qualified syntax that we talked about in
“Advanced Traits” because there are multiple
functions available named `to_string`. Here, we’re using the `to_string`
function defined in the `ToString` trait, which the standard library has
implemented for any type that implements `Display`.

Recall from “Enum Values” in Chapter 6 that the
name of each enum variant that we define also becomes an initializer function.
We can use these initializer functions as function pointers that implement the
closure traits, which means we can specify the initializer functions as
arguments for methods that take closures, as seen in Listing 20-30.

```rust
enum Status {
    Value(u32),
    Stop,
}

let list_of_statuses: Vec<Status> =
    (0u32..20).map(Status::Value).collect();
```

Listing 20-30: Using an enum initializer with the `map` method to create a `Status` instance from numbers

Here, we create `Status::Value` instances using each `u32` value in the range
that `map` is called on by using the initializer function of `Status::Value`.
Some people prefer this style and some people prefer to use closures. They
compile to the same code, so use whichever style is clearer to you.

### Returning Closures

Closures are represented by traits, which means you can’t return closures
directly. In most cases where you might want to return a trait, you can instead
use the concrete type that implements the trait as the return value of the
function. However, you can’t usually do that with closures because they don’t
have a concrete type that is returnable; you’re not allowed to use the function
pointer `fn` as a return type if the closure captures any values from its
scope, for example.

Instead, you will normally use the `impl Trait` syntax we learned about in
Chapter 10. You can return any function type, using `Fn`, `FnOnce` and `FnMut`.
For example, the code in Listing 20-31 will compile just fine.

```rust
fn returns_closure() -> impl Fn(i32) -> i32 {
    |x| x + 1
}
```

Listing 20-31: Returning a closure from a function using the `impl Trait` syntax

However, as we noted in “Closure Type Inference and
Annotation” in Chapter 13, each closure is also
its own distinct type. If you need to work with multiple functions that have the
same signature but different implementations, you will need to use a trait
object for them. Consider what happens if you write code like that shown in
Listing 20-32.

```rust
fn main() {
    let handlers = vec![returns_closure(), returns_initialized_closure(123)];
    for handler in handlers {
        let output = handler(5);
        println!("{}", output);
    }
}

fn returns_closure() -> impl Fn(i32) -> i32 {
    |x| x + 1
}

fn returns_initialized_closure(init: i32) -> impl Fn(i32) -> i32 {
    move |x| x + init
}
```

Listing 20-32: Creating a `Vec<T>` of closures defined by functions that return `impl Fn` types

This code will not compile because the closures have different types, even
though they have the same signature. The error message tells us that distinct
uses of `impl Trait` result in different opaque types. If we try to compile
this, Rust lets us know that it won’t work:

```text
error[E0308]: mismatched types
  --> src/main.rs:2:44
  |
2  |     let handlers = vec![returns_closure(), returns_initialized_closure(123)];
   |                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ expected opaque type, found a different opaque type
...
9  | fn returns_closure() -> impl Fn(i32) -> i32 {
   |                         ------------------- the expected opaque type
...
13 | fn returns_initialized_closure(init: i32) -> impl Fn(i32) -> i32 {
   |                                              ------------------- the found opaque type
   |
   = note: distinct uses of `impl Trait` result in different opaque types
```

The error message tells us that whenever we return an `impl Trait`, Rust
creates a unique _opaque type_, a type where we cannot see into the details of
what Rust constructs for us, nor can we guess the type Rust will generate to
write ourselves. So even though these functions return closures that implement
the same trait, `Fn(i32) -> i32`, the opaque types Rust generates for each are
different.

If we need to return multiple closures with the same type, we can use a trait
object, as in Listing 20-33.

```rust
fn returns_closure() -> Box<dyn Fn(i32) -> i32> {
    Box::new(|x| x + 1)
}

fn returns_initialized_closure(init: i32) -> Box<dyn Fn(i32) -> i32> {
    Box::new(move |x| x + init)
}
```

Listing 20-33: Creating a `Vec<T>` of closures defined by functions that return `Box<dyn Fn>` so they have the same type

This code will compile just fine. For more about trait objects, refer to the
section “Using Trait Objects That Allow for Values of Different
Types” in Chapter 18.

Next, let’s look at macros!

## Macros

We’ve used macros like `println!` throughout this book, but we haven’t fully
explored what a macro is and how it works. The term _macro_ refers to a family
of features in Rust: _declarative_ macros with `macro_rules!` and three kinds
of _procedural_ macros:

* Custom `#[derive]` macros that specify code added with the `derive` attribute
  used on structs and enums
* Attribute-like macros that define custom attributes usable on any item
* Function-like macros that look like function calls but operate on the tokens
  specified as their argument

We’ll talk about each of these in turn, but first, let’s look at why we even
need macros when we already have functions.

### The Difference Between Macros and Functions

Fundamentally, macros are a way of writing code that writes other code, which
is known as _metaprogramming_. In Appendix C, we discuss the `derive`
attribute, which generates an implementation of various traits for you. We’ve
also used the `println!` and `vec!` macros throughout the book. All of these
macros _expand_ to produce more code than the code you’ve written manually.

Metaprogramming is useful for reducing the amount of code you have to write and
maintain, which is also one of the roles of functions. However, macros have
some additional powers that functions don’t have.

A function signature must declare the number and type of parameters the
function has. Macros, on the other hand, can take a variable number of
parameters: we can call `println!("hello")` with one argument or
`println!("hello {}", name)` with two arguments. Also, macros are expanded
before the compiler interprets the meaning of the code, so a macro can, for
example, implement a trait on a given type. A function can’t, because it gets
called at runtime and a trait needs to be implemented at compile time.

The downside to implementing a macro instead of a function is that macro
definitions are more complex than function definitions because you’re writing
Rust code that writes Rust code. Due to this indirection, macro definitions are
generally more difficult to read, understand, and maintain than function
definitions.

Another important difference between macros and functions is that you must
define macros or bring them into scope _before_ you call them in a file, as
opposed to functions you can define anywhere and call anywhere.

### Declarative Macros with `macro_rules!` for General Metaprogramming

The most widely used form of macros in Rust is the _declarative macro_. These
are also sometimes referred to as “macros by example,” “`macro_rules!` macros,”
or just plain “macros.” At their core, declarative macros allow you to write
something similar to a Rust `match` expression. As discussed in Chapter 6,
`match` expressions are control structures that take an expression, compare the
resultant value of the expression to patterns, and then run the code associated
with the matching pattern. Macros also compare a value to patterns that are
associated with particular code: in this situation, the value is the literal
Rust source code passed to the macro; the patterns are compared with the
structure of that source code; and the code associated with each pattern, when
matched, replaces the code passed to the macro. This all happens during
compilation.

To define a macro, you use the `macro_rules!` construct. Let’s explore how to
use `macro_rules!` by looking at how the `vec!` macro is defined. Chapter 8
covered how we can use the `vec!` macro to create a new vector with particular
values. For example, the following macro creates a new vector containing three
integers:

```rust
let v: Vec<u32> = vec![1, 2, 3];
```

We could also use the `vec!` macro to make a vector of two integers or a vector
of five string slices. We wouldn’t be able to use a function to do the same
because we wouldn’t know the number or type of values up front.

Listing 20-34 shows a slightly simplified definition of the `vec!` macro.

```rust
#[macro_export]
macro_rules! vec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}
```

Listing 20-34: A simplified version of the `vec!` macro definition

> Note: The actual definition of the `vec!` macro in the standard library
> includes code to pre-allocate the correct amount of memory up front. That code
> is an optimization that we don’t include here, to make the example simpler.

The `#[macro_export]` annotation indicates that this macro should be made
available whenever the crate in which the macro is defined is brought into
scope. Without this annotation, the macro can’t be brought into scope.

We then start the macro definition with `macro_rules!` and the name of the
macro we’re defining _without_ the exclamation mark. The name, in this case
`vec`, is followed by curly brackets denoting the body of the macro definition.

The structure in the `vec!` body is similar to the structure of a `match`
expression. Here we have one arm with the pattern `( $( $x:expr ),* )`,
followed by `=>` and the block of code associated with this pattern. If the
pattern matches, the associated block of code will be emitted. Given that this
is the only pattern in this macro, there is only one valid way to match; any
other pattern will result in an error. More complex macros will have more than
one arm.

Valid pattern syntax in macro definitions is different from the pattern syntax
covered in Chapter 19 because macro patterns are matched against Rust code
structure rather than values. Let’s walk through what the pattern pieces in
Listing 20-34 mean; for the full macro pattern syntax, see the Rust
Reference at _../reference/macros-by-example.html_.

First we use a set of parentheses to encompass the whole pattern. We use a
dollar sign (`$`) to declare a variable in the macro system that will contain
the Rust code matching the pattern. Within `$()` is `$x:expr`, which matches any
Rust expression and gives the expression the name `$x`.

The comma following `$()` indicates that a literal comma separator character
must appear between each instance of the code that matches the code in `$()`.
The `*` specifies that the pattern matches zero or more of whatever precedes
the `*`.

When we call this macro with `vec![1, 2, 3];`, the `$x` pattern matches three
times with the three expressions `1`, `2`, and `3`.

Now let’s look at the pattern in the body of the code associated with this arm:
`temp_vec.push()` within `$()*` is generated for each part that matches `$()`
in the pattern zero or more times depending on how many times the pattern
matches. The `$x` is replaced with each expression matched. When we call this
macro with `vec![1, 2, 3];`, the code generated that replaces this macro call
will be the following:

```rust
{
    let mut temp_vec = Vec::new();
    temp_vec.push(1);
    temp_vec.push(2);
    temp_vec.push(3);
    temp_vec
}
```

We’ve defined a macro that can take any number of arguments of any type and can
generate code to create a vector containing the specified elements.

### Procedural Macros for Generating Code from Attributes

The second form of macros is the procedural macro, which acts more like a
function (and is a type of procedure). _Procedural macros_ accept some code as
an input, operate on that code, and produce some code as an output rather than
matching against patterns and replacing the code with other code as declarative
macros do. The three kinds of procedural macros are custom `derive`,
attribute-like, and function-like, and all work in a similar fashion.

When creating procedural macros, the definitions must reside in their own crate
with a special crate type. This is for complex technical reasons that we hope
to eliminate in the future. In Listing 20-35, we show how to define a
procedural macro, where `some_attribute` is a placeholder for using a specific
macro variety.

```rust
use proc_macro;

#[some_attribute]
pub fn some_name(input: TokenStream) -> TokenStream {
}
```

Listing 20-35: An example of defining a procedural macro

The function that defines a procedural macro takes a `TokenStream` as input and
produces a `TokenStream` as output. The `TokenStream` type is defined by the
`proc_macro` crate that is included with Rust and represents a sequence of
tokens. This is the core of the macro: the source code that the macro is
operating on makes up the input `TokenStream`, and the code the macro produces
is the output `TokenStream`. The function also has some kind of attribute
attached to it that specifies which kind of procedural macro we’re creating.
We can have multiple kinds of procedural macros in the same crate.

Let’s look at the different kinds of procedural macros. We’ll start with a
custom derive macro and then explain the small differences that make the other
forms different.

#### How to Write a Custom `derive` Macro

Let’s create a crate named `hello_macro` that defines a trait named `HelloMacro`
with one associated function named `hello_macro`. Rather than making our users
implement the `HelloMacro` trait for each of their types, we’ll provide a
procedural macro so users can annotate their type with `#[derive(HelloMacro)]`
to get a default implementation of the `hello_macro` function. The default
implementation will print `Hello, Macro! My name is [the name of the type]!`.

The end result will be that we’ll be able to write code like Listing 20-36.

```rust
use hello_macro::HelloMacro;
use hello_macro_derive::HelloMacro;

#[derive(HelloMacro)]
struct Pancakes;

fn main() {
    Pancakes::hello_macro();
}
```

Listing 20-36: The code a user of our crate will be able to write when using our procedural macro

This code will print `Hello, Macro! My name is Pancakes!`.

To start, let’s create a new library crate:

```rust
cargo new hello_macro --lib
```

Next, in Listing 20-37, we’ll define the `HelloMacro` trait and its associated
function.

```rust
pub trait HelloMacro {
    fn hello_macro();
}
```

Listing 20-37: The trait users will implement to use the procedural macro

We have a trait and its function. At this point, our crate user could implement
the trait to achieve the desired functionality, as in Listing 20-38.

```rust
use hello_macro::HelloMacro;

struct Pancakes;

impl HelloMacro for Pancakes {
    fn hello_macro() {
        println!("Hello, Macro! My name is Pancakes!");
    }
}

fn main() {
    Pancakes::hello_macro();
}
```

Listing 20-38: How it would look if users wrote a manual implementation of the `HelloMacro` trait

However, they would need to write the implementation block for each type they
want to use the trait on. Let’s instead make the procedural macro generate that
code.

Next, we’ll create a new crate for the procedural macro. For technical reasons,
procedural macros must be in their own crate. Eventually, we might be able to
define macros in the same crate as the code that uses them, but for now, this
separation is required. So let’s create a new library crate called
`hello_macro_derive`:

```rust
cargo new hello_macro_derive --lib
```

Our two crates are tightly related, so we create the procedural macro crate
within the directory of the main crate. If we change the directory structure,
we might need to change the paths in the code. For now, let’s keep the
`hello_macro_derive` crate as a sibling to the `hello_macro` crate.

In the `hello_macro_derive` crate’s `Cargo.toml`, we need to specify the
`proc_macro` crate type and add dependencies on `syn` and `quote`, as shown in
Listing 20-39.

```rust
[lib]
proc-macro = true

[dependencies]
syn = "2.0"
quote = "1.0"
```

Listing 20-39: Dependencies needed in `Cargo.toml` to write a procedural macro

Let’s start by defining the `hello_macro_derive` function. Because our
`hello_macro_derive` crate has the `proc_macro` crate type, the code in Listing
20-40 will go into `src/lib.rs`.

```rust
use proc_macro::TokenStream;
use quote::quote;
use syn;

#[proc_macro_derive(HelloMacro)]
pub fn hello_macro_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).unwrap();

    // Build the trait implementation
    impl_hello_macro(&ast)
}
```

Listing 20-40: Code that most procedural macro crates will require to process Rust code

The function that parses the `TokenStream` is `syn::parse`. It takes a
`TokenStream` and returns a `DeriveInput` struct representing the parsed Rust
code. Listing 20-41 shows the relevant parts of the `DeriveInput` struct we get
by parsing the `struct Pancakes;` string:

```rust
DeriveInput {
    // --snip--

    ident: Ident {
        ident: "Pancakes",
        span: #0 bytes(95..103)
    },
    data: Struct(
        DataStruct {
            struct_token: Struct,
            fields: Unit,
            semi_token: Some(
                Semi
            )
        }
    )
}
```

Listing 20-41: The `DeriveInput` instance we get when parsing the code that has the macro’s derive attribute on a struct with one identifier

The fields show that the `ident` (identifier, meaning the name) of the struct
we’re deriving is `Pancakes`, which is what we got when we parsed the code
`struct Pancakes;`. All the other fields have more information about this
struct; check the `syn` documentation for `DeriveInput` for details.

At this point, we haven’t defined the `impl_hello_macro` function that builds
the new Rust code. But before we do, note that the output for our derive macro
is also a `TokenStream`. The returned `TokenStream` is added to the code that
our crate users write, so when they compile their crate, they’ll get the extra
functionality that we provide.

You might have noticed that we’re calling `unwrap` to cause the
`hello_macro_derive` function to panic if the call to `syn::parse` fails. It’s
necessary for our procedural macro to panic on errors because `proc_macro_derive`
functions must return `TokenStream` rather than `Result` to conform to the
procedural macro API. We’ve simplified this example by using `unwrap`; in
production code, you should provide more specific error messages about what
went wrong by using `panic!` or `expect`.

Now that we have the code to parse the `TokenStream` into a syntax tree, let’s
define the code to transform the syntax tree. In Listing 20-42, we add the
`impl_hello_macro` function.

```rust
fn impl_hello_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl HelloMacro for #name {
            fn hello_macro() {
                println!("Hello, Macro! My name is {}!", stringify!(#name));
            }
        }
    };
    gen.into()
}
```

Listing 20-42: Implementing the `HelloMacro` trait using the parsed Rust code

We get an `Ident` struct representing the struct name (identifier) from the
`ident` field of the `DeriveInput` struct. The `quote!` macro lets us define
the Rust code that we want to return. The compiler expects something different
to the direct result of the `quote!` macro’s execution, so we need to convert
it to a `TokenStream`. We do this by calling the `into` method, which consumes
this intermediate representation and returns a value of the required
`TokenStream` type.

The `quote!` macro also provides some very cool templating mechanics: we can
enter `#name`, and `quote!` will replace it with the value in the variable
`name`. You can even do some repetition similar to the way regular macros work.
Check out the `quote` crate’s docs at _<https://docs.rs/quote>_ for a thorough
introduction.

We want our procedural macro to generate an implementation of our `HelloMacro`
trait for the type the user annotated, which we can get by using `#name`. The
trait implementation has the one function `hello_macro`, whose body contains
the functionality we want to provide: printing `Hello, Macro! My name is` and
then the name of the annotated type.

The `stringify!` macro used here is built into Rust. It takes a Rust
expression, such as `1 + 2`, and at compile time turns the expression into a
string literal, such as `"1 + 2"`. This is different from `format!` or
`println!`, macros which evaluate the expression and then turn the result into
a `String`. There is a possibility that the `#name` input might be an
expression we’d want to print literally, so we use `stringify!`. Using
`stringify!` also saves an allocation by using a string literal at compile
time.

At this point, `cargo build` should complete successfully in both `hello_macro`
and `hello_macro_derive`. Let’s hook these crates together to make the
procedural macro work on our code. In the `hello_macro` directory, let’s add
the `hello_macro_derive` crate as a dependency in `Cargo.toml`, as shown in
Listing 20-43.

```rust
[dependencies]
hello_macro_derive = { path = "../hello_macro_derive" }
```

Listing 20-43: Adding `hello_macro_derive` as a dependency in `hello_macro`

Now we can write our code using the procedural macro, as shown in Listing
20-36. If we run `cargo run` in the `hello_macro` directory, it should print
`Hello, Macro! My name is Pancakes!`.

The basic procedural macro pattern is now complete. In the real world, you’ll
want to document your procedural macro crates and provide helpful error
messages for users of your macro, but that’s beyond the scope of this book.
Let’s look at the other kinds of procedural macros.

#### Attribute-Like Macros

Attribute-like macros are similar to custom `derive` macros, but instead of
generating code for the `derive` attribute, they allow you to create new
attributes. They’re also more flexible: `derive` only works for structs and
enums; attributes can be applied to other items as well, such as functions.
Here’s an example of using an attribute-like macro: say you have an attribute
named `route` that annotates functions when using a web application framework:

```rust
#[route(GET, "/")]
fn index() {
```

This `#[route]` attribute would be defined by the framework as a procedural
macro. The signature of the macro definition function would look like this:

```rust
#[proc_macro_attribute]
pub fn route(attr: TokenStream, item: TokenStream) -> TokenStream {
```

Here, we have two parameters of type `TokenStream`. The first is for the
contents of the attribute: the `GET, "/"` part. The second is the body of the
item the attribute is attached to: in this case, `fn index() {}` and the rest
of the function’s body.

Other than that, attribute-like macros work the same way as custom `derive`
macros: you create a crate with the `proc-macro` crate type and implement a
function that generates the code you want!

#### Function-Like Macros

Function-like macros define macros that look like function calls. Similarly to
`macro_rules!` macros, they’re more flexible than functions; for example, they
can take an unknown number of arguments. However, `macro_rules!` macros can
only be defined using the match-like syntax we discussed in “Declarative
Macros with `macro_rules!` for General Metaprogramming” earlier.
Function-like macros take a `TokenStream` parameter, and their definition
manipulates that `TokenStream` using Rust code as the other two types of
procedural macros do. An example of a function-like macro is an `sql!` macro
that might be called like so:

```rust
let sql = sql!(SELECT * FROM posts WHERE id=1);
```

This macro would parse the SQL statement inside it and check that it’s
syntactically correct, which is much more complex processing than a
`macro_rules!` macro can do. The `sql!` macro would be defined like this:

```rust
#[proc_macro]
pub fn sql(input: TokenStream) -> TokenStream {
```

This definition is similar to the custom `derive` macro’s signature: we receive
the tokens that are inside the parentheses and return the code we wanted to
generate.

## Summary

Whew! Now you have some Rust features in your toolbox that you likely won’t use
often, but you’ll know they’re available in very particular circumstances.
We’ve introduced several complex topics so that when you encounter them in
error message suggestions or in other people’s code, you’ll be able to
recognize these concepts and syntax. Use this chapter as a reference to guide
you to solutions.

Next, we’ll put everything we’ve discussed throughout the book into practice
and do one more project!
