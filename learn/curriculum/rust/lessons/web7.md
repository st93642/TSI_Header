# 8 Injecting Rust in the Frontend with WASM

You have probably heard about WASM. However, at the time of writing this book, the WASM ecosystem is still in the early rapid development phases where APIs get outdated quickly, and groups trying out a new approach cease to exist but have remnants of their approach throughout the internet. This can lead to frustration as you burn hours trying to figure out what API does what. However, the promise of compile once and run anywhere including the browser is a clear advantage so it makes sense to understand and get comfortable with WASM.

In this chapter, we are going to keep the interactions with APIs to a minimum and focus on concepts around serving WASM modules to the frontend to be loaded and used in the browser. We are also going to explore directly interacting with the raw memory addresses in WASM so we can transfer complex data types across the WASM boundary, and we are going to load WASM binaries using Rust to also interact with WASM binaries in the backend. By the end of this chapter you will be comfortable enough with WASM to navigate the rapidly changing landscape of WASM.

## Technical requirements

We will also be building on the server code we created in the Chapter 6, Displaying Content in the Browser which can be found at the chapter06 source code.

You can find the full source code that will be used in this chapter here:

the chapter07 source code.

You will also need wasm-pack which can be installed via the following link:

the wasm-pack installer.

And the WebAssembly binary toolkit which can be found in the link below:

the WebAssembly binary toolkit.

This toolkit can also be installed with OS package managers such as brew and apt-get.

## Setting Up Our WASM Build

We must setup a Rust workspace in our frontend that can house the Rust code that we want to compile to a WASM target to be served. In our ingress/frontend directory, we can create a new Rust workspace with the following command:

```bash
cargo new rust-interface --lib
```

Now that we have a workspace, we must configure our `Cargo.toml` of this workspace with the following code:

```toml
# ingress/frontend/rust-interface/Cargo.toml
[lib]
crate-type = ["cdylib"]

[dependencies]
wasm-bindgen = "0.2.92"
```

Here, we can see that we are relying on the `wasm-bindgen` crate to make the WASM to JavaScript bindings and simplify our Rust functions that act as entry points to the WASM program. In the last section of the chapter, we will build our own code to enable us to pass complex data types over the WASM boundary. This is going to involve directly accessing raw unsafe data pointers. However, for our frontend example, the `wasm-bindgen` crate is going to do all the heavy lifting when it comes to passing over even simple data structures like strings.

We can also see that the crate type of our Rust workspace is a `cdylib`. This stands for dynamic library. A dynamic library is linked to when a program is compiling. The compiled program then loads the dynamic library on runtime. There are pros and cons to this. For instance, you can reuse dynamic libraries in multiple programs and these dynamic libraries are easier to update as they are not part of the build. Not being part of the build also results in reducing the compile times as we do not need to recompile the dynamic library when we are compiling the main code. Dynamic libraries will also reduce your main program binary size. However, this is not free as the compiler cannot optimize the code in the dynamic library with respect to the code of your main program. It also has additional overhead as the program needs to load the dynamic library and deployment can be trickier due to more moving parts.

For most of your web development projects you will not need to build dynamic libraries as it is not worth the extra headache of linker errors and handing multiple moving parts. However, if you end up working on a massive codebase then dynamic libraries become essential. Especially if there are interfaces from other languages such as WASM. For instance, at the time of writing this book I am currently doing research in the Kings College London Bioengineering department in surgical robotics. We need to interface with GPUs so interacting with C++ dynamic libraries for these GPUs is essential, there is no getting around this. A lot of hardware support is mainly written in C and C++ dynamic libraries.

We need to compile our Rust binary to a WASM format and move it into our public directory. This will enable us to serve our WASM binary when needed. We can do this via the terminal but there are a couple of commands pointing to specific directories. This is easy to get wrong and we also not want to bother other developers on how to build the WASM package, so we will just put our commands in the `package.json` under the `scripts` section with the following code:

```json
// FILE: ingress/frontend/package.json
"wasm": "cd rust-interface && sudo wasm-pack build --target web && cp pkg/rust_interface_bg.wasm ../public/rust_interface_bg.wasm",
```

Here we can see that we carry out the following steps:

- Move to the rust-interface directory
- Use wasm-pack to build the Rust program to target the web
- Copy the WASM binary from the pkg to the public directory

We will also want to trace our WASM binary to make sure that we are exporting the functions that we expect. We can do this with the wasm2wat command from the wasm toolkit we installed in the technical requirements section with the following scripts command:

```json
// FILE: ingress/frontend/package.json
"wasm-trace": "cd public && wasm2wat rust_interface_bg.wasm > rust_interface_bg.wat"
```

What happens here is that the wasm2wat command converts the WASM binary into wat format which is human readable text format. You can even code in wat and compile to WASM giving you the freedom to do whatever you want, however, you will be directly coding to assembly and this will take you a lot of time to code even the most basic Rust programs.

Before we run any of these commands, we must write some Rust code that can be compiled into WASM. For our example, we will be creating a basic function that takes in a string which is the status of the task and returns the text that should be displayed in the button for that task. This can be done with the following code:

```rust
// FILE: ingress/frontend/rust-interface/src/lib.rs
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn rust_generate_button_text(status: String) -> String {
    match status.to_uppercase().as_str() {
        "PENDING" => "edit".to_string(),
        "DONE" => "delete".to_string(),
        _ => "an error has occurred".to_string(),
    }
}
```

While the code is straightforward enough, we can see that the function is annotated with the `#[wasm_bindgen]` macro. The `#[wasm_bindgen]` macro does the following:

- **JavaScript Integration**: The macro exposes the annotated Rust function so it can be called from JavaScript. This is essential for creating WebAssembly modules that can interact with web-based applications.
- **Type Conversion**: It handles automatic conversion of types between Rust and JavaScript. In your function, it manages the conversion of the Rust `String` type to a JavaScript string and vice versa.
- **Memory Management**: The macro manages memory for the types passed between Rust and JavaScript, ensuring that memory is allocated and freed appropriately to prevent leaks.
- **Function Wrapping**: It wraps the Rust function in a way that it can be imported into JavaScript as a regular function. This allows the function to be invoked from JavaScript just like any other JS function.

We can now run our build with the following commands:

```bash
npm run wasm
npm run wasm-trace
```

After running these commands, the WASM and WAT files should be in the public directory. We cannot really inspect the WASM binary because it's a binary, but if we look at the bottom of the `rust_interface_bg.wat` file we should see the following exports:

```text
// File: ingress/frontend/public/rust_interface_bg.wat
(export "memory" (memory 0))
(export "update_status" (func 9))
(__wbindgen_add_to_stack_pointer (func 35))
(__wbindgen_malloc (func 19))
(__wbindgen_realloc (func 23))
(__wbindgen_free (func 27))
```

Before we explain what the wat code means, we need to understand that the WASM memory model is linear. This means that unlike random access memory, all the memory in the WASM program is contiguous, meaning that the memory is essentially one big array of memory. This makes WASM very fast compared to random access. However, it also makes WASM more vulnerable to attacks if there is no memory safety. For instance, a buffer overflow is where we can access memory outside of the assigned buffer. If a hacker can do this, then the hacker has access to the entire memory stack of the program and can do what it wants. Luckily with Rust you have memory safety. This is why Rust is at the bleeding edge of WASM. Rust has memory safety, but it doesn't have a garbage collector which simplifies the running and compiling of the WASM binary. I keep saying to people, if they want to lead the charge of WASM, they need to learn Rust.

With the WASM memory model in mind, we can now look at the `(memory 0)`. This refers to the first memory instance defined in the module and is allocated at index 0 of the memory array. This memory instance is being exported with the function name "memory", making it accessible for use outside the module. The `(func 9)`, `(func 35)`, `(func 19)`, `(func 23)`, `(func 27)` denote that they are functions that are allocated at certain indexes of the memory.

Now we can cover the function exports:

- `(export "memory" (memory 0))`: This exports the first memory instance defined in the WebAssembly module. It allows external environments, to access and manipulate the memory used by the WebAssembly instance.
- `(export "update_status" (func 9))`: Function Export: Exports a function labeled as update_status. This is the function that we defined in our code, so it's good to know that the code we wrote is available in the WASM binary as update_status.
- `(export "__wbindgen_add_to_stack_pointer" (func 35))`: Stack Pointer Adjustment: Exports a function that adjusts the stack pointer. This is useful for managing the allocation of stack space, particularly during complex operations or recursive calls within the WebAssembly code.
- `(export "__wbindgen_malloc" (func 19))`: Memory Allocation: Exports a function for allocating memory. This function is analogous to malloc in C/C++, providing a way to dynamically allocate a specified amount of memory. Later in the chapter, we will write our own malloc function.
- `(export "__wbindgen_realloc" (func 23))`: Memory Reallocation: Exports a function that adjusts the size of a previously allocated memory block. This is like realloc in C/C++, which is used to expand or contract a memory allocation while attempting to preserve the contents.
- `(export "__wbindgen_free" (func 27))`: Memory Deallocation: Exports a function for freeing allocated memory. This function is like free in C/C++ and is used to release previously allocated memory back to the system, preventing memory leaks. Memory leaks are where the memory allocated to variables that are no longer used is not cleared up. Over time, the memory being used by the program continues to grow until the computer has run out of memory.

Our WASM program is now completed, and we are confident that we can interact with it like we intended. We can now move onto loading the WASM binary in our JavaScript code in the frontend.

## Loading WASM in the front-end

When it comes to loading WASM in our frontend application, we must carry out the steps sown in figure 7.1.

Figure 7.1 – Steps to Run a WASM Function in the Browser

Figure 7.1 depicts the steps below:

- A React component makes a request to the file server for the WASM file.
- The file server gets the WASM file.
- The file server then returns the WASM file back to the frontend for the requesting React component to load it.
- The WASM functions are then stored in the component state to be called as and when needed.

Even though it might be intuitive to handle the WASM loading and function in the task component, we would end up requesting a WASM file for every task in our to-do application. For our toy project this might be ok as we will not notice any difficulties, however, as the application scales and more users use our application, we would really struggle to keep up with demand. We could reduce the number of calls by caching in the browser and we can do this in the header of the response serving the WASM file. We should do this anyway, but to reduce the risk of excessive calls even more we are going to load the WASM file in the main application.

We can load our WASM in the `src/index.tsx` file with the code below:

```typescript
// File: ingress/frontend/src/index.tsx
import init, { rust_generate_button_text } from '../rust-interface/pkg/rust_interface.js'
```

Here, we will use the `init` to get the WASM binary. The `init` function handles the request to the server for the WASM binary. It also handles the initialization of the WASM module, which is loading the WASM module into memory, and importing functions needed for the WASM library to run. We can see that we are loading the `rust_generate_button_text` function from a JavaScript file. If we look at the JavaScript file in the pkg directory of the WASM build and search for the `rust_generate_button_text` function, we get the following code. For now this is just the outline of the function but we will cover the sections.

```javascript
// File: ingress/frontend/rust-interface/pkg/rust_interface.js
let WASM_VECTOR_LEN = 0;
...
export function rust_generate_button_text(status) {
    let deferred2_0;
    let deferred2_1;
    try {
        ...
    } finally {
        ...
    }
}
```

The `deferred2_0` and `deferred2_1` are pointers that are going to be used for deallocating memory. In the try section, the function initially adjusts the memory pointer to reserve space for the function's execution with the following code:

```javascript
// File: ingress/frontend/rust-interface/pkg/rust_interface.js
try {
    const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
    ...
}
```

It then uses the memory allocation that has been generated in the WASM program to allocate memory to the WASM library memory buffer that is the length of the bytes of the string, returning the pointer to where that memory allocation starts and the length of the memory buffer with the code below:

```javascript
// File: ingress/frontend/rust-interface/pkg/rust_interface.js
const ptr0 = passStringToWasm0( status, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc );
const len0 = WASM_VECTOR_LEN;
```

The function can then execute our function and get the result of our function with the following code:

```javascript
// File: ingress/frontend/rust-interface/pkg/rust_interface.js
wasm.rust_generate_button_text(retptr, ptr0, len0);
var r0 = getInt32Memory0()[retptr / 4 + 0];
var r1 = getInt32Memory0()[retptr / 4 + 1];
deferred2_0 = r0;
deferred2_1 = r1;
return getStringFromWasm0(r0, r1);
```

Here we can see that we pass the pointer for our string buffer in memory, and the length of the buffer. This enables our WASM function to extract the string data from the memory and use it in the way that we intended to use. Our WASM function then puts the return string into the memory. The pointers for the return string are then extracted, and this is used to get the string from WASM. To get the string from WASM, you would access the bytes in the memory using the pointer and the length of the buffer, and then deserialize those bytes to a string. Finally, the function deallocates the memory as we no longer need it with the code below:

```javascript
// File: ingress/frontend/rust-interface/pkg/rust_interface.js
finally {
    wasm.__wbindgen_add_to_stack_pointer(16);
    wasm.__wbindgen_free(deferred2_0, deferred2_1, 1);
}
```

Now that we understand how our WASM function interfaces with the application, we know what the JavaScript function that we imported into our `src/index.tsx` file, this means that the interaction with the raw pointers to our WASM function will be bundled into our application. This means that we only must worry about serving the WASM file, not serving or configuring any glue code because the glue code is in our JavaScript bundle.

Going back to our main app, component, we now need to store the WASM function in our state and a flag on whether the WASM binary has been loaded or not. To have the whole app we aware of the WASM loading state the hooks that our App component should have the following:

```typescript
// File: ingress/frontend/src/index.tsx
const [data, setData] = useState(null);
const [wasmReady, setWasmReady] = useState<boolean>(false);
const [ RustGenerateButtonText, setRustGenerateButtonText ] = useState<(input: string) => string>(null);
```

Here we can see that we still house our to-do items in data, but we declare that the WASM is ready with `wasmReady` and we house the Rust function that is compiled to WASM with `RustGenerateButtonText`. We can the load our WASM binary and set the WASM ready flag with the code below:

```typescript
// File: ingress/frontend/src/index.tsx
React.useEffect(() => {
    init().then(() => {
        setRustGenerateButtonText(() => rust_generate_button_text);
        setWasmReady(true);
    }).catch(e => console.error( "Error initializing WASM: ", e ));
}, []);
```

The fetching of the WASM binary might take some time. We do not want to run the risk of getting the items from the server before we get the WASM function because we need the WASM function to create the text for each to-do item. Therefore, we must refactor our loading of the to-do items with the following code:

```typescript
// File: ingress/frontend/src/index.tsx
React.useEffect(() => {
    const fetchData = async () => {
        if (wasmReady) {
            const response = await getAll<ToDoItems>();
            setData(response.data);
        }
    };
    if (wasmReady) {
        fetchData();
    }
}, [wasmReady]);
```

Here we can see that the `useEffect` will only fire when the `wasmReady` changes based on the dependency declared by `[wasmReady]`. We do not know the future and the `wasmReady` might change back to false later, therefore, even in the `wasmReady` changes, we check that the `wasmReady` is true before making calls to get the to-do items from the server.

Finally, in our return statement, we can call the WASM function for the status of each to-do item with the code below:

```typescript
// File: ingress/frontend/src/index.tsx
<h1>Pending Items</h1>
<div>
    {data.pending.map((item, index) => (
        <><ToDoItem key={item.title + item.status} title={item.title} buttonMessage={ RustGenerateButtonText(item.status) } id={item.id} passBackResponse={reRenderItems}/></>
    ))}
</div>
<h1>Done Items</h1>
<div>
    {data.done.map((item, index) => (
        <><ToDoItem key={item.title + item.status} title={item.title} buttonMessage={ RustGenerateButtonText(item.status) } id={item.id} passBackResponse={reRenderItems}/></>
    ))}
</div>
```

You may have noticed a change in the parameters passed into the `ToDoItem` components. This is because we have taken advantage of this WASM refactor to simplify the `ToDoItem` component and get rid of the state of the `ToDoItem` component which was never really needed. In real life, as a project grows and deadlines loom, the best implementation does not always land straight away. Keeping on top of a degrading codebase takes work. It's just like gardening, you need to be constantly pruning. Big refactors can introduce bugs, delay projects, cause breaking changes elsewhere in the system, and hold up other developers as they must review the big refactor. What's more is that the code is not static if other developers are also working on the same codebase. If you have a big refactor, then chances are that your code changes will clash with other developer's work. A good way to prevent these issues but not let the code slip into disorder degrading over time is the philosophy of "leaving code better off than when you found it". This refactor is a perfect example of this. Here we do a small simplification and the scope of code that we are changing is in the scope of where our WASM interactions are based. If you would like to, now would be a good time to try and refactor the `ToDoItem` component yourself.

If you have attempted to refactor the `ToDoItem` component, hopefully, it is like the code below:

```typescript
// File: ingress/frontend/src/components/ToDoItem.tsx
interface ToDoItemProps {
    title: string;
    id: number;
    passBackResponse: (response: any) => void;
    buttonMessage: string;
}
export const ToDoItem: React.FC<ToDoItemProps> = ( {
    title, id, passBackResponse, buttonMessage }) => {
    const sendRequest = async (): void => {
        // The send request code is the same
        ...
    };
    return (
        <div className="itemContainer" id={id}>
            <p>{title}</p>
            <button className="actionButton" onClick={sendRequest}>{buttonMessage}</button>
        </div>
    );
}
```

We now have everything ready to integrate WASM into the frontend. If we run our server now, we should see the network calls shown in figure 7.2.

Figure 7.2 – Frontend network calls with WASM

In figure 7.2 we can see that the bundles are loaded, then the WASM is loaded, then finally the to-do items are loaded. The fact that our buttons are rendering prove that our WASM function works as we intended it to.

And there we have it, we now have WASM loading and running in the frontend of our application! We can execute Rust in the browser!

But what about Rust interfacing with WASM? Or WASM executing on the local machine? We will explore these ideas in the next section.

## Loading WASM on the local machine

In this section, we are going to build a WASM module, and have our own Rust code load the WASM file and interact with it. We are also going to build a kernel to pass complex data structures over the WASM boundary like what we saw in the JavaScript memory allocation code in the previous section. To carry out this exercise, we are going to need the following file layout:

```text
├── kernel
│   ├── Cargo.toml
│   └── src
│       └── lib.rs
├── wasm-client
│   ├── Cargo.toml
│   └── src
│       └── main.rs
└── wasm-lib
    ├── Cargo.toml
    ├── scripts
    │   └── build.sh
    └── src
        └── lib.rs
```

These workspaces have the following purposes:

- `kernel`: to provide structs to both `wasm-lib` and `wasm-client` workspaces so they can both compile the same structs for serialization and deserialization of structs and bytes.
- `wasm-lib`: to build a WASM binary with some functionality with the structs passed into the WASM binary.
- `wasm-client`: to load the WASM binary and interact with that WASM binary, passing data to the WASM program, and extracting results.

Seeing as both the WASM lib and WASM client rely on the kernel, we will start with the kernel.

## Building a WASM kernel

When people here the word "kernel", there tends to be some confusion. Because almost everyone has heard of the term "Linux kernel", is fair to assume that you are going something directly with the operating system. However, the term kernel is more boarder than that. If you google "domain driven design kernel", you will see a range of definitions, but they will all essentially boil down to a kernel being a bounded context that is shared across multiple bounded contexts, and this is what we are building. For our kernel, we simply need to build a data struct that both bounded contexts (client and lib) need to reference.

We could just build the same structs in both WASM clients and libraries, and these would technically work as we are going to serialize our structs before sending them over the WASM boundary. However, maintaining consistency when there's multiple duplicates of a struct is harder, and if both client and lib end up being compiled into the same Rust program later, the compiler will acknowledge that the structs have the same name but are defined in different places. This will result in us manually converting the struct from the client to the lib struct to satisfy the compiler. Considering it being harder to maintain consistency and the two different structs not satisfying the compiler, it makes sense to have a single source of truth for the data struct that is a kernel with its own workspace. Because the kernel is in its own workspace, we can then compile it into any program that needs to communicate with another that has also compiled the kernel as seen in figure 7.3.

Figure 7.3 – Kernel enables easy scaling of programs communicating.

For our Kernel workspace we have the following dependencies:

```toml
// File: kernel/Cargo.toml
[dependencies]
serde = { version = "1.0.203", features = ["derive"] }
```

Now with our dependency defined, we can build out the basic data struct with the code below:

```rust
// File: kernel/src/lib.rs
use serde::{Serialize, Deserialize};
#[derive(Serialize, Deserialize, Debug)]
pub struct SomeDataStruct {
    pub name: String,
    pub names: Vec<String>,
}
```

If we recall how the string was transferred over the WASM boundary when inspecting the JavaScript code that interacted with the WASM binary, we also need a pointer that will point to the serialized struct in memory. We also need this in our kernel. Our pointer takes the following form:

```rust
// File: kernel/src/lib.rs
#[repr(C)]
pub struct ResultPointer {
    pub ptr: i32,
    pub len: i32
}
```

The `ptr` is the pointer to the memory address of the start of where the serialized `SomeDataStruct` is stored in memory. With the length we can calculate the start and end of the memory address to access the serialized `SomeDataStruct` in memory. The `#[repr(c)]` macro for the `ResultPointer` struct ensures that the `ResultPointer` layout is consistent with the C representation for ABI compatibility, field order, and padding/alignment. This ensures that the memory layout is predictable from a C perspective.

What's with all the C references?

At the time of writing this, Rust does not have a stable application binary interface (ABI). C on the other hand does have a stable ABI due to its simplicity and years of maturity. At this point in time, Rust is still changing and trying to improve. If Rust established a stable ABI, it would make it harder to move forward with new improvements. Seeing as the kernels in Windows, MacOS, and Linux are written in C, C data types are universally accepted. Just Google the "foreign function interface C …" and then whatever language you are interested in and I'd be shocked if there isn't an option of interacting with C data and functions in that language.

Our kernel is now ready, and we can move onto our WASM library.

## Building a WASM library

For our WASM library, we are going to receive a pointer and length to the serialized struct in memory, we are then going to carry out the following steps:

1. receive a pointer to a serialized struct in memory and a length.
2. extract the bytes of the struct from memory.
3. deserialize the struct.
4. add a name to the struct.
5. serialize the updated struct to bytes.
6. put the serialized struct into memory.
7. return the pointer to the memory address of the updated serialized structs.

While these numbered steps help distill what we are carrying out, we will not break down the implementation of our WASM library into numbered steps, as the implementation of these numbered steps come all at the end when the entry point function is built.

Before we write any code, our WASM library has the following dependencies:

```toml
// File: wasm-lib/Cargo.toml
[dependencies]
bincode = "1.3.3"
kernel = { path = "../kernel" }
serde = { version = "1.0.203", features = ["derive"] }
[lib]
crate-type = ["cdylib"]
```

We are going to be using `bincode` for the serialization as we are writing our WASM library in Rust and we are interacting with our WASM library with Rust. If it was a different language interacting with the WASM library, we would need another serialization method but for this example, `bincode` works. We must also note that we are not using any WASM dependencies here, instead we are just going to expose C interfaces and directly interact with them. Do not worry, we will still be able to compile into WASM without any WASM crates declared in the `Cargo.toml` file.

We can now start writing our C interfaces. To do this, we need the following imports:

```rust
// File: wasm-lib/src/lib.rs
extern crate alloc;
use core::alloc::Layout;
use kernel::{ SomeDataStruct, ResultPointer };
```

Here, we are basically importing the structs from the kernel, and then memory allocation struct, and library. With these imports we can create our memory allocation function with the code below:

```rust
// File: wasm-lib/src/lib.rs
#[no_mangle]
pub unsafe extern "C" fn ns_malloc( size: u32, alignment: u32 ) -> *mut u8 {
    let layout = Layout::from_size_align_unchecked( size as usize, alignment as usize );
    alloc::alloc::alloc(layout)
}
```

Here, we are taking the size of memory needed, and the alignment or where that memory allocation starts in the memory. We then create a `Layout` struct which is just a slice of memory. Finally, we pass that slice of memory into the `alloc` function which allocates the memory for that slice of memory. We then return a `*mut u8` which is a raw pointer to a memory address. We must note that the function is `unsafe`, as the `from_size_align_unchecked` function is unsafe.

Now that we have our memory allocation, we must free up the memory when we no longer need it. If we do not free up the memory, the used memory will continue to grow. This is called a memory leak. If the program is not stopped, the computer will cease with the program due to running out of memory. Our free function takes the following form:

```rust
// File: wasm-lib/src/lib.rs
#[no_mangle]
pub unsafe extern "C" fn ns_free( ptr: *mut u8, size: u32, alignment: u32 ) {
    let layout = Layout::from_size_align_unchecked( size as usize, alignment as usize );
    alloc::alloc::dealloc(ptr, layout);
}
```

Here we can see that we get the layout again, but we apply a deallocation as opposed to an allocation.

All our memory management code is finally done, we can now move onto our entry point which takes the outline below:

```rust
// File: wasm-lib/src/lib.rs
#[no_mangle]
pub extern "C" fn entry_point( ptr: *const u8, len: usize) -> *const ResultPointer {
    ...
}
```

Inside our entry point function, we get the bytes from the memory, deserialize the bytes to our `SomeDataStruct` struct, and then alter the deserialized struct with the following code:

```rust
// File: wasm-lib/src/lib.rs
let bytes = unsafe { std::slice::from_raw_parts(ptr, len) };
let mut data_struct: SomeDataStruct = bincode::deserialize( bytes ).unwrap();
data_struct.names.push("new name".to_string());
```

That's it, that's the logic of our WASM program done, so we now must serialize the data, and get the length and pointer to the serialized data with the code below:

```rust
// File: wasm-lib/src/lib.rs
let serialized_data = bincode::serialize( &data_struct ).unwrap();
let len = serialized_data.len();
let out_ptr = serialized_data.leak().as_ptr();
```

You can see that we are just getting the pointer from the serialized struct. Here, the memory is not automatically cleaned up otherwise foreign function interfaces would be hard to implement. We can just return the pointer, and then manually clean up the memory later when we need. Now that we have our pointer and length of the memory buffer, we can return the pointer and length with the following code:

```rust
// File: wasm-lib/src/lib.rs
let result = Box::new(ResultPointer{ ptr: out_ptr as i32, len: len as i32 });
Box::into_raw(result) as *const ResultPointer
```

Our library is nearly done, all the rust code is finished, but we now must build a bash script to build and copy over the WASM binary so we can ensure that the WASM binary is where we want it to be. Our bash script just takes the following form:

```bash
# File: wasm-lib/scripts/build.sh
#!/bin/bash
# navigate to directory
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
cd $SCRIPTPATH
cd ..
cargo build --release --target wasm32-wasi
cp target/wasm32-wasi/release/wasm_lib.wasm ./wasm_lib.wasm
wasm2wat ./wasm_lib.wasm > ./wasm_lib.wat
```

This script takes a similar form to our build bash script in our frontend. We are now ready to build the client.

## Building a WASM client

For our client, we must serialize and deserialize data, and load the WASM file. Because of this, we have the following dependencies:

```toml
// File: wasm-client/Cargo.toml
[dependencies]
serde = { version = "1.0.203", features = ["derive"] }
tokio = { version = "1.39.0", features = ["full"] }
bincode = "1.3.3"
kernel = { path = "../kernel" }
wasmtime-wasi = { version = "23.0.0", features = ["preview1"]}
wasmtime = "23.0.0"
```

Now in our dependencies are defined, we can start building the client with the outline below:

```rust
// File: wasm-client/src/main.rs
use wasmtime::{Result, Engine, Linker, Module, Store, Config};
use wasmtime_wasi::preview1::{self, WasiP1Ctx};
use wasmtime_wasi::WasiCtxBuilder;
use std::mem::size_of;
use std::slice::from_raw_parts;
use kernel::{ SomeDataStruct, ResultPointer };
#[tokio::main]
async fn main() -> Result<()> {
    ...
}
```

Inside our main function, we start with starting a WASM engine, and loading the WASM binary with the following code:

```rust
// File: wasm-client/src/main.rs
let mut config = Config::new();
config.async_support(true);
let engine = Engine::new(&config).unwrap();
let module = Module::from_file( &engine, "../wasm-lib/wasm_lib.wasm" ).unwrap();
```

Now that we have loaded our WASM library into our engine, we can link preview1 and configs with the code below:

```rust
// File: wasm-client/src/main.rs
let mut linker: Linker<WasiP1Ctx> = Linker::new(&engine);
preview1::add_to_linker_async(&mut linker, |t| t).unwrap();
let pre = linker.instantiate_pre(&module)?;
let wasi_ctx = WasiCtxBuilder::new()
    .inherit_stdio()
    .inherit_env()
    .build_p1();
```

Here, we can see that we enable standard IO and environment variables. Preview1 is essentially a version of WASM. For instance, at the time of writing this book, preview is currently being worked on for better support for sockets. Our runtime must enable the standard IO and environment variables because by itself, WASM is locked down, and can't access operating system interfaces by itself. This makes sense as WASM can be loaded and ran in the browser.

Now that we have the wasmtime runtime build with our config, we can create a memory store and create an instance of our WASM module with the following code:

```rust
// File: wasm-client/src/main.rs
let mut store = Store::new(&engine, wasi_ctx);
let instance = pre.instantiate_async(&mut store).await.unwrap();
```

With the instance being created, we are now at the stage where we serialize the our data struct with the code below:

```rust
// File: wasm-client/src/main.rs
let data_struct = SomeDataStruct {
    names: vec!["name1".to_string(), "name2".to_string()],
    name: "name3".to_string()
};
let serialized = bincode::serialize(&data_struct).unwrap();
```

With our serialized struct, we can assign our struct bytes to our WASM instance memory with the following code:

```rust
// File: wasm-client/src/main.rs
// allocate the memory for the input data
let malloc = instance.get_typed_func::<(i32, i32), i32>( &mut store, "ns_malloc" ).unwrap();
let input_data_ptr = malloc.call_async( &mut store, (serialized.len() as i32, 0) ).await.unwrap();
// write the contract to the memory
let memory = instance.get_memory( &mut store, "memory" ).unwrap();
memory.write( &mut store, input_data_ptr as usize, &serialized ).unwrap();
```

Our serialized struct is now in the memory of the WASM instance, we can now get our entry point function from the WASM module, and then call the entry point function to get the pointer to the result with the code below:

```rust
// File: wasm-client/src/main.rs
let entry_point = instance.get_typed_func::<(i32, i32), i32>( &mut store, "entry_point" ).unwrap();
let ret = entry_point.call_async( &mut store, (input_data_ptr, serialized.len() as i32) ).await.unwrap();
```

We can now use the pointer to read the bytes from the memory. First, we get the result pointer from the memory with the following code:

```rust
// File: wasm-client/src/main.rs
let mut result_buffer = Vec::with_capacity( size_of::<ResultPointer>() );
for _ in 0..size_of::<ResultPointer>() {
    result_buffer.push(0);
}
memory.read( &mut store, ret as usize, &mut result_buffer ).unwrap();
let result_struct = unsafe { &from_raw_parts::<ResultPointer>( result_buffer.as_ptr() as *const ResultPointer, 1 )[0] };
```

Now that we have our pointer, we use this result pointer to read the struct we want from memory with the code below:

```rust
// File: wasm-client/src/main.rs
let mut output_buffer: Vec<u8> = Vec::with_capacity( result_struct.len as usize );
output_buffer.resize(result_struct.len as usize, 0);
memory.read( &mut store, result_struct.ptr as usize, &mut output_buffer ).unwrap();
let output = bincode::deserialize::<SomeDataStruct>( &output_buffer ).unwrap();
println!("Output contract: {:?}", output);
```

And we now have the altered data that we want. Before we finish our program, we should clean up the rest of the memory with the following code:

```rust
// File: wasm-client/src/main.rs
let free = instance.get_typed_func::<(i32, i32, i32), ()>( &mut store, "ns_free" ).unwrap();
free.call_async( &mut store, (input_data_ptr, serialized.len() as i32, 0) ).await.unwrap();
free.call_async( &mut store, (result_struct.ptr, result_struct.len, 0) ).await.unwrap();
free.call_async( &mut store, (ret, size_of::<ResultPointer>() as i32, 0) ).await.unwrap();
Ok(())
```

And now our program is finished. If we run it, we get the following printout:

```text
Running `target/debug/wasm-client`
Output contract: SomeDataStruct { name: "name3", names: ["name1", "name2", "new name"] }
```

Here, we can see that our struct was passed through to the WASM module, altered, and then passed back to the Rust host to be displayed. We have now managed to directly interact with WASM without any helper crates creating the interfaces in the WASM library. As the features and support for WASM increases, you will be able to interact with these updates and feel confident to run whatever the bytecode alliance throws your way.

## Summary

In this chapter we got to grips with packaging WASM for the frontend and interacting with WASM using Rust. While we did not build anything substantial in WASM, we focused on building a foundational knowledge on how WASM is interacted with. At the time of writing this book, the APIs, and features of WASM is rapidly changing. If we built feature rich WASM program using the current APIs, this book would age quickly, and you would be having to Google the new APIs to figure out what you need to change to get your system running. Keeping an eye on WASM is a good idea. The advantage of compiling once and running anywhere including the browser is a strong advantage to have.

We are now at the stage where our basic application works in terms of creating, updating, and deleting to-do items. However, if we were to deploy it right now for multiple users, our data storage solution would slow down the number of requests and we would be held back by the lack of concurrency and networking capabilities our storage solution has. In the next chapter, we will upgrade our storage solution from a file to a Postgres database.

## Questions

What is the WASM memory model and how is this different from the Random-Access Memory model (RAM)?

How can you send a complex data structure over the WASM boundary?

Let's say you have a compiled WASM binary in your public folder, how do you load functions from that WASM file into your React application.

How do we check to see if the functions that we defined in the WASM program are in the WAM binary?

What are the advantages of building a separate kernel in its own workspace?

## Answers

WASM has a linear memory model meaning that all the memory is next to each other in one block. This results in faster memory access as we can just increment our pointer to access the next segment of memory. However, the linear memory model is more dangerous due to a hacker having access to the entire memory of the program by exploiting a buffer overflow. Luckily Rust is memory safe, so it is not much of an issue, however, be careful when implementing unsafe memory allocation functions as we did in the chapter.

You can send a complex data structure over the WASM boundary by serializing the data structure, allocating the bytes of the serialized data structure in the WASM instance memory, and then passing the pointer and length of the byte buffer through to the WASM program. The WASM program can then access the bytes in the memory and deserialize it.

Your React component can use the `useEffect` to fire the `init` from wasm-pack to fetch the binary. Once the binary is served, we can update the state of the Component to stay that the WASM binary has been loaded. We can also use other `useEffect` statements to perform actions on the other parts of the component that depend on the WASM binary with the `useEffect` statement having a dependency of a flag to say that WASM is loaded.

We use the `wasm2wat` command on the WASM file and write the output to a wat file. In the wat file we can then scroll down to the bottom to see the export functions section. If our function is there, that means that it is accessible to users to interact with the WASM binary.

A kernel in its own workspace acts as a single source of truth, we only must update it once and all modules and programs communicating through that kernel get that update. We can also scale the number of programs and libraries interacting with the program that has the kernel easily without having to write any more boilerplate code.
