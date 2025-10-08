# Displaying Content in the Browser

We are now at the stage where we can build a web application that can manage a range of HTTP requests with different methods and data. This is useful, especially if we are building a server for microservices. However, we might also want non-programmers to interact with our application to use it. To enable non-programmers to use our application, we must create a graphical user interface. However, it must be noted that this chapter does not contain much Rust. This is because other languages exist to render a graphical user interface. We will mainly use HTML, JavaScript, and CSS. These tools are mature and widely used for frontend web development. Whilst I personally love Rust (otherwise I wouldn't be writing a book on it), we must use the right tool for the right job. At the point of writing this book, we can build a frontend application in Rust using the Yew framework. However, being able to fuse more mature tools into our Rust technical stack is a more valuable skill.

This chapter will cover the following topics:

Building out a development setup

Serving frontend from rust

Connecting backed API endpoints to the frontend

Creating React components

Inserting styles with CSS

In the previous edition (Rust Web Programming: A hands-on guide to developing fast and secure web apps with the Rust programming language 2nd edition), we created a React server using and served the frontend separately. In this edition, we still setup a development server for quick iteration, but we embed all the frontend assets into the Rust binary and get the Rust server to directly serve the frontend itself, meaning that we only need one Rust binary for deployment.

By the end of this chapter, you know how frontend assets are served and you will be able to utilize this knowledge to get Rust code to serve JS frontend applications. You will also be able to build a React application with different components and insert CSS into that application so the users can interact with our application. To get the frontend talking to the backend, you will understand how to make API calls in the frontend to the backend.

## Technical requirements

We will also be building on the server code we created in the Chapter 5, Processing HTTP Requests which can be found at [PUT LINK HERE]

You can find the full source code that will be used in this chapter here:

[PUT LINK HERE]

You will also need esbuild so you will have to install esuild using the link below:

`https://esbuild.github.io/getting-started/`

## Building out Development Setup

Before we can start writing code that is executed in the browser to display our to-do items, we must setup our development environment. For our environment to support a React application, we need the following file layout:

─── ingress └── frontend ├── esbuild.js ├── ts.config.json ├── package.json ├── public │ └── index.html └── src └── index.tsx

Here, we can see that we have created an ingress directory. The ingres directory will house all the frontend code and will also have the main Rust server that will serve the frontend application and all backend endpoints. All our frontend code will be in the ingress/frontend directory, and all the assets that we will be serving after a frontend build will be in the ingress/frontend/public directory.

A good place to start is defining the parameters around the frontend build as we will get to see where our entry point for the build is, the options for the build, and the output directory of where the finished build is put. Before we define the build, we must import the following:

```javascript
// File: ingress/frontend/esbuild.js
const esbuild = require('esbuild');
const cssModulesPlugin = require('esbuild-css-modules-plugin');
```

The esbuild is used to define the build, and the cssModulePlugin is a plugin that enables esbuild to locally scope CSS class names to the JavaScript/TypeScript file importing and using that CSS. This removes clashes of CSS names from other JavaScript/TypeScript files. This will help us scale our code as the project grows and gets more complex.

With these imports, we then define the build with the code below:

```javascript
// File: ingress/frontend/esbuild.js
esbuild.build({
  plugins: [cssModulesPlugin()],
  entryPoints: ['src/index.tsx'],
  bundle: true,
  outfile: 'public/bundle.js',
  format: 'esm',
  define: { 'process.env.NODE_ENV': '"production"', },
  minify: true,
  sourcemap: true,
  loader: { '.js': 'jsx', '.tsx': 'tsx', '.ts': 'ts', '.wasm': 'binary', '.css': 'css' },
}).catch(() => process.exit(1));
```

Our build parameters have the following outcomes:

plugins: Here we add our CSS plugin, but we can also add other plugins if we want them.

entryPoints: Defines the entry point for the build. So, if any files are linked to the public/bundle.js file via imports in the code, these files will be included in the build.

bundle: Instructs esbuild to bundle all dependencies into a single output file. This makes is easier for us to deploy.

outfile: Where the bundle will be put once the build has finished.

format: Sets the format module format of the output bundle to ES module standard for JavaScript.

define: replaces instances of process.env.NODE_ENV to "production" in the code for optimizations.

minify: Enables minification to cut out unnecessary symbols and whitespace making the bundle file smaller.

sourcemap: Maps the bundled code back to source files which is handy for debugging.

loader: Specifies the types of files that we load into the builder.

While this build config will enable us to build our react application with Esbuild, we need some node modules and a node process to run the Esbuild. We can define our node dependencies and build processes in our ingress/frontend/package.json file with the following code:

```json
// File: ingress/frontend/package.json
{
  "name": "es_react",
  "version": "1.0.0",
  "scripts": {
    "build": "node esbuild.js",
    "serve": "serve public"
  },
  "dependencies": {
    "react": "^18.2.0",
    "react-dom": "^18.2.0"
  },
  "devDependencies": {
    "esbuild": "^0.19.11",
    "esbuild-css-modules-plugin": "^3.1.0",
    "serve": "^14.2.1"
  }
}
```

Here we can see that our application dependencies are just React based and these dependencies are defined in the "dependencies" section. We have defined the dependencies that we want for the build and serving in the "devDependencies" section. Here we can see that we are using esbuild for the build, we have also defined the plugin that we are using the for the build, and the serve dependency enables us to ser the React app when developing our application. These are in the "devDependencies" section because we do not need to add these modules to our bundle to be served to the client.

Before we move onto writing any typescript code, we must define our typescript build in the ts.config.json file with the following code:

```json
{
  "compilerOptions": {
    "target": "es5",
    "lib": ["dom", "dom.iterable", "esnext"],
    "allowJs": true,
    "skipLibCheck": true,
    "esModuleInterop": true,
    "allowSyntheticDefaultImports": true,
    "strict": true,
    "forceConsistentCasingInFileNames": true,
    "module": "CommonJS",
    "moduleResolution": "node",
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true,
    "jsx": "react-jsx"
  },
  "include": ["src"]
}
```

To avoid bloating the chapter, we will not breakdown the typescript config. This book is a book about Rust web programming, not a book on typescript.

If we tried to build our system now, it would break as we have not defined our app in our entry point. Our app entry point is JavaScript that gets an HTML element by ID, to insert our React app into that HTML element. If we are using Typescript, this Typescript is converted into JavaScript when the bundle is being created as JavaScript is the code that runs in the browser. For our entry point, we can build the logic around injecting our React application into the HTML file that is served by the server with the code below:

```typescript
// File: ingress/frontend/src/index.tsx
import React from 'react';
import ReactDOM from "react-dom/client";
const App = () => {
  return (
    <div>
      <h1>Hello World</h1>
    </div>
  );
};
const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(<App />);
```

For now, we are just injecting a `<h1>Hello World</h1>` into the element called "root" in the HTML file that is served. Now we need to get our development environment to serve our application.

First, we must have a HTML file that has an HTML element called "root" and loads our JavaScript bundle file that houses our React application. This is essentially an entry point for serving our application so we can call this HTML file index.html. We will put this HTML file in our public directory because this is where the assets being served are stored for our server to access. Our index.html file takes the following form:

```html
<!-- File: ingress/frontend/public/index.html -->
<!DOCTYPE html>
<html>
  <head>
    <title>My React App</title>
  </head>
  <body>
    <div id="root"></div>
    <script type="module" src="./bundle.js"></script>
  </body>
</html>
```

And we can configure the server to point to this index.html file with the server config file in the root of the frontend below:

```json
// File: ingress/frontend/serve.json
{
  "rewrites": [
    {
      "source": "**",
      "destination": "./public/index.html"
    }
  ]
}
```

This means that we route all our requests to the server to the index.html file. Therefore, all requests are going to load the HTML file which will then download the JavaScript script which will then insert the React app into the "root" element of the index.html file.

We can now run our server. First, we must install the node modules for the build and serving of our application with the following command:

npm install

The node_modules directory will be created with all the packages needed for the building and serving of our application. We then build our application with the command below:

npm run build

After a stream of build logs, we should see the bundle.js file in the public directory. We can then serve our app with the following command:

npm run serve

Which gives us the printout below:

> es_react@1.0.0 serve > serve public ┌──────────────────────────────────────────┐ │ Serving! │ │ - Local: `http://localhost:3000` │ │ - Network: `http://192.168.1.83:3000` │ │ Copied local address to clipboard! │ └──────────────────────────────────────────┘

If we visit the localhost in the browser, we are greeted with the content shown in Figure 6.1.

Figure 6.1 – Our Basic React App view for the first time

Our logs in our console will show a printout along the same lines as the following:

HTTP 14/04/2024 17:32:54 ::1 GET / HTTP 14/04/2024 17:32:54 ::1 Returned 304 in 17 ms HTTP 14/04/2024 17:32:54 ::1 GET /bundle.js

Here, we can see that our server gets the index.html file, and then gets the bundle.js file. This makes sense as our index.html file loads the bundle.js file. We can also inspect the network panel in our browser, and this will give us the same log as seen in Figure 6.2.

Figure 6.2 – Network logs of serving our react app

We can see that the HTML document is served, and then the JavaScript script is served. And here we have it! Our development environment is ready to build and serve our react application.

We could have just used the create react app tool to create a React application and serve it. However, there is a reason why you're working through a book as opposed to speed reading a tutorial online. Here we are taking the time to go through who our stack works and interacts with each other. This gives us more flexibility and ability to build a system when there are multiple moving parts. For instance, we could reconfigure our serve.json file to have the following configuration:

```json
{
  "rewrites": [
    {
      "source": "/test",
      "destination": "./public/test.html"
    },
    {
      "source": "**",
      "destination": "./public/index.html"
    }
  ]
}
```

With the preceding configuration, we can see that if the endpoint of the URL is test, we serve a different HTML file. This different HTML file could load a different JavaScript application using a different framework, or even load WASM assets. We can have multiple different bundles that are loaded by different HTML files. But we can even go further. Now that we know how the frontend is served, we do not have to use the serve node module, we can write our own servers that do this!

In the next section, we are going to apply what we have learnt about bundling and serving frontend assets to embed those frontend assets into our Rust server binary to be served by our Rust server.

## Serving frontend from Rust

We now know that we can serve our React application if we can serve a HTML file that can point to the bundle.js file. Of course we can do this using a Rust server. To achieve this, we can create an ingress workspace with the following directory layout:

ingress ├── Cargo.toml ├── frontend │ ├── esbuild.js │ ├── package.json │ ├── serve.json │ ├── public │ │ ├── bundle.js │ │ ├── bundle.js.map │ │ └── index.html │ └── src │ └── index.tsx └── src └── main.rs

Here we can see that the server in the ingress/src/main.rs file is going to point to our ingress/frontend/public folder to server the assets. At this point, our Cargo.toml file in the root of the now looks like the following:

```toml
# File: Cargo.toml
[workspace]
resolver = "2"
members = [
  "glue",
  "ingress",
  "nanoservices/to_do/core",
  "nanoservices/to_do/dal",
  "nanoservices/to_do/networking/actix_server"
]
```

By the end of the book, our ingress will accept all incoming requests and either serving frontend assets or backend endpoints. All our nanoservices will essentially be compiled into the ingress workspace. In this chapter, we will only focus on the serving of frontend assets. However, by chapter 11 building RESTful services, our entire system will be compiled into the ingress.

Before we write any code for our ingress, our ingress Cargo.toml file has the following dependencies:

```toml
# File: ingress/Cargo.toml
[package]
name = "ingress"
version = "0.1.0"
edition = "2021"
[dependencies]
rust-embed = "8.3.0"
mime_guess = "2.0.4"
actix-web = "4.5.1"
tokio = { version = "1.35.0", features = ["macros", "rt-multi-thread"] }
```

Here we will be using the rust-embed crate to embed the frontend assets into our Rust binary on the build. We then use the mime_guess crate to work out what type of file we are serving when defining the file type when returning it to the browser. This will help us serve CSS, images, WASM files etc. The last thing boilerplate we need to define is our HTML file in the root of the ingress workspace which has the content below:

```html
<!-- File: ingress/index.html -->
<!DOCTYPE html>
<html>
  <head>
    <title>My To Do App</title>
  </head>
  <body>
    <div id="root"></div>
    <script type="module" src="./frontend/public/bundle.js">
    </script>
  </body>
</html>
```

We are now at the stage where we can write our main.rs file in our ingress workspace to define our server. Before we write any server code, we must import the following structs and traits:

```rust
// File: ingress/src/main.rs
use actix_web::{ web, App, HttpServer, Responder, HttpResponse, HttpRequest };
use rust_embed::RustEmbed;
```

Now we must embed our entry point that is the ingress/index.html file. We also need to return the HTML in a HTTP response with the following code:

```rust
// File: ingress/src/main.rs
async fn index() -> HttpResponse {
    HttpResponse::Ok().content_type("text/html") .body(include_str!("../index.html"))
}
```

The include_str! macro embeds the contents of the file as a string so once the Rust code is compiled, we do not need to move the index.html file with the Rust binary, we only need the compiled Rust binary to serve the HTML. Next, we embed the ingress/frontend/public directory with the code below:

```rust
// File: ingress/src/main.rs
#[derive(RustEmbed)]
#[folder = "./frontend/public"]
struct FrontendAssets;
```

We can now access files from the ingress/frontend/public directory via the FrontendAssets struct as our frontend assets are now embedded into the Rust binary. Now that we have embedded our frontend assets, we can then serve these assets with the following code:

```rust
// File: ingress/src/main.rs
fn serve_frontend_asset(path: String) -> HttpResponse {
    let file = match Path::new(&path).file_name() {
        Some(file) => file.to_str().unwrap(),
        None => return HttpResponse::BadRequest() .body("404 Not Found")
    };
    match FrontendAssets::get(file) {
        Some(content) => HttpResponse::Ok() .content_type(mime_guess::from_path(&file) .first_or_octet_stream().as_ref()) .append_header( ("Cache-Control", "public, max-age=604800") ) .body(content.data),
        None => HttpResponse::NotFound().body("404 Not Found")
    }
}
```

When it comes from getting a file from our FrontendAssets struct, we do not need the full path, instead, we only need the path from the /frontend/public path. Because of this, we trim the path of the request as our HTML file loads the bundle with the /frontend/public/bundle.js path. So, when our embedded HTML file makes the request for the bundle, we will trim the path to bundle.js by just getting the file_name of the path. We then see if the file has been embedded into the FrontendAssets struct. If the file is not present, we return a not found response. If there is a file, we use the mime_guess crate to insert the data type into the header of the response. We also insert a Cache-Control parameter into the header. The max-age is calculated in seconds, so our assets will be cached in the browser for a week. Finally, we insert the data of the file in the body of the HTTP response and there we have it, our embedded frontend assets can be extracted and served to the browser.

We could just wrap the serve_frontend_asset function in an async function that accepts a request and returns a HTTP response. However, we will want to serve frontend assets no matter what the URL request is and serve backend API endpoints. This means that we have a function that is executed when all other endpoints were not hit. This is essentially a catch all. Our catch all function takes the following signature:

```rust
// File: ingress/src/main.rs
async fn catch_all(req: HttpRequest) -> impl Responder {
    . . .
    index().await
}
```

The for the first check in our catch function, if the request has a /api/ in the URL, then we return a not found because this request was clearly intended for a backend endpoint. If the request has hit the catch function, this means that the request has not matched with any of the backend endpoints. Our first check is defined with the code below:

```rust
// File: ingress/src/main.rs (function = catch_all)
if req.path().contains("/api/") {
    return HttpResponse::NotFound().finish()
}
```

For our next check, we can serve the frontend asset if there is a /frontend/public path in the request with the following code:

```rust
// File: ingress/src/main.rs (function = catch_all)
if req.path().contains("frontend/public") {
    return serve_frontend_asset(req.path().to_string())
}
```

Our next check is to inspect the file type with the code below:

```rust
// File: ingress/src/main.rs (function = catch_all)
let file_type = match mime_guess::from_path(&req.path()) .first_raw() {
    Some(file_type) => file_type,
    None => "text/html"
};
```

If there is no file type in the request, it might just be hitting the route or our URL, therefore, we know that we want the index.html. However, it might be another file type. For instance, when adding assets like images, it is common to reference the image path relative to the bundle, meaning that /frontend/public path is not in the asset path. Therefore, if the file type is not an HTML file, we can serve the frontend asset with the code below:

```rust
// File: ingress/src/main.rs (function = catch_all)
if !file_type.contains("text/html") {
    return serve_frontend_asset(req.path().to_string())
}
```

And finally, if the request passes all these checks, we can just serve the index.html file with index.await return as shown in the initial signature of the catch_all function.

We are finally at the point of defining our server that has our catch_all function as the default service with the following code:

```rust
// File: ingress/src/main.rs
#[tokio::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
        .default_service(web::route().to(catch_all))
    })
    .bind("0.0.0.0:8001")?
    .run()
    .await
}
```

And now when we perform a cargo run command, we can access our react application through our Rust server as seen in Figure 6.3.

Figure 6.3 – Our Basic React App view from our Rust server

We can see that even though our URL has /something/else/ at the end, our React application is served. We will let our React application to handle the missing frontend endpoint. If we make a backend call that our server does not support, we get a not found response as seen in Figure 6.4.

Figure 6.4 – A not found response to an API endpoint

And here we conclude that our Rust server is serving our frontend. We can also serve our frontend with NGINX which we will cover in chapter 15: configuring basic HTTPS with NGINX. For the next section, we will connect the API endpoints for our to-do items to the frontend.

## Connecting backend API endpoints to the frontend

We are going to connect our frontend to our backend server via HTTP requests. This means that we must connect our to-do nanoservice to our ingress. Before we can do this we must add the following decencies to our ingress Cargo.toml file with the following code:

```toml
# File: ingress/Cargo.toml
[dependencies]
. . .
actix-cors = "0.7.0"
to_do_server = { path = "../nanoservices/to_do/networking/actix_server", package = "actix_server" }
```

We are going to use actix-cors to enable requests from other locations. Right now, this will not be an issue as our frontend will be making requests from the localhost on the same computer at the server. However, when we deploy our application, the React application will be running on a user's computer and their requests will come from their computer, not the same computer that our server is running on. We have also been compiling our to-do nanoservice into our ingress binary. However, it must be noted that we assign an alias of our actix_server package to to_do_server. This is because we might have another nanoservice that has the networking layer called actix_server and we do not want clashes.

We can now redefine our server in the main.rs file of the ingress workspace. First, we use the following:

```rust
// File: ingress/src/main.rs
. . .
use to_do_server::api::views_factory as to_do_views_factory;
use actix_cors::Cors;
```

We can now attach the views factory and CORs to our server with the code below:

```rust
// File: ingress/src/main.rs
#[tokio::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        let cors = Cors::default().allow_any_origin() .allow_any_method() .allow_any_header();
        App::new()
        .configure(to_do_views_factory)
        .wrap(cors)
        .default_service(web::route() .to(catch_all))
    })
    .bind("0.0.0.0:8001")?
    .run()
    .await
}
```

And this is it, our server is now ready to serve our to-do endpoints to anywhere in the world if we had our server running on a public server that others can make requests to. However, there are a couple of moving parts. For instance, you must build the frontend before embedding it. If you make a change in the frontend, there is a risk you can rebuild your server without rebuilding the frontend. This can lead to time wasted trying to figure out the simple mistake to why your changes are not showing. We can automate this with the following bash script for running a server in the scripts directory of the ingress:

```bash
#!/usr/bin/env bash
# File: ingress/scripts/run_server.sh
# navigate to directory
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
cd $SCRIPTPATH
cd ..
cd frontend
npm install
npm run build
cd ..
cargo clean
cargo run
```

We are now less likely to miss a step in our server build process saving us from confusion. We can now move onto the frontend which has the following new files and directories:

├── . . . ├── src │ ├── api │ │ ├── create.ts │ │ ├── delete.ts │ │ ├── get.ts │ │ ├── update.ts │ │ ├── url.ts │ │ └── utils.ts │ ├── index.tsx │ └── interfaces │ └── toDoItems.ts . . .

Now that we have our project structure, we can implement our HTTP request using the following steps:

define our interfaces

define our API utils

define our get API call

integrate our get API call in our main app

We take this order because each step relies on what is built in the previous step. However, before we write any code, we need the axios package to make HTTP requests so our frontend package.json file dependencies should look like the following:

```json
// File: ingress/frontend/package.json
"dependencies": {
  "react": "^18.2.0",
  "react-dom": "^18.2.0",
  "axios": "^1.6.8"
}
```

We now have everything we need to define our interfaces.

Our interfaces take the following form:

```typescript
// File: ingress/frontend/src/interfaces/toDoItems.ts
export enum TaskStatus {
  PENDING = 'PENDING',
  DONE = 'DONE'
}
export interface ToDoItem {
  title: string;
  status: TaskStatus;
}
export interface ToDoItems {
  pending: ToDoItem[];
  done: ToDoItem[];
}
```

These interfaces are merely the structure of the data that we are passing between the server and the frontend. With these interfaces, we will be able to create items, update them, delete items, and get all items.

There is one more interface that we must construct, and this is the URL. We could define our URL as and when we need however, this makes it harder for us to maintain. If we have all our routes defined in one place, we can update mechanisms around constructing the URL, and we can keep track of all our URL endpoints in one place. Seeing as our URL will be used when making API calls, we can define our URL interface in the api/url.ts file with the following outline:

```typescript
// File: ingress/frontend/src/api/url.ts
export class Url {
  baseUrl: string;
  create: string;
  getAll: string;
  update: string;
  constructor() {
    . . .
  }
  static getBaseUrl(): string {
    . . .
  }
  deleteUrl(name: string): string {
    return `${this.baseUrl}api/v1/delete/${name}`;
  }
}
```

Here we can see that our | it slightly different from the other endpoints and this is because the name is in the URL. All the other URLs are defined in the constructor with the code below:

```typescript
// File: ingress/frontend/src/api/url.ts
constructor() {
  this.baseUrl = Url.getBaseUrl();
  this.create = `${this.baseUrl}api/v1/create`;
  this.getAll = `${this.baseUrl}api/v1/get/all`;
  this.update = `${this.baseUrl}api/v1/update`;
}
```

Finally, we must define our base URL. Generally, we want our base URL to be the location of the browser. However, we must remember that we also have a development server for the frontend that runs on the localhost on port 3000. However, our development server is on port 8001. Therefore, if our location is on the development server we need to change the URL to point to the development rust backend server with the following code:

```typescript
// File: ingress/frontend/src/api/url.ts
static getBaseUrl(): string {
  let url = window.location.href;
  if (url.includes("http://localhost:3000/")) {
    return "http://0.0.0.0:8001/";
  }
  return window.location.href;
}
```

We can now build the API utils that handle these interfaces.

The utils file is about abstracting some repetitive code that all other API calls will use so we do not have to repeat ourselves. Before we write our generic API call functions, we need to import the following:

```typescript
// File: ingress/frontend/src/api/utils.ts
import axios, {AxiosResponse} from "axios";
```

With this import, we can build our handle response function which will have the signature below:

```typescript
// File: ingress/frontend/src/api/utils.ts
async function handleRequest<T, X>( promise: Promise<AxiosResponse<X>>, expectedResponse: number) {
  let response: AxiosResponse<X>;
  . . .
}
```

Here we can see that our general understanding of async programming is helping us out. In this handle function, we accept a promise of a generic request. In our Typescript code, we can handle our promises just like we would handle our futures in async Rust code. Inside the function, we await our API call promise, and handle the outcome with the following code:

```typescript
// File: ingress/frontend/src/api/utils.ts
let response: AxiosResponse<X>;
try {
  response = await promise;
} catch (error) {
  return { status: 500, error: 'Network or other error occurred' };
}
```

If the HTTP request was not successful, we can conclude that this was the result of a network error. However, if our request succeeds, then we can assert that the response code is what we expect for the call such as an OK, CREATED, or other code, and return the data as the type we are expecting with the code below:

```typescript
// File: ingress/frontend/src/api/utils.ts
if (response.status === expectedResponse) {
  return { status: response.status, data: response.data as X };
} else {
  return { status: response.status, error: response.data as string };
}
```

Now our handle request function is defined, we can learn on it to define a function that executes a post request. This post request will also have data in the body. Our generic post request function takes the following form:

```typescript
// File: ingress/frontend/src/api/utils.ts
export async function postCall<T, X>( url: string, body: T, expectedResponse: number) {
  let response = axios.post<X | string>( url, body, { headers: { 'Content-Type': 'application/json', 'token': "jwt" }, validateStatus: () => true });
  return handleRequest(response, expectedResponse);
}
```

The validateStstus it to ensures that we resolve the promise for all response statuses. It must be noted that we have the JWT in the header. Right now, this is a placeholder, but we will be reviewing the JWT in Chapter 10, Managing User Sessions. Finally, we need to define our get call function. Now is a good time to try and build this function yourself as it will be a slight variance to the post call function. If you attempted to build the function yourself, it hopefully looks like the following code:

```typescript
// File: ingress/frontend/src/api/utils.ts
export async function getCall<X>( url: string, expectedResponse: number) {
  let response = axios.get<X | string>( url, { headers: { 'Content-Type': 'application/json', 'token': "jwt" }, validateStatus: () => true });
  return handleRequest(response, expectedResponse);
}
```

And with this, our utils for API calls is fully defined, we can use these utils to define our GET API call.

For our GET API call, our function takes the form below:

```typescript
// File: ingress/frontend/src/api/get.ts
import { ToDoItems } from "../interfaces/toDoItems";
import {getCall} from "./utils";
import { Url } from "./url";
export default async function getAll() {
  let response = await getCall<ToDoItems>( new Url().getAll, 200 );
  return response;
}
```

And that is it! We can see how implementing new API calls are going to be easy. However, before we implement more endpoints, we must check to see if our API calls work. To test this, we add our API GET call in our app.

Before we rewrite the App component, we must ensure that we have the following imported:

```typescript
// File: ingress/frontend/src/index.tsx
import React, { useState } from 'react';
import ReactDOM from "react-dom/client";
import getAll from './api/get';
import {ToDoItems} from "./interfaces/toDoItems";
```

We then define the outline below for our App component:

```typescript
// File: ingress/frontend/src/index.tsx
const App = () => {
  const [data, setData] = useState(null);
  React.useEffect(() => {
    . . .
  }, []);
  return (
    <div>
      { data ? <div>Data loaded: {JSON.stringify(data) } </div> : <div>Loading...</div>}
    </div>
  );
};
```

Here we have the useState which handles the state for the App component. Our state is going to be the to-do items. We also render "Loading…" if our state is not loaded for the App component. If we have loaded the data, we render the data instead. Our React.useEffect fires once the App component has been loaded. In the React.useEffect, we make the GET call with the following code:

```typescript
// File: ingress/frontend/src/index.tsx
React.useEffect(() => {
  const fetchData = async () => {
    const response = await getAll<ToDoItems>();
    setData(response.data);
  }
  fetchData();
}, []);
```

We now have everything we need to run our server with a frontend that serves the to-do items. Before we run the server however, we must ensure that there is a tasks.json file in the root of the ingress workspace with the tasks. Once this is done, we run the ingress/scripts/run_server.sh script and visit the URL. This should give us the output seen in figure Figure 6.5.

Figure 6.5 – Our App view rendering to-do items

And there we have it! We can see that our API call is working, and we can access our data from the server.

We now only need to build the create, delete, and update API calls. At this stage this is a good time to try and build these functions yourself as there is a bit of repetition with some slight variance. This is a good time to test and concrete what you know about creating API calls.

If you tried to create these functions yourself, hopefully, you will have taken an outline that is like the following approach. As we have covered all we need to know the approach will merely spell out the code needed.

First in the utils.rs file we create a DELETE function with the following code:

```typescript
// File: ingress/frontend/src/api/utils.ts
export async function deleteCall<X>( url: string, expectedResponse: number) {
  let response = axios.delete<X | string>( url, { headers: { 'Content-Type': 'application/json', 'token': "jwt" }, validateStatus: () => true });
  return handleRequest(response, expectedResponse);
}
```

In the same file, our PUT function is built with the code below:

```typescript
// File: ingress/frontend/src/api/utils.ts
export async function putCall<T, X>( url: string, body: T, expectedResponse: number) {
  let response = axios.put<X | string>( url, body, { headers: { 'Content-Type': 'application/json', 'token': "jwt" }, validateStatus: () => true });
  return handleRequest(response, expectedResponse);
}
```

The create function then takes the following form:

```typescript
// File: ingress/frontend/src/api/create.ts
import { ToDoItem, ToDoItems, TaskStatus } from "../interfaces/toDoItems";
import { postCall } from "./utils";
import { Url } from "./url";
export async function createToDoItemCall(title: string) {
  const toDoItem: ToDoItem = { title: title, status: TaskStatus.PENDING };
  return postCall<ToDoItem, ToDoItems>( new Url().create, toDoItem, 201 );
}
```

The update function if defined with the following code:

```typescript
// File: ingress/frontend/src/api/update.ts
import { ToDoItem, ToDoItems, TaskStatus } from "../interfaces/toDoItems";
import { putCall } from "./utils";
import { Url } from "./url";
export async function updateToDoItemCall( name: string, status: TaskStatus) {
  const toDoItem: ToDoItem = { title: name, status: status };
  return putCall<ToDoItem, ToDoItems>( new Url().update, toDoItem, 200 );
}
```

And finally, our delete function takes the following form:

```typescript
// File: ingress/frontend/src/api/delete.ts
import { ToDoItems } from "../interfaces/toDoItems";
import { deleteCall } from "./utils";
import { Url } from "./url";
export async function deleteToDoItemCall( name: string) {
  return deleteCall<ToDoItems>( new Url().deleteUrl(name), 200 );
}
```

And now we have defined all the API endpoints that we need. However, we need to interact with these API calls. We must create some to-do item and form components so we can call these API functions with data.

## Creating React Components

With React, we can build JSX components that can be used as templates. These components can have an internal state, which can be updated, and the update can force a re-render of the component. We can explore the basic outline of the React component by building the create to-do item component. This component takes an input for the title of the to-do item being created and keeps track of the state of the title. When the button is clicked the create component then makes an API call with the title to then create the to-do item, and then returns the updated state to the parent component.

We can start building our create component with the following imports and interface in our CreateItemForm.tsx file:

```typescript
// File: ingress/frontend/src/components/CreateItemForm.tsx
import React, { useState } from 'react';
import { createToDoItemCall } from "../api/create";
interface CreateToDoItemProps {
  passBackResponse: (response: any) => void;
}
```

The CreateToDoItemProps interface is what we are going to accept into the component when creating the component. Generally, what gets passed into the component when the component is being created is referred to as props. In this case we are passing a function that accepts the response of the create API call. We will see how the function works when we implement our components in the main application.

Now we can define the outline of our create component with the following code:

```typescript
// File: ingress/frontend/src/components/CreateItemForm.tsx
export const CreateToDoItem: React.FC<CreateToDoItemProps> = ( { passBackResponse } ) => {
  const [title, setTitle] = useState<string>("");
  const handleTitleChange = ( e: React.ChangeEvent<HTMLInputElement> ) => {
    setTitle(e.target.value);
  };
  const createItem = async () => {
    . . .
  };
  return (
    . . .
  );
}
```

Here we can see that we initially define our state which is just a string as all we are doing is keeping track of the name of the to-do item that we are creating. We are assuming that all to-do items being created will be pending and not complete. We then define the function that updates the state of the to-do item title every time there is a change in the input HTML element where the user is inputting the title. This means that everytime the user changes the input contents for the title of the do-to item, the entire React component has access to that data, and we can alter the component however we want. We then define our API call and the tsx that the component is rendering.

For the create API call, our function takes the form below:

```typescript
// File: ingress/frontend/src/components/CreateItemForm.tsx
const createItem = async () => {
  await createToDoItemCall(title).then(response => {
    setTitle("");
    if (response.data) {
      passBackResponse(response.data);
    } else if (response.error) {
      console.log(response);
      console.log( `Error ${response.status}: ${response.error}` );
    }
  });
};
```

Here we can see that we pass the title state into the API call, and then reset the state of the title. We then execute the function that was passed in via props if the API call was successful or print out the error if the API call was unsuccessful.

For our render statement, we have the following code:

```typescript
// File: ingress/frontend/src/components/CreateItemForm.tsx
return (
  <div className="inputContainer">
    <input type="text" id="name" placeholder="create to do item" value={title} onChange={handleTitleChange}/>
    <button className="actionButton" id="create-button" onClick={createItem}>Create</button>
  </div>
);
```

Here we render the title state in the input value so the user can see the state of the title. We then bind our listener to that input and bind the API call function to our button. We have referenced some CSS class names in the tsx. Do not worry, even though we have not created them, the frontend will not crash if we do not have the CSS classes. We will define our CSS classes in the next section of this chapter. However, it is easier to reference the CSS classes now when we are building the React components to save us bloating the chapter by going back and referencing the CSS classes in the components later.

Our form is now ready to import and use, but before we add this form into our main application component, we should define our to-do item component in our ToDoItem.tsx file. First, we need the following imports and interface:

```typescript
// File: ingress/frontend/src/components/ToDoItem.tsx
import React, {useEffect, useState} from 'react';
import {updateToDoItemCall} from "../api/update";
import { deleteToDoItemCall } from "../api/delete";
import {TaskStatus} from "../interfaces/toDoItems";
interface ToDoItemProps {
  title: string;
  status: string;
  id: number;
  passBackResponse: (response: any) => void;
}
```

Here we can see that we are passing in the title, status, and ID of the to-do item. This makes sense as we want to render the item, and handle operations on the to-do item in the backend via making API calls in the to-do item component. With this interface in mind, our to-do item component takes the form below:

```typescript
// File: ingress/frontend/src/components/ToDoItem.tsx
export const ToDoItem: React.FC<ToDoItemProps> = ( { title, status, id, passBackResponse }) => {
  const [itemTitle, setTitle] = useState<string>(title);
  const [button, setButton] = useState<string>('');
  useEffect(() => {
    const processStatus = (status: string): string => {
      return status === "PENDING" ? "edit" : "delete";
    };
    setButton(processStatus(status));
  }, [status]);
  const sendRequest = async (): void => {
    . . .
  };
  return (
    . . .
  );
}
```

Here we can see that we are using the useEffect to define what is going to be rendered in the button. If our status is pending, we can edit the to-do item to a complete, so the button has "complete". If the status is not pending, we then assume that the to-do item is complete, and the only thing that we want to do now is delete the to-do item, so the button renders "delete". It must be noted that there is a [status] array in the useEffect function. This array is a dependencies array. This means that the useEffect function relies on the status before executing which makes sense as we reference the status in the useEffect function.

Now that our state is defined, we can move onto the API call for editing the item. Here, must check the type of button, and make either the create or delete API depending on the button type. At this stage it is a good time to try and build this API call yourself. There is some conditional logic that you must consider, and if you get this right then you are truly comfortable making API calls to our backend. Remember to look at the to-do item component for a starting point.

If you attempted to build the API, call yourself, hopefully you have a structure like the one below:

```typescript
// File: ingress/frontend/src/components/ToDoItem.tsx
const sendRequest = async (): void => {
  if (button === "edit") {
    . . .
  } else {
    . . .
  }
};
```

If our button is an edit button, we have the API call below:

```typescript
// File: ingress/frontend/src/components/ToDoItem.tsx
await updateToDoItemCall( itemTitle, TaskStatus.DONE ).then( response => {
  if (response.data) {
    passBackResponse(response.data);
  } else if (response.error) {
    console.log(response);
  }
} )
```

And then our delete button takes the following form:

```typescript
// File: ingress/frontend/src/components/ToDoItem.tsx
await deleteToDoItemCall(itemTitle).then( response => {
  if (response.data) {
    passBackResponse(response.data);
  } else if (response.error) {
    console.log(response);
  }
} )
```

And now our to-do item component will be able to make API calls to the backend by themselves when their button is called!

Finally, we must render the to-do item with the following return statement:

```typescript
// File: ingress/frontend/src/components/ToDoItem.tsx
return (
  <div className="itemContainer" id={id}>
    <p>{itemTitle}</p>
    <button className="actionButton" onClick={sendRequest}>{button}</button>
  </div>
);
```

Here we can see that we merely render the button type and bind our API call to the button on click.

And here we have it, the components are finished, and all we need to do is place them in our application. In our index file for our application, we now have the following imports:

```typescript
// File: ingress/frontend/src/index.tsx
import React, { useState } from 'react';
import ReactDOM from "react-dom/client";
import getAll from './api/get';
import {ToDoItems} from "./interfaces/toDoItems";
import { ToDoItem } from "./components/ToDoItem";
import { CreateToDoItem } from './components/CreateItemForm';
```

Now that we have our new components, we must handle them in our App component. Our App component now has the following outline:

```typescript
// File: ingress/frontend/src/index.tsx
const App = () => {
  const [data, setData] = useState(null);
  function reRenderItems(items: ToDoItems) {
    setData(items);
  }
  React.useEffect(() => {
    const fetchData = async () => {
      const response = await getAll<ToDoItems>();
      setData(response.data);
    }
    fetchData();
  }, []);
  if (!data) {
    return <div>Loading...</div>;
  } else {
    return (
      . . .
    );
  }
};
```

We have now defined a reRenderItems function. This function just takes in the to-do items data, and updates the state, forcing a re-render of the application. The reRenderItems function is the function that we are going to pass to our create items form and to-do item components, so when these components perform an API backend call that is successful, they will pass the returned state of the to-do items back to our reRenderItems function.

For the return statement of our App component, we merely loop through the to-do items and render them, placing the create form at the bottom with the code below:

```typescript
// File: ingress/frontend/src/index.tsx
<div className="App">
  <div className="mainContainer">
    <div className="header">
      <p>complete tasks: {data.done.length}</p>
      <p>pending tasks: {data.pending.length}</p>
    </div>
    <h1>Pending Items</h1>
    <div>
      {data.pending.map((item, index) => (
        <><ToDoItem key={item.title + item.status} title={item.title} status={item.status} id={item.id} passBackResponse={reRenderItems}/></>
      ))}
    </div>
    <h1>Done Items</h1>
    <div>
      {data.done.map((item, index) => (
        <><ToDoItem key={item.title + item.status} title={item.title} status={item.status} id={item.id} passBackResponse={reRenderItems}/></>
      ))}
    </div>
    <CreateToDoItem passBackResponse={reRenderItems} />
  </div>
</div>
```

And now we have everything we need to get the frontend application working with the backend. If we run our server now, we should have something like Figure 6.6.

Figure 6.6 – Our App functionally working.

You will be able to input a title of a new to-do item in the input, and all buttons will work. However, as we can see in Figure 6.6, there is no styling. We will now be moving onto inserting styles with CSS.

## Inserting Styles with CSS

We are going to insert CSS by defining a CSS next to our index.tsx file and declaring it in our HTML file so that the CSS is requested by the frontend and served. We also need to bundle our CSS. First, we import the CSS in our index.tsx file with the following code:

```typescript
// File: ingress/frontend/src/index.tsx
import "./App.css";
```

This ensures that we will also create a bundle.css file in the public directory when creating a frontend build. Now inside our HTML file in the public directory, we have the following:

```html
<!-- File: ingress/frontend/public/index.html -->
<!DOCTYPE html>
<html>
  <head>
    <title>My React App</title>
  </head>
  <body>
    <div id="root"></div>
    <link rel="stylesheet" href="./bundle.css">
    <script type="module" src="./bundle.js"></script>
  </body>
</html>
```

For our ingress HTML file, we now have the contents below:

```html
<!-- File: ingress/frontend/ingress/index.html -->
<!DOCTYPE html>
<html>
  <head>
    <title>My To Do App</title>
  </head>
  <body>
    <div id="root"></div>
    <link rel="stylesheet" href="./frontend/bundle.css">
    <script type="module" src="./frontend/public/bundle.js">
    </script>
  </body>
</html>
```

Now that everything is in order, we can focus on our CSS. We can do a basic configuration of the body which is the App class with the code below:

```css
/* File: ingress/frontend/src/App.css */
.App {
  background-color: #92a8d1;
  font-family: Arial, Helvetica, sans-serif;
  height: 100vh;
}
```

The background color is a reference to a type of color. This reference might not seem like it makes sense just looking at it but there are color pickers online where you can see and pick a color and the reference code is supplied. Some code editors support this functionality but for some quick reference, simply google HTML color picker and you will be spoilt for choice at the number of free online interactive tools that will be available. With the configuration above, the background for the entire page will be of the code #92a8d1, which is a navy-blue color. If we just had that, most of the page would have a white background. The navy-blue background would only be present where there is content. We set the height to 100vh. vh is relative to 1% of the height of the viewport. With this, we can deduce that 100vh means the styling we defined in the body occupies 100% of the viewport. We then define the font for all text unless overwritten to Arial, Helvetica, sans-serif. We can see that we have defined multiple fonts in the font-family. This does not mean that all of them are implemented or that there are different fonts for different levels of headers or HTML tags. Instead, this s a fallback mechanism. First, the browser will try and render Arial; if it is not supported by the browser, it will then try and render Helvetica, and if that fails too it will try and render sans-serif.

We have now defined the general style for our body but what about different screen sizes? For instance, if we were going to access out application on our phone, it should have different dimensions. We can see this in the following figure:

Figure 6.7 – Difference in margins between a phone and desktop monitor

We can see in Figure 6.7 the ratio of the margin to the space that is filled up by the to-do items list changes. With a phone there is not much screen space so most of the screen needs to be taken up by the to-do item; otherwise, we would not be able to read it. However, if we are using a widescreen desktop monitor, we no longer need most of the screen for the to-do items. In-fact, if the ratio was the same, the to-do items would be so stretched in the x-axis that it would be hard to read and frankly would not look good. This is where media queries come in. We can have different style conditions based on attributes like the width and height of the window. We will start with the phone specification. So, if the width of the screen is up to 500 pixels, in our CSS file we will define the following CSS configuration for our body:

```css
/* File: ingress/frontend/src/App.css */
@media(max-width: 500px) {
  .App {
    padding: 1px;
    display: grid;
    grid-template-columns: 1fr;
  }
}
```

Here we can see that the padding around the edge of the page and each element is just one pixel. We also have a grid display. This is where we can define columns and rows. However, we do not use it to its full extent. We just have one column. This means that our to-do items will take up most of the screen like in the phone depiction of Figure 6.7. Even though we are not using a grid in this context, I have kept it in so you can see the relationship this has to the other configurations for larger screens. If our screen gets a little bigger, we then split our page into three different vertical columns; however, the ratio of the width of the middle column to that of the columns on either side is 5:1. This is because our screen still is not very big, and we want our items to still take up most of the screen. We can adjust for this by adding another media query with different parameters:

```css
/* File: ingress/frontend/src/App.css */
@media(min-width: 501px) and (max-width: 550px) {
  .App {
    padding: 1px;
    display: grid;
    grid-template-columns: 1fr 5fr 1fr;
  }
  .mainContainer {
    grid-column-start: 2;
  }
}
```

We can also see that for our mainContainer CSS class where we house our to-do items we will overwrite the attribute grid-column-start. If we did not, then the mainContainer would be squeezed in the left margin at 1fr width. Instead, we are starting and finishing in the middle at 5fr. We can make our mainContainer span across multiple columns with a grid-column-finish attribute.

If our screen gets larger, we then want to adjust the ratios even more as we do not want our items width to get out of control. To achieve this, we then define a 3 to 1 ratio with for the middle column versus the two side columns, and then a 1 to 1 ratio when the screen width gets higher than 1001px:

```css
/* File: ingress/frontend/src/App.css */
@media(min-width: 551px) and (max-width: 1000px) {
  .App {
    padding: 1px;
    display: grid;
    grid-template-columns: 1fr 3fr 1fr;
  }
  .mainContainer {
    grid-column-start: 2;
  }
}
@media(min-width: 1001px) {
  .App {
    padding: 1px;
    display: grid;
    grid-template-columns: 1fr 1fr 1fr;
  }
  .mainContainer {
    grid-column-start: 2;
  }
}
```

Now that we have defined our general CSS for our entire application, we can move onto our item container. Our item has a different background color giving us the following definition:

```css
/* File: ingress/frontend/src/App.css */
.itemContainer {
  background: #034f84;
  display: flex;
  align-items: stretch;
  justify-content: space-between;
  margin: 0.3rem;
}
```

We can see that this class has a margin of 0.3. We are using the rem because we want the margin to scale relatively to the font size of the root element. The align-items ensures that all the children in the container including the buttons stretch to fill the container height. The flex ensures that all items grow and shink relative to each other and can be displayed next to each other. This will come in handy as we want our action button for the to-do item to sit next to the title. We also want our item to slightly change the color if our cursor hovers over it:

```css
/* File: ingress/frontend/src/App.css */
.itemContainer:hover {
  background: #034f99;
}
```

Inside an item container, the title of our item is denoted with a paragraph tag. We want to define the style of all the paragraphs in the item containers but not elsewhere. We define the style of the paragraphs in the container by the following:

```css
/* File: ingress/frontend/src/App.css */
.itemContainer p {
  color: white;
  display: inline-block;
  margin: 0.5rem;
  margin-right: 0.4rem;
  margin-left: 0.4rem;
}
```

The inline-block allows the title to be displayed alongside the div which will be the acting as the button for the item. The margin definitions merely stop the title from being right up against the edge of the item container. We also ensure that the paragraph color is white.

With our item title styled, the only item styling left is the action button, which is either edit or delete. This action button is going to float to the right with a different background color so we can know where to click. To do this, we define our button style with a class which is outlined in the following code:

```css
/* File: ingress/frontend/src/App.css */
.actionButton {
  display: inline-block;
  float: right;
  background: #f7786b;
  border: none;
  padding: 0.5rem;
  padding-left: 2rem;
  padding-right: 2rem;
  color: white;
  align-self: stretch;
}
```

Here, we've defined the display, we make it float to the right, and define the background color and padding. With this, we can ensure the color changes on hover by running the following code:

```css
/* File: ingress/frontend/src/App.css */
.actionButton:hover {
  background: #f7686b;
  color: black;
}
```

Now that we have covered all the concepts, we only must define the styles for the input container. This can be done by running the following code:

```css
/* File: ingress/frontend/src/App.css */
.inputContainer {
  background: #034f84;
  display: flex;
  align-items: stretch;
  justify-content: space-between;
  margin: 0.3rem;
  margin-top: 2rem;
}
.inputContainer input {
  display: inline-block;
  margin: 0.4rem;
  border: 2px solid transparent;
  background: #ffffff;
  color: #034f84;
}
```

While this defines the styling about our input, we want the user to know that they have selected the input so they can type. We can change the CSS of the input when the input is clicked using focus as seen below:

```css
/* File: ingress/frontend/src/App.css */
.inputContainer input:focus {
  outline: none;
  border-color: #f7786b;
  box-shadow: 0 0 5px #f7786b;
}
```

And finally, we define the CSS for the header with the following code:

```css
/* File: ingress/frontend/src/App.css */
.header {
  background: #034f84;
  margin-bottom: 0.3rem;
}
.header p {
  color: white;
  display: inline-block;
  margin: 0.5rem;
  margin-right: 0.4rem;
  margin-left: 0.4rem;
}
```

And this is it, our system is now ready to run. If we run our server, we should get the same view as seen in Figure 6.8.

Figure 6.8 – Our finished Application

And here we have it, our application is working. Our application can be served by our Rust binary.

## Summary

In this chapter, we have finally enabled our application to be used by a casual user as opposed to having to rely on a third-party application like Postman. We defined our own app views module that housed read file and insert functions. This resulted in us building a process that loaded an HTML file, inserted data from a JavaScript file and CSS file into the view data, and then serve that data.

This gave us a dynamic view that automatically updated when we edited, deleted, or created a to-do item. We also explored some basics around CSS and JavaScript to make API calls from the frontend to the backend to get the data needed to render the state of to-do items.

We also really explored how frontend assets are served which enabled us to embed our frontend into the Rust binary to be served by our Rust server. However, we are not going to stop there, in the next chapter, we are going to serve WASM into the frontend.

## Questions

How is the React application and its CSS served to the frontend?

How can we handle data produced in a child component?

How do you ensure that the background color and style standard of certain elements is consistent across all views of the app?

What do we use the useEffect for in a React component?

When will a React component re-render?

## Answers

The React app is bundled into two files, a bundle.js and bundle.css. We then serve a HTML file that references the CSS and JS file. These files are then requested from the server and loaded into the browser. The JS file is the React application that gets a HTML tag in the HTML file via ID, renders the outcome, and this outcome is inserted into the HTML tag in the served HTML file.

The parent component passes a function to the child component. The child component can then pass data into that function and execute the function. Depending on what the function does, this means that the parent component can be altered.

We can create a CSS class and define the background color and other styling types we want to be universal such as font type and color. In our App component we return a div with that CSS class. Inside that div we put all our components for that application, and these components will inherit the CSS class for the initial div.

The useEffect executes when the React component has been loaded. We can also have dependencies such as data.

A React component re-renders when the data in the useState changes.
