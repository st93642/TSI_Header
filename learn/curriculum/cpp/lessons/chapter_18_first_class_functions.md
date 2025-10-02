# Chapter 18: First-Class Functions

## Learning goals

- Store and invoke function pointers, functors, and lambdas interchangeably.
- Capture state inside lambda expressions safely.
- Use `std::function` as a polymorphic wrapper when flexibility is required.

## Preparation checklist

- Create `~/tsi_cpp/ch18_first_class_functions`.
- Revisit callback patterns in earlier chapters to identify refactoring candidates.
- Prepare simple timing utilities to compare callable types offline.

## 18.1 Function pointers

Implement `function_pointers.cpp` with free functions and pointers to them. Show how to assign, call, and pass them to higher-order functions.

## 18.2 Functors

Create struct-based functors (objects with `operator()`) to encapsulate state. Compare them to function pointers by injecting both into the same processing pipeline.

## 18.3 Lambdas and captures

Write `lambda_capture.cpp` demonstrating value vs reference capture, default captures, and mutable lambdas. Explain how capture lists map to the generated closure objects.

## 18.4 `std::function` and type erasure

Store different callables in `std::function` and observe performance overhead by counting allocations (use a custom allocator or logging). Record the trade-offs in your notes.

## 18.5 Lab: notification hub

1. Build `notification_hub.cpp` that registers callbacks for events (e.g., student enrolled, lab booked).
2. Accept callbacks as `std::function<void(const Event&)>` so callers can supply function pointers, functors, or lambdas.
3. Demonstrate removing callbacks by ID and invoking them in response to simulated events.
4. Ensure captures remain valid by carefully managing lifetimes; log warnings when references dangle.

## 18.6 Self-check prompts

- When should you prefer a functor over a lambda?
- What captures are needed to mutate state inside a lambda?
- How does `std::function` enable polymorphic callables?

## 18.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Callback crashes | Captured reference outlives the lambda | Capture by value or manage lifetime differently. |
| `std::function` throws `bad_function_call` | Wrapper empty | Check `if (callable)` before invoking. |
| Unexpected copy of functor | Missing `std::move` in registration | Move callables into storage when possible. |

## Wrap-up

- [ ] You implemented callbacks with function pointers, functors, and lambdas.
- [ ] You experimented with lambda capture lists.
- [ ] You used `std::function` to store heterogeneous callables.

Chapter 19 concludes with Standard Library containers and algorithms—ideal destinations for callable objects you built here.
