# Chapter 17: Move Semantics

## Learning goals

- Distinguish between lvalues/rvalues and apply the correct reference types.
- Implement move constructors and move assignment operators safely.
- Leverage `std::move` and `std::forward` without violating invariants.

## Preparation checklist

- Create `~/tsi_cpp/ch17_move_semantics`.
- Revisit the class templates from Chapter 16—they are prime candidates for move support.
- Summarise the Rule of Five/Zero in your notes for quick reference.

## 17.1 Lvalues, rvalues, and references

Write `value_categories.cpp` printing addresses and references to illustrate when expressions qualify as lvalues or rvalues. Use helper functions returning by value and by reference to see the differences.

## 17.2 Implementing move members

Enhance a custom container with a move constructor and move assignment operator. Ensure you leave the moved-from object in a valid state and log transitions with console output.

## 17.3 `std::move` vs `std::forward`

Demonstrate a forwarding function template `make_twice` that uses perfect forwarding. Document when `std::forward` preserves value categories versus when `std::move` forces rvalue treatment.

## 17.4 Exception guarantees and `noexcept`

Mark move constructors `noexcept` when they cannot throw, and observe how this affects standard containers (for example, `std::vector` reallocation). Record the behaviour in your offline journal.

## 17.5 Lab: move-aware buffer

1. Create `move_buffer.h`/`.cpp` implementing a simple dynamic buffer with Rule-of-Five members.
2. Provide logging in each special member function to confirm call order.
3. Test moving into `std::vector<MoveBuffer>` to watch reallocation rely on `noexcept`.
4. Run with sanitizers or valgrind (if available offline) to verify there are no leaks or double deletes.

## 17.6 Self-check prompts

- Why do we still define copy operations when adding move operations?
- How does the Rule of Zero simplify move semantics?
- When is it inappropriate to `std::move` a variable?

## 17.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Double free crash | Forgot to null out moved-from resources | Reset pointers in move constructors. |
| Move constructor not called | Type lacks move operations or they’re deleted | Define or `= default` move members. |
| `std::vector` performs copies instead of moves | Move constructor not marked `noexcept` | Add `noexcept` to move operations. |

## Wrap-up

- [ ] You implemented move constructors and assignments for a custom type.
- [ ] You traced value categories with helper functions.
- [ ] You documented how `noexcept` affects container behaviour.

Chapter 18 explores first-class functions, lambdas, and callable wrappers—topics that combine nicely with move-enabled functors.
