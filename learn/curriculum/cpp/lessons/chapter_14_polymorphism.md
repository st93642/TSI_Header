# Chapter 14: Polymorphism

## Learning goals

- Implement virtual functions and observe dynamic dispatch.
- Prevent slicing and manage polymorphic lifetimes with smart pointers.
- Use runtime type information (`dynamic_cast`, `typeid`) safely.

## Preparation checklist

- Continue with the campus role hierarchy in `~/tsi_cpp/ch14_polymorphism`.
- Review how vtables and pure virtual functions work using your earlier notes.
- Capture reminders about polymorphic destruction so you avoid leaks.

## 14.1 Virtual functions in action

Add virtual `describe()` methods to your hierarchy and override them in derived classes. Print output through base-class pointers to prove runtime dispatch.

## 14.2 Abstract base classes

Create a pure virtual function `calculate_workload()` making `CampusRole` abstract. Implement specific logic in each derived class, similar to how geometric shapes might compute area.

## 14.3 Polymorphic collections

Store objects in `std::vector<std::unique_ptr<CampusRole>>`. Iterate and call virtual functions while logging results. Demonstrate why owning pointers are required by attempting to store values and observing slicing.

## 14.4 RTTI helpers

Use `dynamic_cast` to convert base pointers back to derived types for specialised behaviour (for example, `Faculty` receives sabbatical info). Guard casts and handle `nullptr` results responsibly.

## 14.5 Lab: role scheduler

1. Extend the hierarchy with virtual `schedule_day()`.
2. Implement `Scheduler` that accepts base-class references and invokes `schedule_day()` for a timeline.
3. Add logging showing which derived type executed, and use `typeid` to print the runtime type name.
4. Validate behaviour by iterating over your polymorphic container.

## 14.6 Self-check prompts

- Why must base classes intended for polymorphic use have virtual destructors?
- When should you prefer overriding to overloading?
- How does `dynamic_cast` differ from `static_cast`?

## 14.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Virtual call fails | Function not marked `virtual` in base | Add the `virtual` keyword and override in derived classes. |
| `dynamic_cast` returns `nullptr` | Object not of target type | Check the runtime type before casting. |
| Memory leak when deleting through base pointer | Base destructor not virtual | Declare a virtual destructor in the base class. |

## Wrap-up

- [ ] You used polymorphic containers with smart pointers.
- [ ] You implemented pure virtual functions.
- [ ] You practised RTTI for specialised behaviour.

Chapter 15 addresses runtime errors and exceptions—essential for robust polymorphic systems.
