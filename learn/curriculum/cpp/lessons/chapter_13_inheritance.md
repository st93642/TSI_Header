# Chapter 13: Inheritance

## Learning goals

- Build inheritance hierarchies that favour clear is-a relationships.
- Control access with `public`, `protected`, and `private` inheritance.
- Initialise base classes correctly and avoid pitfalls like slicing.

## Preparation checklist

- Create `~/tsi_cpp/ch13_inheritance`.
- Map out a simple hierarchy such as `Person -> Student/Professor` before coding.
- Review constructors, destructors, and virtual base classes in your notes.

## 13.1 Designing the hierarchy

Define a base class `Person` with a virtual destructor and core attributes. Derive `Student` and `Professor`, adding specialised methods. Document the is-a relationship in comments and in your offline journal to confirm the design makes sense.

## 13.2 Access control

Experiment with `protected` members for shared implementation details. Provide functions showing how derived classes interact with inherited protected data, and explain why private members remain inaccessible to derived types.

## 13.3 Constructor chains

Demonstrate base-class constructor invocation using member initialiser lists. Add logging statements that show the order of construction and destruction so you can compare the output with your expectations.

## 13.4 Multiple inheritance considerations

Implement a diamond hierarchy with virtual inheritance to avoid duplicated base subobjects. Keep the example minimal but capture console output proving that base constructors run only once.

## 13.5 Lab: campus role directory

1. Model `CampusRole` with derived classes `Faculty`, `Staff`, and `StudentAssistant`.
2. Provide virtual functions `describe_role()` returning strings.
3. Use a container (`std::vector<std::unique_ptr<CampusRole>>`) to store mixed roles and invoke virtual methods.
4. Demonstrate safe downcasts using `dynamic_cast` when appropriate.

## 13.6 Self-check prompts

- When should inheritance be preferred over composition?
- Why is a virtual destructor essential in polymorphic base classes?
- How does virtual inheritance solve the diamond problem?

## 13.7 Troubleshooting

| Symptom | Likely cause | Remedy |
| --- | --- | --- |
| Slicing behaviour | Copying a derived object into a base by value | Store polymorphic objects by pointer or reference. |
| Duplicate base constructor calls | Missing `virtual` inheritance | Declare intermediate bases with `virtual`. |
| Inaccessible base members | Wrong inheritance specifier | Use `public` inheritance when modelling an is-a relationship. |

## Wrap-up

- [ ] You built a class hierarchy with meaningful behaviour.
- [ ] You traced constructor/destructor order through logging.
- [ ] You practised multiple inheritance with virtual bases.

Chapter 14 builds on these classes with polymorphism—override behaviour cleanly to unlock dynamic dispatch.
