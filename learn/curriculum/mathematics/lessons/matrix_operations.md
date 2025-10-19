# Matrix Operations

## Introduction

Matrices are rectangular arrays of numbers that can represent various mathematical objects and operations. Understanding matrix operations is fundamental to linear algebra and has applications in many fields including computer graphics, physics, and data analysis.

## Basic Matrix Operations

### Matrix Addition

Two matrices can be added if they have the same dimensions. Addition is performed element-wise:

```
A + B = [a₁₁ + b₁₁  a₁₂ + b₁₂]
        [a₂₁ + b₂₁  a₂₂ + b₂₂]
```

**Example:**

```
A = [2  4]    B = [1  3]    A + B = [3  7]
    [3  1]        [4  2]            [7  3]
```

### Scalar Multiplication

A matrix can be multiplied by a scalar (real number) by multiplying each element by that scalar:

```
k × A = [k × a₁₁  k × a₁₂]
        [k × a₂₁  k × a₂₂]
```

**Example:**

```
3 × A = [6  12]  where A = [2  4]
        [9  3]           [3  1]
```

## Matrix Multiplication

Matrix multiplication is more complex and requires specific dimension compatibility. For matrices A (m×n) and B (n×p), the product AB exists and is an (m×p) matrix.

The element in row i, column j of AB is the dot product of row i of A and column j of B.

### Properties of Matrix Operations

1. **Commutativity of Addition:** A + B = B + A
2. **Associativity:** (A + B) + C = A + (B + C)
3. **Identity Element:** A + 0 = A (where 0 is the zero matrix)
4. **Scalar Multiplication:** k(A + B) = kA + kB

## Applications

Matrix operations are used in:

- Computer graphics transformations
- Solving systems of linear equations
- Data compression and analysis
- Quantum mechanics
- Network analysis

## Practice Exercise

Try the matrix operations exercise to reinforce these concepts. Remember to check matrix dimensions before performing operations!
