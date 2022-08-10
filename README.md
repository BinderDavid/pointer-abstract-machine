# Pointer Abstract Machine
[![CI](https://github.com/BinderDavid/pointer-abstract-machine/actions/workflows/ci.yml/badge.svg)](https://github.com/BinderDavid/pointer-abstract-machine/actions/workflows/ci.yml)

The pointer abstract machine was first developed by Danos and Regnier. This is a very simple implementation of the variant described in section 9 of the paper "The Duality of Computation" by Pierre-Louis Curien and Hugo Herbelin.

## How to use

Use `stack` in order to build the project:

```
stack build
```

Use `stack run` in order to execute one of the examples defined in `app/Main.hs`:

```
stack run ex2
```

## Stepping through an example

Press `Enter` to step through the program:

```
stack run ex3
Computing command < μ α.< λx.x | ~μ y.< y | α > > | ~μ x.< x | □ > >

Initial State:
┌───────────────────────────────────────────────────────────────────────────────
│ Term: μ α.< λx.x | ~μ y.< y | α > >{-1}
│ Cont: ~μ x.< x | □ >{-1}
├───────────────────────────────────────────────────────────────────────────────
│ Stack:
│
└───────────────────────────────────────────────────────────────────────────────


Step 1: Evaluated a cut between a mu abstraction and a continuation.
┌───────────────────────────────────────────────────────────────────────────────
│ Term: λx.x{0}
│ Cont: ~μ y.< y | α >{0}
├───────────────────────────────────────────────────────────────────────────────
│ Stack:
│ •  • 0 ⟼ α : ~μ x.< x | □ >{-1}
└───────────────────────────────────────────────────────────────────────────────


Step 2: Evaluated a cut between a term and a tilde mu abstraction.
┌───────────────────────────────────────────────────────────────────────────────
│ Term: y{1}
│ Cont: α{1}
├───────────────────────────────────────────────────────────────────────────────
│ Stack:
│ •  • 1 ⟼ y : λx.x{0}
│      0 ⟼ α : ~μ x.< x | □ >{-1}
└───────────────────────────────────────────────────────────────────────────────


Step 3: Evaluated a term variable by looking up the value in the stack.
┌───────────────────────────────────────────────────────────────────────────────
│ Term: λx.x{0}
│ Cont: α{1}
├───────────────────────────────────────────────────────────────────────────────
│ Stack:
│    • 1 ⟼ y : λx.x{0}
│ •    0 ⟼ α : ~μ x.< x | □ >{-1}
└───────────────────────────────────────────────────────────────────────────────


Step 4: Evaluated a continuation variable by looking up the value in the stack.
┌───────────────────────────────────────────────────────────────────────────────
│ Term: λx.x{0}
│ Cont: ~μ x.< x | □ >{-1}
├───────────────────────────────────────────────────────────────────────────────
│ Stack:
│      1 ⟼ y : λx.x{0}
│ •    0 ⟼ α : ~μ x.< x | □ >{-1}
└───────────────────────────────────────────────────────────────────────────────


Step 5: Garbage collection: Popped the top of the stack.
┌───────────────────────────────────────────────────────────────────────────────
│ Term: λx.x{0}
│ Cont: ~μ x.< x | □ >{-1}
├───────────────────────────────────────────────────────────────────────────────
│ Stack:
│ •    0 ⟼ α : ~μ x.< x | □ >{-1}
└───────────────────────────────────────────────────────────────────────────────


Step 6: Evaluated a cut between a term and a tilde mu abstraction.
┌───────────────────────────────────────────────────────────────────────────────
│ Term: x{1}
│ Cont: □{1}
├───────────────────────────────────────────────────────────────────────────────
│ Stack:
│ •  • 1 ⟼ x : λx.x{0}
│      0 ⟼ α : ~μ x.< x | □ >{-1}
└───────────────────────────────────────────────────────────────────────────────


Step 7: Evaluated a term variable by looking up the value in the stack.
┌───────────────────────────────────────────────────────────────────────────────
│ Term: λx.x{0}
│ Cont: □{1}
├───────────────────────────────────────────────────────────────────────────────
│ Stack:
│    • 1 ⟼ x : λx.x{0}
│ •    0 ⟼ α : ~μ x.< x | □ >{-1}
└───────────────────────────────────────────────────────────────────────────────


Computation finished.
```