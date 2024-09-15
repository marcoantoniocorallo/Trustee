![](https://github.com/marcoantoniocorallo/Trustee/blob/main/cover_1.png)

---
# Trustee

Trustee is the extension of a simple, statically typed, functional language for supporting security primitives. 
These functionalities include
- Block of trusted code, with different qualified level of confidentiality;
- Plugin, that is the possibility to include untrusted functions;
- Static Information-Flow Analysis to prevent data leakage;
- Dynamic Taint Analysis to keep trace of the tainted value;
- Assertion primitives for testing both properties and taintness.

This project has been presented for the exam of *Language-based technology for Security* course @ [*UniPi*](https://di.unipi.it/), and it's an extension of [*Fhree*](https://github.com/marcoantoniocorallo/Fhree).

Trustee is strongly inspired by Ocaml, Java and Rust, is strongly and statically typed and uses an enriched type system that prevents and avoids in advance security errors and data leakages.

---

#### Syntax

Unlike in OCaml, there are no free variables. So there is no *let* construct but only *let-in*.

```ocaml
let x = 5 in x
```

For (also recursive) functions there is a construct *let-fun* similar to the OCaml's *let-rec*.

```ocaml
let fun fact n = 
    if n = 0 then 1
    else n * fact (n - 1)
in fact 5
```

In order to have a *strong type system*, functions declaration make use of mandatory *type annotations*.

```ocaml
let fun fact ( n : int ) : int = 
    if n = 0 then 1
    else n * fact (n - 1)
in fact 5
```

Functions can be used also without declaration, this allows to have both lambdas and recursive lambdas:

```ocaml
5 |> (
    fun fact ( n : int ) : int = 
        if n = 0 then 1
        else n * fact (n - 1)
)
```

```ocaml
"anon-fun" |> lambda (s : string) : string -> " with annotation"
```

Type annotations are available also in every other construct, but optionally.

---
#### Data Types

Fhree provides the most common data types: *integers*, *floats*, *chars*, *booleans* and *strings.* 

Furthermore, it provides **homogeneous** *lists* of values and **heterogeneous** *tuples*.

| Type    | Literal examples                   | Operators                   | Meaning                                                                                       |
|:-------:| ---------------------------------- | --------------------------- | --------------------------------------------------------------------------------------------- |
| int     | `-5`, `0`, `42`                    | `+` `-` `*` `/` `%`         | Arithmetic operations on ints                                                                 |
| float   | `0.15`, `.0002`,`0.1e-22`,         | `+.` `-.` `*.` `/.`         | Arithmetic operations on floats                                                               |
| string  | `"Hello World"`                    | `^`                         | Concatenation of strings                                                                      |
| boolean | `true`, `false`                    | `&&` `\|\|`                 |                                                                                               |
| tuple   | `('a', 0, "hi!")`,`(0,1)`          | `proj t i`                  | Projection of the *i*-th element of *t*                                                       |
| list    | `[2, 4, 6, 8]`, `[]`, `["Hello!"]` | `hd l` <br/>`tl`<br/>`e::l`<br/>`is_empty l` | Get the first element of *l*<br/>Get *l* without the first element<br/>Add *e* in head of *l*<br/>Tests if *l* is empty |

---
#### Comments

There are both *C/Java*-like single-line comments and *OCaml*-like multi-line nested comments

`// this is a comment`

```
(* also
    (* this *) 
is a comment *)
```

---
#### I/O
There are IO directives for each data type. The format for the type `T` is `get_T`/`print_T`.

IO functions are still expressions, and thus evaluation returns a value. In particular, `print` functions are evaluated to the special value `Unit`.

```ocaml
let fun fact(n : int) : int = 
  let _ = print_int n in    // sequencing
  if n = 0 then 
    1
  else
    n * fact (n - 1)
in get_int () |> fact
```

---
#### Trusted Blocks
The idea behind trusted blocks is of blocks of code that group together trusted code and data. They can be used to store confidential data and
operations on them, as well as to mark a snippet of code as trusted, maybe because already verified.
```ocaml
let trust pwd = {
    let secret pass = "abcd" i n // confidential data
    let fun checkpwd ( guess : string ) : bool = declassify ( pass = guess ) in
    handle : { checkpwd } // "public" function i.e. callable from the external environment
} in pwd.checkpwd "try this pass"
```

A trusted block can contain only definitions and a non-empty, mandatory, handle block that specifies which functions can be accessed from
the external environment. A trusted block is treated as an environment binding names to informations, that are:
- data type, qualifier and confidentiality level in phase of type-checking;
- value and integrity level during the evaluation.
Indeed, the type-checker ensures, among other things, that non-public (i.e. not handled) fields are not accessed and it is the responsible for the information-flow analysis that prevents data leakage.
All of these informations are then _erased_ before the evaluation, in which only the integrity informations are retained.

---
Plugins represent the opposite idea of the trusted blocks: they are snippets of untrusted code, provided by third parties.
Plugins are implemented as *(**un**)Trusted blocks*: they share the same syntax with tbs and they are pretty similar:
```ocaml
let plugin filter = {
    let fun string f ( predicate : string -> bool ) ( l : string list ) : string list =
        if l = [] then []
        else
            if predicate ( hd l ) then ( hd l ) : : ( string f predicate ( tl l ) )
            else ( string f predicate ( tl l ) )
    in handle : { string f }
} in filter
```
A plugin can be included by means of the following syntax:`<"filename">`, with which the definition of a plugin is statically parsed and loaded from `plugin/filename`.
If filename is not in the plugin directory, or doesn’t contain a plugin definition, then an exception is raised, in accordance with the principle of *least privilege*.

Trusted blocks and plugins are first-class citizen in Trustee, you can find more information about the type system in chapter 5 of the [report](https://github.com/marcoantoniocorallo/Trustee/blob/main/report.pdf)

---
#### Requirements and usage

Trustee has been developed using the OCaml ecosystem, including:
- `ocamllex` as lexer generator;
- `Menhir` as parser generator;
- `Dune` and `Makefile` as build system;
- `ppx_deriving`, `ppx_test` and `ppx_expect` for preprocessing and testing.
You can install the dependencies by `make setup` and then build the interpreter by `make`.
The interpreter is `Trustee`.
```
Usage: Trustee <filename>
```
