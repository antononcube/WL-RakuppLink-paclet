# RakuppLink

Wolfram Language (WL) paclet with functions for interacting with [Rakupp (Raku++)](https://github.com/ash/rakupp).

----

## Setup

To install this paclet in your Wolfram Language environment, evaluate this code:

```wl
PacletInstall[ResourceObject["https://wolfr.am/1GQZM8FKq"]]
```
To load the code after installation, evaluate this code:

```wl
Needs["AntonAntonov`RakuppLink`"]
```

Setup the `RAKUPP_LIB` environmental variable:

```wl
SetEnvironment["RAKUPP_LIB" -> $HomeDirectory <> "/GitHub/ash/rakupp/build/librakupp.dylib"]
```

----

## Usage examples

### Basic evaluation

Evaluate a simple arithmetic expression with Rakupp:

```wl
RakuppEval["2_000 + 232"]

(* 2232 *)
```

### Grammar definitions and parsing

Here is a string with a Raku-language defined grammar:

```wl
rkCode = "
grammar Parser {
    rule  TOP  { <who> <verb> <lang> }
    token who  {:i I | you | we | they }
    token verb { <love> | <hate> }
    token love { '❤️' | '♥' | love }
	token hate { '🤮' | hate }
    token lang {:i < Raku Rust Go Python Ruby TypeScript PHP WL> }
}
";
```

Compile the grammar in Rakupp interpreter:

```wl
g = RakuppGrammarFromSource[rkCode]

(* RakuppGrammar[0, "<anonymous>"] *)
```

Parse a sentence:

```wl
m = RakuppParse[g, "We 🤮 python"]

(* RakuppMatch[OpaqueRawPointer[46019347968]] *)
```

Show the tree corresponding to the obtained match object:

```wl
RakuppTree[m]

(* <|"lang" -> "python", "verb" -> <|"hate" -> "🤮"|>, "who" -> "We"|> *)
```

Free the interpreter:

```wl
RakuppShutdown[]
```
