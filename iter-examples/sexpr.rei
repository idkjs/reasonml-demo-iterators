/*
 Zipperposition: a functional superposition prover for prototyping
 Copyright (C) 2012 Simon Cruanes

 This is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 This is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 02110-1301 USA.
 */

/* {1 Basic S-expressions, with printing and parsing} */

/** S-expression */

type t =
  | /** An atom */
    Atom(string)
  | /** A list of S-expressions */
    List(list(t));

/** Token that compose a Sexpr once serialized */

type token = [ | `Open | `Close | `Atom(string)];

/** {2 Traverse an iterator of tokens} */;

/** Iterate on the S-expression, calling the callback with tokens */

let iter: (token => unit, t) => unit;

/** Traverse. This yields an iterator of tokens */

let traverse: t => Iter.t(token);

/** Returns the same iterator of tokens, but during iteration, if
      the structure of the Sexpr corresponding to the iterator
      is wrong (bad parenthesing), Invalid_argument is raised
      and iteration is stoped */

let validate: Iter.t(token) => Iter.t(token);

/** {2 Text <-> tokens} */;

/** Lex: create an iterator of tokens from the given iterator of chars. */

let lex: Iter.t(char) => Iter.t(token);

/** Build a Sexpr from an iterator of tokens, or raise Failure */

let of_seq: Iter.t(token) => t;

/** {2 Printing} */;

/** Print a token on the given formatter */

let pp_token: (Format.formatter, token) => unit;

/** Print an iterator of Sexpr tokens on the given formatter */

let pp_tokens: (Format.formatter, Iter.t(token)) => unit;

/** Pretty-print the S-expr. If [indent] is true, the S-expression
      is printed with indentation. */

let pp_sexpr: (~indent: bool=?, Format.formatter, t) => unit;

/** {2 Serializing} */;

/** print a pair "(name @,iterator)" */

let output_seq: (string, Iter.t(token), token => unit) => unit;

/** print a pair "(name str)" */

let output_str: (string, string, token => unit) => unit;

/** {2 Parsing} */;

/** Monadic combinators for parsing data from an iterator of tokens,
    without converting to concrete S-expressions. */;

type parser('a);

exception ParseFailure(string);

/** Monadic bind: computes a parser from the result of
      the first parser */

let (>>=): (parser('a), 'a => parser('b)) => parser('b);

/** Like (>>=), but ignores the result of the first parser */

let (>>): (parser('a), parser('b)) => parser('b);

/** Parser that consumes no input and return the given value */

let return: 'a => parser('a);

/** Fails parsing with the given message */

let fail: string => parser('a);

/** consumes one token with the function */

let one: (token => 'a) => parser('a);

/** Skip the token */

let skip: parser(unit);

/** choose parser given current token */

let lookahead: (token => parser('a)) => parser('a);

/** Parses a `Open */

let left: parser(unit);

/** Parses a `Close */

let right: parser(unit);

let pair: (parser('a), parser('b)) => parser(('a, 'b));
let triple: (parser('a), parser('b), parser('c)) => parser(('a, 'b, 'c));

/** [(name,p) ^|| p'] behaves as [p ()] if the next token is [`Atom name], and
      like [p'] otherwise */

let (^||): ((string, unit => parser('a)), parser('a)) => parser('a);

/** Maps the value returned by the parser */

let map: (parser('a), 'a => 'b) => parser('b);

let p_str: parser(string);
let p_int: parser(int);
let p_bool: parser(bool);

let many: parser('a) => parser(list('a));
let many1: parser('a) => parser(list('a));

/** Parses exactly one value from the iterator of tokens. Raises
      ParseFailure if anything goes wrong. */

let parse: (parser('a), Iter.t(token)) => 'a;

/** Parses an iterator of values */

let parse_seq: (parser('a), Iter.t(token)) => Iter.t('a);
