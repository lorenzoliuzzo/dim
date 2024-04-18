import macros
export macros

from strformat import fmt
export fmt

import strutils


proc getCompact*(node: NimNode): NimNode =
    result = node
    if result.kind == nnkStmtList and result.len == 1:
        result = result[0]

template getAstCompact*(call: untyped): untyped = getCompact(getAst(call))


proc isCallOne*(node: NimNode): bool =
    ## Check whenever the node is a call with single argument.
    node.kind == nnkCall and node.len == 2

proc isIdentCallOne*(node: NimNode): bool =
    ## Check whenever the node is an ident call with single argument.
    node.isCallOne and node[0].kind == nnkIdent

proc isDotPair*(node: NimNode): bool =
    ## Check whenever the node is two-arguments dot expression.
    node.kind == nnkDotExpr and node.len == 2

proc isIdentDotPair*(node: NimNode): bool =
    ## Check whenever the node is two-arguments dot expression of idents.
    node.isDotPair and node[0].kind == nnkIdent and node[1].kind == nnkIdent

proc isAsgn*(node: NimNode): bool =
    ## Check whenever the node is two-arguments asgn.
    node.kind == nnkAsgn and node.len == 2

proc isAsgnToIdent*(node: NimNode): bool =
    ## Check whenever the node is two-arguments asgn to ident.
    node.isAsgn and node[0].kind == nnkIdent


proc callToPar*(node: NimNode): NimNode =
    ## Converts call to its arguments in par.
    result = newNimNode(nnkPar)
    node.copyChildrenTo(result)
    result.del(0)


proc prettyRepr*(s: string): string {.inline.} = s

proc prettyRepr*(node: NimNode): string {.inline.} =
    ## Stringify more as-seen, not as-parsed.
    if node.isCallOne: fmt"{node[0].repr}: {node[1].repr}" else: node.repr

proc errorTrace*(src: NimNode, fmt: string, args: varargs[string, prettyRepr]) =
    ## Nice and handy error shouter.
    var s = @[src.prettyRepr]
    for arg in args: s.add arg
    error(fmt % s, src)


proc formVariants(forms: varargs[string]): string =
    ## Connects many forms to a single string.
    result = ""
    for form in forms:
        result &= form & "' or '"
    result.delete(result.len-6..<result.len)


proc callName*(node: NimNode): NimNode =
    ## Get calling name of any call. Assumes node IS call.
    result = node[0]

proc callArg*(node: NimNode, i: int): NimNode =
    ## Get a call's argument of given number. Assumes node IS call.
    result = node[i+1]

proc callOneArg*(node: NimNode): NimNode =
    ## Get argument of CallOne. Assumes node IS CallOne.
    result = node.callArg(0)

proc asgnL*(node: NimNode): NimNode =
    ## Get assign's lhs. Assumes node IS assign.
    result = node[0]

proc asgnR*(node: NimNode): NimNode =
    ## Get assign's rhs. Assumes node IS assign.
    result = node[1]

proc dotL*(node: NimNode): NimNode =
    ## Get dot's lhs. Assumes node IS dot.
    result = node[0]

proc dotR*(node: NimNode): NimNode =
    ## Get dot's rhs. Assumes node IS dot.
    result = node[1]



const 
    notValidAs* = "invalid $2: '$1'"
    notValidAsIn* = "invalid $2, '$3' expected, but found: '$1'"
    identExpectedAs* = "identifier expected as $2 but '$1' found"
    expectedAsIn* = "$2 in form '$3' expected, but '$1' found"

proc isNotValidAs*(node: NimNode, what: string) =
    ## Errors unconditionally due to node being invalid.
    node.errorTrace(notValidAs, what)

proc isNotValidAsIn*(node: NimNode, what: string, forms: varargs[string]) =
    ## Errors unconditionally due to node being invalid.
    ## Form-variant.
    node.errorTrace(notValidAsIn, what, formVariants(forms))

proc expectIdentAs*(node: NimNode, what: string): NimNode {.discardable} =
    ## Check whenever the node is ident. Return the node for chaining.
    if node.kind != nnkIdent:
        node.errorTrace(identExpectedAs, what)
    node

proc expectCallOneAsIn*(node: NimNode, what: string, forms: varargs[string]): NimNode {.discardable} =
    ## Check whenever the node is call ident. Return the node for chaining.
    if not node.isCallOne:
        node.errorTrace(expectedAsIn, what, formVariants(forms))
    node
    
proc expectAsgnAsIn*(node: NimNode, what: string, forms: varargs[string]): NimNode {.discardable} =
    ## Check whenever the node is call ident. Return the node for chaining.
    if not node.isAsgn:
        node.errorTrace(expectedAsIn, what, formVariants(forms))
    node