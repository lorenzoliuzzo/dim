import macros
export macros

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


proc callToPar*(node: NimNode): NimNode =
    ## Converts call to its arguments in par.
    result = newNimNode(nnkPar)
    node.copyChildrenTo(result)
    result.del(0)