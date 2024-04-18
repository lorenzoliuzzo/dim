import 
    units/[utils, unitInfo, prefix, ops]

from sequtils import mapIt


macro unitSystem*(name, impl: untyped) =
    if name.kind != nnkIdent: error("expected system name", name)

    let info = newSystemInfo(name, impl.mapIt(it.getUnitInfo))

    result = newStmtList()
    result.add info.typeDefinition
    result.add info.outerOpsDefinition

    for i, _ in info.units:
        result.add info.quantityDefinition(i)


macro unitQuantity*(code: untyped) =
    result = newStmtList()

    for quantity in code:
        var qname, uname, aname, definition: NimNode

        # no unit:
        if quantity.isAsgnToIdent:
            qname = quantity.asgnL
            definition = quantity.asgnR

        # with unit:
        elif quantity.isIdentCallOne:
            qname = quantity.callName
            let afterName = quantity.callOneArg
            afterName.expectAsgnAsIn("quantity implementation", fmt"{qname} = impl", fmt"{qname}: unit(abbr) = impl")

            let unames = afterName.asgnL
            definition = afterName.asgnR

            unames.expectCallOneAsIn "quantity unit abbreviation", "unit(abbr)"
            uname = unames.callName.expectIdentAs "quantity unit name"
            aname = unames.callOneArg.expectIdentAs "quantity unit abbreviated name"

        else: quantity.isNotValidAs "quantity declaration"

        template genQuantity(qname, def) =
            type qname* = def

        result.add getAstCompact(genQuantity(qname, definition))
        
        if uname != nil:
            template genUnit(qname, uname, x) =
                proc uname*(x: float): qname {.inline.} = x.qname

            result.add getAstCompact(genUnit(qname, uname, ident"x"))

            template genAbbr(qname, aname, x) =
                const aname* = 1.0.qname

            result.add getAstCompact(genAbbr(qname, aname, ident"x"))    