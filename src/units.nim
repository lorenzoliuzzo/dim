import 
    units/[utils, unitInfo, prefix]

from sequtils import mapIt


macro unitSystem*(name, impl: untyped) =
    if name.kind != nnkIdent: error("expected system name", name)

    let info = newSystemInfo(name, impl.mapIt(it.getUnitInfo))

    result = newStmtList()
    result.add info.typeDefinition

    for i, _ in info.units:
        result.add info.quantityDefinition(i)