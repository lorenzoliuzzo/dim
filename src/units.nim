import 
    units/[utils, unitInfo]

from sequtils import mapIt

macro unitSystem*(name, impl: untyped) =
    if name.kind != nnkIdent: error("expected system name", name)

    let info = newSystemInfo(name, impl.mapIt(it.getUnitInfo))