from system import unitSystem, unitQuantity, unitPrefix, unitAbbr, unitAlias
from math import `^`

unitSystem si:
    Length:             meters(m)
    Mass:               kilograms(kg)
    Time:               seconds(s)
    ElectricCurrent:    amperes(A)
    Temperature:        kelvins(K)
    Amount:             moles(mol)
    LuminousIntensity:  candelas(cd)


unitQuantity:
    Area = Length^2
    Volume = Length * Area
    Density = Mass / Volume
    Frequency: hertz(Hz) = Time^-1

    Angle:       radians(rad)   = Length / Length
    SolidAngle:  steradians(sr) = Area / Area

    Velocity = Length / Time
    AngularVelocity = Angle / Time

    Momentum = Mass * Velocity
    AngularMomentum = Momentum * Length

    Acceleration = Velocity / Time
    AngularAcceleration = AngularVelocity / Time

    Force:     newtons(N)  = Mass * Acceleration
    Pressure:  pascals(Pa) = Force / Area
    Energy:    joules(J)   = Force * Length
    Power:     watts(W)    = Energy / Time

    Impulse = Momentum * Time
    Torque = Force * Length
    Action = Energy * Time

    ElectricCharge: coulombs(C)  = ElectricCurrent * Time
    Voltage:        volts(V)     = Power / ElectricCurrent
    Capacitance:    farads(F)    = ElectricCharge / Voltage
    Impedance:      ohms(Ω)    = Voltage / ElectricCurrent
    # Conductance:    siemenses(S) = Impedance^-1

    MagneticFlux:        webers(Wb) = Voltage * Time
    MagneticFluxDensity: tesla(T)   = MagneticFlux / Area
    Inductance:          henrys(H)  = MagneticFlux / ElectricCurrent

    AbsorbedDose:    grays(Gy)   = Energy / Mass
    EquivalentDose:  sievert(Sv) = Energy / Mass

    CatalyticActivity:  katal(kat) = Amount / Time


unitPrefix:
    atto:   0.1 ^ 18
    femto:  0.1 ^ 15
    pico:   0.1 ^ 12
    nano:   0.1 ^  9
    micro:  0.1 ^  6
    mili:   0.1 ^  3
    centi:  0.1 ^  2
    deci:   0.1 ^  1
    deca:  10.0 ^  1
    hekto: 10.0 ^  2
    kilo:  10.0 ^  3
    mega:  10.0 ^  6
    giga:  10.0 ^  9
    tera:  10.0 ^ 12
    peta:  10.0 ^ 15
    exa:   10.0 ^ 18

unitAbbr:
    ps = pico.seconds
    ns = nano.seconds
    us = micro.seconds
    ms = mili.seconds

    nm = nano.meters
    um = micro.meters
    mm = mili.meters
    dm = deci.meters
    cm = centi.meters
    km = kilo.meters

unitAlias:
    x.minutes(min) = (x * 60).seconds
    x.hours(h)     = (x * 60).minutes
    x.days(d)      = (x * 24).hours
    x.years(y)     = (x * 365.2425).days