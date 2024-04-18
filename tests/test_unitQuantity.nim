import units
import test_unitSystem

unitQuantity: 
    Velocity = Length / Time
    Frequency = Time^-1

let 
    a = 1.0.meters
    b = 2.0.seconds
    c = a / b

assert 1 / b is Frequency
assert b^-1 is Frequency
assert c is Velocity

let 
    d = 3.0 * a
    f = a + d

assert f is Length

echo a
echo d
echo f