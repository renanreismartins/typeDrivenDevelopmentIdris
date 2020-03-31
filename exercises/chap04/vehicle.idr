import data.Vect

data PowerSource = Petrol | Pedal | Electricity

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle : Vehicle Pedal
  Tram : (fuel : Nat) -> Vehicle Electricity
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels Motorcycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Tram fuel) = 6

refuel : Vehicle power -> Vehicle power
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Tram fuel) = Tram 600


vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs
--vectTake (S k) (x :: xs) = x :: vectTake (pred k) xs
--vectTake (S k) (x :: xs) = let r = vectTake (pred k) xs in
--                               x :: r


sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                Just idx => Just ((Vect.index idx xs) + (Vect.index idx ys) )
