import ControlledVisit

ControlledVisit.traverse (\info -> Data.List.sortBy (\x y -> y `compare` x) info) ""

ControlledVisit.traverse (\info -> Data.List.sortBy (\x y -> let filtered = filter (\x -> (infoPath x) != "") info in searchable (Data.Maybe.fromJust (infoPerms x)) `compare` searchable (Data.Maybe.fromJust (infoPerms y))) filtered) ""