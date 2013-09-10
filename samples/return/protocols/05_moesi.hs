data Number = Z | S Number;
data Action = RM | WH2 | WH3A | WH3B | WM;
data State = State Number Number Number Number Number;
data Boolean = True | False;
data List a = Nil | Cons a (List a);
data Result = Result Boolean Boolean Boolean Boolean Boolean Boolean Boolean;


Result 
(chState1a (loop1 (State (S i) Z Z Z Z) (f s)))
(chState1b (loop1 (State (S i) Z Z Z Z) (f s)))
(chState1c (loop1 (State (S i) Z Z Z Z) (f s)))
(chState2a (loop1 (State (S i) Z Z Z Z) (f s)))
(chState2b (loop1 (State (S i) Z Z Z Z) (f s)))
(chState3  (loop1 (State (S i) Z Z Z Z) (f s)))
(chState4  (loop1 (State (S i) Z Z Z Z) (f s))) where

loop1 = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> case (react state a) of {
		State invalid exclusive shared modified owned -> loop1 (State invalid exclusive shared modified owned) as;
	};
};

react = \state action -> case state of {State invalid exclusive shared modified owned ->
	case action of {
		RM -> rm invalid exclusive shared modified owned;
		WH2 -> wh2 invalid exclusive shared modified owned;
		WH3A -> wh3a invalid exclusive shared modified owned;
		WH3B -> wh3b invalid exclusive shared modified owned;
		WM -> wm invalid exclusive shared modified owned;
	};
};

rm = \i e s m o -> case i of {
	S i1 -> State i1 Z (add (S e) s) Z (add o m);
};
wh2 =\i e s m o -> case e of {
	S e1 -> State i e1 s (S m) o;
};
wh3a =\i e s m o -> case s of {
	S s1 -> State (add i (add e (add s1 (add m o)))) (S Z) Z Z Z; 
};
wh3b =\i e s m o -> case o of {
	S o1 -> State (add i (add e (add s (add m o1)))) (S Z) Z Z Z; 
};
wm = \i e s m o -> case i of {
	S i1 -> State (add i1 (add e (add s (add m o)))) (S Z) Z Z Z;
};


chState1a = \state -> case state of {State invalid exclusive shared modified owned ->
	check1a invalid exclusive shared modified owned;
};
chState1b = \state -> case state of {State invalid exclusive shared modified owned ->
	check1b invalid exclusive shared modified owned;
};
chState1c = \state -> case state of {State invalid exclusive shared modified owned ->
	check1c invalid exclusive shared modified owned;
};
chState2a = \state -> case state of {State invalid exclusive shared modified owned ->
	check2a invalid exclusive shared modified owned;
};
chState2b = \state -> case state of {State invalid exclusive shared modified owned ->
	check2b invalid exclusive shared modified owned;
};
chState3 = \state -> case state of {State invalid exclusive shared modified owned ->
	check3 invalid exclusive shared modified owned;
};
chState4 = \state -> case state of {State invalid exclusive shared modified owned ->
	check4 invalid exclusive shared modified owned;
};
check1a = \i e s m o -> case m of {S m1 -> case e of {S e1 -> False;};};
check1b = \i e s m o -> case m of {S m1 -> case s of {S s1 -> False;};};
check1c = \i e s m o -> case m of {S m1 -> case o of {S o1 -> False;};};
check2a = \i e s m o -> case e of {S e1 -> case s of {S s1 -> False;};};
check2b = \i e s m o -> case e of {S e1 -> case o of {S o1 -> False;};};
check3 = \i e s m o -> case m of {S m1 -> case m1 of {S m2 -> False;};};
check4 = \i e s m o -> case e of {S e1 -> case e1 of {S e2 -> False;};};

add = \x y -> case x of { Z -> y; S x1 -> S (add x1 y);};