data Number = Z | S Number;
data Action = RM | WH2 | WH3 | WM;
data State = State Number Number Number Number;
data Boolean = True | False;
data List a = Nil | Cons a (List a);
data Result = Result Boolean Boolean;

Result
(chState1 (loop1 (State (S i) Z Z Z) (f s))) 
(chState2 (loop1 (State (S i) Z Z Z) (f s))) where

loop2 =\state actions -> case state of {
	State i m s o -> case (chState2 state) of {
		False -> False;
		True -> case actions of {
			Nil -> True;
			Cons a as -> --loop2 (react state a) as;
				case react state a of {
					State i1 m1 s1 o1 -> loop2 (State i1 m1 s1 o1) as;
				};
		};
	};
};

loop1 = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> case (react state a) of {
		State invalid exclusive shared modified -> loop1 (State invalid exclusive shared modified) as;
	};
};
react = \state action -> case state of {State invalid exclusive shared modified ->
	case action of {
		RM -> rm invalid exclusive shared modified;
		WH2 -> wh2 invalid exclusive shared modified;
		WH3 -> wh3 invalid exclusive shared modified;
		WM -> wm invalid exclusive shared modified;
	};
};

rm = \i e s m -> case i of {
	S i1 -> State i1 Z (add (S e) (add s m)) Z;
};
wh2 =\i e s m -> case e of {
	S e1 -> State i e1 s (S m);
};
wh3 = \i e s m -> case s of {
	S s1 -> State (add i (add e (add s1 m))) (S Z) Z Z;
};
wm = \i e s m -> case i of {
	S i1 -> State (add i1 (add e (add s m))) (S Z) Z Z;
};

check = \state -> case state of {State invalid exclusive shared modified ->
	check2 invalid exclusive shared modified;
};
chState1 = \state -> case state of {State invalid exclusive shared modified ->
	check1 invalid exclusive shared modified;
};
chState2 = \state -> case state of {State invalid exclusive shared modified ->
	check2 invalid exclusive shared modified;
};
-- invalid exclusive shared modified
check1 = \i e s m -> case s of {S s1 -> case m of {S m1 -> False; Z -> True;};Z -> True;};
check2 = \i e s m -> case m of {S m1 -> case m1 of { S m2 -> False; Z -> True;};Z -> True;};

add = \x y -> case x of { Z -> y; S x1 -> S (add x1 y);};