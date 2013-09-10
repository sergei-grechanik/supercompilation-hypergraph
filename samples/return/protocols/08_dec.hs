data Number = Z | S Number;
data Action = RM1 | RM2 | RM3A | RM3B | WH2 | WH3 | WH4 | WM;
-- Invalid Dirty Shared Exclusive
data State = State Number Number Number Number;
data Boolean = True | False;
data List a = Nil | Cons a (List a);

(chState1 (loop1 (State (S i) Z Z Z) (f s)))
--(loop2 (State (S i) Z Z Z) (f s))
where
loop2 =\state actions -> case state of {
	State i m s o -> case (chState1 state) of {
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
		State i d s e -> loop1 (State i d s e) as;
	};
};

react = \state action -> case state of {State i d s e ->
	case action of {
		RM1 -> rm1 i d s e;
		RM2 -> rm2 i d s e;
		RM3A -> rm3a i d s e;
		RM3B -> rm3b i d s e;
		WH2 -> wh2 i d s e;
		WH3 -> wh3 i d s e;
		WM -> wm i d s e;
	};
};

rm1 = \i d s e -> case i of {
	S i1 -> case d of {
		Z -> case s of {
			Z -> case e of {
				Z -> State i1 Z Z (S Z);
			};
		};
	};	
};
rm2 = \i d s e -> case i of {
	S i1 -> case d of {S d1 -> State i1 d1 (S (S s)) e;};};
	
rm3a = \i d s e -> case i of {
	S i1 -> case s of {
		S s1 -> State i1 d (S (S (add s1 e))) Z;
	};
};
rm3b = \i d s e -> case i of {
	S i1 -> case e of {
		S e1 -> State i1 d (S (S (add s e1))) Z;
	};
};
wh2 = \i d s e -> case e of {
	S e1 -> State i (S d) s e1; 
};
wh3 = \i d s e -> case s of {
	S s1 -> case s1 of {Z -> State i d Z (S e);}; 
};
wm = \i d s e -> case i of {
	S i1 -> State (add i1 (add d (add s e))) (S Z) Z Z;
};

chState1 = \state -> case state of {State i d s e -> check1 i d s e;};
chState2 = \state -> case state of {State i d s e -> check2 i d s e;};
chState3 = \state -> case state of {State i d s e -> check3 i d s e;};
chState4 = \state -> case state of {State i d s e -> check4 i d s e;};
check1 =\i d s e -> case d of {S d1 -> case s of {S s1->False;Z->True;};Z->True;};
check2 =\i d s e -> case d of {S d1 -> case e of {S e1->False;Z->True;};Z->True;};
check3 =\i d s e -> case d of {S d1 -> case d1 of {S d2->False;Z->True;};Z->True;};
check4 =\i d s e -> case e of {S e1 -> case e1 of {S e2->False;Z->True;};Z->True;};
add = \x y -> case x of { Z -> y; S x1 -> S (add x1 y);};
--add = \x y -> case x of { Z -> y; S x1 -> add x1 (S y);};
