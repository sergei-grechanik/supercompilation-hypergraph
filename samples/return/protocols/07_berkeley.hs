data Number = Z | S Number;
data Action = RM | WM | WH1A | WH1B;
-- Invalid Non-exclusive Exclusive Unowned
data State = State Number Number Number Number;
data Boolean = True | False;
data List a = Nil | Cons a (List a);

(chState1 (loop1 (State (S i) Z Z Z) (f s)))

where

loop1 = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> case (react state a) of {
		State i n e u -> loop1 (State i n e u) as;
	};
};

react = \state action -> case state of {State i n e u ->
	case action of {
		RM -> rm i n e u;
		WM -> wm i n e u;
		WH1A -> wh1a i n e u;
		WH1B -> wh1b i n e u;
	};
};

rm = \i n e u -> case i of {
	S i1 -> State i1 (add e n) Z (S u);	
};
wm = \i n e u -> case i of {
	S i1 -> State (add i1 (add n (add e u))) Z (S Z) Z;	
};
wh1a = \i n e u -> case n of {
	S n1 -> State (add i (add n1 u)) Z (S e) Z;	
};
wh1b = \i n e u -> case u of {
	S u1 -> State (add i (add n u1)) Z (S e) Z;	
};

chState1 = \state -> case state of {State i n e u -> check1 i n e u;};
chState2 = \state -> case state of {State i n e u -> check2 i n e u;};
chState3 = \state -> case state of {State i n e u -> check3 i n e u;};
check1 =\i n e u -> case e of {S e1 -> case u of {S u1->False;Z->True;};Z->True;};
check2 =\i n e u -> case n of {S n1 -> case e of {S e1->False;Z->True;};Z->True;};
check3 =\i n e u -> case e of {S e1 -> case e1 of {S e2->False;Z->True;};Z->True;};
add = \x y -> case x of { Z -> y; S x1 -> S (add x1 y);};
