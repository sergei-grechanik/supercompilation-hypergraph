data Number = Z | S Number;
data Action = RM | W0 | WI | WS | SE | WBM | WB0;
data State = State Number Number Number Number;
data Boolean = True | False;
data List a = Nil | Cons a (List a);
data Result = Result Boolean Boolean Boolean;

check (loop (State (S i) Z Z Z) acts) where
	
loop = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> case (react state a) of {
		State i m s o -> loop (State i m s o) as;
	};
};
react = \state action -> case state of {State invalid modified shared owned ->
	case action of {
		RM -> rm invalid modified shared owned;
		W0 -> w0 invalid modified shared owned;
		WI -> wi invalid modified shared owned;
		WS -> ws invalid modified shared owned;
		SE -> se invalid modified shared owned;
		WBM -> wbm invalid modified shared owned;
		WB0 -> wb0 invalid modified shared owned;
	};
};
rm = \i m s o ->
	case i of {
		S i1 -> State i1 Z (S s) (add m o);
	};
w0 = \i m s o ->
	case o of {
		S o1 -> State (add i (add m (add s o1))) (S Z) Z Z;
	};
wi = \i m s o ->
	case i of {
		S i1 -> State (add i1 (add m (add s o))) (S Z) Z Z;
	};
ws = \i m s o ->
	case s of {
		S s1 -> State (add i (add m (add s1 o))) (S Z) Z Z;
	};
se = \i m s o ->
	case s of {
		S s1 -> State (S i) m s1 o;
	};
wbm = \i m s o ->
	case m of {
		S m1 -> State (S i) m1 s o;
	};
wb0 = \i m s o ->
	case o of {
		S o1 -> State (S i) m s o1;
	};
checkAll = \state -> case state of {State invalid modified shared owned ->
	Result
		(check1 invalid modified shared owned)
		(check2 invalid modified shared owned)
		(check3 invalid modified shared owned);
};
chAll = \state -> Result (ch1 state) (ch2 state) (ch3 state);
check = \state -> case state of {State invalid modified shared owned ->
	check1 invalid modified shared owned;
};

ch1 = \state -> case state of {State i m s o -> check1 i m s o;};
ch2 = \state -> case state of {State i m s o -> check2 i m s o;};
ch3 = \state -> case state of {State i m s o -> check3 i m s o;};

check1 = \i m s o -> case o of {S o1 -> case o1 of {S o2 -> False;Z -> True;};Z -> True;};
check2 = \i m s o -> case m of {S m1 -> case m1 of { S m2 -> False;Z -> True;};Z -> True;};
check3 = \i m s o -> case m of {S m1 -> case s of {S s1 -> False;Z -> True;};Z->True;};
add = \x y -> case x of { Z -> y; S x1 -> S (add x1 y);};