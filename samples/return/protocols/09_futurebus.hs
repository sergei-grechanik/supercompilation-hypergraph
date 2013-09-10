data N = Z | S N;
data Action = R1 | R2 | R3 | R4 | R5 | R6 | WM1 | WM2 | WM3 | WH2 | WH3;
-- invalid sharedU exclusiveU exclusiveM pendingR pendingW pendingEMR pendingEMW pendingSU 
data State = State N N N N N N N N N;
data Boolean = True | False;
data List a = Nil | Cons a (List a);

(chState1 (loop (State (S (S Z)) Z Z Z Z Z Z Z Z) acts)) where

loop = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> case (react state a) of {
		State i sU eU eM pR pW pEMR pEMW pSU  -> loop (State i sU eU eM pR pW pEMR pEMW pSU) as;
	};
};
					
react = \state action -> case state of {State i sU eU eM pR pW pEMR pEMW pSU ->
	case action of {
		R2 -> r2 i sU eU eM pR pW pEMR pEMW pSU;
		R3 -> r3 i sU eU eM pR pW pEMR pEMW pSU;
		R4 -> r4 i sU eU eM pR pW pEMR pEMW pSU;
		R5 -> r5 i sU eU eM pR pW pEMR pEMW pSU;
		R6 -> r6 i sU eU eM pR pW pEMR pEMW pSU;
		WM1 -> wm1 i sU eU eM pR pW pEMR pEMW pSU;
		WM2 -> wm2 i sU eU eM pR pW pEMR pEMW pSU;
		WM3 -> wm3 i sU eU eM pR pW pEMR pEMW pSU;
		WH2 -> wh2 i sU eU eM pR pW pEMR pEMW pSU;
		WH3 -> wh3 i sU eU eM pR pW pEMR pEMW pSU;
	};
};
r2 = \i sU eU eM pR pW pEMR pEMW pSU -> 
	case pW of {Z -> case i of {S i1 ->  
	State i1 Z Z Z (S pR) Z (add pEMR eM) pEMW (add pSU (add eU sU))
;};};
r3 = \i sU eU eM pR pW pEMR pEMW pSU -> 
	case pEMR of {S pEMR1 -> 
	State i (S (add sU pR)) eU eM Z pW pEMR1 pEMW pSU;
};
r4 = \i sU eU eM pR pW pEMR pEMW pSU -> 
	case pSU of {S pSU1 -> 
	State i (S (add sU (add pR pSU1))) eU eM Z pW pEMR pEMW Z;
};
r5 = \i sU eU eM pR pW pEMR pEMW pSU -> 
	case pSU of { Z -> case pR of {S pR1 -> case pR1 of { S pR2 -> case pEMR of {Z -> 
	State i (add sU pR) eU eM Z pW Z pEMW Z;
};};};};
r6 = \i sU eU eM pR pW pEMR pEMW pSU -> 
	case pSU of { Z -> case pR of {S pR1 -> case pR1 of {Z ->  case pEMR of { Z ->     
	State i sU (S eU) eM Z pW Z pEMW Z;
};};};};
wm1 = \i sU eU eM pR pW pEMR pEMW pSU -> 
	case pW of { Z -> case i of {S i1 ->      
	State (add i1 (add sU (add eU (add pR (add pEMR pSU))))) Z Z Z Z Z Z (add eM pEMW) Z;
};};
wm2 = \i sU eU eM pR pW pEMR pEMW pSU -> 
	case pEMW of {S pEMW1 ->     
	State (S i) sU eU (add eM pW) pR Z pEMR pEMW1 pSU;
};
wm3 = \i sU eU eM pR pW pEMR pEMW pSU -> 
	case pEMW of {Z ->     
	State i sU eU (add eM pW) pR Z pEMR Z pSU;
};
wh2 = \i sU eU eM pR pW pEMR pEMW pSU -> 
	case eU of {S eU1 ->     
	State i sU eU1 (S eM) pR pW pEMR pEMW pSU;
};
wh3 = \i sU eU eM pR pW pEMR pEMW pSU -> 
	case sU of {S sU1 ->     
	State (add i sU1) Z eU (S eM) pR pW pEMR pEMW pSU;
};
-- sU >= 1, eU >= 1
chState1 = \state -> case state of {State i sU eU eM pR pW pEMR pEMW pSU -> 
	case sU of {S sU1 -> case eU of {S eU1 -> False;};};};
-- sU >= 1, eM >= 1
chState2 = \state -> case state of {State i sU eU eM pR pW pEMR pEMW pSU -> 
	case sU of {S sU1 -> case eM of {S eM1 -> False;};};};
-- eU >= 1, eM >= 1
chState3 = \state -> case state of {State i sU eU eM pR pW pEMR pEMW pSU -> 
	case eU of {S eU1 -> case eM of {S eM1 -> False;};};};
-- eU >= 2	
chState4 = \state -> case state of {State i sU eU eM pR pW pEMR pEMW pSU -> 
	case eU of {S eU1 -> case eU1 of {S eU2 -> False;};};};
-- eM >=2	
chState5 = \state -> case state of {State i sU eU eM pR pW pEMR pEMW pSU -> 
	case eM of {S eM1 -> case eM1 of {S eM2 -> False;};};};	
-- pR >= 1, pW > = 1
chState6 = \state -> case state of {State i sU eU eM pR pW pEMR pEMW pSU -> 
	case pR of {S pR1 -> case pW of {S pW1 -> False;};};};
-- pW >= 2
chState7 = \state -> case state of {State i sU eU eM pR pW pEMR pEMW pSU -> 
	case pW of {S pW1 -> case pW1 of {S pW2 -> False;};};};	
add = \x y -> case x of { Z -> y; S x1 -> S (add x1 y);};