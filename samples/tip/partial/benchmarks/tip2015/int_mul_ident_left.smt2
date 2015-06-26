f_toInteger f_z f_y2 = (case f_z of {
                          C_Pos -> (C_P f_y2);
                          C_Neg -> (case f_y2 of {
                                      C_Zero -> (C_P (C_Zero));
                                      C_Succ f_m -> (C_N f_m); }); });
f_sign f_z = (case f_z of {
                C_P f_y2 -> (C_Pos);
                C_N f_z2 -> (C_Neg); });
f_plus f_z f_y2 = (case f_z of {
                     C_Zero -> f_y2;
                     C_Succ f_n -> (C_Succ (f_plus f_n f_y2)); });
f_opposite f_z = (case f_z of {
                    C_Pos -> (C_Neg);
                    C_Neg -> (C_Pos); });
f_timesSign f_z f_y2 = (case f_z of {
                          C_Pos -> f_y2;
                          C_Neg -> (f_opposite f_y2); });
f_one = (C_P (C_Succ (C_Zero)));
f_mult f_z f_y2 = (case f_z of {
                     C_Zero -> (C_Zero);
                     C_Succ f_n -> (f_plus f_y2 (f_mult f_n f_y2)); });
f_absVal f_z = (case f_z of {
                  C_P f_n -> f_n;
                  C_N f_m -> (C_Succ f_m); });
f_times f_z
        f_y2 = (f_toInteger (f_timesSign (f_sign f_z) (f_sign f_y2)) (f_mult (f_absVal f_z) (f_absVal f_y2)));
prove: (forall f_z . (f_z = (f_times (f_one) f_z)));
