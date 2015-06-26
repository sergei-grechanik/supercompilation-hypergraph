f_zero = (C_P (C_Zero));
f_plus2 f_z f_y2 = (case f_z of {
                      C_Zero -> f_y2;
                      C_Succ f_n -> (C_Succ (f_plus2 f_n f_y2)); });
f_minus f_z f_y2 = (case f_z of {
                      C_Zero -> (case f_y2 of {
                                   C_Zero -> (C_P (C_Zero));
                                   C_Succ f_n -> (C_N f_n); });
                      C_Succ f_m -> (case f_y2 of {
                                       C_Zero -> (C_P f_z);
                                       C_Succ f_o -> (f_minus f_m f_o); }); });
f_plus f_z f_y2 = (case f_z of {
                     C_P f_m -> (case f_y2 of {
                                   C_P f_n -> (C_P (f_plus2 f_m f_n));
                                   C_N f_o -> (f_minus f_m (C_Succ f_o)); });
                     C_N f_m2 -> (case f_y2 of {
                                    C_P f_n2 -> (f_minus f_n2 (C_Succ f_m2));
                                    C_N f_n3 -> (C_N (C_Succ (f_plus2 f_m2 f_n3))); }); });
prove: (forall f_z . (f_z = (f_plus f_z (f_zero))));
