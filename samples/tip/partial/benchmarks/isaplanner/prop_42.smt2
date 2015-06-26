f_take f_z f_y2 = (case f_z of {
                     C_Z -> (C_nil);
                     C_S f_z2 -> (case f_y2 of {
                                    C_nil -> (C_nil);
                                    C_cons f_x2 f_x3 -> (C_cons f_x2 (f_take f_z2 f_x3)); }); });
prove: (forall f_n f_z f_xs . ((f_take (C_S f_n) (C_cons f_z f_xs)) = (C_cons f_z (f_take f_n f_xs))));
