f_drop f_z f_y2 = (case f_z of {
                     C_Z -> f_y2;
                     C_S f_z2 -> (case f_y2 of {
                                    C_nil -> (C_nil);
                                    C_cons f_x2 f_x3 -> (f_drop f_z2 f_x3); }); });
prove: (forall f_xs . ((f_drop (C_Z) f_xs) = f_xs));
