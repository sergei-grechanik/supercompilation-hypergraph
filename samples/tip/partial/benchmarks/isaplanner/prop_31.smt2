f_min2 f_z f_y2 = (case f_z of {
                     C_Z -> (C_Z);
                     C_S f_z2 -> (case f_y2 of {
                                    C_Z -> (C_Z);
                                    C_S f_y1 -> (C_S (f_min2 f_z2 f_y1)); }); });
prove: (forall f_a f_b f_c . ((f_min2 (f_min2 f_a f_b) f_c) = (f_min2 f_a (f_min2 f_b f_c))));
