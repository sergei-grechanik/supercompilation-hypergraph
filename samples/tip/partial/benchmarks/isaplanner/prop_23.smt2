f_max2 f_z f_y2 = (case f_z of {
                     C_Z -> f_y2;
                     C_S f_z2 -> (case f_y2 of {
                                    C_Z -> f_z;
                                    C_S f_x2 -> (C_S (f_max2 f_z2 f_x2)); }); });
prove: (forall f_a f_b . ((f_max2 f_a f_b) = (f_max2 f_b f_a)));
