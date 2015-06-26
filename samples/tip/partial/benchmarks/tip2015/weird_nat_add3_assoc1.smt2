f_add3 f_z f_y2 f_z2 = (case f_z of {
                          C_Z -> (case f_y2 of {
                                    C_Z -> f_z2;
                                    C_S f_y22 -> (C_S (f_add3 (C_Z) f_y22 f_z2)); });
                          C_S f_x2 -> (C_S (f_add3 f_x2 f_y2 f_z2)); });
prove: (forall f_x1 f_x2 f_x3 f_x4 f_x5 . ((f_add3 (f_add3 f_x1 f_x2 f_x3) f_x4 f_x5) = (f_add3 f_x1 f_x2 (f_add3 f_x3 f_x4 f_x5))));
