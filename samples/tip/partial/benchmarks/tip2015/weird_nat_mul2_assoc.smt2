f_add3acc f_z f_y2 f_z2 = (case f_z of {
                             C_Z -> (case f_y2 of {
                                       C_Z -> f_z2;
                                       C_S f_y22 -> (f_add3acc (C_Z) f_y22 (C_S f_z2)); });
                             C_S f_x2 -> (f_add3acc f_x2 (C_S f_y2) f_z2); });
f_mul2 f_z f_y2 = (case f_z of {
                     C_Z -> (C_Z);
                     C_S f_z2 -> (case f_y2 of {
                                    C_Z -> (C_Z);
                                    C_S f_x2 -> (C_S (f_add3acc f_z2 f_x2 (f_mul2 f_z2 f_x2))); }); });
prove: (forall f_z f_y2 f_z2 . ((f_mul2 f_z (f_mul2 f_y2 f_z2)) = (f_mul2 (f_mul2 f_z f_y2) f_z2)));
