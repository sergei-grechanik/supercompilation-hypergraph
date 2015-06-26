f_plus f_z f_y2 = (case f_z of {
                     C_Z -> f_y2;
                     C_S f_n -> (C_S (f_plus f_n f_y2)); });
f_alt_mul f_z f_y2 = (case f_z of {
                        C_Z -> (C_Z);
                        C_S f_z2 -> (case f_y2 of {
                                       C_Z -> (C_Z);
                                       C_S f_x2 -> (C_S (f_plus (f_plus (f_alt_mul f_z2 f_x2) f_z2) f_x2)); }); });
prove: (forall f_z f_y2 f_z2 . ((f_alt_mul f_z (f_alt_mul f_y2 f_z2)) = (f_alt_mul (f_alt_mul f_z f_y2) f_z2)));
