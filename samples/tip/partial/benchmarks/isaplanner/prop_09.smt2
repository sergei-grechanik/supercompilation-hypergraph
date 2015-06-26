f_plus f_z f_y2 = (case f_z of {
                     C_Z -> f_y2;
                     C_S f_z2 -> (C_S (f_plus f_z2 f_y2)); });
f_minus f_z f_y2 = (case f_z of {
                      C_Z -> (C_Z);
                      C_S f_z2 -> (case f_y2 of {
                                     C_Z -> f_z;
                                     C_S f_x2 -> (f_minus f_z2 f_x2); }); });
prove: (forall f_i f_j f_k . ((f_minus (f_minus f_i f_j) f_k) = (f_minus f_i (f_plus f_j f_k))));
