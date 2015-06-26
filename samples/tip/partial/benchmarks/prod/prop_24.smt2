f_plus f_z f_y2 = (case f_z of {
                     C_Z -> f_y2;
                     C_S f_z2 -> (C_S (f_plus f_z2 f_y2)); });
f_even f_z = (case f_z of {
                C_Z -> (C_True);
                C_S f_y2 -> (case f_y2 of {
                               C_Z -> (C_False);
                               C_S f_z2 -> (f_even f_z2); }); });
prove: (forall f_z f_y2 . ((f_even (f_plus f_z f_y2)) = (f_even (f_plus f_y2 f_z))));
