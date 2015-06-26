f_le f_z f_y2 = (case f_z of {
                   C_Z -> (C_True);
                   C_S f_z2 -> (case f_y2 of {
                                  C_Z -> (C_False);
                                  C_S f_x2 -> (f_le f_z2 f_x2); }); });
f_equal f_z f_y2 = (case f_z of {
                      C_Z -> (case f_y2 of {
                                C_Z -> (C_True);
                                C_S f_z2 -> (C_False); });
                      C_S f_x2 -> (case f_y2 of {
                                     C_Z -> (C_False);
                                     C_S f_y22 -> (f_equal f_x2 f_y22); }); });
prove: (forall f_n . ((f_le f_n (C_Z)) = (f_equal f_n (C_Z))));
