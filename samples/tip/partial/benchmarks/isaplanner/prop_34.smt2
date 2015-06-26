f_min2 f_z f_y2 = (case f_z of {
                     C_Z -> (C_Z);
                     C_S f_z2 -> (case f_y2 of {
                                    C_Z -> (C_Z);
                                    C_S f_y1 -> (C_S (f_min2 f_z2 f_y1)); }); });
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
prove: (forall f_a f_b . ((f_equal (f_min2 f_a f_b) f_b) = (f_le f_b f_a)));
