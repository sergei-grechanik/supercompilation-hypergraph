f_le f_z f_y2 = (case f_z of {
                   C_Z -> (C_True);
                   C_S f_z2 -> (case f_y2 of {
                                  C_Z -> (C_False);
                                  C_S f_x2 -> (f_le f_z2 f_x2); }); });
f_insort f_z f_y2 = (case f_y2 of {
                       C_nil -> (C_cons f_z (C_nil));
                       C_cons f_z2 f_xs -> (case (f_le f_z f_z2) of {
                                              C_True -> (C_cons f_z f_y2);
                                              C_False -> (C_cons f_z2 (f_insort f_z f_xs)); }); });
f_sort f_z = (case f_z of {
                C_nil -> (C_nil);
                C_cons f_y2 f_xs -> (f_insort f_y2 (f_sort f_xs)); });
f_equal f_z f_y2 = (case f_z of {
                      C_Z -> (case f_y2 of {
                                C_Z -> (C_True);
                                C_S f_z2 -> (C_False); });
                      C_S f_x2 -> (case f_y2 of {
                                     C_Z -> (C_False);
                                     C_S f_y22 -> (f_equal f_x2 f_y22); }); });
f_count f_z f_y2 = (case f_y2 of {
                      C_nil -> (C_Z);
                      C_cons f_z2 f_ys -> (case (f_equal f_z f_z2) of {
                                             C_True -> (C_S (f_count f_z f_ys));
                                             C_False -> (f_count f_z f_ys); }); });
prove: (forall f_n f_xs . ((f_count f_n f_xs) = (f_count f_n (f_sort f_xs))));
