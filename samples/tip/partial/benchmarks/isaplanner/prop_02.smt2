f_plus f_z f_y2 = (case f_z of {
                     C_Z -> f_y2;
                     C_S f_z2 -> (C_S (f_plus f_z2 f_y2)); });
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
f_append f_z f_y2 = (case f_z of {
                       C_nil -> f_y2;
                       C_cons f_z2 f_xs -> (C_cons f_z2 (f_append f_xs f_y2)); });
prove: (forall f_n f_xs f_ys . ((f_plus (f_count f_n f_xs) (f_count f_n f_ys)) = (f_count f_n (f_append f_xs f_ys))));
