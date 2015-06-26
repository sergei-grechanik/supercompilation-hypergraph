f_length f_z = (case f_z of {
                  C_nil -> (C_Z);
                  C_cons f_y2 f_xs -> (C_S (f_length f_xs)); });
f_even f_z = (case f_z of {
                C_Z -> (C_True);
                C_S f_y2 -> (case f_y2 of {
                               C_Z -> (C_False);
                               C_S f_z2 -> (f_even f_z2); }); });
f_append f_z f_y2 = (case f_z of {
                       C_nil -> f_y2;
                       C_cons f_z2 f_xs -> (C_cons f_z2 (f_append f_xs f_y2)); });
prove: (forall f_z f_y2 . ((f_even (f_length (f_append f_z f_y2))) = (f_even (f_length (f_append f_y2 f_z)))));
