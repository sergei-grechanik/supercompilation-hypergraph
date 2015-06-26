f_qrev f_z f_y2 = (case f_z of {
                     C_nil -> f_y2;
                     C_cons f_z2 f_xs -> (f_qrev f_xs (C_cons f_z2 f_y2)); });
f_append f_z f_y2 = (case f_z of {
                       C_nil -> f_y2;
                       C_cons f_z2 f_xs -> (C_cons f_z2 (f_append f_xs f_y2)); });
f_rev f_z = (case f_z of {
               C_nil -> (C_nil);
               C_cons f_y2 f_xs -> (f_append (f_rev f_xs) (C_cons f_y2 (C_nil))); });
prove: (forall f_z f_y2 . ((f_qrev f_z f_y2) = (f_append (f_rev f_z) f_y2)));
