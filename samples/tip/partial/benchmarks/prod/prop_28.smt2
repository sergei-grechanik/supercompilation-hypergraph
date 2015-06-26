f_append f_z f_y2 = (case f_z of {
                       C_nil -> f_y2;
                       C_cons f_z2 f_xs -> (C_cons f_z2 (f_append f_xs f_y2)); });
f_rev f_z = (case f_z of {
               C_nil -> (C_nil);
               C_cons f_y2 f_xs -> (f_append (f_rev f_xs) (C_cons f_y2 (C_nil))); });
f_qrevflat f_z f_y2 = (case f_z of {
                         C_nil -> f_y2;
                         C_cons f_xs f_xss -> (f_qrevflat f_xss (f_append (f_rev f_xs) f_y2)); });
f_revflat f_z = (case f_z of {
                   C_nil -> (C_nil);
                   C_cons f_xs f_xss -> (f_append (f_revflat f_xss) (f_rev f_xs)); });
prove: (forall f_z . ((f_revflat f_z) = (f_qrevflat f_z (C_nil))));
