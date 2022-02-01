static
r_obj* node_list_interp_fixup(r_obj* x,
                              r_obj* parent,
                              r_obj* env,
                              struct ast_rotation_info* rotation_info,
                              bool expand_lhs);

static
void node_list_interp_fixup_rhs(r_obj* rhs,
                                r_obj* rhs_node,
                                r_obj* parent,
                                r_obj* env,
                                struct ast_rotation_info* info);
