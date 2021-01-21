static
sexp* dict_find_node_info(struct r_dict* dict,
                          sexp* key,
                          r_ssize* hash,
                          sexp** parent);

static
sexp* dict_find_node(struct r_dict* dict, sexp* key);
