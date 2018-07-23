#include <stdio.h>
#include <string.h>

#include "erl_nif.h"

#include "betree.h"
#include "config.h"
#include "tree.h"

// return values
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_bad_expr;
static ERL_NIF_TERM atom_bad_event;

// domain types
static ERL_NIF_TERM atom_int;
static ERL_NIF_TERM atom_int_list;
static ERL_NIF_TERM atom_bin;
static ERL_NIF_TERM atom_bin_list;
static ERL_NIF_TERM atom_bool;
static ERL_NIF_TERM atom_float;
static ERL_NIF_TERM atom_frequency_caps;
static ERL_NIF_TERM atom_segments;
static ERL_NIF_TERM atom_int64;

// domain properties
static ERL_NIF_TERM atom_allow_undefined;
static ERL_NIF_TERM atom_disallow_undefined;

static ErlNifResourceType* MEM_BETREE;

static ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;

    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1))
        return ret;

    return enif_make_atom(env, name);
}

static void cleanup(ErlNifEnv* env, void* res)
{
    (void)env;
    struct betree** betree_res = res;
    struct betree* betree = *betree_res;
    betree_free(betree);
}

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;

    atom_ok = make_atom(env, "ok");
    atom_error = make_atom(env, "error");
    atom_bad_expr = make_atom(env, "bad_expr");
    atom_bad_event = make_atom(env, "bad_event");

    atom_int = make_atom(env, "int");;
    atom_int_list = make_atom(env, "int_list");
    atom_bin = make_atom(env, "bin");
    atom_bin_list = make_atom(env, "bin_list");
    atom_bool = make_atom(env, "bool");;
    atom_float = make_atom(env, "float");
    atom_frequency_caps = make_atom(env, "frequency_caps");
    atom_segments = make_atom(env, "segments");
    atom_int64 = make_atom(env, "int64");

    atom_allow_undefined = make_atom(env, "allow_undefined");
    atom_disallow_undefined = make_atom(env, "disallow_undefined");

    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    MEM_BETREE = enif_open_resource_type(env, NULL, "betree", cleanup, flags, NULL);
    if (MEM_BETREE == NULL) {
        return -1;
    }

    return 0;
}

static struct betree* get_betree(ErlNifEnv* env, const ERL_NIF_TERM term)
{
    struct betree** betree_res = NULL;
    enif_get_resource(env, term, MEM_BETREE, (void**)&betree_res);
    struct betree* betree = *betree_res;
    return betree;
}

static ERL_NIF_TERM nif_betree_free(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;

    if(argc != 1) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    enif_release_resource(MEM_BETREE);

    retval = atom_ok;
cleanup:
    return retval;
}

static ERL_NIF_TERM nif_betree_make(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;(void)argc;(void)argv;
    ERL_NIF_TERM retval;

    struct betree** betree_res = enif_alloc_resource(MEM_BETREE, sizeof(*betree_res));

    struct betree* betree = betree_make();
    memcpy(betree_res, &betree, sizeof(*betree_res));

    ERL_NIF_TERM term = enif_make_resource(env, betree_res);

    enif_release_resource(betree_res);

    retval = enif_make_tuple(env, 2, atom_ok, term);
    return retval;
}

static char *alloc_string(ErlNifBinary bin)
{
    size_t key_len = bin.size;
    char *key = enif_alloc(key_len + 1);
    if (!key) return NULL;

    memcpy(key, bin.data, key_len);
    key[key_len] = 0;

    return key;
}

static ERL_NIF_TERM nif_betree_add_domain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    ErlNifBinary bin;
    char* domain = NULL;
    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);

    if (!enif_inspect_binary(env, argv[1], &bin)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }
    domain = alloc_string(bin);
    if (domain == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    betree_add_domain(betree, domain);
    retval = atom_ok;
cleanup:
    if(domain != NULL) {
        enif_free(domain);
    }

    return retval;
}

#define DOMAIN_NAME_LEN 256

static ERL_NIF_TERM nif_betree_add_domains(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);

    unsigned int list_len;
    if(!enif_get_list_length(env, argv[1], &list_len)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = argv[1];
    const ERL_NIF_TERM* tuple;
    int tuple_len;
    char domain_name[DOMAIN_NAME_LEN];
    bool allow_undefined;

    for(unsigned int i = 0; i < list_len; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }

        if(!enif_get_tuple(env, head, &tuple_len, &tuple)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }
        if(tuple_len < 3) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }

        if(!enif_get_atom(env, tuple[0], domain_name, DOMAIN_NAME_LEN, ERL_NIF_LATIN1)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }

        if(enif_is_identical(atom_allow_undefined, tuple[2])) {
            allow_undefined = true;
        }
        else if(enif_is_identical(atom_disallow_undefined, tuple[2])) {
            allow_undefined = false;
        }
        else {
            retval = enif_make_badarg(env);
            goto cleanup;
        }

        if(enif_is_identical(atom_int, tuple[1]) || enif_is_identical(atom_int64, tuple[1])) {
            if(tuple_len == 5) {
                int64_t min, max;
                if(!enif_get_int64(env, tuple[3], &min) || !enif_get_int64(env, tuple[4], &max)) {
                    retval = enif_make_badarg(env);
                    goto cleanup;
                }
                add_attr_domain_bounded_i(betree->config, domain_name, allow_undefined, min, max);
            }
            else {
                add_attr_domain_i(betree->config, domain_name, allow_undefined);
            }
        }
        else if(enif_is_identical(atom_int_list, tuple[1])) {
            if(tuple_len == 5) {
                int64_t min, max;
                if(!enif_get_int64(env, tuple[3], &min) || !enif_get_int64(env, tuple[4], &max)) {
                    retval = enif_make_badarg(env);
                    goto cleanup;
                }
                add_attr_domain_bounded_il(betree->config, domain_name, allow_undefined, min, max);
            }
            else {
                add_attr_domain_il(betree->config, domain_name, allow_undefined);
            }
        }
        else if(enif_is_identical(atom_bin, tuple[1])) {
            if(tuple_len == 4) {
                uint64_t max;
                if(!enif_get_uint64(env, tuple[3], &max)) {
                    retval = enif_make_badarg(env);
                    goto cleanup;
                }
                add_attr_domain_bounded_s(betree->config, domain_name, allow_undefined, max);
            }
            else {
                add_attr_domain_s(betree->config, domain_name, allow_undefined);
            }
        }
        else if(enif_is_identical(atom_bin_list, tuple[1])) {
            if(tuple_len == 4) {
                uint64_t max;
                if(!enif_get_uint64(env, tuple[3], &max)) {
                    retval = enif_make_badarg(env);
                    goto cleanup;
                }
                add_attr_domain_bounded_sl(betree->config, domain_name, allow_undefined, max);
            }
            else {
                add_attr_domain_sl(betree->config, domain_name, allow_undefined);
            }
        }
        else if(enif_is_identical(atom_bool, tuple[1])) {
            add_attr_domain_b(betree->config, domain_name, allow_undefined);
        }
        else if(enif_is_identical(atom_float, tuple[1])) {
            if(tuple_len == 5) {
                double min, max;
                if(!enif_get_double(env, tuple[3], &min) || !enif_get_double(env, tuple[4], &max)) {
                    retval = enif_make_badarg(env);
                    goto cleanup;
                }
                add_attr_domain_bounded_f(betree->config, domain_name, allow_undefined, min, max);
            }
            else {
                add_attr_domain_f(betree->config, domain_name, allow_undefined);
            }
        }
        else if(enif_is_identical(atom_frequency_caps, tuple[1])) {
            add_attr_domain_frequency(betree->config, domain_name, allow_undefined);
        }
        else if(enif_is_identical(atom_segments, tuple[1])) {
            add_attr_domain_segments(betree->config, domain_name, allow_undefined);
        }
        else {
            retval = enif_make_badarg(env);
            goto cleanup;
        }
    }

    retval = atom_ok;
cleanup:

    return retval;
}

static ERL_NIF_TERM nif_betree_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    ErlNifBinary bin;
    char* expr = NULL;
    if(argc != 3) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);

    betree_sub_t sub_id;
    if(!enif_get_uint64(env, argv[1], &sub_id)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    if (!enif_inspect_binary(env, argv[2], &bin)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }
    expr = alloc_string(bin);
    if (expr == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    bool result = betree_insert(betree, sub_id, expr);
    if(result) {
        retval = atom_ok;
    }
    else {
        retval = atom_error;
    }
cleanup:
    if(expr != NULL) {
        enif_free(expr);
    }

    return retval;
}

static ERL_NIF_TERM nif_betree_search(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    ErlNifBinary bin;
    char* event = NULL;
    struct report* report = NULL;
    ERL_NIF_TERM* subs = NULL;

    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);
    if(betree == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    if (!enif_inspect_binary(env, argv[1], &bin)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }
    event = alloc_string(bin);
    if (event == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    report = make_report();
    betree_search(betree, event, report);
    subs = calloc(report->matched, sizeof(*subs));
    for(size_t i = 0; i < report->matched; i++) {
        subs[i] = enif_make_uint64(env, report->subs[i]);
    }
    ERL_NIF_TERM res = enif_make_list_from_array(env, subs, report->matched);
    retval = enif_make_tuple2(env, atom_ok, res);
cleanup:
    if(event != NULL) {
        enif_free(event);
    }
    if(report != NULL) {
        free_report(report);
    }
    if(subs != NULL) {
        free(subs);
    }
    return retval;
}

static ERL_NIF_TERM nif_betree_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);

    betree_sub_t sub_id;
    if(!enif_get_uint64(env, argv[1], &sub_id)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    betree_delete(betree, sub_id);
    retval = atom_ok;
cleanup:

    return retval;
}

static ErlNifFunc nif_functions[] = {
    {"betree_make", 0, nif_betree_make, 0},
    {"betree_free", 1, nif_betree_free, 0},
    {"betree_add_domain", 2, nif_betree_add_domain, 0},
    {"betree_add_domains", 2, nif_betree_add_domains, 0},
    {"betree_insert", 3, nif_betree_insert, 0},
    {"betree_search", 2, nif_betree_search, 0},
    {"betree_delete", 2, nif_betree_delete, 0},
};

ERL_NIF_INIT(erl_betree, nif_functions, &load, NULL, NULL, NULL);

