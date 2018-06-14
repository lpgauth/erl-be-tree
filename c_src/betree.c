#include <stdio.h>
#include <string.h>

#include "erl_nif.h"

#include "betree.h"

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_bad_expr;
static ERL_NIF_TERM atom_bad_event;

static ErlNifResourceType* MEM_BETREE;

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

    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_bad_expr = enif_make_atom(env, "bad_expr");
    atom_bad_event = enif_make_atom(env, "bad_event");

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

static ERL_NIF_TERM nif_betree_add_domain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    char* domain = NULL;
    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);

    uint32_t len;
    if(!enif_get_list_length(env, argv[1], &len)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    domain = enif_alloc(++len);
    int ret = enif_get_string(env, argv[1], domain, len, ERL_NIF_LATIN1);
    if(ret < 1) {
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

static ERL_NIF_TERM nif_betree_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
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

    uint32_t len;
    if(!enif_get_list_length(env, argv[2], &len)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    expr = enif_alloc(++len);
    int ret = enif_get_string(env, argv[2], expr, len, ERL_NIF_LATIN1);
    if(ret < 1) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    bool result = betree_insert(sub_id, expr, betree);
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
    char* event = NULL;
    struct report* report = NULL;
    ERL_NIF_TERM* subs = NULL;
    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);

    uint32_t len;
    if(!enif_get_list_length(env, argv[1], &len)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    event = enif_alloc(++len);
    if(enif_get_string(env, argv[1], event, len, ERL_NIF_LATIN1) <= 0) {
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

    retval = enif_make_tuple(env, 2, atom_ok, res);
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

    betree_delete(sub_id, betree);
    retval = atom_ok;
cleanup:

    return retval;
}

static ErlNifFunc nif_functions[] = {
    {"betree_make", 0, nif_betree_make, 0},
    {"betree_free", 1, nif_betree_free, 0},
    {"betree_add_domain", 2, nif_betree_add_domain, 0},
    {"betree_insert", 3, nif_betree_insert, 0},
    {"betree_search", 2, nif_betree_search, 0},
    {"betree_delete", 2, nif_betree_delete, 0},
};

ERL_NIF_INIT(erl_betree, nif_functions, &load, NULL, NULL, NULL);

