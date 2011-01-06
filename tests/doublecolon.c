#include "/usr/local/include/urweb/config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "/usr/local/include/urweb/urweb.h"

static void uw_setup_limits()
{
}

void uw_global_custom()
{
    uw_setup_limits();
}
static void uw_client_init(void) { };
static void uw_db_init(uw_context ctx) { };
static int uw_db_begin(uw_context ctx)
{
    return 0;
};
static void uw_db_close(uw_context ctx) { };
static int uw_db_commit(uw_context ctx)
{
    return 0;
};
static int uw_db_rollback(uw_context ctx)
{
    return 0;
};

static const char begin_xhtml[] = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">";


static char jslib[] = "";

static struct __uws_0
__uwn_wrap_main_1100(uw_context ctx, uw_Basis_int __uwr_x0_0,
                     struct __uws_0 __uwr___1)
{
    return(((uw_write(ctx, "<body"), uw_unit_v),
            ((uw_write(ctx, uw_Basis_maybe_onload(ctx, uw_Basis_strcat(ctx, uw_Basis_get_settings(ctx, (
    {
        struct
                __uws_0
                tmp
                =
            {};
        tmp;
    })),""))), uw_unit_v),
    ((uw_write(ctx, uw_Basis_maybe_onunload(ctx, "")), uw_unit_v),
     ((uw_write(ctx, ">"), uw_unit_v),
      ((uw_write(ctx, uw_Basis_get_script(ctx, ( { struct __uws_0 tmp =
              {}; tmp;
                                                 }))), uw_unit_v),
       (uw_Basis_htmlifyInt_w(ctx, __uwr_x0_0 + 1LL),
        (uw_write(ctx, "</body>"), uw_unit_v))))))));
}

static int uw_input_num(const char *name)
{
    return -1;
}

static int uw_check_url(const char *s)
{
    return 0;
}

static int uw_check_mime(const char *s)
{
    return 0;
}

extern void uw_sign(const char *in, char *out);
extern int uw_hash_blocksize;
static uw_Basis_string uw_cookie_sig(uw_context ctx)
{
    uw_Basis_string r = uw_malloc(ctx, uw_hash_blocksize);
    uw_sign("", r);
    return uw_Basis_makeSigString(ctx, r);
}

static void uw_handle(uw_context ctx, char *request)
{
    if (!strcmp(request, "/app.js"))
    {
        uw_Basis_string ims = uw_Basis_requestHeader(ctx, "If-modified-since");
        if (ims && !strcmp(ims, "Sun, 02 Jan 2011 23:45:57"))
        {
            uw_clear_headers(ctx);
            uw_write_header(ctx, "HTTP/1.1 304 Not Modified\r\n");
            return;
        }

        uw_write_header(ctx, "Content-type: text/javascript\r\n");
        uw_write_header(ctx, "Last-modified: Sun, 02 Jan 2011 23:45:57\r\n");
        uw_write(ctx, jslib);
        return;
    }
    if (!strncmp(request, "/Doublecolon/main", 17) && (request[17] == 0 || request[17] == '/'))
    {
        request += 17;
        if (*request == '/') ++request;
        uw_write_header(ctx, "Content-type: text/html\r\n");
        uw_write_header(ctx, "Content-script-type: text/javascript\r\n");
        uw_write(ctx, begin_xhtml);
        uw_set_script_header(ctx, "");
        uw_set_needs_push(ctx, 0);
        uw_set_needs_sig(ctx, 0);
        uw_login(ctx);
        {
            uw_Basis_int arg0 = uw_Basis_unurlifyInt(ctx, &request);
            __uwn_wrap_main_1100(ctx, arg0, uw_unit_v);
            uw_write(ctx, "</html>");
            return;
        }
    }
    uw_clear_headers(ctx);
    uw_write_header(ctx, "HTTP/1.1 404 Not Found\r\nContent-type: text/plain\r\n");
    uw_write(ctx, "Not Found");
}

static void uw_expunger(uw_context ctx, uw_Basis_client cli) { };
static void uw_initializer(uw_context ctx) { };
uw_app uw_application = {1,
                         60,
                         "/",
                         uw_client_init,
                         uw_initializer,
                         uw_expunger,

                         uw_db_init,
                         uw_db_begin,
                         uw_db_commit,
                         uw_db_rollback,

                         uw_db_close,
                         uw_handle,
                         uw_input_num,
                         uw_cookie_sig,

                         uw_check_url,
                         uw_check_mime,
                         NULL
                        };

