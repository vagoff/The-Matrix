static uw_Basis_int __uwn_c_1099(uw_context ctx, uw_Basis_int __uwr_n_0)
{
    return(({
        uw_Basis_int disc0 = __uwr_n_0;
        uw_Basis_int result;
        {
            if (disc0 != 0LL) goto L4;
            result = 1LL;
            goto L3;
        }
L4:     {
            result = __uwn_d_1100(ctx, __uwr_n_0 - 1LL);
            goto L3;
        }
L5:
        uw_error(ctx, FATAL, "/mnt/sources/ur/tce.ur:6:10-10:0: pattern match failure");
L3:     result;
    }));
}

static uw_Basis_int __uwn_d_1100(uw_context ctx, uw_Basis_int __uwr_m_0)
{
    return(({
        uw_Basis_int disc0 = __uwr_m_0;
        uw_Basis_int result;
        {
            if (disc0 != 0LL) goto L1;
            result = 2LL;
            goto L0;
        }
L1:	{
            result = __uwn_c_1099(ctx, __uwr_m_0 - 1LL);
            goto L0;
        }
L2:	uw_error(ctx, FATAL, "/mnt/sources/ur/tce.ur:10:10-15:0: pattern match failure");
L0:	result;
    }));
}
