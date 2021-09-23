/*
** Automatically generated from `fib.m'
** by the Mercury compiler,
** version 20.06-1~focal1
** configured for x86_64-pc-linux-gnu.
** Do not edit.
**
** The autoconfigured grade settings governing
** the generation of this C file were
**
** TAG_BITS=3
** UNBOXED_FLOAT=yes
** UNBOXED_INT64S=yes
** PREGENERATED_DIST=no
** HIGHLEVEL_CODE=no
**
** END_OF_C_GRADE_INFO
*/

/*
INIT mercury__fib__init
ENDINIT
*/

#define MR_ALLOW_RESET
#include "mercury_imp.h"
#line 28 "Mercury/cs/fib.c"
#include "array.mh"

#line 31 "Mercury/cs/fib.c"
#line 32 "Mercury/cs/fib.c"
#include "bitmap.mh"

#line 35 "Mercury/cs/fib.c"
#line 36 "Mercury/cs/fib.c"
#include "fib.mh"

#line 39 "Mercury/cs/fib.c"
#line 40 "Mercury/cs/fib.c"
#include "io.mh"

#line 43 "Mercury/cs/fib.c"
#line 44 "Mercury/cs/fib.c"
#include "string.mh"

#line 47 "Mercury/cs/fib.c"
#line 48 "Mercury/cs/fib.c"
#include "time.mh"

#line 51 "Mercury/cs/fib.c"
#line 52 "Mercury/cs/fib.c"
#ifndef FIB_DECL_GUARD
#define FIB_DECL_GUARD

#line 56 "Mercury/cs/fib.c"
#line 57 "Mercury/cs/fib.c"

#endif
#line 60 "Mercury/cs/fib.c"

#ifdef _MSC_VER
#define MR_STATIC_LINKAGE extern
#else
#define MR_STATIC_LINKAGE static
#endif
MR_decl_label3(fib__fib_2_0, 13,3,4)
MR_decl_label10(main_2_0, 2,3,4,5,6,7,8,9,10,11)
MR_decl_label1(main_2_0, 12)
MR_decl_label3(fn__fib__fibExp_1_0, 13,3,4)
MR_decl_label3(fn__fib__fibFunc_1_0, 13,3,4)
MR_def_extern_entry(main_2_0)
MR_decl_static(fib__fib_2_0)
MR_decl_static(fn__fib__fibFunc_1_0)
MR_decl_static(fn__fib__fibExp_1_0)



MR_decl_entry(io__write_string_3_0);
MR_decl_entry(io__write_int_3_0);
MR_decl_entry(io__nl_2_0);

MR_BEGIN_MODULE(fib_module0)
	MR_init_entry1(main_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__main_2_0);
	MR_init_label10(main_2_0,2,3,4,5,6,7,8,9,10,11)
	MR_init_label1(main_2_0,12)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'main'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__main_2_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	MR_r1 = (MR_Integer) 17;
	MR_np_call_localret_ent(fib__fib_2_0,
		main_2_0_i2);
MR_def_label(main_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_sv(1) = MR_r1;
	MR_r1 = ((MR_Word) MR_string_const("fib(17, ", 8));
	MR_np_call_localret_ent(io__write_string_3_0,
		main_2_0_i3);
MR_def_label(main_2_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(1);
	MR_np_call_localret_ent(io__write_int_3_0,
		main_2_0_i4);
MR_def_label(main_2_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = ((MR_Word) MR_string_const(")\n", 2));
	MR_np_call_localret_ent(io__write_string_3_0,
		main_2_0_i5);
MR_def_label(main_2_0,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = ((MR_Word) MR_string_const("fibFunc(17) = ", 14));
	MR_np_call_localret_ent(io__write_string_3_0,
		main_2_0_i6);
MR_def_label(main_2_0,6)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) 17;
	MR_np_call_localret_ent(fn__fib__fibFunc_1_0,
		main_2_0_i7);
MR_def_label(main_2_0,7)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_call_localret_ent(io__write_int_3_0,
		main_2_0_i8);
MR_def_label(main_2_0,8)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_call_localret_ent(io__nl_2_0,
		main_2_0_i9);
MR_def_label(main_2_0,9)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = ((MR_Word) MR_string_const("fibExp(17) = ", 13));
	MR_np_call_localret_ent(io__write_string_3_0,
		main_2_0_i10);
MR_def_label(main_2_0,10)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) 17;
	MR_np_call_localret_ent(fn__fib__fibExp_1_0,
		main_2_0_i11);
MR_def_label(main_2_0,11)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_call_localret_ent(io__write_int_3_0,
		main_2_0_i12);
MR_def_label(main_2_0,12)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_succip_word = MR_sv(2);
	MR_decr_sp(2);
	MR_np_tailcall_ent(io__nl_2_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(fib_module1)
	MR_init_entry1(fib__fib_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fib__fib_2_0);
	MR_init_label3(fib__fib_2_0,13,3,4)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'fib'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(fib__fib_2_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_GT(MR_r1,2)) {
		MR_GOTO_LAB(fib__fib_2_0_i13);
	}
	MR_r1 = (MR_Integer) 1;
	MR_proceed();
MR_def_label(fib__fib_2_0,13)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	MR_sv(1) = MR_r1;
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_r1) - (MR_Unsigned) (MR_Integer) 1);
	MR_np_localcall_lab(fib__fib_2_0,
		fib__fib_2_0_i3);
MR_def_label(fib__fib_2_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_sv(1);
	MR_sv(1) = MR_r1;
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_r2) - (MR_Unsigned) (MR_Integer) 2);
	MR_np_localcall_lab(fib__fib_2_0,
		fib__fib_2_0_i4);
MR_def_label(fib__fib_2_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_sv(1)) + (MR_Unsigned) ((MR_Integer) MR_r1));
	MR_decr_sp_and_return(2);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(fib_module2)
	MR_init_entry1(fn__fib__fibFunc_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__fib__fibFunc_1_0);
	MR_init_label3(fn__fib__fibFunc_1_0,13,3,4)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'fibFunc'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(fn__fib__fibFunc_1_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_GT(MR_r1,2)) {
		MR_GOTO_LAB(fn__fib__fibFunc_1_0_i13);
	}
	MR_r1 = (MR_Integer) 1;
	MR_proceed();
MR_def_label(fn__fib__fibFunc_1_0,13)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	MR_sv(1) = MR_r1;
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_r1) - (MR_Unsigned) (MR_Integer) 1);
	MR_np_localcall_lab(fn__fib__fibFunc_1_0,
		fn__fib__fibFunc_1_0_i3);
MR_def_label(fn__fib__fibFunc_1_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_sv(1);
	MR_sv(1) = MR_r1;
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_r2) - (MR_Unsigned) (MR_Integer) 2);
	MR_np_localcall_lab(fn__fib__fibFunc_1_0,
		fn__fib__fibFunc_1_0_i4);
MR_def_label(fn__fib__fibFunc_1_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_sv(1)) + (MR_Unsigned) ((MR_Integer) MR_r1));
	MR_decr_sp_and_return(2);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(fib_module3)
	MR_init_entry1(fn__fib__fibExp_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__fib__fibExp_1_0);
	MR_init_label3(fn__fib__fibExp_1_0,13,3,4)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'fibExp'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(fn__fib__fibExp_1_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_GT(MR_r1,2)) {
		MR_GOTO_LAB(fn__fib__fibExp_1_0_i13);
	}
	MR_r1 = (MR_Integer) 1;
	MR_proceed();
MR_def_label(fn__fib__fibExp_1_0,13)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	MR_sv(1) = MR_r1;
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_r1) - (MR_Unsigned) (MR_Integer) 1);
	MR_np_localcall_lab(fn__fib__fibExp_1_0,
		fn__fib__fibExp_1_0_i3);
MR_def_label(fn__fib__fibExp_1_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_sv(1);
	MR_sv(1) = MR_r1;
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_r2) - (MR_Unsigned) (MR_Integer) 2);
	MR_np_localcall_lab(fn__fib__fibExp_1_0,
		fn__fib__fibExp_1_0_i4);
MR_def_label(fn__fib__fibExp_1_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_sv(1)) + (MR_Unsigned) ((MR_Integer) MR_r1));
	MR_decr_sp_and_return(2);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

static void mercury__fib_maybe_bunch_0(void)
{
	fib_module0();
	fib_module1();
	fib_module2();
	fib_module3();
}

/* suppress gcc -Wmissing-decls warnings */
void mercury__fib__init(void);
void mercury__fib__init_type_tables(void);
void mercury__fib__init_debugger(void);
#ifdef MR_DEEP_PROFILING
void mercury__fib__write_out_proc_statics(FILE *deep_fp, FILE *procrep_fp);
#endif
#ifdef MR_RECORD_TERM_SIZES
void mercury__fib__init_complexity_procs(void);
#endif
#ifdef MR_THREADSCOPE
void mercury__fib__init_threadscope_string_table(void);
#endif
const char *mercury__fib__grade_check(void);

void mercury__fib__init(void)
{
	static MR_bool done = MR_FALSE;
	if (done) {
		return;
	}
	done = MR_TRUE;
	mercury__fib_maybe_bunch_0();
	mercury__fib__init_debugger();
}

void mercury__fib__init_type_tables(void)
{
	static MR_bool done = MR_FALSE;
	if (done) {
		return;
	}
	done = MR_TRUE;
}


void mercury__fib__init_debugger(void)
{
	static MR_bool done = MR_FALSE;
	if (done) {
		return;
	}
	done = MR_TRUE;
}

#ifdef MR_DEEP_PROFILING

void mercury__fib__write_out_proc_statics(FILE *deep_fp, FILE *procrep_fp)
{
	MR_write_out_module_proc_reps_start(procrep_fp, &mercury_data__module_layout__fib);
	MR_write_out_module_proc_reps_end(procrep_fp);
}

#endif

#ifdef MR_RECORD_TERM_SIZES

void mercury__fib__init_complexity_procs(void)
{
}

#endif

#ifdef MR_THREADSCOPE

void mercury__fib__init_threadscope_string_table(void)
{
}

#endif

// Ensure everything is compiled with the same grade.
const char *mercury__fib__grade_check(void)
{
    return &MR_GRADE_VAR;
}
