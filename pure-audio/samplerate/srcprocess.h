
#include <samplerate.h>
#include <pure/runtime.h>

pure_expr *
src_process_data(SRC_STATE *state,
		 float *in, long in_size,
		 float *out, long out_size,
		 double ratio, bool wrap);

pure_expr *
src_simple_data(int conv_type, int channels,
		float *in, long in_size,
		float *out, long out_size,
		double ratio);
