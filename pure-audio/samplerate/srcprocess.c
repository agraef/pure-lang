
#include "srcprocess.h"
#include <string.h>

pure_expr *
src_process_data(SRC_STATE *state,
		 float *in, long in_size,
		 float *out, long out_size,
		 double ratio, bool wrap)
{
  int ret;
  SRC_DATA data;
  memset(&data, 0, sizeof(SRC_DATA));
  data.data_in = in; data.data_out = out;
  data.input_frames = in_size; data.output_frames = out_size;
  data.end_of_input = wrap;
  data.src_ratio = ratio;
  ret = src_process(state, &data);
  return pure_tuplel(3, pure_int(ret),
		     pure_int64(data.input_frames_used),
		     pure_int64(data.output_frames_gen));
}

pure_expr *
src_simple_data(int conv_type, int channels,
		float *in, long in_size,
		float *out, long out_size,
		double ratio)
{
  int ret;
  SRC_DATA data;
  memset(&data, 0, sizeof(SRC_DATA));
  data.data_in = in; data.data_out = out;
  data.input_frames = in_size; data.output_frames = out_size;
  data.end_of_input = true;
  data.src_ratio = ratio;
  ret = src_simple(&data, conv_type, channels);
  return pure_tuplel(3, pure_int(ret),
		     pure_int64(data.input_frames_used),
		     pure_int64(data.output_frames_gen));
}
