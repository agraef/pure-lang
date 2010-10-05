
#include <stdlib.h>
#include <string.h>
#include "midifile.h"
#include "mf.h"

/* GSL matrix data types and operations needed to represent MIDI data. */

typedef struct _gsl_block_int
{
  size_t size;
  int *data;
} gsl_block_int;

typedef struct _gsl_matrix_int
{
  size_t size1;
  size_t size2;
  size_t tda;
  int *data;
  gsl_block_int *block;
  int owner;
} gsl_matrix_int;

static gsl_matrix_int* 
gsl_matrix_int_alloc(const size_t n1, const size_t n2)
{
  gsl_block_int* block;
  gsl_matrix_int* m;
  if (n1 == 0 || n2 == 0)
    return 0;
  m = (gsl_matrix_int*)malloc(sizeof(gsl_matrix_int));
  if (m == 0)
    return 0;
  block = (gsl_block_int*)malloc(sizeof(gsl_block_int));
  if (block == 0) {
    free(m);
    return 0;
  }
  block->size = n1*n2;
  block->data = (int*)malloc(block->size*sizeof(int));
  if (block->data == 0) {
    free(m);
    free(block);
    return 0;
  }
  m->data = block->data;
  m->size1 = n1;
  m->size2 = n2;
  m->tda = n2; 
  m->block = block;
  m->owner = 1;
  return m;
}

static gsl_matrix_int*
gsl_matrix_int_calloc(const size_t n1, const size_t n2)
{
  gsl_matrix_int* m = gsl_matrix_int_alloc(n1, n2);
  if (m == 0) return 0;
  memset(m->data, 0, m->block->size*sizeof(int));
  return m;
}

static inline gsl_matrix_int*
create_int_matrix(size_t nrows, size_t ncols)
{
  if (nrows == 0 || ncols == 0 ) {
    size_t nrows1 = (nrows>0)?nrows:1;
    size_t ncols1 = (ncols>0)?ncols:1;
    gsl_matrix_int *m = gsl_matrix_int_calloc(nrows1, ncols1);
    if (!m) return 0;
    m->size1 = nrows; m->size2 = ncols;
    return m;
  } else
    return gsl_matrix_int_alloc(nrows, ncols);
}

/* Create a Pure pointer with midifile::free sentry from a midi file object. */

static pure_expr *make_file(MidiFile_t mf)
{
  if (mf) {
    pure_expr *x = pure_symbol(pure_sym("midifile::free"));
    return pure_sentry(x, pure_pointer(mf));
  } else
    return pure_pointer(0);
}

static bool is_file(pure_expr *x, MidiFile_t *mf)
{
  pure_expr *y;
  return pure_is_pointer(x, (void**)mf) && (y = pure_get_sentry(x)) &&
    y->tag > 0 && strcmp(pure_sym_pname(y->tag), "midifile::free") == 0;
}

/* Interface operations. */

pure_expr *mf_new(int file_format, int division, int resolution)
{
  MidiFileDivisionType_t division_type = MIDI_FILE_DIVISION_TYPE_INVALID;
  switch (division) {
  case 0:
    division_type = MIDI_FILE_DIVISION_TYPE_PPQ;
    break;
  case 24:
    division_type = MIDI_FILE_DIVISION_TYPE_SMPTE24;
    break;
  case 25:
    division_type = MIDI_FILE_DIVISION_TYPE_SMPTE25;
    break;
  case 29:
    division_type = MIDI_FILE_DIVISION_TYPE_SMPTE30DROP;
    break;
  case 30:
    division_type = MIDI_FILE_DIVISION_TYPE_SMPTE30;
    break;
  default:
    break;
  }
  return make_file(MidiFile_new(file_format, division_type, resolution));
}

pure_expr *mf_load(char *filename)
{
  return make_file(MidiFile_load(filename));
}

bool mf_save(pure_expr *x, char *filename)
{
  MidiFile_t mf;
  if (!is_file(x, &mf)) return false;
  return MidiFile_save(mf, filename) == 0;
}

bool mf_free(pure_expr *x)
{
  MidiFile_t mf;
  if (!is_file(x, &mf)) return false;
  pure_sentry(0, x);
  return MidiFile_free(mf) == 0;
}

static const int divisions[] = {0, 24, 25, 29, 30};

pure_expr *mf_info(pure_expr *x)
{
  MidiFile_t mf;
  if (!is_file(x, &mf)) return 0;
  MidiFileDivisionType_t type = MidiFile_getDivisionType(mf);
  int division = (type>=0 && type<5)?divisions[type]:-1;
  return pure_tuplel(4,
		     pure_int(MidiFile_getFileFormat(mf)),
		     pure_int(division),
		     pure_int(MidiFile_getResolution(mf)),
		     pure_int(MidiFile_getNumberOfTracks(mf)));
}

static pure_expr *decode_event(MidiFileEvent_t ev)
{
  if (MidiFileEvent_isVoiceEvent(ev)) {
    uint32_t data = MidiFileVoiceEvent_getData(ev);
    gsl_matrix_int *mat = create_int_matrix(1, 4);
    int i;
    for (i = 0; i < 4; i++) {
      mat->data[i] = data&0xff;
      data >>= 8;
    }
    return pure_tuplel(2, pure_int(MidiFileEvent_getTick(ev)),
		       pure_int_matrix(mat));
  } else {
    MidiFileEventType_t t = MidiFileEvent_getType(ev);
    switch (t) {
    case MIDI_FILE_EVENT_TYPE_SYSEX:
      {
	int i, n = MidiFileSysexEvent_getDataLength(ev);
	unsigned char *data = MidiFileSysexEvent_getData(ev);
	gsl_matrix_int *mat = create_int_matrix(1, n);
	for (i = 0; i < n; i++)
	  mat->data[i] = data[i];
	return pure_tuplel(2, pure_int(MidiFileEvent_getTick(ev)),
			   pure_int_matrix(mat));
      }
    case MIDI_FILE_EVENT_TYPE_META:
      {
	int num = MidiFileMetaEvent_getNumber(ev);
	int i, n = MidiFileMetaEvent_getDataLength(ev);
	unsigned char *data = MidiFileMetaEvent_getData(ev);
	gsl_matrix_int *mat = create_int_matrix(1, n+2);
	mat->data[0] = 0xff; /* indicates a meta event */
	mat->data[1] = num; /* indicates the type of meta event */
	for (i = 0; i < n; i++)
	  mat->data[i+2] = data[i];
	return pure_tuplel(2, pure_int(MidiFileEvent_getTick(ev)),
			   pure_int_matrix(mat));
      }
    default:
      /* Invalid event, maybe we should throw an exception here? For now, we
	 just return the tick along with an empty vector. */
      {
	gsl_matrix_int *mat = create_int_matrix(1, 0);
	return pure_tuplel(2, pure_int(MidiFileEvent_getTick(ev)),
			   pure_int_matrix(mat));
      }
    }
  }
}

static pure_expr *decode_track(MidiFileTrack_t track)
{
  MidiFileEvent_t ev;
  size_t n = 1;
  pure_expr *x, **xs;
  if (!track) return 0;
  ev = MidiFileTrack_getFirstEvent(track);
  if (!ev) return pure_listl(0);
  for (ev = MidiFileEvent_getNextEventInTrack(ev); ev;
       ev = MidiFileEvent_getNextEventInTrack(ev))
    n++;
  xs = malloc(n*sizeof(pure_expr*));
  if (!xs) return 0;
  for (n = 0, ev = MidiFileTrack_getFirstEvent(track); ev;
       ev = MidiFileEvent_getNextEventInTrack(ev))
    xs[n++] = decode_event(ev);
  x = pure_listv(n, xs);
  free(xs);
  return x;
}

pure_expr *mf_get_track(pure_expr *x, int number)
{
  MidiFile_t mf;
  if (!is_file(x, &mf)) return 0;
  MidiFileTrack_t track = MidiFile_getTrackByNumber(mf, number, 0);
  return decode_track(track);
}

pure_expr *mf_get_tracks(pure_expr *x)
{
  MidiFile_t mf;
  if (!is_file(x, &mf)) return 0;
  int n = MidiFile_getNumberOfTracks(mf);
  MidiFileTrack_t track = MidiFile_getFirstTrack(mf);
  pure_expr **xs;
  if (n == 0) return pure_listl(0);
  xs = malloc(n*sizeof(pure_expr*));
  if (!xs) return 0;
  xs[0] = decode_track(track);
  for (n=1, track = MidiFileTrack_getNextTrack(track); track;
       track = MidiFileTrack_getNextTrack(track))
    xs[n++] = decode_track(track);
  x = pure_listv(n, xs);
  free(xs);
  return x;
}

static bool encode_event(MidiFileTrack_t track, pure_expr *x)
{
  pure_expr **xv;
  size_t n;
  if (!pure_is_tuplev(x, &n, NULL) || n != 2)
    return false;
  pure_is_tuplev(x, &n, &xv);
  int32_t tick;
  gsl_matrix_int *mat;
  if (!pure_is_int(xv[0], &tick) ||
      !pure_is_int_matrix(xv[1], (void**)&mat)) {
    free(xv);
    return false;
  }
  x = xv[1];
  free(xv);
  n = mat->size1*mat->size2;
  if (n == 0) return false; // no data
  int status = mat->data[0];
  switch (status & 0xf0) {
  case 0x80: case 0x90: case 0xa0: case 0xb0: case 0xc0: case 0xd0: case 0xe0:
    {
      // voice message
      if (n > 4) return false; // excess data
      int i;
      uint32_t data = 0;
      for (i = 0; i < n; i++) {
	data <<= 8;
	data |= mat->data[n-i-1];
      }
      if (!MidiFileTrack_createVoiceEvent(track, tick, data))
	return false;
    }
    break;
  case 0xf0:
    switch (status) {
    case 0xf0: case 0xf7:
      {
	// sysex
	unsigned char *data = (unsigned char*)matrix_to_byte_array(NULL, x);
	if (!MidiFileTrack_createSysexEvent(track, tick, n, data)) {
	  free(data);
	  return false;
	}
	free(data);
      }
      break;
    case 0xff:
      {
	// meta
	int number = mat->data[0];
	/* We ignore the end-of-track marker here since it gets added
	   automatically by MidiFile_save(). */
	if (number == 0x2f) break;
	unsigned char *data = (unsigned char*)matrix_to_byte_array(NULL, x);
	if (!MidiFileTrack_createMetaEvent(track, tick, number, n-1, data+1)) {
	  free(data);
	  return false;
	}
	free(data);
      }
      break;
    default:
      return false; // invalid status byte
    }
    break;
  default:
    return false; // invalid status byte
  }
  return true;
}

bool mf_put_track(pure_expr *x, pure_expr *xs)
{
  MidiFile_t mf;
  pure_expr **xv;
  size_t i, n;
  if (!is_file(x, &mf) || !pure_is_listv(xs, &n, &xv))
    return false;
  else if (n == 0)
    return true;
  MidiFileTrack_t track = MidiFile_createTrack(mf);
  if (!track) goto err;
  for (i = 0; i < n; i++) {
    if (!encode_event(track, xv[i])) goto err;
  }
  free(xv);
  return true;
 err:
  free(xv);
  return false;
}

bool mf_put_tracks(pure_expr *x, pure_expr *xs)
{
  MidiFile_t mf;
  pure_expr **xv;
  size_t i, n;
  if (!is_file(x, &mf) || !pure_is_listv(xs, &n, &xv))
    return false;
  else if (n == 0)
    return true;
  for (i = 0; i < n; i++) {
    if (!mf_put_track(x, xv[i])) goto err;
  }
  free(xv);
  return true;
 err:
  free(xv);
  return false;
}
